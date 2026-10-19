with Ada.Text_IO;
package body Modeset_Model is
   type Register_Map is array (Unsigned_32 range 0 .. 16#20_0000# / 4 - 1) of Unsigned_32;
   Regs : constant access Register_Map := new Register_Map'[others => 0];
   Clock : Unsigned_64 := 0;
   Bad : Natural := 0;
   SCDC : Unsigned_8 := 0;
   Lock_Attempts : Natural := 0;
   DSL : Unsigned_32 := 0;
   Selected_While_Gated : array (Natural range 0 .. 1) of Boolean := [others => False];

   function Get (Offset : Unsigned_32) return Unsigned_32 is (Regs (Offset / 4));
   procedure Set (Offset, Value : Unsigned_32) is
   begin
      Regs (Offset / 4) := Value;
   end Set;

   procedure Reset is
   begin
      Regs.all := [others => 0];
      Clock := 0; Bad := 0; SCDC := 0; Lock_Attempts := 0;
      PLL_Never_Locks := False; Pipe_Never_Starts := False; Lock_Fault_Once := False;
      Underrun_On_Enable := False;
      Selected_While_Gated := [others => False];
   end Reset;

   procedure Violation (What : String) is
   begin
      Bad := Bad + 1;
      Ada.Text_IO.Put_Line ("ORDER VIOLATION: " & What);
   end Violation;

   function Locked_Any return Boolean is
     ((Get (16#46010#) and 16#4000_0000#) /= 0 or else (Get (16#46014#) and 16#4000_0000#) /= 0);

   function Running (Transcoder : Unsigned_32) return Boolean is
     ((Get (16#70008# + 16#1000# * Transcoder) and 16#4000_0000#) /= 0);

   procedure Tick (Us : Unsigned_64) is
      Before : constant Unsigned_64 := Clock / 16_000;
   begin
      Clock := Clock + Us;
      for T in Unsigned_32 range 0 .. 3 loop
         if Running (T) then
            --  A 16 ms frame.
            Set (16#70040# + 16#1000# * T,
                 Get (16#70040# + 16#1000# * T) + Unsigned_32 (Clock / 16_000 - Before));
            if Underrun_On_Enable then
               Set (16#44408# + 16#10# * T, Get (16#44408# + 16#10# * T) or 16#8000_0000#);
            end if;
         end if;
      end loop;
   end Tick;

   function Read (Offset : Unsigned_32) return Unsigned_32 is
   begin
      if Offset in 16#70000# | 16#71000# | 16#72000# | 16#73000# and then
        Running ((Offset - 16#70000#) / 16#1000#)
      then
         DSL := DSL + 7;
         return DSL mod 2_250;
      end if;
      return Get (Offset);
   end Read;

   procedure Write (Offset, Value : Unsigned_32; Written : out Boolean) is
   begin
      Written := True;
      if Offset in 16#46010# | 16#46014# then
         declare
            V : Unsigned_32;
            Power : constant Boolean := (Value and 16#0800_0000#) /= 0;
            Enable : constant Boolean := (Value and 16#8000_0000#) /= 0;
         begin
            V := Value and (16#0800_0000# or 16#8000_0000#);
            if Power then V := V or 16#0400_0000#; end if;
            if Enable and then Power then
               if (Get (Offset) and 16#8000_0000#) = 0 then
                  Lock_Attempts := Lock_Attempts + 1;
               end if;
               if not PLL_Never_Locks and then not (Lock_Fault_Once and then Lock_Attempts = 1) then
                  V := V or 16#4000_0000#;
               end if;
            end if;
            Set (Offset, V);
         end;
         return;
      end if;
      if Offset in 16#164284# | 16#164288# | 16#16428C# | 16#164290# then
         declare
            Enable_Reg : constant Unsigned_32 := (if Offset < 16#16428C# then 16#46010# else 16#46014#);
         begin
            if (Get (Enable_Reg) and 16#0400_0000#) = 0 or else (Get (Enable_Reg) and 16#8000_0000#) /= 0 then
               Violation ("CFGCR written while the PLL is unpowered or enabled");
            end if;
         end;
      elsif Offset = 16#164280# then
         for Phy in 0 .. 1 loop
            declare
               Mask : constant Unsigned_32 := Shift_Left (3, 2 * Phy);
               Off : constant Unsigned_32 := Shift_Left (1, 10 + Phy);
               Was_Gated : constant Boolean := (Get (Offset) and Off) /= 0;
               Stays_Gated : constant Boolean := (Value and Off) /= 0;
            begin
               if (Get (Offset) and Mask) /= (Value and Mask) and then not Was_Gated then
                  Violation ("DPCLKA select changed while the port clock runs");
               end if;
               --  i915: select the PLL and ungate the clock in separate writes.
               if Was_Gated and then Stays_Gated then
                  Selected_While_Gated (Phy) := True;
               elsif Was_Gated and then not Stays_Gated then
                  if not Selected_While_Gated (Phy) then
                     Violation ("DPCLKA clock ungated without a prior select write");
                  end if;
               elsif not Was_Gated and then Stays_Gated then
                  Selected_While_Gated (Phy) := False;
               end if;
            end;
         end loop;
      elsif Offset in 16#70008# | 16#71008# | 16#72008# | 16#73008# then
         declare
            T : constant Unsigned_32 := (Offset - 16#70008#) / 16#1000#;
         begin
            if (Value and 16#8000_0000#) /= 0 then
               if not Locked_Any or else (Get (16#60400# + 16#1000# * T) and 16#8000_0000#) = 0 then
                  Violation ("TRANSCONF enabled without a locked PLL and DDI function");
               end if;
               Set (Offset, (if Pipe_Never_Starts then Value and not 16#4000_0000#
                             else Value or 16#4000_0000#));
            else
               Set (Offset, Value and not 16#4000_0000#);
            end if;
            return;
         end;
      elsif Offset >= 16#64000# and then Offset < 16#64700# and then Offset mod 16#100# = 0 then
         if (Value and 16#8000_0000#) /= 0 then
            declare
               Port_Code : constant Unsigned_32 := (Offset - 16#64000#) / 16#100# + 1;
               Found : Boolean := False;
            begin
               for T in Unsigned_32 range 0 .. 3 loop
                  if Running (T) and then
                    Shift_Right (Get (16#60400# + 16#1000# * T), 27) mod 16 = Port_Code
                  then
                     Found := True;
                  end if;
               end loop;
               if not Found then
                  Violation ("DDI buffer enabled before its pipe runs");
               end if;
            end;
            Set (Offset, Value and not 16#80#);
         else
            Set (Offset, Value or 16#80#);
         end if;
         return;
      elsif Offset in 16#44408# | 16#44418# | 16#44428# | 16#44438# then
         Set (Offset, Get (Offset) and not Value);
         return;
      elsif Offset >= 16#60000# and then Offset < 16#64000# and then Offset mod 16#1000# <= 16#1C# then
         if (Get (16#70008# + 16#1000# * ((Offset - 16#60000#) / 16#1000#)) and 16#8000_0000#) /= 0 then
            Violation ("timings written while the pipe is enabled");
         end if;
      end if;
      Set (Offset, Value);
   end Write;

   function Now_Us return Unsigned_64 is
   begin
      Tick (100);
      return Clock;
   end Now_Us;

   procedure Pause is
   begin
      Tick (1_000);
   end Pause;

   procedure Write_TMDS_Config (Value : Unsigned_8; Done : out Boolean) is
   begin
      SCDC := Value;
      Done := True;
   end Write_TMDS_Config;

   procedure Log (Text : String) is
   begin
      if Verbose then
         Ada.Text_IO.Put_Line (Text);
      end if;
   end Log;

   function Violations return Natural is (Bad);
   function SCDC_Value return Unsigned_8 is (SCDC);
end Modeset_Model;
