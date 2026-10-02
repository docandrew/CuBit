with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_PCI_Power; use Intel_GPU_PCI_Power;
with Intel_GPU_PCI_IRQ_Disable;
procedure PCI_IRQ_Executor_Tests is
   procedure Run (Fault : Natural; Change, No_Op : Boolean := False;
                  Invalid_Case : Natural := 0;
                  Change_Byte : Natural := 16#10#;
                  Change_Mask : Unsigned_8 := 1;
                  Drop_Write : Natural := 0) is
      Data : Configuration := [others => 0];
      Step, Reads, Writes : Natural := 0;
      procedure Read (Result : out Configuration; Success : out Boolean) is
      begin
         Step := Step + 1; Reads := Reads + 1;
         -- Ordinary W1C status may change, but capability-list presence may not.
         Data (6) := Data (6) xor 8;
         Data (7) := Data (7) xor 16#80#;
         Result := Data;
         Success := Step /= Fault or Change;
         if Step = Fault and Change then
            Result (Change_Byte) := Result (Change_Byte) xor Change_Mask;
         end if;
      end Read;
      procedure Write (Offset : Natural; Value : Unsigned_16; Success : out Boolean) is
      begin
         Step := Step + 1; Writes := Writes + 1;
         pragma Assert (Offset = (case Writes is when 1 => 4,
           when 2 => 16#42#, when others => 16#62#));
         pragma Assert (Value = (case Writes is when 1 => 16#407#,
           when 2 => 0, when others => 16#4000#));
         if Writes /= Drop_Write then
            Data (Offset) := Unsigned_8 (Value and 255);
            Data (Offset + 1) := Unsigned_8 (Shift_Right (Value, 8));
         end if;
         Success := Step /= Fault; -- ambiguous AFTER-store failure
      end Write;
      package Executor is new Intel_GPU_PCI_IRQ_Disable (Read, Write);
      Status : Executor.Result;
      Saved : Natural;
      Saved_State : Executor.Phase;
      use type Executor.Result;
      use type Executor.Phase;
   begin
      Data (0 .. 3) := [16#86#, 16#80#, 16#D2#, 16#46#];
      Data (4) := 7; Data (6) := 16#10#;
      Data (16#34#) := 16#40#;
      Data (16#40#) := 5; Data (16#41#) := 16#60#; Data (16#42#) := 1;
      Data (16#60#) := 16#11#; Data (16#61#) := 16#80#; Data (16#63#) := 16#80#;
      Data (16#80#) := 1; Data (16#82#) := 3; -- PM v3/D0
      if No_Op then
         Data (5) := 4; Data (16#42#) := 0; Data (16#63#) := 16#40#;
      end if;
      case Invalid_Case is
         when 1 => Data (0) := 0;
         when 2 => Data (16#84#) := 3;
         when 3 => Data (16#60#) := 5;
         when 4 => Data (4 .. 5) := [255, 255];
         when 5 => Data (16#81#) := 16#40#;
         when others => null;
      end case;
      Executor.Execute (False, Status);
      pragma Assert (Status = Executor.Rejected and Step = 0 and Executor.State = Executor.Fresh);
      Executor.Execute (True, Status);
      if Invalid_Case /= 0 then
         pragma Assert (Step = 1 and Writes = 0 and Executor.State = Executor.Consumed_No_Writes);
         pragma Assert (Status = (if Invalid_Case in 3 .. 4 then Executor.Invalid_Plan
           else Executor.Invalid_Device));
      elsif Drop_Write /= 0 then
         pragma Assert (Writes = Drop_Write and Step = 2 * Drop_Write + 2);
         pragma Assert (Executor.State = Executor.Uncertain);
         pragma Assert (Status = (if Drop_Write = 3 then Executor.Verification_Failed
           else Executor.Configuration_Changed));
      elsif Fault = 0 then
         pragma Assert (Status = Executor.Complete and Executor.State = Executor.PCI_Disabled);
         pragma Assert (Reads = (if No_Op then 2 else 5) and Writes = (if No_Op then 0 else 3));
      else
         pragma Assert (Step = Fault and Status /= Executor.Complete);
         pragma Assert (Executor.State =
           (if Fault < 3 then Executor.Consumed_No_Writes else Executor.Uncertain));
         if Change then
            pragma Assert (Status = (if Fault = 8 then Executor.Verification_Failed
              else Executor.Configuration_Changed));
         else
            pragma Assert (Status = (if Fault in 3 | 5 | 7 then Executor.Write_Failed
              else Executor.Read_Failed));
         end if;
      end if;
      Saved := Step; Saved_State := Executor.State;
      Executor.Execute (True, Status);
      pragma Assert (Status = Executor.Rejected and Step = Saved and Executor.State = Saved_State);
   end Run;
   Changes : constant array (1 .. 4) of Natural := [2, 4, 6, 8];
begin
   for Fault in 0 .. 8 loop Run (Fault); end loop;
   for Fault of Changes loop Run (Fault, Change => True); end loop;
   -- Every nonvolatile bit of the snapshot is a baseline constraint, not just
   -- BAR0. In particular the capability-list-presence bit remains constrained
   -- even though the rest of the status word is allowed to change.
   for Fault of Changes loop
      for Byte in Configuration'Range loop
         for Bit in 0 .. 7 loop
            if Byte not in 6 .. 7 or else (Byte = 6 and Bit = 4) then
               Run (Fault, Change => True, Change_Byte => Byte,
                    Change_Mask => Shift_Left (Unsigned_8 (1), Bit));
            end if;
         end loop;
      end loop;
   end loop;
   Run (0, No_Op => True);
   -- Successful transport is not successful register programming. A dropped
   -- store must be caught before another write, or at final verification.
   for Write_Index in 1 .. 3 loop Run (0, Drop_Write => Write_Index); end loop;
   for Invalid_Case in 1 .. 5 loop Run (0, Invalid_Case => Invalid_Case); end loop;
   Ada.Text_IO.Put_Line ("PCI IRQ executor PASS: fresh snapshots, ordered word writes, every callback fault, 8132 baseline bit mutations, 3 dropped writes, no-op and reuse");
end PCI_IRQ_Executor_Tests;
