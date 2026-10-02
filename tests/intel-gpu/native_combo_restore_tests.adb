with Ada.Text_IO; with Interfaces; with Interfaces.C; with System;
with System.Storage_Elements;
with Intel_GPU_Native_Combo_Restore;
procedure Native_Combo_Restore_Tests is
   use Interfaces; use System; use System.Storage_Elements;
   use type Interfaces.C.int;
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
     Protection, Flags, FD : Interfaces.C.int; Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t) return Interfaces.C.int
     with Import, Convention => C, External_Name => "munmap";
   type Pair is record Writable, Readable : Integer_Address; end record;
   Registers : constant array (1 .. 17) of Pair :=
     ((16#61500C00#,16#60064C00#),(16#615016A0#,16#601628A0#),
      (16#61501604#,16#60162804#),(16#61501104#,16#60162104#),
      (16#61501124#,16#60162124#),(16#61501128#,16#60162128#),
      (16#61501120#,16#60162120#),(16#61501100#,16#60162100#),
      (16#61501014#,16#60162014#),(16#61500C04#,16#60064C04#),
      (16#615026A0#,16#6006C8A0#),(16#61502604#,16#6006C804#),
      (16#61502104#,16#6006C104#),(16#61502124#,16#6006C124#),
      (16#61502128#,16#6006C128#),(16#61502100#,16#6006C100#),
      (16#61502014#,16#6006C014#));
   function Load (Address : Integer_Address) return Unsigned_32 is
      V : Unsigned_32 with Import, Volatile_Full_Access, Address => To_Address (Address);
   begin return V; end Load;
   procedure Store (Address : Integer_Address; Value : Unsigned_32) is
      V : Unsigned_32 with Import, Volatile_Full_Access, Address => To_Address (Address);
   begin V := Value; end Store;
   Poison : constant Unsigned_32 := 16#F00DBAAD#;
   procedure Test (Mode : Natural) is
      Written : Natural := 0;
      After_A_Writes : Natural := 0;
      function Power return Boolean is
      begin
         -- Emulate device read aliases / group broadcasts, not hardware timing.
         if Mode <= 2 or Mode in 6 .. 8 then
            for I in Registers'Range loop
               if Load (Registers (I).Writable) /= Poison then
                  pragma Assert (I = Written + 1);
                  Written := Written + 1;
                  if Mode /= 1 then Store (Registers (I).Readable, Load (Registers (I).Writable)); end if;
                  Store (Registers (I).Writable, Poison);
               end if;
            end loop;
         end if;
         if Mode = 6 and Written = 9 then Store (16#6016210C#, Unsigned_32'Last); end if;
         if Mode in 7 .. 8 and Written = 9 then
            After_A_Writes := After_A_Writes + 1;
            -- At this callback the second verification sample begins.
            if After_A_Writes = 16 then
               if Mode = 7 then
                  -- Voltage selector change must still invalidate the plan.
                  Store (16#6016210C#, 16#01000000#);
               else
                  -- Unowned low-byte movement must not reject COMP_INIT.
                  Store (16#60162100#, 16#80005F23#);
               end if;
            elsif Mode = 8 and After_A_Writes = 1 then
               Store (16#60162100#, 16#80005F24#);
            end if;
         end if;
         return Mode /= 3 and then not (Mode = 2 and Written > 0);
      end Power;
      function DC return Boolean is (Mode /= 4);
      function Pages return Boolean is (Mode /= 5);
      package Native is new Intel_GPU_Native_Combo_Restore (Power, DC, Pages);
   begin
      pragma Assert (Native.Diagnostic = "PHY=not-run");
      if Mode <= 2 or Mode in 6 .. 8 then
         for R of Registers loop Store (R.Writable, Poison); Store (R.Readable, 0); end loop;
         Store (16#6016210C#, 0); Store (16#6006C10C#, 0);
      end if;
      declare Result : constant String := Native.Execute (True); begin
         if Mode = 0 or Mode = 8 then
            pragma Assert (Result = "PHY=B result=ready writes= 17" and Native.Last_Succeeded and Written = 17);
            pragma Assert (Load (16#601628A0#) = 16#A0000000#);
            pragma Assert (Load (16#6006C124#) = 16#62AB67BB#);
         else
            pragma Assert (not Native.Last_Succeeded);
            pragma Assert (Result = (case Mode is
               when 1 => "PHY=A result=verification-failed writes= 9",
               when 2 => "PHY=A result=power-lost writes= 1",
               when 6 => "PHY=A result=read-failed writes= 9 sample=invalid-MMIO reads= 9 pass= 1 reg=0016210C first=FFFFFFFF second=FFFFFFFF",
               when 7 => "PHY=A result=read-failed writes= 9 sample=changing reads= 20 pass= 2 reg=0016210C first=00000000 second=01000000",
               when others => "PHY=A result=rejected writes= 0"));
            pragma Assert (Written = (if Mode = 1 or Mode in 6 .. 7 then 9 elsif Mode = 2 then 1 else 0));
         end if;
      end;
      pragma Assert (Native.Execute (True) = "PHY=A result=rejected writes= 0");
   end Test;
   procedure Reserve (Base : Integer_Address; Size : Interfaces.C.size_t) is
      Mapping : constant System.Address := Mmap (To_Address (Base), Size, 3, 16#100022#, -1, 0);
   begin
      if Mapping /= To_Address (Base) then
         if Mapping /= To_Address (Integer_Address'Last) then
            declare Ignored : constant Interfaces.C.int := Munmap (Mapping, Size); begin null; end;
         end if;
         raise Program_Error with "cannot reserve native PHY fixture pages";
      end if;
   end Reserve;
begin
   for Mode in 3 .. 5 loop Test (Mode); end loop; -- no mapped pages yet
   Reserve (16#60064000#, 16#FF000#); Reserve (16#61500000#, 16#3000#);
   for Mode in 0 .. 2 loop Test (Mode); end loop;
   Test (6);
   Test (7);
   Test (8);
   pragma Assert (Munmap (To_Address (16#60064000#), 16#FF000#) = 0);
   pragma Assert (Munmap (To_Address (16#61500000#), 16#3000#) = 0);
   Ada.Text_IO.Put_Line ("native PHY restore PASS: actual stores, alias model, gate failures, readback failure, no replay");
end Native_Combo_Restore_Tests;
