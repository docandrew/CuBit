with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_DC_State; use Intel_GPU_DC_State;
with Intel_GPU_Native_DC_State;
procedure Native_DC_State_Tests is
   use type Interfaces.C.int;
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   Base : constant Integer_Address := 16#60044000#;
   Mapping : System.Address;
   -- Independent literal addresses catch an incorrect selector in the driver.
   Offsets : constant array (Field) of Integer_Address :=
     [16#46000#, 16#46070#, 16#51004#, 16#45008#, 16#44FE8#, 16#44300#, 16#44304#];
   procedure Store (F : Field; Value : Unsigned_32) is
      Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (16#60000000# + Offsets (F));
   begin Register_Value := Value; end Store;
   Calls, Drop_At : Natural := 0;
   Mutate : Boolean := False;
   function Held return Boolean is
   begin
      Calls := Calls + 1;
      if Mutate and Calls = 9 then Store (Clock_Control, 100); end if;
      return Drop_At = 0 or else Calls < Drop_At;
   end Held;
   package Native is new Intel_GPU_Native_DC_State (Held);
   use Native;
   R : Observation;
   Expected : Snapshot;
   procedure Reset is
   begin
      Calls := 0; Drop_At := 0; Mutate := False;
      for F in Field loop
         Expected (F) := Unsigned_32 (Field'Pos (F) + 1);
         Store (F, Expected (F));
      end loop;
   end Reset;
begin
   R := Capture (False);
   pragma Assert (R.Status = Rejected and Calls = 0);
   Drop_At := 1; R := Capture (True);
   pragma Assert (R.Status = Rejected and R.Reads = 0);
   Mapping := Mmap (To_Address (Base), 16#10000#, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (Base) then
      if Mapping /= To_Address (Integer_Address'Last) then
         declare Ignored : constant Interfaces.C.int := Munmap (Mapping, 16#10000#); begin null; end;
      end if;
      raise Program_Error with "cannot reserve DC fixture pages";
   end if;
   Reset; R := Capture (True);
   pragma Assert (R.Status = Collected and R.Reads = 14 and R.Values = Expected);
   for F in Field loop
      Reset; Store (F, Unsigned_32'Last); R := Capture (True);
      pragma Assert (R.Status = Read_Failed and R.Reads = Field'Pos (F));
      pragma Assert (R.Values = Snapshot'(others => 0));
      Reset; R := Capture (True); pragma Assert (R.Status = Collected);
   end loop;
   for Drop in 2 .. 16 loop
      Reset; Drop_At := Drop; R := Capture (True);
      pragma Assert (R.Status = Read_Failed and R.Reads = Drop - 2);
      pragma Assert (R.Values = Snapshot'(others => 0));
   end loop;
   Reset; Mutate := True; R := Capture (True);
   pragma Assert (R.Status = Changing and R.Reads = 14 and R.Values = Snapshot'(others => 0));
   Reset; R := Capture (True); pragma Assert (R.Status = Collected);
   pragma Assert (Munmap (Mapping, 16#10000#) = 0);
   Ada.Text_IO.Put_Line ("native DC snapshot PASS: actual loads, each sentinel/power loss, changing samples (hosted)");
end Native_DC_State_Tests;
