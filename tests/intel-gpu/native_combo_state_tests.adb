with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Combo_PHY; use Intel_GPU_Combo_PHY;
with Intel_GPU_Native_Combo_State;
procedure Native_Combo_State_Tests is
   use type Interfaces.C.int;
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   Base : constant Integer_Address := 16#60064000#;
   Size : constant Interfaces.C.size_t := 16#FF000#;
   Mapping : System.Address;
   Offsets : constant array (PHY, Field) of Integer_Address :=
     (A => (16#64C00#, 16#1628A0#, 16#162804#, 16#162104#, 16#162124#,
            16#162128#, 16#162120#, 16#162100#, 16#162014#, 16#16210C#),
      B => (16#64C04#, 16#6C8A0#, 16#6C804#, 16#6C104#, 16#6C124#,
            16#6C128#, 16#6C120#, 16#6C100#, 16#6C014#, 16#6C10C#));
   procedure Store (Port : PHY; F : Field; Value : Unsigned_32) is
      Register_Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => To_Address (16#60000000# + Offsets (Port, F));
   begin Register_Value := Value; end Store;
   Calls, Drop_At : Natural := 0;
   Mutate : Boolean := False;
   Mutation_Field : Field := Misc;
   Mutation_Value : Unsigned_32 := 1000;
   Current_Port : PHY := A;
   Owned_Bit : constant array (Field) of Natural :=
     (Misc => 23, TX_8 => 31, PCS_1 => 20, Comp_1 => 16,
      Comp_9 => 0, Comp_10 => 0, Comp_8 => 24, Comp_0 => 31,
      CL_5 => 4, Comp_3 => 24);
   function Held return Boolean is
   begin
      Calls := Calls + 1;
      if Mutate and Calls = 12 then Store (Current_Port, Mutation_Field, Mutation_Value); end if;
      return Drop_At = 0 or else Calls < Drop_At;
   end Held;
   package Native is new Intel_GPU_Native_Combo_State (Held);
   use Native;
   R : Observation;
   Expected : array (PHY) of Snapshot;
   procedure Reset is
   begin
      Calls := 0; Drop_At := 0; Mutate := False;
      for Port in PHY loop
         for F in Field loop
            Expected (Port) (F) := Unsigned_32 (100 * PHY'Pos (Port) + Field'Pos (F) + 1);
            Store (Port, F, Expected (Port) (F));
         end loop;
      end loop;
   end Reset;
begin
   R := Capture (False, A);
   pragma Assert (R.Status = Rejected and Calls = 0);
   Drop_At := 1; R := Capture (True, A);
   pragma Assert (R.Status = Rejected and R.Reads = 0);
   Mapping := Mmap (To_Address (Base), Size, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (Base) then
      if Mapping /= To_Address (Integer_Address'Last) then
         declare Ignored : constant Interfaces.C.int := Munmap (Mapping, Size); begin null; end;
      end if;
      raise Program_Error with "cannot reserve PHY fixture pages";
   end if;
   for Port in PHY loop
      Current_Port := Port;
      Reset; R := Capture (True, Port);
      pragma Assert (R.Status = Collected and R.Reads = 20 and R.Values = Expected (Port));
      for F in Field loop
         Reset; Store (Port, F, Unsigned_32'Last); R := Capture (True, Port);
         pragma Assert (R.Status = Read_Failed and R.Reads = Field'Pos (F));
         pragma Assert (R.Reason = Invalid_MMIO and R.Field_Known and
           R.Offset = Unsigned_32 (Offsets (Port, F)) and R.Failed_Pass = 1 and
           R.First_Value = Unsigned_32'Last and R.Second_Value = Unsigned_32'Last);
         pragma Assert (R.Values = Snapshot'(others => 0));
         Reset; R := Capture (True, Port); pragma Assert (R.Status = Collected);
         Reset; Mutate := True; Mutation_Field := F; Mutation_Value := Unsigned_32'Last;
         R := Capture (True, Port);
         pragma Assert (R.Reason = Invalid_MMIO and R.Failed_Pass = 2 and
           R.Reads = 10 + Field'Pos (F) and R.Field_Known and
           R.Offset = Unsigned_32 (Offsets (Port, F)) and
           R.First_Value = Expected (Port) (F) and R.Second_Value = Unsigned_32'Last);
         Reset; Mutate := True; Mutation_Field := F;
         Mutation_Value := Expected (Port) (F) xor Shift_Left (Unsigned_32'(1), Owned_Bit (F));
         R := Capture (True, Port);
         pragma Assert (R.Status = Changing and R.Reason = Unstable and
           R.Reads = 20 and R.Field_Known and R.Failed_Pass = 2 and
           R.Offset = Unsigned_32 (Offsets (Port, F)) and
           R.First_Value = Expected (Port) (F) and R.Second_Value = Mutation_Value);
      end loop;
      for Drop in 2 .. 22 loop
         Reset; Drop_At := Drop; R := Capture (True, Port);
         pragma Assert (R.Status = Read_Failed and R.Reads = Drop - 2);
         pragma Assert (R.Reason = Power_Lost and not R.Field_Known);
         pragma Assert (R.Values = Snapshot'(others => 0));
      end loop;
      -- Real N95 observation: unrelated low-byte movement must not masquerade
      -- as a change to COMP_INIT. Return the newest complete sample.
      Reset; Store (Port, Comp_0, 16#8000_5F24#);
      Mutate := True; Mutation_Field := Comp_0; Mutation_Value := 16#8000_5F23#;
      R := Capture (True, Port);
      pragma Assert (R.Status = Collected and R.Reads = 20 and
        R.Values (Comp_0) = 16#8000_5F23#);
      Reset; R := Capture (True, Port); pragma Assert (R.Status = Collected);
   end loop;
   pragma Assert (Munmap (Mapping, Size) = 0);
   Ada.Text_IO.Put_Line ("native PHY snapshots PASS: A/B actual loads, sentinel/power loss, changing samples (hosted)");
end Native_Combo_State_Tests;
