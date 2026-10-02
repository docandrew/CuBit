with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Golden; use Intel_GPU_ADLN_Golden;
procedure ADLN_Golden_Tests is
   Description : Inventory;
   Value : Reservation;
   Expected_Bytes, Cursor : Unsigned_64;
   Expected_Address, Expected_State : Class_Values;
   Base : constant Unsigned_64 := 16#400000#;
begin
   for Disabled in Unsigned_32 range 0 .. 7 loop
      Description := Decode (16#8086#, 16#46D2#,
        (Disabled and 1) or Shift_Left (Disabled and 2, 1) or Shift_Left (Disabled and 4, 14));
      Expected_Address := [others => 0]; Expected_State := [others => 0];
      Expected_Address (0) := Unsigned_32 (Base); Expected_State (0) := 52928;
      Expected_Address (3) := Unsigned_32 (Base + 57344); Expected_State (3) := 3776;
      Cursor := Base + 65536;
      if (Disabled and 3) /= 3 then
         Expected_Address (1) := Unsigned_32 (Cursor); Expected_State (1) := 3776;
         Cursor := Cursor + 8192;
      end if;
      if (Disabled and 4) = 0 then
         Expected_Address (2) := Unsigned_32 (Cursor); Expected_State (2) := 3776;
         Cursor := Cursor + 8192;
      end if;
      Expected_Bytes := Cursor - Base;
      pragma Assert (Required_Bytes (Description) = Expected_Bytes);
      Value := Plan (Description, Base, Expected_Bytes);
      pragma Assert (Value.Valid and Value.Bytes = Expected_Bytes and
                     Value.Addresses = Expected_Address and Value.State_Bytes = Expected_State);
      pragma Assert (not Plan (Description, Base, Expected_Bytes - 1).Valid);
      pragma Assert (Plan (Description, 16#FEE00000# - Expected_Bytes, Expected_Bytes).Valid);
      pragma Assert (not Plan (Description, 16#FEE00000# - Expected_Bytes + 4096, Expected_Bytes).Valid);
   end loop;
   pragma Assert (not Plan (Description, 0, Unsigned_64'Last).Valid);
   pragma Assert (not Plan (Description, Base + 1, Unsigned_64'Last).Valid);
   pragma Assert (not Plan (Description, Unsigned_64'Last, Unsigned_64'Last).Valid);
   Description.Valid := False;
   pragma Assert (Required_Bytes (Description) = 0 and
                  not Plan (Description, Base, Unsigned_64'Last).Valid);
   Ada.Text_IO.Put_Line ("ADL-N golden reservations: eight inventories, per-class sizes and boundaries PASS");
end ADLN_Golden_Tests;
