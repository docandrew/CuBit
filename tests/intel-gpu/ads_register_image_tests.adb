with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Inventory; use Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADS_Register_Image; use Intel_GPU_ADS_Register_Image;
with Intel_GPU_ADLN_Regset;
with Intel_GPU_ADS_Regset;
procedure ADS_Register_Image_Tests is
   Description : Inventory;
   Steering : constant Intel_GPU_ADLN_Steering.Topology :=
     Intel_GPU_ADLN_Steering.Decode (1, 4, 0);
   Result : Register_Image;
   Expected_Descriptors : Descriptor_Bytes;
   Cursor, D, Class_Number, Physical : Natural;
   List : Intel_GPU_ADLN_Regset.Register_Set;
   Base : constant Unsigned_64 := 16#200000# + 21692;
   Address_Value : Unsigned_32;
begin
   for Disabled in Unsigned_32 range 0 .. 7 loop
      Description := Decode (16#8086#, 16#46D2#,
        (Disabled and 1) or Shift_Left (Disabled and 2, 1) or Shift_Left (Disabled and 4, 14));
      Result := Build (Description, Steering, 16#200000#, Base);
      pragma Assert (Result.Valid);
      Expected_Descriptors := [others => 0];
      Cursor := 0;
      for E in Engine loop
         if Description.Engines (E) then
            Class_Number := (case E is when Render => 0, when Copy => 3,
                               when Video_0 | Video_2 => 1, when Enhance_0 => 2);
            Physical := (if E = Video_2 then 2 else 0);
            D := Class_Number * 256 + Physical * 8;
            List := Intel_GPU_ADLN_Regset.Build (Description, E, Steering, 16#200000#);
            Address_Value := Unsigned_32 (Base) + Unsigned_32 (Cursor);
            for B in 0 .. 3 loop
               Expected_Descriptors (D + B) := Unsigned_8 ((Address_Value / 256 ** B) mod 256);
            end loop;
            Expected_Descriptors (D + 4) := (if E = Render then 63 else 55);
            for I in 1 .. List.Registers.Count loop
               declare
                  Wire : constant Intel_GPU_ADS_Regset.Wire_Entry :=
                    Intel_GPU_ADS_Regset.Encode (List.Registers.Entries (I));
               begin
                  for B in Wire'Range loop
                     pragma Assert (Result.Registers (Cursor + B) = Wire (B));
                  end loop;
                  Cursor := Cursor + 16;
               end;
            end loop;
         end if;
      end loop;
      pragma Assert (Result.Used = Cursor and Result.Descriptors = Expected_Descriptors);
      pragma Assert ((for all B in Cursor .. Capacity - 1 => Result.Registers (B) = 0));
      pragma Assert (Build (Description, Steering, 16#200000#,
                           16#FEE00000# - Unsigned_64 (Cursor)).Valid);
      pragma Assert (not Build (Description, Steering, 16#200000#,
                               16#FEE00000# - Unsigned_64 (Cursor) + 4).Valid);
   end loop;
   pragma Assert (not Build (Description, Steering, 16#200000#, 0).Valid);
   pragma Assert (not Build (Description, Steering, 16#200000#, Base + 1).Valid);
   pragma Assert (not Build (Description, Steering, 16#200000#, Unsigned_64'Last).Valid);
   Result := Build (Description, Steering, 0, Base);
   pragma Assert (not Result.Valid and Result.Used = 0 and
                  Result.Descriptors = Descriptor_Bytes'(others => 0));
   Ada.Text_IO.Put_Line ("ADS register image: eight inventories, full payload/descriptors and address bounds PASS");
end ADS_Register_Image_Tests;
