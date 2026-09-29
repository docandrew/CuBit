with Intel_GPU_ADLN_Regset;
with Intel_GPU_ADS_Regset;
package body Intel_GPU_ADS_Register_Image with SPARK_Mode is
   use Intel_GPU_ADLN_Inventory;
   function Build (Description : Inventory;
                   Steering : Intel_GPU_ADLN_Steering.Topology;
                   MMIO_Bytes : Unsigned_32;
                   Register_GPU_Base : Unsigned_64) return Register_Image is
      Result : Register_Image;
      Required : Unsigned_64 := 0;
      Limit : constant Unsigned_64 := 16#FEE0_0000#;
      List : Intel_GPU_ADLN_Regset.Register_Set;
      Descriptor : Natural range 0 .. 4095;
      Address_Value : Unsigned_32;
      Length : Natural;
      function Class_ID (E : Engine) return Natural is
        (case E is when Render => 0, when Copy => 3,
                   when Video_0 | Video_2 => 1, when Enhance_0 => 2);
   begin
      if not Description.Valid or else not Description.Engines (Render) or else
        not Description.Engines (Copy) or else not Steering.Valid
      then return Result; end if;
      for E in Engine loop
         if Description.Engines (E) then
            Required := Required + (if E = Render then 63 * 16 else 55 * 16);
         end if;
      end loop;
      if Register_GPU_Base = 0 or else Register_GPU_Base mod 4 /= 0 or else
        Register_GPU_Base >= Limit or else Required > Limit - Register_GPU_Base
      then return Result; end if;
      for E in Engine loop
         if Description.Engines (E) then
            List := Intel_GPU_ADLN_Regset.Build (Description, E, Steering, MMIO_Bytes);
            Length := List.Registers.Count * 16;
            if not List.Ready or else Length > Capacity - Result.Used or else
              Unsigned_64 (Result.Used) > Limit - Register_GPU_Base or else
              Unsigned_64 (Length) > Limit - Register_GPU_Base - Unsigned_64 (Result.Used)
            then
               return (others => <>);
            end if;
            -- ADS descriptors use PHYSICAL engine instance, unlike system-info
            -- mapping-table logical indices. VCS2 belongs in [video][2].
            Descriptor := (Class_ID (E) * 32 + (if E = Video_2 then 2 else 0)) * 8;
            Address_Value := Unsigned_32 (Register_GPU_Base + Unsigned_64 (Result.Used));
            for Byte in 0 .. 3 loop
               Result.Descriptors (Descriptor + Byte) :=
                 Unsigned_8 (Shift_Right (Address_Value, Byte * 8) and 255);
            end loop;
            Result.Descriptors (Descriptor + 4) := Unsigned_8 (List.Registers.Count mod 256);
            Result.Descriptors (Descriptor + 5) := Unsigned_8 (List.Registers.Count / 256);
            for I in 1 .. List.Registers.Count loop
               declare
                  Wire : constant Intel_GPU_ADS_Regset.Wire_Entry :=
                    Intel_GPU_ADS_Regset.Encode (List.Registers.Entries (I));
               begin
                  for Byte in Wire'Range loop
                     Result.Registers (Result.Used + (I - 1) * 16 + Byte) := Wire (Byte);
                  end loop;
               end;
            end loop;
            Result.Used := Result.Used + Length;
         end if;
      end loop;
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADS_Register_Image;
