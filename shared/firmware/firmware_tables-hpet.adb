pragma Ada_2022;
package body Firmware_Tables.HPET with SPARK_Mode is
   function Decode (Data : Bytes; Last_Address : Address_Value) return Descriptor is
      Table : constant Table_Result := Read_Table (Data, "HPET");
      Base : Address_Value := 0;
   begin
      if Table.Status /= Accepted then return (Valid => False); end if;
      if Table.Extent < 56 or else Table.Revision /= 1 then
         return (Valid => False);
      end if;
      -- GAS: system memory, whole register, unspecified or 64-bit width.
      -- The access-size byte was reserved in the original HPET specification;
      -- accept unspecified or explicit 64-bit access, not another space.
      if Data (Data'First + 40) /= 0 or else
        Data (Data'First + 41) not in 0 | 64 or else
        Data (Data'First + 42) /= 0 or else
        Data (Data'First + 43) not in 0 | 4
      then return (Valid => False); end if;
      for I in 0 .. 7 loop
         Base := Base or Shift_Left
           (Address_Value (Data (Data'First + 44 + I)), 8 * I);
      end loop;
      if Base = 0 or else Base mod 1024 /= 0 or else
        Base > Last_Address or else Last_Address - Base < 1023
      then return (Valid => False); end if;
      return (True, Base);
   end Decode;
end Firmware_Tables.HPET;
