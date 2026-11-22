pragma Ada_2022;
package body ACPI_FADT.Sleep_Registers with SPARK_Mode is
   function Describe (Item : Description) return Selection is
      R : Selection;
      A, B : Registers.Block_Result;
   begin
      R.Low_Power_Idle := Item.Revision >= ACPI_5_Revision and then (Item.Flags and Low_Power_Idle_Flag) /= 0;
      if ACPI_FADT.Hardware_Reduced (Item) then
         R.A := Registers.Hardware_Reduced; R.B := Registers.Hardware_Reduced;
         R.Control := Item.Sleep_Control;
         if not Item.Sleep_Control.Present or else Item.Sleep_Control.Value.Address = 0 then
            R.Code := Selected; return R;
         end if;
         if Item.Sleep_Control.Value.Space not in 0 | 1 | 2
           or else Item.Sleep_Control.Value.Width /= Byte_Width
           or else Item.Sleep_Control.Value.Bit_Offset /= 0
           or else Item.Sleep_Control.Value.Access_Size not in 0 | 1 then
            R.Code := Invalid_Sleep_Control; return R;
         end if;
         R.Code := Selected; R.Model := Hardware_Reduced; return R;
      end if;
      A := Registers.Describe (Item, PM1A_Control, Unsigned_64'Last);
      B := Registers.Describe (Item, PM1B_Control, Unsigned_64'Last);
      R.A := A.Code; R.B := B.Code;
      if A.Code = Registers.Described then R.Legacy_Fallback_A := A.Extended_Rejected; end if;
      if B.Code = Registers.Described then R.Legacy_Fallback_B := B.Extended_Rejected; end if;
      if A.Code not in Registers.Described | Registers.Absent
        or else B.Code not in Registers.Described | Registers.Absent then return R; end if;
      if A.Code = Registers.Absent and then B.Code = Registers.Described then
         R.Code := Inconsistent_Fixed_Blocks; return R;
      end if;
      R.Code := Selected;
      if A.Code = Registers.Described then
         R.Model := (if B.Code = Registers.Described then PM1A_And_B else PM1A_Only);
      end if;
      return R;
   end Describe;
end ACPI_FADT.Sleep_Registers;
