pragma Ada_2022;
package body ACPI_SCI_Quiesce with SPARK_Mode is
   function Fixed_Mask_Of (Events : R.Fixed_Set) return Unsigned_16 is
      Mask : Unsigned_16 := 0;
   begin
      for E in R.Fixed_Event loop
         if Events (E) then
            Mask := Mask or R.Fixed_Mask (E);
         end if;
         pragma Loop_Invariant
           (for all F in R.Fixed_Event'First .. E =>
              (if Events (F) then (Mask and R.Fixed_Mask (F)) /= 0));
      end loop;
      return Mask;
   end Fixed_Mask_Of;
end ACPI_SCI_Quiesce;
