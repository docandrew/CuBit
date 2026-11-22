with Interfaces; use Interfaces;
package Region_PTE with Pure, SPARK_Mode => On is
   type Access_Mode is (Inaccessible, Read_Only, Read_Write, Read_Execute);
   Frame_Mask : constant Unsigned_64 := 16#0000_FFFF_FFFF_F000#;
   Change_Mask : constant Unsigned_64 := 16#8000_0000_0000_0003#;
   type Decision is record
      Allowed : Boolean;
      Value : Unsigned_64;
   end record;
   -- Expected physical page address, not PFN. Only normal user RAM leaves;
   -- caller establishes exclusive physical ownership separately.
   function Plan (Old, Expected_Address : Unsigned_64; Mode : Access_Mode)
     return Decision
   with Post =>
     (if not Plan'Result.Allowed then Plan'Result.Value = Old
      else (Plan'Result.Value and not Change_Mask) = (Old and not Change_Mask)
        and then not ((Plan'Result.Value and 2) /= 0 and
                      (Plan'Result.Value and 16#8000_0000_0000_0000#) = 0));
end Region_PTE;
