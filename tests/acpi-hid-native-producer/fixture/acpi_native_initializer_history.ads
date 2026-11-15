with Interfaces;
package ACPI_Native_Initializer_History is
   type Report is record
      Invocations : Interfaces.Unsigned_32 := 17;
      Ordered_Digest : Interfaces.Unsigned_64 := 16#F111_2222_3333_4444#;
      Complete : Boolean := True;
   end record;
   function Observe return Report;
end ACPI_Native_Initializer_History;
