with Interfaces; with Producer_Model;
package ACPI_Requests is
   type Revision_Number is new Interfaces.Unsigned_64;
   type State is record
      Version : Revision_Number := 16#FFFF_FFFF_0000_0001#;
      Busy : Boolean := False;
   end record;
   function Retained_Results_Pending (Server : State) return Boolean is (Producer_Model.Pending or else Server.Busy);
   function Revision (Server : State) return Revision_Number is (Server.Version);
end ACPI_Requests;
