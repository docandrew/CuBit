with ACPI_Service_Core;
with Firmware_Tables;
with AML_Names;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Controller_Source with SPARK_Mode is
   type Outcome is (Resolved, Invalid_Context, Bad_Source, Above_Root,
      Not_Found, Lookup_Failed, Not_Device);
   type Report is record
      Status : Outcome := Invalid_Context;
      Parsing : AML_Names.Parse_Status := AML_Names.Truncated;
      Access_Result : Core.Values.Access_Status := Core.Values.Available;
      Target : Core.Node_Descriptor := Core.Values.No_Node;
   end record;
   -- Trusted owner selection in the original device's namespace lifetime.
   -- No AML evaluation, side effects, IPC, endpoint selection or authority.
   -- The first visible unprefixed single name shadows ancestors even if it
   -- has the wrong kind. Prefixed/multipart paths do not search ancestors.
   -- Canonical lookup follows authenticated aliases. Success describes a
   -- current Device only; the controller registry must separately admit it.
   procedure Resolve (Service : Core.State; Context : Core.Node_Descriptor;
      Source : Firmware_Tables.Bytes; Result : out Report)
      with Pre => Core.Valid (Service);
end ACPI_Controller_Source;
