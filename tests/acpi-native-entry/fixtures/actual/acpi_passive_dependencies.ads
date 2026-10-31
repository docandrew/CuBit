pragma Ada_2022;
with ACPI_Service_Core;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Passive_Dependencies with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   type Element_Count is range 0 .. Natural'Last;
   type Device_Array is array (Natural range <>) of Core.Node_Descriptor;
   type Outcome is (Not_Observed, Static_List, Missing, Method_Valued,
      Unsupported, Invalid_Device, Unavailable, Capacity_Required,
      Work_Limit, Read_Failed, Cleanup_Failed);
   type Report is record
      Status : Outcome := Not_Observed;
      Required, Copied : Element_Count := 0;
      Size_Known : Boolean := False;
      Access_Result, Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
   end record;
   use type Core.Node_Descriptor;
   -- Observe only the device's canonical direct static _DEP package. No AML,
   -- Field evaluation, ancestor fallback, dependency traversal or readiness
   -- assertion occurs. Device descriptors are live descriptive identities,
   -- never grants. Order and duplicate references are preserved, including
   -- self/cyclic edges; scheduling/cycle policy and _REG readiness are separate.
   -- Quota/capacity failures return the required element count without partial
   -- edges. Retained roots are released before return. Caller serializes access
   -- and rechecks descriptor freshness before consuming these observations.
   procedure Read
     (Service : in out Core.State; Device : Core.Node_Descriptor;
      Maximum_Elements : Element_Count; Items : out Device_Array; Result : out Report)
     with Pre => Core.Valid (Service),
       Post => Core.Valid (Service)
         and then Natural (Result.Copied) <= Items'Length
         and then (if Result.Status = Static_List then
           Result.Size_Known and then Result.Copied = Result.Required
           and then Result.Copied <= Maximum_Elements
         else Result.Copied = 0 and then (for all Item of Items => Item = Core.Values.No_Node))
         and then (Result.Status = Cleanup_Failed
           or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
end ACPI_Passive_Dependencies;
