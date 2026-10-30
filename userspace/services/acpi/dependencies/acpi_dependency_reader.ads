pragma Ada_2022;
with ACPI_Service_Core;
generic
   with package Core is new ACPI_Service_Core (<>);
package ACPI_Dependency_Reader with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   type Element_Count is range 0 .. Natural'Last;
   type Device_Array is array (Natural range <>) of Core.Node_Descriptor;
   type Outcome is (Not_Read, Copied_List, Invalid_Result, Not_A_Package,
      Capacity_Required, Work_Limit, Read_Failed, Unsupported_Element, Cleanup_Failed);
   type Report is record
      Status : Outcome := Not_Read;
      Required, Copied : Element_Count := 0;
      Size_Known : Boolean := False;
      Access_Result, Cleanup_Result : Core.Values.Access_Status := Core.Values.Available;
   end record;
   use type Core.Node_Descriptor;
   -- Borrows an existing retained Package, never evaluates AML or consumes the
   -- caller's root. Retry copy/quota sizing with that SAME retained result.
   -- Only live authenticated Device metadata is accepted; no string coercion,
   -- arbitrary reference dereference or recursive dependency traversal.
   procedure Copy
     (Service : in out Core.State; Handle : Core.Values.Value_Handle;
      Maximum_Elements : Element_Count; Items : out Device_Array; Result : out Report)
     with Pre => Core.Valid (Service), Post => Core.Valid (Service)
       and then Natural (Result.Copied) <= Items'Length
       and then (if Result.Status = Copied_List then
         Result.Size_Known and then Result.Copied = Result.Required and then Result.Copied <= Maximum_Elements
       else Result.Copied = 0 and then (for all Item of Items => Item = Core.Values.No_Node))
       and then (Result.Status = Cleanup_Failed
         or else Core.Retained_Results (Service) = Core.Retained_Results (Service)'Old);
end ACPI_Dependency_Reader;
