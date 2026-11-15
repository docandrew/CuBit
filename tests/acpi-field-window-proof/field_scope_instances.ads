pragma Ada_2022;
with ACPI_Local_Field_Scope;
with Field_Scope_Model;
package Field_Scope_Instances with SPARK_Mode is
   package M renames Field_Scope_Model;
   package Depth_Four is new ACPI_Local_Field_Scope
     (M.Lease,M.Admission,M.Due,M.Window,M.Acquire,M.Validate,M.Release,4,M.Inspect_Time);
   package Depth_Sixteen is new ACPI_Local_Field_Scope
     (M.Lease,M.Admission,M.Due,M.Window,M.Acquire,M.Validate,M.Release,16,M.Inspect_Time);
end Field_Scope_Instances;
