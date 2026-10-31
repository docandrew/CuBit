with ACPI_Service_Core;
with ACPI_HID_I2C_Candidate;
with ACPI_Controller_Source;
with Firmware_Tables;
with HID_I2C_Descriptors;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Candidates is new ACPI_HID_I2C_Candidate (<>);
package ACPI_HID_I2C_Resolved with SPARK_Mode is
   package Sources is new ACPI_Controller_Source (Core);
   type Observation is private;
   Empty_Observation : constant Observation;
   function Device (Item : Observation) return Core.Node_Descriptor;
   function I2C_Controller (Item : Observation) return Core.Node_Descriptor;
   function GPIO_Controller (Item : Observation) return Core.Node_Descriptor;
   function Facts (Item : Observation) return Candidates.Candidate;
   -- Descriptive freshness only. Does not establish _STA/_DEP, initialization,
   -- transport readiness or hardware capability authority. Must be called in
   -- the same serialized owning Core before consuming bindings after AML.
   function Current (Service : Core.State; Item : Observation) return Boolean
      with Pre => Core.Valid (Service);
   type Outcome is (Resolved, Invalid_Device, Candidate_Failed,
                    I2C_Source_Failed, GPIO_Source_Failed);
   type Report is record
      Status : Outcome := Invalid_Device;
      Copy : Candidates.Report;
      I2C, GPIO : Sources.Report;
      Value : Observation := Empty_Observation;
   end record;
   -- Fixed local owner composition: current device + exact copied _CRS +
   -- trusted descriptor-register observation. Own bytes, then resolve BOTH
   -- sources in that original context without executing AML. No partially
   -- resolved payload escapes. Reports preserve per-stage diagnostics.
   procedure Resolve (Service : Core.State; Device : Core.Node_Descriptor;
      Data : Firmware_Tables.Bytes;
      Descriptor_Register : HID_I2C_Descriptors.Register_Number;
      Result : out Report;
      Maximum_Items : Candidates.R.Item_Count := Candidates.R.Default_Item_Limit)
      with Pre => Core.Valid (Service),
        Post => (if Result.Status /= Resolved then Result.Value = Empty_Observation);
private
   type Observation is record
      Device_Node, I2C_Node, GPIO_Node : Core.Node_Descriptor := Core.Values.No_Node;
      Data : Candidates.Candidate := Candidates.Empty_Candidate;
   end record;
   Empty_Observation : constant Observation := (others => <>);
   function Device (Item : Observation) return Core.Node_Descriptor is (Item.Device_Node);
   function I2C_Controller (Item : Observation) return Core.Node_Descriptor is (Item.I2C_Node);
   function GPIO_Controller (Item : Observation) return Core.Node_Descriptor is (Item.GPIO_Node);
   function Facts (Item : Observation) return Candidates.Candidate is (Item.Data);
end ACPI_HID_I2C_Resolved;
