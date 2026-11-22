with ACPI_Service;
package ACPI_Native_HID_Detection is
   Maximum_Candidates : constant := 16;
   Maximum_AML_Fuel : constant := 100_000;
   Maximum_Resource_Bytes : constant := 65_536;
   type Outcome is (Not_Attempted, No_Devices, Finished, Candidate_Limit);
   subtype Count is Natural range 0 .. Maximum_Candidates;
   type Observation is record Device : ACPI_Service.Node_Descriptor := 0; end record;
   type Observation_Array is array (Positive range 1 .. Maximum_Candidates) of Observation;
   type Report is record
      Status : Outcome := Not_Attempted;
      Candidates, Observed : Count := 0;
      Failed_Candidates : Count := 0;
      Node_Errors : Natural := 0;
      Cleanup_Required, Recovery_Needed : Boolean := False;
      Items : Observation_Array;
   end record;
end ACPI_Native_HID_Detection;
