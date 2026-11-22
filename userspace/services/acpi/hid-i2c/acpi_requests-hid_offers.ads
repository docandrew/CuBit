pragma Ada_2022;
with ACPI_HID_I2C_Candidate;
with ACPI_HID_I2C_Discovery;
with ACPI_HID_I2C_Resolved;
with ACPI_HID_Offer_Builder;
with ACPI_HID_Offer_Provenance;
with ACPI_PCI_Observation_Reader;
with ACPI_Requests.HID_Devices;
with ACPI_Requests.PCI_Observations;
with GPIO_Bootstrap_Payload;
generic
   with package Candidates is new ACPI_HID_I2C_Candidate (<>);
   with package Discovery is new ACPI_HID_I2C_Discovery
     (Core => ACPI_Service, others => <>);
   with package Resolver is new ACPI_HID_I2C_Resolved (ACPI_Service, Candidates);
   with package Builder is new ACPI_HID_Offer_Builder (Candidates);
   with package Provenance is new ACPI_HID_Offer_Provenance
     (ACPI_Service, Candidates, Resolver, Builder);
   with package Reader is new ACPI_PCI_Observation_Reader
     (Core => ACPI_Service, others => <>);
package ACPI_Requests.HID_Offers with SPARK_Mode is
   Maximum_Candidates : constant := 16;
   subtype Candidate_Count is Natural range 0 .. Maximum_Candidates;
   subtype Candidate_Index is Positive range 1 .. Maximum_Candidates;
   type Device_Array is array (Candidate_Index) of ACPI_Service.Node_Descriptor;
   type Payload_Array is array (Candidate_Index) of Builder.Wire.Request;
   package HID is new ACPI_Requests.HID_Devices (Discovery, Resolver);
   type Outcome is (Not_Attempted, Collecting, Prepared, Owner_Busy,
      Stale_Device, Duplicate_Device, Probe_Refused, Probe_Failed,
      Sources_Failed, Device_Unavailable, Policy_Unresolved, Capacity_Refused, Budget_Exhausted, Topology_Refused,
      Topology_Unresolved, Description_Refused, Recovery_Required);
   Maximum_Captured_Errors : constant Positive := 8;
   Maximum_Path_Segments : constant Positive := 32;
   Segment_Characters : constant Positive := 4;
   subtype Error_Count is Natural range 0 .. Maximum_Captured_Errors;
   subtype Error_Index is Positive range 1 .. Maximum_Captured_Errors;
   subtype Error_Path is String (1 .. Maximum_Path_Segments * (Segment_Characters + 1));
   type Error_Kind is (Policy_Error, Firmware_Error, Source_Error, Topology_Error, Description_Error);
   function Error_Name (Kind : Error_Kind) return String with Global => null;
   type Error_Observation is record
      Kind : Error_Kind := Policy_Error;
      Input_Index : Candidate_Index := Candidate_Index'First;
      Device : ACPI_Service.Node_Descriptor := ACPI_Service.Values.No_Node;
      Path : Error_Path := [others => ' '];
      Path_Complete : Boolean := False;
      Charged : Natural := 0;
      Policy : HID.Controller.Policy_Report;
      Firmware : Discovery.Report;
      Source_Status : Resolver.Outcome := Resolver.Invalid_Device;
      Topology : Reader.Topology_Report;
      Description_Status : Builder.Outcome := Builder.Unsupported_Candidate;
      Provenance_Status : Provenance.Outcome := Provenance.Stale_Observation;
   end record;
   type Error_Array is array (Error_Index) of Error_Observation;
   type Report is record
      Status : Outcome := Not_Attempted;
      Attempted : Boolean := False;
      Working : Boolean := False;
      Recovery_Needed : Boolean := False;
      Charged : Natural := 0;
      Collected, Count : Candidate_Count := 0;
      Unavailable, Node_Errors : Candidate_Count := 0;
      Captured_Errors : Error_Count := 0;
      Omitted_Errors : Candidate_Count := 0;
      Errors : Error_Array;
      Last_Policy : HID.Controller.Policy_Report;
      Status_Known : Boolean := False;
      Status_Flags : Builder.Wire.Word := 0;
      Values : Payload_Array := [others => Builder.Wire.Empty_Request];
   end record;
   type Run is limited private;
   function Observe (Item : Run) return Report;
   -- One serialized, process-lived owner and one call per Run. The caller
   -- opens the SAME runtime's finite event window and supplies the exact
   -- previously submitted/admitted GPIO facts and candidate incarnations.
   -- All probes and topology queries share Budget; each _CRS is copied once,
   -- without AML sizing retries. All candidates finish before ANY IPC.
   -- Retain Run and Core on Recovery_Needed; no reset, replay or more AML.
   -- On refusal Count=0 and all payloads are empty, including earlier rows.
   -- Ordinary node errors and known inactive devices are counted and skipped.
   -- On success descriptions are compacted in original order. Final
   -- binding checks establish identity, not atomic resource/hardware freshness.
   -- Caller closes the event window and checks physical/provider quiescence,
   -- admission receipt and startup deadline before the separate transport.
   procedure Collect (Item : in out Run; Server : in out State;
      Devices : Device_Array; Count : Candidate_Count;
      Admission_Device : ACPI_Service.Node_Descriptor;
      Admission : GPIO_Bootstrap_Payload.Request; History : Provenance.Stamp;
      Budget : Natural; Resource_Bytes : Positive; Result : out Report)
     with Pre => Valid (Server);
   generic
      with procedure Check_Candidate
        (Service : aliased in out ACPI_Service.State;
         Item : ACPI_Service.Node_Descriptor; Budget : Natural;
         Result : out HID.Controller.Policy_Report);
   procedure Collect_With_Policy (Item : in out Run; Server : in out State;
      Devices : Device_Array; Count : Candidate_Count;
      Admission_Device : ACPI_Service.Node_Descriptor;
      Admission : GPIO_Bootstrap_Payload.Request; History : Provenance.Stamp;
      Budget : Natural; Resource_Bytes : Positive; Result : out Report)
     with Pre => Valid (Server);
private
   package PCI is new ACPI_Requests.PCI_Observations (Reader);
   type Observation_Array is array (Candidate_Index) of Resolver.Observation;
   type Inclusion_Array is array (Candidate_Index) of Boolean;
   type Run is limited record
      View : Report;
      Probe : HID.Controller.Probe;
      Observations : Observation_Array;
      Included : Inclusion_Array := [others => False];
      Last_Topology : Reader.Topology_Report;
   end record;
end ACPI_Requests.HID_Offers;
