pragma Ada_2022;
with ACPI_Service;
with ACPI_Device_Policy;
with AML_Execute;
-- Process-lived registration owner on the existing Core/Fields runtime.
-- Provides no hardware grants and logs nothing from inside region AML.
package ACPI_Native_Regions is
   type Report is record
      Attempted, Connected, Announced, Pending, Working : Boolean := False;
      Regions, Unavailable, Methods, Charged : Natural := 0;
      Error : AML_Execute.Execution_Status := AML_Execute.No_Return;
      Ancestor_Steps : Natural := 0;
      Last_Ancestor : ACPI_Service.Namespace.Node_ID := ACPI_Service.Namespace.Root;
   end record;
   function Observe return Report;
   function Quiet return Boolean;
   -- Copy only, even if Initialize was never reached. No AML or logger IPC.
   procedure Capture_If_Unvisited;
   -- Opening/flushing a publisher is allowed only with no active, pending or
   -- uncertain Field/EC command. Ring append needs no synchronous peer call.
   -- This predicate never grants retirement or releases an owner.
   function May_Open_Diagnostic_Channel return Boolean;
   -- Always emits detached records through the established publisher ring.
   -- Unsafe peer boundaries cannot open a channel or flush/wait for logstore.
   type Emission_Point is (Before_Cleanup, After_Cleanup);
   procedure Report_Copied_Diagnostics (Point : Emission_Point := After_Cleanup);
   -- Physical/descriptor readiness is established before any initializer.
   -- REG is separately activated after its closest initializer ancestor.
   procedure Prepare (Service : aliased in out ACPI_Service.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome);
   -- Read-only prerequisite membership from the prepared group plan.
   procedure Classify_Prerequisite
     (Service : ACPI_Service.State; Scope : ACPI_Service.Namespace.Node_ID;
      Required, Accepted : in out Boolean);
   procedure After_Ancestor
     (Service : aliased in out ACPI_Service.State; Scope : ACPI_Service.Namespace.Node_ID; Initialization_Completed : Boolean;
      Budget : Natural; Charged : in out Natural;
      Outcome : in out ACPI_Device_Policy.Region_Outcome);
   procedure Finish
     (Service : aliased in out ACPI_Service.State; Budget : Natural;
      Charged : in out Natural; Outcome : in out ACPI_Device_Policy.Region_Outcome);
   -- Caller first opens a finite window on the SAME runtime (or retains a
   -- valid startup window). False retains this owner, Core and backing.
   -- No deadline renewal, repeated connect or automatic release/replay.
   procedure Disconnect (Service : aliased in out ACPI_Service.State; Budget : Natural;
      Charged : out Natural; Completed : out Boolean);
end ACPI_Native_Regions;
