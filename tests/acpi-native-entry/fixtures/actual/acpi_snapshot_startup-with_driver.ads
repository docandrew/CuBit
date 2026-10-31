pragma Ada_2022;
with ACPI_Device_Initialization;
with ACPI_Service;
generic
   with package Device_Driver is new ACPI_Device_Initialization
     (Core => ACPI_Service, others => <>);
   with procedure Info (Item : out Inventory; Status : out R.Copy_Status);
   with procedure Describe
     (ID : Positive; Item : out Description; Status : out R.Copy_Status);
   Chunk_Max : Positive;
   with procedure Read_Part
     (ID : R.S.Table_ID; Offset : Natural; Into : out R.F.Bytes;
      Copied : out R.Byte_Count; Status : out R.Copy_Status);
   with procedure Clock (Now : out R.Tick; Available : out Boolean);
      with procedure Execute_Module_Startup
        (Service : in out ACPI_Service.State; Budget : Natural;
         Report : out ACPI_Service.Execution_Report;
         Status : out ACPI_Service.Values.Access_Status) is ACPI_Service.Execute_Frozen;
package ACPI_Snapshot_Startup.With_Driver is
   type Session is limited private;
   function Consumed (Context : Session) return Boolean;
   -- Trusted process-lifetime startup selection, never selected by wire words.
   -- Serialize Context, Run, provider and every service entry point.
   -- Callbacks must not reset/reenter or retain borrowed buffers/service aliases.
   -- A false Owner_Retained flag never releases independent backend obligations.
   -- Driver/provider authority and lifetimes must be established independently.
   -- The first attempt consumes Context before callbacks. A consumed Run is
   -- rejected before importing/executing another snapshot. Repeated calls on
   -- Context return the original owner with Startup_Already_Consumed, no work.
   -- On failure after completion entry, Owner_Retained means preserve Owner,
   -- Run, provider and backing until recovery/process exit; never serve/retry
   -- AML or dereference a callback-failed owner to inspect partial diagnostics.
   -- Loaded is still snapshot completion only; caller separately checks device
   -- readiness/Fatal/Field uncertainty and all lower provider obligations.
   -- The trusted module callback may use independently provisioned named-target
   -- Field_IO before device _INI. It must preserve owner/provider obligations.
   -- This selects no native provisioning, region connection or event enrollment.
   procedure Load (Context : in out Session; Run : in out Device_Driver.Session;
      Deadline : R.Tick; Owner : out State_Access; Result : out Report);
private
   type Session is limited record
      Attempt : Controlled_Attempt;
   end record;
end ACPI_Snapshot_Startup.With_Driver;
