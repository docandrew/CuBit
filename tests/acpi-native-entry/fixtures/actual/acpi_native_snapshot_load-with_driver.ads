pragma Ada_2022;
with ACPI_Device_Initialization;
with ACPI_Service;
generic
   with package Device_Driver is new ACPI_Device_Initialization
     (Core => ACPI_Service, others => <>);
   with procedure Execute_Module_Startup
     (Service : in out ACPI_Service.State; Budget : Natural;
      Result : out ACPI_Service.Execution_Report;
      Status : out ACPI_Service.Values.Access_Status) is ACPI_Service.Execute_Frozen;
package ACPI_Native_Snapshot_Load.With_Driver is
   -- One serialized process-lifetime startup per package instance. No reset.
   -- Run/provider authority, deadlines, backing and recovery lifetimes must be
   -- established independently and preserved after effects or uncertainty.
   -- Kernel-source diagnostics are copied values and survive repeated Load.
   -- The callback must honor Execute_Frozen's owner/catalog/fuel contract;
   -- do not reenter this facade, capture service aliases or retain buffers.
   -- Loaded means copied snapshot completion, not device/provider readiness.
   -- This facade installs no native provider, region connection or authority.
   function Consumed return Boolean;
   procedure Load
     (Run : in out Device_Driver.Session; Deadline : L.R.Tick;
      Owner : out L.State_Access; Result : out Report);
end ACPI_Native_Snapshot_Load.With_Driver;
