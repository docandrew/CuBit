with CCL.Declarations;
with CCL.Scheduling_Limits;

--  Shared native/host evaluator. Produces owned plans, never performs I/O.
package CCL.Configurations with SPARK_Mode => On is
   MAX_SETTINGS : constant := 128;
   --  Programs one startup plan may start: a named ceiling (LIM-001). The
   --  laptop build plans start 17 since the metrics store and viewer joined.
   MAX_LAUNCHES : constant := 32;
   type Profile_Kind is (System_Profile, Startup_Profile);
   type Network_Approval is (Deny, Approve_Declared);
   --  Trusted startup wiring, not a role an executable may claim for itself.
   type Startup_Role is (Application, Config_Storage);
   subtype Startup_Priority is Positive range 1 .. 10;
   --  At_Startup launches once at boot; Per_Device launches a driver for
   --  each discovered device its manifest matches (docs/ccl-driver-manifests.md).
   type Launch_Mode is (At_Startup, Per_Device);
   --  (restart on-failure): procmgr starts the entry's program again when
   --  it ends with a failure (a nonzero code, killed or faulted), after a
   --  delay that grows while it keeps failing. Never: it stays ended.
   type Restart_Policy is (Never, On_Failure);
   --  (replace <contract>): the entry's restart contract, how a running
   --  instance may be switched to another program (CuBit.Service_Replacement:
   --  a system generation's pin for it changed), always with the same
   --  approvals and only with the same manifest, while the device owner
   --  allows it. A per-service policy exception written in the trusted
   --  plan (REMOTE-001):
   --    reboot       (the default) never live: a new program takes effect
   --                 at the next boot (services holding hardware, firmware
   --                 or lock state: the ACPI pair until its stop, rebind
   --                 and health handshake is validated)
   --    transparent  it may be stopped and ended at any time; clients
   --                 reconnect
   --    graceful     it must quiesce: Stop, and it exits cleanly within
   --                 the caller's grace or the switch is refused (never
   --                 ended to make way; never two instances at once)
   type Restart_Contract is (Reboot_To_Replace, Transparent, Graceful);
   MAX_DEPENDENCIES : constant := 4;
   subtype Launch_Number is Natural range 0 .. MAX_LAUNCHES;
   subtype Dependency_Count is Natural range 0 .. MAX_DEPENDENCIES;
   --  Dependencies name earlier entries only, so startup order is acyclic
   --  by construction.
   type Dependency_Array is array (1 .. MAX_DEPENDENCIES) of Launch_Number;
   MAX_READY_DEADLINE_MS : constant := 60_000;
   subtype Ready_Deadline is Positive range 1 .. MAX_READY_DEADLINE_MS;
   --  (stop-grace-ms N): how long the entry has to end after Stop when the
   --  system stops (PWR-003, CuBit.System_Stops), before procmgr ends it.
   --  CuBit.Child_Specs.Maximum_Grace_Ms.
   MAX_STOP_GRACE_MS : constant := 600_000;
   subtype Stop_Grace is Positive range 1 .. MAX_STOP_GRACE_MS;
   --  (stop-stage apps | services | power-tail): when the root stop stops
   --  the entry (CuBit.Child_Specs.Stop_Stage, the same three): a declared
   --  fact. Power_Tail (the ACPI service) runs until the firmware's last
   --  step of a power-off.
   type Stop_Stage is (Services_Stage, Apps_Stage, Power_Tail_Stage);
   type Key_Text is record
      Length : Natural range 0 .. 128 := 0;
      Data : String (1 .. 128) := [others => ' '];
   end record;
   type Value_Text is record
      Length : Natural range 0 .. 1024 := 0;
      Data : String (1 .. 1024) := [others => ' '];
   end record;
   type Executable_Text is record
      Length : Natural range 0 .. 64 := 0;
      Data : String (1 .. 64) := [others => ' '];
   end record;
   --  Who approves a program's GPU render session (HW-002): no one, the
   --  startup plan itself, or a typed GPU_Rendering catalog setting the
   --  plan names, (render setting "desktop.render.gpu"), which procmgr
   --  reads when it starts the program (absent: enabled).
   type Render_Approval is (Render_Denied, Render_Declared, Render_By_Setting);
   type Setting_Entry is record
      Key : Key_Text;
      Value : Value_Text;
   end record;
   type Launch_Entry is record
      Executable : Executable_Text;
      Priority : Startup_Priority := 5;
      Approval : Network_Approval := Deny;
      -- Trusted startup approval, not authority claimed by the executable.
      -- Still requires its declared render endpoint and broker admission.
      Render : Render_Approval := Render_Denied;
      --  Render_By_Setting: the GPU_Rendering catalog setting that decides.
      Render_Setting : Key_Text;
      Role : Startup_Role := Application;
      Mode : Launch_Mode := At_Startup;
      Restart : Restart_Policy := Never;
      Replacement : Restart_Contract := Reboot_To_Replace;
      --  Per_Device drivers only: the manifest's device resources may be
      --  granted for the matched device.
      Approve_Device : Boolean := False;
      Dependency_Total : Dependency_Count := 0;
      Dependencies : Dependency_Array := [others => 0];
      --  A ceiling on the manifest's real-time request, never a grant by
      --  itself.
      Approve_Scheduling : Boolean := False;
      --  (firmware-snapshot approve-declared): the trusted startup plan
      --  approves the manifest's declared firmware snapshot reader
      --  (Firmware_Snapshot_ABI). Never claimed by the executable itself.
      Approve_Firmware_Snapshot : Boolean := False;
      --  (acpi-hardware approve-declared): the plan approves the manifest's
      --  declared ACPI hardware windows (Request.ACPI_Hardware); procmgr
      --  grants them to nothing else (coordination/acpi-region-boundary.md).
      Approve_ACPI_Hardware : Boolean := False;
      --  (display-capture approve-declared): the plan approves the
      --  manifest's declared "display-capture" endpoint, Desktop's endpoint
      --  badged for a read-only copy of the composed frame
      --  (CuBit.Display_Capture). Off unless written: never ambient.
      Approve_Display_Capture : Boolean := False;
      --  (acpi-pci-config approve-declared): on an entry that also approves
      --  acpi-hardware, the grant includes the read-only, uncached PCI
      --  segment 0 bus 0 configuration window (ACPI-PCI-001, the kernel's
      --  Firmware_PCI_Config_Tag root). Off unless written: an entry without
      --  it never sees the window. Without acpi-hardware it is refused.
      Approve_ACPI_PCI_Config : Boolean := False;
      --  (acpi-firmware-data approve-declared): on an entry that also
      --  approves acpi-hardware, the grant includes devmgr's firmware-data
      --  endpoint (CuBit.Firmware_Data_Exchange: table-named firmware data,
      --  the kernel's Firmware_Data_Tag roots, one envelope per request).
      --  Off unless written. Without acpi-hardware it is refused.
      Approve_ACPI_Firmware_Data : Boolean := False;
      --  (platform-power approve-declared): the plan approves the manifest's
      --  declared platform power object (Request.Platform_Power, KERN-004):
      --  entering S5 or reset through the kernel. Off unless written.
      Approve_Platform_Power : Boolean := False;
      --  (trace approve-declared): the plan approves the manifest's
      --  declared kernel trace object (Request.Trace, Trace_ABI): reset and
      --  summarize the kernel trace (the benchmarks). Off unless written.
      Approve_Trace : Boolean := False;
      --  (test-console approve-declared): the plan approves the manifest's
      --  declared test console object (Kernel_Object.Test_Console,
      --  Test_Console_ABI): test builds' logstore mirrors debug text onto
      --  the serial console. Test plans only. Off unless written.
      Approve_Test_Console : Boolean := False;
      --  (vm-create approve-declared): the plan approves the manifest's
      --  declared VM creation authority (Kernel_Object.VM_Create, VMM-001,
      --  docs/hypervisor.md): procmgr installs a copy, the program creates
      --  virtual machines. Off unless written.
      Approve_VM_Create : Boolean := False;
      Scheduling_Budget, Scheduling_Period : CCL.Scheduling_Limits.Microseconds :=
        CCL.Scheduling_Limits.Microseconds'First;
      Has_Ready_Deadline : Boolean := False;
      Ready_Deadline_Ms : Ready_Deadline := Ready_Deadline'Last;
      --  Absent: the stop request's grace for undeclared entries applies.
      Has_Stop_Grace : Boolean := False;
      Stop_Grace_Ms : Stop_Grace := Stop_Grace'First;
      Stage : Stop_Stage := Services_Stage;
      --  (arguments (Record field => value ...)): the program's typed
      --  parameters (docs/ccl-launch-parameters.md), as written. procmgr
      --  checks it against the program's .cubit.parameters signature when
      --  it launches it, and so does the image build; empty: none given.
      --  Arguments_At: where the expression starts in the profile, from 1.
      Arguments : Value_Text;
      Arguments_At : Natural := 0;
      --  Where the entry's executable name starts in the profile, from 1.
      Entry_At : Natural := 0;
   end record;
   --  A device-class launch (SUP-001, docs/otp-supervision-study.md): the
   --  class is the entry's key, as a setting's key is, and the rest is ONE
   --  typed CCL value, a Device_Class_Launch, kept as written like a start
   --  entry's (arguments ...):
   --
   --    (driver-class "PNP0C50"
   --      (Device_Class_Launch
   --        facts => ["descriptor_register"]
   --        policy => (HID_Parameters x => ... timing => ...)))
   --
   --  The driver a supervisor (devmgr) starts for a device of the class
   --  through Start_Child (CuBit.Child_Specs) gets these typed parameters:
   --  policy writes the POLICY fields of the driver's parameter record and
   --  facts names the FACT fields the supervisor fills from the device.
   --  Nothing inside the value is read here: its type involves the driver's
   --  own record, known only with its file. procmgr checks it with CCL's
   --  type checker (procmgr's Class_Launch_Checks) when it starts the driver: the
   --  policy against the driver's record without the fact fields (so a
   --  policy naming a fact, or leaving a policy field out, is a type
   --  error), the facts against the supervisor's, then the merged record
   --  against the driver's signature. Class: an ACPI _HID/_CID, once per
   --  plan.
   MAX_DRIVER_CLASSES : constant := 8;
   MAX_CLASS_ID_BYTES : constant := 8;
   subtype Class_Count is Natural range 0 .. MAX_DRIVER_CLASSES;
   type Class_Block is record
      Class : Key_Text;
      Launch : Value_Text;
      --  Where the value and the class start in the profile, from 1.
      Launch_At : Natural := 0;
      Class_At : Natural := 0;
   end record;
   type Class_Array is array (1 .. MAX_DRIVER_CLASSES) of Class_Block;
   type Setting_Array is array (Positive range 1 .. MAX_SETTINGS) of Setting_Entry;
   type Launch_Array is array (Positive range 1 .. MAX_LAUNCHES) of Launch_Entry;
   type Configuration_Plan is record
      Kind : Profile_Kind := System_Profile;
      Setting_Count : Natural range 0 .. MAX_SETTINGS := 0;
      Launch_Count : Natural range 0 .. MAX_LAUNCHES := 0;
      Settings : Setting_Array;
      Launches : Launch_Array;
      Class_Total : Class_Count := 0;
      Classes : Class_Array;
      --  A startup profile's (policy ...): the image's system policy, a
      --  typed CCL System_Policy (CCL.Interfaces.System_Policy, key
      --  system.policy), stored as its canonical source. procmgr reads it
      --  once at boot. Absent: Has_Policy False (a development plan).
      Has_Policy : Boolean := False;
      Policy : Value_Text;
   end record;
   type Diagnostic_Code is
     (No_Error, Invalid_Syntax, Unsupported_Version, Unknown_Declaration,
      Invalid_Key, Invalid_Value, Duplicate_Key, Too_Many_Entries,
      Invalid_Executable, Invalid_Priority, Invalid_Approval, Invalid_Role, Duplicate_Field,
      Missing_Field, Trailing_Input, Invalid_Dependency, Invalid_Launch_Mode,
      Invalid_Scheduling, Invalid_Deadline, Invalid_Typed_Value,
      --  (arguments ...) given twice, or longer than a Value_Text.
      Invalid_Arguments,
      --  A (driver-class ...) entry: its class is no ACPI ID or is given
      --  twice, or it is not exactly one value that fits a Value_Text.
      Invalid_Driver_Class);
   type Compilation_Result is record
      Success : Boolean := False;
      Diagnostic : Diagnostic_Code := No_Error;
      Syntax_Diagnostic : CCL.Declarations.Diagnostic_Code := CCL.Declarations.No_Error;
      Position : Positive := 1;
      --  Why a typed setting's value was refused (Invalid_Typed_Value).
      Typed_Message : String (1 .. 160) := [others => ' '];
      Typed_Message_Length : Natural range 0 .. 160 := 0;
      Plan : Configuration_Plan;
   end record;
   procedure Compile (Source : String; Result : out Compilation_Result);
   --  Native runtime enumeration 'Image currently formats numeric positions.
   function Diagnostic_Name (Code : Diagnostic_Code) return String;
end CCL.Configurations;
