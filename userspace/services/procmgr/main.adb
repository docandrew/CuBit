with ACPI_CMOS_Handover_Protocol;
with ACPI_CMOS_Manifest_Windows;
with CCL.Resource_Sections;
with CCL.Setting_Codecs.ACPI_Firmware_Profile;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Process Manager Service
--
--  Handles OP_LAUNCH requests from userspace clients. Reads an ELF binary
--  from the filesystem service and calls SPAWN to load it into a
--  new process.
--
--  Capabilities (CAP-002): the handles devmgr placed before procmgr ran
--  (CuBit.Handles.Boot_Roots.Process_Manager), the per-child handles SPAWN
--  gives it, and the endpoints of the services it started. Each child gets
--  its startup table, built from its manifest (CuBit.Startup_Builder), in
--  its launch block before it first runs.
------------------------------------------------------------------------------
with CuBit.Waits;
with CuBit.Introduction_Ledgers;
with CuBit.Late_Introduction;
with Deferred_Warnings;
with Driver_Log_Issuances;
with CuBit.Console_Route;
with Source_Notices;
with CuBit.Log_Sources;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Config_Worker_Startup;
with Interfaces; use Interfaces;
with CCL.Configurations;
with GPU_Rendering_Settings;
with Plan_Restarts;
with Service_Switches;
with Manifest_Fingerprints;
with Boot_Generations;
with System_Generation_Records.Reading;
with Storehouse.Hashing;
with CuBit.Service_Replacement;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CCL.Declarations;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Control;
with CuBit.Handles.Devices;
with CuBit.Handles.Boot_Roots;
with CuBit.Handles.Lines;
with CuBit.ACPI_SCI_Route;
with Interrupt_Line_ABI;
with CuBit.Startup_Tables;
with CuBit.Startup_Builder;
with CuBit.Launch_Envelopes;
with CuBit.Service_Exchange;
with CuBit.Launch_Configurations;
with Launch_Configuration;
with CuBit.Process_IDs.Text;
with CuBit.Audio_Control;
with CuBit.Clock_Control;
with CuBit.Disk_Install;
with CuBit.Installer_Protocol;
with CuBit.TLS_Protocol;
with CuBit.Llm_Protocol;
with CuBit.Secrets_Protocol;
with CuBit.TLS_Scopes;
with CuBit.Authority_Policy;
with CuBit.Log_Protocol;
with CuBit.Metric_Protocol;
with CuBit.Authority; use CuBit.Authority;
with CuBit.Memory_Grants;
with CuBit.Process_Observer;
with CuBit.Boot_Milestone_Codes;
with CuBit.Boot_Milestones;
with CuBit.Grant_References;
with CuBit.Launch_Arguments;
with CuBit.Launch_Failure_Text;
with CuBit.Launch_Grants;
with CuBit.Program_Descriptions;
with CuBit.Outlet_Rings;
with CuBit.Parameter_Sections;
with CuBit.Parameter_Signatures;
with CCL.Typed_Values;
with CuBit.Stream_Regions;
with CuBit.Stream_Rings;
with CuBit.Child_Exits;
with CuBit.Startup_Check;
with CuBit.Control_Events;
with CuBit.Launch_Authority;
with CuBit.Failures;
with CuBit.Filesystems;
with Windowed_Reads;
with CuBit.File_Access;
with CuBit.Network_Authority;
with CuBit.Render_Authority;
with CuBit.Firmware_Snapshot_Authority;
with CuBit.Publication_Grants;
with CuBit.Self_Offers;
with CuBit.Platform_Power_Authority;
with CuBit.Trace_Authority;
with CuBit.Test_Console_Authority;
with CuBit.VM_Create_Authority;
with System_Publications;
with CuBit.Firmware_Snapshot;
with CuBit.ACPI_Hardware_Authority;
with CuBit.ACPI_Fixed_Windows;
with CuBit.ACPI_Window_Exchange;
with CuBit.Firmware_Data_Exchange;
with ACPI_Protocol;
with ACPI_PCI_Window_Exchange;
with ACPI_PCI_Window_Approval;
with CuBit.Render_Startup;
with CuBit.Launch_Policy; use CuBit.Launch_Policy;
with CuBit.Capability_Grants;
with CuBit.Identity_Tables;
with CuBit.Process_List;
with Intel_GPU_Broker_Request;
with Intel_Render_Launch_Client;
with CuBit.Input;
with CuBit.Refusal_Notes;
with CuBit.Startup_Wait_Test;
with CuBit.Net_Manager_Control;
with CuBit.Display_Capture;
with CuBit.Child_Specs;
with Class_Launch_Checks;
with Launch_Attenuation;
with Session_Launch;
with Handle_Admission;
with CCL.Setting_Codecs.System_Policy;
with CuBit.Launch_Handles;
with CuBit.Handles.Debug;
with Debug_ABI;
with Child_Parameters;
with Supervision_Edges;
with Shutdown_Sequences;
with CuBit.System_Stops;
with CuBit.Power_Requests;
with CuBit.Power_Sources;
with CuBit.Debug_Protocol;
with CuBit.Power_Event_Exchange;
with CCL.Device_Sections;
with CuBit.Monotonic;

procedure main is
   function Image (Process : Process_ID) return String
     renames CuBit.Process_IDs.Text.Image;
   --  Refused mints reach logstore once it runs (the kernel's own
   --  "kernel: device-memory denied" record says why).
   --  logstore's endpoint, once it runs (defined with the process records).
   function Logstore_Collector return CuBit.Handles.Endpoint;
   --  Who each process procmgr starts is, for logstore (LOG-003): its
   --  records keep their name after it exits.
   package Sources is new Source_Notices
     (Issuance => CuBit.Log_Sources.Process_Manager_Issuance, Collector => Logstore_Collector);
   package Warnings is new Deferred_Warnings
     (Issuance => Driver_Log_Issuances.Fixed (Driver_Log_Issuances.Process_Manager),
      Collector => Logstore_Collector, Before_Bind => Sources.Pump);
   --  The same notices for metrics.svc (MET-002): a series is labelled with
   --  its program's identity and name, which only its launcher can say.
   function Metrics_Collector return CuBit.Handles.Endpoint;
   package Metric_Sources is new Source_Notices
     (Issuance => CuBit.Log_Sources.Process_Manager_Issuance, Collector => Metrics_Collector);
   use ASCII;
   --  Never reuse issuance identities within this bootstrap issuer lifetime.
   --  Independent procmgr restart requires an epoch protocol, not yet supported.
   Next_Log_Issuance : Unsigned_32 := 1;
   --  metrics.svc's package identity (MET-002): it alone registers the role.
   Metrics_Identity : constant String := "com.cubit.metrics";
   --  Same rule for metrics.svc publisher/observer tags; zero = exhausted.
   Next_Metric_Issuance : Unsigned_32 := 1;
   package GPU_Grants renames CuBit.Capability_Grants;
   package H renames CuBit.Handles;
   use all type H.Endpoint, H.Process, H.Cspace, H.Device_Memory, H.Notification, H.Firmware_Snapshot,
                H.Platform_Power, H.Trace, H.Test_Console, H.VM_Create,
                H.Rights, H.Derivation;
   package HIPC renames CuBit.Handles.IPC;
   package HC renames CuBit.Handles.Control;
   package ST renames CuBit.Startup_Tables;

   --  What devmgr placed in procmgr's table before it ran.
   PH : constant CuBit.Handles.Boot_Roots.Launcher_Handles :=
     CuBit.Handles.Boot_Roots.Process_Manager;

   function Render_Broker return CuBit.Handles.Endpoint is (PH.Device_Manager);
   package Render_Launch is new Intel_Render_Launch_Client
     (Render_Broker, 16#4750_2000#, 16#4750_2FFF#);
   Render_Launcher : Render_Launch.Launcher;
   type Render_Request is record
      Requested, Valid : Boolean := False;
      Demand : CuBit.Render_Startup.Requirement := CuBit.Render_Startup.Required;
   end record;

   --  Rights of the copies procmgr keeps or hands on (CuBit.Handles has
   --  Read_Only, Write_Only, Read_Write).
   Read_Write_Grant : constant H.Rights :=
     (Read => True, Write => True, Grant => True, others => False);
   Write_Grant      : constant H.Rights := (Write => True, Grant => True, others => False);

   --  A manifest's rights byte (CCL.Manifests Rights_Kind: 1 read, 2 write,
   --  3 both) as the rights of the copy.
   function Manifest_Rights (Mask : Unsigned_64) return H.Rights is
     ((Read => (Mask and 1) /= 0, Write => (Mask and 2) /= 0, others => False));

   --  IPC label constants
   OP_LAUNCH  : constant Unsigned_32 := CuBit.Launch_Arguments.Launch_Operation;
   --  The kernel's retirement notice (kernel/src/ipc_labels.ads).
   EVENT_CHILD_EXIT : constant Unsigned_32 := 16#0103#;
   REPLY_OK     : constant Unsigned_32 := 16#F000#;
   REPLY_ERR    : constant Unsigned_32 := 16#F001#;
   OP_SET_ACL   : constant Unsigned_32 := 16#0080#;

   --  ELF buffer — starts small, grows dynamically via sbrk when needed.
   INITIAL_BUF_PAGES : constant := 16;  --  64 KB starting size
   PAGE_SIZE         : constant := 4096;
   bufPages    : Natural := INITIAL_BUF_PAGES;
   bufCapacity : Natural := INITIAL_BUF_PAGES * PAGE_SIZE;

   Config_Storage_Selected : Boolean := False;

   --  Bounded launch-authority provenance ledger.  This records why procmgr
   --  attempted or installed authority; live authority remains authoritative
   --  in the kernel capability table.
   MAX_AUTHORITY_RECORDS : constant := 512;

   type Authority_Record is record
      valid       : Boolean := False;
      authorityId : Unsigned_32 := 0;
      pid         : Process_ID := No_Process;
      --  The startup entry (CuBit.Startup_Tables) it explains; 0 for
      --  authority outside the table (quota, kernel self entries).
      entryIndex  : Unsigned_8 := 0;
      source      : Unsigned_8 := 0;
      reason      : Unsigned_8 := 0;
      capType     : Unsigned_8 := 0;
      requested   : Boolean := False;
      granted     : Boolean := False;
      rights      : Unsigned_8 := 0;
      objectRef   : Unsigned_64 := 0;
      objectParam : Unsigned_64 := 0;
   end record;

   type Authority_Record_Array is
      array (Natural range 0 .. MAX_AUTHORITY_RECORDS - 1) of Authority_Record;
   authorityRecords : Authority_Record_Array;
   nextAuthorityRecord : Natural := 0;
   nextAuthorityId : Unsigned_32 := 1;

   --  Service routing identifiers
   SERVICE_FS     : constant Unsigned_8 := 0;
   --  One filesystem scope in an OP_SET_ACL batch (CuBit.File_Access): the
   --  rights, the prefix length (u16), reserved bytes, then the prefix.
   FS_ENTRY_BYTES : constant := CuBit.File_Access.Wire_Entry_Bytes;
   FS_PREFIX_AT   : constant := CuBit.File_Access.Wire_Header_Bytes;
   SERVICE_CONFIG : constant Unsigned_8 := 1;
   SERVICE_TLS    : constant Unsigned_8 := 2;
   --  A metrics read scope (MET-002): a series-name prefix procmgr installs
   --  in metrics.svc (CuBit.Metric_Protocol.Set_Scopes), as TLS scopes in
   --  tls.svc.
   SERVICE_METRICS : constant Unsigned_8 := CuBit.Launch_Authority.Metrics_Service;

   --  The launch buffer. Its first INITIAL_BUF_PAGES are the exchange pages
   --  lent for as long as procmgr runs (paths and delegated scopes to the
   --  filesystem, keys and values to config, scope lists to tls.svc and
   --  metrics.svc), so those peers can write them at any time. Files
   --  procmgr reads (ELF images, init.ccl, generations) go after them, at
   --  imageBuf, through windows lent to the filesystem only while it reads
   --  and retired before procmgr looks at a byte (SEC-025: no peer can
   --  change an image between procmgr's checks, its fingerprint and the
   --  kernel's load).
   elfBuf : System.Address := System.Null_Address;
   EXCHANGE_BYTES : constant := INITIAL_BUF_PAGES * PAGE_SIZE;
   function imageBuf return System.Address is (elfBuf + Storage_Offset (EXCHANGE_BYTES));

   --  Grant to FS server covering elfBuf
   fsGrant : CuBit.Memory_Grants.Grant_Reference;
   --  A failed retirement quarantines the entire buffer until restart.
   --  Never overwrite pages which might still be lent to a failed peer.
   fileBufferUsable : Boolean := True;

   --  Grant to config service covering elfBuf
   Config_Grant : CuBit.Memory_Grants.Grant_Reference;
   Config_Grant_Ready : Boolean := False;

   --  procmgr's own policy endpoint to tls.svc (a copy badged Policy_Tag)
   --  and a loan of elfBuf for scope lists, made once tls.svc has registered.
   TLS_Policy : CuBit.Handles.Endpoint := H.No_Endpoint;
   TLS_Policy_PID : Process_ID := No_Process;
   TLS_Grant : CuBit.Memory_Grants.Grant_Reference;
   TLS_Policy_Ready : Boolean := False;
   --  The same for metrics.svc's read scopes (MET-002).
   Metrics_Policy : CuBit.Handles.Endpoint := H.No_Endpoint;
   Metrics_Policy_PID : Process_ID := No_Process;
   Metrics_Grant : CuBit.Memory_Grants.Grant_Reference;
   Metrics_Policy_Ready : Boolean := False;

   --  OP_LAUNCH: the validated launch block of the request being served
   --  (procmgr's own copy, never the requester's grant), installed into
   --  each child Spawn_Attempt creates for it. Zero bytes: no block.
   launchBlock : CuBit.Launch_Arguments.Block
     (1 .. CuBit.Launch_Arguments.Maximum_Block_Bytes) := [others => 0];
   pendingArgumentBytes : CuBit.Launch_Arguments.Block_Length := 0;
   --  The startup plan's (arguments ...) for the program being started
   --  (docs/ccl-launch-parameters.md), as written; empty for every other
   --  launch (OP_LAUNCH), which cannot give typed parameters.
   pendingParameters : CCL.Configurations.Value_Text;
   package CMOS_Wire renames ACPI_CMOS_Handover_Protocol;
   package CMOS_Windows renames ACPI_CMOS_Manifest_Windows;
   use type CMOS_Windows.Selection_Status, CMOS_Windows.Window_Kind;
   package CMOS_Profile_Codec renames CCL.Setting_Codecs.ACPI_Firmware_Profile;
   CMOS_Boot_Ports : H.IO_Ports := H.No_IO_Ports;
   CMOS_Boot_Receipt : CMOS_Wire.Receipt;
   CMOS_Boot_Valid : Boolean := False;
   CMOS_Recipient : Process_ID := No_Process;

   --  The block a child is given: that payload, then its startup table
   --  (CuBit.Launch_Envelopes).
   launchEnvelope : CuBit.Launch_Envelopes.Block
     (1 .. CuBit.Launch_Envelopes.Maximum_Block_Bytes) := [others => 0];
   --  The places the launcher delegates to the child being started
   --  (CuBit.Launch_Grants), validated; none when pendingGrantBytes is 0.
   pendingGrants : CuBit.Launch_Grants.Bytes (1 .. CuBit.Launch_Grants.Maximum_Bytes) :=
     [others => 0];
   pendingGrantBytes : CuBit.Launch_Grants.Byte_Count := 0;
   --  The rings the launcher lends the child for its output ports
   --  (CuBit.Outlet_Rings, its grants to procmgr), validated; none when
   --  pendingRings.Count is 0.
   pendingRings : CuBit.Outlet_Rings.Table;

   --  Launch authority (CuBit.Launch_Authority), per process: the programs it
   --  may start (its .cubit.launch table; none without one) and what it was
   --  granted, so a child it starts can be held to a subset of that.
   package LAuth renames CuBit.Launch_Authority;
   package LAtt renames Launch_Attenuation;
   type Held_Scope is record
      Service, Rights : Unsigned_8 := 0;
      Length : Natural range 0 .. LAuth.Maximum_Prefix_Bytes := 0;
      Prefix : String (1 .. LAuth.Maximum_Prefix_Bytes) := [others => ' '];
      --  Delegated by its launcher (CuBit.Launch_Grants), not its manifest.
      Delegated : Boolean := False;
   end record;
   type Held_Scope_Array is
     array (1 .. CuBit.File_Access.Maximum_Entries) of Held_Scope;
   --  Service roles below this are tracked as held; others never pass on.
   TRACKED_SERVICE_ROLES : constant := 64;
   --  Per role, the rights of each endpoint held (Launch_Attenuation).
   type Role_Holdings is array (0 .. TRACKED_SERVICE_ROLES - 1) of LAtt.Held_Masks;
   --  The network scopes a process was granted, which a child it launches
   --  may be given within (at most this many are tracked; more never pass on).
   MAXIMUM_HELD_NETWORK : constant := 4;
   type Held_Network_Array is array (1 .. MAXIMUM_HELD_NETWORK) of CuBit.Network_Authority.Scope;
   type Ring_Parent_Array is array (1 .. CuBit.Outlet_Rings.Maximum_Entries)
     of CuBit.Memory_Grants.Grant_Reference;
   type Launch_State is record
      Table        : LAuth.Table_Bytes (1 .. LAuth.Maximum_Table_Bytes) :=
                       [others => 0];
      Table_Length : LAuth.Table_Length := 0;
      Services     : Role_Holdings := [others => LAtt.None_Held];
      --  Render approved for it, for its declared render request: a child
      --  it launches may then ask devmgr for a render session too.
      Render_Approved : Boolean := False;
      Network       : Held_Network_Array := [others => CuBit.Network_Authority.Denied_Scope];
      Network_Count : Natural range 0 .. MAXIMUM_HELD_NETWORK := 0;
      Scope_Count  : Natural range 0 .. CuBit.File_Access.Maximum_Entries := 0;
      Scopes       : Held_Scope_Array;
      --  The launcher's ring grants procmgr acquired to derive this
      --  process's port rings; returned when it ends.
      Ring_Parents : Ring_Parent_Array := [others => (others => <>)];
      Ring_Parent_Count : Natural range 0 .. CuBit.Outlet_Rings.Maximum_Entries := 0;
      --  It holds an offerable self endpoint (CuBit.Self_Offers): a child
      --  it launches may declare one too.
      Offerable_Self : Boolean := False;
   end record;
   --  Keyed by process identity (KERN-003): one entry per process seen.
   Empty_Launch_State : constant Launch_State := (others => <>);
   package Launches is new CuBit.Identity_Tables
     (Element => Launch_State, Empty => Empty_Launch_State, Capacity_Bits => 9);
   --  pid's launch state, added if new; 0 if the table is full.
   function launchIndex (pid : Process_ID) return Launches.Index renames Launches.Ensure;
   --  The OP_LAUNCH requester whose authority bounds the child being
   --  spawned (0: none), and whether the child asked for more.
   attenuateFor : Process_ID := No_Process;
   --  The launch being served is a Launch-into-S (OP_LAUNCH through a
   --  Launch badge, Session_Launch): the child gets its approval within the
   --  user session's envelope, not its requester's authority (attenuateFor
   --  is then the requester, for what it introduces).
   launchSession : Boolean := False;

   --  The image's debugging gate (CCL.Interfaces.System_Policy): the
   --  startup plan's (policy ...), read once at boot (processInitCCL) and
   --  fixed for the boot. A plan without one is a development plan.
   Debugging_Gate : Handle_Admission.Debugging_Gate := Handle_Admission.Development;
   --  The first required request the envelope withheld, for the
   --  Request_Withheld refusal (CuBit.Launch_Arguments.Encode_Label).
   withheldLabel : CuBit.Launch_Arguments.Label_Text := [others => ' '];
   withheldLength : CuBit.Launch_Arguments.Label_Length := 0;
   attenuationRefused : Boolean := False;
   --  The launched child could not read the working directory its launch
   --  block names; refused like an attenuation failure.
   directoryRefused : Boolean := False;
   --  Start_Child (SUP-001, CuBit.Child_Specs) of a device-class spec: the
   --  class (an ACPI ID) the program's manifest must drive, checked once
   --  its ELF is read (Spawn_Attempt); empty for every other launch. A
   --  program that does not declare it is refused (classRefused).
   pendingClass : String (1 .. CuBit.Child_Specs.Maximum_Class_Bytes) := [others => ' '];
   pendingClassLength : Natural range 0 .. CuBit.Child_Specs.Maximum_Class_Bytes := 0;
   classRefused : Boolean := False;
   --  The device manager's badges for the child's startup labels (an I2C
   --  target, a GPIO input): the provider's endpoint for that service
   --  request is badged with it (Mint_Service). None for any other launch.
   pendingBindings : CuBit.Child_Specs.Binding_Array := [others => (others => <>)];
   pendingBindingCount : CuBit.Child_Specs.Binding_Count := 0;
   --  A device-class Start_Child's launch value (the plan's
   --  Device_Class_Launch) and the supervisor's facts, type-checked once the
   --  child's signature is read (Spawn_Attempt, Class_Launch_Checks); a
   --  refusal there is reported by classLaunchRefusal (Merged: none).
   pendingClassLaunch : CCL.Configurations.Value_Text;
   pendingFacts : CuBit.Child_Specs.Fact_Array := [others => (others => <>)];
   pendingFactCount : CuBit.Child_Specs.Fact_Count := 0;
   classLaunchRefusal : Class_Launch_Checks.Outcome := Class_Launch_Checks.Merged;
   classLaunchMessage : String (1 .. Class_Launch_Checks.Maximum_Message) := [others => ' '];
   classLaunchMessageLength : Natural range 0 .. Class_Launch_Checks.Maximum_Message := 0;
   --  A Start_Child spec with Start Held: the child is spawned and given
   --  everything but not resumed; its supervisor releases it.
   pendingHold : Boolean := False;

   --  Return the ring grants procmgr acquired to derive pid's port rings.
   procedure releaseRings (pid : Process_ID) is
      Returned : Boolean;
      I : constant Launches.Index := Launches.Find (pid);
   begin
      if I /= 0 then
         declare
            State : Launch_State renames Launches.Values (I);
         begin
            for P of State.Ring_Parents (1 .. State.Ring_Parent_Count) loop
               CuBit.Memory_Grants.Return_Acquisition (P, Returned);
            end loop;
            State.Ring_Parent_Count := 0;
         end;
      end if;
   end releaseRings;

   --  pid starts afresh: no launch table, services or scopes yet. Its ring
   --  parents stay until it ends (releaseRings).
   procedure resetLaunchState (pid : Process_ID) is
      I : constant Launches.Index := Launches.Find (pid);
   begin
      if I /= 0 then
         Launches.Values (I).Table_Length := 0;
         Launches.Values (I).Services := [others => LAtt.None_Held];
         Launches.Values (I).Render_Approved := False;
         Launches.Values (I).Network_Count := 0;
         Launches.Values (I).Scope_Count := 0;
      end if;
   end resetLaunchState;

   --  pid has ended and its rings are returned: drop its entry.
   procedure forgetLaunchState (pid : Process_ID) is
   begin
      Launches.Remove (pid);
   end forgetLaunchState;

   ---------------------------------------------------------------------------
   --  printDec - print a small unsigned number in decimal
   ---------------------------------------------------------------------------
   procedure printDec (val : Unsigned_64) is
      buf : String (1 .. 20);
      pos : Natural := buf'Last;
      v   : Unsigned_64 := val;
   begin
      if v = 0 then
         debugPrint ("0");
         return;
      end if;
      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;
      debugPrint (buf (pos + 1 .. buf'Last));
   end printDec;

   procedure printDec (val : Unsigned_32) is
   begin
      printDec (Unsigned_64 (val));
   end printDec;

   ---------------------------------------------------------------------------
   --  sendReply - send a reply message to a waiting sender
   ---------------------------------------------------------------------------
   procedure sendReply
     (dest  : Process_ID;
      label : Unsigned_32;
      word0 : Unsigned_64)
   is
      replyMsg : Message := NULL_MESSAGE;
      ignore   : Unsigned_64;
   begin
      replyMsg.tag := (label  => label,
                       length => 1,
                       protocol  => 0, ipc => <>,
                       reserved  => 0);
      replyMsg.words := [0 => word0, others => 0];
      ignore := reply (dest, replyMsg);
   end sendReply;

   procedure recordAuthority
     (pid         : Process_ID;
      entryIndex  : Natural;
      source      : Unsigned_8;
      reason      : Unsigned_8;
      capType     : Unsigned_64;
      requested   : Boolean;
      granted     : Boolean;
      rights      : Unsigned_64;
      objectRef   : Unsigned_64;
      objectParam : Unsigned_64)
   is
      index : Natural := MAX_AUTHORITY_RECORDS;
   begin
      --  A startup entry has one launch-time explanation.  Replace an
      --  earlier record when later launch policy explains the same entry.
      for i in authorityRecords'Range loop
         if authorityRecords (i).valid and then
            authorityRecords (i).pid = pid and then
            entryIndex /= 0 and then
            Natural (authorityRecords (i).entryIndex) = entryIndex
         then
            index := i;
            exit;
         end if;
      end loop;

      if index = MAX_AUTHORITY_RECORDS then
         index := nextAuthorityRecord;
         nextAuthorityRecord :=
            (nextAuthorityRecord + 1) mod MAX_AUTHORITY_RECORDS;
      end if;

      authorityRecords (index) :=
        (valid       => True,
         authorityId => nextAuthorityId,
         pid         => pid,
         entryIndex  => Unsigned_8 (Natural'Min (entryIndex, Natural (Unsigned_8'Last))),
         source      => source,
         reason      => reason,
         capType     => Unsigned_8 (capType and 16#FF#),
         requested   => requested,
         granted     => granted,
         rights      => Unsigned_8 (rights and 16#1F#),
         objectRef   => objectRef,
         objectParam => objectParam);
      if nextAuthorityId = Unsigned_32'Last then
         nextAuthorityId := 1;
      else
         nextAuthorityId := nextAuthorityId + 1;
      end if;
   end recordAuthority;

   --  netstack releases what Owner held: channels, listeners, arenas,
   --  scopes and their reservations (CuBit.Network_Authority.OP_RELEASE_OWNER).
   procedure releaseNetworkOwner (owner : Process_ID) is
      request : Message := NULL_MESSAGE;
      ignore : MessageTag;
   begin
      if owner = No_Process then
         return;
      end if;
      request.tag.label := CuBit.Network_Authority.OP_RELEASE_OWNER;
      request.tag.length := 1;
      request.words (0) := To_Word (owner);
      ignore := capCall (PH.Network_Policy, request, CuBit.Messages.Wait_Forever);
   end releaseNetworkOwner;

   --  filesystem.svc releases what Owner held: handles (buffered writes
   --  harvested first), its request queue and grants, its access profile
   --  (CuBit.Filesystems.OP_RELEASE_OWNER).
   procedure releaseFilesystemOwner (owner : Process_ID) is
      request : Message := NULL_MESSAGE;
      ignore : MessageTag;
   begin
      if owner = No_Process then
         return;
      end if;
      request.tag.label := CuBit.Filesystems.OP_RELEASE_OWNER;
      request.tag.length := 1;
      request.words (0) := To_Word (owner);
      ignore := capCall (PH.Filesystem, request, CuBit.Messages.Wait_Forever);
   end releaseFilesystemOwner;

   --  The kernel's process list still holds pid.
   Process_List_Bytes : constant := 8_192;
   Process_Entry_Bytes : constant := CuBit.Process_List.Entry_Bytes;
   processList : array (0 .. Process_List_Bytes - 1) of Unsigned_8 := [others => 0];
   function processListed (pid : Process_ID) return Boolean is
      count : constant Unsigned_64 :=
        HC.Process_List (PH.Process_List_Object, processList'Address, Process_List_Bytes);
   begin
      if count = Unsigned_64'Last then
         return True;   --  unknown: do not act on the claim
      end if;
      for i in 0 .. Natural (Unsigned_64'Min (count, Process_List_Bytes / Process_Entry_Bytes)) - 1 loop
         if CuBit.Process_List.Get (processList'Address, i).Identity = pid then
            return True;
         end if;
      end loop;
      return False;
   end processListed;

   --  What only procmgr knows about each process it started, for the
   --  process-observer role (CuBit.Process_Observer): manifest identity,
   --  launcher and start time. Cleared when the process retires.
   package PO renames CuBit.Process_Observer;
   type Process_Record is record
      Valid : Boolean := False;
      Identity : String (1 .. PO.Identity_Bytes) := [others => ' '];
      Identity_Length : Natural range 0 .. PO.Identity_Bytes := 0;
      Launcher : Process_ID := No_Process;
      Started : Unsigned_64 := 0;
      --  What SPAWN gave procmgr for it: authority over it (kill, resume,
      --  launch block) and an untagged endpoint to it (badged copies for
      --  its clients when it provides a service; its port rings' and render
      --  session's recipient). Closed when it retires.
      Control : CuBit.Handles.Process := CuBit.Handles.No_Process;
      Mailbox : CuBit.Handles.Endpoint := H.No_Endpoint;
      --  procmgr resumed it. A launch withdrawn before that (refused, or a
      --  GPU attempt that a software start replaces) never ran: its exit
      --  is not a stop, a kill or a fault.
      Resumed : Boolean := False;
      --  Started Held by Start_Child (SUP-001): unrun until its supervisor
      --  releases it (releaseChild), once.
      Held : Boolean := False;
      --  Its Start_Child spec's stop grace in ms (0: none declared; a plan
      --  entry's is its (stop-grace-ms N)), for the root stop (PWR-003).
      Stop_Grace : Unsigned_32 := 0;
      --  Launch order, from 1: a later launch stops earlier (PWR-003).
      Sequence : Unsigned_64 := 0;
      --  When the root stop stops it: declared by its plan entry's
      --  (stop-stage ...) or its Start_Child spec; a program another
      --  program launched is an app.
      Stage : CuBit.Child_Specs.Stop_Stage := CuBit.Child_Specs.Apps;
      --  Its manifest declares the com.cubit-os.stdmetric outlet (MET-002):
      --  metrics.svc hears who it is, at its start and its exit.
      Metric_Outlet : Boolean := False;
      --  Launched into the user session (Launch-into-S): procmgr is its
      --  kernel parent and tells its requester when it ends (launch
      --  handles); the debug envelope counts it a session member.
      Session_Child : Boolean := False;
   end record;
   Empty_Process_Record : constant Process_Record := (others => <>);
   package Records is new CuBit.Identity_Tables
     (Element => Process_Record, Empty => Empty_Process_Record, Capacity_Bits => 9);
   --  Observer issuances never wrap: exhaustion denies the role.
   Next_Process_Issuance : Unsigned_32 := 1;
   --  The next launch's Sequence.
   Next_Launch_Sequence : Unsigned_64 := 1;

   --  PWR-003: a root stop (CuBit.System_Stops) has begun. From then on
   --  procmgr starts nothing (spawnByName refuses), restarts nothing, and
   --  answers a second stop Already_Stopping. It never ends: the machine
   --  goes off or halts.
   Shutting_Down : Boolean := False;

   --  Close the handles SPAWN gave procmgr for a child it no longer needs
   --  them for (it retired, or its launch failed).
   procedure closeChildHandles (Control : CuBit.Handles.Process; Mailbox : CuBit.Handles.Endpoint) is
      Ignore : HIPC.Close_Result;
   begin
      if Valid (Control) then
         Ignore := HIPC.Close (Control);
      end if;
      if Valid (Mailbox) then
         Ignore := HIPC.Close (Mailbox);
      end if;
   end closeChildHandles;

   procedure noteProcess
     (pid, launcher : Process_ID; identity : String;
      Control : CuBit.Handles.Process; Mailbox : CuBit.Handles.Endpoint)
   is
      Length : constant Natural := Natural'Min (identity'Length, PO.Identity_Bytes);
      I : constant Records.Index := Records.Ensure (pid);
   begin
      if I = 0 then return; end if;
      Records.Values (I) := (Valid => True, Identity => [others => ' '], Identity_Length => Length,
                             Launcher => launcher, Started => CuBit.Monotonic.Milliseconds,
                             Control => Control, Mailbox => Mailbox, Resumed => False, Held => False,
                             Stop_Grace => 0, Sequence => Next_Launch_Sequence,
                             Stage => CuBit.Child_Specs.Apps, Metric_Outlet => False,
                             Session_Child => False);
      if Next_Launch_Sequence < Unsigned_64'Last then
         Next_Launch_Sequence := Next_Launch_Sequence + 1;
      end if;
      Records.Values (I).Identity (1 .. Length) := identity (identity'First .. identity'First + Length - 1);
   end noteProcess;

   --  procmgr's untagged endpoint to a process it started (null if none).
   function mailboxOf (pid : Process_ID) return CuBit.Handles.Endpoint is
      I : Records.Index;
   begin
      if pid = No_Process then
         return H.No_Endpoint;   --  e.g. a role nobody has registered
      end if;
      I := Records.Find (pid);
      return (if I /= 0 and then Records.Values (I).Valid then Records.Values (I).Mailbox
              else H.No_Endpoint);
   end mailboxOf;

   function Logstore_Collector return CuBit.Handles.Endpoint is
     (mailboxOf (Registered_Driver (DRIVER_LOGSTORE)));
   function Metrics_Collector return CuBit.Handles.Endpoint is
     (mailboxOf (Registered_Driver (CuBit.Metric_Protocol.Publisher_Service_Role)));

   --  List: the kernel's table joined with Records, written into one
   --  page the caller lends, from the process at First on (PO.List_Label's
   --  paging). Only a holder of the observer tag may ask.
   procedure handleProcessList (sender : Process_ID; msg : Message) is
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped : System.Address;
      Ok, Returned : Boolean;
      Count, Written, Total : Natural := 0;
      First : constant Natural :=
        (if msg.tag.length >= PO.Paged_Request_Words
         then Natural (Unsigned_64'Min (msg.words (PO.First_Record_Word), Unsigned_64 (Natural'Last)))
         else 0);
      replyMsg : Message := NULL_MESSAGE;
      ignore : Unsigned_64;
      procedure Answer (Result : PO.Status) is
      begin
         replyMsg.tag := (label => Unsigned_32 (PO.Status'Enum_Rep (Result)), length => 3, protocol => 0, ipc => <>, reserved => 0);
         replyMsg.words := [0 => Unsigned_64 (PO.Status'Enum_Rep (Result)), 1 => Unsigned_64 (Written),
                            2 => Unsigned_64 (Total), others => 0];
         ignore := reply (sender, replyMsg);
      end Answer;
   begin
      if not PO.Is_Observer (msg.authorityTag) then
         Answer (PO.Denied);
         return;
      end if;
      if msg.tag.length < 1 or else not CuBit.Grant_References.Valid_Wire (msg.words (0)) then
         Answer (PO.Invalid_Request);
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (Reference, sender, 0, PO.Page_Bytes, CuBit.Memory_Grants.Write_Access, Mapped, Ok);
      if not Ok then
         Answer (PO.Invalid_Request);
         return;
      end if;
      declare
         Page : array (0 .. PO.Page_Bytes - 1) of Unsigned_8 with Import, Address => Mapped;
         Listed : constant Unsigned_64 :=
           HC.Process_List (PH.Process_List_Object, processList'Address, Process_List_Bytes);
         procedure Put (At_Byte : Natural; Value : Unsigned_64; Bytes : Positive) is
         begin
            for I in 0 .. Bytes - 1 loop
               Page (At_Byte + I) := Unsigned_8 (Shift_Right (Value, 8 * I) and 16#FF#);
            end loop;
         end Put;
      begin
         Page := [others => 0];
         if Listed /= Unsigned_64'Last then
            Count := Natural (Unsigned_64'Min (Listed, Process_List_Bytes / Process_Entry_Bytes));
         end if;
         for I in 0 .. Count - 1 loop
            declare
               Item : constant CuBit.Process_List.Process_Entry :=
                 CuBit.Process_List.Get (processList'Address, I);
               Pid : constant Process_ID := Item.Identity;
               Name : constant String := CuBit.Process_List.Name_Of (Item);
               Name_Length : constant Natural := Name'Length;
               SUSPENDED_STATE : constant := 10;
               Known_Index : constant Records.Index := Records.Find (Pid);
            begin
               --  Reserved slots are suspended and never named.
               if not (Name_Length = 0 and then Item.State = SUSPENDED_STATE) then
                  Total := Total + 1;
                  --  Total - 1 is this process's position in the listing.
                  if Total > First and then Written < PO.Page_Records then
                     declare
                        R : constant Natural := Written * PO.Record_Bytes;
                     begin
                        Put (R + PO.Pid_Offset, To_Word (Pid), PO.Pid_Bytes);
                        Page (R + PO.State_Offset) := Item.State;
                        Page (R + PO.CPU_Offset) := Item.CPU;
                        Page (R + PO.Class_Offset) := Item.Class;
                        Put (R + PO.Frames_Offset, Unsigned_64 (Item.Frames), 4);
                        Page (R + PO.Name_Length_Offset) := Unsigned_8 (Name_Length);
                        for J in 1 .. Name_Length loop
                           Page (R + PO.Name_Offset + J - 1) := Character'Pos (Name (Name'First + J - 1));
                        end loop;
                        if Known_Index /= 0 and then Records.Values (Known_Index).Valid then
                           declare
                              Known : Process_Record renames Records.Values (Known_Index);
                           begin
                              Put (R + PO.Launcher_Offset, To_Word (Known.Launcher), PO.Pid_Bytes);
                              Put (R + PO.Started_Offset, Known.Started, 8);
                              Page (R + PO.Identity_Length_Offset) := Unsigned_8 (Known.Identity_Length);
                              for J in 1 .. Known.Identity_Length loop
                                 Page (R + PO.Identity_Offset + J - 1) := Character'Pos (Known.Identity (J));
                              end loop;
                           end;
                        end if;
                        Written := Written + 1;
                     end;
                  end if;
               end if;
            end;
         end loop;
      end;
      CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
      Answer ((if Count = 0 then PO.Unavailable else PO.OK));
   end handleProcessList;

   procedure clearAuthorityForPID (pid : Process_ID) is
   begin
      for item of authorityRecords loop
         if item.valid and then item.pid = pid then
            item.valid := False;
         end if;
      end loop;
   end clearAuthorityForPID;

   ---------------------------------------------------------------------------
   --  Heap capacity and IPC transfer size are independent. Keep the small
   --  permanent FS grant for paths/ACLs; lend file destinations in windows.
   ---------------------------------------------------------------------------
   function ensureBuffer (needed : Unsigned_64) return Boolean is
      newPages      : Natural;
      extra         : Natural;
      ret           : Unsigned_64;
   begin
      if needed <= Unsigned_64 (bufCapacity) then
         return True;
      end if;

      if needed > Unsigned_64 (Natural'Last / PAGE_SIZE * PAGE_SIZE) then
         debugPrint ("procmgr: image exceeds addressable buffer size" & LF);
         return False;
      end if;

      --  Round up to page-aligned size
      newPages := Natural ((needed + Unsigned_64 (PAGE_SIZE) - 1) /
                           Unsigned_64 (PAGE_SIZE));
      extra := newPages - bufPages;

      debugPrint ("procmgr: growing buffer to ");
      printDec (Unsigned_32 (newPages));
      debugPrint (" pages" & LF);

      if extra > 0 then
         ret := syscall (SYSCALL_SBRK, Unsigned_64 (extra) *
                         Unsigned_64 (PAGE_SIZE));
         if ret = Unsigned_64'Last then
            debugPrint ("procmgr: sbrk grow failed" & LF);
            return False;
         end if;

         bufPages := newPages;
      end if;
      bufCapacity := newPages * PAGE_SIZE;
      return True;
   end ensureBuffer;

   ---------------------------------------------------------------------------
   --  readFileFromFS - open and read an entire file via FS IPC
   --  Reads directly into imageBuf through page-aligned, bounded loans.
   --  Returns number of bytes read, or 0 on failure.
   ---------------------------------------------------------------------------
   --  Every filesystem request procmgr makes has a deadline (IPC-000 rule
   --  6: never Wait_Forever in the startup path). One request is an open,
   --  a 1 MiB window or a close. It must outlast the storage driver's own
   --  bounded recovery of one slow command, or procmgr gives up on a read
   --  the driver is about to deliver: nvme.drv's is two 30 s attempts
   --  around an abort and a controller reset (BUG-003, NVMe_Recovery).
   File_Request_Deadline_Ms : constant := 120_000;
   --  A launch slower than this logs where its time went (Spawn_Attempt).
   Slow_Spawn_Ms : constant := 500;
   --  A file read at boot before the plan runs (the startup profile, the
   --  generation pointer and record): a missing or unreadable root must
   --  stop startup with a named error, never a silent wait (SYS-002). A
   --  missing file is answered at once; only storage that is still working
   --  on the read (recovering a slow command) uses the whole deadline.
   Boot_Read_Deadline_Ms : constant := File_Request_Deadline_Ms;

   --  The whole file Name into imageBuf; its size, or 0 when it could not be
   --  read (absent, unreadable, or a request past Within_Ms: Timed_Out).
   Last_Read_Timed_Out : Boolean := False;
   function readFileFromFS (name : String; Within_Ms : Unsigned_64) return Unsigned_64
   is
      msg       : Message;
      tag       : MessageTag;
      handle    : CuBit.Filesystems.File_Handle;
      fileSize  : Unsigned_64;
      Complete : Boolean := False;

      procedure Transfer
        (Offset : Natural; Count : Positive;
         Transferred : out Natural; Success : out Boolean)
      is
         Loan : CuBit.Memory_Grants.Grant_Reference;
         Ok : Boolean;
      begin
         Transferred := 0;
         Success := False;
         CuBit.Memory_Grants.Create_Via_Capability
           (slot => PH.Filesystem,
            localAddr => imageBuf + Storage_Offset (Offset),
            numPages => (Count + PAGE_SIZE - 1) / PAGE_SIZE,
            readWrite => True, reference => Loan, success => Ok);
         if not Ok then
            debugPrint ("procmgr: file window grant failed" & LF);
            return;
         end if;
         msg := CuBit.Filesystems.Read_Request (handle, Loan, Unsigned_64 (Count));
         tag := capCall (PH.Filesystem, msg, Deadline_After (Within_Ms));
         Last_Read_Timed_Out := Last_Read_Timed_Out or else tag.label = REPLY_TIMEOUT;
         CuBit.Memory_Grants.Revoke (Loan, Ok);
         if not Ok or else not CuBit.Memory_Grants.Retirement_Confirmed (Loan) then
            fileBufferUsable := False;
            debugPrint ("procmgr: file window retirement failed; buffer quarantined" & LF);
            return;
         end if;
         if tag.label /= REPLY_OK or else msg.words (0) /= Unsigned_64 (Count) then
            debugPrint ("procmgr: incomplete file window, refusing image" & LF);
            return;
         end if;
         Transferred := Count;
         Success := True;
      end Transfer;
      package Reader is new Windowed_Reads (1024 * 1024, Transfer);
   begin
      Last_Read_Timed_Out := False;
      if not fileBufferUsable then
         return 0;
      end if;
      if name'Length = 0 or else
         name'Length > CuBit.Filesystems.MAXIMUM_PATH_BYTES
      then
         debugPrint ("procmgr: invalid filesystem path length" & LF);
         return 0;
      end if;

      --  Write filename into elfBuf (grant buffer); overwritten by OP_READ
      declare
         grantBuf : array (0 .. name'Length - 1) of Unsigned_8 with
            Import, Address => elfBuf;
      begin
         for i in 0 .. name'Length - 1 loop
            grantBuf (i) := Unsigned_8 (
               Character'Pos (name (name'First + i)));
         end loop;
      end;

      msg := CuBit.Filesystems.Open_Request
        (fsGrant, CuBit.Filesystems.Nonempty_Path_Byte_Count (name'Length));
      tag := capCall (PH.Filesystem, msg, Deadline_After (Within_Ms));
      Last_Read_Timed_Out := tag.label = REPLY_TIMEOUT;

      if tag.label /= REPLY_OK then
         debugPrint ("procmgr: OP_OPEN failed" & LF);
         return 0;
      end if;

      handle   := CuBit.Filesystems.File_Handle (msg.words (0));
      fileSize := msg.words (1);

      if fileSize > 0 and then fileSize <= Unsigned_64'Last - EXCHANGE_BYTES and then
        ensureBuffer (fileSize + EXCHANGE_BYTES)
      then
         --  The 1 MiB windows are page multiples; only the last is short.
         --  A short backend read is a failure, never a partially loaded ELF.
         Complete := Reader.Read_All (Natural (fileSize));
      end if;

      --  Close file handle
      msg := CuBit.Filesystems.Close_Request (handle);
      tag := capCall (PH.Filesystem, msg, Deadline_After (Within_Ms));
      Last_Read_Timed_Out := Last_Read_Timed_Out or else tag.label = REPLY_TIMEOUT;

      return (if Complete and then tag.label = REPLY_OK then fileSize else 0);
   end readFileFromFS;

   ---------------------------------------------------------------------------
   --  ELF and manifest constants
   ---------------------------------------------------------------------------
   MANIFEST_MAGIC   : constant Unsigned_32 := 16#43424954#;  -- "CBIT" LE
   ID_MAGIC         : constant Unsigned_32 := 16#44494243#;  -- "CBID" LE

   --  ELF64 header and section header fields procmgr reads.
   ELF_HEADER_BYTES     : constant := 64;
   E_SHOFF_AT           : constant := 40;
   E_SHENTSIZE_AT       : constant := 58;
   E_SHNUM_AT           : constant := 60;
   SECTION_HEADER_BYTES : constant := 64;
   SH_TYPE_AT           : constant := 4;
   SH_OFFSET_AT         : constant := 24;
   SH_SIZE_AT           : constant := 32;

   --  .cubit.caps: magic, version (u16), count (u16), then 16-byte entries
   --  (kind, rights, reserved u16, parameter u32, parameter u64).
   CAPS_HEADER_BYTES : constant := 8;
   CAPS_ENTRY_BYTES  : constant := 16;

   --  Manifest request kinds (CCL.Manifests.Model.Request_Kind codes).
   REQ_FRAMEBUFFER  : constant Unsigned_8 := 1;
   REQ_SERVICE      : constant Unsigned_8 := 2;
   REQ_NOTIFICATION : constant Unsigned_8 := 7;

   --  Stream notification label
   OP_STREAM_AVAILABLE : constant Unsigned_32 := 16#0706#;

   --  Config store IPC label
   OP_CONFIG_GET : constant Unsigned_32 := 16#0600#;

   --  ELF section header type for PROGBITS
   SHT_PROGBITS     : constant Unsigned_32 := 1;

   --  Capability kinds, for the authority ledger (Capability_ABI.Kind codes).
   CAP_TYPE_ENDPOINT      : constant Unsigned_64 := 1;
   CAP_TYPE_NOTIFICATION  : constant Unsigned_64 := 2;
   CAP_TYPE_PROCESS       : constant Unsigned_64 := 6;
   CAP_TYPE_DEVICE_MEM    : constant Unsigned_64 := 7;
   CAP_TYPE_FIRMWARE_SNAPSHOT : constant Unsigned_64 :=
     CuBit.Handles.ABI.Kind'Enum_Rep (CuBit.Handles.ABI.Firmware_Snapshot);
   CAP_TYPE_PLATFORM_POWER : constant Unsigned_64 :=
     CuBit.Handles.ABI.Kind'Enum_Rep (CuBit.Handles.ABI.Platform_Power);
   CAP_TYPE_TRACE : constant Unsigned_64 :=
     CuBit.Handles.ABI.Kind'Enum_Rep (CuBit.Handles.ABI.Trace);
   CAP_TYPE_TEST_CONSOLE : constant Unsigned_64 :=
     CuBit.Handles.ABI.Kind'Enum_Rep (CuBit.Handles.ABI.Test_Console);
   CAP_TYPE_VM_CREATE : constant Unsigned_64 :=
     CuBit.Handles.ABI.Kind'Enum_Rep (CuBit.Handles.ABI.VM_Create);
   CAP_TYPE_SCHEDULING : constant Unsigned_64 :=
     CuBit.Handles.ABI.Kind'Enum_Rep (CuBit.Handles.ABI.Scheduling);
   CAP_TYPE_IO_PORTS : constant Unsigned_64 :=
     CuBit.Handles.ABI.Kind'Enum_Rep (CuBit.Handles.ABI.IO_Ports);
   --  A publication page is no capability: recorded as none, its
   --  publication code in the record's parameter.
   CAP_TYPE_PUBLICATION : constant Unsigned_64 :=
     CuBit.Handles.ABI.Kind'Enum_Rep (CuBit.Handles.ABI.No_Capability);

   ---------------------------------------------------------------------------
   --  The ACPI fixed-hardware I/O windows (coordination/acpi-region-boundary.md,
   --  slice 1): the FADT's PM1, PM timer and GPE blocks and the ECDT's EC
   --  ports, read once through procmgr's firmware snapshot reader (the
   --  snapshot is sealed at boot). devmgr grants each window
   --  (CuBit.ACPI_Window_Exchange) unless other holders have its ports.
   ---------------------------------------------------------------------------
   ACPI_Windows_Read : Boolean := False;
   ACPI_Fixed : CuBit.ACPI_Fixed_Windows.Window_List;
   --  The most of a table read: past the FADT's last 64-bit block (ACPI 6.5's
   --  FADT is 276 bytes) and the ECDT's EC registers.
   ACPI_Table_Read_Bytes : constant := 512;

   function acpiWindows return CuBit.ACPI_Fixed_Windows.Window_List is
      package FS renames CuBit.Firmware_Snapshot;
      package AF renames CuBit.ACPI_Fixed_Windows;
      use type FS.Outcome, FS.Byte_Count;

      --  The first bytes (at most ACPI_Table_Read_Bytes) of the snapshot's
      --  table named Signature; Length 0 when there is none.
      procedure Read_Table
        (Tables : FS.Authority; Count : Unsigned_32; Signature : String;
         Into : out AF.Byte_Array; Length : out Natural)
      is
         Described : FS.Table_Description;
         Status : FS.Outcome;
      begin
         Into := [others => 0];
         Length := 0;
         for T in 1 .. FS.Table_ID (Count) loop
            FS.Describe (Tables, T, Described, Status);
            if Status = FS.Ok
              and then (for all I in FS.Signature_Index =>
                          Described.Signature (I) = Character'Pos (Signature (Signature'First + I - 1)))
            then
               declare
                  Wanted : constant Natural :=
                    Natural (FS.Byte_Count'Min (Described.Extent, FS.Byte_Count (Into'Length)));
                  Chunk : FS.Byte_Array (1 .. Wanted);
                  Copied : FS.Byte_Count;
               begin
                  FS.Copy_Part (Tables, T, 0, Chunk, Copied, Status);
                  if Status = FS.Ok and then Copied = FS.Byte_Count (Wanted) then
                     for I in 1 .. Wanted loop
                        Into (I) := Chunk (I);
                     end loop;
                     Length := Wanted;
                  end if;
               end;
               return;
            end if;
         end loop;
      end Read_Table;
   begin
      if not ACPI_Windows_Read then
         ACPI_Windows_Read := True;
         declare
            Tables : constant FS.Authority := FS.Held (PH.Firmware_Snapshot_Reader);
            Catalog : FS.Catalog_Info;
            Status : FS.Outcome;
            FADT, ECDT : AF.Byte_Array (1 .. ACPI_Table_Read_Bytes);
            FADT_Length, ECDT_Length : Natural := 0;
         begin
            FS.Info (Tables, Catalog, Status);
            if Status = FS.Ok and then Catalog.Ready then
               Read_Table (Tables, Catalog.Table_Count, AF.FADT_Signature, FADT, FADT_Length);
               Read_Table (Tables, Catalog.Table_Count, AF.ECDT_Signature, ECDT, ECDT_Length);
            end if;
            AF.Compute (FADT (1 .. FADT_Length), ECDT (1 .. ECDT_Length), ACPI_Fixed);
            debugPrint ("procmgr: ACPI fixed windows" & Natural'Image (ACPI_Fixed.Count) &
                        " (FADT" & Natural'Image (FADT_Length) & " bytes, ECDT" &
                        Natural'Image (ECDT_Length) & ")" & LF);
            if ACPI_Fixed.Count = 0 then
               Warnings.Warn ("procmgr: ACPI fixed windows 0 (snapshot " &
                              (if Status = FS.Ok and then Catalog.Ready then "ready" else "not ready") &
                              ", FADT" & Natural'Image (FADT_Length) & " bytes, ECDT" &
                              Natural'Image (ECDT_Length) & ")");
            end if;
         end;
      end if;
      return ACPI_Fixed;
   end acpiWindows;

   ---------------------------------------------------------------------------
   --  readU16 - read a little-endian Unsigned_16 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU16 (offset : Unsigned_64) return Unsigned_16 is
      val : Unsigned_16 with
         Import, Address => imageBuf + Storage_Offset (offset);
   begin
      return val;
   end readU16;

   ---------------------------------------------------------------------------
   --  readU32 - read a little-endian Unsigned_32 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU32 (offset : Unsigned_64) return Unsigned_32 is
      val : Unsigned_32 with
         Import, Address => imageBuf + Storage_Offset (offset);
   begin
      return val;
   end readU32;

   ---------------------------------------------------------------------------
   --  readU64 - read a little-endian Unsigned_64 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU64 (offset : Unsigned_64) return Unsigned_64 is
      val : Unsigned_64 with
         Import, Address => imageBuf + Storage_Offset (offset);
   begin
      return val;
   end readU64;

   ---------------------------------------------------------------------------
   --  readU8 - read a Unsigned_8 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU8 (offset : Unsigned_64) return Unsigned_8 is
      val : Unsigned_8 with
         Import, Address => imageBuf + Storage_Offset (offset);
   begin
      return val;
   end readU8;

   ---------------------------------------------------------------------------
   --  parseIdSection
   --  Parse the .cubit.id section from the ELF in elfBuf. Searches for
   --  the "id" key and copies its value into pkgId.
   ---------------------------------------------------------------------------
   procedure parseIdSection
     (elfSize  : Unsigned_64;
      pkgId    : out String;
      pkgIdLen : out Natural)
   is
      e_shoff     : Unsigned_64;
      e_shnum     : Unsigned_16;
   begin
      pkgIdLen := 0;

      if elfSize < 64 then
         return;
      end if;

      e_shoff := readU64 (40);
      e_shnum := readU16 (60);

      if readU16 (58) /= 64 or e_shoff = 0 or e_shnum = 0 then
         return;
      end if;

      if e_shoff + Unsigned_64 (e_shnum) * 64 > elfSize then
         return;
      end if;

      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase    : constant Unsigned_64 :=
               e_shoff + Unsigned_64 (i) * 64;
            sh_type   : constant Unsigned_32 := readU32 (shBase + 4);
            sh_offset : Unsigned_64;
            sh_size   : Unsigned_64;
         begin
            if sh_type = SHT_PROGBITS then
               sh_offset := readU64 (shBase + 24);
               sh_size   := readU64 (shBase + 32);

               if sh_size >= 8 and then
                  sh_offset + sh_size <= elfSize and then
                  readU32 (sh_offset) = ID_MAGIC
               then
                  declare
                     version : constant Unsigned_16 :=
                        readU16 (sh_offset + 4);
                     count   : constant Unsigned_16 :=
                        readU16 (sh_offset + 6);
                     pos     : Unsigned_64 := sh_offset + 8;
                  begin
                     if version /= 1 then
                        return;
                     end if;

                     --  Avoid count - 1 underflow for an empty identity
                     --  section; userspace range checks are suppressed.
                     if count > 0 then
                     for j in 0 .. Unsigned_16'(count - 1) loop
                        exit when pos + 3 > sh_offset + sh_size;

                        declare
                           kLen : constant Natural :=
                              Natural (readU8 (pos));
                           vLen : constant Natural :=
                              Natural (readU16 (pos + 1));
                        begin
                           pos := pos + 3;

                           exit when pos + Unsigned_64 (kLen) +
                                     Unsigned_64 (vLen) >
                                     sh_offset + sh_size;

                           --  Check if key is "id" (2 bytes)
                           if kLen = 2 and then
                              readU8 (pos) =
                                 Unsigned_8 (Character'Pos ('i'))
                              and then
                              readU8 (pos + 1) =
                                 Unsigned_8 (Character'Pos ('d'))
                           then
                              pkgIdLen := Natural'Min (
                                 vLen, pkgId'Length);
                              for c in 0 .. pkgIdLen - 1 loop
                                 pkgId (pkgId'First + c) :=
                                    Character'Val (Natural (readU8 (
                                       pos + Unsigned_64 (kLen) +
                                       Unsigned_64 (c))));
                              end loop;

                              debugPrint ("procmgr: pkg id=");
                              debugPrint (
                                 pkgId (pkgId'First ..
                                    pkgId'First + pkgIdLen - 1));
                              debugPrint ("" & LF);
                              return;
                           end if;

                           pos := pos + Unsigned_64 (kLen) +
                                  Unsigned_64 (vLen);
                        end;
                     end loop;
                     end if;

                     return;  -- only process first matching section
                  end;
               end if;
            end if;
         end;
      end loop;
   end parseIdSection;

   ---------------------------------------------------------------------------
   --  findDescription
   --  The program's .cubit.description (CuBit.Program_Descriptions) in the
   --  ELF in elfBuf: where it is and how long (0: none), whether it decodes,
   --  and the rings of its output ports (bit N: ring N, the port's position
   --  plus one) for OP_STREAM_AVAILABLE.
   ---------------------------------------------------------------------------
   procedure findDescription
     (elfSize : Unsigned_64; Found_At : out Unsigned_64;
      Found_Length : out CuBit.Program_Descriptions.Descriptor_Length;
      Valid : out Boolean; Rings : out Unsigned_64)
   is
      package PD renames CuBit.Program_Descriptions;
      use type PD.Connector_Direction;
      Magic : constant Unsigned_32 :=
        Unsigned_32 (PD.Magic_0) or Shift_Left (Unsigned_32 (PD.Magic_1), 8)
        or Shift_Left (Unsigned_32 (PD.Magic_2), 16)
        or Shift_Left (Unsigned_32 (PD.Magic_3), 24);
      e_shoff : Unsigned_64;
      e_shnum : Unsigned_16;
   begin
      Found_At := 0;
      Found_Length := 0;
      Valid := False;
      Rings := 0;
      if elfSize < 64 then
         return;
      end if;
      e_shoff := readU64 (40);
      e_shnum := readU16 (60);
      if readU16 (58) /= 64 or else e_shoff = 0 or else e_shnum = 0
        or else e_shoff > elfSize
        or else Unsigned_64 (e_shnum) > (elfSize - e_shoff) / 64
      then
         return;
      end if;
      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase    : constant Unsigned_64 := e_shoff + Unsigned_64 (i) * 64;
            sh_offset : constant Unsigned_64 := readU64 (shBase + 24);
            sh_size   : constant Unsigned_64 := readU64 (shBase + 32);
         begin
            if readU32 (shBase + 4) = SHT_PROGBITS
              and then sh_size >= PD.Header_Bytes
              and then sh_offset <= elfSize
              and then sh_size <= elfSize - sh_offset
              and then readU32 (sh_offset) = Magic
            then
               if sh_size > PD.Maximum_Descriptor_Bytes then
                  Found_Length := PD.Maximum_Descriptor_Bytes;   --  present, invalid
                  return;
               end if;
               Found_At := sh_offset;
               Found_Length := Natural (sh_size);
               declare
                  Source : constant PD.Bytes (1 .. Found_Length)
                    with Import, Address => imageBuf + Storage_Offset (sh_offset);
                  Copy : constant PD.Bytes (1 .. Found_Length) := Source;
                  Decoded : PD.Signature;
               begin
                  PD.Decode (Copy, Decoded, Valid);
                  if Valid then
                     for P in 0 .. Decoded.Connector_Total - 1 loop
                        if Decoded.Connectors (P).Direction = PD.Outlet then
                           Rings := Rings or
                             Shift_Left (Unsigned_64'(1), Natural (PD.Ring_Id (P)));
                        end if;
                     end loop;
                  end if;
               end;
               return;
            end if;
         end;
      end loop;
   end findDescription;

   ---------------------------------------------------------------------------
   --  findParameters
   --  The program's .cubit.parameters (CuBit.Parameter_Signatures) in the
   --  ELF in elfBuf: where it is and how long (0: the program declares no
   --  parameters). One too long to be a descriptor is reported one byte
   --  over the limit, so it fails validation.
   ---------------------------------------------------------------------------
   procedure findParameters
     (elfSize : Unsigned_64; Found_At : out Unsigned_64; Found_Length : out Natural)
   is
      package PS renames CuBit.Parameter_Signatures;
      Magic : constant Unsigned_32 :=
        Unsigned_32 (PS.Magic_0) or Shift_Left (Unsigned_32 (PS.Magic_1), 8)
        or Shift_Left (Unsigned_32 (PS.Magic_2), 16)
        or Shift_Left (Unsigned_32 (PS.Magic_3), 24);
      e_shoff : Unsigned_64;
      e_shnum : Unsigned_16;
   begin
      Found_At := 0;
      Found_Length := 0;
      if elfSize < 64 then
         return;
      end if;
      e_shoff := readU64 (40);
      e_shnum := readU16 (60);
      if readU16 (58) /= 64 or else e_shoff = 0 or else e_shnum = 0
        or else e_shoff > elfSize
        or else Unsigned_64 (e_shnum) > (elfSize - e_shoff) / 64
      then
         return;
      end if;
      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase    : constant Unsigned_64 := e_shoff + Unsigned_64 (i) * 64;
            sh_offset : constant Unsigned_64 := readU64 (shBase + 24);
            sh_size   : constant Unsigned_64 := readU64 (shBase + 32);
         begin
            if readU32 (shBase + 4) = SHT_PROGBITS
              and then sh_size >= PS.Magic_Bytes
              and then sh_offset <= elfSize
              and then sh_size <= elfSize - sh_offset
              and then readU32 (sh_offset) = Magic
            then
               Found_At := sh_offset;
               Found_Length := Natural (Unsigned_64'Min (sh_size, PS.Maximum_Descriptor_Bytes + 1));
               return;
            end if;
         end;
      end loop;
   end findParameters;

   ---------------------------------------------------------------------------
   --  findSection
   --  The first PROGBITS section of the ELF in elfBuf whose first four
   --  bytes are Magic (a .cubit.* section): where it is and how long,
   --  capped one byte over Maximum so an overlong one fails its decoder;
   --  length 0 when there is none.
   ---------------------------------------------------------------------------
   procedure findSection
     (elfSize : Unsigned_64; Magic : Unsigned_32; Maximum : Natural;
      Found_At : out Unsigned_64; Found_Length : out Natural)
   is
      e_shoff : Unsigned_64;
      e_shnum : Unsigned_16;
   begin
      Found_At := 0;
      Found_Length := 0;
      if elfSize < 64 then
         return;
      end if;
      e_shoff := readU64 (40);
      e_shnum := readU16 (60);
      if readU16 (58) /= 64 or else e_shoff = 0 or else e_shnum = 0
        or else e_shoff > elfSize
        or else Unsigned_64 (e_shnum) > (elfSize - e_shoff) / 64
      then
         return;
      end if;
      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase    : constant Unsigned_64 := e_shoff + Unsigned_64 (i) * 64;
            sh_offset : constant Unsigned_64 := readU64 (shBase + 24);
            sh_size   : constant Unsigned_64 := readU64 (shBase + 32);
         begin
            if readU32 (shBase + 4) = SHT_PROGBITS
              and then sh_size >= 4
              and then sh_offset <= elfSize
              and then sh_size <= elfSize - sh_offset
              and then readU32 (sh_offset) = Magic
            then
               Found_At := sh_offset;
               Found_Length := Natural (Unsigned_64'Min (sh_size, Unsigned_64 (Maximum) + 1));
               return;
            end if;
         end;
      end loop;
   end findSection;

   ---------------------------------------------------------------------------
   --  The manifest a child's ELF carries (CCL.Manifests): its declared
   --  startup table (.cubit.startup, CuBit.Startup_Tables) and the
   --  parameters of its requests (.cubit.caps). Declared entry i is request
   --  i; the requests of resource kinds (devices, DMA, scheduling) are in
   --  .cubit.resources are decoded separately for typed named ACPI windows.
   ---------------------------------------------------------------------------
   --  pid now holds an endpoint for a service role with manifest rights
   --  Kept (bit 0 Read, bit 1 Write): a child it launches may ask for that
   --  role with at most those rights (Launch_Attenuation).
   procedure noteService (pid : Process_ID; role : Unsigned_32; Kept : Unsigned_64) is
   begin
      if launchIndex (pid) /= 0 and then role < TRACKED_SERVICE_ROLES then
         Launches.Values (launchIndex (pid)).Services (Natural (role)) := LAtt.With_Endpoint
           (Launches.Values (launchIndex (pid)).Services (Natural (role)),
            LAtt.Rights_Mask (Kept and Unsigned_64 (LAtt.Read_Right + LAtt.Write_Right)));
      end if;
   end noteService;

   --  pid was granted the network scope Item: a child it launches may be
   --  given scopes within it.
   procedure noteNetwork (pid : Process_ID; Item : CuBit.Network_Authority.Scope) is
   begin
      if launchIndex (pid) /= 0 then
         declare
            Held : Launch_State renames Launches.Values (launchIndex (pid));
         begin
            if Held.Network_Count < MAXIMUM_HELD_NETWORK then
               Held.Network_Count := Held.Network_Count + 1;
               Held.Network (Held.Network_Count) := Item;
            end if;
         end;
      end if;
   end noteNetwork;

   --  Whether the launcher being attenuated for holds a network scope
   --  covering Requested's reach: its action, addresses, ports and name
   --  resolution. Not its channel count: that is a reservation netstack
   --  makes for each holder against its own capacity (Network_Grants), so
   --  a launcher need not reserve its children's channels as well.
   function launcherNetworkCovers (Requested : CuBit.Network_Authority.Scope) return Boolean is
     (launchIndex (attenuateFor) /= 0
      and then (for some I in 1 .. Launches.Values (launchIndex (attenuateFor)).Network_Count =>
                  CuBit.Network_Authority.Includes
                    (Launches.Values (launchIndex (attenuateFor)).Network (I),
                     (Requested with delta Connections =>
                        Natural'Min (Requested.Connections,
                                     Launches.Values (launchIndex (attenuateFor)).Network (I).Connections)))));

   --  Malformed: the image's section headers, its .cubit.caps or its
   --  .cubit.startup cannot be read, or they disagree, so its requests are
   --  unknown. The launch must fail then (never run the program with
   --  whatever was granted so far, or with nothing, as if it had asked for
   --  nothing). An image without sections, or without both sections, asks
   --  for nothing and is not malformed.
   type Manifest_Reading is
     (Manifest_Read, Image_Too_Short, Bad_Section_Header_Size,
      Section_Headers_Outside_Image, Unknown_Manifest_Version, Manifest_Truncated,
      Startup_Table_Missing, Startup_Table_Invalid, Requests_Disagree);
   subtype Manifest_Malformed is Manifest_Reading
     range Image_Too_Short .. Requests_Disagree;

   --  One .cubit.caps request.
   type Caps_Request is record
      Kind   : Unsigned_8 := 0;
      Rights : Unsigned_8 := 0;
      Param0 : Unsigned_32 := 0;
      Param1 : Unsigned_64 := 0;
   end record;
   type Caps_Array is array (ST.Entry_Index) of Caps_Request;
   type Request_Map is array (ST.Entry_Index) of ST.Entry_Count;

   type Manifest_View is record
      Declared   : ST.Table := ST.Empty_Table;
      Caps       : Caps_Array;
      Caps_Count : ST.Entry_Count := 0;
      CMOS_Declaration : CMOS_Windows.Declaration;
      --  Declared entry i's request in Caps; 0 for a resource kind.
      Request_Of : Request_Map := [others => 0];
      --  The manifest's first (request-scheduling NAME realtime ...): Budget
      --  microseconds every Period, from .cubit.resources; Period 0: none.
      Scheduling_Budget, Scheduling_Period : Unsigned_64 := 0;
   end record;

   --  The startup kinds whose requests are in .cubit.caps.
   function In_Caps (Kind : ST.Capability_Kind) return Boolean is
     (Kind in ST.Endpoint | ST.Notification | ST.Framebuffer | ST.Render | ST.Network
              | ST.Firmware_Snapshot | ST.ACPI_Hardware | ST.Publication
              | ST.Platform_Power | ST.Trace | ST.Test_Console | ST.VM_Create);

   procedure readManifest
     (elfSize : Unsigned_64;
      View    : out Manifest_View;
      reading : out Manifest_Reading)
   is
      e_shoff     : Unsigned_64;
      e_shentsize : Unsigned_16;
      e_shnum     : Unsigned_16;
      Caps_Found, Startup_Found : Boolean := False;
   begin
      View := (others => <>);
      reading := Manifest_Read;
      if elfSize < ELF_HEADER_BYTES then
         reading := Image_Too_Short;
         return;
      end if;
      e_shoff     := readU64 (E_SHOFF_AT);
      e_shentsize := readU16 (E_SHENTSIZE_AT);
      e_shnum     := readU16 (E_SHNUM_AT);
      if e_shentsize /= SECTION_HEADER_BYTES then
         reading := Bad_Section_Header_Size;
         return;
      end if;
      if e_shoff = 0 or else e_shnum = 0 then
         return;
      end if;
      if e_shoff > elfSize
        or else Unsigned_64 (e_shnum) > (elfSize - e_shoff) / SECTION_HEADER_BYTES
      then
         reading := Section_Headers_Outside_Image;
         return;
      end if;

      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase    : constant Unsigned_64 := e_shoff + Unsigned_64 (i) * SECTION_HEADER_BYTES;
            sh_offset : constant Unsigned_64 := readU64 (shBase + SH_OFFSET_AT);
            sh_size   : constant Unsigned_64 := readU64 (shBase + SH_SIZE_AT);
         begin
            if readU32 (shBase + SH_TYPE_AT) = SHT_PROGBITS
              and then sh_size >= CAPS_HEADER_BYTES
              and then sh_offset <= elfSize
              and then sh_size <= elfSize - sh_offset
            then
               if not Caps_Found and then readU32 (sh_offset) = MANIFEST_MAGIC then
                  Caps_Found := True;
                  declare
                     version : constant Unsigned_16 := readU16 (sh_offset + 4);
                     count   : constant Unsigned_16 := readU16 (sh_offset + 6);
                  begin
                     if version /= 1 then
                        reading := Unknown_Manifest_Version;
                        return;
                     end if;
                     if Natural (count) > ST.Maximum_Entries
                       or else sh_size < CAPS_HEADER_BYTES + Unsigned_64 (count) * CAPS_ENTRY_BYTES
                     then
                        reading := Manifest_Truncated;
                        return;
                     end if;
                     View.Caps_Count := Natural (count);
                     for j in 1 .. View.Caps_Count loop
                        declare
                           At_Byte : constant Unsigned_64 :=
                             sh_offset + CAPS_HEADER_BYTES + Unsigned_64 (j - 1) * CAPS_ENTRY_BYTES;
                        begin
                           View.Caps (j) :=
                             (Kind   => readU8 (At_Byte),
                              Rights => readU8 (At_Byte + 1),
                              Param0 => readU32 (At_Byte + 4),
                              Param1 => readU64 (At_Byte + 8));
                        end;
                     end loop;
                  end;
               elsif not Startup_Found and then readU32 (sh_offset) = ST.Magic then
                  Startup_Found := True;
                  if sh_size > ST.Maximum_Table_Bytes then
                     reading := Startup_Table_Invalid;
                     return;
                  end if;
                  declare
                     Length : constant ST.Table_Length := Natural (sh_size);
                     Source : constant ST.Bytes (1 .. Length)
                       with Import, Address => imageBuf + Storage_Offset (sh_offset);
                     --  One read of the image's bytes (single fetch).
                     Copy : constant ST.Bytes (1 .. Length) := Source;
                     Outcome : ST.Decode_Result;
                     use type ST.Decode_Result;
                  begin
                     ST.Decode (Copy, View.Declared, Outcome);
                     if Outcome /= ST.Decoded or else not ST.Declaration (View.Declared) then
                        reading := Startup_Table_Invalid;
                        return;
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;

      if Caps_Found and then not Startup_Found then
         reading := Startup_Table_Missing;
         return;
      end if;
      --  Entry i of a capability kind is the next .cubit.caps request, and
      --  its kind agrees; every request is some entry's.
      declare
         Next : ST.Entry_Count := 0;
      begin
         for I in 1 .. View.Declared.Count loop
            if In_Caps (View.Declared.Entries (I).Kind) then
               if Next = View.Caps_Count then
                  reading := Requests_Disagree;
                  return;
               end if;
               Next := Next + 1;
               View.Request_Of (I) := Next;
               declare
                  Kind : constant Unsigned_8 := View.Caps (Next).Kind;
               begin
                  if (case View.Declared.Entries (I).Kind is
                        when ST.Endpoint     =>
                          Kind not in REQ_SERVICE | CuBit.Self_Offers.Manifest_Request,
                        when ST.Notification => Kind /= REQ_NOTIFICATION,
                        when ST.Framebuffer  => Kind /= REQ_FRAMEBUFFER,
                        when ST.Render       => Kind /= CuBit.Render_Authority.Manifest_Request,
                        when ST.Network      => Kind /= CuBit.Network_Authority.Manifest_Request,
                        when ST.Firmware_Snapshot =>
                          Kind /= CuBit.Firmware_Snapshot_Authority.Manifest_Request,
                        when ST.ACPI_Hardware =>
                          Kind /= CuBit.ACPI_Hardware_Authority.Manifest_Request,
                        when ST.Publication =>
                          Kind /= CuBit.Publication_Grants.Manifest_Request,
                        when ST.Platform_Power =>
                          Kind /= CuBit.Platform_Power_Authority.Manifest_Request,
                        when ST.Trace =>
                          Kind /= CuBit.Trace_Authority.Manifest_Request,
                        when ST.Test_Console =>
                          Kind /= CuBit.Test_Console_Authority.Manifest_Request,
                        when ST.VM_Create =>
                          Kind /= CuBit.VM_Create_Authority.Manifest_Request,
                        when others          => True)
                  then
                     reading := Requests_Disagree;
                     return;
                  end if;
               end;
            end if;
         end loop;
         if Next /= View.Caps_Count then
            reading := Requests_Disagree;
         end if;
      end;
      -- CMOS is a typed device resource, never a fabricated .caps request.
      -- Copy each canonical section once, after ordinary request-map validation.
      if reading = Manifest_Read and then Startup_Found then
         declare
            package RS renames CCL.Resource_Sections;
            package DS renames CCL.Device_Sections;
            Resource_At, Device_At : Unsigned_64;
            Resource_Length, Device_Length : Natural;
         begin
            findSection (elfSize, RS.MAGIC, RS.MAX_SECTION_BYTES, Resource_At, Resource_Length);
            findSection (elfSize, DS.MAGIC, DS.MAX_SECTION_BYTES, Device_At, Device_Length);
            declare
               Resource_Source : constant RS.Byte_Array (1 .. Resource_Length)
                 with Import, Address => imageBuf + Storage_Offset (Resource_At);
               Device_Source : constant DS.Byte_Array (1 .. Device_Length)
                 with Import, Address => imageBuf + Storage_Offset (Device_At);
               Resource_Copy : constant RS.Byte_Array := Resource_Source;
               Device_Copy : constant DS.Byte_Array := Device_Source;
            begin
               View.CMOS_Declaration := CMOS_Windows.Inspect
                 (View.Declared, Resource_Copy, Device_Copy);
               if Resource_Length >= 1 then
                  declare
                     Plan : RS.Section_Plan;
                     Status : RS.Decode_Status;
                     use type RS.Decode_Status;
                     use type RS.Resource_Kind;
                  begin
                     RS.Decode (Resource_Copy, Plan, Status);
                     if Status = RS.Decoded then
                        for I in 1 .. Plan.Count loop
                           if Plan.Entries (I).Kind = RS.Scheduling then
                              View.Scheduling_Budget := Plan.Entries (I).Amount;
                              View.Scheduling_Period := Plan.Entries (I).Extra;
                              exit;
                           end if;
                        end loop;
                     end if;
                  end;
               end if;
            end;
         end;
      end if;
   end readManifest;

   ---------------------------------------------------------------------------
   --  Service providers: an untagged endpoint (Grant kept) to the service
   --  registered for a role, from which procmgr badges or copies its
   --  children's endpoints. Fixed ones come from devmgr's handles; a
   --  registered service procmgr started is reached through its Mailbox;
   --  one devmgr started is asked of devmgr (CuBit.Service_Exchange) and
   --  kept while the same process holds the role.
   ---------------------------------------------------------------------------
   --  The kernel registry's role bound (System_Publications.Maximum_Role).
   Maximum_Role : constant := 127;
   subtype Service_Role is Unsigned_64 range 0 .. Maximum_Role;
   type Provider is record
      Service : CuBit.Handles.Endpoint := H.No_Endpoint;
      --  No_Process: one of devmgr's handles, valid for procmgr's lifetime.
      Holder  : Process_ID := No_Process;
   end record;
   Providers : array (Service_Role) of Provider;

   --  How long a launch waits for the provider of a role procmgr does not
   --  launch itself (devmgr's drivers, the free roles), whose start it does
   --  not see; and how often a waiting launch looks again. Bounded by an
   --  explicit deadline, never forever. A provider procmgr launched is
   --  waited for until its own ready deadline instead (Registrant_Ready_By).
   Registration_Wait_Ms : constant := 1_000;
   Registration_Poll_Ms : constant := 50;

   --  The roles only procmgr lets a program register (registrationAllowed's
   --  identity-gated roles, granted from a manifest's registration
   --  notification). Every process that may provide one is a Registrant.
   --  Other roles' providers may be devmgr's (gpu, input), whose start
   --  procmgr does not see.
   function Launched_Service_Role (Role : Service_Role) return Boolean is
     (Role = DRIVER_LOGSTORE or else Role = DRIVER_DESKTOP or else
      Role = DRIVER_DISPLAY or else Role = DRIVER_CCL_TEST or else
      Role = DRIVER_CLOCK or else Role = CuBit.TLS_Protocol.Service_Role or else
      Role = CuBit.Metric_Protocol.Publisher_Service_Role or else Role = DRIVER_LEXICON or else
      Role = DRIVER_ACPI or else Role = CuBit.Installer_Protocol.Service_Role or else
      Role = CuBit.Startup_Wait_Test.Service_Role or else
      Role = CuBit.Llm_Protocol.Service_Role or else Role = CuBit.Secrets_Protocol.Service_Role or else
      Role = CuBit.Power_Requests.Service_Role);

   --  For each launched-service role: the live process procmgr started with
   --  its registration, until that process exits.
   Registrants : array (Service_Role) of Process_ID := [others => No_Process];
   --  And when (monotonic milliseconds) it must have registered by: its
   --  start plus its startup entry's (ready-deadline-ms N). An entry that
   --  declares none has the plan's named ceiling,
   --  CCL.Configurations.MAX_READY_DEADLINE_MS (Launch_Entry's default).
   Registrant_Ready_By : array (Service_Role) of Unsigned_64 := [others => 0];

   --  The ready deadline of a launch the startup plan did not make (OP_LAUNCH,
   --  Start_Child), which cannot say (ready-deadline-ms N): the plan's ceiling.
   Unplanned_Ready_Deadline : constant CCL.Configurations.Ready_Deadline :=
     CCL.Configurations.MAX_READY_DEADLINE_MS;

   ---------------------------------------------------------------------------
   --  Launches waiting for a starting provider (CAP-003). A service entry
   --  whose provider has started but not registered yet does not refuse the
   --  launch: the launch is parked before its child is spawned, the service
   --  loop goes on, and the launch runs once every awaited provider has
   --  registered. A required entry refuses it, typed, when its provider ends
   --  first or misses its deadline; an optional one is then started without.
   --  A required service nothing started provides refuses it at once.
   ---------------------------------------------------------------------------
   type Provider_State is
     (Provider_Registered,   --  resolve the entry now
      Provider_Starting,     --  started, alive, not registered, before its deadline
      No_Provider,           --  nothing procmgr started provides it
      Provider_Exited,       --  the provider awaited ended before registering
      Deadline_Passed);      --  it did not register by its deadline
   subtype Wait_Refusal is Provider_State range No_Provider .. Deadline_Passed;

   --  The refusal's stable name in the log (native enumeration images are
   --  positions), and what it means.
   function Refusal_Name (Why : Wait_Refusal) return String is
     (case Why is
        when No_Provider     => "NO_PROVIDER",
        when Provider_Exited => "PROVIDER_EXITED",
        when Deadline_Passed => "DEADLINE_PASSED");
   function Refusal_Text (Why : Wait_Refusal) return String is
     (case Why is
        when No_Provider     => "no started program provides it",
        when Provider_Exited => "its provider ended before it registered",
        when Deadline_Passed => "its provider did not register by its deadline");

   --  One service entry a parked launch waits for.
   type Awaited_Service is record
      Label    : ST.Name;
      Role     : Service_Role := 0;
      --  The registrant awaited; No_Process for a role procmgr does not
      --  launch, which only its deadline ends.
      Provider : Process_ID := No_Process;
      Deadline : Unsigned_64 := 0;   --  monotonic milliseconds
      Required : Boolean := True;
      Resolved : Boolean := False;
   end record;
   type Awaited_Services is array (ST.Entry_Index range <>) of Awaited_Service;
   type Awaited_Access is access Awaited_Services;
   procedure Free is new Ada.Unchecked_Deallocation (Awaited_Services, Awaited_Access);

   --  Where Role's provider is now, for a launch about to wait for it.
   procedure classifyProvider
     (Role : Service_Role; Now : Unsigned_64;
      State : out Provider_State; Provider : out Process_ID; Deadline : out Unsigned_64) is
   begin
      Provider := No_Process;
      Deadline := Now;
      if (Valid (Providers (Role).Service) and then Providers (Role).Holder = No_Process)
        or else Registered_Driver (Role) /= No_Process
      then
         State := Provider_Registered;
      elsif Launched_Service_Role (Role) then
         if Registrants (Role) = No_Process then
            State := No_Provider;
         else
            Provider := Registrants (Role);
            Deadline := Registrant_Ready_By (Role);
            State := (if Now >= Deadline then Deadline_Passed else Provider_Starting);
         end if;
      else
         Deadline := Now + Registration_Wait_Ms;
         State := Provider_Starting;
      end if;
   end classifyProvider;

   --  Where an awaited provider is now.
   function awaitedState (Item : Awaited_Service; Now : Unsigned_64) return Provider_State is
     (if Registered_Driver (Item.Role) /= No_Process then Provider_Registered
      elsif Item.Provider /= No_Process and then Registrants (Item.Role) /= Item.Provider
      then Provider_Exited
      elsif Now >= Item.Deadline then Deadline_Passed
      else Provider_Starting);

   --  The role whose service answers a manifest's service request for Role.
   function Provider_Role_Of (Role : Unsigned_64) return Unsigned_64 is
     (if Role = CuBit.Audio_Control.Service_Role then DRIVER_MIXER
      elsif Role = CuBit.Clock_Control.Service_Role then DRIVER_CLOCK
      elsif Role = CuBit.Log_Protocol.Observer_Service_Role
        or else Role = CuBit.Log_Protocol.Control_Service_Role then DRIVER_LOGSTORE
      elsif Role = CuBit.Metric_Protocol.Observer_Service_Role
      then CuBit.Metric_Protocol.Publisher_Service_Role
      elsif Role = CuBit.Process_Observer.Observer_Service_Role
        or else Role = Session_Launch.Launch_Role
        or else Role = Session_Launch.Remote_Launch_Role
        or else Role = CuBit.Launch_Handles.Session_Debug_Role then DRIVER_PROCMGR
      elsif Role = CuBit.Disk_Install.Service_Role
        or else Role = ACPI_PCI_Window_Exchange.Service_Role then DRIVER_DEVMGR
      --  A read-only copy of the composed frame: Desktop composes it.
      elsif Role = CuBit.Display_Capture.Service_Role then DRIVER_DESKTOP
      --  PWR-003: power.svc's requesters, and power.svc's own endpoint to
      --  acpi.svc's power-button watch and S5.
      --  PWR-004: power.svc's read-only power-source observers.
      elsif Role = CuBit.Power_Requests.Desktop_Role
        or else Role = CuBit.Power_Requests.Console_Role
        or else Role = CuBit.Power_Sources.Observer_Role then CuBit.Power_Requests.Service_Role
      elsif Role = CuBit.Power_Event_Exchange.Service_Role then DRIVER_ACPI
      else Role);

   ---------------------------------------------------------------------------
   --  Whether the launcher being attenuated for holds what this manifest
   --  request asks (Launch_Attenuation): a service role through one
   --  endpoint with every right it asks, or as that role's own registered
   --  provider; an offerable self endpoint; render, when render was
   --  approved for the launcher; a network scope covering the one asked.
   --  Notifications, framebuffer, firmware, hardware and publications are
   --  never passed to a launched child.
   ---------------------------------------------------------------------------
   function launcherHolds
     (reqType : Unsigned_8; Param0 : Unsigned_32; Rights : Unsigned_8; Param1 : Unsigned_64)
     return Boolean
   is
      Index : constant Launches.Index := launchIndex (attenuateFor);
      Requested : CuBit.Network_Authority.Scope;
      Valid_Scope : Boolean;
   begin
      if Index = 0 then
         return False;
      elsif reqType = REQ_SERVICE then
         return Rights <= LAtt.Rights_Mask'Last
           and then LAtt.Service_Held
             ((if Param0 < TRACKED_SERVICE_ROLES
               then Launches.Values (Index).Services (Natural (Param0))
               else LAtt.None_Held),
              --  The provider of a role issues its own endpoint (never a
              --  role another service answers, such as display capture).
              Provider_Role_Of (Unsigned_64 (Param0)) = Unsigned_64 (Param0)
              and then Unsigned_64 (Param0) <= Maximum_Role
              and then Registered_Driver (Unsigned_64 (Param0)) = attenuateFor,
              LAtt.Rights_Mask (Rights));
      elsif reqType = CuBit.Self_Offers.Manifest_Request then
         --  An offerable self endpoint passes on only from a launcher that has one.
         return Launches.Values (Index).Offerable_Self;
      elsif reqType = CuBit.Render_Authority.Manifest_Request then
         return Launches.Values (Index).Render_Approved;
      elsif reqType = CuBit.Network_Authority.Manifest_Request then
         CuBit.Network_Authority.Decode (Unsigned_64 (Param0), Param1, Requested, Valid_Scope);
         return Valid_Scope and then launcherNetworkCovers (Requested);
      end if;
      return False;
   end launcherHolds;

   ---------------------------------------------------------------------------
   --  A declared request's kind for Launch-into-S (Session_Launch): what the
   --  session's envelope is checked against.
   ---------------------------------------------------------------------------
   function sessionKind
     (reqType : Unsigned_8; Param0 : Unsigned_32; Param1 : Unsigned_64)
     return Session_Launch.Request_Kind
   is
      package SLK renames Session_Launch;
      Role : constant Unsigned_64 := Unsigned_64 (Param0);
      Requested : CuBit.Network_Authority.Scope;
      Valid_Scope : Boolean;
   begin
      if reqType = REQ_SERVICE then
         if Role in SLK.Launch_Role | SLK.Remote_Launch_Role then
            return SLK.Launch_Badge;
         elsif Role = CuBit.Log_Protocol.Observer_Service_Role
           or else Role = CuBit.Log_Protocol.Control_Service_Role
           or else Role = CuBit.Metric_Protocol.Observer_Service_Role
           or else Role = CuBit.Process_Observer.Observer_Service_Role
           --  Debug-observe for the session (DBG-001): observation, within
           --  the session's debug envelope (Handle_Admission).
           or else Role = CuBit.Launch_Handles.Session_Debug_Role
         then
            return SLK.Observation;
         elsif Role = CuBit.Audio_Control.Service_Role
           or else Role = CuBit.Clock_Control.Service_Role
           or else Role = CuBit.Disk_Install.Service_Role
           or else Role = CuBit.Installer_Protocol.Service_Role
           or else Role = CuBit.Net_Manager_Control.Service_Role
           or else Role = CuBit.Display_Capture.Service_Role
           or else Role = CuBit.Power_Requests.Desktop_Role
           or else Role = CuBit.Power_Requests.Console_Role
           or else Role = CuBit.Power_Sources.Observer_Role
           or else Role = CuBit.Power_Event_Exchange.Service_Role
           or else Role = ACPI_PCI_Window_Exchange.Service_Role
         then
            return SLK.Startup_Only_Control;
         end if;
         return SLK.Service;
      elsif reqType = CuBit.Self_Offers.Manifest_Request then
         return SLK.Offerable_Self;
      elsif reqType = CuBit.Render_Authority.Manifest_Request then
         return SLK.Render;
      elsif reqType = CuBit.Network_Authority.Manifest_Request then
         CuBit.Network_Authority.Decode (Role, Param1, Requested, Valid_Scope);
         return (if Valid_Scope and then CuBit.Network_Authority.Includes
                   (CuBit.Network_Authority.Broad_Outbound_TCP, Requested)
                 then SLK.Outbound_Network else SLK.Other_Network);
      elsif reqType = REQ_FRAMEBUFFER then
         return SLK.Framebuffer;
      elsif reqType = REQ_NOTIFICATION then
         return SLK.Notification;
      end if;
      return SLK.Hardware;
   end sessionKind;

   --  The Launch badge the requester being served holds: the session's, the
   --  remote one, or none. A launched program declaring a Launch badge gets
   --  the same kind its requester holds, so a remote requester's children
   --  keep the remote badge (and any confirmation it comes to need).
   function requesterBadge return Session_Launch.Badge is
     (if attenuateFor = No_Process then Session_Launch.No_Badge
      elsif launcherHolds (REQ_SERVICE, Session_Launch.Launch_Role, LAtt.Read_Right + LAtt.Write_Right, 0)
      then Session_Launch.Session_Badge
      elsif launcherHolds (REQ_SERVICE, Session_Launch.Remote_Launch_Role,
                           LAtt.Read_Right + LAtt.Write_Right, 0)
      then Session_Launch.Remote_Badge
      else Session_Launch.No_Badge);

   --  Whether the launch being served may give this request: within the
   --  user session's envelope for a Launch-into-S (a Launch badge only when
   --  its requester introduces one it holds); within what the supervisor
   --  holds for a supervisor's Start_Child.
   function launchGives
     (reqType : Unsigned_8; Param0 : Unsigned_32; Rights : Unsigned_8; Param1 : Unsigned_64)
     return Boolean
   is
      Kind : constant Session_Launch.Request_Kind := sessionKind (reqType, Param0, Param1);
      use type Session_Launch.Request_Kind;
   begin
      if not launchSession then
         return launcherHolds (reqType, Param0, Rights, Param1);
      elsif Kind = Session_Launch.Launch_Badge then
         --  Introduced from the requester's own Launch badge, of either kind.
         return Session_Launch."/=" (requesterBadge, Session_Launch.No_Badge)
           and then Rights <= LAtt.Read_Right + LAtt.Write_Right;
      end if;
      return Rights <= Launch_Attenuation.Rights_Mask'Last
        and then Session_Launch.Within
          (Session_Launch.User_Session, Kind, Launch_Attenuation.Rights_Mask (Rights));
   end launchGives;


   --  Say why Program's launch is refused for its required service Label.
   procedure reportWaitRefusal
     (Program : String; Label : ST.Name; Role : Service_Role; Why : Wait_Refusal) is
   begin
      Warnings.Warn
        ("procmgr: launch of " & Program & " refused: required service """ &
         String (Label.Text (1 .. Label.Length)) & """ (role" & Unsigned_64'Image (Role) &
         ") " & Refusal_Name (Why) & ": " & Refusal_Text (Why) & " (CAP-003)");
   end reportWaitRefusal;

   procedure setFixedProviders is
   begin
      Providers (DRIVER_NETSTACK) := (PH.Netstack, No_Process);
      Providers (DRIVER_PROCMGR)  := (PH.Self_Grantable, No_Process);
      Providers (DRIVER_FS)       := (PH.Filesystem, No_Process);
      --  Inspection only: the broker-tagged endpoint never leaves procmgr.
      Providers (DRIVER_DEVMGR)   := (PH.Device_Inspection, No_Process);
      Providers (DRIVER_CONFIG)   := (PH.Config, No_Process);
      if Valid (PH.Mixer) then
         Providers (DRIVER_MIXER) := (PH.Mixer, No_Process);
      end if;
   end setFixedProviders;

   --  The registered provider of Role now, or none; it never waits. A
   --  launch waits for a starting provider before it gets here, parked
   --  (Spawn_Attempt's Await_Providers, resumeParked).
   function providerFor (Role : Service_Role) return CuBit.Handles.Endpoint is
      P : Provider renames Providers (Role);
      Registered : Process_ID := No_Process;
   begin
      if Valid (P.Service) and then P.Holder = No_Process then
         return P.Service;
      end if;
      Registered := Registered_Driver (Role);
      if Registered = No_Process then
         return H.No_Endpoint;
      elsif Valid (P.Service) and then P.Holder = Registered then
         return P.Service;
      end if;
      --  A service procmgr started: its own Mailbox, kept in its record.
      if Valid (mailboxOf (Registered)) then
         return mailboxOf (Registered);
      end if;
      --  A service devmgr started: ask devmgr; drop an older answer first.
      if Valid (P.Service) then
         declare
            Closed : HIPC.Close_Result;
         begin
            Closed := HIPC.Close (P.Service);
         end;
      end if;
      P := (others => <>);
      declare
         package SX renames CuBit.Service_Exchange;
         Request : Message := NULL_MESSAGE;
         Received : HIPC.Carried;
         Tag : MessageTag;
      begin
         Request.tag := (label => SX.Endpoint_Request, length => SX.Request_Words,
                         protocol => 0, ipc => <>, reserved => 0);
         Request.words (SX.Role_Word) := Role;
         Tag := HIPC.Call_Accepting
           (PH.Device_Manager, Request, CuBit.Messages.Wait_Forever, Received);
         if Tag.label = SX.Reply_OK then
            declare
               Taken : constant HIPC.Accepted_Endpoint := HIPC.Accept_Endpoint (Received);
            begin
               if Taken.OK then
                  P := (Taken.Value, Registered);
               end if;
            end;
         end if;
      end;
      return P.Service;
   end providerFor;

   --  The endpoint of one named provider of Role (a Start_Child binding's
   --  Provider, SUP-001): its Mailbox when procmgr started it, else devmgr's
   --  endpoint to that child (Service_Exchange.Endpoint_Request with the
   --  provider's identity), which Fetched says the caller must close.
   --  Several processes may serve one role (an I2C controller driver per
   --  controller); the registered one is only the last to register.
   function providerProcess
     (Role : Service_Role; Provider : Process_ID; Fetched : out Boolean) return CuBit.Handles.Endpoint
   is
      package SX renames CuBit.Service_Exchange;
      Request : Message := NULL_MESSAGE;
      Received : HIPC.Carried;
      Tag : MessageTag;
   begin
      Fetched := False;
      if Valid (mailboxOf (Provider)) then
         return mailboxOf (Provider);
      end if;
      Request.tag := (label => SX.Endpoint_Request, length => SX.Provider_Request_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words (SX.Role_Word) := Role;
      Request.words (SX.Provider_Word) := To_Word (Provider);
      Tag := HIPC.Call_Accepting
        (PH.Device_Manager, Request, CuBit.Messages.Wait_Forever, Received);
      if Tag.label = SX.Reply_OK then
         declare
            Taken : constant HIPC.Accepted_Endpoint := HIPC.Accept_Endpoint (Received);
         begin
            if Taken.OK then
               Fetched := True;
               return Taken.Value;
            end if;
         end;
      end if;
      declare
         Ignore : constant HIPC.Close_Result := HIPC.Close (Received);
      begin
         return H.No_Endpoint;
      end;
   end providerProcess;

   --  Offer devmgr the endpoint of a service procmgr started, for the roles
   --  devmgr uses (CuBit.Service_Exchange.Available).
   procedure offerToDeviceManager (Role : Service_Role; Service : CuBit.Handles.Endpoint) is
      package SX renames CuBit.Service_Exchange;
      Request : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      if not Valid (Service) then
         return;
      end if;
      Request.tag := (label => SX.Available, length => SX.Request_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words (SX.Role_Word) := Role;
      Tag := HIPC.Call_Offering
        (PH.Device_Manager, Request, CuBit.Messages.Wait_Forever,
         Copy (Service, Read_Write_Grant));
      if Tag.label /= SX.Reply_OK then
         debugPrint ("procmgr: devmgr did not take a service endpoint" & LF);
      end if;
   end offerToDeviceManager;

   ---------------------------------------------------------------------------
   --  Late introduction (CuBit.Late_Introduction, ABI-002): the launch
   --  decisions kept for optional service entries whose provider was not
   --  running (CuBit.Introduction_Ledgers), each holder's kept Await call, and the
   --  introductions made when a provider starts.
   ---------------------------------------------------------------------------
   package LI renames CuBit.Late_Introduction;
   package LIL renames CuBit.Introduction_Ledgers;
   use type LIL.Delivery;
   Late : LIL.Ledger := LIL.Empty;
   Late_Replies : array (LIL.Slot) of H.Reply := [others => H.No_Reply];

   --  devmgr is not launched from a manifest: its declaration, until it
   --  is, is this one. It takes logstore's and the desktop's untagged
   --  endpoints, offered (a call down to it), each time one starts.
   Device_Manager_Requests : constant array (1 .. 2) of Unsigned_64 :=
     [DRIVER_LOGSTORE, DRIVER_DESKTOP];
   Device_Manager_Kept : Process_ID := No_Process;

   procedure keepDeviceManagerRequests is
      Holder : constant Process_ID := Registered_Driver (DRIVER_DEVMGR);
      Into : LIL.Slot_Count;
   begin
      if Holder = No_Process or else Holder = Device_Manager_Kept then
         return;
      end if;
      if Device_Manager_Kept /= No_Process then
         LIL.Forget (Late, To_Word (Device_Manager_Kept));
      end if;
      for K in Device_Manager_Requests'Range loop
         LIL.Keep (Late, (Holder => To_Word (Holder), Index => Unsigned_64 (K),
                          Role => Device_Manager_Requests (K), Rights => 0, Tag => 0,
                          Way => LIL.Offer, Waiting => False), Into);
      end loop;
      Device_Manager_Kept := Holder;
   end keepDeviceManagerRequests;

   --  What a kept decision mints from its provider's endpoint.
   function lateCopy (D : LIL.Decision; Service : CuBit.Handles.Endpoint) return H.Derivation is
     (if D.Tag /= 0 then Badged (Service, Manifest_Rights (D.Rights), D.Tag)
      else Copy (Service, Manifest_Rights (D.Rights)));

   function lateAnswer (Label : Unsigned_32) return Message is
     ((tag => (label => Label, length => 0, protocol => 0, ipc => <>, reserved => 0),
       authorityTag => 0, words => [others => 0]));

   --  A provider of Role started; Service is its endpoint. Every holder
   --  waiting for one is introduced: devmgr by an offer, a program by the
   --  answer to its kept call (its decision is then used up).
   procedure introduceAll (Role : Unsigned_64; Service : CuBit.Handles.Endpoint) is
      use type HIPC.Reply_Outcome;
      S : LIL.Slot_Count;
      From : LIL.Slot := LIL.Slot'First;
   begin
      if not Valid (Service) then
         return;
      end if;
      keepDeviceManagerRequests;
      loop
         S := LIL.Next_For (Late, Role, From);
         exit when S = 0;
         declare
            D : constant LIL.Decision := Late (S);
            Holder : constant Process_ID := From_Word (D.Holder);
         begin
            if D.Way = LIL.Offer then
               offerToDeviceManager (D.Role, Service);
            else
               declare
                  Sent : constant HIPC.Reply_Result := HIPC.Reply_Saved_With
                    (Late_Replies (S), lateAnswer (LI.Introduced), lateCopy (D, Service));
               begin
                  if Sent.Outcome = HIPC.Transferred then
                     Warnings.Inform ("procmgr: pid" & Image (Holder) & " introduced late to role" &
                                      Unsigned_64'Image (D.Role) & " (its entry" &
                                      Unsigned_64'Image (D.Index) & ")");
                  else
                     Warnings.Warn ("procmgr: late introduction of pid" & Image (Holder) &
                                    " to role" & Unsigned_64'Image (D.Role) & " not delivered");
                  end if;
                  Late_Replies (S) := H.No_Reply;
                  LIL.Remove (Late, S);
               end;
            end if;
         end;
         exit when S = LIL.Slot'Last;
         From := S + 1;
      end loop;
   end introduceAll;

   --  The roles whose provider procmgr started and that have not registered
   --  yet: introduceRegistered introduces their waiting holders once they
   --  have (after procmgr's own registrar channel to logstore is open, so
   --  logstore names a holder's first records), polled every
   --  Registration_Poll_Ms while any is set, until the provider's ready
   --  deadline.
   Introduce_When_Registered : array (Service_Role) of Boolean := [others => False];

   function introductionsDue return Boolean is
     (for some Due of Introduce_When_Registered => Due);

   procedure introduceRegistered is
      Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
   begin
      for Role in Introduce_When_Registered'Range loop
         if Introduce_When_Registered (Role) then
            if Registered_Driver (Role) /= No_Process then
               Introduce_When_Registered (Role) := False;
               introduceAll (Role, providerFor (Role));
            elsif Registrants (Role) = No_Process or else Now >= Registrant_Ready_By (Role) then
               --  It ended, or missed its deadline: whoever provides the
               --  role next is introduced instead.
               Introduce_When_Registered (Role) := False;
            end if;
         end if;
      end loop;
   end introduceRegistered;

   --  A holder's Await call (CuBit.Late_Introduction) over its introducer
   --  endpoint: answered now, refused now, or kept until a provider starts.
   procedure handleAwaitIntroduction (Sender : Process_ID; Request : Message) is
      use type HIPC.Reply_Outcome;
      S : LIL.Slot_Count := 0;
      Service : CuBit.Handles.Endpoint;
      Ignore : Unsigned_64;
   begin
      if Request.tag.length = LI.Await_Words and then Sender /= No_Process then
         S := LIL.Find (Late, To_Word (Sender), Request.words (LI.Index_Word));
      end if;
      if S = 0 or else Late (S).Way /= LIL.Saved_Reply then
         Warnings.Warn ("procmgr: pid" & Image (Sender) &
                        " asked to be introduced for an entry it was not started without (refused)");
         Ignore := reply (Sender, lateAnswer (LI.Not_Declared));
         return;
      elsif Late (S).Waiting then
         Ignore := reply (Sender, lateAnswer (LI.Withdrawn));
         return;
      end if;
      --  A registered provider: now. Otherwise when one registers.
      Service := providerFor (Late (S).Role);
      if Valid (Service) and then not Introduce_When_Registered (Late (S).Role) then
         declare
            Sent : constant HIPC.Reply_Result :=
              HIPC.Reply_With (Sender, lateAnswer (LI.Introduced), lateCopy (Late (S), Service));
         begin
            if Sent.Outcome = HIPC.Refused then
               Ignore := reply (Sender, lateAnswer (LI.Withdrawn));
            else
               Warnings.Inform ("procmgr: pid" & Image (Sender) & " introduced late to role" &
                                Unsigned_64'Image (Late (S).Role) & " (its entry" &
                                Unsigned_64'Image (Late (S).Index) & ", at once)");
            end if;
            LIL.Remove (Late, S);
         end;
         return;
      end if;
      declare
         Saved : constant HIPC.Saved_Reply := HIPC.Save_Reply;
      begin
         if Saved.Saved then
            Late_Replies (S) := Saved.Handle;
            LIL.Set_Waiting (Late, S);
         else
            Ignore := reply (Sender, lateAnswer (LI.Withdrawn));
            LIL.Remove (Late, S);
         end if;
      end;
   end handleAwaitIntroduction;

   --  Holder ended: its kept calls end with it, its decisions are dropped.
   procedure forgetLate (Holder : Process_ID) is
      Ignore : HIPC.Close_Result;
   begin
      for S in LIL.Slot loop
         if LIL.Used (Late (S)) and then Late (S).Holder = To_Word (Holder)
           and then H.Valid (Late_Replies (S))
         then
            Ignore := HIPC.Close (Late_Replies (S));
            Late_Replies (S) := H.No_Reply;
         end if;
      end loop;
      if Holder /= No_Process then
         LIL.Forget (Late, To_Word (Holder));
      end if;
   end forgetLate;

   ---------------------------------------------------------------------------
   --  Registration roles: which program may register which role, by its
   --  manifest's notification request. Package identity is self-declared,
   --  so the roles whose holder sees others' secrets also need the trusted
   --  startup plan. Free roles may be requested by any manifest.
   ---------------------------------------------------------------------------
   function registrationAllowed
     (Role : Unsigned_64; Identity : String; systemStartup : Boolean) return Boolean is
   begin
      if Role = DRIVER_KEYBOARD or else Role = DRIVER_MOUSE then
         return True;   --  input publishers and focus consumers
      elsif Role = DRIVER_IPCTEST then
         return True;   --  the IPC regression servers (and log-retire)
      elsif Role = DRIVER_LOGSTORE then
         return Identity = "com.cubit.logstore";
      elsif Role = DRIVER_DESKTOP then
         return Identity = "com.cubit.desktop";
      elsif Role = DRIVER_DISPLAY then
         return Identity = "com.cubit.display";
      elsif Role = DRIVER_CCL_TEST then
         return Identity = "com.cubit.ccl-test-host";
      elsif Role = DRIVER_CLOCK then
         return Identity = "com.cubit.clock";
      elsif Role = CuBit.TLS_Protocol.Service_Role then
         --  An impostor TLS service would see every client's plaintext.
         return systemStartup and then Identity = "com.cubit.tls";
      elsif Role = CuBit.Metric_Protocol.Publisher_Service_Role then
         return systemStartup and then Identity = Metrics_Identity;
      elsif Role = DRIVER_LEXICON then
         --  An impostor would read every text its clients spell-check.
         return systemStartup and then Identity = "com.cubit.lexicon";
      elsif Role = DRIVER_ACPI then
         --  An impostor would answer for the machine's firmware.
         return systemStartup and then Identity = "com.cubit.acpi";
      elsif Role = CuBit.Installer_Protocol.Service_Role then
         --  An impostor would see what is installed where, and refuse it.
         return systemStartup and then Identity = "com.cubit.installer";
      elsif Role = CuBit.Startup_Wait_Test.Service_Role then
         --  Test only (tests/startup-wait): from a test's startup plan.
         return systemStartup and then Identity = "com.cubit.startup-wait-test";
      elsif Role = CuBit.Llm_Protocol.Service_Role then
         --  An impostor would read every prompt, and secrets.svc grants the
         --  API key's use to whoever holds this role.
         return systemStartup and then Identity = "com.cubit.llm";
      elsif Role = CuBit.Secrets_Protocol.Service_Role then
         --  An impostor would receive every secret typed into it.
         return systemStartup and then Identity = "com.cubit.secrets";
      elsif Role = CuBit.Debug_Protocol.Service_Role then
         --  The debugger preview (DBG-001): only its demo targets (Ada and
         --  Rust) may offer themselves to debuggers, so a debugger never
         --  attaches to an impostor.
         return Identity = "com.cubit.debug-demo" or else Identity = "com.cubit.debug-demo-rs";
      elsif Role = CuBit.Power_Requests.Service_Role then
         --  An impostor would take shutdown requests and could ask procmgr
         --  to stop the system (CuBit.System_Stops): the plan's power.svc only.
         return systemStartup and then Identity = "com.cubit.power";
      end if;
      return False;
   end registrationAllowed;

   ---------------------------------------------------------------------------
   --  Access manifest constants
   ---------------------------------------------------------------------------
   ACCESS_MAGIC : constant Unsigned_32 := 16#43434143#;  -- "CACC" LE
   MAX_ACL      : constant := 16;
   --  An access entry withheld from a launched child (its launcher does not
   --  cover it): no service is this, so no pass installs it.
   WITHHELD_SCOPE : constant Unsigned_8 := Unsigned_8'Last;

   SANDBOX_NONE       : constant Unsigned_8 := 0;
   SANDBOX_RUN_FOLDER : constant Unsigned_8 := 1;
   SANDBOX_APP_FOLDER : constant Unsigned_8 := 2;

   ---------------------------------------------------------------------------
   --  ensureTLSPolicy
   --  Once tls.svc has registered, keep procmgr's own policy endpoint to it
   --  (a copy of its Mailbox badged Policy_Tag) and lend it elfBuf for scope
   --  lists. False while tls.svc is absent: TLS scopes are then not
   --  installed, so clients are denied, never broadened. A restarted
   --  tls.svc (new PID) is not yet re-bound.
   ---------------------------------------------------------------------------
   function ensureTLSPolicy return Boolean is
      TLS_PID : constant Process_ID :=
        Registered_Driver (CuBit.TLS_Protocol.Service_Role);
      Service : CuBit.Handles.Endpoint;
   begin
      if TLS_Policy_Ready then
         return TLS_PID = TLS_Policy_PID;
      end if;
      if TLS_PID = No_Process then
         return False;
      end if;
      --  Only a tls.svc procmgr started (registration needs its startup).
      Service := mailboxOf (TLS_PID);
      if Valid (Service) and then not Valid (TLS_Policy) then
         TLS_Policy := HC.Derive (Badged (Service, H.Read_Write, CuBit.TLS_Protocol.Policy_Tag));
      end if;
      recordAuthority
        (Own_Process, 0, AUTH_SOURCE_KERNEL_BOOTSTRAP, AUTH_REASON_SELF_BOOTSTRAP,
         CAP_TYPE_ENDPOINT, False, Valid (TLS_Policy), Mask (H.Read_Write),
         To_Word (TLS_PID), CuBit.TLS_Protocol.Policy_Tag);
      if not Valid (TLS_Policy) then
         debugPrint ("procmgr: tls policy endpoint unavailable" & LF);
         return False;
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (TLS_Policy, elfBuf, INITIAL_BUF_PAGES, True,
         TLS_Grant, TLS_Policy_Ready);
      if TLS_Policy_Ready then
         TLS_Policy_PID := TLS_PID;
      else
         debugPrint ("procmgr: tls policy grant failed" & LF);
      end if;
      return TLS_Policy_Ready;
   end ensureTLSPolicy;

   ---------------------------------------------------------------------------
   --  ensureMetricsPolicy: as ensureTLSPolicy, for metrics.svc's read scopes
   --  (MET-002). False while metrics.svc is absent: a reader's scopes are then
   --  not installed, so it is refused, never broadened.
   ---------------------------------------------------------------------------
   function ensureMetricsPolicy return Boolean is
      Metrics_PID : constant Process_ID :=
        Registered_Driver (CuBit.Metric_Protocol.Publisher_Service_Role);
      Service : CuBit.Handles.Endpoint;
   begin
      if Metrics_Policy_Ready then
         return Metrics_PID = Metrics_Policy_PID;
      end if;
      if Metrics_PID = No_Process then
         return False;
      end if;
      Service := mailboxOf (Metrics_PID);
      if Valid (Service) and then not Valid (Metrics_Policy) then
         Metrics_Policy := HC.Derive (Badged (Service, H.Read_Write, CuBit.Metric_Protocol.Policy_Tag));
      end if;
      if not Valid (Metrics_Policy) then
         debugPrint ("procmgr: metrics policy endpoint unavailable" & LF);
         return False;
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (Metrics_Policy, elfBuf, INITIAL_BUF_PAGES, False, Metrics_Grant, Metrics_Policy_Ready);
      if Metrics_Policy_Ready then
         Metrics_Policy_PID := Metrics_PID;
      else
         debugPrint ("procmgr: metrics policy grant failed" & LF);
      end if;
      return Metrics_Policy_Ready;
   end ensureMetricsPolicy;

   ---------------------------------------------------------------------------
   --  Delegated places (CuBit.Launch_Grants): how many the launch carries,
   --  and placing them into an OP_SET_ACL batch (72-byte entries: rights,
   --  prefix length, then the prefix: FS_ENTRY_BYTES each) from entry idx. Each must
   --  be covered by a scope the launcher holds (attenuateFor); it is then
   --  recorded as held by the child, which may delegate it in turn. ok is
   --  False, and nothing should be installed, when one is not covered.
   ---------------------------------------------------------------------------
   function delegatedCount return Natural is
     (if pendingGrantBytes = 0 then 0
      else CuBit.Launch_Grants.Count_Of (pendingGrants (1 .. pendingGrantBytes)));

   procedure placeDelegated
     (childPID : Process_ID; batch : System.Address; batchEntries : Natural;
      idx : in out Natural; ok : out Boolean)
   is
      package LG renames CuBit.Launch_Grants;
      grantBuf : array (0 .. batchEntries * FS_ENTRY_BYTES - 1) of Unsigned_8
        with Import, Address => batch;
      position : Positive := LG.Header_Bytes + 1;
      rights : Unsigned_8;
      first, last : Positive;
   begin
      ok := True;
      if pendingGrantBytes = 0 or else launchIndex (attenuateFor) = 0 then
         ok := pendingGrantBytes = 0;
         return;
      end if;
      for g in 1 .. delegatedCount loop
         LG.Next (pendingGrants (1 .. pendingGrantBytes), position, rights, first, last);
         declare
            name : constant String :=
              [for k in first .. last => Character'Val (pendingGrants (k))];
            Launcher : Launch_State renames Launches.Values (launchIndex (attenuateFor));
            covered : Boolean := False;
         begin
            for h in 1 .. Launcher.Scope_Count loop
               covered := covered or else LAuth.Scope_Covered
                 (Launcher.Scopes (h).Service, Launcher.Scopes (h).Rights,
                  Launcher.Scopes (h).Prefix (1 .. Launcher.Scopes (h).Length),
                  SERVICE_FS, rights, name);
            end loop;
            if not covered or else idx >= batchEntries then
               --  A logstore line: the launcher hears only Not_Granted.
               Warnings.Warn ("procmgr: launch refused: its launcher does not hold the delegated place " & name);
               attenuationRefused := True;
               ok := False;
               return;
            end if;
            grantBuf (idx * FS_ENTRY_BYTES) := rights;
            grantBuf (idx * FS_ENTRY_BYTES + 1) := Unsigned_8 (name'Length mod 256);
            grantBuf (idx * FS_ENTRY_BYTES + 2) := Unsigned_8 (name'Length / 256);
            for c in name'Range loop
               grantBuf (idx * FS_ENTRY_BYTES + FS_PREFIX_AT + (c - name'First)) :=
                 Character'Pos (name (c));
            end loop;
            idx := idx + 1;
            if launchIndex (childPID) /= 0 then
               declare
                  Held : Launch_State renames Launches.Values (launchIndex (childPID));
                  prefix : String (1 .. LAuth.Maximum_Prefix_Bytes) := [others => ' '];
               begin
                  prefix (1 .. name'Length) := name;
                  if Held.Scope_Count < Held.Scopes'Last then
                     Held.Scope_Count := Held.Scope_Count + 1;
                     Held.Scopes (Held.Scope_Count) :=
                       (Service => SERVICE_FS, Rights => rights,
                        Length => name'Length, Prefix => prefix,
                        Delegated => True);
                  end if;
               end;
            end if;
         end;
      end loop;
   end placeDelegated;

   ---------------------------------------------------------------------------
   --  parseAndSendACL
   --  Parse .cubit.access section from the ELF in elfBuf and send
   --  OP_SET_ACL to the FS server. Missing/malformed sections grant nothing.
   --  binaryName enables the manifest's APP_FOLDER prefix rewriting for
   --  wildcard FS entries; RUN_FOLDER has no launch directory to use (a
   --  launcher's directory is a launch-block argument, checked separately:
   --  launchDirectoryVisible) and grants nothing.
   ---------------------------------------------------------------------------
   procedure parseAndSendACL
     (childPID        : Process_ID;
      elfSize         : Unsigned_64;
      policyReady     : out Boolean;
      binaryName      : String := "";
      --  TLS scopes reach the network through tls.svc, so they need the
      --  same launch approval as network scopes; a manifest alone never
      --  grants them.
      networkApproved : Boolean := False)
   is
      e_shoff     : Unsigned_64;
      e_shentsize : Unsigned_16;
      e_shnum     : Unsigned_16;
   begin
      -- Missing/malformed policy still grants nothing. Once valid FS/Config
      -- scopes are requested, failure to install them must fail the launch.
      policyReady := True;
      if elfSize < 64 then
         goto No_Access_Policy;
      end if;

      e_shoff     := readU64 (40);
      e_shentsize := readU16 (58);
      e_shnum     := readU16 (60);

      if e_shentsize /= 64 or e_shoff = 0 or e_shnum = 0 then
         goto No_Access_Policy;
      end if;

      if e_shoff > elfSize or else
        Unsigned_64 (e_shnum) > (elfSize - e_shoff) / 64
      then
         goto No_Access_Policy;
      end if;

      --  Scan section headers for .cubit.access (PROGBITS with CACC magic)
      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase    : constant Unsigned_64 :=
               e_shoff + Unsigned_64 (i) * 64;
            sh_type   : constant Unsigned_32 := readU32 (shBase + 4);
            sh_offset : Unsigned_64;
            sh_size   : Unsigned_64;
         begin
            if sh_type = SHT_PROGBITS then
               sh_offset := readU64 (shBase + 24);
               sh_size   := readU64 (shBase + 32);

               --  Need at least 16 bytes for access header
               if sh_size >= 16 and then
                  sh_offset <= elfSize and then
                  sh_size <= elfSize - sh_offset and then
                  readU32 (sh_offset) = ACCESS_MAGIC
               then
                  declare
                     version : constant Unsigned_16 :=
                        readU16 (sh_offset + 4);
                     count   : constant Unsigned_16 :=
                        readU16 (sh_offset + 6);
                  begin
                     if version /= 1 then
                        debugPrint ("procmgr: unknown access v" & LF);
                        goto No_Access_Policy;
                     end if;

                     if count = 0 or count > MAX_ACL then
                        goto No_Access_Policy;
                     end if;

                     --  Validate entries fit: 16 + 80*count
                     if sh_size < 16 + Unsigned_64 (count) * 80 then
                        debugPrint ("procmgr: access truncated" & LF);
                        goto No_Access_Policy;
                     end if;

                     debugPrint ("procmgr: access has ");
                     printDec (Unsigned_32 (count));
                     debugPrint (" ACL entries" & LF);

                     --  Read sandbox mode from header byte 13
                     declare
                        effectiveSandbox : constant Unsigned_8 :=
                           readU8 (sh_offset + 13);
                        sandboxPrefix    : String (1 .. 64) :=
                           (others => ' ');
                        sandboxPrefixLen : Natural := 0;
                     begin
                        --  An invalid sandbox cannot mean unrestricted.
                        if effectiveSandbox > SANDBOX_APP_FOLDER then
                           goto No_Access_Policy;
                        end if;

                        --  Compute sandbox prefix (RUN_FOLDER: none, so
                        --  refused below).
                        if effectiveSandbox = SANDBOX_APP_FOLDER
                        then
                           --  dirname(binaryName): find last '/'
                           declare
                              lastSlash : Natural := 0;
                           begin
                              for c in 0 .. binaryName'Length - 1 loop
                                 if binaryName (
                                    binaryName'First + c) = '/'
                                 then
                                    lastSlash := c + 1;
                                 end if;
                              end loop;
                              sandboxPrefixLen := lastSlash;
                              if sandboxPrefixLen > 64 then
                                 goto No_Access_Policy;
                              end if;
                              for c in 0 ..
                                 sandboxPrefixLen - 1
                              loop
                                 sandboxPrefix (1 + c) :=
                                    binaryName (
                                       binaryName'First + c);
                              end loop;
                           end;
                        end if;

                        if effectiveSandbox /= SANDBOX_NONE and then
                          sandboxPrefixLen = 0
                        then
                           goto No_Access_Policy;
                        end if;

                        if effectiveSandbox /= SANDBOX_NONE
                           and sandboxPrefixLen > 0
                        then
                           debugPrint ("procmgr: sandbox prefix=");
                           debugPrint (
                              sandboxPrefix (1 .. sandboxPrefixLen));
                           debugPrint ("" & LF);
                        end if;

                     --  Copy entries to stack-local array before
                     --  overwriting elfBuf with grant format.
                     --  Per entry: 1 byte rights + 1 byte prefixLen +
                     --  1 byte service + 64 bytes prefix.
                     declare
                        type LocalEntry is record
                           rights    : Unsigned_8;
                           prefixLen : Unsigned_8;
                           service   : Unsigned_8;
                           prefix    : String (1 .. 64);
                        end record;
                        locals : array (0 .. Natural (count) - 1)
                           of LocalEntry;
                        entBase : Unsigned_64;
                        pLen    : Natural;
                        fsCount     : Natural := 0;
                        configCount : Natural := 0;
                        tlsCount    : Natural := 0;
                        metricsCount : Natural := 0;
                     begin
                        for j in 0 .. Natural (count) - 1 loop
                           entBase := sh_offset + 16 +
                              Unsigned_64 (j) * 80;
                           locals (j).rights :=
                              readU8 (entBase);
                           locals (j).prefixLen :=
                              readU8 (entBase + 1);
                           locals (j).service :=
                              readU8 (entBase + 2);
                           pLen := Natural (locals (j).prefixLen);
                           if pLen > 64 then
                              goto No_Access_Policy;
                           end if;
                           if locals (j).service not in
                             SERVICE_FS | SERVICE_CONFIG | SERVICE_TLS | SERVICE_METRICS
                           then
                              goto No_Access_Policy;
                           end if;
                           for c in 0 .. pLen - 1 loop
                              locals (j).prefix (1 + c) :=
                                 Character'Val (
                                    Natural (readU8 (entBase + 8 +
                                       Unsigned_64 (c))));
                           end loop;
                           if locals (j).service = SERVICE_FS and then
                             (not CuBit.File_Access.Valid_Rights (locals (j).rights)
                              or else not CuBit.File_Access.Valid_Path
                                (locals (j).prefix (1 .. pLen)))
                           then
                              goto No_Access_Policy;
                           end if;
                           if locals (j).service = SERVICE_METRICS and then
                             (locals (j).rights /= CuBit.Metric_Protocol.Read_Right or else
                              (for some c in 1 .. pLen =>
                                 locals (j).prefix (c) not in 'a' .. 'z' | '0' .. '9' | '.' | '_' | '-'))
                           then
                              goto No_Access_Policy;
                           end if;
                           if locals (j).service = SERVICE_TLS then
                              declare
                                 Parsed : CuBit.TLS_Scopes.Scope;
                                 Valid : Boolean;
                              begin
                                 CuBit.TLS_Scopes.Parse
                                   (locals (j).prefix (1 .. pLen), Parsed, Valid);
                                 if not Valid or else locals (j).rights /= 1 then
                                    goto No_Access_Policy;
                                 end if;
                              end;
                           end if;
                        end loop;

                        --  Sandbox rewrite: set prefix on wildcard FS
                        --  entries (prefixLen=0, service=FS)
                        if effectiveSandbox /= SANDBOX_NONE
                           and sandboxPrefixLen > 0
                        then
                           for j in 0 .. Natural (count) - 1 loop
                              if locals (j).service = SERVICE_FS
                                 and locals (j).prefixLen = 0
                              then
                                 locals (j).prefixLen :=
                                    Unsigned_8 (sandboxPrefixLen);
                                 for c in 0 ..
                                    sandboxPrefixLen - 1
                                 loop
                                    locals (j).prefix (1 + c) :=
                                       sandboxPrefix (1 + c);
                                 end loop;
                              end if;
                           end loop;
                        end if;

                        --  A launched child's scopes must each be covered by
                        --  one its launcher holds; record what this process
                        --  holds for the children it may start.
                        for j in 0 .. Natural (count) - 1 loop
                           --  A Launch-into-S child gets its declared scopes
                           --  (the user session narrows none yet: CTX-001);
                           --  a supervisor's child, those it covers.
                           if attenuateFor /= No_Process and then not launchSession then
                              declare
                                 Launcher : Launch_State renames
                                   Launches.Values (launchIndex (attenuateFor));
                                 Covered : Boolean := False;
                              begin
                                 for h in 1 .. Launcher.Scope_Count loop
                                    Covered := Covered or else LAuth.Scope_Covered
                                      (Launcher.Scopes (h).Service,
                                       Launcher.Scopes (h).Rights,
                                       Launcher.Scopes (h).Prefix
                                         (1 .. Launcher.Scopes (h).Length),
                                       locals (j).service, locals (j).rights,
                                       locals (j).prefix
                                         (1 .. Natural (locals (j).prefixLen)));
                                 end loop;
                                 if not Covered then
                                    --  Withheld (Launch_Attenuation): the
                                    --  child gets the scopes its launcher
                                    --  covers, never one it does not.
                                    Warnings.Inform ("procmgr: " & binaryName &
                                      ": scope " & locals (j).prefix
                                        (1 .. Natural (locals (j).prefixLen)) &
                                      " withheld: its launcher does not hold it");
                                    locals (j).service := WITHHELD_SCOPE;
                                 end if;
                              end;
                           end if;
                           if launchIndex (childPID) /= 0 and then locals (j).service /= WITHHELD_SCOPE then
                              declare
                                 Held : Launch_State renames
                                   Launches.Values (launchIndex (childPID));
                              begin
                                 if Held.Scope_Count < Held.Scopes'Last then
                                    Held.Scope_Count := Held.Scope_Count + 1;
                                    Held.Scopes (Held.Scope_Count) :=
                                      (Service => locals (j).service,
                                       Rights => locals (j).rights,
                                       Length => Natural (locals (j).prefixLen),
                                       --  A manifest prefix (64 bytes at
                                       --  most), padded to a held scope.
                                       Prefix => locals (j).prefix &
                                         [1 .. LAuth.Maximum_Prefix_Bytes -
                                               locals (j).prefix'Length => ' '],
                                       Delegated => False);
                                 end if;
                              end;
                           end if;
                        end loop;

                        --  Count entries per service
                        for j in 0 .. Natural (count) - 1 loop
                           if locals (j).service = SERVICE_CONFIG then
                              configCount := configCount + 1;
                           elsif locals (j).service = SERVICE_TLS then
                              tlsCount := tlsCount + 1;
                           elsif locals (j).service = SERVICE_METRICS then
                              metricsCount := metricsCount + 1;
                           elsif locals (j).service = SERVICE_FS then
                              fsCount := fsCount + 1;
                           end if;
                        end loop;

                        --  Pass 1: Write FS entries (the manifest's, then any
                        --  the launcher delegates) to elfBuf and send
                        if fsCount + delegatedCount > 0 then
                           declare
                              fsTotal : constant Natural := fsCount + delegatedCount;
                              grantBuf : array (0 .. fsTotal * FS_ENTRY_BYTES - 1)
                                 of Unsigned_8 with
                                 Import, Address => elfBuf;
                              base : Natural;
                              idx  : Natural := 0;
                              placed : Boolean;
                           begin
                              for b in grantBuf'Range loop
                                 grantBuf (b) := 0;
                              end loop;

                              for j in 0 .. Natural (count) - 1 loop
                                 if locals (j).service = SERVICE_FS
                                 then
                                    base := idx * FS_ENTRY_BYTES;
                                    grantBuf (base) := locals (j).rights;
                                    grantBuf (base + 1) :=
                                       locals (j).prefixLen;
                                    pLen :=
                                       Natural (locals (j).prefixLen);
                                    for c in 0 .. pLen - 1 loop
                                       grantBuf (base + FS_PREFIX_AT + c) :=
                                          Unsigned_8 (Character'Pos (
                                             locals (j).prefix (1 + c)));
                                    end loop;
                                    idx := idx + 1;
                                 end if;
                              end loop;
                              placeDelegated (childPID, elfBuf, fsTotal, idx, placed);
                              if not placed then
                                 policyReady := False;
                                 return;
                              end if;
                           end;

                           declare
                              aclMsg : Message := NULL_MESSAGE;
                              aclTag : MessageTag;
                           begin
                              aclMsg.tag := (label  => OP_SET_ACL,
                                             length => 4,
                                             protocol  => 0, ipc => <>,
                                             reserved  => 0);
                              aclMsg.words := [0 => To_Word (childPID),
                                               1 => Unsigned_64 (fsCount + delegatedCount),
                                               2 => fsGrant.slot,
                                               3 => fsGrant.generation];
                              aclTag := capCall (
                                 PH.Filesystem, aclMsg, CuBit.Messages.Wait_Forever);
                              if aclTag.label /= REPLY_OK or else aclTag.length /= 1 or else
                                aclTag.protocol /= 0 or else aclTag.reserved /= 0
                              then
                                 debugPrint ("procmgr: filesystem scope installation failed" & LF);
                                 policyReady := False;
                                 return;
                              end if;
                           end;
                        end if;

                        --  Pass 2: Write CONFIG entries and send
                        if configCount > 0 then
                           if not Config_Grant_Ready then
                              debugPrint ("procmgr: config policy grant unavailable" & LF);
                              policyReady := False;
                              return;
                           end if;
                           declare
                              grantBuf : array
                                 (0 .. configCount * 72 - 1)
                                 of Unsigned_8 with
                                 Import, Address => elfBuf;
                              base : Natural;
                              idx  : Natural := 0;
                           begin
                              for b in grantBuf'Range loop
                                 grantBuf (b) := 0;
                              end loop;

                              for j in 0 .. Natural (count) - 1 loop
                                 if locals (j).service = SERVICE_CONFIG
                                 then
                                    base := idx * 72;
                                    grantBuf (base) := locals (j).rights;
                                    grantBuf (base + 1) :=
                                       locals (j).prefixLen;
                                    pLen :=
                                       Natural (locals (j).prefixLen);
                                    for c in 0 .. pLen - 1 loop
                                       grantBuf (base + 8 + c) :=
                                          Unsigned_8 (Character'Pos (
                                             locals (j).prefix (1 + c)));
                                    end loop;
                                    idx := idx + 1;
                                 end if;
                              end loop;
                           end;

                           declare
                              aclMsg : Message := NULL_MESSAGE;
                              aclTag : MessageTag;
                           begin
                              aclMsg.tag := (label  => OP_SET_ACL,
                                             length => 4,
                                             protocol  => 0, ipc => <>,
                                             reserved  => 0);
                              aclMsg.words := [
                                 0 => To_Word (childPID),
                                 1 => Unsigned_64 (configCount),
                                 2 => Config_Grant.slot,
                                 3 => Config_Grant.generation];
                              aclTag := capCall (
                                 PH.Config, aclMsg, CuBit.Messages.Wait_Forever);
                              if aclTag.label /= REPLY_OK or else aclTag.length /= 1 or else
                                aclTag.protocol /= 0 or else aclTag.reserved /= 0
                              then
                                 debugPrint ("procmgr: config scope installation failed" & LF);
                                 policyReady := False;
                                 return;
                              end if;
                           end;
                        end if;

                        --  Pass 3: TLS scopes to tls.svc, if it is running.
                        if tlsCount > 0 then
                           if not networkApproved then
                              debugPrint ("procmgr: TLS scopes need network " &
                                          "approval; not installed" & LF);
                           elsif not ensureTLSPolicy then
                              debugPrint ("procmgr: tls.svc unavailable; " &
                                          "TLS scopes not installed" & LF);
                           else
                              declare
                                 grantBuf : array
                                    (0 .. tlsCount *
                                       CuBit.TLS_Protocol.Scope_Entry_Bytes - 1)
                                    of Unsigned_8 with
                                    Import, Address => elfBuf;
                                 base : Natural;
                                 idx  : Natural := 0;
                                 tlsMsg : Message := NULL_MESSAGE;
                                 tlsTag : MessageTag;
                              begin
                                 grantBuf := [others => 0];
                                 for j in 0 .. Natural (count) - 1 loop
                                    if locals (j).service = SERVICE_TLS then
                                       base := idx *
                                         CuBit.TLS_Protocol.Scope_Entry_Bytes;
                                       grantBuf (base) := locals (j).rights;
                                       grantBuf (base + 1) :=
                                          locals (j).prefixLen;
                                       for c in 0 .. Natural (locals (j).prefixLen) - 1 loop
                                          grantBuf (base + 8 + c) :=
                                             Unsigned_8 (Character'Pos (
                                                locals (j).prefix (1 + c)));
                                       end loop;
                                       idx := idx + 1;
                                    end if;
                                 end loop;
                                 tlsMsg.tag :=
                                   (label => CuBit.TLS_Protocol.Set_Scopes_Operation,
                                    length => 4, protocol => 0, ipc => <>, reserved => 0);
                                 tlsMsg.words :=
                                   [To_Word (childPID), Unsigned_64 (tlsCount),
                                    TLS_Grant.slot, TLS_Grant.generation];
                                 tlsTag := capCall (TLS_Policy, tlsMsg, CuBit.Messages.Wait_Forever);
                                 if tlsTag.label /= REPLY_OK then
                                    debugPrint ("procmgr: tls scopes rejected" & LF);
                                 end if;
                              end;
                           end if;
                        end if;

                        --  Pass 4: metrics read scopes to metrics.svc, if it is
                        --  running (MET-002). The observer badge alone reads
                        --  nothing: these name what the reader may read.
                        if metricsCount > 0 then
                           if not ensureMetricsPolicy then
                              debugPrint ("procmgr: metrics.svc unavailable; " &
                                          "metrics scopes not installed" & LF);
                           else
                              declare
                                 grantBuf : array
                                    (0 .. metricsCount *
                                       CuBit.Metric_Protocol.Scope_Entry_Bytes - 1)
                                    of Unsigned_8 with
                                    Import, Address => elfBuf;
                                 base : Natural;
                                 idx  : Natural := 0;
                                 scopeMsg : Message := NULL_MESSAGE;
                                 scopeTag : MessageTag;
                              begin
                                 grantBuf := [others => 0];
                                 for j in 0 .. Natural (count) - 1 loop
                                    if locals (j).service = SERVICE_METRICS then
                                       base := idx * CuBit.Metric_Protocol.Scope_Entry_Bytes;
                                       grantBuf (base) := locals (j).rights;
                                       grantBuf (base + 1) := locals (j).prefixLen;
                                       for c in 0 .. Natural (locals (j).prefixLen) - 1 loop
                                          grantBuf (base + CuBit.Metric_Protocol.Scope_Prefix_At + c) :=
                                             Unsigned_8 (Character'Pos (locals (j).prefix (1 + c)));
                                       end loop;
                                       idx := idx + 1;
                                    end if;
                                 end loop;
                                 scopeMsg.tag :=
                                   (label => CuBit.Metric_Protocol.Operation'Enum_Rep
                                               (CuBit.Metric_Protocol.Set_Scopes),
                                    length => CuBit.Metric_Protocol.Message_Words,
                                    protocol => 0, ipc => <>, reserved => 0);
                                 scopeMsg.words :=
                                   [To_Word (childPID), Unsigned_64 (metricsCount),
                                    Metrics_Grant.slot, Metrics_Grant.generation];
                                 scopeTag := capCall (Metrics_Policy, scopeMsg, CuBit.Messages.Wait_Forever);
                                 if scopeTag.label /= REPLY_OK then
                                    debugPrint ("procmgr: metrics scopes rejected" & LF);
                                 end if;
                              end;
                           end if;
                        end if;

                        return;  -- policy dispatched
                     end;
                     end;  -- sandbox declare
                  end;
               end if;
            end if;
         end;
      end loop;

   <<No_Access_Policy>>
      --  No .cubit.access section: deny-by-default, except for places the
      --  launcher delegates (checked against what it holds).
      if delegatedCount > 0 then
         declare
            idx : Natural := 0;
            placed : Boolean;
            batch : array (0 .. delegatedCount * FS_ENTRY_BYTES - 1) of Unsigned_8
              with Import, Address => elfBuf;
            aclMsg : Message := NULL_MESSAGE;
            aclTag : MessageTag;
         begin
            batch := [others => 0];
            placeDelegated (childPID, elfBuf, delegatedCount, idx, placed);
            if not placed then
               policyReady := False;
               return;
            end if;
            aclMsg.tag := (label => OP_SET_ACL, length => 4, protocol => 0, ipc => <>, reserved => 0);
            aclMsg.words := [0 => To_Word (childPID), 1 => Unsigned_64 (delegatedCount),
                             2 => fsGrant.slot, 3 => fsGrant.generation];
            aclTag := capCall (PH.Filesystem, aclMsg, CuBit.Messages.Wait_Forever);
            if aclTag.label /= REPLY_OK or else aclTag.length /= 1 or else
              aclTag.protocol /= 0 or else aclTag.reserved /= 0
            then
               debugPrint ("procmgr: delegated scope installation failed" & LF);
               policyReady := False;
            end if;
            return;
         end;
      end if;
      --  The FS server denies access for processes with no ACL profile,
      --  so we simply don't send OP_SET_ACL.
      debugPrint ("procmgr: absent/invalid .cubit.access, deny-by-default" & LF);
   end parseAndSendACL;

   ---------------------------------------------------------------------------
   --  A launched child may start only in a working directory its own
   --  filesystem scopes let it read: a launcher cannot place it anywhere
   --  else. True when there is no launch block or it names no directory.
   ---------------------------------------------------------------------------
   function launchDirectoryVisible (childPID : Process_ID) return Boolean is
      package LA renames CuBit.Launch_Arguments;
      use type LA.Validation;
      Length : constant LA.Block_Length := pendingArgumentBytes;
      Read_Only : constant CuBit.File_Access.Rights_Set :=
        [CuBit.File_Access.Read_Objects => True, others => False];
   begin
      if attenuateFor = No_Process or else Length = 0 then
         return True;
      elsif LA.Validate (launchBlock (1 .. Length)) /= LA.Valid then
         return False;
      elsif LA.Directory_Declared (launchBlock (1 .. Length)) = 0 then
         return True;
      elsif launchIndex (childPID) = 0 then
         return False;
      end if;
      declare
         Item : LA.Block renames launchBlock (1 .. Length);
         First : Positive;
         Last : Natural;
         Found : Boolean;
      begin
         LA.Locate (Item, LA.Strings_Declared (Item), First, Last, Found);
         if not Found then
            return False;
         end if;
         declare
            Directory : String (1 .. Last - First + 1);
            Held : Launch_State renames Launches.Values (launchIndex (childPID));
         begin
            for K in Directory'Range loop
               Directory (K) := Character'Val (Item (First + K - 1));
            end loop;
            for H in 1 .. Held.Scope_Count loop
               if Held.Scopes (H).Service = SERVICE_FS
                 and then CuBit.File_Access.Includes
                   (CuBit.File_Access.Rights_From_Wire (Held.Scopes (H).Rights),
                    Read_Only)
                 and then CuBit.File_Access.Scope_Matches
                   (Held.Scopes (H).Prefix (1 .. Held.Scopes (H).Length),
                    Directory)
               then
                  return True;
               end if;
            end loop;
            debugPrint ("procmgr: launched child cannot read its working " &
                        "directory: " & Directory & LF);
            return False;
         end;
      end;
   end launchDirectoryVisible;

   ---------------------------------------------------------------------------
   --  queryConfigQuota
   --  Query the config store for resource quotas keyed by package ID.
   --  Uses Config_Grant (which maps elfBuf). Safe to call after all ELF
   --  section parsing is done, since this overwrites elfBuf.
   ---------------------------------------------------------------------------
   procedure queryConfigQuota
     (pkgId       : String;
      maxFrames   : out Unsigned_32;
      cpuQuotaUs  : out Unsigned_32;
      cpuPeriodUs : out Unsigned_32)
   is
      procedure queryOne (suffix : String; result : out Unsigned_32) is
         prefix   : constant String := "resource/";
         totalLen : constant Natural :=
            prefix'Length + pkgId'Length + 1 + suffix'Length;
         cfgMsg   : Message;
      begin
         result := 0;

         if not Config_Grant_Ready or totalLen > 128 then
            return;
         end if;

         --  Write key to elfBuf (grant buffer for config)
         declare
            keyBuf : array (0 .. totalLen - 1) of Unsigned_8 with
               Import, Address => elfBuf;
            pos : Natural := 0;
         begin
            for c in prefix'Range loop
               keyBuf (pos) :=
                  Unsigned_8 (Character'Pos (prefix (c)));
               pos := pos + 1;
            end loop;
            for c in pkgId'Range loop
               keyBuf (pos) :=
                  Unsigned_8 (Character'Pos (pkgId (c)));
               pos := pos + 1;
            end loop;
            keyBuf (pos) := Unsigned_8 (Character'Pos ('/'));
            pos := pos + 1;
            for c in suffix'Range loop
               keyBuf (pos) :=
                  Unsigned_8 (Character'Pos (suffix (c)));
               pos := pos + 1;
            end loop;
         end;

         cfgMsg := NULL_MESSAGE;
         cfgMsg.tag := (label  => OP_CONFIG_GET,
                        length => 4,
                        protocol  => 0, ipc => <>,
                        reserved  => 0);
         cfgMsg.words (0) := Config_Grant.slot;
         cfgMsg.words (1) := Config_Grant.generation;
         cfgMsg.words (2) := Unsigned_64 (totalLen);
         cfgMsg.tag := capCall (PH.Config, cfgMsg, CuBit.Messages.Wait_Forever);

         if cfgMsg.tag.label /= REPLY_OK or else cfgMsg.tag.length /= 1 or else
           cfgMsg.words (0) > PAGE_SIZE
         then
            return;
         end if;

         --  Parse decimal value from grant buffer
         declare
            valLen : constant Natural :=
               Natural (cfgMsg.words (0));
         begin
            if valLen > 0 and valLen <= PAGE_SIZE then
               declare
                  valBuf : array (0 .. valLen - 1) of Unsigned_8
                     with Import, Address => elfBuf;
                  acc : Unsigned_32 := 0;
                  ch  : Unsigned_8;
               begin
                  for v in 0 .. valLen - 1 loop
                     ch := valBuf (v);
                     exit when ch < Unsigned_8 (Character'Pos ('0'))
                        or ch > Unsigned_8 (Character'Pos ('9'));
                     acc := acc * 10 +
                        Unsigned_32 (ch -
                           Unsigned_8 (Character'Pos ('0')));
                  end loop;
                  result := acc;
               end;
            end if;
         end;
      end queryOne;
   begin
      queryOne ("max-frames", maxFrames);
      queryOne ("cpu-quota-us", cpuQuotaUs);
      queryOne ("cpu-period-us", cpuPeriodUs);
   end queryConfigQuota;

   ---------------------------------------------------------------------------
   --  spawnByName
   --  Read ELF from filesystem, spawn suspended, grant manifest caps, resume.
   --  Returns PID on success; No_Process on failure, with Failure saying why.
   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------
   --  loadLaunchTable
   --  Keep pid's .cubit.launch table (from the ELF in elfBuf) if it is well
   --  formed; without one, the process may start nothing with OP_LAUNCH.
   ---------------------------------------------------------------------------
   procedure loadLaunchTable (elfSize : Unsigned_64; pid : Process_ID) is
      e_shoff : Unsigned_64;
      e_shnum : Unsigned_16;
   begin
      if launchIndex (pid) = 0 or else elfSize < 64 then
         return;
      end if;
      e_shoff := readU64 (40);
      e_shnum := readU16 (60);
      if readU16 (58) /= 64 or else e_shoff = 0 or else e_shnum = 0
        or else e_shoff > elfSize
        or else Unsigned_64 (e_shnum) > (elfSize - e_shoff) / 64
      then
         return;
      end if;
      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase    : constant Unsigned_64 := e_shoff + Unsigned_64 (i) * 64;
            sh_offset : constant Unsigned_64 := readU64 (shBase + 24);
            sh_size   : constant Unsigned_64 := readU64 (shBase + 32);
         begin
            if readU32 (shBase + 4) = SHT_PROGBITS
              and then sh_size >= LAuth.Header_Bytes
              and then sh_offset <= elfSize
              and then sh_size <= elfSize - sh_offset
              and then readU32 (sh_offset) = LAuth.Magic
            then
               if sh_size > LAuth.Maximum_Table_Bytes then
                  debugPrint ("procmgr: launch table too large" & LF);
                  return;
               end if;
               declare
                  Length : constant LAuth.Table_Length := Natural (sh_size);
                  Source : constant LAuth.Table_Bytes (1 .. Length)
                    with Import, Address => imageBuf + Storage_Offset (sh_offset);
               begin
                  if LAuth.Valid (Source) then
                     Launches.Values (launchIndex (pid)).Table (1 .. Length) := Source;
                     Launches.Values (launchIndex (pid)).Table_Length := Length;
                  else
                     debugPrint ("procmgr: invalid launch table" & LF);
                  end if;
               end;
               return;
            end if;
         end;
      end loop;
   end loadLaunchTable;

   ---------------------------------------------------------------------------
   --  deriveRings
   --  The rings the OP_LAUNCH launcher lends the child (pendingRings): for
   --  each, acquire the launcher's grant, derive one for the child (named by
   --  procmgr's endpoint to it) over the port's declared pages, and list it
   --  for the child's launch block with the launcher as owner. A ring that
   --  cannot be derived is left out: the child then makes its own.
   --  Recipient is procmgr's endpoint to the child (SPAWN's Mailbox).
   ---------------------------------------------------------------------------
   function deriveRings
     (childPID : Process_ID; Description : CuBit.Launch_Arguments.Block;
      Recipient : CuBit.Handles.Endpoint)
      return CuBit.Outlet_Rings.Table
   is
      package PD renames CuBit.Program_Descriptions;
      use type PD.Connector_Direction;
      Result : CuBit.Outlet_Rings.Table;
      S : PD.Signature;
      Decoded : Boolean;
      Copy : PD.Bytes (1 .. Description'Length);
   begin
      if pendingRings.Count = 0 or else attenuateFor = No_Process
        or else launchIndex (childPID) = 0
        or else Description'Length > PD.Maximum_Descriptor_Bytes
      then
         if attenuateFor /= No_Process then
            debugPrint ("procmgr: port rings lent" & Natural'Image (pendingRings.Count) & LF);
         end if;
         return Result;
      end if;
      for K in Copy'Range loop
         Copy (K) := Description (Description'First + K - 1);
      end loop;
      PD.Decode (Copy, S, Decoded);
      if not Decoded then
         return Result;
      end if;
      if not Valid (Recipient) then
         debugPrint ("procmgr: port rings: no endpoint to the child" & LF);
         return Result;
      end if;
      --  Derived grants live in procmgr's grant namespace: the child names
      --  procmgr as their owner when it acquires them.
      Result.Owner := Own_Process_Word;
      for E of pendingRings.Entries (1 .. pendingRings.Count) loop
         if E.Outlet < S.Connector_Total and then S.Connectors (E.Outlet).Direction = PD.Outlet
           and then CuBit.Grant_References.Valid_Wire (E.Grant)
         then
            declare
               Parent : constant CuBit.Memory_Grants.Grant_Reference :=
                 CuBit.Grant_References.Decode (E.Grant);
               --  The whole region the launcher lent (control page and
               --  ring), as the child will map it.
               Pages : constant Positive :=
                 CuBit.Stream_Rings.Region_Pages
                   (CuBit.Stream_Regions.Declared (S.Connectors (E.Outlet).Pages));
               Mapped : System.Address;
               Child : CuBit.Memory_Grants.Grant_Reference;
               Ok : Boolean;
               State : Launch_State renames Launches.Values (launchIndex (childPID));
            begin
               CuBit.Memory_Grants.Acquire
                 (Parent, attenuateFor, 0, Unsigned_64 (Pages) * PAGE_SIZE,
                  CuBit.Memory_Grants.Write_Access, Mapped, Ok);
               if not Ok then
                  debugPrint ("procmgr: port ring not acquired from the launcher" & LF);
               end if;
               if Ok then
                  CuBit.Memory_Grants.Derive_Via_Capability
                    (Recipient, Parent, 0, Pages, True, Child, Ok);
                  if not Ok then
                     debugPrint ("procmgr: port ring not derived for the child" & LF);
                  end if;
                  if Ok and then State.Ring_Parent_Count < State.Ring_Parents'Last then
                     State.Ring_Parent_Count := State.Ring_Parent_Count + 1;
                     State.Ring_Parents (State.Ring_Parent_Count) := Parent;
                     Result.Count := Result.Count + 1;
                     Result.Entries (Result.Count) :=
                       (Outlet => E.Outlet, Grant => CuBit.Grant_References.Encode (Child));
                  else
                     CuBit.Memory_Grants.Return_Acquisition (Parent, Ok);
                  end if;
               end if;
            end;
         end if;
      end loop;
      debugPrint ("procmgr: port rings lent" & Natural'Image (pendingRings.Count) &
                  ", derived" & Natural'Image (Result.Count) & LF);
      return Result;
   end deriveRings;

   function Spawn_Attempt
     (Force_Software : Boolean;
      Prior_Incarnation : Process_ID;
      Retry : out Boolean;
      Identity : out Process_ID;
      Failure : out CuBit.Launch_Arguments.Launch_Failure;
      name        : String;
      requester   : Process_ID := No_Process;
      approveNetwork : Network_Approval := No_Network;
      approveRender : Boolean := False;
      systemStartup : Boolean := False;
      startupRole : CCL.Configurations.Startup_Role := CCL.Configurations.Application;
      approveFirmwareSnapshot : Boolean := False;
      approveACPIHardware : Boolean := False;
      approvePCIConfig : Boolean := False;
      approveFirmwareData : Boolean := False;
      approveDisplayCapture : Boolean := False;
      approvePlatformPower : Boolean := False;
      approveTrace : Boolean := False;
      approveTestConsole : Boolean := False;
      approveVMCreate : Boolean := False;
      --  The time it has to register a role it provides (Registrant_Ready_By).
      readyDeadlineMs : CCL.Configurations.Ready_Deadline;
      --  Whether it may wait for a starting provider; if it must, Waits
      --  names what for, and nothing was spawned (Await_Providers).
      mayWait : Boolean;
      Waits : out Awaited_Access)
      return Process_ID
   is
      package SB renames CuBit.Startup_Builder;
      package AHA renames CuBit.ACPI_Hardware_Authority;
      package LA renames CuBit.Launch_Arguments;
      use type SB.Mint_Outcome, ST.Capability_Kind;
      elfSize       : Unsigned_64;
      newPID        : Process_ID := No_Process;
      --  What SPAWN gave procmgr for the child.
      Child         : HC.Spawned_Child;
      t0, t1        : Unsigned_64;
      --  Where a slow launch spent its time (logged at Slow_Spawn_Ms):
      --  the file read, the kernel spawn, the policy resets, the access
      --  section, the startup table and grants, and the quota query.
      Span_Start, Span_Read, Span_Spawned, Span_Reset, Span_Setup : Unsigned_64 := 0;
      pkgId         : String (1 .. 128);
      pkgIdLen      : Natural := 0;
      streamBitmask : Unsigned_64 := 0;
      Render : Render_Request;
      Reading : Manifest_Reading;
      View : Manifest_View;
      Captured_Child : GPU_Grants.Recipient;
      Stop_Accepted : Boolean := False;
      --  The child's record holds its Control and Mailbox; without one
      --  (the table is full) they are closed here once it is stopped.
      Recorded : Boolean := False;
      --  The admitted render session (procmgr's copy) and its ticket.
      Render_Endpoint : CuBit.Handles.Endpoint := H.No_Endpoint;
      --  The kernel publications granted (System_Publications.Bit set),
      --  mapped with the launch block.
      Granted_Publications : Unsigned_64 := System_Publications.No_Publications;
      Render_Ticket : Render_Launch.Ticket := 0;
      --  The launch block payload (arguments, description) built for it.
      Block_Length : LA.Block_Length := pendingArgumentBytes;
      --  The roles this child may register (procmgr approved them): once
      --  it runs, every holder waiting for one is introduced to it.
      Provides : array (Service_Role) of Boolean := [others => False];
      --  A decision kept for late introduction: the child gets the
      --  introducer endpoint (CuBit.Late_Introduction).
      Late_Kept : Boolean := False;
      --  Declared requests a launched child does not get: optional ones its
      --  launcher does not hold (Launch_Attenuation.Withheld).
      Withheld : array (ST.Entry_Index) of Boolean := [others => False];
      --  The ACPI hardware windows installed in the child (Mint_ACPI_Hardware),
      --  in ascending port order: appended to its startup table as
      --  AHA.IO_Label (ACPI_Binding, K).
      ACPI_Window_Words : array (AHA.IO_Window_Number) of Unsigned_64 := [others => ST.No_Handle];
      ACPI_Window_Total : Natural range 0 .. AHA.Maximum_IO_Windows := 0;
      CMOS_Window : CMOS_Windows.Selection;
      function Declares_ACPI_Hardware return Boolean is
      begin
         for I in 1 .. View.Declared.Count loop
            if View.Declared.Entries (I).Kind = ST.ACPI_Hardware
              and then View.Request_Of (I) > 0
            then
               declare R : constant Caps_Request := View.Caps (View.Request_Of (I)); begin
                  if R.Kind = AHA.Manifest_Request and then R.Param0 = 0 and then R.Param1 = 0
                    and then Manifest_Rights (Unsigned_64 (R.Rights)) = H.Read_Write
                  then return True; end if;
               end;
            end if;
         end loop;
         return False;
      end Declares_ACPI_Hardware;
      --  Then its firmware-memory windows, ascending by address, appended
      --  as AHA.Memory_Label (ACPI_Binding, K).
      ACPI_Memory_Words : array (AHA.Memory_Window_Number) of Unsigned_64 := [others => ST.No_Handle];
      ACPI_Memory_Total : Natural range 0 .. AHA.Maximum_Memory_Windows := 0;
      --  Then devmgr's firmware-data endpoint (CuBit.Firmware_Data_Exchange),
      --  when the plan approves it: appended as Data_Label (ACPI_Binding).
      ACPI_Data_Word : Unsigned_64 := ST.No_Handle;
      --  Then the SCI line (Mint_ACPI_SCI), appended as SCI_Label
      --  (ACPI_Binding): Granted with this handle, or Absent (refused, and
      --  logged why) when ACPI_SCI_Asked.
      ACPI_SCI_Word  : Unsigned_64 := ST.No_Handle;
      ACPI_SCI_Asked : Boolean := False;
      ACPI_Binding : ST.Name;
      use type CuBit.Render_Startup.Requirement, CuBit.Render_Startup.Attempt;
      use type CCL.Configurations.Startup_Role;

      procedure Close_Launch_Handles is
         Ignore : HIPC.Close_Result;
      begin
         if Valid (Child.Space) then
            Ignore := HIPC.Close (Child.Space);
            Child.Space := H.No_Cspace;
         end if;
         if Render_Ticket /= 0 then
            Render_Launch.Forget (Render_Launcher, Render_Ticket);
            Render_Ticket := 0;
         end if;
      end Close_Launch_Handles;

      procedure Stop_Suspended_Child is
      begin
         -- The child's own Control handle: the kernel binds it to this
         -- incarnation, so it can never stop a later occupant of the PID.
         Stop_Accepted := False;
         Close_Launch_Handles;
         --  Its kept late-introduction decisions go with it.
         if newPID /= No_Process then
            forgetLate (newPID);
         end if;
         if not Valid (Child.Control) or else not HC.Kill (newPID, Child.Control) then
            debugPrint ("procmgr: failed launch process cleanup rejected" & LF);
         else
            Stop_Accepted := True;
            debugPrint ("procmgr: failed launch child stop requested" & LF);
         end if;
         if not Recorded then
            closeChildHandles (Child.Control, Child.Mailbox);
         end if;
      end Stop_Suspended_Child;

      procedure Discard_Authorized_Child is
         type Policy_Service is (Files, Configuration);
      begin
         --  Revoke before releasing the PID. capCall overwrites its message
         --  with a reply: construct a fresh request for EACH policy service.
         for Service in Policy_Service loop
            if Service = Files or (Service = Configuration and Config_Grant_Ready) then
               declare
                  Cleanup : Message := NULL_MESSAGE;
                  Tag : MessageTag;
               begin
                  Cleanup.tag := (label => CuBit.Filesystems.OP_REVOKE_ACL,
                    length => 1, protocol => 0, ipc => <>, reserved => 0);
                  Cleanup.words (0) := To_Word (newPID);
                  Tag := capCall
                    ((if Service = Files then PH.Filesystem else PH.Config), Cleanup,
                     CuBit.Messages.Wait_Forever);
                  if Tag.label /= REPLY_OK then
                     debugPrint ("procmgr: failed launch policy cleanup rejected" & LF);
                  end if;
               end;
            end if;
         end loop;
         Stop_Suspended_Child;
      end Discard_Authorized_Child;

      ------------------------------------------------------------------------
      --  The child's startup table (CuBit.Startup_Builder): each declared
      --  entry is obtained by procmgr's policy and installed through the
      --  child's CSPACE, never wider than procmgr's own handle.
      ------------------------------------------------------------------------
      --  Logged: Mint already said why the current entry was not granted.
      type Build_Context is record
         Logged : Boolean := False;
      end record;
      Context : Build_Context;

      procedure Install
        (Copy_Of : H.Derivation; Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         Placed : constant H.Install_Result := HC.Install (Child.Space, Copy_Of);
         use type H.ABI.Refusal;
      begin
         Handle := ST.No_Handle;
         if Placed.Installed then
            Handle := HC.Table_Word (Placed.Placed);
            Outcome := SB.Granted;
         elsif Placed.Why = H.ABI.Limit_Reached then
            Outcome := SB.Limit_Reached;
         elsif Placed.Why in H.ABI.Out_Of_Memory | H.ABI.Table_Full then
            Outcome := SB.Out_Of_Memory;
         else
            Outcome := SB.Refused_By_Policy;
         end if;
      end Install;

      --  Whether a service request for Role (with manifest Rights) is for a
      --  startup-only authority this launch is not approved for: startup
      --  approval; or, for an observer role, a launcher that holds it with
      --  those rights (attenuated launches, CuBit.Authority_Policy.
      --  Launcher_Approves). Never a program's name (CAP-011). These grant
      --  observation only, not the other startup-only authorities.
      function Startup_Only_Denied (Role : Unsigned_64; Rights : Unsigned_8) return Boolean is
         isAudioControl : constant Boolean := Role = CuBit.Audio_Control.Service_Role;
         isLogObserver : constant Boolean := Role = CuBit.Log_Protocol.Observer_Service_Role;
         isClockControl : constant Boolean := Role = CuBit.Clock_Control.Service_Role;
         isMetricObserver : constant Boolean :=
           Role = CuBit.Metric_Protocol.Observer_Service_Role;
         isMetricPublisher : constant Boolean :=
           Role = CuBit.Metric_Protocol.Publisher_Service_Role;
         isProcessObserver : constant Boolean :=
           Role = CuBit.Process_Observer.Observer_Service_Role;
         isLogControl : constant Boolean := Role = CuBit.Log_Protocol.Control_Service_Role;
         --  The installer's whole-disk authority: devmgr's endpoint badged
         --  for it, only for the trusted startup plan.
         isDiskInstall : constant Boolean := Role = CuBit.Disk_Install.Service_Role;
         --  installer.svc's clients: they can ask for a disk to be erased.
         isInstaller : constant Boolean := Role = CuBit.Installer_Protocol.Service_Role;
         --  netmgr's client endpoint: it reconfigures every interface.
         isNetManager : constant Boolean := Role = CuBit.Net_Manager_Control.Service_Role;
         --  Desktop's display-capture badge: only for an entry approving it.
         isDisplayCapture : constant Boolean := Role = CuBit.Display_Capture.Service_Role;
         use CuBit.Authority_Policy;
         Authority : constant Bootstrap_Authority :=
           (if isAudioControl then Master_Audio
            elsif isClockControl then Clock_Adjustment
            elsif isLogObserver then Log_Observation
            elsif isMetricObserver then Metric_Observation
            elsif isMetricPublisher then Metric_Publication
            elsif isProcessObserver then Process_Observation
            elsif isLogControl then Log_Control
            elsif isDiskInstall then Disk_Installation
            elsif isInstaller then Installer_Use
            elsif isNetManager then Network_Configuration
            elsif isDisplayCapture then Display_Capture
            else Log_Publication);
         Approval : constant Decision := Evaluate
           (Requested => True,
            Installation_Approved =>
              Startup_Approves
                (Authority, systemStartup,
                 isDisplayCapture and then approveDisplayCapture) or else
              Launcher_Approves
                (Authority,
                 attenuateFor /= No_Process and then launchIndex (attenuateFor) /= 0
                 and then Role <= Unsigned_64 (Unsigned_32'Last)
                 and then launchGives (REQ_SERVICE, Unsigned_32 (Role), Rights, 0)),
            Session_Approved => True,
            Issuer_Allowed => True);
      begin
         --  A Launch badge (Session_Launch): a startup plan entry declaring
         --  it, or a launched program whose requester holds it and
         --  introduces it. The remote badge only from the plan.
         --  The session's debug authority: only where the image's system
         --  policy debugs at all (Handle_Admission.Debug_Authority_Granted).
         if Role = CuBit.Launch_Handles.Session_Debug_Role
           and then not Handle_Admission.Debug_Authority_Granted (Debugging_Gate)
         then
            Warnings.Warn ("procmgr: " & name & ": debug-session withheld: " &
                           (if Handle_Admission."=" (Debugging_Gate, Handle_Admission.Production)
                            then "production debugging needs owner approval (not built yet)"
                            else "debugging is disabled for this image"));
            return True;
         end if;
         if Role = Session_Launch.Launch_Role then
            return not (systemStartup
                        or else Session_Launch."/=" (requesterBadge, Session_Launch.No_Badge));
         elsif Role = Session_Launch.Remote_Launch_Role then
            return not systemStartup;
         end if;
         --  PWR-003: shutdown requesters (Desktop's, the console's badge
         --  on power.svc) only from the trusted startup plan; acpi.svc's
         --  power badge (button watch, S5) only for its power manager.
         --  PWR-004: the power-source observers (read only) likewise.
         if Role = CuBit.Power_Requests.Desktop_Role or else Role = CuBit.Power_Requests.Console_Role
           or else Role = CuBit.Power_Sources.Observer_Role
         then
            return not systemStartup;
         elsif Role = CuBit.Power_Event_Exchange.Service_Role then
            return not (systemStartup and then pkgId (1 .. pkgIdLen) = "com.cubit.power");
         end if;
         if Role = ACPI_PCI_Window_Exchange.Service_Role then
            return not ACPI_PCI_Window_Approval.May_Issue
              (systemStartup, approveACPIHardware, approvePCIConfig,
               pkgId (1 .. pkgIdLen));
         end if;
         return (isAudioControl or isLogObserver or isClockControl
                 or isMetricObserver or isProcessObserver or isLogControl
                 or isDiskInstall or isInstaller or isNetManager
                 or isDisplayCapture)
           and then Approval /= Approved;
      end Startup_Only_Denied;

      --  A service endpoint: the role's provider, badged with the tag the
      --  role's protocol issues (none: an untagged copy without Grant).
      procedure Mint_Service
        (R : Caps_Request; Index : ST.Entry_Index; Item : ST.Startup_Entry;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome; Logged : out Boolean)
      is
         use type ST.Need;
         Role : constant Unsigned_64 := Unsigned_64 (R.Param0);
         isAudioControl : constant Boolean := Role = CuBit.Audio_Control.Service_Role;
         isLogObserver : constant Boolean := Role = CuBit.Log_Protocol.Observer_Service_Role;
         isLogPublisher : constant Boolean := Role = DRIVER_LOGSTORE;
         isClockControl : constant Boolean := Role = CuBit.Clock_Control.Service_Role;
         isMetricObserver : constant Boolean :=
           Role = CuBit.Metric_Protocol.Observer_Service_Role;
         isMetricPublisher : constant Boolean :=
           Role = CuBit.Metric_Protocol.Publisher_Service_Role;
         isProcessObserver : constant Boolean :=
           Role = CuBit.Process_Observer.Observer_Service_Role;
         isLogControl : constant Boolean := Role = CuBit.Log_Protocol.Control_Service_Role;
         isDiskInstall : constant Boolean := Role = CuBit.Disk_Install.Service_Role;
         --  An ACPI client: its one client role, the observer badge.
         isACPI : constant Boolean := Role = DRIVER_ACPI;
         isDisplayCapture : constant Boolean := Role = CuBit.Display_Capture.Service_Role;
         --  The role whose service answers this one.
         Provider_Role : constant Unsigned_64 := Provider_Role_Of (Role);
         Kept : constant H.Rights := Manifest_Rights (Unsigned_64 (R.Rights));
         Issued_Tag : Unsigned_64 := 0;
         Supervisor_Badged : Boolean := False;
         Bound_Provider : Unsigned_64 := CuBit.Child_Specs.No_Provider;
         Fetched : Boolean := False;
         Service : CuBit.Handles.Endpoint;

         --  An endpoint fetched for one named provider is this child's only.
         procedure Close_Fetched is
            Ignore : HIPC.Close_Result;
         begin
            if Fetched then
               Ignore := HIPC.Close (Service);
               Fetched := False;
            end if;
         end Close_Fetched;
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         Logged := False;
         if Startup_Only_Denied (Role, R.Rights) then
            recordAuthority
              (newPID, Index, AUTH_SOURCE_MANIFEST, AUTH_REASON_STARTUP_REQUIRED,
               CAP_TYPE_ENDPOINT, True, False, Mask (Kept), Role, 0);
            debugPrint ("procmgr: startup-only authority denied" & LF);
            return;
         end if;
         if Provider_Role > Maximum_Role then
            Outcome := SB.Not_Available;
            return;
         end if;
         --  A Start_Child binding from the device manager (SUP-001): this
         --  request's label badged as the device it admitted says (an I2C
         --  target, a GPIO input), on the endpoint of the provider it names
         --  (the driver of that device's controller). Roles with a protocol
         --  tag keep theirs.
         for B in 1 .. pendingBindingCount loop
            declare
               Bound : CuBit.Child_Specs.Field_Text renames pendingBindings (B).Label;
            begin
               if Natural (Item.Label.Length) = Bound.Length
                 and then String (Item.Label.Text (1 .. Item.Label.Length)) = Bound.Data (1 .. Bound.Length)
               then
                  Issued_Tag := pendingBindings (B).Badge;
                  Bound_Provider := pendingBindings (B).Provider;
                  Supervisor_Badged := True;
               end if;
            end;
         end loop;
         if Bound_Provider /= CuBit.Child_Specs.No_Provider then
            Service := providerProcess (Provider_Role, From_Word (Bound_Provider), Fetched);
         else
            Service := providerFor (Provider_Role);
         end if;
         if Role = ACPI_PCI_Window_Exchange.Service_Role then
            Issued_Tag := ACPI_PCI_Window_Exchange.Backend_Tag;
         --  Debug-observe for the user session (DBG-001): procmgr's
         --  endpoint badged for Session_Debug_Operation alone; what it may
         --  observe is the session's debug envelope (Handle_Admission).
         elsif Role = CuBit.Launch_Handles.Session_Debug_Role then
            Issued_Tag := CuBit.Launch_Handles.Session_Debug_Tag;
         --  A Launch badge naming the user session (Session_Launch).
         elsif Role in Session_Launch.Launch_Role | Session_Launch.Remote_Launch_Role then
            Issued_Tag :=
              (if Session_Launch."=" (requesterBadge, Session_Launch.Remote_Badge)
               then Session_Launch.Remote_Launch_Tag
               else Session_Launch.Tag_Of (Role));
         --  PWR-003: fixed compile-time stamps; the copies keep no Grant.
         elsif Role = CuBit.Power_Requests.Desktop_Role then
            Issued_Tag := CuBit.Power_Requests.Desktop_Tag;
         elsif Role = CuBit.Power_Requests.Console_Role then
            Issued_Tag := CuBit.Power_Requests.Console_Tag;
         elsif Role = CuBit.Power_Sources.Observer_Role then
            Issued_Tag := CuBit.Power_Sources.Observer_Tag;
         elsif Role = CuBit.Power_Event_Exchange.Service_Role then
            Issued_Tag := CuBit.Power_Event_Exchange.Authority_Tag;
         elsif isACPI then
            Issued_Tag := Unsigned_64 (ACPI_Protocol.Observer_Tag);
         elsif isDisplayCapture then
            --  A fixed compile-time stamp; the copy keeps no Grant, so the
            --  holder can never re-badge it (Manifest_Rights).
            Issued_Tag := CuBit.Display_Capture.Authority_Tag;
         elsif isAudioControl then
            Issued_Tag := CuBit.Audio_Control.Authority_Tag;
         elsif isClockControl then
            Issued_Tag := CuBit.Clock_Control.Authority_Tag;
         elsif isDiskInstall then
            Issued_Tag := CuBit.Disk_Install.Authority_Tag;
         elsif (isLogObserver or isLogPublisher or isLogControl)
           and then Next_Log_Issuance /= 0
         then
            if isLogObserver then
               Issued_Tag := CuBit.Log_Protocol.Observer_Tag_Base +
                 Unsigned_64 (Next_Log_Issuance);
            elsif isLogControl then
               Issued_Tag := CuBit.Log_Protocol.Control_Tag (Unsigned_64 (Next_Log_Issuance));
            else
               Issued_Tag := CuBit.Log_Protocol.Publisher_Tag
                 (CuBit.Log_Protocol.Bootstrap_Budget, Unsigned_64 (Next_Log_Issuance));
            end if;
            if Next_Log_Issuance = Unsigned_32
              (CuBit.Log_Protocol.Publication_Issuance'Last)
            then
               Next_Log_Issuance := 0;
            else
               Next_Log_Issuance := Next_Log_Issuance + 1;
            end if;
         end if;
         if (isMetricObserver or isMetricPublisher) and then Next_Metric_Issuance /= 0 then
            Issued_Tag :=
              (if isMetricObserver
               then CuBit.Metric_Protocol.Observer_Tag (Unsigned_64 (Next_Metric_Issuance))
               else CuBit.Metric_Protocol.Publisher_Tag (Unsigned_64 (Next_Metric_Issuance)));
            --  Never wrap: exhaustion denies issuance.
            Next_Metric_Issuance :=
              (if Next_Metric_Issuance = Unsigned_32'Last then 0 else Next_Metric_Issuance + 1);
         end if;
         if isProcessObserver and then Next_Process_Issuance /= 0 then
            Issued_Tag := CuBit.Process_Observer.Observer_Tag (Next_Process_Issuance);
            Next_Process_Issuance :=
              (if Next_Process_Issuance = Unsigned_32'Last then 0 else Next_Process_Issuance + 1);
         end if;

         if (isLogObserver or isLogPublisher or isLogControl
             or isMetricObserver or isMetricPublisher or isProcessObserver)
           and then Issued_Tag = 0
         then
            recordAuthority
              (newPID, Index, AUTH_SOURCE_MANIFEST, AUTH_REASON_MINT_FAILED,
               CAP_TYPE_ENDPOINT, True, False, Mask (Kept), Role, 0);
            debugPrint ("procmgr: telemetry issuance exhausted" & LF);
            Close_Fetched;
            return;
         elsif not Valid (Service) then
            recordAuthority
              (newPID, Index, AUTH_SOURCE_MANIFEST, AUTH_REASON_SERVICE_MISSING,
               CAP_TYPE_ENDPOINT, True, False, Mask (Kept), Role, 0);
            Outcome := SB.Not_Available;
            if Item.Requirement = ST.Required then
               debugPrint ("procmgr: service not found, role" & Unsigned_64'Image (Role) & LF);
            else
               --  Every check passed but the provider: keep the decision,
               --  to introduce the program when a provider starts
               --  (CuBit.Late_Introduction). Not for a supervisor's binding
               --  (that names one provider instance).
               declare
                  Into : LIL.Slot_Count := 0;
               begin
                  if Bound_Provider = CuBit.Child_Specs.No_Provider then
                     LIL.Keep
                       (Late, (Holder => To_Word (newPID), Index => Unsigned_64 (Index),
                               Role => Provider_Role, Rights => Unsigned_64 (R.Rights),
                               Tag => Issued_Tag, Way => LIL.Saved_Reply,
                               Waiting => False), Into);
                  end if;
                  Late_Kept := Late_Kept or else Into /= 0;
                  Warnings.Inform
                    ("procmgr: " & name & ": optional service """ &
                     String (Item.Label.Text (1 .. Item.Label.Length)) &
                     """ not running; started without it" &
                     (if Into /= 0 then "; introduced when it starts" else ""));
               end;
               Logged := True;
            end if;
            return;
         end if;
         Install ((if Issued_Tag /= 0 then Badged (Service, Kept, Issued_Tag)
                   else Copy (Service, Kept)), Handle, Outcome);
         Close_Fetched;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_MANIFEST,
            (if Outcome = SB.Granted then AUTH_REASON_MANIFEST_REQUEST else AUTH_REASON_MINT_FAILED),
            CAP_TYPE_ENDPOINT, True, Outcome = SB.Granted, Mask (Kept), Provider_Role, Issued_Tag);
         if Outcome = SB.Granted then
            noteService (newPID, R.Param0, Mask (Kept));
            if Supervisor_Badged then
               debugPrint ("procmgr: " & name & ": " & String (Item.Label.Text (1 .. Item.Label.Length)) &
                           " badged by its supervisor's binding" & LF);
            end if;
         end if;
      end Mint_Service;

      --  A notification over one role: input publishers and focus consumers,
      --  and the registrations procmgr's policy approves for this identity.
      procedure Mint_Notification
        (R : Caps_Request; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         Role : constant Unsigned_64 := Unsigned_64 (R.Param0);
         Kept : constant H.Rights := Manifest_Rights (Unsigned_64 (R.Rights));
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         if Role = 0 or else Role > Maximum_Role
           or else not registrationAllowed (Role, pkgId (1 .. pkgIdLen), systemStartup)
         then
            recordAuthority
              (newPID, Index, AUTH_SOURCE_IDENTITY_POLICY, AUTH_REASON_PACKAGE_ID,
               CAP_TYPE_NOTIFICATION, True, False, Mask (Kept), Role, 0);
            debugPrint ("procmgr: notification role" & Unsigned_64'Image (Role) &
                        " not allowed for " & pkgId (1 .. pkgIdLen) & LF);
            return;
         end if;
         --  An input role's publish notification (Read: publication; Write
         --  is registration) carries the input badge, which Desktop requires
         --  of every source report.
         Install ((if CuBit.Input.Is_Input_Role (Role) and then Kept = H.Read_Only
                   then Badged (PH.Notifications, Kept, Role, 1, CuBit.Input.Publisher_Tag)
                   else Narrowed (PH.Notifications, Kept, Role, 1)), Handle, Outcome);
         recordAuthority
           (newPID, Index, AUTH_SOURCE_IDENTITY_POLICY, AUTH_REASON_PACKAGE_ID,
            CAP_TYPE_NOTIFICATION, True, Outcome = SB.Granted, Mask (Kept), Role, 0);
         if Outcome = SB.Granted then
            --  Write is registration: this process may provide the role.
            if Kept.Write and then Launched_Service_Role (Role) then
               Provides (Role) := True;
               Registrants (Role) := newPID;
               Registrant_Ready_By (Role) :=
                 CuBit.Monotonic.Milliseconds + Unsigned_64 (readyDeadlineMs);
            end if;
         end if;
      end Mint_Notification;

      --  The boot framebuffer's pages (devmgr's handle); MAPFB checks them.
      procedure Mint_Framebuffer
        (R : Caps_Request; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         Kept : constant H.Rights := Manifest_Rights (Unsigned_64 (R.Rights));
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Not_Available;
         if Valid (PH.Framebuffer) then
            Install (Copy (PH.Framebuffer, Kept), Handle, Outcome);
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_MANIFEST, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_DEVICE_MEM, True, Outcome = SB.Granted, Mask (Kept), 0, 0);
         if Outcome /= SB.Granted then
            Warnings.Warn
              ("procmgr: framebuffer refused" &
               (if Valid (PH.Framebuffer) then "" else " (no framebuffer pages configured)") &
               " for pid" & Image (newPID));
         end if;
      end Mint_Framebuffer;

      --  A network scope: netstack installs it and names it by a tag, which
      --  badges the child's copy of procmgr's netstack endpoint.
      procedure Mint_Network
        (R : Caps_Request; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         scope : CuBit.Network_Authority.Scope;
         valid_Scope : Boolean;
         request : Message := NULL_MESSAGE;
         resultTag : MessageTag;
         Kept : constant H.Rights := Manifest_Rights (Unsigned_64 (R.Rights));
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         CuBit.Network_Authority.Decode (Unsigned_64 (R.Param0), R.Param1, scope, valid_Scope);
         --  A launched child's scope lies within one its launcher was
         --  granted (Launch_Attenuation); a plan entry's, as the plan says.
         if not (valid_Scope
                 and then (if attenuateFor /= No_Process
                           then (if launchSession
                                 then CuBit.Network_Authority.Includes
                                   (CuBit.Network_Authority.Broad_Outbound_TCP, scope)
                                 else launcherNetworkCovers (scope))
                           else Allows (approveNetwork, scope))
                 and then R.Rights = 3 and then Valid (PH.Netstack))
         then
            debugPrint ("procmgr: network scope denied (not approved or invalid)" & LF);
            return;
         end if;
         request.tag.label := CuBit.Network_Authority.OP_INSTALL_SCOPE;
         request.tag.length := 3;
         request.words (0) := To_Word (newPID);
         request.words (1) := Unsigned_64 (scope.Network);
         request.words (2) := CuBit.Network_Authority.Descriptor (scope);
         resultTag := capCall (PH.Network_Policy, request, CuBit.Messages.Wait_Forever);
         if resultTag.label /= REPLY_OK or else request.words (0) not in
           CuBit.Network_Authority.First_Grant_Tag .. CuBit.Network_Authority.Last_Grant_Tag
         then
            Outcome := SB.Not_Available;
            return;
         end if;
         declare
            authorityTag : constant Unsigned_64 := request.words (0);
         begin
            Install (Badged (PH.Netstack, Kept, authorityTag), Handle, Outcome);
            recordAuthority
              (newPID, Index, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
               CAP_TYPE_ENDPOINT, True, Outcome = SB.Granted, Mask (Kept),
               DRIVER_NETSTACK, authorityTag);
            if Outcome = SB.Granted then
               noteNetwork (newPID, scope);
            end if;
            if Outcome /= SB.Granted then
               request := NULL_MESSAGE;
               request.tag.label := CuBit.Network_Authority.OP_RELEASE_SCOPE;
               request.tag.length := 2;
               request.words (0) := To_Word (newPID);
               request.words (1) := authorityTag;
               resultTag := capCall (PH.Network_Policy, request, CuBit.Messages.Wait_Forever);
            end if;
         end;
      end Mint_Network;

      --  The firmware snapshot reader (FIRMWARE_SNAPSHOT): a read-only copy
      --  of devmgr's root, only for a program the trusted startup plan
      --  started and approved, (firmware-snapshot approve-declared). The
      --  request itself (read only, no parameters) is never approval.
      procedure Mint_Firmware_Snapshot
        (R : Caps_Request; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         if Manifest_Rights (Unsigned_64 (R.Rights)) /= H.Read_Only or else R.Param0 /= 0 or else R.Param1 /= 0 then
            Warnings.Warn ("procmgr: firmware snapshot request malformed for pid" & Image (newPID));
         elsif not (systemStartup and then approveFirmwareSnapshot) then
            Warnings.Warn ("procmgr: firmware snapshot refused: " & name &
                           " is not approved by the startup plan");
         elsif not Valid (PH.Firmware_Snapshot_Reader) then
            Outcome := SB.Not_Available;
            Warnings.Warn ("procmgr: firmware snapshot refused: no reader configured");
         else
            Install (Copy (PH.Firmware_Snapshot_Reader, H.Read_Only), Handle, Outcome);
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_FIRMWARE_SNAPSHOT, True, Outcome = SB.Granted, Mask (H.Read_Only), 0, 0);
      end Mint_Firmware_Snapshot;

      --  The platform power object (Request.Platform_Power, KERN-004): a
      --  Write-only copy of devmgr's root, only for a program the trusted
      --  startup plan started and approved, (platform-power
      --  approve-declared). The request itself is never approval.
      procedure Mint_Platform_Power
        (R : Caps_Request; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         if Manifest_Rights (Unsigned_64 (R.Rights)) /= H.Write_Only
           or else R.Param0 /= 0 or else R.Param1 /= 0
         then
            Warnings.Warn ("procmgr: platform power request malformed for pid" & Image (newPID));
         elsif not (systemStartup and then approvePlatformPower) then
            Warnings.Warn ("procmgr: platform power refused: " & name &
                           " is not approved by the startup plan");
         elsif not Valid (PH.Platform_Power_Object) then
            Outcome := SB.Not_Available;
            Warnings.Warn ("procmgr: platform power refused: no object configured");
         else
            Install (Copy (PH.Platform_Power_Object, H.Write_Only), Handle, Outcome);
            if Outcome = SB.Granted then
               debugPrint ("procmgr: " & name & " pid" & Image (newPID) &
                           ": platform power object installed (write)" & LF);
            end if;
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_PLATFORM_POWER, True, Outcome = SB.Granted, Mask (H.Write_Only), 0, 0);
      end Mint_Platform_Power;

      --  Real-time CPU (SCHED-001): the manifest's (request-scheduling ...)
      --  share, narrowed from devmgr's program-sized copy, with no rights
      --  of its own to pass on. Any program procmgr launches may hold one
      --  within that bound (an app's audio thread, so a slow frame drops a
      --  frame and never a period); the kernel admits a reservation only
      --  while the system's real-time total stays within its share.
      procedure Mint_Scheduling
        (Index : ST.Entry_Index; Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         if View.Scheduling_Period = 0 then
            Warnings.Warn ("procmgr: scheduling request without a share for pid" & Image (newPID));
         elsif not H.Valid (PH.Program_Scheduling) then
            Outcome := SB.Not_Available;
            Warnings.Warn ("procmgr: scheduling refused: no authority configured");
         else
            Install (H.Narrowed (PH.Program_Scheduling, View.Scheduling_Budget,
                                 View.Scheduling_Period),
                     Handle, Outcome);
            debugPrint ("procmgr: " & name & " pid" & Image (newPID) & ": real-time share" &
                        Unsigned_64'Image (View.Scheduling_Budget) & " us of" &
                        Unsigned_64'Image (View.Scheduling_Period) & " us " &
                        (if Outcome = SB.Granted then "installed" else "refused") & LF);
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_MANIFEST, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_SCHEDULING, True, Outcome = SB.Granted, 0,
            View.Scheduling_Budget, View.Scheduling_Period);
      end Mint_Scheduling;

      --  The kernel trace object (Request.Trace, Trace_ABI): a Write-only
      --  copy of devmgr's root, only for a program the trusted startup
      --  plan started and approved, (trace approve-declared): the
      --  benchmarks. The request itself is never approval.
      procedure Mint_Trace
        (R : Caps_Request; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         if Manifest_Rights (Unsigned_64 (R.Rights)) /= H.Write_Only
           or else R.Param0 /= 0 or else R.Param1 /= 0
         then
            Warnings.Warn ("procmgr: trace request malformed for pid" & Image (newPID));
         elsif not (systemStartup and then approveTrace) then
            Warnings.Warn ("procmgr: trace refused: " & name &
                           " is not approved by the startup plan");
         elsif not Valid (PH.Trace_Object) then
            Outcome := SB.Not_Available;
            Warnings.Warn ("procmgr: trace refused: no object configured");
         else
            Install (Copy (PH.Trace_Object, H.Write_Only), Handle, Outcome);
            if Outcome = SB.Granted then
               debugPrint ("procmgr: " & name & " pid" & Image (newPID) &
                           ": trace object installed (write)" & LF);
            end if;
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_TRACE, True, Outcome = SB.Granted, Mask (H.Write_Only), 0, 0);
      end Mint_Trace;

      --  The test console object (Kernel_Object.Test_Console,
      --  Test_Console_ABI): a Write-only copy of devmgr's root, only for a
      --  program the trusted startup plan started and approved,
      --  (test-console approve-declared): test builds' logstore. The
      --  request itself is never approval.
      procedure Mint_Test_Console
        (R : Caps_Request; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         if Manifest_Rights (Unsigned_64 (R.Rights)) /= H.Write_Only
           or else R.Param0 /= 0 or else R.Param1 /= 0
         then
            Warnings.Warn ("procmgr: test console request malformed for pid" & Image (newPID));
         elsif not (systemStartup and then approveTestConsole) then
            Warnings.Warn ("procmgr: test console refused: " & name &
                           " is not approved by the startup plan");
         elsif not Valid (PH.Test_Console_Object) then
            Outcome := SB.Not_Available;
            Warnings.Warn ("procmgr: test console refused: no object configured");
         else
            Install (Copy (PH.Test_Console_Object, H.Write_Only), Handle, Outcome);
            if Outcome = SB.Granted then
               debugPrint ("procmgr: " & name & " pid" & Image (newPID) &
                           ": test console object installed (write)" & LF);
            end if;
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_TEST_CONSOLE, True, Outcome = SB.Granted, Mask (H.Write_Only), 0, 0);
      end Mint_Test_Console;

      --  The VM creation authority (Kernel_Object.VM_Create, VMM-001,
      --  docs/hypervisor.md): procmgr is the root issuer of VM creation. A
      --  Write-only copy of devmgr's root, only for a program the trusted
      --  startup plan started and approved, (vm-create approve-declared).
      --  The request itself is never approval.
      procedure Mint_VM_Create
        (R : Caps_Request; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         if Manifest_Rights (Unsigned_64 (R.Rights)) /= H.Write_Only
           or else R.Param0 /= 0 or else R.Param1 /= 0
         then
            Warnings.Warn ("procmgr: VM creation request malformed for pid" & Image (newPID));
         elsif not (systemStartup and then approveVMCreate) then
            Warnings.Warn ("procmgr: VM creation refused: " & name &
                           " is not approved by the startup plan");
         elsif not Valid (PH.VM_Create_Authority) then
            Outcome := SB.Not_Available;
            Warnings.Warn ("procmgr: VM creation refused: no authority configured");
         else
            Install (Copy (PH.VM_Create_Authority, H.Write_Only), Handle, Outcome);
            if Outcome = SB.Granted then
               debugPrint ("procmgr: " & name & " pid" & Image (newPID) &
                           ": VM creation authority installed (write)" & LF);
            end if;
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_VM_CREATE, True, Outcome = SB.Granted, Mask (H.Write_Only), 0, 0);
      end Mint_VM_Create;

      --  The ACPI hardware windows (Request.ACPI_Hardware): only for a program
      --  the trusted startup plan started and approved, (acpi-hardware
      --  approve-declared). Each window devmgr grants is installed as its own
      --  read-write I/O-port capability (no Grant), exactly its range; a window
      --  devmgr refuses (another holder has a port of it) is left out, never
      --  narrowed or widened. The declared entry holds the first window's
      --  handle; the windows are appended after the table is built.
      procedure Mint_ACPI_Resource_Window
        (Index : ST.Entry_Index; Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         use type H.ABI.Kind;
         Actual : constant HIPC.Description :=
           (if CMOS_Boot_Valid then HIPC.Describe (CMOS_Boot_Ports)
            else (Current => False, Why => H.ABI.Invalid_Handle));
      begin
         Handle := ST.No_Handle; Outcome := SB.Refused_By_Policy;
         if CMOS_Window.Status /= CMOS_Windows.Admitted or else CMOS_Window.Binding /= Index
           or else not CMOS_Boot_Valid
           or else (CMOS_Recipient /= No_Process and then CMOS_Recipient /= newPID)
         then return; end if;
         case CMOS_Window.Kind is
            when CMOS_Windows.System_CMOS_Window =>
               if not Actual.Current or else Actual.Kind /= H.ABI.IO_Ports
                 or else Actual.Object /= CMOS_Wire.First_Port
                 or else Actual.Parameter /= Unsigned_64 (CMOS_Window.Ports)
                 or else Actual.Parameter /= CMOS_Wire.Port_Count (CMOS_Boot_Receipt.Device)
                 or else Actual.Held /= Read_Write_Grant
               then return; end if;
               -- The selected named lane belongs to ACPI_Hardware approval,
               -- not to the ordinary SystemIO/FADT register-window list.
               Install (Copy (CMOS_Boot_Ports, H.Read_Write), Handle, Outcome);
               if Outcome = SB.Granted then
                  CMOS_Recipient := newPID;
                  debugPrint ("procmgr: ACPI CMOS window installed for " & name & LF);
               end if;
            when CMOS_Windows.No_Window => null;
         end case;
      end Mint_ACPI_Resource_Window;

      procedure Mint_ACPI_Hardware
        (R : Caps_Request; Index : ST.Entry_Index; Item : ST.Startup_Entry;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         package AX renames CuBit.ACPI_Window_Exchange;
         package AF renames CuBit.ACPI_Fixed_Windows;
         Windows : AF.Window_List;

         --  ACPI's SCI (CuBit.ACPI_Window_Exchange, SCI_Request): devmgr
         --  resolves the line from its own sealed FADT and MADT and gives
         --  it with Grant; procmgr routes it to the child (the kernel
         --  refuses a trigger/polarity another owner disagrees with) and
         --  installs a read-only copy. Every refusal is logged with its
         --  typed reason and leaves an Absent entry.
         procedure Mint_ACPI_SCI is
            package LA renames Interrupt_Line_ABI;
            use type LA.Route_Outcome;
            use type AX.SCI_Refusal;
            Request : Message := NULL_MESSAGE;
            Received : HIPC.Carried;
            Tag : MessageTag;
            Placed : SB.Mint_Outcome := SB.Not_Available;
            Data_Entries : constant Natural := (if ACPI_Data_Word /= ST.No_Handle then 1 else 0);
         begin
            ACPI_SCI_Asked := True;
            if View.Declared.Count + SB.Self_Entries + ACPI_Window_Total + ACPI_Memory_Total
                 + Data_Entries + 1 > ST.Maximum_Entries
            then
               Warnings.Warn ("procmgr: ACPI SCI refused for " & name &
                              ": STARTUP_TABLE_FULL (no entry left for it)");
               return;
            end if;
            Request.tag := (label => AX.SCI_Request, length => AX.SCI_Request_Words,
                            protocol => 0, ipc => <>, reserved => 0);
            --  devmgr reads two sealed tables and answers at once; past the
            --  deadline the SCI is refused, never waited on.
            Tag := HIPC.Call_Accepting
              (PH.Device_Manager, Request, Deadline_After (AX.SCI_Request_Deadline_Ms), Received);
            if Tag.label /= AX.Reply_OK then
               declare
                  Reason : constant Unsigned_64 := Request.words (AX.Reason_Word);
                  Detail : constant Unsigned_64 := Request.words (AX.Detail_Word);
                  Ignore : HIPC.Close_Result := HIPC.Close (Received);
               begin
                  Warnings.Warn
                    ("procmgr: ACPI SCI refused for " & name & ": " &
                     (if Tag.label = AX.Reply_Error and then Tag.length = AX.SCI_Error_Words
                        and then Reason <= AX.SCI_Refusal'Pos (AX.SCI_Refusal'Last)
                      then AX.SCI_Refusal'Image (AX.SCI_Refusal'Val (Reason)) &
                           (if AX.SCI_Refusal'Val (Reason) = AX.Firmware_Refused
                              and then Detail <= CuBit.ACPI_SCI_Route.Refusal'Pos
                                                   (CuBit.ACPI_SCI_Route.Refusal'Last)
                            then " " & CuBit.ACPI_SCI_Route.Refusal'Image
                                         (CuBit.ACPI_SCI_Route.Refusal'Val (Detail))
                            else "")
                      else "DEVICE_MANAGER_UNAVAILABLE"));
               end;
               return;
            end if;
            declare
               Taken : constant HIPC.Accepted_Interrupt := HIPC.Accept_Interrupt (Received);
               Vector : constant Unsigned_64 := Request.words (AX.Vector_Word);
               Route : constant Unsigned_64 := Request.words (AX.Route_Word);
               Routed : LA.Route_Outcome := LA.Not_A_Line;
               Ignore : HIPC.Close_Result;
            begin
               if not Taken.OK then
                  Warnings.Warn ("procmgr: ACPI SCI refused for " & name &
                                 ": NOT_TRANSFERRED " & H.Refusal'Image (Taken.Why));
                  return;
               end if;
               if Vector in LA.First_Vector .. LA.Last_Vector and then LA.Defined_Route (Route) then
                  Routed := CuBit.Handles.Lines.Route
                    (LA.Line_Vector (Vector), newPID, LA.CPU_Of (Route), LA.Trigger_Of (Route),
                     LA.Polarity_Of (Route), Child.Control, Taken.Value);
               end if;
               if Routed = LA.Routed then
                  Install (Copy (Taken.Value, H.Read_Only), ACPI_SCI_Word, Placed);
                  if Placed = SB.Granted then
                     debugPrint ("procmgr: ACPI SCI line" & Unsigned_64'Image (Vector) & " (GSI" &
                                 Unsigned_64'Image (Request.words (AX.GSI_Word)) & ", " &
                                 (case LA.Trigger_Of (Route) is
                                    when LA.Level => "level", when LA.Edge => "edge") & "/" &
                                 (case LA.Polarity_Of (Route) is
                                    when LA.Active_Low => "active-low",
                                    when LA.Active_High => "active-high") &
                                 (if Request.words (AX.Overridden_Word) = 1
                                  then ", MADT override" else ", default") &
                                 ") routed to " & name & LF);
                  else
                     ACPI_SCI_Word := ST.No_Handle;
                     Warnings.Warn ("procmgr: ACPI SCI refused for " & name & ": INSTALL_" &
                                    SB.Mint_Outcome'Image (Placed));
                  end if;
               else
                  Warnings.Warn ("procmgr: ACPI SCI refused for " & name & ": ROUTE_" &
                                 LA.Route_Outcome'Image (Routed));
               end if;
               Ignore := HIPC.Close (Taken.Value);
            end;
         end Mint_ACPI_SCI;
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         if Manifest_Rights (Unsigned_64 (R.Rights)) /= H.Read_Write or else R.Param0 /= 0
           or else R.Param1 /= 0 or else Item.Label.Length > AHA.Maximum_Binding_Length
         then
            Warnings.Warn ("procmgr: ACPI hardware request malformed for pid" & Image (newPID));
         elsif not (systemStartup and then approveACPIHardware) then
            Warnings.Warn ("procmgr: ACPI hardware refused: " & name &
                           " is not approved by the startup plan");
         else
            Windows := acpiWindows;
            if Windows.Count > AHA.Maximum_IO_Windows
              or else View.Declared.Count + SB.Self_Entries + Windows.Count > ST.Maximum_Entries
            then
               Outcome := SB.Limit_Reached;
               Warnings.Warn ("procmgr: ACPI hardware refused: " & name &
                              "'s startup table has no room for its windows");
            else
               Outcome := SB.Granted;
               for W of Windows.Windows (1 .. Windows.Count) loop
                  declare
                     Request : Message := NULL_MESSAGE;
                     Received : HIPC.Carried;
                     Tag : MessageTag;
                     Placed : SB.Mint_Outcome := SB.Not_Available;
                     Word : Unsigned_64 := ST.No_Handle;
                  begin
                     Request.tag := (label => AX.Window_Request, length => AX.Request_Words,
                                     protocol => 0, ipc => <>, reserved => 0);
                     Request.words (AX.First_Word) := Unsigned_64 (W.First);
                     Request.words (AX.Count_Word) := Unsigned_64 (W.Count);
                     Tag := HIPC.Call_Accepting
                       (PH.Device_Manager, Request, CuBit.Messages.Wait_Forever, Received);
                     if Tag.label = AX.Reply_OK then
                        declare
                           Taken : constant HIPC.Accepted_IO_Ports := HIPC.Accept_IO_Ports (Received);
                           Ignore : HIPC.Close_Result;
                        begin
                           if Taken.OK then
                              Install (Copy (Taken.Value, H.Read_Write), Word, Placed);
                              Ignore := HIPC.Close (Taken.Value);
                           end if;
                        end;
                     end if;
                     if Placed = SB.Granted then
                        ACPI_Window_Total := ACPI_Window_Total + 1;
                        ACPI_Window_Words (ACPI_Window_Total) := Word;
                     elsif Placed in SB.Limit_Reached | SB.Out_Of_Memory then
                        Outcome := Placed;   --  the child's table: the launch fails
                     else
                        Warnings.Warn ("procmgr: ACPI window" & Unsigned_16'Image (W.First) & " +" &
                                       Unsigned_32'Image (W.Count) & " left out for " & name &
                                       ": devmgr refused it (its ports are held)");
                     end if;
                  end;
                  exit when Outcome /= SB.Granted;
               end loop;
               --  The firmware-memory windows (slice 2): devmgr's ACPI NVS
               --  runs and FACS page, each the kernel's root exactly,
               --  installed read-write without grant, until devmgr has no
               --  further window. Each must fit the child's startup table.
               -- PCI buses are requested on demand through the approved
               -- devmgr endpoint; startup memory enumeration carries NVS/FACS.
               if Outcome = SB.Granted then
                  for K in AHA.Memory_Window_Number loop
                     declare
                        Request : Message := NULL_MESSAGE;
                        Received : HIPC.Carried;
                        Tag : MessageTag;
                        Placed : SB.Mint_Outcome := SB.Not_Available;
                        Word : Unsigned_64 := ST.No_Handle;
                     begin
                        Request.tag := (label => AX.Memory_Window_Request, length => AX.Request_Words,
                                        protocol => 0, ipc => <>, reserved => 0);
                        Request.words (AX.Window_Word) := Unsigned_64 (K);
                        Tag := HIPC.Call_Accepting
                          (PH.Device_Manager, Request, CuBit.Messages.Wait_Forever, Received);
                        exit when Tag.label /= AX.Reply_OK;
                        declare
                           Taken : constant HIPC.Accepted_Device_Memory :=
                             HIPC.Accept_Device_Memory (Received);
                           What : constant HIPC.Description :=
                             (if Taken.OK then HIPC.Describe (Taken.Value)
                              else (Current => False, Why => H.ABI.Invalid_Handle));
                           Ignore : HIPC.Close_Result;
                        begin
                           if What.Current and then What.Authority_Tag = H.ABI.Firmware_PCI_Config_Tag then
                              -- Stale enumerators must not inject a PCI pregrant.
                              -- Named buses are acquired only through devmgr's
                              -- independently approved PCI-window endpoint.
                              Placed := SB.Not_Available;
                              Warnings.Warn ("procmgr: stage acpi-startup-memory fail PCI_PREGRANT_REFUSED");
                           elsif View.Declared.Count + SB.Self_Entries + ACPI_Window_Total
                                   + ACPI_Memory_Total + 1 > ST.Maximum_Entries
                           then
                              Placed := SB.Limit_Reached;
                           elsif Taken.OK then
                              Install (Copy (Taken.Value, H.Read_Write), Word, Placed);
                           end if;
                           if Taken.OK then
                              Ignore := HIPC.Close (Taken.Value);
                           end if;
                           if Placed = SB.Granted then
                              ACPI_Memory_Total := ACPI_Memory_Total + 1;
                              ACPI_Memory_Words (ACPI_Memory_Total) := Word;
                              debugPrint ("procmgr: ACPI memory window" & Natural'Image (ACPI_Memory_Total) &
                                          " installed for " & name & LF);
                           else
                              Outcome := (if Placed in SB.Limit_Reached | SB.Out_Of_Memory then Placed
                                          else SB.Not_Available);
                              Warnings.Warn ("procmgr: ACPI hardware refused: " & name &
                                             "'s firmware-memory window" & Natural'Image (K) &
                                             " could not be installed");
                           end if;
                        end;
                        exit when Outcome /= SB.Granted;
                     end;
                  end loop;
               end if;
               --  Table-named firmware data: devmgr's endpoint badged for
               --  the grant (CuBit.Firmware_Data_Exchange.Authority_Tag),
               --  without Grant, only when the plan also approves
               --  (acpi-firmware-data approve-declared). devmgr answers
               --  Data_Window_Request through this badge and nothing else.
               if Outcome = SB.Granted and then approveFirmwareData then
                  declare
                     Placed : SB.Mint_Outcome := SB.Not_Available;
                  begin
                     if View.Declared.Count + SB.Self_Entries + ACPI_Window_Total
                          + ACPI_Memory_Total + 1 > ST.Maximum_Entries
                     then
                        Placed := SB.Limit_Reached;
                     elsif Valid (PH.Device_Inspection) then
                        Install (Badged (PH.Device_Inspection, H.Read_Write,
                                         CuBit.Firmware_Data_Exchange.Authority_Tag),
                                 ACPI_Data_Word, Placed);
                     end if;
                     if Placed = SB.Granted then
                        debugPrint ("procmgr: ACPI firmware data approved for " & name & LF);
                     else
                        ACPI_Data_Word := ST.No_Handle;
                        Outcome := (if Placed in SB.Limit_Reached | SB.Out_Of_Memory then Placed
                                    else SB.Not_Available);
                        Warnings.Warn ("procmgr: ACPI hardware refused: " & name &
                                       "'s firmware-data endpoint could not be installed");
                     end if;
                  end;
               end if;
               --  A grant is never empty: at least one I/O or memory window
               --  (a hardware-reduced platform has no fixed I/O blocks). The
               --  declared entry holds the first one's handle.
               if Outcome = SB.Granted then
                  case AHA.Declared_Holds (ACPI_Window_Total, ACPI_Memory_Total) is
                     when AHA.First_IO_Window =>
                        Handle := ACPI_Window_Words (1);
                        ACPI_Binding := Item.Label;
                     when AHA.First_Memory_Window =>
                        Handle := ACPI_Memory_Words (1);
                        ACPI_Binding := Item.Label;
                     when AHA.No_Window =>
                        Outcome := SB.Not_Available;
                        Warnings.Warn ("procmgr: ACPI hardware refused: no admissible window for " & name);
                  end case;
               end if;
               --  The SCI comes with an admitted grant; its refusal leaves
               --  the windows as they are.
               if Outcome = SB.Granted then
                  Mint_ACPI_SCI;
               end if;
            end if;
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_IO_PORTS, True, Outcome = SB.Granted, Mask (H.Read_Write),
            0, Unsigned_64 (ACPI_Window_Total));
      end Mint_ACPI_Hardware;

      --  A kernel publication page (Request.Publication, KERN-002): granted
      --  to any program whose manifest declares it (it names nothing but
      --  scheduling statistics); the kernel maps it with the launch block,
      --  and the entry's handle word is the page's address.
      procedure Mint_Publication
        (R : Caps_Request; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         package PG renames CuBit.Publication_Grants;
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         if Manifest_Rights (Unsigned_64 (R.Rights)) /= H.Read_Only or else not PG.Known (R.Param0)
           or else R.Param1 /= 0
         then
            Warnings.Warn ("procmgr: publication request malformed for pid" & Image (newPID));
         else
            Granted_Publications := Granted_Publications or
              System_Publications.Bit (PG.Publication_Of (R.Param0));
            Handle := PG.Page_Address (PG.Publication_Of (R.Param0));
            Outcome := SB.Granted;
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_PUBLICATION, True, Outcome = SB.Granted, Mask (H.Read_Only),
            0, Unsigned_64 (R.Param0));
      end Mint_Publication;

      --  An offerable self endpoint (Request.Offerable_Self, UI-020): a copy
      --  of procmgr's endpoint to the child, to the child, with Read and
      --  Grant only. Never Write: no holder of it, or of a copy offered on,
      --  can call the child. Installed only because the child's own
      --  manifest declares it.
      procedure Mint_Self_Offer
        (R : Caps_Request; Index : ST.Entry_Index; Item : ST.Startup_Entry;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         Kept : constant H.Rights := (Read | Grant => True, others => False);
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         if R.Rights /= CuBit.Self_Offers.Manifest_Rights or else R.Param0 /= 0 or else R.Param1 /= 0
           or else not Valid (Child.Mailbox)
         then
            Warnings.Warn ("procmgr: offerable self request malformed for pid" & Image (newPID));
         else
            Install (Copy (Child.Mailbox, Kept), Handle, Outcome);
            if Outcome = SB.Granted and then launchIndex (newPID) /= 0 then
               Launches.Values (launchIndex (newPID)).Offerable_Self := True;
            end if;
            debugPrint ("procmgr: " & name & " pid" & Image (newPID) & ": offerable self endpoint """ &
                        String (Item.Label.Text (1 .. Item.Label.Length)) & """ (read, grant) " &
                        (if Outcome = SB.Granted then "installed" else "refused") & LF);
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_MANIFEST, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_ENDPOINT, True, Outcome = SB.Granted, Mask (Kept), To_Word (newPID), 0);
      end Mint_Self_Offer;

      procedure Mint
        (Into : in out Build_Context; Item : ST.Startup_Entry; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         R : Caps_Request;
      begin
         Handle := ST.No_Handle;
         Into.Logged := False;
         --  Withheld from a launched child: its launcher does not hold it
         --  (said when the requests were checked). Never introduced later.
         if Withheld (Index) then
            Outcome := SB.Refused_By_Policy;
            Into.Logged := True;
            return;
         end if;
         --  A device-resource entry is supplied only by an admitted typed
         --  ACPI window. Other resources, including DMA, remain devmgr-owned.
         if View.Request_Of (Index) = 0 then
            if Item.Kind = ST.IO_Ports and then CMOS_Window.Binding = Index
              and then CMOS_Window.Status = CMOS_Windows.Admitted
            then
               Mint_ACPI_Resource_Window (Index, Handle, Outcome);
            elsif Item.Kind = ST.Scheduling then
               Mint_Scheduling (Index, Handle, Outcome);
            else
               Outcome := (if Item.Kind = ST.DMA then SB.Not_Available else SB.Refused_By_Policy);
            end if;
            return;
         end if;
         R := View.Caps (View.Request_Of (Index));
         case Item.Kind is
            when ST.Endpoint     =>
               if R.Kind = CuBit.Self_Offers.Manifest_Request then
                  Mint_Self_Offer (R, Index, Item, Handle, Outcome);
               else
                  Mint_Service (R, Index, Item, Handle, Outcome, Into.Logged);
               end if;
            when ST.Notification => Mint_Notification (R, Index, Handle, Outcome);
            when ST.Framebuffer  => Mint_Framebuffer (R, Index, Handle, Outcome);
            when ST.Network      => Mint_Network (R, Index, Handle, Outcome);
            when ST.Firmware_Snapshot => Mint_Firmware_Snapshot (R, Index, Handle, Outcome);
            when ST.Platform_Power => Mint_Platform_Power (R, Index, Handle, Outcome);
            when ST.Trace => Mint_Trace (R, Index, Handle, Outcome);
            when ST.Test_Console => Mint_Test_Console (R, Index, Handle, Outcome);
            when ST.VM_Create => Mint_VM_Create (R, Index, Handle, Outcome);
            when ST.ACPI_Hardware => Mint_ACPI_Hardware (R, Index, Item, Handle, Outcome);
            when ST.Publication => Mint_Publication (R, Index, Handle, Outcome);
            when ST.Render =>
               --  The session devmgr admitted before the table was built;
               --  a software start has none.
               Outcome := SB.Not_Available;
               if Valid (Render_Endpoint) then
                  Install (Copy (Render_Endpoint, H.Read_Write), Handle, Outcome);
               end if;
               recordAuthority
                 (newPID, Index, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
                  CAP_TYPE_ENDPOINT, True, Outcome = SB.Granted, Mask (H.Read_Write), 0, 0);
            when others =>
               Outcome := SB.Refused_By_Policy;
         end case;
      end Mint;

      procedure Report
        (Into : in out Build_Context; Item : ST.Startup_Entry; Index : ST.Entry_Index;
         Outcome : SB.Mint_Outcome)
      is
         pragma Unreferenced (Index);
         use type ST.Need;
      begin
         if Outcome /= SB.Granted and then not Into.Logged then
            if Item.Requirement = ST.Required then
               --  The launch fails; said once per program and entry (CAP-003).
               CuBit.Refusal_Notes.Warn_Once
                 ("procmgr", name & " " & String (Item.Label.Text (1 .. Item.Label.Length)),
                  name & "'s required startup entry """ &
                  String (Item.Label.Text (1 .. Item.Label.Length)) & """ (" &
                  SB.Mint_Outcome'Image (Outcome) & "), so its launch");
            else
               debugPrint ("procmgr: " & name & ": optional capability """ &
                           String (Item.Label.Text (1 .. Item.Label.Length)) &
                           """ not granted (" & SB.Mint_Outcome'Image (Outcome) & ")" & LF);
            end if;
         end if;
      end Report;

      procedure Build is new SB.Build (Build_Context, Mint, Report);

      --  CAP-003 startup waits, before the child exists: the service entries
      --  whose provider has started but not registered yet, in Waits when
      --  this launch may wait. A required entry that nothing started
      --  provides, or whose provider is already past its deadline, refuses
      --  the launch here, typed. An entry for a startup-only authority this
      --  launch may not have is left to Mint_Service, which refuses it.
      procedure Await_Providers (Refused : out Boolean) is
         use type ST.Need;
         Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
         --  Bounded by the manifest's own table (ST.Maximum_Entries); only
         --  the entries found are kept, on the heap.
         Found : Awaited_Services (ST.Entry_Index);
         Count : ST.Entry_Count := 0;
         State : Provider_State;
         Provider : Process_ID;
         Deadline : Unsigned_64;
      begin
         Refused := False;
         for I in 1 .. View.Declared.Count loop
            if View.Declared.Entries (I).Kind = ST.Endpoint and then View.Request_Of (I) /= 0
              and then View.Caps (View.Request_Of (I)).Kind = REQ_SERVICE
            then
               declare
                  Item : ST.Startup_Entry renames View.Declared.Entries (I);
                  Role : constant Unsigned_64 :=
                    Unsigned_64 (View.Caps (View.Request_Of (I)).Param0);
                  Provider_Role : constant Unsigned_64 := Provider_Role_Of (Role);
                  Required : constant Boolean := Item.Requirement = ST.Required;
               begin
                  if Provider_Role <= Maximum_Role
                    and then not Startup_Only_Denied (Role, View.Caps (View.Request_Of (I)).Rights)
                  then
                     classifyProvider (Provider_Role, Now, State, Provider, Deadline);
                     if State in Wait_Refusal and then Required then
                        reportWaitRefusal (name, Item.Label, Provider_Role, State);
                        Refused := True;
                        return;
                     elsif State = Provider_Starting and then mayWait then
                        Count := Count + 1;
                        Found (Count) :=
                          (Label => Item.Label, Role => Provider_Role, Provider => Provider,
                           Deadline => Deadline, Required => Required, Resolved => False);
                        debugPrint ("procmgr: launch of " & name & " waits for service """ &
                                    String (Item.Label.Text (1 .. Item.Label.Length)) &
                                    """ (role" & Unsigned_64'Image (Provider_Role) &
                                    ") to register (CAP-003)" & LF);
                     end if;
                  end if;
               end;
            end if;
         end loop;
         if Count > 0 then
            Waits := new Awaited_Services'(Found (1 .. Count));
         end if;
      end Await_Providers;
   begin
      Waits := null;
      Retry := False;
      Identity := No_Process;
      --  Refined below where a failure has a more specific reason.
      Failure := LA.Spawn_Failed;
      if startupRole = CCL.Configurations.Config_Storage and then
        (not systemStartup or Config_Storage_Selected)
      then
         debugPrint ("procmgr: Config storage role not available" & LF);
         Failure := LA.Manifest_Refused;
         return No_Process;
      end if;
      debugPrint ("procmgr: spawn: ");
      debugPrint (name);
      debugPrint ("" & LF);

      t0 := CuBit.Monotonic.Milliseconds;
      Span_Start := t0;
      elfSize := readFileFromFS (name, File_Request_Deadline_Ms);
      t1 := CuBit.Monotonic.Milliseconds;
      Span_Read := t1;

      if elfSize = 0 then
         debugPrint ("procmgr: file read failed" & LF);
         Failure := LA.Executable_Not_Found;
         return No_Process;
      end if;

      debugPrint ("procmgr: read ");
      printDec (Unsigned_32 (elfSize));
      debugPrint (" bytes in ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);

      --  The manifest first: its handle limit sizes the child's table.
      readManifest (elfSize, View, Reading);
      if Reading in Manifest_Malformed then
         Warnings.Warn ("procmgr: launch of " & name & " refused: unreadable capability manifest (" &
                        Manifest_Reading'Image (Reading) & ")");
         Failure :=
           (case Reading is
              when Startup_Table_Missing => LA.Startup_Table_Missing,
              when Startup_Table_Invalid | Requests_Disagree => LA.Startup_Table_Invalid,
              when others => LA.Manifest_Refused);
         return No_Process;
      end if;

      --  The package identity next: whether a launch may wait for a
      --  provider depends on it (an identity-gated role such as
      --  acpi-power, Startup_Only_Denied), so it is known before the wait
      --  is decided, not only at minting.
      parseIdSection (elfSize, pkgId, pkgIdLen);

      --  A starting provider is waited for, parked, before anything runs.
      declare
         Refused : Boolean;
      begin
         Await_Providers (Refused);
         if Refused then
            Failure := LA.Service_Unavailable;
            return No_Process;
         elsif Waits /= null then
            return No_Process;
         end if;
      end;

      t0 := CuBit.Monotonic.Milliseconds;
      declare
         Spawned : constant HC.Spawn_Result := HC.Spawn
           (Authority => PH.Process_Root, Image => imageBuf, Bytes => elfSize,
            Name => name (name'First .. name'First + Natural'Min (name'Length, 16) - 1),
            --  Launch-into-S: the child's supervisor is the session S, a
            --  node in procmgr, so procmgr is its kernel parent; the
            --  requester gets a launch handle (answerLaunched), not the
            --  parent's place. A plan entry or a supervisor's Start_Child
            --  names its parent as before.
            Parent => (if launchSession then No_Process else requester),
            Limit => View.Declared.Limit);
      begin
         if not Spawned.Spawned then
            debugPrint ("procmgr: spawn refused: " & H.ABI.Refusal'Image (Spawned.Why) & LF);
            if Spawned.Why in H.ABI.Out_Of_Memory then
               Failure := LA.Out_Of_Memory;
            end if;
            return No_Process;
         end if;
         Child := Spawned.Value;
      end;
      t1 := CuBit.Monotonic.Milliseconds;
      Span_Spawned := t1;
      newPID := Child.Child;

      debugPrint ("procmgr: SYSCALL_SPAWN took ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);

      --  The child's incarnation, from its own Control handle: the launcher
      --  and the render broker match it by PID and generation.
      Captured_Child := GPU_Grants.Capture (Child.Control);
      if not GPU_Grants.Valid (Captured_Child)
        or else GPU_Grants.Process_ID (Captured_Child) /= newPID
      then
         debugPrint ("procmgr: launched child identity unavailable" & LF);
         Stop_Suspended_Child;
         return No_Process;
      end if;

      -- Select the typed CMOS named window under the same manifest request
      -- and trusted startup approval as the ACPI_Hardware grant. Ordering of
      -- manifest entries cannot turn an unapproved resource into authority.
      CMOS_Window := CMOS_Windows.Select_Window
        (View.CMOS_Declaration,
         Approved => systemStartup and then approveACPIHardware and then Declares_ACPI_Hardware,
         Receipt_Known => CMOS_Boot_Valid,
         Recipient_Available => CMOS_Recipient = No_Process or else CMOS_Recipient = newPID,
         Receipt => CMOS_Boot_Receipt);

      --  The launch block's payload: launch arguments, and a program with a
      --  description (its ports and descriptor map) gets it attached, so it
      --  learns its ports at start; it gets a block with just its name when
      --  its launcher gave none. A malformed description refuses the
      --  launch. The startup table follows the payload (installed below).
      declare
         Description_At : Unsigned_64;
         Description_Length : CuBit.Program_Descriptions.Descriptor_Length;
         Description_Valid : Boolean;
         Accepted : Boolean := True;
         Parameter_Section : CuBit.Parameter_Sections.Bytes (1 .. CuBit.Parameter_Sections.Maximum_Section_Bytes);
         Parameter_Length : CuBit.Parameter_Sections.Section_Length := 0;
      begin
         findDescription
           (elfSize, Description_At, Description_Length, Description_Valid, streamBitmask);
         if Description_Length > 0 and then not Description_Valid then
            debugPrint ("procmgr: invalid program description" & LF);
            Stop_Suspended_Child;
            Failure := LA.Manifest_Refused;
            return No_Process;
         end if;
         --  A device-class Start_Child (SUP-001): the program's own
         --  .cubit.devices must declare the class, or the supervisor's
         --  class scope does not cover it.
         if pendingClassLength > 0 then
            declare
               package DS renames CCL.Device_Sections;
               use type DS.Decode_Status;
               Section_At : Unsigned_64 := 0;
               Section_Length : Natural := 0;
               Table : DS.Match_Table;
               Status : DS.Decode_Status := DS.Invalid_Length;
               Wanted : DS.ACPI_Text := [others => ASCII.NUL];
               Drives : Boolean := False;
            begin
               findSection (elfSize, DS.MAGIC, DS.MAX_SECTION_BYTES, Section_At, Section_Length);
               if Section_Length in 1 .. DS.MAX_SECTION_BYTES then
                  declare
                     Source : constant DS.Byte_Array (1 .. Section_Length)
                       with Import, Address => imageBuf + Storage_Offset (Section_At);
                     Copy : constant DS.Byte_Array (1 .. Section_Length) := Source;
                  begin
                     DS.Decode (Copy, Table, Status);
                  end;
               end if;
               for K in 1 .. pendingClassLength loop
                  Wanted (K) := pendingClass (K);
               end loop;
               if Status = DS.Decoded then
                  for K in 1 .. Table.Count loop
                     Drives := Drives or else DS.Names_ACPI (Table.Entries (K), Wanted, pendingClassLength);
                  end loop;
               end if;
               if not Drives then
                  debugPrint ("procmgr: " & name & " does not drive ACPI_ID " &
                              pendingClass (1 .. pendingClassLength) & "; class launch refused" & LF);
                  classRefused := True;
                  Stop_Suspended_Child;
                  Failure := LA.Not_Granted;
                  return No_Process;
               end if;
            end;
         end if;
         --  Typed parameters (docs/ccl-launch-parameters.md): the
         --  launcher's value is checked against the program's signature
         --  and placed, canonical, in the trailer between the rings and
         --  the description. A program that declares parameters needs a
         --  value; one that declares none takes none.
         declare
            package PS renames CuBit.Parameter_Signatures;
            package PV renames CuBit.Parameter_Sections;
            use type PS.Validation;
            Signature_At : Unsigned_64;
            Signature_Length : Natural;
            Refused : Boolean := False;
         begin
            findParameters (elfSize, Signature_At, Signature_Length);
            --  A device-class launch (SUP-001): the plan's Device_Class_Launch
            --  typed against this program's own record less the
            --  supervisor's facts, then merged with them (Class_Launch_Checks).
            --  The merged record is then checked below like any parameters.
            if pendingClassLength > 0 then
               declare
                  use type Class_Launch_Checks.Outcome;
                  Checked : Class_Launch_Checks.Result;
               begin
                  if Signature_Length = 0 or else Signature_Length > PS.Maximum_Descriptor_Bytes then
                     Checked.Status := Class_Launch_Checks.Not_Typed;
                     Checked.Message_Length := 0;
                  else
                     declare
                        Source : constant PS.Bytes (1 .. Signature_Length)
                          with Import, Address => imageBuf + Storage_Offset (Signature_At);
                        Descriptor : constant PS.Bytes := Source;
                     begin
                        if PS.Validate (Descriptor) /= PS.Valid
                          or else PS.Signature_Of (Descriptor)'Length > Child_Parameters.Maximum_Signature_Bytes
                          or else PS.Root_Of (Descriptor)'Length not in 1 .. Child_Parameters.Maximum_Root_Bytes
                        then
                           Checked.Status := Class_Launch_Checks.Schema_Refused;
                        else
                           Class_Launch_Checks.Check
                             (PS.Signature_Of (Descriptor), PS.Root_Of (Descriptor),
                              pendingClassLaunch.Data (1 .. pendingClassLaunch.Length),
                              pendingFacts, pendingFactCount, Checked);
                        end if;
                     end;
                  end if;
                  if Checked.Status /= Class_Launch_Checks.Merged then
                     classLaunchRefusal := Checked.Status;
                     classLaunchMessageLength := Checked.Message_Length;
                     classLaunchMessage (1 .. Checked.Message_Length) := Checked.Message (1 .. Checked.Message_Length);
                     debugPrint ("procmgr: class launch refused: " & name & ": " &
                                 Checked.Message (1 .. Checked.Message_Length) & LF);
                     Stop_Suspended_Child;
                     Failure := LA.Parameters_Rejected;
                     return No_Process;
                  end if;
                  pendingParameters := (others => <>);
                  pendingParameters.Length := Checked.Length;
                  pendingParameters.Data (1 .. Checked.Length) := Checked.Merged_Value (1 .. Checked.Length);
               end;
            end if;
            declare
            Given : String renames pendingParameters.Data (1 .. pendingParameters.Length);
            begin
            if Signature_Length = 0 and then Given'Length > 0 then
               debugPrint ("procmgr: parameters refused: " & name & " declares none" & LF);
               Failure := LA.Parameters_Unexpected;
               Refused := True;
            elsif Signature_Length > 0 then
               declare
                  Source : constant PS.Bytes (1 .. Natural'Min (Signature_Length, PS.Maximum_Descriptor_Bytes))
                    with Import, Address => imageBuf + Storage_Offset (Signature_At);
                  Descriptor : constant PS.Bytes := Source;
                  Checked : CCL.Typed_Values.Check_Result;
               begin
                  Refused := True;
                  if Signature_Length > PS.Maximum_Descriptor_Bytes
                    or else PS.Validate (Descriptor) /= PS.Valid
                  then
                     debugPrint ("procmgr: parameters refused: " & name & ": malformed .cubit.parameters" & LF);
                     Failure := LA.Parameter_Signature_Invalid;
                  elsif Given'Length = 0 then
                     debugPrint ("procmgr: parameters refused: " & name & " requires " &
                                 PS.Root_Of (Descriptor) & " and the launch gave none" & LF);
                     Failure := LA.Parameters_Missing;
                  else
                     CCL.Typed_Values.Check (PS.Signature_Of (Descriptor), PS.Root_Of (Descriptor), Given, Checked);
                     if not Checked.Success then
                        debugPrint ("procmgr: parameters refused: " & name & ": " &
                                    Checked.Message (1 .. Checked.Message_Length) & LF);
                        Failure := LA.Parameters_Rejected;
                     else
                        if PS.Root_Of (Descriptor) = "ACPI_Firmware_Profile" then
                           declare
                              Profile : CMOS_Profile_Codec.ACPI_Firmware_Profile_Value;
                              Decoded : Boolean;
                           begin
                              CMOS_Profile_Codec.Decode (Checked.Canonical (1 .. Checked.Length), Profile, Decoded);
                              if not Decoded then
                                 Failure := LA.Parameters_Rejected; Stop_Suspended_Child; return No_Process;
                              end if;
                              Profile := CMOS_Profile_Codec.Bind_CMOS
                                (Profile,
                                 (case CMOS_Boot_Receipt.Device is
                                    when CMOS_Wire.Unidentified => CMOS_Profile_Codec.No_CMOS,
                                    when CMOS_Wire.Legacy_64 => CMOS_Profile_Codec.PC_AT_64,
                                    when CMOS_Wire.Intel_128 => CMOS_Profile_Codec.Intel_128,
                                    when CMOS_Wire.Intel_256 => CMOS_Profile_Codec.Intel_256),
                                 Known => CMOS_Boot_Valid,
                                 NMI_Disabled => CMOS_Boot_Receipt.NMI_Disabled,
                                 Approved => CMOS_Window.Status = CMOS_Windows.Admitted);
                              CCL.Typed_Values.Check (PS.Signature_Of (Descriptor), PS.Root_Of (Descriptor),
                                CMOS_Profile_Codec.Encode (Profile), Checked);
                              if not Checked.Success then
                                 Failure := LA.Parameters_Rejected; Stop_Suspended_Child; return No_Process;
                              end if;
                           end;
                        end if;
                        PV.Encode (PS.Identity_Of (Descriptor), Checked.Canonical (1 .. Checked.Length),
                                   Parameter_Section, Parameter_Length, Accepted);
                        if not Accepted then
                           debugPrint ("procmgr: parameters refused: " & name &
                                       ": the value does not fit its section" & LF);
                           Failure := LA.Parameters_Rejected;
                           Accepted := True;
                        else
                           debugPrint ("procmgr: parameters " & name & ": " &
                                       Checked.Canonical (1 .. Checked.Length) & LF);
                           Refused := False;
                        end if;
                     end if;
                  end if;
               end;
            end if;
            end;
            if Refused then
               Stop_Suspended_Child;
               return No_Process;
            end if;
         end;
         if Description_Length > 0 or else Parameter_Length > 0 then
            if Block_Length = 0 then
               declare
                  Builder : LA.Builder;
                  Length : LA.Present_Length;
               begin
                  LA.Start (Builder);
                  LA.Add_Argument (Builder, name, Accepted);
                  if Accepted then
                     LA.Finish (Builder, Length, Accepted);
                  end if;
                  if Accepted then
                     launchBlock (1 .. Length) := Builder.Data (1 .. Length);
                     Block_Length := Length;
                  end if;
               end;
            end if;
            if Accepted then
               declare
                  Source : constant LA.Block (1 .. Description_Length)
                    with Import, Address => imageBuf + Storage_Offset (Description_At);
                  Rings : constant CuBit.Outlet_Rings.Table :=
                    (if Description_Length > 0 then deriveRings (newPID, Source, Child.Mailbox)
                     else (others => <>));
                  Ring_Table : CuBit.Outlet_Rings.Bytes (1 .. CuBit.Outlet_Rings.Maximum_Bytes);
                  Ring_Length : CuBit.Outlet_Rings.Table_Length;
                  Length : LA.Present_Length := Block_Length;
               begin
                  CuBit.Outlet_Rings.Encode (Rings, Ring_Table, Ring_Length);
                  declare
                     --  Rings, then the parameters section, then the description.
                     Trailer : LA.Block (1 .. Ring_Length + Parameter_Length + Description_Length);
                  begin
                     for K in 1 .. Ring_Length loop
                        Trailer (K) := Ring_Table (K);
                     end loop;
                     for K in 1 .. Parameter_Length loop
                        Trailer (Ring_Length + K) := Parameter_Section (K);
                     end loop;
                     Trailer (Ring_Length + Parameter_Length + 1 .. Trailer'Last) := Source;
                     LA.Attach_Description (launchBlock, Length, Trailer, Accepted);
                  end;
                  Block_Length := Length;
               end;
            end if;
            if not Accepted then
               debugPrint ("procmgr: program description does not fit its launch block" & LF);
               Stop_Suspended_Child;
               Failure := LA.Manifest_Refused;
               return No_Process;
            end if;
         end if;
      end;
      resetLaunchState (newPID);
      loadLaunchTable (elfSize, newPID);

      --  PIDs are reusable.  Never attribute records retained from an earlier
      --  process generation to the new child.
      clearAuthorityForPID (newPID);
      --  Nothing a previous holder of this PID had in netstack survives.
      releaseNetworkOwner (newPID);

      --  SPAWN placed the child's own endpoint and process handles; its
      --  startup table names them last ("self", "self-process").
      recordAuthority
        (newPID, View.Declared.Count + 1, AUTH_SOURCE_KERNEL_BOOTSTRAP,
         AUTH_REASON_SELF_BOOTSTRAP, CAP_TYPE_ENDPOINT, False, True,
         Mask (H.Read_Write), To_Word (newPID), 0);
      recordAuthority
        (newPID, View.Declared.Count + 2, AUTH_SOURCE_KERNEL_BOOTSTRAP,
         AUTH_REASON_SELF_BOOTSTRAP, CAP_TYPE_PROCESS, False, True,
         Mask (H.Read_Write), To_Word (newPID), 0);

      --  The package identity (.cubit.id) was parsed before the waits.
      noteProcess (newPID, requester, pkgId (1 .. pkgIdLen), Child.Control, Child.Mailbox);
      Sources.Started (newPID, pkgId (1 .. pkgIdLen), name);
      Recorded := Records.Find (newPID) /= 0;
      --  Only a program that declares the metric outlet publishes metrics;
      --  only its identity is metrics.svc's to know.
      if Recorded and then
        ST.Find (View.Declared, ST.To_Name (CuBit.Program_Descriptions.Metric_Outlet_Binding)) /= 0
      then
         Records.Values (Records.Find (newPID)).Metric_Outlet := True;
         Metric_Sources.Started (newPID, pkgId (1 .. pkgIdLen), name);
      end if;

      --  A launched child gets only what its launcher holds, with at most
      --  the launcher's rights (Launch_Attenuation): an optional request it
      --  does not hold is withheld, a required one refuses the launch. Note
      --  a render request (granted only after admission, below).
      for I in 1 .. View.Declared.Count loop
         if View.Request_Of (I) /= 0 then
            declare
               R : Caps_Request renames View.Caps (View.Request_Of (I));
               Label : constant String := String (View.Declared.Entries (I).Label.Text
                                                    (1 .. View.Declared.Entries (I).Label.Length));
               use type ST.Need, LAtt.Verdict;
               Verdict : constant LAtt.Verdict :=
                 (if attenuateFor = No_Process then LAtt.Given
                  else LAtt.Decide
                    (launchGives (R.Kind, R.Param0, R.Rights, R.Param1),
                     Optional => View.Declared.Entries (I).Requirement /= ST.Required));
            begin
               if Verdict = LAtt.Refused then
                  attenuationRefused := True;
                  if withheldLength = 0 then
                     withheldLength := Natural'Min (Label'Length, CuBit.Launch_Arguments.Withheld_Label_Bytes);
                     withheldLabel (1 .. withheldLength) :=
                       Label (Label'First .. Label'First + withheldLength - 1);
                  end if;
                  Warnings.Warn ("procmgr: launch of " & name & " refused: it requires """ &
                                 Label & """, which " &
                                 (if launchSession then "its session withholds"
                                  else "its launcher does not hold"));
               elsif Verdict = LAtt.Withheld then
                  Withheld (I) := True;
                  Warnings.Inform ("procmgr: " & name & ": optional """ & Label &
                                   """ withheld: " &
                                   (if launchSession then "outside its session's envelope"
                                    else "its launcher does not hold it"));
               elsif R.Kind = CuBit.Render_Authority.Manifest_Request then
                  -- A render endpoint is admitted by devmgr, never copied by
                  -- the generic service path.
                  Render.Valid := not Render.Requested and then
                    R.Rights = 3 and then R.Param0 in 0 .. 1 and then R.Param1 = 0;
                  Render.Requested := True;
                  if Render.Valid then
                     Render.Demand := (if R.Param0 = 1 then CuBit.Render_Startup.Optional
                                       else CuBit.Render_Startup.Required);
                  end if;
               end if;
            end;
         end if;
      end loop;
      --  Render approved for this child's own render request: a child it
      --  launches may ask for render too (launcherHolds).
      if launchIndex (newPID) /= 0 then
         Launches.Values (launchIndex (newPID)).Render_Approved :=
           approveRender and then Render.Requested and then Render.Valid;
      end if;

      -- A software retry must retain explicit executable opt-in. Reject
      -- malformed requests and Config storage roles before external attachment.
      if (Force_Software and then not Render.Requested) or else
        (Render.Requested and then
         (not Render.Valid or else
          (Render.Demand = CuBit.Render_Startup.Optional and then
           startupRole /= CCL.Configurations.Application) or else
          (Force_Software and then Render.Demand /= CuBit.Render_Startup.Optional)))
      then
         Stop_Suspended_Child;
         Failure := LA.Render_Denied;
         return No_Process;
      end if;

      --  A recycled PID must not inherit the previous occupant's service
      --  policy or open handles, including when this ELF has no access section.
      --  The child is still suspended: failure to establish default-deny is
      --  a launch failure, not a reason to resume with unknown authority.
      declare
         resetMessage : Message := NULL_MESSAGE;
         resetTag : MessageTag;
      begin
         resetMessage.tag :=
           (label => CuBit.Filesystems.OP_REVOKE_ACL, length => 1,
            protocol => 0, ipc => <>, reserved => 0);
         resetMessage.words (0) := To_Word (newPID);
         resetTag := capCall (PH.Filesystem, resetMessage, CuBit.Messages.Wait_Forever);
         if resetTag.label /= CuBit.Filesystems.REPLY_OK then
            debugPrint ("procmgr: filesystem policy reset failed" & LF);
            Stop_Suspended_Child;
            return No_Process;
         end if;
         -- Config scopes are also keyed by PID. Reset even when the new ELF
         -- declares no Config scopes; absence must mean deny, not inheritance.
         if Registered_Driver (DRIVER_CONFIG) /= No_Process then
            declare
               Config_Reset : Message := NULL_MESSAGE;
            begin
               Config_Reset.tag :=
                 (label => CuBit.Filesystems.OP_REVOKE_ACL, length => 1,
                  protocol => 0, ipc => <>, reserved => 0);
               Config_Reset.words (0) := To_Word (newPID);
               resetTag := capCall (PH.Config, Config_Reset, CuBit.Messages.Wait_Forever);
               if resetTag.label /= CuBit.Filesystems.REPLY_OK then
                  debugPrint ("procmgr: config policy reset failed" & LF);
                  Stop_Suspended_Child;
                  return No_Process;
               end if;
            end;
         end if;
         --  Metrics scopes likewise (MET-002).
         if ensureMetricsPolicy then
            declare
               Metrics_Reset : Message := NULL_MESSAGE;
            begin
               Metrics_Reset.tag :=
                 (label => CuBit.Metric_Protocol.Operation'Enum_Rep (CuBit.Metric_Protocol.Revoke_Scopes),
                  length => CuBit.Metric_Protocol.Message_Words, protocol => 0, ipc => <>, reserved => 0);
               Metrics_Reset.words (0) := To_Word (newPID);
               resetTag := capCall (Metrics_Policy, Metrics_Reset, CuBit.Messages.Wait_Forever);
               if resetTag.label /= REPLY_OK then
                  debugPrint ("procmgr: metrics policy reset failed" & LF);
                  Stop_Suspended_Child;
                  return No_Process;
               end if;
            end;
         end if;
         --  TLS scopes are keyed by PID too: a reused PID must not inherit
         --  another program's names.
         if ensureTLSPolicy then
            declare
               TLS_Reset : Message := NULL_MESSAGE;
            begin
               TLS_Reset.tag :=
                 (label => CuBit.TLS_Protocol.Revoke_Operation, length => 1,
                  protocol => 0, ipc => <>, reserved => 0);
               TLS_Reset.words (0) := To_Word (newPID);
               resetTag := capCall (TLS_Policy, TLS_Reset, CuBit.Messages.Wait_Forever);
               if resetTag.label /= REPLY_OK then
                  debugPrint ("procmgr: tls policy reset failed" & LF);
                  Stop_Suspended_Child;
                  return No_Process;
               end if;
            end;
         end if;
      end;

      --  Parse .cubit.access and install only its validated scopes.
      Span_Reset := CuBit.Monotonic.Milliseconds;
      declare
         Policy_Ready : Boolean;
      begin
         parseAndSendACL (newPID, elfSize, Policy_Ready,
                          binaryName => name,
                          networkApproved => approveNetwork /= No_Network or else launchSession);
         if Policy_Ready and then not attenuationRefused and then
           not launchDirectoryVisible (newPID)
         then
            directoryRefused := True;
         end if;
         if not Policy_Ready or else attenuationRefused or else directoryRefused then
            -- Child has never run. Remove partial service-side installation
            -- BEFORE killing it, while its PID is still occupied; cleanup
            -- must not accidentally address a later occupant of that PID.
            Failure :=
              (if attenuationRefused or else directoryRefused then LA.Not_Granted
               else LA.Capability_Denied);
            Discard_Authorized_Child;
            return No_Process;
         end if;
      end;

      if startupRole = CCL.Configurations.Config_Storage then
         declare
            Attachment : Message := NULL_MESSAGE;
         begin
            --  Selected by trusted init.ccl, not by executable identity or a
            --  public spawn request. Never overwrite an installed backend.
            --  config.svc takes the worker's endpoint from this call's offer.
            Config_Storage_Selected := True;
            if Registered_Driver (DRIVER_CONFIG) = No_Process then
               Failure := LA.Service_Unavailable;
               Discard_Authorized_Child;
               return No_Process;
            end if;
            Attachment.tag := (label => Config_Worker_Startup.Operation'Enum_Rep
              (Config_Worker_Startup.Attach_Worker), length => 1, protocol => 0, ipc => <>, reserved => 0);
            Attachment.words (0) := To_Word (newPID);
            --  Badged: the worker accepts only requests carrying this tag
            --  from the registered Config (Config_Worker_Startup).
            Attachment.tag := HIPC.Call_Offering
              (PH.Config, Attachment, CuBit.Messages.Wait_Forever,
               Badged (Child.Mailbox, H.Read_Write, Config_Worker_Startup.Worker_Request_Tag));
            if Attachment.tag.label /= REPLY_OK or Attachment.tag.length /= 1
              or Attachment.tag.protocol /= 0 or Attachment.tag.reserved /= 0
              or Attachment.words /= [0, 0, 0, 0]
            then
               debugPrint ("procmgr: Config storage attachment failed" & LF);
               Discard_Authorized_Child;
               return No_Process;
            end if;
         end;
      end if;

      if Render.Requested then
         declare
            use type Render_Launch.Phase, CuBit.Render_Startup.Decision;
            Receipt : aliased CompletionEntry;
            Consumed, Accepted : Boolean := False;
            Now : Unsigned_64 := CuBit.Monotonic.Milliseconds;
            Deadline : Unsigned_64 := Now;
            Activity : Activity_Result;
            Mode : constant CuBit.Render_Startup.Attempt :=
              (if Force_Software then CuBit.Render_Startup.Software_Only else
               CuBit.Render_Startup.Initial_Attempt
                 (Render.Demand, approveRender));
            Next : CuBit.Render_Startup.Decision;
            Entry_State : CuBit.Render_Startup.Capability := CuBit.Render_Startup.Unknown;
         begin
            Identity := GPU_Grants.Incarnation (Captured_Child);
            if Force_Software
              and then not CuBit.Render_Startup.Fresh_Retry (Prior_Incarnation, Identity)
            then
               Discard_Authorized_Child;
               return No_Process;
            end if;
            debugPrint ("procmgr: render attempt incarnation=" & Image (Identity) &
              " software=" & Boolean'Image (Mode = CuBit.Render_Startup.Software_Only) & LF);
            -- Only a trusted startup plan supplies this approval, or a
            -- launcher it was approved for passes it on (launcherHolds).
            if Mode = CuBit.Render_Startup.With_Render and then
              approveRender and then Render.Valid and then
              Now <= Unsigned_64'Last - 6_000
            then
               Deadline := Now + 6_000;
               Render_Launch.Start
                 (Render_Launcher, True, Captured_Child, Render_Ticket);
               if Render_Launch.State (Render_Launcher, Render_Ticket) = Render_Launch.Pending then
                  debugPrint ("procmgr: render admission submitted; child suspended" & LF);
               end if;
               -- Startup already executes sequential policy handshakes. Do
               -- not resume this child while its render admission is pending.
               -- This is the sole completion-token owner in procmgr today.
               while Render_Launch.State (Render_Launcher, Render_Ticket) = Render_Launch.Pending loop
                  Now := CuBit.Monotonic.Milliseconds;
                  exit when Now >= Deadline;
                  if Poll_Completion (Receipt'Address) = 1 then
                     Render_Launch.Complete (Render_Launcher, Receipt, Consumed);
                     if not Consumed then
                        debugPrint ("procmgr: unexpected render completion" & LF);
                     end if;
                  else
                     Activity := Wait_For_Activity_Until (Deadline);
                     exit when Activity = Unavailable;
                  end if;
               end loop;
               Accepted := Render_Launch.State (Render_Launcher, Render_Ticket) =
                 Render_Launch.Admitted;
               if Accepted then
                  Render_Endpoint := Render_Launch.Render (Render_Launcher, Render_Ticket);
                  Accepted := Valid (Render_Endpoint);
               end if;
            end if;
            --  What the child's render entry will be: the admitted session,
            --  or nothing (a software start installs none).
            if Mode = CuBit.Render_Startup.Software_Only then
               Entry_State := CuBit.Render_Startup.Empty;
            elsif Accepted then
               Entry_State := CuBit.Render_Startup.Render_Endpoint;
            end if;
            Next := CuBit.Render_Startup.Decide
              (Render.Demand, Mode,
               approveRender and Render.Valid,
               (case Render_Launch.State (Render_Launcher, Render_Ticket) is
                  when Render_Launch.Absent => CuBit.Render_Startup.Not_Requested,
                  when Render_Launch.Pending => CuBit.Render_Startup.Pending,
                  when Render_Launch.Rejected => CuBit.Render_Startup.Rejected,
                  when Render_Launch.Admitted => CuBit.Render_Startup.Admitted,
                  when Render_Launch.Uncertain => CuBit.Render_Startup.Uncertain),
               Entry_State);
            if Next not in CuBit.Render_Startup.Resume_Render | CuBit.Render_Startup.Resume_Software then
               -- Timeout is NOT remote cancellation or retirement; this
               -- child is discarded and a late success can never resume it.
               debugPrint ("procmgr: render admission denied; child not resumed" & LF);
               declare
                  --  Discarding forgets the ticket: name its outcome first.
                  --  Named here: the runtime discards enumeration names.
                  Outcome : constant String :=
                    (case Render_Launch.State (Render_Launcher, Render_Ticket) is
                       when Render_Launch.Absent => "not requested",
                       when Render_Launch.Rejected => "rejected",
                       when Render_Launch.Pending => "unanswered",
                       when Render_Launch.Admitted => "admitted",
                       when Render_Launch.Uncertain => "uncertain");
               begin
                  Discard_Authorized_Child;
                  Retry := CuBit.Render_Startup.Retry_Software (Next, Stop_Accepted);
                  Warnings.Inform
                    ("procmgr: " & name & " render admission " & Outcome &
                     "; launch attempt pid" & Image (newPID) & " withdrawn" &
                     (if Retry then ", retrying with software rendering" else ""));
               end;
               Failure := LA.Render_Denied;
               return No_Process;
            end if;
            if Next = CuBit.Render_Startup.Resume_Render then
               debugPrint ("procmgr: render admission complete" & LF);
               Warnings.Inform ("procmgr: stage render-admission ok " & name & " GPU session admitted");
            else
               Render_Endpoint := H.No_Endpoint;
               debugPrint ("procmgr: render software-only admitted" & LF);
               if approveRender and then Mode /= CuBit.Render_Startup.Software_Only then
                  Warnings.Inform ("procmgr: stage render-admission fail " & name &
                    " admission=" & (case Render_Launch.State (Render_Launcher, Render_Ticket) is
                                       when Render_Launch.Absent => "not-requested",
                                       when Render_Launch.Rejected => "rejected",
                                       when Render_Launch.Pending => "unanswered",
                                       when Render_Launch.Admitted => "admitted",
                                       when Render_Launch.Uncertain => "uncertain") &
                    "; software rendering");
               end if;
            end if;
         end;
      end if;

      --  The startup table, appended to the launch block, which goes in
      --  before the child first runs (the kernel takes it only then, once).
      t0 := CuBit.Monotonic.Milliseconds;
      declare
         package LE renames CuBit.Launch_Envelopes;
         Built : ST.Table;
         Result : SB.Build_Result;
         Encoded : ST.Bytes (1 .. ST.Maximum_Table_Bytes);
         Encoded_Length : ST.Table_Length;
         Envelope_Length : LE.Block_Length := Block_Length;
         Fits : Boolean;
         use type SB.Build_Status;
      begin
         Build (Context, View.Declared,
                HC.Table_Word (Child.Child_Self), HC.Table_Word (Child.Child_Self_Process),
                Built, Result);
         if Result.Status /= SB.Ready then
            Warnings.Warn
              ("procmgr: launch of " & name & " refused: startup table " &
               SB.Build_Status'Image (Result.Status) &
               (if Result.Index in 1 .. View.Declared.Count
                then " at """ & String (View.Declared.Entries (Result.Index).Label.Text
                         (1 .. View.Declared.Entries (Result.Index).Label.Length)) &
                     """ (" & SB.Mint_Outcome'Image (Result.Why) & ")"
                else ""));
            Failure :=
              (if Result.Status /= SB.Required_Refused then LA.Startup_Table_Invalid
               else
                 (case Result.Why is
                    when SB.Not_Available | SB.Timed_Out => LA.Service_Unavailable,
                    when SB.Out_Of_Memory => LA.Out_Of_Memory,
                    when others => LA.Capability_Denied));
            Discard_Authorized_Child;
            return No_Process;
         end if;
         --  The introducer (CuBit.Late_Introduction): procmgr's endpoint,
         --  badged, when a decision was kept for a late introduction. Its
         --  name is never a declared one; without room it is left out (the
         --  program runs without the optional services, as before).
         if Late_Kept then
            declare
               Label : constant ST.Name := ST.To_Name (LI.Binding);
               Handle : Unsigned_64;
               Outcome : SB.Mint_Outcome;
            begin
               if Built.Count < ST.Maximum_Entries and then ST.Find (Built, Label) = 0 then
                  Install (Badged (PH.Self_Grantable, H.Read_Write, LI.Introducer_Tag), Handle, Outcome);
                  if Outcome = SB.Granted then
                     Built.Count := Built.Count + 1;
                     Built.Entries (Built.Count) :=
                       (Label => Label, Kind => ST.Endpoint, Requirement => ST.Optional,
                        State => ST.Granted, Handle => Handle);
                  end if;
               end if;
               if Built.Count = 0 or else ST.Find (Built, Label) = 0 then
                  Warnings.Warn ("procmgr: " & name & ": no room for its introducer; " &
                                 "its optional services stay absent");
                  forgetLate (newPID);
               end if;
            end;
         end if;
         --  The ACPI windows, after the built entries, each under its own
         --  name (CuBit.ACPI_Hardware_Authority): never in place of a
         --  declared name, never past the table.
         for K in 1 .. ACPI_Window_Total loop
            declare
               Label : constant ST.Name := ST.To_Name
                 (AHA.IO_Label (String (ACPI_Binding.Text (1 .. ACPI_Binding.Length)), K));
            begin
               if Built.Count = ST.Maximum_Entries or else ST.Find (Built, Label) /= 0
                 or else not ST.Valid_Name (Label)
               then
                  Warnings.Warn ("procmgr: launch of " & name &
                                 " refused: its ACPI window names do not fit its startup table");
                  Failure := LA.Startup_Table_Invalid;
                  Discard_Authorized_Child;
                  return No_Process;
               end if;
               Built.Count := Built.Count + 1;
               Built.Entries (Built.Count) :=
                 (Label => Label, Kind => ST.IO_Ports, Requirement => ST.Required,
                  State => ST.Granted, Handle => ACPI_Window_Words (K));
            end;
         end loop;
         for K in 1 .. ACPI_Memory_Total loop
            declare
               Label : constant ST.Name := ST.To_Name
                 (AHA.Memory_Label (String (ACPI_Binding.Text (1 .. ACPI_Binding.Length)), K));
            begin
               if Built.Count = ST.Maximum_Entries or else ST.Find (Built, Label) /= 0
                 or else not ST.Valid_Name (Label)
               then
                  Warnings.Warn ("procmgr: launch of " & name &
                                 " refused: its ACPI memory window names do not fit its startup table");
                  Failure := LA.Startup_Table_Invalid;
                  Discard_Authorized_Child;
                  return No_Process;
               end if;
               Built.Count := Built.Count + 1;
               Built.Entries (Built.Count) :=
                 (Label => Label, Kind => ST.Device_Memory, Requirement => ST.Required,
                  State => ST.Granted, Handle => ACPI_Memory_Words (K));
            end;
         end loop;
         if ACPI_Data_Word /= ST.No_Handle then
            declare
               Label : constant ST.Name := ST.To_Name
                 (CuBit.Firmware_Data_Exchange.Data_Label
                    (String (ACPI_Binding.Text (1 .. ACPI_Binding.Length))));
            begin
               if Built.Count = ST.Maximum_Entries or else ST.Find (Built, Label) /= 0
                 or else not ST.Valid_Name (Label)
               then
                  Warnings.Warn ("procmgr: launch of " & name &
                                 " refused: its firmware-data endpoint name does not fit its startup table");
                  Failure := LA.Startup_Table_Invalid;
                  Discard_Authorized_Child;
                  return No_Process;
               end if;
               Built.Count := Built.Count + 1;
               Built.Entries (Built.Count) :=
                 (Label => Label, Kind => ST.Endpoint, Requirement => ST.Required,
                  State => ST.Granted, Handle => ACPI_Data_Word);
            end;
         end if;
         --  The SCI line: Granted, or Absent when it was asked and refused.
         if ACPI_SCI_Asked then
            declare
               Label : constant ST.Name := ST.To_Name
                 (AHA.SCI_Label (String (ACPI_Binding.Text (1 .. ACPI_Binding.Length))));
            begin
               if Built.Count = ST.Maximum_Entries or else ST.Find (Built, Label) /= 0
                 or else not ST.Valid_Name (Label)
               then
                  Warnings.Warn ("procmgr: launch of " & name &
                                 " refused: its ACPI SCI name does not fit its startup table");
                  Failure := LA.Startup_Table_Invalid;
                  Discard_Authorized_Child;
                  return No_Process;
               end if;
               Built.Count := Built.Count + 1;
               Built.Entries (Built.Count) :=
                 (if ACPI_SCI_Word /= ST.No_Handle
                  then (Label => Label, Kind => ST.Interrupt, Requirement => ST.Optional,
                        State => ST.Granted, Handle => ACPI_SCI_Word)
                  else (Label => Label, Kind => ST.Interrupt, Requirement => ST.Optional,
                        State => ST.Absent, Handle => ST.No_Handle));
            end;
         end if;
         ST.Encode (Built, Encoded, Encoded_Length);
         for K in 1 .. Block_Length loop
            launchEnvelope (K) := launchBlock (K);
         end loop;
         LE.Append (launchEnvelope, Envelope_Length, Encoded (1 .. Encoded_Length), Fits);
         if not Fits or else not HC.Install_Launch_Block
           (newPID, Child.Control, launchEnvelope'Address, Unsigned_64 (Envelope_Length),
            Granted_Publications)
         then
            debugPrint ("procmgr: launch block not installed" & LF);
            Discard_Authorized_Child;
            return No_Process;
         end if;
      end;
      t1 := CuBit.Monotonic.Milliseconds;
      debugPrint ("procmgr: startup table took ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);
      --  The CSPACE works only until the child runs; procmgr's copy of the
      --  render session is the child's now.
      Close_Launch_Handles;

      --  Resource quotas from the config store (overwrites elfBuf), given
      --  to the kernel as the child starts.
      Span_Setup := CuBit.Monotonic.Milliseconds;
      declare
         maxFrames   : Unsigned_32 := 0;
         cpuQuotaUs  : Unsigned_32 := 0;
         cpuPeriodUs : Unsigned_32 := 0;
      begin
         if pkgIdLen > 0 then
            queryConfigQuota (pkgId (1 .. pkgIdLen), maxFrames, cpuQuotaUs, cpuPeriodUs);
         else
            queryConfigQuota (name, maxFrames, cpuQuotaUs, cpuPeriodUs);
         end if;
         if maxFrames /= 0 or cpuQuotaUs /= 0 then
            recordAuthority
              (newPID, 0, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_CONFIG_QUOTA,
               0, False, True, 0, Unsigned_64 (maxFrames),
               Unsigned_64 (cpuQuotaUs) or Shift_Left (Unsigned_64 (cpuPeriodUs), 32));
            debugPrint ("procmgr: config resource quota applied" & LF);
         end if;
         --  A held Start_Child (SUP-001) waits for its supervisor's
         --  Release (releaseChild), which applies the same quotas.
         if pendingHold then
            debugPrint ("procmgr: held PID " & Image (newPID) & " for its supervisor" & LF);
         elsif not HC.Resume
           (newPID, Child.Control, Unsigned_64 (maxFrames),
            Unsigned_64 (cpuQuotaUs) or Shift_Left (Unsigned_64 (cpuPeriodUs), 32))
         then
            debugPrint ("procmgr: child resume rejected" & LF);
            Discard_Authorized_Child;
            return No_Process;
         end if;
      end;
      declare
         Known : constant Records.Index := Records.Find (newPID);
      begin
         if Known /= 0 and then not pendingHold then
            Records.Values (Known).Resumed := True;
         end if;
      end;

      debugPrint ("procmgr: resumed PID ");
      debugPrint (Image (newPID));
      debugPrint ("" & LF);

      declare
         Done : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
         function Ms (From, To : Unsigned_64) return String is
            Text : constant String := Unsigned_64'Image (if To >= From then To - From else 0);
         begin
            return Text (Text'First + 1 .. Text'Last);
         end Ms;
      begin
         if Done - Span_Start >= Slow_Spawn_Ms then
            Warnings.Inform ("procmgr: spawn " & name & " pid" & Image (newPID) & " took " &
                             Ms (Span_Start, Done) & " ms: read " & Ms (Span_Start, Span_Read) &
                             ", kernel spawn " & Ms (Span_Read, Span_Spawned) &
                             ", policy resets " & Ms (Span_Spawned, Span_Reset) &
                             ", access and grants " & Ms (Span_Reset, Span_Setup) &
                             ", quota and resume " & Ms (Span_Setup, Done));
         end if;
      end;

      --  A provider started: once it registers, every holder waiting for
      --  its role is introduced to it (devmgr: logstore and the desktop;
      --  programs: the optional entries they were started without).
      for Role in Provides'Range loop
         if Provides (Role) then
            Introduce_When_Registered (Role) := True;
         end if;
      end loop;

      --  Notify requester about declared streams (after resume).
      if streamBitmask /= 0 and then requester /= No_Process then
         declare
            evMsg : Message := NULL_MESSAGE;
            Requester_Endpoint : constant CuBit.Handles.Endpoint := mailboxOf (requester);
         begin
            evMsg.tag := (label => OP_STREAM_AVAILABLE, length => 2, protocol => 0, ipc => <>, reserved => 0);
            evMsg.words (0) := To_Word (newPID);
            evMsg.words (1) := streamBitmask;
            if Valid (Requester_Endpoint) then
               sendEvent (requester, evMsg, Requester_Endpoint);
               debugPrint ("procmgr: sent stream available" & LF);
            else
               debugPrint ("procmgr: stream available not sent (requester not procmgr's)" & LF);
            end if;
         end;
      end if;

      return newPID;
   end Spawn_Attempt;

   function spawnByName
     (name        : String;
      requester   : Process_ID := No_Process;
      approveNetwork : Network_Approval := No_Network;
      approveRender : Boolean := False;
      systemStartup : Boolean := False;
      startupRole : CCL.Configurations.Startup_Role := CCL.Configurations.Application;
      approveFirmwareSnapshot : Boolean := False;
      approveACPIHardware : Boolean := False;
      approvePCIConfig : Boolean := False;
      approveFirmwareData : Boolean := False;
      approveDisplayCapture : Boolean := False;
      approvePlatformPower : Boolean := False;
      approveTrace : Boolean := False;
      approveTestConsole : Boolean := False;
      approveVMCreate : Boolean := False;
      readyDeadlineMs : CCL.Configurations.Ready_Deadline;
      mayWait : Boolean;
      Waits : out Awaited_Access;
      Failure : out CuBit.Launch_Arguments.Launch_Failure)
      return Process_ID
   is
      Retry : Boolean;
      Identity, Retry_Identity : Process_ID;
      Result : Process_ID;
   begin
      --  PWR-003: nothing starts once the system is stopping.
      if Shutting_Down then
         Waits := null;
         Failure := CuBit.Launch_Arguments.Spawn_Failed;
         Warnings.Warn ("procmgr: launch of " & name & " refused: the system is stopping");
         return No_Process;
      end if;
      Result := Spawn_Attempt
        (False, No_Process, Retry, Identity, Failure, name, requester,
         approveNetwork, approveRender, systemStartup, startupRole, approveFirmwareSnapshot,
         approveACPIHardware, approvePCIConfig, approveFirmwareData,
         approveDisplayCapture, approvePlatformPower, approveTrace, approveTestConsole,
         approveVMCreate, readyDeadlineMs, mayWait, Waits);
      if Result = No_Process and then Retry then
         -- Exactly one fresh attempt, with no broker request. The failed
         -- attempt's launcher resources stay retained independently. The
         -- first got past its waits, so this one never waits.
         debugPrint ("procmgr: render retry software with fresh child" & LF);
         Result := Spawn_Attempt
           (True, Identity, Retry, Retry_Identity, Failure, name, requester,
            approveNetwork, approveRender, systemStartup, startupRole,
            approveFirmwareSnapshot, approveACPIHardware, approvePCIConfig,
            approveFirmwareData, approveDisplayCapture, approvePlatformPower, approveTrace,
            approveTestConsole, approveVMCreate,
            readyDeadlineMs, False, Waits);
      end if;
      return Result;
   end spawnByName;

   ---------------------------------------------------------------------------
   --  Parked launches (CAP-003 startup waits): launches waiting for their
   --  starting providers, oldest first, on the heap (CuAlloc), as many as
   --  are waiting. The service loop looks at them every
   --  Registration_Poll_Ms while any is parked (resumeParked); registration
   --  is the kernel's registry publication (Registered_Driver), which
   --  procmgr reads without a call, and a provider's end is its
   --  EVENT_CHILD_EXIT, which clears its Registrants entry.
   --  A startup plan entry holds the plan's later entries back (they start
   --  in order); an OP_LAUNCH request keeps its requester's
   --  reply, saved, and is served again from its message when its waits end.
   ---------------------------------------------------------------------------
   type Parked_Kind is (Plan_Launch, Launch_Request);
   type Parked_Launch;
   type Parked_Access is access Parked_Launch;
   type Parked_Launch (Name_Length : Natural) is record
      Kind         : Parked_Kind := Plan_Launch;
      --  For its refusal.
      Name         : String (1 .. Name_Length);
      --  Plan_Launch: its plan entry.
      Entry_Number : CCL.Configurations.Launch_Number := 0;
      --  Requests: the requester, its request and its saved reply.
      Sender       : Process_ID := No_Process;
      Request      : Message := NULL_MESSAGE;
      Answer       : H.Reply := H.No_Reply;
      Waits        : Awaited_Access;
      Next         : Parked_Access;
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Parked_Launch, Parked_Access);
   Parked_First, Parked_Last : Parked_Access := null;

   procedure park (Item : Parked_Access) is
   begin
      if Parked_Last = null then
         Parked_First := Item;
      else
         Parked_Last.Next := Item;
      end if;
      Parked_Last := Item;
   end park;

   --  A request served again once its waits end: answered through the reply
   --  saved when it was parked, and never parked again.
   Deferred_Answer : H.Reply := H.No_Reply;
   function servingDeferred return Boolean is (H.Valid (Deferred_Answer));

   --  Answer the OP_LAUNCH request being served.
   procedure answerRequest
     (dest : Process_ID; label : Unsigned_32; word0 : Unsigned_64;
      More : CuBit.Launch_Arguments.Label_Words := [others => 0];
      Length : Unsigned_8 := 1)
   is
      Answer : Message := NULL_MESSAGE;
      Ignore : Boolean;
      Ignore_Sent : Unsigned_64;
   begin
      Answer.tag := (label => label, length => Length, protocol => 0, ipc => <>, reserved => 0);
      Answer.words := [word0, More (1), More (2), More (3)];
      if servingDeferred then
         --  Used up either way; a requester gone since has no one to tell.
         Ignore := HIPC.Answer (Deferred_Answer, Answer);
         Deferred_Answer := H.No_Reply;
      else
         Ignore_Sent := reply (dest, Answer);
      end if;
   end answerRequest;

   --  Park the request being served until its Waits end, keeping its reply;
   --  False (Waits freed) when the reply cannot be kept.
   function parkRequest
     (Kind : Parked_Kind; sender : Process_ID; msg : Message; Name : String;
      Waits : in out Awaited_Access) return Boolean
   is
      Saved : constant HIPC.Saved_Reply := HIPC.Save_Reply;
   begin
      if not Saved.Saved then
         debugPrint ("procmgr: launch of " & Name & " cannot wait: its reply was not kept" & LF);
         Free (Waits);
         return False;
      end if;
      park (new Parked_Launch'
              (Name_Length => Name'Length, Kind => Kind, Name => Name, Entry_Number => 0,
               Sender => sender, Request => msg, Answer => Saved.Handle, Waits => Waits,
               Next => null));
      Waits := null;
      return True;
   end parkRequest;

   ---------------------------------------------------------------------------
   --  Staged programs (SELF-002, device builds): a launch-table directory
   --  (CuBit.Launch_Authority.Admits_Staged) admits programs beneath it
   --  while the device owner allows it: the Desktop's "Allow app
   --  replacement" switch, which only the registered Desktop sets
   --  (Staged_Switch_Operation). Off until then. The launcher must also
   --  hold a read scope covering the program's name. A staged program is
   --  not signature-checked: signed admission is not built yet.
   ---------------------------------------------------------------------------
   Staged_Switch_Operation : constant Unsigned_32 := CuBit.Launch_Authority.Staged_Switch_Operation;
   Staged_Launches_Allowed : Boolean := False;

   type Launch_Admission is (Named, Staged, Staged_Switch_Off, Not_Admitted);

   function admitLaunch (Launcher : Launch_State; Name : String) return Launch_Admission is
      Readable : Boolean := False;
   begin
      if Launcher.Table_Length = 0 then
         return Not_Admitted;
      elsif LAuth.Contains (Launcher.Table (1 .. Launcher.Table_Length), Name) then
         return Named;
      elsif not LAuth.Admits_Staged (Launcher.Table (1 .. Launcher.Table_Length), Name) then
         return Not_Admitted;
      end if;
      for h in 1 .. Launcher.Scope_Count loop
         Readable := Readable or else LAuth.Scope_Covered
           (Launcher.Scopes (h).Service, Launcher.Scopes (h).Rights,
            Launcher.Scopes (h).Prefix (1 .. Launcher.Scopes (h).Length),
            SERVICE_FS, CuBit.Launch_Authority.Read_Right, Name);
      end loop;
      if not Readable then
         return Not_Admitted;
      end if;
      return (if Staged_Launches_Allowed then Staged else Staged_Switch_Off);
   end admitLaunch;

   --  Staged_Switch_Operation: words (0) = 1 allows staged launches, 0
   --  stops them (running staged programs keep running). Only from the
   --  registered Desktop.
   procedure handleStagedSwitch (sender : Process_ID; msg : Message) is
   begin
      if sender = No_Process or else sender /= Registered_Driver (DRIVER_DESKTOP)
        or else msg.tag.length /= 1 or else msg.words (0) not in LAuth.Staged_Switch_Off | LAuth.Staged_Switch_On
      then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;
      Staged_Launches_Allowed := msg.words (0) = LAuth.Staged_Switch_On;
      Warnings.Inform ("procmgr: staged app launches " &
                       (if Staged_Launches_Allowed then "allowed" else "stopped") &
                       " by the Desktop switch");
      sendReply (sender, REPLY_OK, 0);
   end handleStagedSwitch;

   --  procmgr's own identity (its own endpoint names it), found once.
   Own_Identity : Process_ID := No_Process;
   function ownIdentity return Process_ID is
   begin
      if Own_Identity = No_Process then
         declare
            D : constant HIPC.Description := HIPC.Describe (PH.Self_Grantable);
         begin
            if D.Current then
               Own_Identity := From_Word (D.Object);
            end if;
         end;
      end if;
      return Own_Identity;
   end ownIdentity;

   --  A process's name for the log: its manifest identity, "devmgr",
   --  "procmgr", or its identity number.
   function processName (Process : Process_ID) return String is
      Known : constant Records.Index := Records.Find (Process);
   begin
      if Process /= No_Process and then Process = Registered_Driver (DRIVER_DEVMGR) then
         return "devmgr";
      elsif Process /= No_Process and then Process = ownIdentity then
         return "procmgr";
      elsif Known /= 0 and then Records.Values (Known).Valid then
         return Records.Values (Known).Identity (1 .. Records.Values (Known).Identity_Length);
      end if;
      return "process";
   end processName;

   ---------------------------------------------------------------------------
   --  Launch handles (CAP-012, docs/authority-model.md 7.5 item 5): what a
   --  Launch-into-S launch gives its requester. A handle is procmgr's own
   --  endpoint badged with one issuance's tag (CuBit.Launch_Handles); the
   --  issuance names the child and the rights. procmgr is the child's
   --  kernel parent (its session supervisor S is a node here), so the
   --  handle is the requester's only way to it: observe it and its exit,
   --  stop or end it, and ask procmgr to mint Debug_Observe on it. The
   --  decisions are Handle_Admission's (proved); this acts on them.
   --
   --  Issuances live on the heap (CuAlloc), as many as are held, up to
   --  Issuance_Ceiling (a named ceiling: past it a launch still runs, its
   --  reply says Capacity and carries no handle). An issuance outlives its
   --  child (Child_Gone: Observe answers the exit outcome) until its holder
   --  revokes it or ends; a revoked one answers Revoked while its child
   --  runs, and goes when the child ends or the table needs the room.
   ---------------------------------------------------------------------------
   package LH renames CuBit.Launch_Handles;
   package HA renames Handle_Admission;
   package HD renames CuBit.Handles.Debug;
   use type LH.Outcome, LH.Operation, LH.Control_Request, LH.Debug_Path;
   use type HA.Issuance_State, HA.Route, HA.Target_Class;
   use type HIPC.Reply_Outcome, CuBit.Child_Exits.Termination_Kind;

   type Issuance is record
      --  An LH.Serial; 0: a free entry.
      Serial : Unsigned_64 := 0;
      --  The issuance it was narrowed from; 0: issued by a launch.
      Parent : Unsigned_64 := 0;
      Child  : Process_ID := No_Process;
      --  Who it was issued to (the requester, or who narrowed it).
      Holder : Process_ID := No_Process;
      Rights : LH.Rights := LH.No_Rights;
      State  : HA.Issuance_State := HA.Unknown;
      Ended  : CuBit.Child_Exits.Report;
   end record;
   type Issuance_Array is array (Positive range <>) of Issuance;
   type Issuance_Block is access Issuance_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Issuance_Array, Issuance_Block);
   Issuances : Issuance_Block := null;
   Initial_Issuances : constant := 16;
   Issuance_Ceiling  : constant := 65_536;
   Next_Serial : Unsigned_64 := 1;

   function issuanceAt (Serial : Unsigned_64) return Natural is
   begin
      if Issuances /= null and then Serial /= 0 then
         for K in Issuances'Range loop
            if Issuances (K).Serial = Serial then
               return K;
            end if;
         end loop;
      end if;
      return 0;
   end issuanceAt;

   --  A free entry, growing the table (doubling, up to the ceiling) or
   --  dropping revoked issuances when it is full; 0 when none can be had.
   function freeIssuance return Natural is
      Grown : Issuance_Block;
   begin
      if Issuances = null then
         Issuances := new Issuance_Array (1 .. Initial_Issuances);
      end if;
      for K in Issuances'Range loop
         if Issuances (K).Serial = 0 then
            return K;
         end if;
      end loop;
      if Issuances'Length < Issuance_Ceiling then
         Grown := new Issuance_Array (1 .. Natural'Min (2 * Issuances'Length, Issuance_Ceiling));
         Grown (Issuances'Range) := Issuances.all;
         Free (Issuances);
         Issuances := Grown;
         return Issuances'Length / 2 + 1;
      end if;
      for K in Issuances'Range loop
         if Issuances (K).State = HA.Revoked then
            Issuances (K) := (others => <>);
         end if;
      end loop;
      for K in Issuances'Range loop
         if Issuances (K).Serial = 0 then
            return K;
         end if;
      end loop;
      return 0;
   end freeIssuance;

   --  A new issuance for Child; Serial 0 when the ceiling is reached or
   --  the serials are spent (never reused, never wrapped).
   procedure issue
     (Child, Holder : Process_ID; Rights : LH.Rights; Parent : Unsigned_64;
      State : HA.Issuance_State; Ended : CuBit.Child_Exits.Report; Serial : out Unsigned_64)
   is
      K : Natural;
   begin
      Serial := 0;
      if Next_Serial > LH.Serial'Last then
         return;
      end if;
      K := freeIssuance;
      if K = 0 then
         return;
      end if;
      Serial := Next_Serial;
      Next_Serial := Next_Serial + 1;
      Issuances (K) := (Serial => Serial, Parent => Parent, Child => Child, Holder => Holder,
                        Rights => Rights, State => State, Ended => Ended);
   end issue;

   procedure forgetIssuance (Serial : Unsigned_64) is
      K : constant Natural := issuanceAt (Serial);
   begin
      if K /= 0 then
         Issuances (K) := (others => <>);
      end if;
   end forgetIssuance;

   --  Serial descends from Ancestor (or is it) through Parent links.
   function descends (Serial, Ancestor : Unsigned_64) return Boolean is
      Current : Unsigned_64 := Serial;
      K : Natural;
   begin
      --  Parents are always older (smaller serials), so this ends.
      while Current /= 0 loop
         if Current = Ancestor then
            return True;
         end if;
         K := issuanceAt (Current);
         exit when K = 0 or else Issuances (K).Parent >= Current;
         Current := Issuances (K).Parent;
      end loop;
      return False;
   end descends;

   ---------------------------------------------------------------------------
   --  Debug sessions procmgr minted (DBG-001): the published state the
   --  Desktop reads to show "being debugged by" (Debug_Records_Operation),
   --  and what revoking a launch handle ends. procmgr keeps its own copy of
   --  each session to see whether it still lives (a detached or ended
   --  session answers Revoked) and revokes through the target's Control.
   ---------------------------------------------------------------------------
   type Debug_Record is record
      Live         : Boolean := False;
      Target       : Process_ID := No_Process;
      Holder       : Process_ID := No_Process;
      Kind         : Debug_ABI.Debug_Kind := Debug_ABI.Observe;
      Path         : LH.Debug_Path := LH.Session_Authority_Path;
      --  The launch handle it was minted through (0: the session's debug
      --  authority).
      Via          : Unsigned_64 := 0;
      Session_Word : Unsigned_64 := 0;
      Own          : HD.Session;
   end record;
   type Debug_Record_Array is array (Positive range <>) of Debug_Record;
   type Debug_Record_Block is access Debug_Record_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Debug_Record_Array, Debug_Record_Block);
   Debug_Records : Debug_Record_Block := null;
   Initial_Debug_Records : constant := 8;
   Debug_Record_Ceiling  : constant := 4_096;

   procedure endDebugRecord (K : Positive; Revoke : Boolean) is
      R : Debug_Record renames Debug_Records (K);
      Known : constant Records.Index := Records.Find (R.Target);
      Ignore_Word : Unsigned_64;
      Ignore : Boolean;
   begin
      if Revoke and then Known /= 0 and then Records.Values (Known).Valid
        and then Valid (Records.Values (Known).Control)
      then
         Ignore_Word := HD.Revoke (Records.Values (Known).Control, R.Session_Word);
      end if;
      if HD.Valid (R.Own) then
         Ignore := HD.Close (R.Own);
      end if;
      R := (others => <>);
   end endDebugRecord;

   --  Forget sessions that ended by themselves (Detach, the viewer's end).
   procedure pruneDebugRecords is
   begin
      if Debug_Records = null then
         return;
      end if;
      for K in Debug_Records'Range loop
         if Debug_Records (K).Live
           and then not HD.Outcome_Is (HD.Call (Debug_Records (K).Own, Debug_ABI.Describe),
                                       Debug_ABI.Done)
         then
            Warnings.Inform ("procmgr: debug: session on pid" & Image (Debug_Records (K).Target) &
                             " for pid" & Image (Debug_Records (K).Holder) & " has ended");
            endDebugRecord (K, Revoke => False);
         end if;
      end loop;
   end pruneDebugRecords;

   procedure keepDebugRecord (Item : Debug_Record; Kept : out Boolean) is
      Grown : Debug_Record_Block;
   begin
      Kept := False;
      if Debug_Records = null then
         Debug_Records := new Debug_Record_Array (1 .. Initial_Debug_Records);
      end if;
      for Pass in 1 .. 2 loop
         for K in Debug_Records'Range loop
            if not Debug_Records (K).Live then
               Debug_Records (K) := Item;
               Kept := True;
               return;
            end if;
         end loop;
         exit when Debug_Records'Length >= Debug_Record_Ceiling;
         Grown := new Debug_Record_Array
           (1 .. Natural'Min (2 * Debug_Records'Length, Debug_Record_Ceiling));
         Grown (Debug_Records'Range) := Debug_Records.all;
         Free (Debug_Records);
         Debug_Records := Grown;
      end loop;
   end keepDebugRecord;

   --  The answer to a handle or session-debug request: words (0) the
   --  outcome, the rest as the operation says.
   function handleAnswer
     (What : LH.Outcome; W1, W2, W3 : Unsigned_64 := 0; Length : Unsigned_8 := 1) return Message
   is
      Answer : Message := NULL_MESSAGE;
   begin
      Answer.tag := (label => REPLY_OK, length => Length, protocol => 0, ipc => <>, reserved => 0);
      Answer.words := [LH.Outcome_Word (What), W1, W2, W3];
      return Answer;
   end handleAnswer;

   procedure answerHandle
     (sender : Process_ID; What : LH.Outcome; W1, W2, W3 : Unsigned_64 := 0;
      Length : Unsigned_8 := 1)
   is
      Ignore : Unsigned_64;
   begin
      Ignore := reply (sender, handleAnswer (What, W1, W2, W3, Length));
   end answerHandle;

   --  What the target is, for the debug envelope. The trusted path, the
   --  secrets store and devmgr by the roles only they may register (the
   --  same identities registrationAllowed admits); a program launched into
   --  the session by its record; anything else procmgr started is a
   --  system service.
   function targetClass (Target : Process_ID; Known : Records.Index) return HA.Target_Class is
      function Is_Identity (Name : String) return Boolean is
        (Known /= 0 and then Records.Values (Known).Valid
         and then Records.Values (Known).Identity (1 .. Records.Values (Known).Identity_Length) = Name);
   begin
      if Target = No_Process or else Target = ownIdentity then
         return HA.Process_Manager;
      elsif Target = Registered_Driver (DRIVER_DEVMGR) then
         return HA.Device_Manager;
      elsif Target = Registrants (CuBit.Secrets_Protocol.Service_Role)
        or else Is_Identity ("com.cubit.secrets")
      then
         return HA.Secrets_Service;
      elsif Target = Registrants (DRIVER_DESKTOP) or else Is_Identity ("com.cubit.desktop") then
         return HA.Trusted_Path;
      elsif Known /= 0 and then Records.Values (Known).Valid
        and then Records.Values (Known).Session_Child
      then
         return HA.Session_Member;
      end if;
      return HA.System_Service;
   end targetClass;

   function debugKindName (Kind : Debug_ABI.Debug_Kind) return String is
     (case Kind is
        when Debug_ABI.Observe => "Debug_Observe", when Debug_ABI.Pause => "Debug_Pause",
        when Debug_ABI.Modify => "Debug_Modify", when Debug_ABI.Instrument => "Debug_Instrument");

   function debugPathName (Path : LH.Debug_Path) return String is
     (if Path = LH.Launch_Handle_Path then "through its launch handle"
      else "through the session's debug authority");

   --  Mint a debug session of the kind msg asks (Kind_Word) on Target for
   --  sender, if the session's debug envelope allows it, and hand it over
   --  in the reply (the request must accept a capability). Every mint and
   --  every refusal is logged (logstore, through Warnings).
   procedure mintDebug
     (sender : Process_ID; Target : Process_ID; Kind_Word : Unsigned_64;
      Path : LH.Debug_Path; Via : Unsigned_64)
   is
      Known : constant Records.Index := (if Target = No_Process then 0 else Records.Find (Target));
      Facts : HA.Debug_Facts;
      Decision : LH.Outcome;
   begin
      if not Debug_ABI.Is_Kind_Word (Kind_Word) then
         answerHandle (sender, LH.Malformed);
         return;
      end if;
      Facts := (Path => Path, Class => targetClass (Target, Known),
                Held_Control => Known /= 0 and then Records.Values (Known).Valid
                                and then Records.Values (Known).Resumed
                                and then Valid (Records.Values (Known).Control),
                Kind => Debug_ABI.To_Kind (Kind_Word));
      Decision := HA.Debug_Decision (Facts, HA.User_Session_Debug, Debugging_Gate);
      if Decision /= LH.Done then
         Warnings.Warn ("procmgr: debug: " & debugKindName (Facts.Kind) & " on " &
                        processName (Target) & " pid" & Image (Target) & " refused for " &
                        processName (sender) & " pid" & Image (sender) & " " &
                        debugPathName (Path) & ": " &
                        (case Decision is
                           when LH.Approval_Needed =>
                             "production debugging needs owner approval (not built yet)",
                           when LH.Debugging_Disabled => "debugging is disabled for this image",
                           when others => LH.Outcome'Image (Decision)));
         answerHandle (sender, Decision);
         return;
      end if;
      pruneDebugRecords;
      declare
         Minted : constant HD.Mint_Result :=
           HD.Mint (Records.Values (Known).Control, Facts.Kind);
         Kept : Boolean;
         Ignore_Word : Unsigned_64;
         Ignore : Boolean;
         Result : HIPC.Reply_Result;
      begin
         if not Minted.Minted then
            Warnings.Warn ("procmgr: debug: the kernel refused " & debugKindName (Facts.Kind) &
                           " on " & processName (Target) & " pid" & Image (Target) &
                           " (outcome" & Unsigned_64'Image (Minted.Why_Word) & ")");
            answerHandle (sender, LH.Failed, Minted.Why_Word, Length => 2);
            return;
         end if;
         keepDebugRecord
           ((Live => True, Target => Target, Holder => sender, Kind => Facts.Kind, Path => Path,
             Via => Via, Session_Word => Minted.Session_Word, Own => Minted.Handle), Kept);
         if not Kept then
            Ignore_Word := HD.Revoke (Records.Values (Known).Control, Minted.Session_Word);
            Ignore := HD.Close (Minted.Handle);
            answerHandle (sender, LH.Capacity);
            return;
         end if;
         --  Read only, no Grant: the holder may not pass it on.
         Result := HIPC.Reply_With
           (sender, handleAnswer (LH.Done, To_Word (Target), Debug_ABI.Kind_Word (Facts.Kind),
                                  Length => 3),
            HD.Copy (Minted.Handle, Facts.Kind, (Read => True, others => False)));
         if Result.Outcome = HIPC.Transferred then
            Warnings.Inform ("procmgr: debug: " & debugKindName (Facts.Kind) & " on " &
                             processName (Target) & " pid" & Image (Target) & " minted for " &
                             processName (sender) & " pid" & Image (sender) & " " &
                             debugPathName (Path) & "; " & processName (Target) &
                             " is being debugged by " & processName (sender));
            return;
         end if;
         --  Not taken: end the session at once.
         for K in Debug_Records'Range loop
            if Debug_Records (K).Live and then Debug_Records (K).Session_Word = Minted.Session_Word
              and then Debug_Records (K).Target = Target
            then
               endDebugRecord (K, Revoke => True);
            end if;
         end loop;
         if Result.Outcome = HIPC.Refused then
            answerHandle (sender, LH.Not_Accepted);
         end if;
         Warnings.Warn ("procmgr: debug: " & processName (sender) & " pid" & Image (sender) &
                        " did not take the session on " & processName (Target));
      end;
   end mintDebug;

   --  Revoke an issuance and every issuance narrowed from it, and end the
   --  debug sessions minted through any of them.
   procedure revokeIssuance (Serial : Unsigned_64) is
   begin
      for K in Issuances'Range loop
         if Issuances (K).Serial /= 0 and then descends (Issuances (K).Serial, Serial) then
            if Debug_Records /= null then
               for D in Debug_Records'Range loop
                  if Debug_Records (D).Live and then Debug_Records (D).Via = Issuances (K).Serial then
                     endDebugRecord (D, Revoke => True);
                  end if;
               end loop;
            end if;
            Issuances (K).State := HA.Revoked;
         end if;
      end loop;
      --  An ended child's revoked issuances have nothing left to answer.
      for K in Issuances'Range loop
         if Issuances (K).State = HA.Revoked
           and then not processListed (Issuances (K).Child)
         then
            Issuances (K) := (others => <>);
         end if;
      end loop;
   end revokeIssuance;

   --  The child's state for Observe: (state, exit code).
   procedure observed (Item : Issuance; State, Code : out Unsigned_64) is
   begin
      if Item.State = HA.Child_Gone then
         State := LH.State_Word
           (if Item.Ended.Kind = CuBit.Child_Exits.Exited then LH.Exited else LH.Stopped);
         Code := Item.Ended.Code;
      else
         State := LH.State_Word (LH.Running);
         Code := 0;
      end if;
   end observed;

   --  A request through a launch handle (Handle_Admission.Route_Of said so).
   procedure handleHandleRequest (sender : Process_ID; msg : Message) is
      Serial : constant Unsigned_64 := LH.Serial_Of (msg.authorityTag);
      At_Index : constant Natural := issuanceAt (Serial);
      Op : constant LH.Operation := LH.To_Operation (msg.tag.label);
      Facts : constant HA.Facts :=
        (if At_Index = 0 then (State => HA.Unknown, Rights => LH.No_Rights)
         else (State => Issuances (At_Index).State, Rights => Issuances (At_Index).Rights));
      Decision : constant LH.Outcome := HA.Decide (Facts, Op, msg.tag.length);
   begin
      if Decision /= LH.Done then
         answerHandle (sender, Decision);
         return;
      end if;
      declare
         Item : constant Issuance := Issuances (At_Index);
         Known : constant Records.Index := Records.Find (Item.Child);
         Has_Control : constant Boolean :=
           Known /= 0 and then Records.Values (Known).Valid and then Valid (Records.Values (Known).Control);
      begin
         case Op is
            when LH.Observe_Op =>
               declare
                  State, Code : Unsigned_64;
               begin
                  observed (Item, State, Code);
                  answerHandle (sender, LH.Done, To_Word (Item.Child), State, Code, Length => 4);
               end;
            when LH.Control_Op =>
               if not LH.Is_Control_Word (msg.words (0)) then
                  answerHandle (sender, LH.Malformed);
               elsif not Has_Control then
                  answerHandle (sender, LH.Failed);
               elsif LH.To_Control (msg.words (0)) = LH.End_Now then
                  Warnings.Inform ("procmgr: ending " & processName (Item.Child) & " pid" &
                                   Image (Item.Child) & " for its launch handle's holder " &
                                   processName (sender) & " pid" & Image (sender));
                  answerHandle
                    (sender, (if killProcess (Item.Child, Records.Values (Known).Control) = 0
                              then LH.Done else LH.Failed));
               else
                  declare
                     Kind : constant CuBit.Control_Events.Control_Kind :=
                       (case LH.To_Control (msg.words (0)) is
                          when LH.Ask_Stop      => CuBit.Control_Events.Stop,
                          when LH.Ask_Interrupt => CuBit.Control_Events.Interrupt,
                          when others           => CuBit.Control_Events.Reload);
                     Answer : constant Unsigned_64 := HC.Send_Control
                       (Item.Child, CuBit.Control_Events.Control_Kind'Enum_Rep (Kind),
                        Records.Values (Known).Control);
                  begin
                     answerHandle (sender, (if Answer = 0 then LH.Done
                                            elsif Answer = HC.Busy_Word then LH.Busy
                                            else LH.Failed));
                  end;
               end if;
            when LH.Attach_Op =>
               --  Post-hoc outlet attach (7.5 item 7): the right and the
               --  request exist; connecting arrives with the outlet ledger.
               answerHandle (sender, LH.Not_Yet);
            when LH.Introduce_Op =>
               --  A late introduction from the holder's own holdings
               --  through the ledger: procmgr does not yet keep a running
               --  child's manifest entries to check the index against.
               answerHandle (sender, LH.Not_Yet);
            when LH.Debug_Op =>
               mintDebug (sender, Item.Child, msg.words (0), LH.Launch_Handle_Path, Serial);
            when LH.Narrow_Op =>
               if not LH.Valid_Rights_Word (msg.words (0)) then
                  answerHandle (sender, LH.Malformed);
                  return;
               end if;
               declare
                  Rights : constant LH.Rights :=
                    HA.Narrowed (Item.Rights, LH.Rights_Of (msg.words (0)));
                  New_Serial : Unsigned_64;
                  Result : HIPC.Reply_Result;
               begin
                  issue (Item.Child, sender, Rights, Serial, Item.State, Item.Ended, New_Serial);
                  if New_Serial = 0 then
                     answerHandle (sender, LH.Capacity);
                     return;
                  end if;
                  Result := HIPC.Reply_With
                    (sender, handleAnswer (LH.Done, LH.Word_Of (Rights), To_Word (Item.Child),
                                           Length => 3),
                     Badged (PH.Self_Grantable, Read_Write_Grant, LH.Tag_Of (New_Serial)));
                  if Result.Outcome /= HIPC.Transferred then
                     forgetIssuance (New_Serial);
                  end if;
                  if Result.Outcome = HIPC.Refused then
                     answerHandle (sender, LH.Not_Accepted);
                  end if;
               end;
            when LH.Revoke_Op =>
               revokeIssuance (Serial);
               answerHandle (sender, LH.Done);
         end case;
      end;
   end handleHandleRequest;

   --  Session_Debug_Operation through the session's debug badge: words (0)
   --  the target's identity, (1) a debug kind word.
   procedure handleSessionDebug (sender : Process_ID; msg : Message) is
   begin
      if msg.tag.length /= LH.Session_Debug_Words then
         answerHandle (sender, LH.Malformed);
         return;
      end if;
      mintDebug (sender, From_Word (msg.words (0)), msg.words (1), LH.Session_Authority_Path, 0);
   end handleSessionDebug;

   --  Debug_Records_Operation (a process observer): record words (0).
   procedure handleDebugRecords (sender : Process_ID; msg : Message) is
      Wanted : Unsigned_64;
      Seen : Unsigned_64 := 0;
   begin
      if not PO.Is_Observer (msg.authorityTag) then
         answerHandle (sender, LH.Right_Missing);
         return;
      elsif msg.tag.length /= LH.Debug_Records_Words then
         answerHandle (sender, LH.Malformed);
         return;
      end if;
      Wanted := msg.words (0);
      pruneDebugRecords;
      if Debug_Records /= null then
         for K in Debug_Records'Range loop
            if Debug_Records (K).Live then
               if Seen = Wanted then
                  declare
                     More : Boolean := False;
                  begin
                     for J in K + 1 .. Debug_Records'Last loop
                        More := More or else Debug_Records (J).Live;
                     end loop;
                     answerHandle
                       (sender, LH.Done, To_Word (Debug_Records (K).Target),
                        To_Word (Debug_Records (K).Holder),
                        LH.Record_Word (Debug_ABI.Kind_Word (Debug_Records (K).Kind),
                                        Debug_Records (K).Path, More),
                        Length => 4);
                     return;
                  end;
               end if;
               Seen := Seen + 1;
            end if;
         end loop;
      end if;
      --  Past the last record: no target.
      answerHandle (sender, LH.Done, 0, 0, 0, Length => 4);
   end handleDebugRecords;

   ---------------------------------------------------------------------------
   --  Exit notices: a child launched into the session has procmgr as its
   --  kernel parent, so procmgr tells its requester it ended
   --  (LH.Exit_Notice_Label through procmgr's Notice_Tag badge on the
   --  requester's mailbox). An event the requester's lane cannot take yet
   --  waits here and is sent again every Notice_Retry_Ms while the
   --  requester lives: none is dropped (docs/ipc-delivery.md).
   ---------------------------------------------------------------------------
   Notice_Retry_Ms : constant := 20;
   type Pending_Notice;
   type Pending_Notice_Access is access Pending_Notice;
   type Pending_Notice is record
      Requester : Process_ID := No_Process;
      Report    : CuBit.Child_Exits.Report;
      Next      : Pending_Notice_Access;
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Pending_Notice, Pending_Notice_Access);
   Notices_First : Pending_Notice_Access := null;
   Notices_Due : Unsigned_64 := 0;

   --  True when sent, or when there is no one to tell any more.
   function sendNotice (Requester : Process_ID; Report : CuBit.Child_Exits.Report) return Boolean is
      Mailbox : constant CuBit.Handles.Endpoint := mailboxOf (Requester);
      Badge : CuBit.Handles.Endpoint;
      Notice : Message := NULL_MESSAGE;
      Sent : Boolean;
      Ignore : HIPC.Close_Result;
   begin
      if not Valid (Mailbox) then
         return True;
      end if;
      Badge := HC.Derive (Badged (Mailbox, H.Read_Write, LH.Notice_Tag));
      if not Valid (Badge) then
         return False;
      end if;
      Notice.tag := (label => LH.Exit_Notice_Label, length => CuBit.Child_Exits.Event_Words,
                     protocol => 0, ipc => <>, reserved => 0);
      Notice.words := [To_Word (Report.Process),
                       Unsigned_64 (CuBit.Child_Exits.Termination_Kind'Enum_Rep (Report.Kind)),
                       Report.Code, 0];
      Sent := trySendEvent (Requester, Notice, Badge);
      Ignore := HIPC.Close (Badge);
      return Sent;
   end sendNotice;

   procedure queueNotice (Requester : Process_ID; Report : CuBit.Child_Exits.Report) is
      Last : Pending_Notice_Access := Notices_First;
      Item : constant Pending_Notice_Access :=
        new Pending_Notice'(Requester => Requester, Report => Report, Next => null);
   begin
      if Last = null then
         Notices_First := Item;
      else
         while Last.Next /= null loop
            Last := Last.Next;
         end loop;
         Last.Next := Item;
      end if;
      Notices_Due := CuBit.Monotonic.Milliseconds + Notice_Retry_Ms;
   end queueNotice;

   --  Send the waiting notices again, oldest first, each requester's in
   --  order: one that still cannot go holds its requester's later ones.
   procedure retryNotices is
      Item : Pending_Notice_Access := Notices_First;
      Prior : Pending_Notice_Access := null;
      Next : Pending_Notice_Access;

      --  An older notice to the same requester is still waiting.
      function Held_Back (Requester : Process_ID) return Boolean is
         Scan : Pending_Notice_Access := Notices_First;
      begin
         while Scan /= null and then Scan /= Item loop
            if Scan.Requester = Requester then
               return True;
            end if;
            Scan := Scan.Next;
         end loop;
         return False;
      end Held_Back;
   begin
      if Notices_First = null or else CuBit.Monotonic.Milliseconds < Notices_Due then
         return;
      end if;
      while Item /= null loop
         Next := Item.Next;
         if not Held_Back (Item.Requester) and then sendNotice (Item.Requester, Item.Report) then
            if Prior = null then
               Notices_First := Next;
            else
               Prior.Next := Next;
            end if;
            Free (Item);
         else
            Prior := Item;
         end if;
         Item := Next;
      end loop;
      Notices_Due := (if Notices_First = null then 0
                      else CuBit.Monotonic.Milliseconds + Notice_Retry_Ms);
   end retryNotices;

   function noticeWaiting (Requester : Process_ID) return Boolean is
      Item : Pending_Notice_Access := Notices_First;
   begin
      while Item /= null loop
         if Item.Requester = Requester then
            return True;
         end if;
         Item := Item.Next;
      end loop;
      return False;
   end noticeWaiting;

   --  A process ended: its launch handles answer its exit from now on, its
   --  requester hears of it, and what it held or was told goes.
   procedure launchHandlesExited (Exited : Process_ID; Known : Records.Index;
                                  Report : CuBit.Child_Exits.Report) is
      Item, Prior, Next : Pending_Notice_Access;
   begin
      if Known /= 0 and then Records.Values (Known).Valid
        and then Records.Values (Known).Session_Child
        and then Records.Values (Known).Launcher /= No_Process
      then
         declare
            Requester : constant Process_ID := Records.Values (Known).Launcher;
         begin
            if noticeWaiting (Requester) or else not sendNotice (Requester, Report) then
               queueNotice (Requester, Report);
            end if;
         end;
      end if;
      --  Notices for it as a requester have no one to go to.
      Item := Notices_First;
      Prior := null;
      while Item /= null loop
         Next := Item.Next;
         if Item.Requester = Exited then
            if Prior = null then
               Notices_First := Next;
            else
               Prior.Next := Next;
            end if;
            Free (Item);
         else
            Prior := Item;
         end if;
         Item := Next;
      end loop;
      if Issuances /= null then
         for K in Issuances'Range loop
            if Issuances (K).Serial /= 0 and then Issuances (K).Child = Exited then
               if Issuances (K).State = HA.Revoked
                 or else (Issuances (K).Holder /= No_Process
                          and then not processListed (Issuances (K).Holder))
               then
                  Issuances (K) := (others => <>);
               else
                  Issuances (K).State := HA.Child_Gone;
                  Issuances (K).Ended := Report;
               end if;
            elsif Issuances (K).Serial /= 0 and then Issuances (K).Holder = Exited
              and then Issuances (K).State /= HA.Live
            then
               Issuances (K) := (others => <>);
            end if;
         end loop;
      end if;
      if Debug_Records /= null then
         for K in Debug_Records'Range loop
            if Debug_Records (K).Live
              and then (Debug_Records (K).Target = Exited or else Debug_Records (K).Holder = Exited)
            then
               Warnings.Inform ("procmgr: debug: session on " & processName (Debug_Records (K).Target) &
                                " pid" & Image (Debug_Records (K).Target) & " ended with pid" &
                                Image (Exited));
               endDebugRecord (K, Revoke => False);
            end if;
         end loop;
      end if;
   end launchHandlesExited;

   --  Answer a launch into the session: the child's identity, the rights
   --  of its launch handle, and the handle itself when the request accepts
   --  a capability. words (0) = the child's identity, (1) = the handle's
   --  rights word, (2) = an LH outcome word for the handle (Done: carried;
   --  Not_Accepted: the request took none; Capacity: none could be issued).
   procedure answerLaunched (dest : Process_ID; Child : Process_ID; Badge : Session_Launch.Badge) is
      Rights : constant LH.Rights := HA.Issued (Badge);
      Known : constant Records.Index := Records.Find (Child);
      Serial : Unsigned_64;
      Answer : Message := NULL_MESSAGE;
      Result : HIPC.Reply_Result;
      Ignore : Boolean;
      Ignore_Sent : Unsigned_64;
   begin
      if Known /= 0 then
         Records.Values (Known).Session_Child := True;
      end if;
      issue (Child, dest, Rights, 0, HA.Live, (others => <>), Serial);
      Answer.tag := (label => REPLY_OK, length => LH.Launched_Reply_Words, protocol => 0, ipc => <>,
                     reserved => 0);
      Answer.words := [To_Word (Child), LH.Word_Of (Rights),
                       LH.Outcome_Word (if Serial = 0 then LH.Capacity else LH.Done), 0];
      if Serial = 0 then
         Warnings.Warn ("procmgr: launch handle for pid" & Image (Child) &
                        " not issued: the handle table is at its ceiling");
         if servingDeferred then
            Ignore := HIPC.Answer (Deferred_Answer, Answer);
            Deferred_Answer := H.No_Reply;
         else
            Ignore_Sent := reply (dest, Answer);
         end if;
         return;
      end if;
      if servingDeferred then
         Result := HIPC.Reply_Saved_With
           (Deferred_Answer, Answer, Badged (PH.Self_Grantable, Read_Write_Grant, LH.Tag_Of (Serial)));
      else
         Result := HIPC.Reply_With
           (dest, Answer, Badged (PH.Self_Grantable, Read_Write_Grant, LH.Tag_Of (Serial)));
      end if;
      if Result.Outcome = HIPC.Transferred then
         Deferred_Answer := H.No_Reply;
         return;
      end if;
      --  Nobody holds it.
      forgetIssuance (Serial);
      if Result.Outcome = HIPC.Refused then
         Answer.words (2) := LH.Outcome_Word (LH.Not_Accepted);
         if servingDeferred then
            Ignore := HIPC.Answer (Deferred_Answer, Answer);
         else
            Ignore_Sent := reply (dest, Answer);
         end if;
      end if;
      Deferred_Answer := H.No_Reply;
   end answerLaunched;


   ---------------------------------------------------------------------------
   --  handleLaunch
   --  OP_LAUNCH (CuBit.Launch_Arguments): spawn a program with launch
   --  arguments. The name and block are copied out of the requester's grant
   --  first, and the block is validated in procmgr's copy; arguments are
   --  data and never change what authority the child receives.
   ---------------------------------------------------------------------------
   procedure handleLaunch (sender : Process_ID; msg : Message) is
      package LA renames CuBit.Launch_Arguments;
      use type LA.Validation;
      Name_Bytes     : constant Unsigned_64 := msg.words (1);
      Argument_Bytes : constant Unsigned_64 := msg.words (2);
      Grant_Bytes    : constant Unsigned_64 :=
        CuBit.Launch_Grants.Request_Grant_Bytes (msg.words (3));
      Ring_Bytes     : constant Unsigned_64 :=
        CuBit.Launch_Grants.Request_Ring_Bytes (msg.words (3));
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped    : System.Address;
      Ok, Returned : Boolean;
      newPID    : Process_ID;
      Failure   : LA.Launch_Failure := LA.Spawn_Failed;
      Waits     : Awaited_Access;
      use type Session_Launch.Badge;

      procedure Fail (Reason : LA.Launch_Failure) is
      begin
         answerRequest (sender, REPLY_ERR, LA.Launch_Failure'Enum_Rep (Reason));
      end Fail;
   begin
      if msg.tag.length /= LA.Launch_Request_Words
        or else not LA.Request_Valid (Name_Bytes, Argument_Bytes)
        or else not CuBit.Launch_Grants.Request_Reserved_Clear (msg.words (3))
        or else not CuBit.Launch_Grants.Grant_Bytes_Valid (Grant_Bytes)
        or else Ring_Bytes > CuBit.Outlet_Rings.Maximum_Bytes
        or else not CuBit.Grant_References.Valid_Wire (msg.words (0))
      then
         Fail (LA.Malformed_Request);
         return;
      end if;
      if launchIndex (sender) = 0 then
         Fail (LA.Malformed_Request);
         return;
      end if;
      --  Launch-into-S (CAP-012, Session_Launch): only through a Launch
      --  badge on procmgr's endpoint. Holding procmgr's endpoint is not
      --  launch authority.
      if Session_Launch.Badge_Of (msg.authorityTag) = Session_Launch.No_Badge then
         Warnings.Warn ("procmgr: launch refused: the request came without a Launch badge");
         Fail (LA.Not_Granted);
         return;
      elsif Session_Launch.Needs_Device_Confirmation (Session_Launch.Badge_Of (msg.authorityTag)) then
         --  The typed hook for confirmation on the device: none needs it yet.
         Fail (LA.Not_Granted);
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (Reference, sender, 0,
         Name_Bytes + Argument_Bytes + Grant_Bytes + Ring_Bytes,
         CuBit.Memory_Grants.Read_Access, Mapped, Ok);
      if not Ok then
         Fail (LA.Grant_Unavailable);
         return;
      end if;

      declare
         Name_Length : constant LA.Name_Length := LA.Name_Length (Name_Bytes);
         Arguments_Length : constant LA.Block_Length :=
           LA.Block_Length (Argument_Bytes);
         Source_Name : constant String (1 .. Name_Length)
           with Import, Address => Mapped;
         Name : constant String (1 .. Name_Length) := Source_Name;
      begin
         if Arguments_Length > 0 then
            declare
               Source_Block : constant LA.Block (1 .. Arguments_Length)
                 with Import,
                      Address => Mapped + Storage_Offset (Name_Length);
            begin
               launchBlock (1 .. Arguments_Length) := Source_Block;
            end;
         end if;
         pendingGrantBytes := 0;
         if Grant_Bytes > 0 then
            declare
               Source_Grants : constant CuBit.Launch_Grants.Bytes
                 (1 .. Natural (Grant_Bytes))
                 with Import,
                      Address => Mapped + Storage_Offset (Name_Length) +
                                 Storage_Offset (Arguments_Length);
            begin
               pendingGrants (1 .. Natural (Grant_Bytes)) := Source_Grants;
            end;
         end if;
         pendingRings := (others => <>);
         if Ring_Bytes > 0 then
            declare
               Source_Rings : constant CuBit.Outlet_Rings.Bytes (1 .. Natural (Ring_Bytes))
                 with Import,
                      Address => Mapped + Storage_Offset (Name_Length) +
                                 Storage_Offset (Arguments_Length) +
                                 Storage_Offset (Grant_Bytes);
               Copy : constant CuBit.Outlet_Rings.Bytes (1 .. Natural (Ring_Bytes)) := Source_Rings;
               Rings_Valid : Boolean;
            begin
               CuBit.Outlet_Rings.Decode (Copy, pendingRings, Rings_Valid);
               if not Rings_Valid then
                  CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
                  debugPrint ("procmgr: port rings rejected" & LF);
                  Fail (LA.Arguments_Rejected);
                  return;
               end if;
            end;
         end if;
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         if not Returned then
            debugPrint ("procmgr: launch grant return failed" & LF);
         end if;
         if Grant_Bytes > 0 then
            if not CuBit.Launch_Grants.Valid
              (pendingGrants (1 .. Natural (Grant_Bytes)))
            then
               debugPrint ("procmgr: delegated places rejected" & LF);
               Fail (LA.Arguments_Rejected);
               return;
            end if;
            pendingGrantBytes := Natural (Grant_Bytes);
         end if;
         if Arguments_Length > 0
           and then LA.Validate (launchBlock (1 .. Arguments_Length)) /= LA.Valid
         then
            debugPrint ("procmgr: launch arguments rejected" & LF);
            Fail (LA.Arguments_Rejected);
            return;
         end if;

         --  Only programs the requester's manifest names, by exact name:
         --  no search path. Refused before anything is read or runs.
         declare
            Launcher : Launch_State renames Launches.Values (launchIndex (sender));
            Admission : constant Launch_Admission := admitLaunch (Launcher, Name);
         begin
            if Admission = Staged_Switch_Off then
               Warnings.Warn ("procmgr: " & CuBit.Failures.Explain
                 ("Starting the staged program " & Name,
                  CuBit.Failures.Failed
                    (CuBit.Failures.Not_Granted,
                     "the Desktop's Allow app replacement switch is off",
                     "turn on Allow app replacement in the Desktop's status panel")));
               Fail (LA.Not_Granted);
               return;
            elsif Admission = Staged then
               Warnings.Inform ("procmgr: staged program " & Name &
                                " started by its launcher's staging directory;" &
                                " not signature-checked (signed admission is not built)");
            elsif Admission = Not_Admitted then
               debugPrint ("procmgr: " & CuBit.Failures.Explain
                 ("Starting " & Name,
                  CuBit.Failures.Failed
                    (CuBit.Failures.Not_Granted,
                     "the launcher's manifest does not name it",
                     "add (may-launch " & '"' & Name & '"' &
                     ") to the launcher's manifest")) & LF);
               Fail (LA.Not_Granted);
               return;
            end if;
         end;

         --  The child holds a subset of its launcher's authority: each
         --  request is checked against the user session's envelope, rights
         --  included (Session_Launch); what the requester introduces
         --  (delegated places, rings, its Launch badge) against what it
         --  holds (attenuateFor).
         pendingArgumentBytes := Arguments_Length;
         attenuateFor := sender;
         launchSession := True;
         withheldLength := 0;
         attenuationRefused := False;
         directoryRefused := False;
         newPID := spawnByName
           (Name, sender,
            approveRender => Session_Launch.Within
              (Session_Launch.User_Session, Session_Launch.Render, 0),
            readyDeadlineMs => Unplanned_Ready_Deadline,
            mayWait => not servingDeferred, Waits => Waits, Failure => Failure);
         pendingArgumentBytes := 0;
         pendingGrantBytes := 0;
         pendingRings := (others => <>);
         attenuateFor := No_Process;
         launchSession := False;
         --  Waiting for a starting provider: served again from this
         --  request once it registers (resumeParked).
         if Waits /= null then
            if parkRequest (Launch_Request, sender, msg, Name, Waits) then
               return;
            end if;
            Failure := LA.Service_Unavailable;
         end if;
      end;

      if newPID = No_Process and then directoryRefused then
         debugPrint ("procmgr: " & CuBit.Failures.Explain
           ("Starting a program",
            CuBit.Failures.Failed
              (CuBit.Failures.Not_Granted,
               "its working directory is outside what it may read",
               "start it in a directory its manifest grants, or grant " &
               "that directory")) & LF);
         directoryRefused := False;
         Fail (LA.Not_Granted);
      elsif newPID = No_Process and then attenuationRefused and then withheldLength = 0 then
         --  A delegated place the requester does not hold.
         debugPrint ("procmgr: " & CuBit.Failures.Explain
           ("Starting a program",
            CuBit.Failures.Failed
              (CuBit.Failures.Not_Granted,
               "its launcher delegates a place it does not hold",
               "delegate only places the launcher holds")) & LF);
         attenuationRefused := False;
         Fail (LA.Not_Granted);
      elsif newPID = No_Process and then attenuationRefused then
         --  A required request the session withholds, named (Request_Withheld).
         attenuationRefused := False;
         declare
            Words : LA.Label_Words;
         begin
            LA.Encode_Label (withheldLabel (1 .. withheldLength), Words);
            debugPrint ("procmgr: " & CuBit.Failures.Explain
              ("Starting a program",
               CuBit.Failures.Failed
                 (CuBit.Failures.Not_Granted,
                  "it requires """ & withheldLabel (1 .. withheldLength) &
                  """, which its session withholds",
                  "make the request optional, or start it where it is approved")) & LF);
            answerRequest (sender, REPLY_ERR, LA.Launch_Failure'Enum_Rep (LA.Request_Withheld),
                           Words, Length => 4);
         end;
      elsif newPID = No_Process then
         Fail (Failure);
      else
         --  The child's identity (KERN-003): its exit notice names it
         --  alike; and its launch handle (CuBit.Launch_Handles).
         answerLaunched (sender, newPID, Session_Launch.Badge_Of (msg.authorityTag));
      end if;
   end handleLaunch;

   ---------------------------------------------------------------------------
   --  handleLaunchTable
   --  OP_LAUNCH_TABLE (CuBit.Launch_Authority): the requester's own launch
   --  table, written into the grant it lends.
   ---------------------------------------------------------------------------
   procedure handleLaunchTable (sender : Process_ID; msg : Message) is
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped    : System.Address;
      Ok, Returned : Boolean;
   begin
      if msg.tag.length /= LAuth.Table_Request_Words
        or else not CuBit.Grant_References.Valid_Wire (msg.words (0))
        or else launchIndex (sender) = 0
      then
         sendReply (sender, REPLY_ERR, CuBit.Launch_Arguments.Launch_Failure'Enum_Rep
                      (CuBit.Launch_Arguments.Malformed_Request));
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (Reference, sender, 0, LAuth.Maximum_Table_Bytes,
         CuBit.Memory_Grants.Write_Access, Mapped, Ok);
      if not Ok then
         sendReply (sender, REPLY_ERR, CuBit.Launch_Arguments.Launch_Failure'Enum_Rep
                      (CuBit.Launch_Arguments.Grant_Unavailable));
         return;
      end if;
      declare
         Launcher : Launch_State renames Launches.Values (launchIndex (sender));
         Target : LAuth.Table_Bytes (1 .. LAuth.Maximum_Table_Bytes)
           with Import, Address => Mapped;
      begin
         Target (1 .. Launcher.Table_Length) := Launcher.Table (1 .. Launcher.Table_Length);
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         sendReply (sender, REPLY_OK, Unsigned_64 (Launcher.Table_Length));
      end;
   end handleLaunchTable;

   ---------------------------------------------------------------------------
   --  handleDelegatedPlaces
   --  OP_DELEGATED_PLACES (CuBit.Launch_Grants): the places the requester's
   --  launcher delegated to it, as a region written into the grant it lends.
   ---------------------------------------------------------------------------
   procedure handleDelegatedPlaces (sender : Process_ID; msg : Message) is
      package LG renames CuBit.Launch_Grants;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped    : System.Address;
      Ok, Returned, Added : Boolean;
      Places : LG.Builder;
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Length : LG.Byte_Count;
   begin
      if msg.tag.length /= LG.Places_Request_Words
        or else not CuBit.Grant_References.Valid_Wire (msg.words (0))
        or else launchIndex (sender) = 0
      then
         sendReply (sender, REPLY_ERR, CuBit.Launch_Arguments.Launch_Failure'Enum_Rep
                      (CuBit.Launch_Arguments.Malformed_Request));
         return;
      end if;
      LG.Start (Places);
      declare
         Held : Launch_State renames Launches.Values (launchIndex (sender));
      begin
         for h in 1 .. Held.Scope_Count loop
            if Held.Scopes (h).Delegated and then Held.Scopes (h).Service = SERVICE_FS then
               LG.Add (Places, Held.Scopes (h).Rights,
                       Held.Scopes (h).Prefix (1 .. Held.Scopes (h).Length), Added);
               --  Each was a valid delegation when it was recorded.
               pragma Assert (Added);
            end if;
         end loop;
      end;
      LG.Finish (Places, Region, Length);
      Reference := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (Reference, sender, 0, LG.Maximum_Bytes,
         CuBit.Memory_Grants.Write_Access, Mapped, Ok);
      if not Ok then
         sendReply (sender, REPLY_ERR, CuBit.Launch_Arguments.Launch_Failure'Enum_Rep
                      (CuBit.Launch_Arguments.Grant_Unavailable));
         return;
      end if;
      declare
         Target : LG.Bytes (1 .. LG.Maximum_Bytes) with Import, Address => Mapped;
      begin
         Target (1 .. Length) := Region (1 .. Length);
      end;
      CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
      sendReply (sender, REPLY_OK, Unsigned_64 (Length));
   end handleDelegatedPlaces;

   ---------------------------------------------------------------------------
   --  handleProgramDescription
   --  OP_PROGRAM_DESCRIPTION (CuBit.Program_Descriptions): a program's
   --  parameters, for a requester that may launch it. The descriptor is
   --  validated with Decode before it is written back after the name.
   ---------------------------------------------------------------------------
   procedure handleProgramDescription (sender : Process_ID; msg : Message) is
      package LA renames CuBit.Launch_Arguments;
      package PP renames CuBit.Program_Descriptions;
      Name_Bytes : constant Unsigned_64 := msg.words (1);
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped    : System.Address;
      Ok, Returned : Boolean;
      elfSize   : Unsigned_64;
      Found_Length : PP.Descriptor_Length := 0;
      Found_At  : Unsigned_64 := 0;
      Valid     : Boolean := False;
      Rings     : Unsigned_64;

      procedure Fail (Reason : LA.Launch_Failure) is
      begin
         sendReply (sender, REPLY_ERR, LA.Launch_Failure'Enum_Rep (Reason));
      end Fail;
   begin
      if msg.tag.length /= PP.Description_Request_Words
        or else Name_Bytes not in 1 .. LA.Maximum_Name_Bytes
        or else not CuBit.Grant_References.Valid_Wire (msg.words (0))
        or else launchIndex (sender) = 0
      then
         Fail (LA.Malformed_Request);
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (Reference, sender, 0,
         Name_Bytes + PP.Maximum_Descriptor_Bytes,
         CuBit.Memory_Grants.Write_Access, Mapped, Ok);
      if not Ok then
         Fail (LA.Grant_Unavailable);
         return;
      end if;

      declare
         Name_Length : constant LA.Name_Length := LA.Name_Length (Name_Bytes);
         Source_Name : constant String (1 .. Name_Length)
           with Import, Address => Mapped;
         Name : constant String (1 .. Name_Length) := Source_Name;
         Launcher : Launch_State renames Launches.Values (launchIndex (sender));
         Reason : LA.Launch_Failure := LA.Malformed_Request;
         Answered : Boolean := False;
      begin
         --  Only programs the requester may launch: the same check as
         --  OP_LAUNCH, before anything is read.
         if admitLaunch (Launcher, Name) not in Named | Staged then
            Reason := LA.Not_Granted;
         else
            elfSize := readFileFromFS (Name, File_Request_Deadline_Ms);
            if elfSize < 64 then
               Reason := LA.Spawn_Failed;
            else
               findDescription (elfSize, Found_At, Found_Length, Valid, Rings);
               Answered := Found_Length = 0 or else Valid;
               if not Answered then
                  debugPrint ("procmgr: invalid program description" & LF);
                  Reason := LA.Arguments_Rejected;
               end if;
            end if;
         end if;

         if Answered and then Found_Length > 0 then
            declare
               Source : constant PP.Bytes (1 .. Found_Length)
                 with Import, Address => imageBuf + Storage_Offset (Found_At);
               Target : PP.Bytes (1 .. Found_Length)
                 with Import, Address => Mapped + Storage_Offset (Name_Length);
            begin
               Target := Source;
            end;
         end if;
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         if not Returned then
            debugPrint ("procmgr: description grant return failed" & LF);
         end if;
         if Answered then
            sendReply (sender, REPLY_OK, Unsigned_64 (Found_Length));
         else
            Fail (Reason);
         end if;
      end;
   end handleProgramDescription;

   ---------------------------------------------------------------------------
   --  Whether a startup entry may render on the GPU (HW-002): as its plan
   --  says, or, when the plan delegates it, (render setting "<key>"), as the
   --  GPU_Rendering setting says. Absent means enabled; one that does not
   --  decode refuses. Either way devmgr still admits the session only for a
   --  present, supported GPU (otherwise the program renders in software).
   ---------------------------------------------------------------------------
   function Render_Granted (Item : CCL.Configurations.Launch_Entry; Name : String) return Boolean is
      use CCL.Configurations;
      Key : String renames Item.Render_Setting.Data (1 .. Item.Render_Setting.Length);
      cfgMsg : Message := NULL_MESSAGE;
      Enabled, Decoded : Boolean;
   begin
      case Item.Render is
         when Render_Denied => return False;
         when Render_Declared => return True;
         when Render_By_Setting => null;
      end case;
      if not Config_Grant_Ready or else Key'Length > PAGE_SIZE then
         Warnings.Warn ("procmgr: stage render-grant fail " & Name & " setting=" & Key & " unreadable");
         return False;
      end if;
      declare
         Buffer : String (1 .. Key'Length) with Import, Address => elfBuf;
      begin
         Buffer := Key;
      end;
      cfgMsg.tag := (label => OP_CONFIG_GET, length => 4, protocol => 0, ipc => <>, reserved => 0);
      cfgMsg.words (0) := Config_Grant.slot;
      cfgMsg.words (1) := Config_Grant.generation;
      cfgMsg.words (2) := Unsigned_64 (Key'Length);
      cfgMsg.tag := capCall (PH.Config, cfgMsg, CuBit.Messages.Wait_Forever);
      if cfgMsg.tag.label /= REPLY_OK or else cfgMsg.tag.length /= 1 then
         Warnings.Inform ("procmgr: stage render-grant ok " & Name & " setting=" & Key &
                          " absent, default enabled");
         return GPU_Rendering_Settings.Default_Enabled;
      end if;
      if cfgMsg.words (0) not in 1 .. PAGE_SIZE then
         Warnings.Warn ("procmgr: stage render-grant fail " & Name & " setting=" & Key & " empty");
         return False;
      end if;
      declare
         Value : String (1 .. Natural (cfgMsg.words (0))) with Import, Address => elfBuf;
      begin
         GPU_Rendering_Settings.Decode (Value, Enabled, Decoded);
      end;
      if not Decoded then
         Warnings.Warn ("procmgr: stage render-grant fail " & Name & " setting=" & Key &
                        " is not a GPU_Rendering");
         return False;
      end if;
      if Enabled then
         Warnings.Inform ("procmgr: stage render-grant ok " & Name & " setting=" & Key & " enabled");
      else
         Warnings.Inform ("procmgr: stage render-grant fail " & Name & " setting=" & Key &
                          " disabled; software rendering");
      end if;
      return Enabled;
   end Render_Granted;

   ---------------------------------------------------------------------------
   --  processInitCCL
   --  Read init.ccl from the filesystem, compile it, and start its entries
   --  in order (continuePlan). An entry waiting for a starting provider is
   --  parked and holds the later ones back, while the service loop runs.
   ---------------------------------------------------------------------------
   --  The compiled startup plan; it owns its strings before spawnByName
   --  overwrites elfBuf.
   Startup_Plan : CCL.Configurations.Compilation_Result;
   --  How many of its entries were started or refused; the plan is running
   --  until all were.
   Plan_Done : CCL.Configurations.Launch_Number := 0;
   Plan_Running : Boolean := False;
   --  Diagnostics never depend on what they diagnose (ABI-002): once the
   --  plan has started logstore, its later entries (each a read from
   --  storage) wait until logstore registered and was offered to devmgr
   --  and the holders waiting for it (introduceRegistered), or ended, or
   --  missed its ready deadline. A stalled disk then still has its
   --  drivers' lines reach the log.
   Plan_Awaits_Logstore : Boolean := False;
   --  Supervision (Plan_Restarts, BUG-001): each plan entry's current
   --  process, and its restart state when the entry says (restart on-failure).
   subtype Plan_Entry is Positive range 1 .. CCL.Configurations.MAX_LAUNCHES;
   Plan_PIDs : array (Plan_Entry) of Process_ID := [others => No_Process];
   Plan_Runs : array (Plan_Entry) of Plan_Restarts.Run;
   use type Plan_Restarts.Phase;

   --  Service switches (CuBit.Service_Replacement, Service_Switches): the
   --  program each plan entry runs when it is not its image's (a system
   --  generation's pin, switched to while running; empty: the image's),
   --  and the one switch in progress.
   Maximum_Program_Bytes : constant := CuBit.Service_Replacement.Maximum_Program_Bytes;
   type Program_Text is record
      Length : Natural range 0 .. Maximum_Program_Bytes := 0;
      Data   : String (1 .. Maximum_Program_Bytes) := [others => ' '];
   end record;
   Plan_Programs : array (Plan_Entry) of Program_Text;
   Switch : Service_Switches.Switch;
   --  Its plan entry (0: none), the program it switches to, the program
   --  the entry ran before (for a rollback), and the requester's saved reply.
   Switch_Entry : CCL.Configurations.Launch_Number := 0;
   Switch_Program, Switch_Previous : Program_Text;
   Switch_Reply : H.Reply := H.No_Reply;
   --  Late stops (graceful entries): a switch that ended Stop_Failed because
   --  the running instance did not end within the caller's stop grace left
   --  it running, as the generation it is. The Stop it was sent cannot be
   --  taken back (a control event is one-way and carries no deadline), so
   --  it may still stop later. Its entry is then the aborted switch's
   --  generation with nothing running: procmgr starts the entry's current
   --  program again at once (relaunchLateStop), so an aborted replacement
   --  always leaves the old generation running. The typed Ready / Delay /
   --  Refuse answer to Stop, which would let the service decline instead
   --  (and remove this case), is the follow-up (LIVE-001).
   Late_Stop : array (Plan_Entry) of Process_ID := [others => No_Process];

   function Text_Of (Item : Program_Text) return String is (Item.Data (1 .. Item.Length));

   --  Start plan entry Number; Waits: it waits, parked, instead. Program:
   --  the program to start for it (empty: the entry's current one, its
   --  switched program or else its image's).
   procedure launchPlanEntry
     (Number : Positive; mayWait : Boolean; Waits : out Awaited_Access; Program : String := "")
   is
      use CCL.Configurations;
      Item : Launch_Entry renames Startup_Plan.Plan.Launches (Number);
      Image : String renames Item.Executable.Data (1 .. Item.Executable.Length);
      Name : constant String :=
        (if Program'Length > 0 then Program
         elsif Plan_Programs (Number).Length > 0 then Text_Of (Plan_Programs (Number))
         else Image);
      PID : Process_ID;
      Failure : CuBit.Launch_Arguments.Launch_Failure;
   begin
      --  These bounded milestones are also retained by the firmware
      --  boot panel until display.svc takes ownership.  They make a
      --  physical boot stall attributable to one launch boundary,
      --  without turning the panel into an unbounded serial log.
      debugPrint ("procmgr: init launch ");
      printDec (Unsigned_32 (Number));
      debugPrint ("/" & Unsigned_32'Image (Unsigned_32 (Startup_Plan.Plan.Launch_Count)) & ": " & Name & LF);
      pendingParameters := Item.Arguments;
      PID := spawnByName
        (Name, systemStartup => True, startupRole => Item.Role,
         approveRender => Render_Granted (Item, Name),
         approveFirmwareSnapshot => Item.Approve_Firmware_Snapshot,
         approveACPIHardware => Item.Approve_ACPI_Hardware,
         approvePCIConfig => Item.Approve_ACPI_PCI_Config,
         approveFirmwareData => Item.Approve_ACPI_Firmware_Data,
         approveDisplayCapture => Item.Approve_Display_Capture,
         approvePlatformPower => Item.Approve_Platform_Power,
         approveTrace => Item.Approve_Trace,
         approveTestConsole => Item.Approve_Test_Console,
         approveVMCreate => Item.Approve_VM_Create,
         approveNetwork =>
           (if Item.Approval = Approve_Declared then Declared_Network
            else No_Network),
         readyDeadlineMs => Item.Ready_Deadline_Ms,
         mayWait => mayWait, Waits => Waits,
         Failure => Failure);
      pendingParameters := (others => <>);
      if Waits /= null then
         debugPrint ("procmgr: init launch waits: " & Name & LF);
      elsif PID = No_Process then
         debugPrint ("procmgr: init spawn failed: " & Name & ": " &
                     CuBit.Launch_Failure_Text.Explanation (Failure) & LF);
      else
         debugPrint ("procmgr: init launched: " & Name & LF);
         Plan_PIDs (Number) := PID;
         --  Its declared stop stage (PWR-003).
         declare
            Known : constant Records.Index := Records.Find (PID);
         begin
            if Known /= 0 and then Records.Values (Known).Valid then
               Records.Values (Known).Stage :=
                 (case Item.Stage is
                    when CCL.Configurations.Services_Stage   => CuBit.Child_Specs.Services,
                    when CCL.Configurations.Apps_Stage       => CuBit.Child_Specs.Apps,
                    when CCL.Configurations.Power_Tail_Stage => CuBit.Child_Specs.Power_Tail);
            end if;
         end;
         Plan_Restarts.Started (Plan_Runs (Number), CuBit.Monotonic.Milliseconds);
      end if;
   end launchPlanEntry;

   --  Start the plan's next entries until one waits or none is left.
   procedure continuePlan is
      Waits : Awaited_Access;
   begin
      while Plan_Running and then Plan_Done < Startup_Plan.Plan.Launch_Count loop
         launchPlanEntry (Plan_Done + 1, True, Waits);
         if Waits /= null then
            declare
               Item : CCL.Configurations.Launch_Entry renames
                 Startup_Plan.Plan.Launches (Plan_Done + 1);
            begin
               park (new Parked_Launch'
                       (Name_Length => Item.Executable.Length, Kind => Plan_Launch,
                        Name => Item.Executable.Data (1 .. Item.Executable.Length),
                        Entry_Number => Plan_Done + 1, Sender => No_Process,
                        Request => NULL_MESSAGE, Answer => H.No_Reply, Waits => Waits,
                        Next => null));
            end;
            return;
         end if;
         Plan_Done := Plan_Done + 1;
         if Plan_PIDs (Plan_Done) /= No_Process
           and then Registrants (DRIVER_LOGSTORE) = Plan_PIDs (Plan_Done)
           and then Introduce_When_Registered (DRIVER_LOGSTORE)
         then
            if Registered_Driver (DRIVER_LOGSTORE) /= No_Process then
               --  Registered already: introduce it now (procmgr's own
               --  registrar first, as in the service loop).
               Warnings.Pump;
               Sources.Pump;
               introduceRegistered;
            end if;
            if Introduce_When_Registered (DRIVER_LOGSTORE) then
               debugPrint ("procmgr: init waits for logstore's registration before reading more" & LF);
               Plan_Awaits_Logstore := True;
               return;
            end if;
         end if;
      end loop;
      if Plan_Running then
         Plan_Running := False;
         CuBit.Boot_Milestones.Mark (CuBit.Boot_Milestone_Codes.Startup_Profile_Launched, PH.Registration);
      end if;
   end continuePlan;

   --  SYS-002: the startup profile is read within an explicit deadline.
   --  Absent (an install that never finished, or the wrong disk booted)
   --  or unreadable, startup stops with a named error on the boot panel
   --  (the "procmgr: bootstrap" line is its CURRENT STEP) and in the log;
   --  never a silent wait. The read is retried every Profile_Retry_Ms
   --  until Startup_Profile_Deadline_Ms (the filesystem may still be
   --  attaching the system volume). That window bounds the retries only:
   --  a read the storage is still serving gets Boot_Read_Deadline_Ms.
   Startup_Profile_Name : constant String := "init.ccl";
   Startup_Profile_Deadline_Ms : constant := 10_000;
   Profile_Retry_Ms : constant := 100;

   procedure processInitCCL (Compiled : out Boolean) is
      use CCL.Configurations;
      Source_Size : Unsigned_64 := 0;
      Started_At : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
      Deadline : constant Unsigned_64 := Started_At + Startup_Profile_Deadline_Ms;
      Now : Unsigned_64 := Started_At;
      Timed_Out : Boolean := False;

      procedure Stop (Why : String; Deadline_Ms : Unsigned_64 := Startup_Profile_Deadline_Ms) is
         Line : constant String :=
           "procmgr: bootstrap STOPPED: startup profile " & Startup_Profile_Name & " on the system volume " &
           Why & " (deadline" & Unsigned_64'Image (Deadline_Ms) & " ms)";
      begin
         debugPrint (Line & LF);
         Warnings.Warn (Line);
      end Stop;
   begin
      Compiled := False;
      CuBit.Boot_Milestones.Mark (CuBit.Boot_Milestone_Codes.Startup_Profile_Started, PH.Registration);
      debugPrint ("procmgr: reading init.ccl..." & LF);
      loop
         Source_Size := readFileFromFS
           (Startup_Profile_Name, Boot_Read_Deadline_Ms);
         Timed_Out := Last_Read_Timed_Out;
         exit when Source_Size > 0 or else Timed_Out;
         Now := CuBit.Monotonic.Milliseconds;
         exit when Now + Profile_Retry_Ms >= Deadline;
         declare
            Ignore : Unsigned_64;
         begin
            Ignore := CuBit.Waits.Sleep_For (Profile_Retry_Ms);
         end;
         Now := CuBit.Monotonic.Milliseconds;
      end loop;
      if Source_Size = 0 then
         if Timed_Out then
            Stop ("did not answer in time: the system volume's storage is not responding",
                  Boot_Read_Deadline_Ms);
         else
            Stop ("is missing: the install looks incomplete, or this is not a CuBit system disk");
         end if;
         return;
      elsif Source_Size > CCL.Declarations.MAX_SOURCE then
         Stop ("is larger than" & Natural'Image (CCL.Declarations.MAX_SOURCE) & " bytes");
         return;
      end if;
      declare
         Source : String (1 .. Natural (Source_Size))
           with Import, Address => imageBuf;
      begin
         Compile (Source, Startup_Plan);
      end;
      if not Startup_Plan.Success or else Startup_Plan.Plan.Kind /= Startup_Profile then
         debugPrint ("procmgr: init.ccl rejected at" & Startup_Plan.Position'Image &
                     ": " & Diagnostic_Name (Startup_Plan.Diagnostic) &
                     " (expected startup profile)" & LF);
         Stop ("does not check: " & Diagnostic_Name (Startup_Plan.Diagnostic));
         return;
      end if;
      debugPrint ("procmgr: init.ccl: ");
      printDec (Unsigned_32 (Startup_Plan.Plan.Launch_Count));
      debugPrint (" entries" & LF);
      --  The image's system policy, once, fixed for the boot.
      if Startup_Plan.Plan.Has_Policy then
         declare
            package SPC renames CCL.Setting_Codecs.System_Policy;
            Policy : SPC.System_Policy_Value;
            Decoded : Boolean;
         begin
            SPC.Decode (Startup_Plan.Plan.Policy.Data (1 .. Startup_Plan.Plan.Policy.Length), Policy, Decoded);
            if not Decoded then
               Stop ("its (policy ...) does not decode as a System_Policy");
               return;
            end if;
            Debugging_Gate :=
              (case Policy.debugging is
                 when SPC.Disabled    => Handle_Admission.Disabled,
                 when SPC.Production  => Handle_Admission.Production,
                 when SPC.Development => Handle_Admission.Development);
         end;
      end if;
      Warnings.Inform
        ("procmgr: system policy: debugging " &
         (case Debugging_Gate is
            when Handle_Admission.Disabled => "Disabled: debugging is disabled for this image",
            when Handle_Admission.Production =>
              "Production: production debugging needs owner approval (not built yet)",
            when Handle_Admission.Development =>
              "Development: Debug_Observe within the session's debug envelope") &
         (if Startup_Plan.Plan.Has_Policy then "" else " (no (policy ...): a development plan)"));
      Compiled := True;
   end processInitCCL;

   --  Run the plan from its first entry.
   procedure startPlan is
   begin
      Plan_Done := 0;
      Plan_Running := True;
      continuePlan;
   end startPlan;

   --  The instance a Stop_Failed switch left running has ended after all
   --  (it obeyed the Stop late, see Late_Stop): start the entry's current
   --  program again, the generation the aborted switch kept. Not a restart
   --  under the entry's (restart ..) policy: the switch was aborted, so the
   --  generation must still be running.
   procedure relaunchLateStop (Number : Positive; Failed : Boolean) is
      Item : CCL.Configurations.Launch_Entry renames Startup_Plan.Plan.Launches (Number);
      Name : String renames Item.Executable.Data (1 .. Item.Executable.Length);
      Waits : Awaited_Access;
      Ignore : Boolean;
   begin
      if Plan_Runs (Number).State = Plan_Restarts.Running then
         Plan_Restarts.Ended (Plan_Runs (Number), CuBit.Monotonic.Milliseconds, False, False, Ignore);
      end if;
      Warnings.Warn ("procmgr: " & Name & " ended after its stop grace (" &
                     (if Failed then "failing" else "cleanly") &
                     "; the switch was aborted): starting it again");
      --  Its providers are running already: never park it.
      launchPlanEntry (Number, False, Waits);
      if Waits /= null then
         Free (Waits);
      end if;
      if Plan_PIDs (Number) /= No_Process then
         Warnings.Inform ("procmgr: " & Name & " runs again pid" & Image (Plan_PIDs (Number)) &
                          " (its generation kept by the aborted switch)");
      else
         Warnings.Warn ("procmgr: " & Name & " did not start again after its late stop");
      end if;
   end relaunchLateStop;

   ---------------------------------------------------------------------------
   --  Supervision: a plan entry's process ended. Failed: a nonzero code, or
   --  killed or faulted. An entry with (restart on-failure) is started again
   --  when its delay ends (restartDue); procmgr never restarts anything else.
   ---------------------------------------------------------------------------
   procedure superviseExit (Exited : Process_ID; Failed : Boolean) is
      Scheduled : Boolean;
   begin
      for Number in 1 .. Startup_Plan.Plan.Launch_Count loop
         if Plan_PIDs (Number) = Exited and then Exited /= No_Process
           and then Service_Switches.Active (Switch) and then Switch_Entry = Number
         then
            --  The switch in progress decides what runs next.
            Plan_PIDs (Number) := No_Process;
            if Plan_Runs (Number).State = Plan_Restarts.Running then
               declare
                  Ignore : Boolean;
               begin
                  Plan_Restarts.Ended (Plan_Runs (Number), CuBit.Monotonic.Milliseconds, False, False, Ignore);
               end;
            end if;
         elsif Plan_PIDs (Number) = Exited and then Exited /= No_Process
           and then Late_Stop (Number) = Exited
         then
            Plan_PIDs (Number) := No_Process;
            Late_Stop (Number) := No_Process;
            if not Shutting_Down then
               relaunchLateStop (Number, Failed);
            end if;
         elsif Plan_PIDs (Number) = Exited and then Exited /= No_Process then
            Plan_PIDs (Number) := No_Process;
            declare
               use type CCL.Configurations.Restart_Policy;
               Item : CCL.Configurations.Launch_Entry renames Startup_Plan.Plan.Launches (Number);
               Name : String renames Item.Executable.Data (1 .. Item.Executable.Length);
               Run : Plan_Restarts.Run renames Plan_Runs (Number);
            begin
               if Run.State = Plan_Restarts.Running then
                  --  Nothing restarts while the system stops (PWR-003).
                  Plan_Restarts.Ended
                    (Run, CuBit.Monotonic.Milliseconds, Failed,
                     Item.Restart = CCL.Configurations.On_Failure and then not Shutting_Down, Scheduled);
                  if Scheduled then
                     Warnings.Warn ("procmgr: restarting " & Name & " in" &
                                    Unsigned_64'Image (Run.Delay_Ms) & " ms (restart" &
                                    Natural'Image (Run.Restarts + 1) & ")");
                  end if;
               end if;
            end;
         end if;
      end loop;
   end superviseExit;

   --  The earliest restart time, or 0 when none is waiting.
   function nextRestart return Unsigned_64 is
      Earliest : Unsigned_64 := 0;
   begin
      --  Nothing restarts while the system stops (PWR-003).
      if Shutting_Down then
         return 0;
      end if;
      for Run of Plan_Runs loop
         if Run.State = Plan_Restarts.Waiting and then
           (Earliest = 0 or else Run.Due_At < Earliest)
         then
            Earliest := Unsigned_64'Max (1, Run.Due_At);
         end if;
      end loop;
      return Earliest;
   end nextRestart;

   --  Start every entry whose restart delay ended. Not while the plan is
   --  still starting: its own order and waits come first.
   procedure restartDue is
      Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
      Waits : Awaited_Access;
   begin
      if Plan_Running or else Shutting_Down then return; end if;
      for Number in 1 .. Startup_Plan.Plan.Launch_Count loop
         if Plan_Restarts.Due (Plan_Runs (Number), Now) then
            declare
               Item : CCL.Configurations.Launch_Entry renames Startup_Plan.Plan.Launches (Number);
               Name : String renames Item.Executable.Data (1 .. Item.Executable.Length);
               Restarts : constant Natural := Plan_Runs (Number).Restarts;
            begin
               --  Its providers are running already: never park a restart.
               launchPlanEntry (Number, False, Waits);
               if Plan_PIDs (Number) /= No_Process then
                  Plan_Runs (Number).Restarts := Natural'Min (Restarts, Natural'Last - 1) + 1;
                  Warnings.Inform ("procmgr: restarted " & Name & " pid" &
                                   Image (Plan_PIDs (Number)) & " (restart" &
                                   Natural'Image (Plan_Runs (Number).Restarts) & ")");
               else
                  Plan_Restarts.Refused (Plan_Runs (Number), Now);
                  Warnings.Warn ("procmgr: restart of " & Name & " refused; again in" &
                                 Unsigned_64'Image (Plan_Runs (Number).Delay_Ms) & " ms");
               end if;
            end;
         end if;
      end loop;
   end restartDue;

   ---------------------------------------------------------------------------
   --  Service switches (CuBit.Service_Replacement): a startup-plan service
   --  switched to another program while it runs, the first piece of system
   --  generations (a generation's pin changed). Admitted by
   --  CuBit.Service_Replacement.Authorize and Content_Admits; driven by
   --  Service_Switches (stop, start as the same entry, health window,
   --  rollback to the program the entry ran before). procmgr never waits
   --  for an instance: each step is one action, and the requester's reply
   --  is saved until the switch ends (within Answer_Bound_Ms).
   ---------------------------------------------------------------------------
   package SR renames CuBit.Service_Replacement;
   package SS renames Service_Switches;
   use type SR.Refusal;
   use type SR.Outcome;
   use type SS.Action;
   use type SS.Phase;

   --  The role Process provides (registers), if any: its Registrants entry.
   function providedRole (Process : Process_ID; Role : out Service_Role) return Boolean is
   begin
      Role := 0;
      for R in Service_Role loop
         if Registrants (R) = Process and then Process /= No_Process then
            Role := R;
            return True;
         end if;
      end loop;
      return False;
   end providedRole;

   --  The manifest fingerprint of the program procmgr reads by Name, into
   --  Into (heap, Maximum_Fingerprint_Bytes): Readable False when it could
   --  not be read or its sections do not decode.
   procedure fingerprintOf
     (Name : String; Into : in out Manifest_Fingerprints.Bytes;
      Length : out Manifest_Fingerprints.Fingerprint_Length; Readable : out Boolean)
   is
      use type Manifest_Fingerprints.Reading;
      Size : constant Unsigned_64 := readFileFromFS (Name, File_Request_Deadline_Ms);
      Result : Manifest_Fingerprints.Reading := Manifest_Fingerprints.Not_ELF64;
   begin
      Length := 0;
      Readable := False;
      if Size < 64 or else Size > Unsigned_64 (Natural'Last / 4) then
         Warnings.Warn ("procmgr: switch: " & Name & " could not be read");
         return;
      end if;
      declare
         Elf : Manifest_Fingerprints.Bytes (1 .. Natural (Size))
           with Import, Address => imageBuf;
      begin
         Manifest_Fingerprints.Fingerprint (Elf, Into, Length, Result);
      end;
      Readable := Result = Manifest_Fingerprints.Read;
      if not Readable then
         Warnings.Warn ("procmgr: switch: the manifest sections of " & Name &
                        " do not read (" & Manifest_Fingerprints.Reading'Image (Result) & ")");
      end if;
   end fingerprintOf;

   function Failure_Text (Why : SR.Health_Failure) return String is
     (case Why is
        when SR.Healthy => "healthy",
        when SR.Start_Refused => "the program did not start",
        when SR.Exited_Early => "the program ended inside its health window",
        when SR.Not_Registered => "the program did not register its service in time");

   function Outcome_Text (Item : SS.Switch) return String is
     (case Item.Result is
        when SR.Switched => "switched",
        when SR.Rolled_Back => "rolled back (" & Failure_Text (Item.Failure) & ")",
        when SR.Stop_Failed =>
          (if Item.Old_Unclean then "not switched: the running instance did not quiesce cleanly; " &
                                    "the program before was started again"
           else "not switched: the running instance did not end"),
        when SR.Rollback_Failed => "FAILED (" & Failure_Text (Item.Failure) &
                                   ") and the program before did not start again");

   --  Take the action First asks for, and every action that follows at once.
   procedure driveSwitch (First : SS.Action) is
      Next : SS.Action := First;
      Number : constant Positive := Positive'Max (1, Switch_Entry);
      Item : CCL.Configurations.Launch_Entry renames Startup_Plan.Plan.Launches (Number);
      Entry_Name : String renames Item.Executable.Data (1 .. Item.Executable.Length);
      Now : SS.Time;

      function Program_Or_Image (P : Program_Text) return String is
        (if P.Length = 0 then Entry_Name else Text_Of (P));

      procedure End_Process (Process : SS.Process_Word) is
         Known : constant Records.Index := Records.Find (From_Word (Process));
      begin
         if Known /= 0 and then Records.Values (Known).Valid then
            if killProcess (From_Word (Process), Records.Values (Known).Control) /= 0 then
               Warnings.Warn ("procmgr: switch: ending pid" & Image (From_Word (Process)) & " was refused");
            end if;
         end if;
      end End_Process;

      procedure Start (Program : String) is
         Waits : Awaited_Access;
         Role : Service_Role;
         pragma Warnings (Off, Role);   --  only whether it has one matters here
         Must : Boolean;
      begin
         launchPlanEntry (Number, False, Waits, Program);
         if Waits /= null then
            Free (Waits);
         end if;
         Must := providedRole (Plan_PIDs (Number), Role);
         Now := Unsigned_64'Min (CuBit.Monotonic.Milliseconds, SS.Latest_Time);
         SS.Started (Switch, To_Word (Plan_PIDs (Number)), Must, Now, Next);
      end Start;
   begin
      if Switch_Entry = 0 then
         return;
      end if;
      while Next /= SS.Nothing loop
         case Next is
            when SS.Nothing => null;
            when SS.Ask_Stop =>
               declare
                  Answer : constant Unsigned_64 := CuBit.Kernel_Calls.Call
                    (CuBit.Kernel_ABI.Send_Control, Switch.Old_Process,
                     CuBit.Control_Events.Control_Kind'Enum_Rep (CuBit.Control_Events.Stop));
               begin
                  Warnings.Inform ("procmgr: switch " & Entry_Name & ": Stop sent to pid" &
                                   Image (From_Word (Switch.Old_Process)) &
                                   (if Answer = 0 then "" else " (not taken; ended at the grace deadline)"));
               end;
               Next := SS.Nothing;
            when SS.End_Old =>
               Warnings.Inform ("procmgr: switch " & Entry_Name & ": ending pid" &
                                Image (From_Word (Switch.Old_Process)));
               End_Process (Switch.Old_Process);
               Next := SS.Nothing;
            when SS.Ask_Stop_New =>
               declare
                  Answer : constant Unsigned_64 := CuBit.Kernel_Calls.Call
                    (CuBit.Kernel_ABI.Send_Control, Switch.New_Process,
                     CuBit.Control_Events.Control_Kind'Enum_Rep (CuBit.Control_Events.Stop));
               begin
                  Warnings.Warn ("procmgr: switch " & Entry_Name & ": " & Failure_Text (Switch.Failure) &
                                 "; Stop sent to pid" & Image (From_Word (Switch.New_Process)) &
                                 (if Answer = 0 then "" else " (not taken; ended at the grace deadline)"));
               end;
               Next := SS.Nothing;
            when SS.End_New =>
               Warnings.Warn ("procmgr: switch " & Entry_Name & ": ending pid" &
                              Image (From_Word (Switch.New_Process)) & ", which failed its health window");
               End_Process (Switch.New_Process);
               Next := SS.Nothing;
            when SS.Start_Program =>
               Warnings.Inform ("procmgr: switch " & Entry_Name & ": starting " &
                                Program_Or_Image (Switch_Program) & " as the entry; health window" &
                                Unsigned_64'Image (Switch.Health) & " ms");
               Start (Program_Or_Image (Switch_Program));
            when SS.Start_Previous =>
               Warnings.Warn ("procmgr: switch " & Entry_Name & ": " & Failure_Text (Switch.Failure) &
                              "; starting the program before again: " & Program_Or_Image (Switch_Previous));
               Start (Program_Or_Image (Switch_Previous));
            when SS.Answer =>
               declare
                  Reply : Message := NULL_MESSAGE;
                  Ignore : Boolean;
               begin
                  if Switch.Result = SR.Switched then
                     Plan_Programs (Number) := Switch_Program;
                  elsif Switch.Result = SR.Rolled_Back then
                     Plan_Programs (Number) := Switch_Previous;
                  end if;
                  --  Graceful and not ended in its grace: it was sent Stop
                  --  and still runs; if it ends later, it runs again.
                  if Switch.Result = SR.Stop_Failed and then not Switch.Old_Unclean
                    and then Switch.Graceful and then Plan_PIDs (Number) /= No_Process
                    and then To_Word (Plan_PIDs (Number)) = Switch.Old_Process
                  then
                     Late_Stop (Number) := Plan_PIDs (Number);
                  end if;
                  declare
                     Line : constant String :=
                       "procmgr: switch " & Entry_Name & " " & Outcome_Text (Switch) & "; it runs " &
                       (if Plan_PIDs (Number) = No_Process then "nothing"
                        else Program_Or_Image (Plan_Programs (Number)) & " pid" & Image (Plan_PIDs (Number)));
                  begin
                     if Switch.Result = SR.Switched then
                        Warnings.Inform (Line);
                     else
                        Warnings.Warn (Line);
                     end if;
                  end;
                  Reply.tag := (label => REPLY_OK, length => 3, protocol => 0, ipc => <>, reserved => 0);
                  Reply.words (0) := SR.Outcome'Enum_Rep (Switch.Result);
                  Reply.words (1) := SR.Health_Failure'Enum_Rep (Switch.Failure);
                  Reply.words (2) := To_Word (Plan_PIDs (Number));
                  if H.Valid (Switch_Reply) then
                     Ignore := HIPC.Answer (Switch_Reply, Reply);
                  end if;
                  Switch_Reply := H.No_Reply;
                  SS.Answered (Switch);
                  Switch_Entry := 0;
               end;
               Next := SS.Nothing;
         end case;
      end loop;
   end driveSwitch;

   --  A process ended: the switch's instances move it on.
   procedure switchExited (Exited : Process_ID; Clean : Boolean) is
      Next : SS.Action;
   begin
      if SS.Active (Switch) and then Exited /= No_Process then
         SS.Exited (Switch, To_Word (Exited), Clean, Next);
         driveSwitch (Next);
      end if;
   end switchExited;

   --  Time passed: deadlines and the new instance's registration.
   procedure switchTick is
      Next : SS.Action;
      Role : Service_Role;
      Registered : Boolean := False;
   begin
      if SS.Active (Switch) and then Switch.State = SS.Checking then
         Registered := providedRole (From_Word (Switch.New_Process), Role)
           and then Registered_Driver (Role) = From_Word (Switch.New_Process);
      end if;
      if SS.Active (Switch) then
         SS.Tick (Switch, Unsigned_64'Min (CuBit.Monotonic.Milliseconds, SS.Latest_Time), Registered, Next);
         driveSwitch (Next);
      end if;
   end switchTick;

   --  Who may switch a service: a launcher that stages programs (its
   --  launch table has the staging directory, as for staged app launches)
   --  and reads the program it names (a read scope covering it, as the
   --  storehouse's programs, @system/storehouse/executables/<sha256>, are
   --  for ccl-remote).
   Staging_Probe : constant String := "@system/work/generations/probe";

   function launcherReads (Launcher : Launch_State; Name : String) return Boolean is
   begin
      for h in 1 .. Launcher.Scope_Count loop
         if LAuth.Scope_Covered
              (Launcher.Scopes (h).Service, Launcher.Scopes (h).Rights,
               Launcher.Scopes (h).Prefix (1 .. Launcher.Scopes (h).Length),
               SERVICE_FS, CuBit.Launch_Authority.Read_Right, Name)
         then
            return True;
         end if;
      end loop;
      return False;
   end launcherReads;

   --  SR.Switch_Operation: switch a plan entry to a program (or back to its
   --  image's). Refused at once, typed; admitted, the reply is saved and
   --  given when the switch ends.
   --  SR.Boot_Stage_Operation (Stage): the same admission, without the
   --  restart contract, for the next boot; nothing is stopped or started.
   procedure handleServiceSwitch (sender : Process_ID; msg : Message; Stage : Boolean := False) is
      Service_Bytes : constant Unsigned_64 := msg.words (1);
      Program_Bytes : constant Unsigned_64 := msg.words (2);
      Grace : constant Unsigned_64 := SR.Grace_Of (msg.words (3));
      Health : constant Unsigned_64 := SR.Health_Window_Of (msg.words (3));
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped : System.Address;
      Ok, Returned : Boolean;

      procedure Refuse (Why : SR.Refusal; Detail : String := "") is
      begin
         Warnings.Warn ("procmgr: switch refused: " & SR.Refusal_Name (Why) &
                        (if Detail'Length > 0 then ": " & Detail else ""));
         sendReply (sender, REPLY_ERR, SR.Refusal'Enum_Rep (Why));
      end Refuse;
   begin
      if msg.tag.length /= SR.Request_Words
        or else (if Stage then not SR.Stage_Request_Valid (Service_Bytes, Program_Bytes, msg.words (3))
                 else not SR.Request_Valid (Service_Bytes, Program_Bytes, Grace, Health))
        or else not CuBit.Grant_References.Valid_Wire (msg.words (0))
        or else launchIndex (sender) = 0
      then
         Refuse (SR.Malformed);
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (Reference, sender, 0, Service_Bytes + Program_Bytes,
         CuBit.Memory_Grants.Read_Access, Mapped, Ok);
      if not Ok then
         Refuse (SR.Grant_Unavailable);
         return;
      end if;
      declare
         Service_Source : constant String (1 .. Natural (Service_Bytes)) with Import, Address => Mapped;
         Program_Source : constant String (1 .. Natural (Program_Bytes))
           with Import, Address => Mapped + Storage_Offset (Service_Bytes);
         Service : constant String (1 .. Natural (Service_Bytes)) := Service_Source;
         Program : constant String (1 .. Natural (Program_Bytes)) := Program_Source;
         Number : CCL.Configurations.Launch_Number := 0;
         Facts : SR.Authority_Facts;
         Verdict : SR.Decision;
         Launcher : Launch_State renames Launches.Values (launchIndex (sender));
      begin
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         for N in 1 .. Startup_Plan.Plan.Launch_Count loop
            declare
               E : CCL.Configurations.Executable_Text renames Startup_Plan.Plan.Launches (N).Executable;
            begin
               if E.Data (1 .. E.Length) = Service then
                  Number := N;
               end if;
            end;
         end loop;
         Facts.Entry_Known := Number /= 0;
         --  Its restart contract: reboot means never live.
         Facts.Entry_Replaceable := Number /= 0 and then CCL.Configurations."/="
           (Startup_Plan.Plan.Launches (Positive'Max (1, Number)).Replacement,
            CCL.Configurations.Reboot_To_Replace);
         --  The requester stages the program (as for a staged app); a switch
         --  back to the image needs a staging directory of its own.
         Facts.Requester_Stages :=
           admitLaunch (Launcher, Staging_Probe) in Staged | Staged_Switch_Off
           and then (Program'Length = 0 or else launcherReads (Launcher, Program));
         Facts.Switch_On := Staged_Launches_Allowed;
         --  Nothing is signed yet (AUTH-001): every program is Unsigned.
         Facts.Signature := SR.Unsigned;
         Facts.In_Progress := SS.Active (Switch) or else Plan_Running;
         Verdict := (if Stage then SR.Authorize_For_Boot (Facts) else SR.Authorize (Facts));
         if not Stage and then Verdict.Admitted and then Grace = 0 and then CCL.Configurations."="
           (Startup_Plan.Plan.Launches (Number).Replacement, CCL.Configurations.Graceful)
         then
            --  A graceful stop needs time to quiesce: the caller must give it.
            Refuse (SR.Malformed, Service & " is (replace graceful): give a stop grace");
            return;
         end if;
         if not Verdict.Admitted then
            Refuse (Verdict.Why, Service & " -> " & (if Program'Length = 0 then "its image" else Program) &
                    (if Verdict.Why = SR.Switch_Off
                     then " (turn on Allow app replacement in the Desktop's status panel)" else ""));
            return;
         end if;
         --  The content: the program's manifest must be the image's.
         if Program'Length > 0 then
            declare
               type Fingerprint_Access is access Manifest_Fingerprints.Bytes;
               procedure Release is new Ada.Unchecked_Deallocation
                 (Manifest_Fingerprints.Bytes, Fingerprint_Access);
               Image_Print : Fingerprint_Access :=
                 new Manifest_Fingerprints.Bytes (1 .. Manifest_Fingerprints.Maximum_Fingerprint_Bytes);
               Program_Print : Fingerprint_Access :=
                 new Manifest_Fingerprints.Bytes (1 .. Manifest_Fingerprints.Maximum_Fingerprint_Bytes);
               Image_Length, Program_Length : Manifest_Fingerprints.Fingerprint_Length := 0;
               Image_Readable : Boolean;
               Program_Readable : Boolean := False;
            begin
               fingerprintOf (Service, Image_Print.all, Image_Length, Image_Readable);
               if Image_Readable then
                  fingerprintOf (Program, Program_Print.all, Program_Length, Program_Readable);
               end if;
               Verdict := SR.Content_Admits
                 (Image_Readable, Program_Readable,
                  Image_Readable and then Program_Readable and then Manifest_Fingerprints.Same
                    (Image_Print (1 .. Image_Length), Program_Print (1 .. Program_Length)));
               Release (Image_Print);
               Release (Program_Print);
            end;
            if not Verdict.Admitted then
               Refuse (Verdict.Why, Service & " -> " & Program);
               return;
            end if;
         end if;
         if Stage then
            --  Admitted for the next boot: Boot_Generations takes the pin
            --  when the plan next starts (and checks it again then).
            Warnings.Inform ("procmgr: " & Service & " -> " & Program &
                             " admitted for the next boot (pid" & Image (sender) &
                             "); not signature-checked (AUTH-001)");
            sendReply (sender, REPLY_OK, 0);
            return;
         end if;
         --  Admitted: keep the reply, and go.
         declare
            Saved : constant HIPC.Saved_Reply := HIPC.Save_Reply;
            Next : SS.Action;
         begin
            if not Saved.Saved then
               Refuse (SR.Busy, "its reply could not be kept");
               return;
            end if;
            Switch_Reply := Saved.Handle;
            Switch_Entry := Number;
            --  This switch watches the running instance itself now.
            Late_Stop (Number) := No_Process;
            Switch_Program := (others => <>);
            Switch_Program.Length := Program'Length;
            Switch_Program.Data (1 .. Program'Length) := Program;
            Switch_Previous := Plan_Programs (Number);
            Warnings.Inform ("procmgr: switch " & Service & " to " &
                             (if Program'Length = 0 then "its image's program" else Program) &
                             " for pid" & Image (sender) & " (stop grace" & Unsigned_64'Image (Grace) &
                             " ms, health window" & Unsigned_64'Image (Health) &
                             " ms); not signature-checked (AUTH-001)");
            SS.Begin_Switch
              (Switch, To_Word (Plan_PIDs (Number)),
               CCL.Configurations."=" (Startup_Plan.Plan.Launches (Number).Replacement,
                                       CCL.Configurations.Graceful),
               Grace, Health,
               Unsigned_64'Min (CuBit.Monotonic.Milliseconds, SS.Latest_Time), Next);
            driveSwitch (Next);
         end;
      end;
   end handleServiceSwitch;

   ---------------------------------------------------------------------------
   --  The current system generation at boot (SYS-003, Boot_Generations):
   --  its pins choose the programs plan entries start from, before the plan
   --  runs. Every pin is checked as a live switch's program is (its manifest
   --  is the image program's) and against its content address; a pin that
   --  does not check is not taken: the entry runs its image's program and
   --  the drift is logged (logstore keeps an installed system's log on its
   --  disk). Reads here have Boot_Read_Deadline_Ms each.
   ---------------------------------------------------------------------------
   Storehouse_Root : constant String := "@system/storehouse";
   Current_Generation_Name : constant String := Storehouse_Root & "/profiles/system/current";
   Generation_Directory : constant String := Storehouse_Root & "/profiles/system/";
   Executables_Directory : constant String := Storehouse_Root & "/executables/";
   --  Named ceilings on what is read: the pointer and one record.
   Maximum_Pointer_Bytes : constant := 16;
   Maximum_Record_Bytes : constant := 4_096;
   --  Pins not taken at this boot (drift), for the log's summary line.
   Generation_Drift : Natural := 0;

   procedure applyCurrentGeneration is
      package BG renames Boot_Generations;
      package Records renames System_Generation_Records;
      package Reading renames System_Generation_Records.Reading;
      use type BG.Pin_Verdict;
      Size : Unsigned_64;
      Reading_State : BG.Generation_Reading := BG.No_Generation;
      Number : Reading.Kept_Generation := Records.Image_Generation;
      G : Records.Generation_Value;
      Valid : Boolean;

      function Decimal (Value : Unsigned_64) return String is
         Text : constant String := Value'Image;
      begin
         return Text (Text'First + 1 .. Text'Last);
      end Decimal;

      function Generation_Text return String is ("generation " & Decimal (Unsigned_64 (Number)));

      --  Text of the file just read (in imageBuf).
      function Read_Text return String is
         Text : constant String (1 .. Natural (Size)) with Import, Address => imageBuf;
      begin
         return Text;
      end Read_Text;
   begin
      Size := readFileFromFS (Current_Generation_Name, Boot_Read_Deadline_Ms);
      if Size = 0 and then not Last_Read_Timed_Out then
         Warnings.Inform ("procmgr: no current system generation (" & Current_Generation_Name &
                          "): every entry runs its image's program");
         return;
      elsif Size = 0 or else Size > Maximum_Pointer_Bytes then
         Reading_State := (if Size = 0 then BG.Record_Missing else BG.Pointer_Invalid);
      else
         Reading.Parse_Current (Read_Text, Number, Valid);
         if not Valid then
            Reading_State := BG.Pointer_Invalid;
         elsif Number = Records.Image_Generation then
            Warnings.Inform ("procmgr: generation 0 (the image's) is current");
            return;
         else
            Size := readFileFromFS (Generation_Directory & Decimal (Unsigned_64 (Number)), Boot_Read_Deadline_Ms);
            if Size = 0 or else Size > Maximum_Record_Bytes then
               Reading_State := BG.Record_Missing;
            else
               Reading.Parse (Read_Text, Number, G, Valid);
               Reading_State := (if Valid then BG.Read else BG.Record_Invalid);
            end if;
         end if;
      end if;
      if not BG.Applies (Reading_State) then
         Generation_Drift := Generation_Drift + 1;
         Warnings.Warn ("procmgr: DRIFT: system generation not applied at boot (" &
                        BG.Reading_Name (Reading_State) & "; " & Current_Generation_Name &
                        "): every entry runs its image's program");
         debugPrint ("procmgr: generation drift: " & BG.Reading_Name (Reading_State) & LF);
         return;
      end if;
      for P in 1 .. G.Count loop
         declare
            Service : constant String := G.Pins (P).Service (1 .. G.Pins (P).Length);
            Program : constant String := Executables_Directory & Records.Hex (G.Pins (P).Program);
            Facts : BG.Pin_Facts;
            Verdict : BG.Pin_Verdict;
            Number_Of_Entry : CCL.Configurations.Launch_Number := 0;
            type Fingerprint_Access is access Manifest_Fingerprints.Bytes;
            procedure Release is new Ada.Unchecked_Deallocation
              (Manifest_Fingerprints.Bytes, Fingerprint_Access);
            Image_Print : Fingerprint_Access;
            Program_Print : Fingerprint_Access;
            Image_Length, Program_Length : Manifest_Fingerprints.Fingerprint_Length := 0;
         begin
            for N in 1 .. Startup_Plan.Plan.Launch_Count loop
               declare
                  E : CCL.Configurations.Executable_Text renames Startup_Plan.Plan.Launches (N).Executable;
               begin
                  if E.Data (1 .. E.Length) = Service then
                     Number_Of_Entry := N;
                  end if;
               end;
            end loop;
            Facts.Entry_Known := Number_Of_Entry /= 0;
            --  Reading.Parse admits each service once.
            Facts.First_For_Entry := Facts.Entry_Known;
            if Facts.First_For_Entry then
               Program_Print := new Manifest_Fingerprints.Bytes (1 .. Manifest_Fingerprints.Maximum_Fingerprint_Bytes);
               Image_Print := new Manifest_Fingerprints.Bytes (1 .. Manifest_Fingerprints.Maximum_Fingerprint_Bytes);
               --  The pinned program: whole, its SHA-256, then its manifest.
               Size := readFileFromFS (Program, Boot_Read_Deadline_Ms);
               Facts.Program_Readable := Size >= 64 and then Size <= Unsigned_64 (Natural'Last / 4);
               if Facts.Program_Readable then
                  declare
                     Bytes : constant Storehouse.Byte_Array (1 .. Natural (Size)) with Import, Address => imageBuf;
                     Elf : constant Manifest_Fingerprints.Bytes (1 .. Natural (Size)) with Import, Address => imageBuf;
                     Result : Manifest_Fingerprints.Reading;
                     use type Storehouse.Digest;
                     use type Manifest_Fingerprints.Reading;
                  begin
                     Facts.Content_Matches :=
                       Storehouse.Hashing.Of_Bytes (Bytes) = Storehouse.Digest (G.Pins (P).Program);
                     Manifest_Fingerprints.Fingerprint (Elf, Program_Print.all, Program_Length, Result);
                     Facts.Program_Readable := Result = Manifest_Fingerprints.Read;
                  end;
               end if;
               if Facts.Program_Readable and then Facts.Content_Matches then
                  fingerprintOf (Service, Image_Print.all, Image_Length, Facts.Image_Readable);
                  Facts.Same_Manifest := Facts.Image_Readable and then Manifest_Fingerprints.Same
                    (Image_Print (1 .. Image_Length), Program_Print (1 .. Program_Length));
               end if;
               Release (Program_Print);
               Release (Image_Print);
            end if;
            Verdict := BG.Judge (Facts);
            if Verdict = BG.Applied then
               Plan_Programs (Number_Of_Entry) := (others => <>);
               Plan_Programs (Number_Of_Entry).Length := Program'Length;
               Plan_Programs (Number_Of_Entry).Data (1 .. Program'Length) := Program;
               Warnings.Inform ("procmgr: " & Generation_Text & ": " & Service & " runs " & Program &
                                " (pinned; manifest checked; not signature-checked, AUTH-001)");
               debugPrint ("procmgr: generation pin applied: " & Service & LF);
            else
               Generation_Drift := Generation_Drift + 1;
               Warnings.Warn ("procmgr: DRIFT: " & Generation_Text & " pins " & Service & " to " &
                              Program & ", not applied: " & BG.Verdict_Name (Verdict) &
                              "; it runs its image's program");
               debugPrint ("procmgr: generation drift: " & Service & ": " & BG.Verdict_Name (Verdict) & LF);
            end if;
         end;
      end loop;
      Warnings.Inform ("procmgr: " & Generation_Text & " is current:" & Natural'Image (G.Count) & " pin(s)," &
                       Natural'Image (Generation_Drift) & " not applied");
   end applyCurrentGeneration;

   ---------------------------------------------------------------------------
   --  Start_Child (SUP-001, CuBit.Child_Specs; docs/otp-supervision-study.md).
   --  procmgr is the only process creator; supervision is the typed edge
   --  it records here (Supervision_Edges), supervisor -> child. The child
   --  is spawned with its supervisor as the kernel's parent, so the kernel
   --  sends the supervisor the child's EVENT_CHILD_EXIT, asynchronously:
   --  procmgr never calls a supervisor. When a supervisor ends (normal
   --  exit, exception or crash), procmgr reaps its children (stops them);
   --  foster care during a supervisor's replacement is a later slice.
   --
   --  The start-children scope: a program's launch table (may_launch)
   --  names the programs it may start with a value it gives; the device
   --  manager may start device-class children only (Source Device_Class),
   --  and only programs whose own manifest drives the class, with the
   --  startup plan's class launch block for their policy parameters. Only
   --  the device manager may give badge bindings.
   ---------------------------------------------------------------------------
   package CS renames CuBit.Child_Specs;
   package SE renames Supervision_Edges;
   use type CS.Decode_Status, CS.Parameter_Source, CS.Start_Mode, CS.Sink_Kind;
   use type Class_Launch_Checks.Outcome;

   --  The edges, in a heap block that doubles as needed (CuAlloc), up to a
   --  named ceiling of supervised processes alive at once.
   Maximum_Supervised : constant := 65_536;
   Initial_Edges : constant := 32;
   type Edge_Block is access SE.Edge_Array;
   procedure Free is new Ada.Unchecked_Deallocation (SE.Edge_Array, Edge_Block);
   Edges : Edge_Block := null;
   Edge_Length : Natural := 0;
   Next_Rank : Unsigned_64 := 1;

   --  Room for one more edge; False (nothing changed) at the ceiling or
   --  when the heap refuses.
   function edgeRoom return Boolean is
      Grown : Edge_Block;
   begin
      if Edges /= null and then Edge_Length < Edges'Last then
         return True;
      end if;
      declare
         Capacity : constant Natural :=
           (if Edges = null then Initial_Edges else Natural'Min (Edges'Length * 2, Maximum_Supervised));
      begin
         if Capacity <= Edge_Length then
            return False;
         end if;
         Grown := new SE.Edge_Array'(1 .. Capacity => (others => <>));
         if Edges /= null then
            Grown (1 .. Edge_Length) := Edges (1 .. Edge_Length);
            Free (Edges);
         end if;
         Edges := Grown;
         return True;
      end;
   exception
      when Storage_Error =>
         return False;
   end edgeRoom;


   procedure handleStartChild (sender : Process_ID; msg : Message) is
      package LA renames CuBit.Launch_Arguments;
      Spec_Bytes : constant Unsigned_64 := msg.words (1);
      Spec : CS.Spec;
      Status : CS.Decode_Status := CS.Too_Short;
      From_Device_Manager : constant Boolean :=
        sender /= No_Process and then sender = Registered_Driver (DRIVER_DEVMGR);

      procedure Answer (Label : Unsigned_32; Word0, Word1 : Unsigned_64) is
         Reply_Message : Message := NULL_MESSAGE;
         Ignore : Unsigned_64;
      begin
         Reply_Message.tag := (label => Label, length => CS.Reply_Words, protocol => 0, ipc => <>, reserved => 0);
         Reply_Message.words := [0 => Word0, 1 => Word1, others => 0];
         Ignore := reply (sender, Reply_Message);
      end Answer;

      procedure Refuse (Why : CS.Refusal; Detail : Unsigned_64 := 0; Text : String := "") is
      begin
         Warnings.Warn ("procmgr: start_child from " & processName (sender) & " pid" & Image (sender) &
                        (if Spec.Name_Bytes > 0 then " for " & Spec.Name (1 .. Spec.Name_Bytes) else "") &
                        " refused: " & CS.Refusal_Name (Why) &
                        (if Text'Length > 0 then " (" & Text & ")" else ""));
         Answer (REPLY_ERR, CS.Refusal'Enum_Rep (Why), Detail);
      end Refuse;
   begin
      if msg.tag.length /= CS.Request_Words
        or else Spec_Bytes not in 1 .. CS.Maximum_Spec_Bytes
        or else not CuBit.Grant_References.Valid_Wire (msg.words (0))
        or else launchIndex (sender) = 0
      then
         Refuse (CS.Malformed_Request);
         return;
      end if;

      --  The spec, copied out of the caller's grant before it is decoded.
      declare
         Reference : constant CuBit.Memory_Grants.Grant_Reference :=
           CuBit.Grant_References.Decode (msg.words (0));
         Mapped : System.Address;
         Ok, Returned : Boolean;
         Copy : CS.Byte_Array (1 .. Natural (Spec_Bytes));
      begin
         CuBit.Memory_Grants.Acquire
           (Reference, sender, 0, Spec_Bytes, CuBit.Memory_Grants.Read_Access, Mapped, Ok);
         if not Ok then
            Refuse (CS.Grant_Unavailable);
            return;
         end if;
         declare
            Source : constant CS.Byte_Array (1 .. Natural (Spec_Bytes)) with Import, Address => Mapped;
         begin
            Copy := Source;
         end;
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         CS.Decode (Copy, Spec, Status);
      end;
      if Status /= CS.Decoded then
         Spec := (others => <>);
         Refuse (CS.Malformed_Spec, CS.Decode_Status'Pos (Status), CS.Status_Name (Status));
         return;
      end if;

      declare
         Name : constant String := Spec.Name (1 .. Spec.Name_Bytes);
         Merged : String (1 .. Child_Parameters.Maximum_Merged_Bytes);
         Merged_Length : Natural := 0;
         newPID : Process_ID;
         Failure : LA.Launch_Failure := LA.Spawn_Failed;
         Waits : Awaited_Access;
         Class_Line : Natural := 0;
      begin
         --  Badges come only from the device manager, which admitted the
         --  device they name.
         if Spec.Bindings > 0 and then not From_Device_Manager then
            Refuse (CS.Bindings_Not_Granted);
            return;
         end if;
         --  A sink (an endpoint to the child, for its supervisor to hand on)
         --  and a held start are the device manager's: it sets up what a
         --  driver needs (its GPIO sink admission) before the driver runs.
         if (Spec.Sink /= CS.No_Sink or else Spec.Start = CS.Held) and then not From_Device_Manager then
            Refuse (CS.Sink_Not_Granted, 0, "only the device manager starts a child held or asks for its sink");
            return;
         end if;
         case Spec.Source is
            when CS.Device_Class =>
               declare
                  Class : constant String := Spec.Value (1 .. Spec.Value_Bytes);
               begin
                  if not From_Device_Manager then
                     Refuse (CS.Class_Not_Granted, 0, "only the device manager starts device-class children");
                     return;
                  end if;
                  for K in 1 .. Startup_Plan.Plan.Class_Total loop
                     declare
                        Block : CCL.Configurations.Class_Block renames Startup_Plan.Plan.Classes (K);
                     begin
                        if Block.Class.Data (1 .. Block.Class.Length) = Class then
                           Class_Line := K;
                        end if;
                     end;
                  end loop;
                  if Class_Line = 0 then
                     Refuse (CS.No_Class_Block, 0, "no (driver-class """ & Class & """ ...) in the startup plan");
                     return;
                  end if;
                  --  The launch value is typed against the child's own
                  --  record once its file is read (Spawn_Attempt).
                  pendingClassLaunch := Startup_Plan.Plan.Classes (Class_Line).Launch;
                  pendingFacts := Spec.Fact_List;
                  pendingFactCount := Spec.Facts;
                  pendingClassLength := Class'Length;
                  pendingClass (1 .. Class'Length) := Class;
               end;
            when CS.Given | CS.No_Parameters =>
               --  The device manager starts device-class children only.
               if From_Device_Manager then
                  Refuse (CS.Not_Granted, 0, "the device manager starts device-class children only");
                  return;
               end if;
               declare
                  Launcher : Launch_State renames Launches.Values (launchIndex (sender));
               begin
                  if admitLaunch (Launcher, Name) /= Named then
                     Refuse (CS.Not_Granted, 0, "its manifest's may_launch does not name it");
                     return;
                  end if;
               end;
               Merged_Length := Spec.Value_Bytes;
               Merged (1 .. Merged_Length) := Spec.Value (1 .. Spec.Value_Bytes);
         end case;

         --  Dependencies: running children of the same supervisor (their
         --  order is what rest-for-one strategies will follow).
         for D in 1 .. Spec.Dependencies loop
            declare
               At_Edge : constant Natural :=
                 (if Edges = null then 0
                  else SE.Find_Child (Edges.all, Edge_Length, Spec.Dependency_List (D)));
            begin
               if At_Edge = 0 or else Edges (At_Edge).Supervisor /= To_Word (sender)
                 or else Edges (At_Edge).Reaping
               then
                  pendingClassLength := 0;
                  Refuse (CS.Dependency_Unknown);
                  return;
               end if;
            end;
         end loop;
         --  Room to record the edge before anything is spawned.
         if not edgeRoom then
            pendingClassLength := 0;
            Refuse (CS.Supervision_Capacity);
            return;
         end if;

         pendingParameters := (others => <>);
         pendingParameters.Length := Merged_Length;
         pendingParameters.Data (1 .. Merged_Length) := Merged (1 .. Merged_Length);
         pendingBindingCount := Spec.Bindings;
         pendingBindings := Spec.Binding_List;
         pendingHold := Spec.Start = CS.Held;
         classRefused := False;
         classLaunchRefusal := Class_Launch_Checks.Merged;
         attenuationRefused := False;
         --  A program's child holds a subset of its supervisor's authority
         --  (as OP_LAUNCH); the device manager's drivers get what their
         --  manifests ask, as from its own launches.
         attenuateFor := (if From_Device_Manager then No_Process else sender);
         --  Never parked: the receipt is bounded. A provider still starting
         --  refuses it (the supervisor may ask again).
         newPID := spawnByName
           (Name, sender,
            approveRender => not From_Device_Manager and then launchIndex (sender) /= 0
                             and then Launches.Values (launchIndex (sender)).Render_Approved,
            readyDeadlineMs => Unplanned_Ready_Deadline,
            mayWait => False, Waits => Waits, Failure => Failure);
         attenuateFor := No_Process;
         pendingParameters := (others => <>);
         pendingBindingCount := 0;
         pendingClassLength := 0;
         pendingClassLaunch := (others => <>);
         pendingFactCount := 0;
         pendingHold := False;
         if Waits /= null then
            Free (Waits);
         end if;
         if newPID = No_Process then
            if classLaunchRefusal /= Class_Launch_Checks.Merged then
               declare
                  Why : constant Class_Launch_Checks.Outcome := classLaunchRefusal;
                  Text : constant String := classLaunchMessage (1 .. classLaunchMessageLength);
               begin
                  classLaunchRefusal := Class_Launch_Checks.Merged;
                  case Why is
                     when Class_Launch_Checks.Facts_Differ | Class_Launch_Checks.Schema_Refused =>
                        Refuse (CS.Facts_Mismatch, 0, Text);
                     when Class_Launch_Checks.Too_Long =>
                        Refuse (CS.Parameters_Too_Long, 0, Text);
                     when Class_Launch_Checks.Not_Typed | Class_Launch_Checks.Merged =>
                        Refuse (CS.Launch_Refused, LA.Launch_Failure'Enum_Rep (LA.Parameters_Rejected), Text);
                  end case;
               end;
            elsif classRefused then
               classRefused := False;
               Refuse (CS.Class_Not_Granted, 0, Name & " does not drive " & Spec.Value (1 .. Spec.Value_Bytes));
            elsif attenuationRefused then
               attenuationRefused := False;
               Refuse (CS.Launch_Refused, LA.Launch_Failure'Enum_Rep (LA.Not_Granted),
                       "it asks for authority its supervisor does not hold");
            else
               Refuse (CS.Launch_Refused, LA.Launch_Failure'Enum_Rep (Failure),
                       CuBit.Launch_Failure_Text.Explanation (Failure));
            end if;
            return;
         end if;

         --  The edge. A fresh identity is never in an edge (KERN-003); one
         --  that is would break the proved invariant, so it is refused.
         if not SE.Fresh (Edges.all, Edge_Length, To_Word (newPID))
           or else Edge_Length >= Edges'Last or else Next_Rank = Unsigned_64'Last
           or else To_Word (newPID) = To_Word (sender)
         then
            declare
               Known : constant Records.Index := Records.Find (newPID);
               Ignore : Unsigned_64;
            begin
               if Known /= 0 and then Valid (Records.Values (Known).Control) then
                  Ignore := killProcess (newPID, Records.Values (Known).Control);
               end if;
            end;
            Refuse (CS.Supervision_Capacity, 0, "the new process could not be recorded");
            return;
         end if;
         SE.Add (Edges.all, Edge_Length, Next_Rank, To_Word (newPID), To_Word (sender), Spec.Restart);
         if Spec.Start = CS.Held then
            declare
               Known : constant Records.Index := Records.Find (newPID);
            begin
               if Known /= 0 then
                  Records.Values (Known).Held := True;
               end if;
            end;
         end if;
         --  Its declared stop grace, for the root stop (PWR-003).
         declare
            Known : constant Records.Index := Records.Find (newPID);
         begin
            if Known /= 0 and then Records.Values (Known).Valid then
               Records.Values (Known).Stop_Grace := Spec.Stop_Grace;
               Records.Values (Known).Stage := Spec.Stage;
            end if;
         end;
         Warnings.Inform
           ("procmgr: start_child " & Name & " pid" & Image (newPID) & " supervised by " &
            processName (sender) & " pid" & Image (sender) & " (restart " & CS.Restart_Name (Spec.Restart) &
            ", stop grace" & Unsigned_32'Image (Spec.Stop_Grace) & " ms" &
            (if Spec.Source = CS.Device_Class
             then ", class " & Spec.Value (1 .. Spec.Value_Bytes) & ":" & Natural'Image (Spec.Facts) & " facts"
             else "") & (if Spec.Start = CS.Held then ", held for its supervisor" else "") & ")");
         if Spec.Sink = CS.No_Sink then
            Answer (REPLY_OK, To_Word (newPID), 0);
            return;
         end if;
         --  The receipt carries the sink: procmgr's own endpoint to the
         --  child (SPAWN's untagged Mailbox), Write and Grant, badged as the
         --  device manager asked. Grant lets it hand a Write-only copy on.
         declare
            Known : constant Records.Index := Records.Find (newPID);
            Reply_Message : Message := NULL_MESSAGE;
            Given : HIPC.Reply_Result;
            Ignore : Unsigned_64;
         begin
            Reply_Message.tag := (label => REPLY_OK, length => CS.Reply_Words, protocol => 0, ipc => <>, reserved => 0);
            Reply_Message.words := [0 => To_Word (newPID), others => 0];
            if Known /= 0 and then Valid (Records.Values (Known).Mailbox) then
               Given := HIPC.Reply_With
                 (sender, Reply_Message,
                  Badged (Records.Values (Known).Mailbox, H.Rights'(Write | Grant => True, others => False),
                          Spec.Sink_Badge));
            end if;
            if Known = 0 or else not Valid (Records.Values (Known).Mailbox)
              or else Given.Outcome = HIPC.Refused
            then
               --  Nothing was answered: the child never runs without its sink.
               if Known /= 0 and then Valid (Records.Values (Known).Control) then
                  Ignore := killProcess (newPID, Records.Values (Known).Control);
               end if;
               Refuse (CS.Sink_Not_Granted, 0, "its sink could not be given");
            elsif Given.Outcome = HIPC.Delivered_Refused then
               Warnings.Warn ("procmgr: start_child " & Name & " pid" & Image (newPID) &
                              ": the supervisor's table refused its sink; the receipt went without it");
            end if;
         end;
      end;
   end handleStartChild;

   ---------------------------------------------------------------------------
   --  Release_Operation (CuBit.Child_Specs): a held child's supervisor
   --  starts it (Run, with the quotas any launch gets) or ends it unrun
   --  (Withdraw), once. Answered at once.
   ---------------------------------------------------------------------------
   procedure releaseChild (sender : Process_ID; msg : Message) is
      Child_Word : constant Unsigned_64 := msg.words (0);
      Child : constant Process_ID := From_Word (Child_Word);

      procedure Answer (Label : Unsigned_32; Word0 : Unsigned_64) is
         Reply_Message : Message := NULL_MESSAGE;
         Ignore : Unsigned_64;
      begin
         Reply_Message.tag := (label => Label, length => 1, protocol => 0, ipc => <>, reserved => 0);
         Reply_Message.words := [0 => Word0, others => 0];
         Ignore := reply (sender, Reply_Message);
      end Answer;

      procedure Refuse (Why : CS.Release_Refusal) is
      begin
         Warnings.Warn ("procmgr: release of pid" & Image (Child) & " by " & processName (sender) &
                        " refused: " & CS.Release_Refusal_Name (Why));
         Answer (REPLY_ERR, CS.Release_Refusal'Enum_Rep (Why));
      end Refuse;

      At_Edge : Natural;
      Known : Records.Index;
   begin
      if msg.tag.length /= CS.Release_Words or else not CS.Is_Decision (msg.words (1))
        or else not CuBit.Process_IDs.Is_Process (Child)
      then
         Refuse (CS.Malformed_Release);
         return;
      end if;
      At_Edge := (if Edges = null then 0 else SE.Find_Child (Edges.all, Edge_Length, Child_Word));
      if At_Edge = 0 or else Edges (At_Edge).Supervisor /= To_Word (sender) or else Edges (At_Edge).Reaping then
         Refuse (CS.Not_Supervised);
         return;
      end if;
      Known := Records.Find (Child);
      if Known = 0 or else not Records.Values (Known).Valid or else not Records.Values (Known).Held
        or else Records.Values (Known).Resumed
      then
         Refuse (CS.Not_Held);
         return;
      end if;
      Records.Values (Known).Held := False;
      case CS.Decision_Of (msg.words (1)) is
         when CS.Withdraw =>
            declare
               Ignore : Unsigned_64;
            begin
               Ignore := killProcess (Child, Records.Values (Known).Control);
            end;
            Warnings.Inform ("procmgr: held pid" & Image (Child) & " withdrawn by " & processName (sender));
            Answer (REPLY_OK, Child_Word);
         when CS.Run =>
            declare
               Identity : constant String :=
                 Records.Values (Known).Identity (1 .. Records.Values (Known).Identity_Length);
               maxFrames, cpuQuotaUs, cpuPeriodUs : Unsigned_32 := 0;
               Ignore : Unsigned_64;
            begin
               if Identity'Length > 0 then
                  queryConfigQuota (Identity, maxFrames, cpuQuotaUs, cpuPeriodUs);
               end if;
               --  Said before it runs, so the line precedes anything it says.
               Warnings.Inform ("procmgr: held pid" & Image (Child) & " released by " & processName (sender));
               if not HC.Resume
                 (Child, Records.Values (Known).Control, Unsigned_64 (maxFrames),
                  Unsigned_64 (cpuQuotaUs) or Shift_Left (Unsigned_64 (cpuPeriodUs), 32))
               then
                  Ignore := killProcess (Child, Records.Values (Known).Control);
                  Refuse (CS.Resume_Failed);
                  return;
               end if;
               Records.Values (Known).Resumed := True;
               Answer (REPLY_OK, Child_Word);
            end;
      end case;
   end releaseChild;

   ---------------------------------------------------------------------------
   --  A process ended (procmgr's EVENT_CHILD_EXIT): its own supervision
   --  edge goes (its supervisor got the kernel's exit event itself), and
   --  if it supervised children, they are reaped: procmgr stops each with
   --  the process authority it holds, and their own ends cascade to
   --  theirs. Called before Exited's record is forgotten (its name).
   ---------------------------------------------------------------------------
   procedure superviseEnded (Exited : Process_ID; Abnormal : Boolean) is
      Word : constant Unsigned_64 := To_Word (Exited);
      At_Edge : Natural;
   begin
      -- Called only after the kernel child-exit receipt is validated below.
      -- A live recipient or an unconfirmed stop never releases this binding.
      if CMOS_Recipient = Exited then CMOS_Recipient := No_Process; end if;
      if Edges = null or else Edge_Length = 0 then
         return;
      end if;
      At_Edge := SE.Find_Child (Edges.all, Edge_Length, Word);
      if At_Edge /= 0 then
         declare
            Item : constant SE.Edge := Edges (At_Edge);
            History : SE.Restart_History;
            Decision : SE.Restart_Decision;
            use type SE.Restart_Decision;
         begin
            --  Restart strategies arrive in slice 2: the decision is
            --  recorded in the log, a temporary child is never restarted.
            SE.Decide (Item.Restart, 1, 1, Abnormal, Item.Reaping, CuBit.Monotonic.Milliseconds,
                       History, Decision);
            Warnings.Inform
              ("procmgr: supervised child " & processName (Exited) & " pid" & Image (Exited) &
               (if Item.Reaping
                then " reaped (its supervisor pid" & Image (From_Word (Item.Supervisor)) & " ended)"
                else " ended; its supervisor pid" & Image (From_Word (Item.Supervisor)) & " has its exit event") &
               (if Decision = SE.Stay_Ended then ""
                else " (restart " & CS.Restart_Name (Item.Restart) &
                     ": not restarted, restarts arrive with SUP-001 slice 2)"));
            SE.Remove (Edges.all, Edge_Length, Next_Rank, At_Edge);
         end;
      end if;
      --  Its children, if it supervised any.
      for K in 1 .. Edge_Length loop
         if Edges (K).Supervisor = Word and then not Edges (K).Reaping then
            SE.Mark_Reaping (Edges.all, Edge_Length, Next_Rank, K);
            declare
               Child : constant Process_ID := From_Word (Edges (K).Child);
               Known : constant Records.Index := Records.Find (Child);
               Killed : Boolean := False;
            begin
               if Known /= 0 and then Records.Values (Known).Valid
                 and then Valid (Records.Values (Known).Control)
               then
                  Killed := killProcess (Child, Records.Values (Known).Control) = 0;
               end if;
               Warnings.Inform
                 ("procmgr: supervisor " & processName (Exited) & " pid" & Image (Exited) &
                  " ended; reaping its child " & processName (Child) & " pid" & Image (Child) &
                  (if Killed then "" else " (it could not be stopped)"));
            end;
         end if;
      end loop;
   end superviseEnded;

   ---------------------------------------------------------------------------
   --  The root stop (PWR-003 slice 1, CuBit.System_Stops): the power manager
   --  asks; procmgr stops what it started, one process at a time in reverse
   --  startup order (Shutdown_Sequences decides; this drives it), each within
   --  its declared grace, then has the filesystem commit and devmgr stop its
   --  drivers, and answers with the typed outcome. Calls go down only:
   --  procmgr never waits on the power manager.
   ---------------------------------------------------------------------------
   package SQ renames Shutdown_Sequences;
   package STP renames CuBit.System_Stops;
   use type SQ.Action, SQ.Unit_State, SQ.Phase;

   --  The units, on the heap, exactly as many as procmgr had running; their
   --  names alongside, kept for the log after a process's record is gone.
   type Unit_Block is access SQ.Unit_Array;
   type Unit_Name is record
      Text   : String (1 .. PO.Identity_Bytes) := [others => ' '];
      Length : Natural range 0 .. PO.Identity_Bytes := 0;
   end record;
   type Name_Array is array (Positive range <>) of Unit_Name;
   type Name_Block is access Name_Array;
   type Flag_Array is array (Positive range <>) of Boolean;
   type Flag_Block is access Flag_Array;

   Stop_Sequence : SQ.Sequence;
   Stop_Units : Unit_Block := null;
   Stop_Names : Name_Block := null;
   Stop_Logged : Flag_Block := null;
   Stop_Length : Natural := 0;
   Stop_Reply : H.Reply := H.No_Reply;
   Stop_Request_Id : Unsigned_64 := 0;
   Stop_Storage_Ms : Unsigned_64 := 0;
   Stop_Began : Unsigned_64 := 0;
   Volumes_Committed, Volumes_Failed : Natural := 0;
   Driver_Counts : Unsigned_64 := 0;
   --  The request being served (Stop_Operation or Stop_Apps_Operation), and
   --  the firmware step's provider whose protected tail is kept (none for
   --  Halt).
   Stop_Label : Unsigned_32 := STP.Stop_Operation;
   Stop_Keep_Tail : Boolean := False;
   --  How long devmgr has to answer each of procmgr's calls (it answers at
   --  once; this bounds a devmgr that does not).
   Driver_Answer_Ms : constant := 1_000;

   function stopNow return SQ.Time is
     (Unsigned_64'Min (CuBit.Monotonic.Milliseconds, SQ.Latest_Time));

   function Stage_Name (Stage : SQ.Stop_Stage) return String is
     (case Stage is
        when SQ.Apps => "app", when SQ.Services => "service", when SQ.Power_Tail => "power tail");

   function Outcome_Name (State : SQ.Unit_State) return String is
     (case State is
        when SQ.Stopped_Clean  => "STOPPED_CLEAN",
        when SQ.Exited_Failing => "EXITED_FAILING",
        when SQ.Ended_Early    => "ENDED_EARLY",
        when SQ.Forced         => "FORCED",
        when SQ.Force_Failed   => "NOT_ENDED",
        when SQ.Kept           => "KEPT",
        when others            => "OPEN");

   function unitName (K : Positive) return String is
     (Stop_Names (K).Text (1 .. Stop_Names (K).Length));

   --  One line per unit as it is decided: who, how, how long, its grace.
   procedure reportUnits is
   begin
      for K in 1 .. Stop_Length loop
         declare
            U : SQ.Unit renames Stop_Units (K);
         begin
            if not Stop_Logged (K) and then U.State in SQ.Decided then
               Stop_Logged (K) := True;
               declare
                  Line : constant String :=
                    "procmgr: shutdown: " & unitName (K) & " pid" & Image (From_Word (U.Process)) & " " &
                    Outcome_Name (U.State) &
                    (if U.State = SQ.Ended_Early then ""
                     else " after" & Unsigned_64'Image (U.Decided_At - U.Stop_At) & " ms") &
                    " (" & Stage_Name (U.Stage) & ", grace" & Unsigned_32'Image (U.Grace) & " ms" &
                    (if U.State in SQ.Forced | SQ.Force_Failed and then not U.Stop_Taken
                     then ", Stop not taken" else "") & ")";
               begin
                  if U.State in SQ.Forced | SQ.Force_Failed | SQ.Exited_Failing then
                     Warnings.Warn (Line);
                  else
                     Warnings.Inform (Line);
                  end if;
               end;
            end if;
         end;
      end loop;
   end reportUnits;

   --  logstore keeps running to the end (its unit is Kept: the system's
   --  final lines, procmgr's own among them, go there), but its log file
   --  is on the storage about to stop: it persists what it holds and
   --  closes the file first (CuBit.Log_Sources.Detach_Storage), within
   --  Log_Detach_Ms.
   Log_Detach_Ms : constant := 5_000;
   procedure detachLogStorage is
      Collector : constant CuBit.Handles.Endpoint := Logstore_Collector;
      Via : CuBit.Handles.Endpoint;
      Request : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Ignore : HIPC.Close_Result;
   begin
      if not Valid (Collector) then
         return;
      end if;
      Via := HC.Derive (Badged (Collector, H.Read_Write,
                                CuBit.Log_Sources.Registrar_Tag
                                  (CuBit.Log_Sources.Process_Manager_Issuance)));
      if not Valid (Via) then
         Warnings.Warn ("procmgr: shutdown: logstore not asked to detach its storage (no endpoint)");
         return;
      end if;
      Request.tag := (label => CuBit.Log_Sources.Detach_Storage, length => 0,
                      protocol => 0, ipc => <>, reserved => 0);
      Tag := capCall (Via, Request, CuBit.Messages.Deadline_After (Log_Detach_Ms));
      Ignore := HIPC.Close (Via);
      Warnings.Inform
        ("procmgr: shutdown: logstore detached its storage: " &
         (if Ending_Of (Tag) /= Call_Answered then "no answer"
          elsif Request.words (0) = 1 then "log file persisted and closed"
          else "no log file open"));
   end detachLogStorage;

   --  The filesystem's commit, within the request's storage deadline.
   procedure commitStorage (Result : out STP.Storage_Outcome) is
      Request : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      Request.tag := (label => STP.Storage_Operation, length => STP.Storage_Request_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words (STP.Request_Word) := Stop_Request_Id;
      Warnings.Inform ("procmgr: shutdown: the filesystem commits (deadline" &
                       Unsigned_64'Image (Stop_Storage_Ms) & " ms)");
      --  ABI-002: procmgr's lines so far reach logstore before the
      --  filesystem's (the console mirror keeps the order of the steps).
      CuBit.Console_Route.Flush;
      Tag := capCall (PH.Filesystem, Request, CuBit.Messages.Deadline_After (Stop_Storage_Ms));
      if Ending_Of (Tag) = Call_Timed_Out then
         Result := STP.Storage_Timed_Out;
      elsif Ending_Of (Tag) = Call_Answered and then Tag.label = STP.Storage_Operation
        and then Request.words (STP.Reply_Request_Word) = Stop_Request_Id
        and then STP.Valid_Storage (Request.words (STP.Storage_Status_Word))
        and then Request.words (STP.Storage_Status_Word) /=
                   STP.Storage_Outcome'Enum_Rep (STP.Not_Asked)
      then
         Result := STP.Storage_Outcome'Enum_Val (Request.words (STP.Storage_Status_Word));
         Volumes_Committed := STP.Field_Of (Request.words (STP.Volumes_Word), 0);
         Volumes_Failed := STP.Field_Of (Request.words (STP.Volumes_Word), 1);
      else
         Result := STP.Storage_Refused;
      end if;
      Warnings.Inform ("procmgr: shutdown: storage " & STP.Name (Result) & " (" &
                       Natural'Image (Volumes_Committed) & " committed," &
                       Natural'Image (Volumes_Failed) & " failed)");
   end commitStorage;

   --  devmgr's drivers: a receipt at once, then polls.
   procedure callDevmgr (Label : Unsigned_32; Length : Unsigned_8; Word1, Word2 : Unsigned_64;
                         Answered : out Boolean; Reply : out Message) is
      Tag : MessageTag;
   begin
      Reply := NULL_MESSAGE;
      Reply.tag := (label => Label, length => Length, protocol => 0, ipc => <>, reserved => 0);
      Reply.words (STP.Request_Word) := Stop_Request_Id;
      Reply.words (1) := Word1;
      Reply.words (2) := Word2;
      --  ABI-002: procmgr's lines so far reach logstore before devmgr's.
      CuBit.Console_Route.Flush;
      Tag := capCall (PH.Device_Manager, Reply, CuBit.Messages.Deadline_After (Driver_Answer_Ms));
      Answered := Ending_Of (Tag) = Call_Answered and then Tag.label = Label
        and then Reply.words (STP.Reply_Request_Word) = Stop_Request_Id;
   end callDevmgr;

   procedure answerStop is
      Reply : Message := NULL_MESSAGE;
      Ignore : Boolean;
      Clean : constant Natural := SQ.Count (Stop_Units.all, Stop_Length, SQ.Stopped_Clean);
      Failing : constant Natural := SQ.Count (Stop_Units.all, Stop_Length, SQ.Exited_Failing);
      Forced : constant Natural := SQ.Count (Stop_Units.all, Stop_Length, SQ.Forced);
      Not_Ended : constant Natural := SQ.Count (Stop_Units.all, Stop_Length, SQ.Force_Failed);
      Early : constant Natural := SQ.Count (Stop_Units.all, Stop_Length, SQ.Ended_Early);
      Kept : constant Natural := SQ.Count (Stop_Units.all, Stop_Length, SQ.Kept);
      Driver_Clean : constant Natural := STP.Field_Of (Driver_Counts, 0);
      Driver_Failing : constant Natural := STP.Field_Of (Driver_Counts, 1);
      Driver_Forced : constant Natural := STP.Field_Of (Driver_Counts, 2);
      Driver_Running : constant Natural := STP.Field_Of (Driver_Counts, 3);
   begin
      pragma Assert (Stop_Sequence.State = SQ.Finished);
      Warnings.Inform
        ("procmgr: shutdown: " & (if Stop_Sequence.Apps_Only then "apps " else "") &
         "stopped in" & Unsigned_64'Image (CuBit.Monotonic.Milliseconds - Stop_Began) &
         " ms:" & Natural'Image (Clean) & " clean," & Natural'Image (Failing) & " failing," &
         Natural'Image (Forced) & " forced," & Natural'Image (Not_Ended) & " not ended," &
         Natural'Image (Early) & " ended early," & Natural'Image (Kept) & " kept; storage " &
         STP.Name (Stop_Sequence.Storage) & "; drivers " & STP.Name (Stop_Sequence.Drivers) & " (" &
         Natural'Image (Driver_Clean) & " clean," & Natural'Image (Driver_Failing) & " failing," &
         Natural'Image (Driver_Forced) & " forced," & Natural'Image (Driver_Running) & " still running)");
      Reply.tag := (label => Stop_Label, length => STP.Stop_Reply_Words,
                    protocol => 0, ipc => <>, reserved => 0);
      Reply.words (STP.Reply_Request_Word) := Stop_Request_Id;
      Reply.words (STP.Result_Word) := STP.Stop_Result'Enum_Rep (STP.Stopped);
      Reply.words (STP.Processes_Word) := STP.Counts_Word (Clean, Failing, Forced, Not_Ended);
      Reply.words (STP.Stages_Word) := STP.Stages_Word_Of
        (Stop_Sequence.Storage, Volumes_Committed, Volumes_Failed, Stop_Sequence.Drivers,
         Driver_Clean + Driver_Failing, Driver_Forced);
      --  The final lines reach logstore before the requester hears the
      --  stop (power.svc then asks for S5).
      CuBit.Console_Route.Flush;
      if H.Valid (Stop_Reply) then
         Ignore := HIPC.Answer (Stop_Reply, Reply);
      end if;
      Stop_Reply := H.No_Reply;
   end answerStop;

   --  Take the action First asks for, and every action that follows at once.
   procedure driveShutdown (First : SQ.Action) is
      Next : SQ.Action := First;
   begin
      while Next /= SQ.Nothing loop
         reportUnits;
         case Next is
            when SQ.Nothing =>
               null;
            when SQ.Send_Stop =>
               declare
                  K : constant Positive := Stop_Sequence.Current;
                  U : SQ.Unit renames Stop_Units (K);
                  Known : constant Records.Index := Records.Find (From_Word (U.Process));
                  --  With procmgr's process authority: a Start_Child
                  --  child's kernel parent is its supervisor, not procmgr.
                  Answer : constant Unsigned_64 := HC.Send_Control
                    (From_Word (U.Process),
                     CuBit.Control_Events.Control_Kind'Enum_Rep (CuBit.Control_Events.Stop),
                     (if Known /= 0 and then Records.Values (Known).Valid
                      then Records.Values (Known).Control else CuBit.Handles.No_Process));
               begin
                  SQ.Stop_Sent (Stop_Sequence, Stop_Units.all, Stop_Length, Answer = 0);
                  Warnings.Inform ("procmgr: shutdown: stopping " & unitName (K) & " pid" &
                                   Image (From_Word (U.Process)) & " (" & Stage_Name (U.Stage) &
                                   ", grace" & Unsigned_32'Image (U.Grace) & " ms)" &
                                   (if Answer = 0 then "" else "; Stop not taken"));
               end;
               Next := SQ.Nothing;
            when SQ.End_Unit =>
               declare
                  K : constant Positive := Stop_Sequence.Current;
                  Process : constant Process_ID := From_Word (Stop_Units (K).Process);
                  Known : constant Records.Index := Records.Find (Process);
                  Accepted : constant Boolean :=
                    Known /= 0 and then Records.Values (Known).Valid
                    and then Valid (Records.Values (Known).Control)
                    and then killProcess (Process, Records.Values (Known).Control) = 0;
               begin
                  Warnings.Warn ("procmgr: shutdown: " & unitName (K) & " pid" & Image (Process) &
                                 " did not end within its grace of" & Unsigned_32'Image (Stop_Units (K).Grace) &
                                 " ms; ending it" & (if Accepted then "" else " (refused)"));
                  SQ.End_Sent (Stop_Sequence, Stop_Units.all, Stop_Length, Accepted, stopNow, Next);
               end;
            when SQ.Commit_Storage =>
               declare
                  Result : STP.Storage_Outcome;
               begin
                  detachLogStorage;
                  commitStorage (Result);
                  SQ.Storage_Done (Stop_Sequence, Stop_Units.all, Stop_Length, Result, stopNow, Next);
               end;
            when SQ.Ask_Drivers =>
               declare
                  Reply : Message;
                  Answered : Boolean;
               begin
                  --  devmgr keeps its own Power_Tail children for a power-off
                  --  (it declared them when it started them).
                  callDevmgr (STP.Driver_Stop_Operation, STP.Driver_Request_Words,
                              Unsigned_64 (Stop_Sequence.Driver_Grace),
                              (if Stop_Keep_Tail then STP.Keep_Tail else STP.Stop_Tail),
                              Answered, Reply);
                  Answered := Answered and then Reply.words (STP.Receipt_Word) =
                    STP.Driver_Receipt'Enum_Rep (STP.Accepted);
                  Warnings.Inform ("procmgr: shutdown: devmgr stops its drivers" &
                                   (if Answered
                                    then " (" & Unsigned_64'Image (Reply.words (STP.Asked_Word)) &
                                         " asked, grace" & Unsigned_32'Image (Stop_Sequence.Driver_Grace) & " ms)"
                                    else ": refused or no answer"));
                  SQ.Drivers_Asked (Stop_Sequence, Stop_Units.all, Stop_Length, Answered, stopNow, Next);
               end;
            when SQ.Poll_Drivers =>
               declare
                  Reply : Message;
                  Answered : Boolean;
               begin
                  callDevmgr (STP.Driver_Status_Operation, STP.Driver_Status_Words, 0, 0, Answered, Reply);
                  if Answered then
                     Driver_Counts := Reply.words (STP.Driver_Counts_Word);
                  end if;
                  SQ.Drivers_Polled
                    (Stop_Sequence, Stop_Units.all, Stop_Length,
                     Answered and then Reply.words (STP.Phase_Word) =
                       STP.Driver_Phase'Enum_Rep (STP.Finished),
                     stopNow, Next);
               end;
            when SQ.Answer =>
               answerStop;
               Next := SQ.Nothing;
         end case;
      end loop;
      reportUnits;
   end driveShutdown;

   --  A process ended: the stop moves on when it was the one stopping.
   procedure shutdownExited (Exited : Process_ID; Clean : Boolean) is
      Next : SQ.Action;
   begin
      if SQ.Active (Stop_Sequence) and then Exited /= No_Process then
         SQ.Exited (Stop_Sequence, Stop_Units.all, Stop_Length, To_Word (Exited), Clean, stopNow, Next);
         driveShutdown (Next);
      end if;
   end shutdownExited;

   --  Time passed: graces, end deadlines, devmgr's polls.
   procedure shutdownTick is
      Next : SQ.Action;
   begin
      if SQ.Active (Stop_Sequence) then
         SQ.Tick (Stop_Sequence, Stop_Units.all, Stop_Length, stopNow, Next);
         driveShutdown (Next);
      end if;
   end shutdownTick;

   --  STP.Stop_Operation: refused at once, typed; admitted, the reply is
   --  kept and given when the stop ends.
   --  Apps_Only: STP.Stop_Apps_Operation (the Apps stage alone).
   procedure handleSystemStop (sender : Process_ID; msg : Message; Apps_Only : Boolean) is
      Power_Manager : constant Process_ID := Registered_Driver (CuBit.Power_Requests.Service_Role);

      procedure Refuse (Why : STP.Stop_Result; Detail : String) is
         Answer : Message := NULL_MESSAGE;
         Ignore : Unsigned_64;
      begin
         Warnings.Warn ("procmgr: shutdown refused for pid" & Image (sender) & ": " & STP.Name (Why) &
                        " (" & Detail & ")");
         Answer.tag := (label => msg.tag.label, length => STP.Stop_Reply_Words,
                        protocol => 0, ipc => <>, reserved => 0);
         Answer.words (STP.Reply_Request_Word) := msg.words (STP.Request_Word);
         Answer.words (STP.Result_Word) := STP.Stop_Result'Enum_Rep (Why);
         Ignore := reply (sender, Answer);
      end Refuse;
   begin
      if sender = No_Process or else Power_Manager = No_Process or else sender /= Power_Manager then
         Refuse (STP.Not_Approved, "only the registered power manager may stop the system");
         return;
      elsif not STP.Request_Valid (Unsigned_64 (msg.tag.length), msg.words (0), msg.words (1),
                                   msg.words (2), msg.words (3))
      then
         Refuse (STP.Malformed, "request id, final step or a grace out of range");
         return;
      --  One stop: Stop_Apps_Operation may come first, then one
      --  Stop_Operation once the apps' stop has ended.
      elsif Shutting_Down
        and then (Apps_Only or else not Stop_Sequence.Apps_Only
                  or else Stop_Sequence.State /= SQ.Finished)
      then
         Refuse (STP.Already_Stopping, "a stop is in progress");
         return;
      end if;
      declare
         Final : constant STP.Final_Step := STP.To_Final (msg.words (STP.Final_Word));
         Undeclared : constant SQ.Grace_Ms :=
           SQ.Grace_Ms (STP.Undeclared_Of (msg.words (STP.Grace_Word)));
         --  A power-off keeps the declared Power_Tail for the firmware step.
         Keep_Tail : constant Boolean := STP."=" (Final, STP.Power_Off);
         Listed : constant Unsigned_64 :=
           HC.Process_List (PH.Process_List_Object, processList'Address, Process_List_Bytes);
         Count : constant Natural :=
           (if Listed = Unsigned_64'Last then 0
            else Natural (Unsigned_64'Min (Listed, Process_List_Bytes / Process_Entry_Bytes)));
         Total : Natural := 0;
         Saved : HIPC.Saved_Reply;
         Next : SQ.Action;

         --  A process procmgr started and resumed: one of its units.
         function Candidate (I : Natural) return Records.Index is
            Known : constant Records.Index :=
              Records.Find (CuBit.Process_List.Get (processList'Address, I).Identity);
         begin
            return (if Known /= 0 and then Records.Values (Known).Valid and then Records.Values (Known).Resumed
                    then Known else 0);
         end Candidate;
         --  Whether Process is a plan entry, and which.
         function Entry_Of (Process : Process_ID) return Natural is
         begin
            for N in 1 .. Startup_Plan.Plan.Launch_Count loop
               if Plan_PIDs (N) = Process then
                  return N;
               end if;
            end loop;
            return 0;
         end Entry_Of;

         --  The stage a process stops in: its declared one.
         function Stage_Of (Known : Records.Index) return SQ.Stop_Stage is
           (case Records.Values (Known).Stage is
              when CuBit.Child_Specs.Apps       => SQ.Apps,
              when CuBit.Child_Specs.Services   => SQ.Services,
              when CuBit.Child_Specs.Power_Tail => SQ.Power_Tail);

         function Wanted (I : Natural) return Records.Index is
            Known : constant Records.Index := Candidate (I);
         begin
            return (if Known /= 0 and then (not Apps_Only or else SQ."=" (Stage_Of (Known), SQ.Apps))
                    then Known else 0);
         end Wanted;
      begin
         for I in 0 .. Count - 1 loop
            if Wanted (I) /= 0 then
               Total := Total + 1;
            end if;
         end loop;
         begin
            Stop_Units := new SQ.Unit_Array (1 .. Natural'Max (1, Total));
            Stop_Names := new Name_Array (1 .. Natural'Max (1, Total));
            Stop_Logged := new Flag_Array'(1 .. Natural'Max (1, Total) => False);
         exception
            when Storage_Error =>
               Refuse (STP.Unanswerable, "no memory for the stop's records");
               return;
         end;
         Saved := HIPC.Save_Reply;
         if not Saved.Saved then
            Refuse (STP.Unanswerable, "its reply could not be kept");
            return;
         end if;
         Stop_Reply := Saved.Handle;
         Stop_Label := msg.tag.label;
         Stop_Keep_Tail := Keep_Tail;
         Stop_Request_Id := msg.words (STP.Request_Word);
         Stop_Storage_Ms := STP.Storage_Of (msg.words (STP.Grace_Word));
         Stop_Began := CuBit.Monotonic.Milliseconds;
         Shutting_Down := True;
         Plan_Running := False;
         Stop_Length := 0;
         for I in 0 .. Count - 1 loop
            declare
               Known : constant Records.Index := Wanted (I);
            begin
               if Known /= 0 and then Stop_Length < Total then
                  declare
                     Process : constant Process_ID := CuBit.Process_List.Get (processList'Address, I).Identity;
                     R : Process_Record renames Records.Values (Known);
                     Number : constant Natural := Entry_Of (Process);
                     U : SQ.Unit;
                  begin
                     U.Process := To_Word (Process);
                     --  Its declared stage; within it, reverse launch order.
                     U.Stage := Stage_Of (Known);
                     U.Key := R.Sequence;
                     U.Grace :=
                       (if Number /= 0 and then Startup_Plan.Plan.Launches (Number).Has_Stop_Grace
                        then SQ.Grace_Ms (Startup_Plan.Plan.Launches (Number).Stop_Grace_Ms)
                        elsif Number = 0 and then R.Stop_Grace in SQ.Grace_Ms then R.Stop_Grace
                        else Undeclared);
                     --  logstore is never stopped by a system stop: it
                     --  takes the final lines (drivers', procmgr's, S5)
                     --  until the machine stops (detachLogStorage).
                     U.State :=
                       (if Process = sender or else (Keep_Tail and then SQ."=" (U.Stage, SQ.Power_Tail))
                          or else (not Apps_Only and then Process = Registered_Driver (DRIVER_LOGSTORE))
                        then SQ.Kept else SQ.Pending);
                     Stop_Length := Stop_Length + 1;
                     Stop_Units (Stop_Length) := U;
                     Stop_Names (Stop_Length).Length := R.Identity_Length;
                     Stop_Names (Stop_Length).Text := R.Identity;
                     if U.State = SQ.Kept then
                        Warnings.Inform ("procmgr: shutdown: keeping " & unitName (Stop_Length) & " pid" &
                                         Image (Process) &
                                         (if Process = sender then " (the requester)"
                                          elsif Process = Registered_Driver (DRIVER_LOGSTORE)
                                          then " (logstore: the final lines)"
                                          else " (Power_Tail: the firmware step)"));
                     end if;
                  end;
               end if;
            end;
         end loop;
         Warnings.Inform
           ("procmgr: shutdown requested by " & processName (sender) & " pid" & Image (sender) & ": " &
            (case Final is when STP.Power_Off => "power off", when STP.Halt => "halt") &
            (if Apps_Only then ", apps first" else "") & ";" &
            Natural'Image (Stop_Length) & " processes (undeclared grace" & Unsigned_32'Image (Undeclared) &
            " ms, storage deadline" & Unsigned_64'Image (Stop_Storage_Ms) & " ms, drivers' grace" &
            Unsigned_64'Image (msg.words (STP.Driver_Word)) & " ms)");
         SQ.Begin_Stop (Stop_Sequence, Stop_Units.all, Stop_Length,
                        SQ.Grace_Ms (msg.words (STP.Driver_Word)), Apps_Only, stopNow, Next);
         driveShutdown (Next);
      end;
   end handleSystemStop;

   ---------------------------------------------------------------------------
   --  resumeParked
   --  Look at every parked launch: one whose required provider ended or
   --  missed its deadline is refused, typed; one whose waits all ended is
   --  started (a plan entry, then the plan's next entries) or its request
   --  served again, answering through its saved reply.
   ---------------------------------------------------------------------------
   procedure resumeParked is
      Now : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
      Previous : Parked_Access := null;
      Item : Parked_Access := Parked_First;
      Following : Parked_Access;
      Refusal : Provider_State;
      Refused_At : Natural;
      Pending : Boolean;
      Resume_Plan : Boolean := False;
   begin
      while Item /= null loop
         Following := Item.Next;
         Refused_At := 0;
         Refusal := Provider_Registered;
         Pending := False;
         for I in Item.Waits'Range loop
            if not Item.Waits (I).Resolved then
               declare
                  State : constant Provider_State := awaitedState (Item.Waits (I), Now);
               begin
                  if State = Provider_Starting then
                     Pending := True;
                  elsif State in Wait_Refusal and then Item.Waits (I).Required then
                     Refusal := State;
                     Refused_At := I;
                     exit;
                  else
                     --  Registered; or an optional entry it starts without.
                     Item.Waits (I).Resolved := True;
                  end if;
               end;
            end if;
         end loop;
         if Refused_At /= 0 or else not Pending then
            --  Done waiting: off the list first, so serving it (which may
            --  park other launches) never sees it.
            if Previous = null then
               Parked_First := Following;
            else
               Previous.Next := Following;
            end if;
            if Parked_Last = Item then
               Parked_Last := Previous;
            end if;
            if Refused_At /= 0 then
               reportWaitRefusal
                 (Item.Name, Item.Waits (Refused_At).Label, Item.Waits (Refused_At).Role, Refusal);
            end if;
            case Item.Kind is
               when Plan_Launch =>
                  if Refused_At /= 0 then
                     debugPrint ("procmgr: init spawn failed: " & Item.Name & ": " &
                                 CuBit.Launch_Failure_Text.Explanation
                                   (CuBit.Launch_Arguments.Service_Unavailable) & LF);
                  else
                     declare
                        Again : Awaited_Access;
                     begin
                        launchPlanEntry (Item.Entry_Number, False, Again);
                     end;
                  end if;
                  Plan_Done := Item.Entry_Number;
                  Resume_Plan := True;
               when Launch_Request =>
                  Deferred_Answer := Item.Answer;
                  if Refused_At /= 0 then
                     answerRequest
                       (Item.Sender, REPLY_ERR, CuBit.Launch_Arguments.Launch_Failure'Enum_Rep
                          (CuBit.Launch_Arguments.Service_Unavailable));
                  else
                     handleLaunch (Item.Sender, Item.Request);
                  end if;
                  --  Every path answers; never leave the requester waiting.
                  if servingDeferred then
                     answerRequest
                       (Item.Sender, REPLY_ERR, CuBit.Launch_Arguments.Launch_Failure'Enum_Rep
                          (CuBit.Launch_Arguments.Spawn_Failed));
                  end if;
            end case;
            Free (Item.Waits);
            Free (Item);
         else
            Previous := Item;
         end if;
         Item := Following;
      end loop;
      if Resume_Plan then
         continuePlan;
      end if;
   end resumeParked;

   ---------------------------------------------------------------------------
   --  Main variables
   ---------------------------------------------------------------------------
   sender : Process_ID;
   received : Boolean;
   Route : Handle_Admission.Route;
   Awaiting_Logstore : Boolean := False;
   msg    : Message;

begin
   --  ABI-002: this process's debug text goes into its logstore ring, never
   --  the kernel console. It starts before logstore: the lines wait in the
   --  route's queue until logstore is known (Warnings binds it), and the
   --  registrar channel, opened first, names this process to logstore.
   CuBit.Console_Route.Route_Awaiting;
   Sources.Started (Own_Process, "com.cubit.procmgr", "procmgr.svc");
   debugPrint ("procmgr: starting..." & LF);
   if not PH.Valid then
      --  Without devmgr's handles procmgr can start nothing.
      debugPrint ("procmgr: launch configuration missing its handles; stopping" & LF);
      declare
         ignore : Unsigned_64;
      begin
         ignore := syscall (SYSCALL_EXIT, 1);
      end;
      return;
   end if;
   setFixedProviders;

   --  Register as DRIVER_PROCMGR
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_PROCMGR, PH.Registration);
   end;

   -- Cache CMOS at the bootstrap rendezvous, before OP_READY/startPlan.
   -- devmgr is receiving here, not synchronously launching a child through us.
   -- No later ACPI or driver launch makes this handover request.
   declare
      Request : Message := NULL_MESSAGE;
      Received : HIPC.Carried;
      Tag : MessageTag;
      use type H.ABI.Kind;
   begin
      Request.tag := (label => CMOS_Wire.Request_Label, length => CMOS_Wire.Request_Words,
        protocol => 0, ipc => <>, reserved => 0);
      Tag := HIPC.Call_Accepting (PH.Ready, Request, Deadline_After (1_000), Received);
      declare
         Taken : constant HIPC.Accepted_IO_Ports := HIPC.Accept_IO_Ports (Received);
         Actual : constant HIPC.Description :=
           (if Taken.OK then HIPC.Describe (Taken.Value) else (Current => False, Why => H.ABI.Invalid_Handle));
         Frame : constant CMOS_Wire.Decoded := CMOS_Wire.Decode
           ([Request.words (0), Request.words (1), Request.words (2), Request.words (3)]);
      begin
         -- Retain every received handle until process teardown, even on bad
         -- metadata. Invalid carriage is never installed or used for access.
         if Taken.OK then CMOS_Boot_Ports := Taken.Value; end if;
         if Tag.label = CMOS_Wire.Reply_Label and then Tag.length = CMOS_Wire.Reply_Words
           and then Tag.protocol = 0 and then Frame.Valid and then Actual.Current
           and then Actual.Kind = H.ABI.IO_Ports and then Actual.Object = CMOS_Wire.First_Port
           and then Actual.Parameter = CMOS_Wire.Port_Count (Frame.Value.Device)
           and then Actual.Held = Read_Write_Grant
         then
            CMOS_Boot_Receipt := Frame.Value; CMOS_Boot_Valid := True;
            debugPrint ("procmgr: cmos bootstrap receipt cached" & LF);
         else debugPrint ("procmgr: optional cmos handover unavailable" & LF);
         end if;
      end;
   end;

   --  Signal devmgr that we are ready
   declare
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      ignore : MessageTag;
   begin
      --  This is intentionally before the synchronous ready handoff.  The
      --  firmware diagnostic panel retains the bounded bootstrap line, so a
      --  headless physical boot distinguishes "blocked in handoff" from
      --  "resumed and failed during bootstrap" without relying on serial.
      debugPrint ("procmgr: bootstrap handoff sent" & LF);
      ignore := capSend (PH.Ready,
         (tag      => (label => OP_READY, length => 0,
                       protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0,
          words    => [others => 0]), CuBit.Messages.Wait_Forever);
      debugPrint ("procmgr: bootstrap handoff returned" & LF);
   end;

   debugPrint ("procmgr: registered as driver" & LF);

   --  Allocate initial ELF buffer via sbrk (grows dynamically as needed)
   debugPrint ("procmgr: bootstrap 1/4 allocating launch buffer" & LF);
   declare
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_SBRK,
                      Unsigned_64 (INITIAL_BUF_PAGES) *
                      Unsigned_64 (PAGE_SIZE));
      if ret = Unsigned_64'Last then
         debugPrint ("procmgr: sbrk failed" & LF);
         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_EXIT, 1);
         end;
         return;
      end if;
      elfBuf := To_Address (Integer_Address (ret));
   end;
   debugPrint ("procmgr: bootstrap 1/4 launch buffer ready" & LF);

   --  Grant elfBuf directly to FS server for zero-copy reads, through our
   --  FS endpoint capability rather than the FS server's process ID.
   debugPrint ("procmgr: bootstrap 2/4 creating filesystem grant" & LF);
   declare
      ok : Boolean;
   begin
      CuBit.Memory_Grants.Create_Via_Capability (
         slot      => PH.Filesystem,
         localAddr => elfBuf,
         numPages  => INITIAL_BUF_PAGES,
         readWrite => True,
         reference => fsGrant,
         success   => ok);

      if not ok then
         debugPrint ("procmgr: createGrantViaCap to FS failed" & LF);
         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_EXIT, 1);
         end;
         return;
      end if;
   end;
   debugPrint ("procmgr: bootstrap 2/4 filesystem grant ready" & LF);

   -- Generation-bearing Config loan. All callers supply owner-checked metadata.
   debugPrint ("procmgr: bootstrap 3/4 creating config grant" & LF);
   CuBit.Memory_Grants.Create_Via_Capability
     (PH.Config, elfBuf, INITIAL_BUF_PAGES, True,
      Config_Grant, Config_Grant_Ready);
   if not Config_Grant_Ready then
      debugPrint ("procmgr: config grant unavailable" & LF);
   else
      debugPrint ("procmgr: bootstrap 3/4 config grant ready" & LF);
   end if;

   --  Process init.ccl to spawn Stage 2 programs
   debugPrint ("procmgr: bootstrap 4/4 reading startup profile" & LF);
   declare
      Compiled : Boolean;
   begin
      processInitCCL (Compiled);
      if Compiled then
         --  The current system generation's pins, before anything starts.
         applyCurrentGeneration;
         startPlan;
      end if;
   end;

   debugPrint ("procmgr: ready, entering receive loop" & LF);

   --  Main service loop. While launches are parked it also wakes every
   --  Registration_Poll_Ms to look at their providers (resumeParked).
   loop
      --  procmgr's own lines wait for logstore (Warnings, the console
      --  route): between logstore's start and its registration, look every
      --  Registration_Poll_Ms, so they are bound as soon as it registers.
      Awaiting_Logstore :=
        (Warnings.Pending and then Registrants (DRIVER_LOGSTORE) /= No_Process) or else introductionsDue;
      if Parked_First = null and then nextRestart = 0 and then not Service_Switches.Active (Switch)
        and then SQ.Wake_At (Stop_Sequence) = 0 and then not Awaiting_Logstore
        and then Notices_Due = 0
      then
         receive (sender, msg);
         received := True;
      else
         declare
            Deadline : Unsigned_64 := Unsigned_64'Last;
            Restart_At : constant Unsigned_64 := nextRestart;
         begin
            if Parked_First /= null or else Awaiting_Logstore then
               Deadline := CuBit.Monotonic.Milliseconds + Registration_Poll_Ms;
            end if;
            if Restart_At /= 0 then
               Deadline := Unsigned_64'Min (Deadline, Restart_At);
            end if;
            --  A switch looks at its deadline and its instance's
            --  registration every Registration_Poll_Ms.
            if Service_Switches.Active (Switch) then
               Deadline := Unsigned_64'Min
                 (Deadline, CuBit.Monotonic.Milliseconds + Registration_Poll_Ms);
            end if;
            --  The root stop's next grace or poll (PWR-003).
            if SQ.Wake_At (Stop_Sequence) /= 0 then
               Deadline := Unsigned_64'Min (Deadline, SQ.Wake_At (Stop_Sequence));
            end if;
            --  Exit notices a requester's lane could not take yet.
            if Notices_Due /= 0 then
               Deadline := Unsigned_64'Min (Deadline, Notices_Due);
            end if;
            receiveUntil (Deadline, sender, msg, received);
         end;
      end if;
      Warnings.Pump;
      Sources.Pump;
      Metric_Sources.Pump;

      --  A launch handle reaches only its operations, the session's debug
      --  badge only its one (Handle_Admission.Route_Of).
      Route := (if received then HA.Route_Of (msg.authorityTag, msg.tag.label) else HA.Ordinary);
      if received and then Route = HA.Handle_Request then
         handleHandleRequest (sender, msg);
      elsif received and then Route = HA.Session_Debug_Request then
         handleSessionDebug (sender, msg);
      elsif received and then Route = HA.Refused then
         if LH.Is_Operation_Label (msg.tag.label) or else msg.tag.label = LH.Session_Debug_Operation then
            answerHandle (sender, LH.Not_A_Handle);
         else
            sendReply (sender, REPLY_ERR, 0);
         end if;
      elsif received then
         case msg.tag.label is
            when OP_LAUNCH =>
               handleLaunch (sender, msg);
            when LAuth.Table_Operation =>
               handleLaunchTable (sender, msg);
            when Staged_Switch_Operation =>
               handleStagedSwitch (sender, msg);
            when LH.Debug_Records_Operation =>
               handleDebugRecords (sender, msg);
            when CuBit.Child_Specs.Start_Operation =>
               handleStartChild (sender, msg);
            when CuBit.Child_Specs.Release_Operation =>
               releaseChild (sender, msg);
            when CuBit.System_Stops.Stop_Operation =>
               handleSystemStop (sender, msg, Apps_Only => False);
            when CuBit.System_Stops.Stop_Apps_Operation =>
               handleSystemStop (sender, msg, Apps_Only => True);
            when CuBit.Service_Replacement.Switch_Operation =>
               handleServiceSwitch (sender, msg);
            when CuBit.Service_Replacement.Boot_Stage_Operation =>
               handleServiceSwitch (sender, msg, Stage => True);
            when CuBit.Launch_Grants.Places_Operation =>
               handleDelegatedPlaces (sender, msg);
            when CuBit.Program_Descriptions.Description_Operation =>
               handleProgramDescription (sender, msg);
            when CuBit.Process_Observer.List_Label =>
               handleProcessList (sender, msg);
            when LI.Await_Request =>
               if msg.authorityTag = LI.Introducer_Tag then
                  handleAwaitIntroduction (sender, msg);
               else
                  sendReply (sender, REPLY_ERR, 0);
               end if;
            when EVENT_CHILD_EXIT =>
               --  A process retired (the kernel tells procmgr of each; only the
               --  kernel posts this label, with no sender). Act only if it is
               --  really gone, too.
               if sender = No_Process and then CuBit.Child_Exits.Valid
                    (msg.tag.length, msg.words (0), msg.words (1), msg.words (2))
                 and then not processListed (From_Word (msg.words (0)))
               then
                  declare
                     Exited : constant Process_ID := From_Word (msg.words (0));
                     Known  : constant Records.Index := Records.Find (Exited);
                     Report : constant CuBit.Child_Exits.Report :=
                       CuBit.Child_Exits.Decode (msg.words (0), msg.words (1), msg.words (2));
                  begin
                     --  Processes procmgr launched, by manifest identity; the
                     --  kernel's "kernel: process killed" record says why when
                     --  one faulted. Others (devmgr's) are devmgr's to log.
                     --  A launch procmgr withdrew before resuming it never ran;
                     --  the withdrawal was reported where procmgr decided it.
                     if Known /= 0 and then Records.Values (Known).Valid
                       and then not Records.Values (Known).Resumed
                     then
                        Warnings.Inform
                          ("procmgr: launch attempt " &
                           Records.Values (Known).Identity (1 .. Records.Values (Known).Identity_Length) &
                           " pid" & Image (Exited) & " withdrawn before it ran");
                     elsif Known /= 0 and then Records.Values (Known).Valid then
                        declare
                           Line : constant String :=
                             "procmgr: process " &
                             Records.Values (Known).Identity (1 .. Records.Values (Known).Identity_Length) &
                             " pid" & Image (Exited) &
                             (if Report.Kind = CuBit.Child_Exits.Stopped
                              then " stopped (killed or faulted)"
                              else " exited code" & Unsigned_64'Image (Report.Code)) &
                             --  Its own reason goes to its logstore ring, if
                             --  it has one: the kernel console is gone (ABI-002).
                             (if Report.Kind /= CuBit.Child_Exits.Stopped
                                and then Report.Code = CuBit.Startup_Check.Startup_Refused
                              then " (stopped by its CAP-003 startup check)" else "") &
                             --  Why, filed under it in logstore (LOG-004): the
                             --  kernel's typed fault report, its own last words.
                             (if Report.Kind = CuBit.Child_Exits.Stopped or else Report.Code /= 0
                              then "; why: (logs.recent """ &
                                   Records.Values (Known).Identity
                                     (1 .. Records.Values (Known).Identity_Length) & """)"
                              else "");
                        begin
                           if Report.Kind = CuBit.Child_Exits.Stopped or else Report.Code /= 0 then
                              Warnings.Warn (Line);
                           else
                              Warnings.Inform (Line);
                           end if;
                        end;
                     end if;
                     --  Supervision (SUP-001): its edge, and its children reaped.
                     superviseEnded
                       (Exited, Report.Kind = CuBit.Child_Exits.Stopped or else Report.Code /= 0);
                     if Known /= 0 then
                        Sources.Exited (Exited);
                        if Records.Values (Known).Metric_Outlet then
                           Metric_Sources.Exited (Exited);
                        end if;
                     end if;
                     forgetLate (Exited);
                     --  Its launch handles, its requester's notice, its
                     --  debug sessions (before its record goes).
                     launchHandlesExited (Exited, Known, Report);
                     for Registrant of Registrants loop
                        if Registrant = Exited then
                           Registrant := No_Process;
                        end if;
                     end loop;
                     releaseRings (Exited);
                     forgetLaunchState (Exited);
                     if Known /= 0 and then Records.Values (Known).Valid then
                        closeChildHandles
                          (Records.Values (Known).Control, Records.Values (Known).Mailbox);
                     end if;
                     Records.Remove (Exited);
                     clearAuthorityForPID (Exited);
                     releaseNetworkOwner (Exited);
                     releaseFilesystemOwner (Exited);
                     superviseExit
                       (Exited, Report.Kind = CuBit.Child_Exits.Stopped or else Report.Code /= 0);
                     switchExited
                       (Exited, Report.Kind /= CuBit.Child_Exits.Stopped and then Report.Code = 0);
                     --  Last: its filesystem owner was released above, so
                     --  a commit this starts sees its writes (PWR-003).
                     shutdownExited
                       (Exited, Report.Kind /= CuBit.Child_Exits.Stopped and then Report.Code = 0);
                  end;
               end if;
            when CuBit.Control_Events.Grant_Revoked_Label
               | CuBit.Control_Events.Grant_Returned_Label =>
               --  A launcher revoked a ring procmgr holds for a child: the
               --  child's derived grant goes with it, and procmgr returns its
               --  own hold when the child ends (releaseRings). Not a request.
               null;
            when others =>
               sendReply (sender, REPLY_ERR, 0);
         end case;
      end if;
      if Parked_First /= null then
         resumeParked;
      end if;
      if introductionsDue then
         --  After this pass's Warnings.Pump: procmgr's registrar is open.
         introduceRegistered;
      end if;
      if Plan_Awaits_Logstore and then not Introduce_When_Registered (DRIVER_LOGSTORE) then
         --  logstore registered and was introduced (or will not be): the
         --  plan goes on.
         Plan_Awaits_Logstore := False;
         continuePlan;
      end if;
      restartDue;
      switchTick;
      shutdownTick;
      retryNotices;
   end loop;
end main;
