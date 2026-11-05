------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Process Manager Service
--
--  Handles OP_SPAWN requests from userspace clients. Reads an ELF binary
--  from the filesystem service and calls SPAWN to load it into a
--  new process.
--
--  Capabilities (CAP-002): the handles devmgr placed before procmgr ran
--  (CuBit.Handles.Boot_Roots.Process_Manager), the per-child handles SPAWN
--  gives it, and the endpoints of the services it started. Each child gets
--  its startup table, built from its manifest (CuBit.Startup_Builder), in
--  its launch block before it first runs.
------------------------------------------------------------------------------
with Deferred_Warnings;
with Source_Notices;
with CuBit.Log_Sources;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Config_Worker_Startup;
with Interfaces; use Interfaces;
with CCL.Configurations;
with GPU_Rendering_Settings;
with CCL.Declarations;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Control;
with CuBit.Handles.Devices;
with CuBit.Handles.Boot_Roots;
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
with CuBit.Control_Events;
with CuBit.Launch_Authority;
with CuBit.Failures;
with CuBit.Filesystems;
with Windowed_Reads;
with CuBit.File_Access;
with CuBit.Network_Authority;
with CuBit.Render_Authority;
with CuBit.Firmware_Snapshot_Authority;
with CuBit.Firmware_Snapshot;
with CuBit.ACPI_Hardware_Authority;
with CuBit.ACPI_Fixed_Windows;
with CuBit.ACPI_Window_Exchange;
with CuBit.Firmware_Data_Exchange;
with ACPI_Protocol;
with ACPI_Register_Roles;
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

procedure main is
   function Image (Process : Process_ID) return String
     renames CuBit.Process_IDs.Text.Image;
   --  Refused mints reach logstore once it runs (the kernel's own
   --  "kernel: device-memory denied" record says why).
   --  logstore's endpoint, once it runs (defined with the process records).
   function Logstore_Collector return CuBit.Handles.Endpoint;
   package Warnings is new Deferred_Warnings (Issuance => 4, Collector => Logstore_Collector);
   --  Who each process procmgr starts is, for logstore (LOG-003): its
   --  records keep their name after it exits.
   package Sources is new Source_Notices
     (Issuance => CuBit.Log_Sources.Process_Manager_Issuance, Collector => Logstore_Collector);
   use ASCII;
   --  Never reuse issuance identities within this bootstrap issuer lifetime.
   --  Independent procmgr restart requires an epoch protocol, not yet supported.
   Next_Log_Issuance : Unsigned_32 := 1;
   --  Same rule for metrics.svc publisher/observer tags; zero = exhausted.
   Next_Metric_Issuance : Unsigned_32 := 1;
   package GPU_Grants renames CuBit.Capability_Grants;
   package H renames CuBit.Handles;
   use all type H.Endpoint, H.Process, H.Cspace, H.Device_Memory, H.Notification, H.Firmware_Snapshot,
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
   OP_SPAWN   : constant Unsigned_32 := 16#0100#;
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

   --  ELF buffer (also serves as grant buffer to FS for zero-copy reads)
   elfBuf : System.Address := System.Null_Address;

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

   --  OP_LAUNCH: the validated launch block of the request being served
   --  (procmgr's own copy, never the requester's grant), installed into
   --  each child Spawn_Attempt creates for it. Zero bytes: no block.
   launchBlock : CuBit.Launch_Arguments.Block
     (1 .. CuBit.Launch_Arguments.Maximum_Block_Bytes) := [others => 0];
   pendingArgumentBytes : CuBit.Launch_Arguments.Block_Length := 0;
   --  The startup plan's (arguments ...) for the program being started
   --  (docs/ccl-launch-parameters.md), as written; empty for every other
   --  launch (OP_LAUNCH, OP_SPAWN), which cannot give typed parameters.
   pendingParameters : CCL.Configurations.Value_Text;
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
   type Ring_Parent_Array is array (1 .. CuBit.Outlet_Rings.Maximum_Entries)
     of CuBit.Memory_Grants.Grant_Reference;
   type Launch_State is record
      Table        : LAuth.Table_Bytes (1 .. LAuth.Maximum_Table_Bytes) :=
                       [others => 0];
      Table_Length : LAuth.Table_Length := 0;
      Services     : Unsigned_64 := 0;
      Scope_Count  : Natural range 0 .. CuBit.File_Access.Maximum_Entries := 0;
      Scopes       : Held_Scope_Array;
      --  The launcher's ring grants procmgr acquired to derive this
      --  process's port rings; returned when it ends.
      Ring_Parents : Ring_Parent_Array := [others => (others => <>)];
      Ring_Parent_Count : Natural range 0 .. CuBit.Outlet_Rings.Maximum_Entries := 0;
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
   attenuationRefused : Boolean := False;
   --  The launched child could not read the working directory its launch
   --  block names; refused like an attenuation failure.
   directoryRefused : Boolean := False;

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
         Launches.Values (I).Services := 0;
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
        HC.Process_List (PH.Process_Root, processList'Address, Process_List_Bytes);
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
   end record;
   Empty_Process_Record : constant Process_Record := (others => <>);
   package Records is new CuBit.Identity_Tables
     (Element => Process_Record, Empty => Empty_Process_Record, Capacity_Bits => 9);
   --  Observer issuances never wrap: exhaustion denies the role.
   Next_Process_Issuance : Unsigned_32 := 1;

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
                             Launcher => launcher, Started => syscall (SYSCALL_GETTIME),
                             Control => Control, Mailbox => Mailbox, Resumed => False);
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

   --  List: the kernel's table joined with Records, written into one
   --  page the caller lends. Only a holder of the observer tag may ask.
   procedure handleProcessList (sender : Process_ID; msg : Message) is
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped : System.Address;
      Ok, Returned : Boolean;
      Count, Written, Total : Natural := 0;
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
           HC.Process_List (PH.Process_Root, processList'Address, Process_List_Bytes);
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
                  if Written < PO.Page_Records then
                     declare
                        R : constant Natural := Written * PO.Record_Bytes;
                     begin
                        Put (R + PO.Pid_Offset, To_Word (Pid), PO.Pid_Bytes);
                        Page (R + PO.State_Offset) := Item.State;
                        Page (R + PO.CPU_Offset) := Item.CPU;
                        Put (R + PO.Priority_Offset,
                             Unsigned_64 (Unsigned_16'Mod (Item.Priority)), 2);
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

   --  Boot timeline (docs/boot-timeline.md): one page of the kernel's
   --  timeline image, for a process observer. procmgr holds the CAP_PROCESS
   --  READ the kernel requires; observers get it only through this role.
   --  The kernel writes only procmgr's own memory, never a received grant:
   --  each chunk lands here first.
   Timeline_Chunk : array (0 .. CuBit.Boot_Milestone_Codes.Read_Chunk_Bytes - 1) of Unsigned_8 :=
     [others => 0];

   procedure handleBootTimeline (sender : Process_ID; msg : Message) is
      package BMC renames CuBit.Boot_Milestone_Codes;
      --  Pages of the image a reader may ask for; the image is smaller.
      Maximum_Page_Index : constant := 15;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped : System.Address;
      Ok, Returned : Boolean;
      Written : Unsigned_64 := 0;
      Image_Bytes : Unsigned_64 := 0;
      replyMsg : Message := NULL_MESSAGE;
      ignore : Unsigned_64;
      procedure Answer (Result : PO.Status) is
      begin
         replyMsg.tag := (label => Unsigned_32 (PO.Status'Enum_Rep (Result)), length => 3, protocol => 0, ipc => <>, reserved => 0);
         replyMsg.words := [0 => Unsigned_64 (PO.Status'Enum_Rep (Result)), 1 => Written,
                            2 => Image_Bytes, others => 0];
         ignore := reply (sender, replyMsg);
      end Answer;
   begin
      if not PO.Is_Observer (msg.authorityTag) then
         Answer (PO.Denied);
         return;
      end if;
      if msg.tag.length < 2 or else not CuBit.Grant_References.Valid_Wire (msg.words (0))
        or else msg.words (1) > Maximum_Page_Index
      then
         Answer (PO.Invalid_Request);
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (Reference, sender, 0, BMC.Page_Bytes, CuBit.Memory_Grants.Write_Access, Mapped, Ok);
      if not Ok then
         Answer (PO.Invalid_Request);
         return;
      end if;
      declare
         Page : array (0 .. BMC.Page_Bytes - 1) of Unsigned_8 with Import, Address => Mapped;
         Base : constant Unsigned_64 := msg.words (1) * BMC.Page_Bytes;
         Copied : Unsigned_64;
      begin
         Page := [others => 0];
         --  The kernel copies one chunk per call; loop until the page is
         --  full or the image ends.
         while Written < BMC.Page_Bytes loop
            Copied := CuBit.Handles.Devices.Read_Timeline
              (PH.Process_Root, Unsigned_64 (To_Integer (Timeline_Chunk'Address)),
               Base + Written,
               Unsigned_64'Min (BMC.Read_Chunk_Bytes, BMC.Page_Bytes - Written));
            exit when Copied = 0 or else Copied > BMC.Read_Chunk_Bytes;
            for I in 0 .. Natural (Copied) - 1 loop
               Page (Natural (Written) + I) := Timeline_Chunk (I);
            end loop;
            Written := Written + Copied;
         end loop;
         --  The image's end: where a read from past it returns nothing.
         Image_Bytes := Base + Written;
         if Written = BMC.Page_Bytes then
            Image_Bytes := 0;   --  more follows; the last page tells the size
         end if;
      end;
      CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
      Answer ((if Written = 0 and then msg.words (1) = 0 then PO.Unavailable else PO.OK));
   end handleBootTimeline;

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
   --  Reads directly into elfBuf through page-aligned, bounded loans.
   --  Returns number of bytes read, or 0 on failure.
   ---------------------------------------------------------------------------
   function readFileFromFS (name : String) return Unsigned_64
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
            localAddr => elfBuf + Storage_Offset (Offset),
            numPages => (Count + PAGE_SIZE - 1) / PAGE_SIZE,
            readWrite => True, reference => Loan, success => Ok);
         if not Ok then
            debugPrint ("procmgr: file window grant failed" & LF);
            return;
         end if;
         msg := CuBit.Filesystems.Read_Request (handle, Loan, Unsigned_64 (Count));
         tag := capCall (PH.Filesystem, msg, CuBit.Messages.Wait_Forever);
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
      tag := capCall (PH.Filesystem, msg, CuBit.Messages.Wait_Forever);

      if tag.label /= REPLY_OK then
         debugPrint ("procmgr: OP_OPEN failed" & LF);
         return 0;
      end if;

      handle   := CuBit.Filesystems.File_Handle (msg.words (0));
      fileSize := msg.words (1);

      if fileSize > 0 and then ensureBuffer (fileSize) then
         --  The 1 MiB windows are page multiples; only the last is short.
         --  A short backend read is a failure, never a partially loaded ELF.
         Complete := Reader.Read_All (Natural (fileSize));
      end if;

      --  Close file handle
      msg := CuBit.Filesystems.Close_Request (handle);
      tag := capCall (PH.Filesystem, msg, CuBit.Messages.Wait_Forever);

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
   CAP_TYPE_IO_PORTS : constant Unsigned_64 :=
     CuBit.Handles.ABI.Kind'Enum_Rep (CuBit.Handles.ABI.IO_Ports);

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
         Import, Address => elfBuf + Storage_Offset (offset);
   begin
      return val;
   end readU16;

   ---------------------------------------------------------------------------
   --  readU32 - read a little-endian Unsigned_32 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU32 (offset : Unsigned_64) return Unsigned_32 is
      val : Unsigned_32 with
         Import, Address => elfBuf + Storage_Offset (offset);
   begin
      return val;
   end readU32;

   ---------------------------------------------------------------------------
   --  readU64 - read a little-endian Unsigned_64 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU64 (offset : Unsigned_64) return Unsigned_64 is
      val : Unsigned_64 with
         Import, Address => elfBuf + Storage_Offset (offset);
   begin
      return val;
   end readU64;

   ---------------------------------------------------------------------------
   --  readU8 - read a Unsigned_8 from elfBuf at byte offset
   ---------------------------------------------------------------------------
   function readU8 (offset : Unsigned_64) return Unsigned_8 is
      val : Unsigned_8 with
         Import, Address => elfBuf + Storage_Offset (offset);
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
                    with Import, Address => elfBuf + Storage_Offset (sh_offset);
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
   --  The manifest a child's ELF carries (CCL.Manifests): its declared
   --  startup table (.cubit.startup, CuBit.Startup_Tables) and the
   --  parameters of its requests (.cubit.caps). Declared entry i is request
   --  i; the requests of resource kinds (devices, DMA, scheduling) are in
   --  .cubit.resources, which procmgr never grants from.
   ---------------------------------------------------------------------------
   --  Whether the launcher being attenuated for holds what this manifest
   --  request asks: the same service role. Notifications, framebuffer,
   --  render and network are never passed to a launched child.
   function launcherHolds (reqType : Unsigned_8; role : Unsigned_32)
     return Boolean is
   begin
      if reqType = REQ_SERVICE and then role < TRACKED_SERVICE_ROLES then
         return (Launches.Values (launchIndex (attenuateFor)).Services and
                 Shift_Left (Unsigned_64'(1), Natural (role))) /= 0;
      end if;
      return False;
   end launcherHolds;

   procedure noteService (pid : Process_ID; role : Unsigned_32) is
   begin
      if launchIndex (pid) /= 0 and then role < TRACKED_SERVICE_ROLES then
         Launches.Values (launchIndex (pid)).Services := Launches.Values (launchIndex (pid)).Services or
           Shift_Left (Unsigned_64'(1), Natural (role));
      end if;
   end noteService;

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
      --  Declared entry i's request in Caps; 0 for a resource kind.
      Request_Of : Request_Map := [others => 0];
   end record;

   --  The startup kinds whose requests are in .cubit.caps.
   function In_Caps (Kind : ST.Capability_Kind) return Boolean is
     (Kind in ST.Endpoint | ST.Notification | ST.Framebuffer | ST.Render | ST.Network
              | ST.Firmware_Snapshot | ST.ACPI_Hardware);

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
                       with Import, Address => elfBuf + Storage_Offset (sh_offset);
                     --  One read of the image's bytes (single fetch).
                     Copy : constant ST.Bytes (1 .. Length) := Source;
                     Outcome : ST.Decode_Result;
                     use type ST.Decode_Result;
                  begin
                     ST.Decode (Copy, View.Declared, Outcome);
                     if Outcome /= ST.Decoded then
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
                        when ST.Endpoint     => Kind /= REQ_SERVICE,
                        when ST.Notification => Kind /= REQ_NOTIFICATION,
                        when ST.Framebuffer  => Kind /= REQ_FRAMEBUFFER,
                        when ST.Render       => Kind /= CuBit.Render_Authority.Manifest_Request,
                        when ST.Network      => Kind /= CuBit.Network_Authority.Manifest_Request,
                        when ST.Firmware_Snapshot =>
                          Kind /= CuBit.Firmware_Snapshot_Authority.Manifest_Request,
                        when ST.ACPI_Hardware =>
                          Kind /= CuBit.ACPI_Hardware_Authority.Manifest_Request,
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
      Role = DRIVER_ACPI_REGISTERS or else Role = CuBit.Startup_Wait_Test.Service_Role);

   --  For each launched-service role: the live process procmgr started with
   --  its registration, until that process exits.
   Registrants : array (Service_Role) of Process_ID := [others => No_Process];
   --  And when (monotonic milliseconds) it must have registered by: its
   --  start plus its startup entry's (ready-deadline-ms N). An entry that
   --  declares none has the plan's named ceiling,
   --  CCL.Configurations.MAX_READY_DEADLINE_MS (Launch_Entry's default).
   Registrant_Ready_By : array (Service_Role) of Unsigned_64 := [others => 0];

   --  The ready deadline of a launch the startup plan did not make (OP_LAUNCH,
   --  OP_SPAWN), which cannot say (ready-deadline-ms N): the plan's ceiling.
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
      elsif Role = CuBit.Process_Observer.Observer_Service_Role then DRIVER_PROCMGR
      elsif Role = CuBit.Disk_Install.Service_Role then DRIVER_DEVMGR
      else Role);

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
         return systemStartup and then Identity = "com.cubit.metrics";
      elsif Role = DRIVER_LEXICON then
         --  An impostor would read every text its clients spell-check.
         return systemStartup and then Identity = "com.cubit.lexicon";
      elsif Role = DRIVER_ACPI then
         --  An impostor would answer for the machine's firmware.
         return systemStartup and then Identity = "com.cubit.acpi";
      elsif Role = DRIVER_ACPI_REGISTERS then
         --  An impostor would see (and answer) every register access.
         return systemStartup and then Identity = "com.cubit.acpi-backend";
      elsif Role = CuBit.Installer_Protocol.Service_Role then
         --  An impostor would see what is installed where, and refuse it.
         return systemStartup and then Identity = "com.cubit.installer";
      elsif Role = CuBit.Startup_Wait_Test.Service_Role then
         --  Test only (tests/startup-wait): from a test's startup plan.
         return systemStartup and then Identity = "com.cubit.startup-wait-test";
      end if;
      return False;
   end registrationAllowed;

   ---------------------------------------------------------------------------
   --  Access manifest constants
   ---------------------------------------------------------------------------
   ACCESS_MAGIC : constant Unsigned_32 := 16#43434143#;  -- "CACC" LE
   MAX_ACL      : constant := 16;

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
               debugPrint ("procmgr: delegated place not held by the launcher: " &
                           name & LF);
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
   --  sandboxOverride / cwd / binaryName enable RUN_FOLDER / APP_FOLDER
   --  prefix rewriting for wildcard FS entries.
   ---------------------------------------------------------------------------
   procedure parseAndSendACL
     (childPID        : Process_ID;
      elfSize         : Unsigned_64;
      policyReady     : out Boolean;
      sandboxOverride : Unsigned_8 := SANDBOX_NONE;
      cwd             : String := "";
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
                        manifestSandbox : Unsigned_8 :=
                           readU8 (sh_offset + 13);
                        effectiveSandbox : Unsigned_8;
                        sandboxPrefix    : String (1 .. 64) :=
                           (others => ' ');
                        sandboxPrefixLen : Natural := 0;
                     begin
                        --  An invalid sandbox cannot mean unrestricted.
                        if manifestSandbox > SANDBOX_APP_FOLDER then
                           goto No_Access_Policy;
                        end if;

                        --  Most restrictive wins (higher = more
                        --  restrictive)
                        if sandboxOverride > manifestSandbox then
                           effectiveSandbox := sandboxOverride;
                        else
                           effectiveSandbox := manifestSandbox;
                        end if;

                        --  Compute sandbox prefix
                        if effectiveSandbox = SANDBOX_RUN_FOLDER then
                           --  Use cwd from spawner
                           sandboxPrefixLen := cwd'Length;
                           if sandboxPrefixLen > 64 then
                              goto No_Access_Policy;
                           end if;
                           for c in 0 .. sandboxPrefixLen - 1 loop
                              sandboxPrefix (1 + c) :=
                                 cwd (cwd'First + c);
                           end loop;
                        elsif effectiveSandbox = SANDBOX_APP_FOLDER
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
                             SERVICE_FS | SERVICE_CONFIG | SERVICE_TLS
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
                           if attenuateFor /= No_Process then
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
                                    attenuationRefused := True;
                                    debugPrint ("procmgr: launched child scope " &
                                      "not held by its launcher: " &
                                      locals (j).prefix
                                        (1 .. Natural (locals (j).prefixLen)) & LF);
                                    policyReady := False;
                                    return;
                                 end if;
                              end;
                           end if;
                           if launchIndex (childPID) /= 0 then
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
                           else
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
                    with Import, Address => elfBuf + Storage_Offset (sh_offset);
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
      Result.Owner := syscall (SYSCALL_GETPID);
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
      priority    : Unsigned_64;
      requester   : Process_ID := No_Process;
      sandboxMode : Unsigned_8 := SANDBOX_NONE;
      cwd         : String := "";
      approveNetwork : Network_Approval := No_Network;
      approveRender : Boolean := False;
      systemStartup : Boolean := False;
      startupRole : CCL.Configurations.Startup_Role := CCL.Configurations.Application;
      approveFirmwareSnapshot : Boolean := False;
      approveACPIHardware : Boolean := False;
      approveACPIRegisters : Boolean := False;
      approvePCIConfig : Boolean := False;
      approveFirmwareData : Boolean := False;
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
      pri           : Unsigned_64 := priority;
      t0, t1        : Unsigned_64;
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
      Render_Ticket : Render_Launch.Ticket := 0;
      --  The launch block payload (arguments, description) built for it.
      Block_Length : LA.Block_Length := pendingArgumentBytes;
      --  Registrations procmgr approved, offered to devmgr once it runs.
      Offer_Logstore, Offer_Desktop : Boolean := False;
      --  The ACPI hardware windows installed in the child (Mint_ACPI_Hardware),
      --  in ascending port order: appended to its startup table as
      --  AHA.IO_Label (ACPI_Binding, K).
      ACPI_Window_Words : array (AHA.IO_Window_Number) of Unsigned_64 := [others => ST.No_Handle];
      ACPI_Window_Total : Natural range 0 .. AHA.Maximum_IO_Windows := 0;
      --  Then its firmware-memory windows, ascending by address, appended
      --  as AHA.Memory_Label (ACPI_Binding, K).
      ACPI_Memory_Words : array (AHA.Memory_Window_Number) of Unsigned_64 := [others => ST.No_Handle];
      ACPI_Memory_Total : Natural range 0 .. AHA.Maximum_Memory_Windows := 0;
      --  Then devmgr's firmware-data endpoint (CuBit.Firmware_Data_Exchange),
      --  when the plan approves it: appended as Data_Label (ACPI_Binding).
      ACPI_Data_Word : Unsigned_64 := ST.No_Handle;
      ACPI_Binding : ST.Name;
      use type CuBit.Render_Startup.Requirement, CuBit.Render_Startup.Attempt;
      -- Transitional installed-system-app policy, like Desktop_Approval for
      -- browsers. Exact boot-namespace name, never a supplied path/identity;
      -- assumes the system executable namespace is administrator-controlled.
      -- Do not propagate this as systemStartup or honor caller-supplied flags.
      --  The CCL Workbench's and console's REPLs read logs too:
      --  (logs.recent "service").
      Log_Viewer_Approved : constant Boolean :=
        (name = "boot-logs.app" or else name = "ccl-workbench.app" or else
         name = "ccl-console.app" or else name = "logs.app")
        and then requester /= No_Process and then
        sandboxMode = SANDBOX_NONE and then cwd'Length = 0 and then
        requester = Registered_Driver (DRIVER_DESKTOP);
      --  The CCL Console's and Workbench's :ps / (proc.list) see what runs,
      --  under the same narrow desktop-launched exception.
      Process_Viewer_Approved : constant Boolean :=
        (name = "ccl-workbench.app" or else name = "ccl-console.app" or else name = "logs.app")
        and then requester /= No_Process and then
        sandboxMode = SANDBOX_NONE and then cwd'Length = 0 and then
        requester = Registered_Driver (DRIVER_DESKTOP);
      --  Logs, the console and the Workbench may change what logstore keeps
      --  (log-control), under the same desktop-launched exception.
      Log_Control_Approved : constant Boolean := Process_Viewer_Approved;
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

      --  Whether a service request for Role is for a startup-only authority
      --  this launch is not approved for: startup approval, or the trusted
      --  Desktop's narrowly selected system log viewer. This exception
      --  grants observation only, not the other startup-only authorities.
      function Startup_Only_Denied (Role : Unsigned_64) return Boolean is
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
         --  acpi.svc's endpoint to the backend's named registers: the full
         --  frontend scope, only for an entry approving it.
         isACPIRegisters : constant Boolean := Role = DRIVER_ACPI_REGISTERS;
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
            elsif isACPIRegisters then ACPI_Register_Frontend
            else Log_Publication);
         Approval : constant Decision := Evaluate
           (Requested => True,
            Installation_Approved => Startup_Approves (Authority, systemStartup, approveACPIRegisters) or else
              (isLogObserver and then Log_Viewer_Approved) or else
              (isProcessObserver and then Process_Viewer_Approved) or else
              (isLogControl and then Log_Control_Approved),
            Session_Approved => True,
            Issuer_Allowed => True);
      begin
         return (isAudioControl or isLogObserver or isClockControl
                 or isMetricObserver or isProcessObserver or isLogControl
                 or isDiskInstall or isInstaller or isACPIRegisters)
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
         isACPIRegisters : constant Boolean := Role = DRIVER_ACPI_REGISTERS;
         --  The role whose service answers this one.
         Provider_Role : constant Unsigned_64 := Provider_Role_Of (Role);
         Kept : constant H.Rights := Manifest_Rights (Unsigned_64 (R.Rights));
         Issued_Tag : Unsigned_64 := 0;
         Service : CuBit.Handles.Endpoint;
      begin
         Handle := ST.No_Handle;
         Outcome := SB.Refused_By_Policy;
         Logged := False;
         if Startup_Only_Denied (Role) then
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
         Service := providerFor (Provider_Role);

         if isACPI then
            Issued_Tag := Unsigned_64 (ACPI_Protocol.Observer_Tag);
         elsif isACPIRegisters then
            --  A fixed compile-time stamp; the copy keeps no Grant, so the
            --  holder can never re-badge it (Manifest_Rights).
            Issued_Tag := Unsigned_64 (ACPI_Register_Roles.Frontend_Tag);
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
            return;
         elsif not Valid (Service) then
            recordAuthority
              (newPID, Index, AUTH_SOURCE_MANIFEST, AUTH_REASON_SERVICE_MISSING,
               CAP_TYPE_ENDPOINT, True, False, Mask (Kept), Role, 0);
            Outcome := SB.Not_Available;
            if Item.Requirement = ST.Required then
               debugPrint ("procmgr: service not found, role" & Unsigned_64'Image (Role) & LF);
            else
               Warnings.Inform
                 ("procmgr: " & name & ": optional service """ &
                  String (Item.Label.Text (1 .. Item.Label.Length)) &
                  """ not running; started without it");
               Logged := True;
            end if;
            return;
         end if;
         Install ((if Issued_Tag /= 0 then Badged (Service, Kept, Issued_Tag)
                   else Copy (Service, Kept)), Handle, Outcome);
         recordAuthority
           (newPID, Index, AUTH_SOURCE_MANIFEST,
            (if Outcome = SB.Granted then AUTH_REASON_MANIFEST_REQUEST else AUTH_REASON_MINT_FAILED),
            CAP_TYPE_ENDPOINT, True, Outcome = SB.Granted, Mask (Kept), Provider_Role, Issued_Tag);
         if Outcome = SB.Granted then
            noteService (newPID, R.Param0);
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
            Offer_Logstore := Offer_Logstore or else Role = DRIVER_LOGSTORE;
            Offer_Desktop := Offer_Desktop or else Role = DRIVER_DESKTOP;
            --  Write is registration: this process may provide the role.
            if Kept.Write and then Launched_Service_Role (Role) then
               Registrants (Role) := newPID;
               Registrant_Ready_By (Role) :=
                 syscall (SYSCALL_GETTIME) + Unsigned_64 (readyDeadlineMs);
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
         if not (valid_Scope and then Allows (approveNetwork, scope)
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

      --  The ACPI hardware windows (Request.ACPI_Hardware): only for a program
      --  the trusted startup plan started and approved, (acpi-hardware
      --  approve-declared). Each window devmgr grants is installed as its own
      --  read-write I/O-port capability (no Grant), exactly its range; a window
      --  devmgr refuses (another holder has a port of it) is left out, never
      --  narrowed or widened. The declared entry holds the first window's
      --  handle; the windows are appended after the table is built.
      procedure Mint_ACPI_Hardware
        (R : Caps_Request; Index : ST.Entry_Index; Item : ST.Startup_Entry;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         package AX renames CuBit.ACPI_Window_Exchange;
         package AF renames CuBit.ACPI_Fixed_Windows;
         Windows : AF.Window_List;
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
               --  devmgr's PCI configuration window (ACPI-PCI-001, kernel tag
               --  Firmware_PCI_Config_Tag) is installed read-only, and only
               --  when the plan also approves (acpi-pci-config
               --  approve-declared); otherwise it is closed unseen and the
               --  installed windows are numbered as if it did not exist.
               if Outcome = SB.Granted then
                  for K in AHA.Memory_Window_Number loop
                     declare
                        Request : Message := NULL_MESSAGE;
                        Received : HIPC.Carried;
                        Tag : MessageTag;
                        Placed : SB.Mint_Outcome := SB.Not_Available;
                        Word : Unsigned_64 := ST.No_Handle;
                        Skipped : Boolean := False;
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
                           PCI : constant Boolean :=
                             What.Current
                             and then What.Authority_Tag = H.ABI.Firmware_PCI_Config_Tag;
                           Ignore : HIPC.Close_Result;
                        begin
                           if PCI and then not approvePCIConfig then
                              Skipped := True;
                           elsif View.Declared.Count + SB.Self_Entries + ACPI_Window_Total
                                   + ACPI_Memory_Total + 1 > ST.Maximum_Entries
                           then
                              Placed := SB.Limit_Reached;
                           elsif Taken.OK then
                              Install (Copy (Taken.Value,
                                             (if PCI then H.Read_Only else H.Read_Write)),
                                       Word, Placed);
                           end if;
                           if Taken.OK then
                              Ignore := HIPC.Close (Taken.Value);
                           end if;
                           if Skipped then
                              null;
                           elsif Placed = SB.Granted then
                              ACPI_Memory_Total := ACPI_Memory_Total + 1;
                              ACPI_Memory_Words (ACPI_Memory_Total) := Word;
                              debugPrint ("procmgr: ACPI memory window" & Natural'Image (ACPI_Memory_Total) &
                                          (if PCI then " (PCI configuration, read-only)" else "") &
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
                  if approvePCIConfig then
                     debugPrint ("procmgr: ACPI PCI configuration approved for " & name & LF);
                  end if;
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
            end if;
         end if;
         recordAuthority
           (newPID, Index, AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
            CAP_TYPE_IO_PORTS, True, Outcome = SB.Granted, Mask (H.Read_Write),
            0, Unsigned_64 (ACPI_Window_Total));
      end Mint_ACPI_Hardware;

      procedure Mint
        (Into : in out Build_Context; Item : ST.Startup_Entry; Index : ST.Entry_Index;
         Handle : out Unsigned_64; Outcome : out SB.Mint_Outcome)
      is
         R : Caps_Request;
      begin
         Handle := ST.No_Handle;
         Into.Logged := False;
         --  procmgr hands out no device, port, interrupt or scheduling
         --  authority; a DMA region is devmgr's to allocate.
         if View.Request_Of (Index) = 0 then
            Outcome := (if Item.Kind = ST.DMA then SB.Not_Available else SB.Refused_By_Policy);
            return;
         end if;
         R := View.Caps (View.Request_Of (Index));
         case Item.Kind is
            when ST.Endpoint     => Mint_Service (R, Index, Item, Handle, Outcome, Into.Logged);
            when ST.Notification => Mint_Notification (R, Index, Handle, Outcome);
            when ST.Framebuffer  => Mint_Framebuffer (R, Index, Handle, Outcome);
            when ST.Network      => Mint_Network (R, Index, Handle, Outcome);
            when ST.Firmware_Snapshot => Mint_Firmware_Snapshot (R, Index, Handle, Outcome);
            when ST.ACPI_Hardware => Mint_ACPI_Hardware (R, Index, Item, Handle, Outcome);
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
         Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
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
            if View.Declared.Entries (I).Kind = ST.Endpoint and then View.Request_Of (I) /= 0 then
               declare
                  Item : ST.Startup_Entry renames View.Declared.Entries (I);
                  Role : constant Unsigned_64 :=
                    Unsigned_64 (View.Caps (View.Request_Of (I)).Param0);
                  Provider_Role : constant Unsigned_64 := Provider_Role_Of (Role);
                  Required : constant Boolean := Item.Requirement = ST.Required;
               begin
                  if Provider_Role <= Maximum_Role and then not Startup_Only_Denied (Role) then
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

      t0 := syscall (SYSCALL_GETTIME);
      elfSize := readFileFromFS (name);
      t1 := syscall (SYSCALL_GETTIME);

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

      if pri = 0 or pri > 10 then
         pri := 5;
      end if;

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

      t0 := syscall (SYSCALL_GETTIME);
      declare
         Spawned : constant HC.Spawn_Result := HC.Spawn
           (Authority => PH.Process_Root, Image => elfBuf, Bytes => elfSize,
            Priority => pri,
            Name => name (name'First .. name'First + Natural'Min (name'Length, 16) - 1),
            Parent => requester, Limit => View.Declared.Limit);
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
      t1 := syscall (SYSCALL_GETTIME);
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
            Given : String renames pendingParameters.Data (1 .. pendingParameters.Length);
            Refused : Boolean := False;
         begin
            findParameters (elfSize, Signature_At, Signature_Length);
            if Signature_Length = 0 and then Given'Length > 0 then
               debugPrint ("procmgr: parameters refused: " & name & " declares none" & LF);
               Failure := LA.Parameters_Unexpected;
               Refused := True;
            elsif Signature_Length > 0 then
               declare
                  Source : constant PS.Bytes (1 .. Natural'Min (Signature_Length, PS.Maximum_Descriptor_Bytes))
                    with Import, Address => elfBuf + Storage_Offset (Signature_At);
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
                    with Import, Address => elfBuf + Storage_Offset (Description_At);
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

      --  Parse .cubit.id section for package identity
      parseIdSection (elfSize, pkgId, pkgIdLen);
      noteProcess (newPID, requester, pkgId (1 .. pkgIdLen), Child.Control, Child.Mailbox);
      Sources.Started (newPID, pkgId (1 .. pkgIdLen), name);
      Recorded := Records.Find (newPID) /= 0;

      --  A launched child asks only for services its launcher holds; note a
      --  render request (granted only after admission, below).
      for I in 1 .. View.Declared.Count loop
         if View.Request_Of (I) /= 0 then
            declare
               R : Caps_Request renames View.Caps (View.Request_Of (I));
            begin
               if attenuateFor /= No_Process and then not launcherHolds (R.Kind, R.Param0) then
                  attenuationRefused := True;
                  debugPrint ("procmgr: launched child requests " &
                              "authority its launcher does not hold" & LF);
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
      declare
         Policy_Ready : Boolean;
      begin
         parseAndSendACL (newPID, elfSize, Policy_Ready,
                          sandboxMode, cwd, name,
                          networkApproved => approveNetwork /= No_Network);
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
            Now : Unsigned_64 := syscall (SYSCALL_GETTIME);
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
            -- Only a trusted startup plan or Desktop's named GPU-app rule
            -- (CuBit.Launch_Policy.Desktop_Render_Approval) supplies this
            -- approval. Other spawn requests never propagate it.
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
                  Now := syscall (SYSCALL_GETTIME);
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
      t0 := syscall (SYSCALL_GETTIME);
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
         ST.Encode (Built, Encoded, Encoded_Length);
         for K in 1 .. Block_Length loop
            launchEnvelope (K) := launchBlock (K);
         end loop;
         LE.Append (launchEnvelope, Envelope_Length, Encoded (1 .. Encoded_Length), Fits);
         if not Fits or else not HC.Install_Launch_Block
           (newPID, Child.Control, launchEnvelope'Address, Unsigned_64 (Envelope_Length))
         then
            debugPrint ("procmgr: launch block not installed" & LF);
            Discard_Authorized_Child;
            return No_Process;
         end if;
      end;
      t1 := syscall (SYSCALL_GETTIME);
      debugPrint ("procmgr: startup table took ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);
      --  The CSPACE works only until the child runs; procmgr's copy of the
      --  render session is the child's now.
      Close_Launch_Handles;

      --  Resource quotas from the config store (overwrites elfBuf), given
      --  to the kernel as the child starts.
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
         if not HC.Resume
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
         if Known /= 0 then
            Records.Values (Known).Resumed := True;
         end if;
      end;

      debugPrint ("procmgr: resumed PID ");
      debugPrint (Image (newPID));
      debugPrint ("" & LF);

      --  devmgr uses logstore and the desktop: offer it their endpoints.
      if Offer_Logstore then
         offerToDeviceManager (DRIVER_LOGSTORE, Child.Mailbox);
      end if;
      if Offer_Desktop then
         offerToDeviceManager (DRIVER_DESKTOP, Child.Mailbox);
      end if;

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
      priority    : Unsigned_64;
      requester   : Process_ID := No_Process;
      sandboxMode : Unsigned_8 := SANDBOX_NONE;
      cwd         : String := "";
      approveNetwork : Network_Approval := No_Network;
      approveRender : Boolean := False;
      systemStartup : Boolean := False;
      startupRole : CCL.Configurations.Startup_Role := CCL.Configurations.Application;
      approveFirmwareSnapshot : Boolean := False;
      approveACPIHardware : Boolean := False;
      approveACPIRegisters : Boolean := False;
      approvePCIConfig : Boolean := False;
      approveFirmwareData : Boolean := False;
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
      Result := Spawn_Attempt
        (False, No_Process, Retry, Identity, Failure, name, priority, requester, sandboxMode,
         cwd, approveNetwork, approveRender, systemStartup, startupRole, approveFirmwareSnapshot,
         approveACPIHardware, approveACPIRegisters, approvePCIConfig, approveFirmwareData,
         readyDeadlineMs, mayWait, Waits);
      if Result = No_Process and then Retry then
         -- Exactly one fresh attempt, with no broker request. The failed
         -- attempt's launcher resources stay retained independently. The
         -- first got past its waits, so this one never waits.
         debugPrint ("procmgr: render retry software with fresh child" & LF);
         Result := Spawn_Attempt
           (True, Identity, Retry, Retry_Identity, Failure, name, priority, requester,
            sandboxMode, cwd, approveNetwork, approveRender, systemStartup, startupRole,
            approveFirmwareSnapshot, approveACPIHardware, approveACPIRegisters, approvePCIConfig,
            approveFirmwareData, readyDeadlineMs, False, Waits);
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
   --  in order); an OP_LAUNCH or OP_SPAWN request keeps its requester's
   --  reply, saved, and is served again from its message when its waits end.
   ---------------------------------------------------------------------------
   type Parked_Kind is (Plan_Launch, Launch_Request, Spawn_Request);
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

   --  Answer the OP_LAUNCH or OP_SPAWN request being served.
   procedure answerRequest (dest : Process_ID; label : Unsigned_32; word0 : Unsigned_64) is
   begin
      if servingDeferred then
         declare
            Answer : Message := NULL_MESSAGE;
            Ignore : Boolean;
         begin
            Answer.tag := (label => label, length => 1, protocol => 0, ipc => <>, reserved => 0);
            Answer.words := [0 => word0, others => 0];
            --  Used up either way; a requester gone since has no one to tell.
            Ignore := HIPC.Answer (Deferred_Answer, Answer);
            Deferred_Answer := H.No_Reply;
         end;
      else
         sendReply (dest, label, word0);
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
      Priority       : constant Unsigned_64 :=
        Unsigned_64 (CuBit.Launch_Grants.Request_Priority (msg.words (3)));
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

      procedure Fail (Reason : LA.Launch_Failure) is
      begin
         answerRequest (sender, REPLY_ERR, LA.Launch_Failure'Enum_Rep (Reason));
      end Fail;
   begin
      if msg.tag.length /= LA.Launch_Request_Words
        or else not LA.Request_Valid (Name_Bytes, Argument_Bytes)
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
         begin
            if Launcher.Table_Length = 0 or else not LAuth.Contains
              (Launcher.Table (1 .. Launcher.Table_Length), Name)
            then
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

         --  The child holds a subset of its launcher's authority: no
         --  network or startup approvals, and its requests are checked
         --  against what the launcher holds (attenuateFor).
         pendingArgumentBytes := Arguments_Length;
         attenuateFor := sender;
         attenuationRefused := False;
         directoryRefused := False;
         newPID := spawnByName
           (Name, Priority, sender, readyDeadlineMs => Unplanned_Ready_Deadline,
            mayWait => not servingDeferred, Waits => Waits, Failure => Failure);
         pendingArgumentBytes := 0;
         pendingGrantBytes := 0;
         pendingRings := (others => <>);
         attenuateFor := No_Process;
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
      elsif newPID = No_Process and then attenuationRefused then
         debugPrint ("procmgr: " & CuBit.Failures.Explain
           ("Starting a program",
            CuBit.Failures.Failed
              (CuBit.Failures.Not_Granted,
               "it asks for authority its launcher does not hold",
               "grant the launcher that authority, or remove the request " &
               "from the program's manifest")) & LF);
         attenuationRefused := False;
         Fail (LA.Not_Granted);
      elsif newPID = No_Process then
         Fail (Failure);
      else
         --  The child's identity (KERN-003): its exit event names it alike.
         answerRequest (sender, REPLY_OK, To_Word (newPID));
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
         if Launcher.Table_Length = 0 or else not LAuth.Contains
           (Launcher.Table (1 .. Launcher.Table_Length), Name)
         then
            Reason := LA.Not_Granted;
         else
            elfSize := readFileFromFS (Name);
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
                 with Import, Address => elfBuf + Storage_Offset (Found_At);
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
   --  handleSpawn
   --  Request: tag.label=OP_SPAWN, words(0)=grant_id (filename in grant buf),
   --           tag.length=filename length, words(1)=priority,
   --           words(2)=spawnFlags (low 2 bits = sandbox override),
   --           words(3)=cwdLen (cwd bytes follow filename in grant buf)
   --  Reply OK: words(0)=child identity; the requester is its parent and
   --  receives its EVENT_CHILD_EXIT. Reply REPLY_ERR: words(0)=
   --  CuBit.Launch_Arguments.Launch_Failure.
   ---------------------------------------------------------------------------
   procedure handleSpawn (sender : Process_ID; msg : Message) is
      MAX_NAME    : constant := 255;
      MAX_CWD     : constant := 128;
      nameLen     : constant Natural := Natural (msg.tag.length);
      priority    : constant Unsigned_64 := msg.words (1);
      spawnFlags  : constant Unsigned_64 := msg.words (2);
      cwdLen      : Natural := Natural (msg.words (3) and 16#FF#);
      sandboxMode : constant Unsigned_8 :=
         Unsigned_8 (spawnFlags and 16#03#);
      buffer      : CuBit.Memory_Grants.Grant_Reference;
      grantAddr   : System.Address;
      acquired    : Boolean;
      returned    : Boolean;
      name        : String (1 .. MAX_NAME);
      cwd         : String (1 .. MAX_CWD);
      newPID      : Process_ID;
      Failure     : CuBit.Launch_Arguments.Launch_Failure;
      Waits       : Awaited_Access;

      procedure Fail (Reason : CuBit.Launch_Arguments.Launch_Failure) is
      begin
         answerRequest (sender, REPLY_ERR, CuBit.Launch_Arguments.Launch_Failure'Enum_Rep (Reason));
      end Fail;
   begin
      if nameLen = 0 or else nameLen > MAX_NAME or else
        not CuBit.Grant_References.Valid_Wire (msg.words (0))
      then
         Fail (CuBit.Launch_Arguments.Malformed_Request);
         return;
      end if;

      --  Clamp cwdLen to fit in grant buffer after filename
      if cwdLen > MAX_CWD then
         cwdLen := MAX_CWD;
      end if;

      --  The kernel checks the sender owns the buffer grant and says where
      --  it is mapped. Copy the name and cwd out before using them: the
      --  sender can still write the shared pages.
      buffer := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (buffer, sender, 0, Unsigned_64 (nameLen + cwdLen),
         CuBit.Memory_Grants.Read_Access, grantAddr, acquired);
      if not acquired then
         Fail (CuBit.Launch_Arguments.Grant_Unavailable);
         return;
      end if;
      declare
         shared : constant String (1 .. nameLen + cwdLen) with
            Import, Address => grantAddr;
      begin
         name (1 .. nameLen) := shared (1 .. nameLen);
         cwd (1 .. cwdLen) := shared (nameLen + 1 .. nameLen + cwdLen);
      end;
      CuBit.Memory_Grants.Return_Acquisition (buffer, returned);
      if not returned then
         Fail (CuBit.Launch_Arguments.Grant_Unavailable);
         return;
      end if;

      declare
         approval : constant Network_Approval := Desktop_Approval
           (name (1 .. nameLen), sender,
            Registered_Driver (DRIVER_DESKTOP));
         render : constant Boolean := CuBit.Launch_Policy.Desktop_Render_Approval
           (name (1 .. nameLen), sender, Registered_Driver (DRIVER_DESKTOP));
      begin
         if approval = Browser_Outbound then
            debugPrint ("procmgr: desktop browser outbound approval: " &
                        name (1 .. nameLen) & LF);
         end if;
         if cwdLen > 0 then
            newPID := spawnByName (name (1 .. nameLen), priority,
                                   sender,
                                   sandboxMode, cwd (1 .. cwdLen), approval,
                                   approveRender => render,
                                   readyDeadlineMs => Unplanned_Ready_Deadline,
                                   mayWait => not servingDeferred, Waits => Waits,
                                   Failure => Failure);
         else
            newPID := spawnByName (name (1 .. nameLen), priority,
                                   sender,
                                   sandboxMode, approveNetwork => approval,
                                   approveRender => render,
                                   readyDeadlineMs => Unplanned_Ready_Deadline,
                                   mayWait => not servingDeferred, Waits => Waits,
                                   Failure => Failure);
         end if;
      end;
      --  Waiting for a starting provider: served again from this request
      --  once it registers (resumeParked).
      if Waits /= null then
         if parkRequest (Spawn_Request, sender, msg, name (1 .. nameLen), Waits) then
            return;
         end if;
         Failure := CuBit.Launch_Arguments.Service_Unavailable;
      end if;

      if newPID = No_Process then
         Fail (Failure);
      else
         answerRequest (sender, REPLY_OK, To_Word (newPID));
      end if;
   end handleSpawn;

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

   --  Start plan entry Number; Waits: it waits, parked, instead.
   procedure launchPlanEntry
     (Number : Positive; mayWait : Boolean; Waits : out Awaited_Access)
   is
      use CCL.Configurations;
      Item : Launch_Entry renames Startup_Plan.Plan.Launches (Number);
      Name : String renames Item.Executable.Data (1 .. Item.Executable.Length);
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
        (Name, Unsigned_64 (Item.Priority), systemStartup => True, startupRole => Item.Role,
         approveRender => Render_Granted (Item, Name),
         approveFirmwareSnapshot => Item.Approve_Firmware_Snapshot,
         approveACPIHardware => Item.Approve_ACPI_Hardware,
         approveACPIRegisters => Item.Approve_ACPI_Registers,
         approvePCIConfig => Item.Approve_ACPI_PCI_Config,
         approveFirmwareData => Item.Approve_ACPI_Firmware_Data,
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
      end loop;
      if Plan_Running then
         Plan_Running := False;
         CuBit.Boot_Milestones.Mark (CuBit.Boot_Milestone_Codes.Startup_Profile_Launched, PH.Registration);
      end if;
   end continuePlan;

   procedure processInitCCL is
      use CCL.Configurations;
      Source_Size : Unsigned_64;
   begin
      CuBit.Boot_Milestones.Mark (CuBit.Boot_Milestone_Codes.Startup_Profile_Started, PH.Registration);
      debugPrint ("procmgr: reading init.ccl..." & LF);
      for Attempt in 1 .. 10 loop
         Source_Size := readFileFromFS ("init.ccl");
         exit when Source_Size > 0;
         if Attempt < 10 then
            declare
               Ignore : Unsigned_64;
            begin
               Ignore := syscall (SYSCALL_SLEEP, 100);
            end;
         end if;
      end loop;
      if Source_Size not in 1 .. CCL.Declarations.MAX_SOURCE then
         debugPrint ("procmgr: init.ccl missing or oversized; startup denied" & LF);
         return;
      end if;
      declare
         Source : String (1 .. Natural (Source_Size))
           with Import, Address => elfBuf;
      begin
         Compile (Source, Startup_Plan);
      end;
      if not Startup_Plan.Success or else Startup_Plan.Plan.Kind /= Startup_Profile then
         debugPrint ("procmgr: init.ccl rejected at" & Startup_Plan.Position'Image &
                     ": " & Diagnostic_Name (Startup_Plan.Diagnostic) &
                     " (expected startup profile)" & LF);
         return;
      end if;
      debugPrint ("procmgr: init.ccl: ");
      printDec (Unsigned_32 (Startup_Plan.Plan.Launch_Count));
      debugPrint (" entries" & LF);
      Plan_Done := 0;
      Plan_Running := True;
      continuePlan;
   end processInitCCL;

   ---------------------------------------------------------------------------
   --  resumeParked
   --  Look at every parked launch: one whose required provider ended or
   --  missed its deadline is refused, typed; one whose waits all ended is
   --  started (a plan entry, then the plan's next entries) or its request
   --  served again, answering through its saved reply.
   ---------------------------------------------------------------------------
   procedure resumeParked is
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
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
               when Launch_Request | Spawn_Request =>
                  Deferred_Answer := Item.Answer;
                  if Refused_At /= 0 then
                     answerRequest
                       (Item.Sender, REPLY_ERR, CuBit.Launch_Arguments.Launch_Failure'Enum_Rep
                          (CuBit.Launch_Arguments.Service_Unavailable));
                  elsif Item.Kind = Launch_Request then
                     handleLaunch (Item.Sender, Item.Request);
                  else
                     handleSpawn (Item.Sender, Item.Request);
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
   msg    : Message;

begin
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
   processInitCCL;

   debugPrint ("procmgr: ready, entering receive loop" & LF);

   --  Main service loop. While launches are parked it also wakes every
   --  Registration_Poll_Ms to look at their providers (resumeParked).
   loop
      if Parked_First = null then
         receive (sender, msg);
         received := True;
      else
         receiveUntil
           (syscall (SYSCALL_GETTIME) + Registration_Poll_Ms, sender, msg, received);
      end if;
      Warnings.Pump;
      Sources.Pump;

      if received then
         case msg.tag.label is
            when OP_SPAWN =>
               handleSpawn (sender, msg);
            when OP_LAUNCH =>
               handleLaunch (sender, msg);
            when LAuth.Table_Operation =>
               handleLaunchTable (sender, msg);
            when CuBit.Launch_Grants.Places_Operation =>
               handleDelegatedPlaces (sender, msg);
            when CuBit.Program_Descriptions.Description_Operation =>
               handleProgramDescription (sender, msg);
            when CuBit.Process_Observer.List_Label =>
               handleProcessList (sender, msg);
            when CuBit.Boot_Milestone_Codes.Timeline_Label =>
               handleBootTimeline (sender, msg);
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
                     use type CuBit.Child_Exits.Termination_Kind;
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
                              else " exited code" & Unsigned_64'Image (Report.Code));
                        begin
                           if Report.Kind = CuBit.Child_Exits.Stopped or else Report.Code /= 0 then
                              Warnings.Warn (Line);
                           else
                              Warnings.Inform (Line);
                           end if;
                        end;
                     end if;
                     if Known /= 0 then
                        Sources.Exited (Exited);
                     end if;
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
   end loop;
end main;
