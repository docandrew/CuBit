------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Process Manager Service
--
--  Handles OP_SPAWN requests from userspace clients. Reads an ELF binary
--  from the filesystem service and calls SYSCALL_SPAWN to load it into a
--  new process.
--
--  Capability slots:
--    1 = CAP_ENDPOINT to filesystem server
--    4 = CAP_PROCESS with RIGHT_EXECUTE + RIGHT_GRANT
------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Config_Worker_Startup;
with Interfaces; use Interfaces;
with CCL.Configurations;
with CCL.Declarations;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Audio_Control;
with CuBit.Clock_Control;
with CuBit.TLS_Protocol;
with CuBit.TLS_Scopes;
with CuBit.Authority_Policy;
with CuBit.Log_Protocol;
with CuBit.Metric_Protocol;
with CuBit.Authority; use CuBit.Authority;
with CuBit.Memory_Grants;
with CuBit.Process_Observer;
with CuBit.Grant_References;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Program_Descriptions;
with CuBit.Outlet_Rings;
with CuBit.Child_Exits;
with CuBit.Launch_Authority;
with CuBit.Failures;
with CuBit.Filesystems;
with Windowed_Reads;
with CuBit.File_Access;
with CuBit.Network_Authority;
with CuBit.Render_Authority;
with CuBit.Render_Startup;
with CuBit.Launch_Policy; use CuBit.Launch_Policy;
with CuBit.Capability_Grants;
with Intel_GPU_Broker_Request;
with Intel_Render_Launch_Client;

procedure main is
   use ASCII;
   --  Never reuse issuance identities within this bootstrap issuer lifetime.
   --  Independent procmgr restart requires an epoch protocol, not yet supported.
   Next_Log_Issuance : Unsigned_32 := 1;
   --  Same rule for metrics.svc publisher/observer tags; zero = exhausted.
   Next_Metric_Issuance : Unsigned_32 := 1;
   package GPU_Grants renames CuBit.Capability_Grants;
   package Render_Launch is new Intel_Render_Launch_Client
     (Intel_GPU_Broker_Request.Launcher_Endpoint_Slot,
      16#4750_2000#, 16#4750_2FFF#);
   Render_Launcher : Render_Launch.Launcher;
   Render_Source_Slot : constant CapabilitySlot := 58;
   Failed_Launch_Control_Slot : constant CapabilitySlot := 59;
   --  procmgr's endpoint to the child being started, naming it as the
   --  recipient of the port rings derived for it (launcher-owned rings).
   Ring_Recipient_Slot : constant CapabilitySlot := 57;
   type Render_Request is record
      Requested, Valid : Boolean := False;
      Destination : CapabilitySlot := 0;
      Demand : CuBit.Render_Startup.Requirement := CuBit.Render_Startup.Required;
   end record;

   --  IPC label constants
   OP_SPAWN   : constant Unsigned_32 := 16#0100#;
   OP_LAUNCH  : constant Unsigned_32 := CuBit.Launch_Arguments.Launch_Operation;
   SYSCALL_INSTALL_LAUNCH_ARGUMENTS : constant Unsigned_64 := 126;
   --  The kernel's retirement notice (kernel/src/ipc_labels.ads).
   EVENT_CHILD_EXIT : constant Unsigned_32 := 16#0103#;
   REPLY_OK     : constant Unsigned_32 := 16#F000#;
   REPLY_ERR    : constant Unsigned_32 := 16#F001#;
   OP_SET_ACL   : constant Unsigned_32 := 16#0080#;

   --  Grant region constants (must match kernel process.ads)
   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096; -- 16 MiB

   --  ELF buffer — starts small, grows dynamically via sbrk when needed.
   INITIAL_BUF_PAGES : constant := 16;  --  64 KB starting size
   PAGE_SIZE         : constant := 4096;
   bufPages    : Natural := INITIAL_BUF_PAGES;
   bufCapacity : Natural := INITIAL_BUF_PAGES * PAGE_SIZE;

   --  FS capability slot
   CAP_SLOT_FS_LOCAL     : constant Unsigned_64 := 1;
   CAP_SLOT_CONFIG_LOCAL : constant Unsigned_64 := 2;
   Config_Storage_Selected : Boolean := False;

   --  Bounded launch-authority provenance ledger.  This records why procmgr
   --  attempted or installed authority; live authority remains authoritative
   --  in the kernel capability table.
   MAX_AUTHORITY_RECORDS : constant := 512;

   type Authority_Record is record
      valid       : Boolean := False;
      authorityId : Unsigned_32 := 0;
      pid         : Unsigned_64 := 0;
      slot        : Unsigned_8 := 0;
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

   --  procmgr's own policy endpoint to tls.svc (Policy_Tag) and a loan of
   --  elfBuf for scope lists, minted once tls.svc has registered.
   CAP_SLOT_TLS_LOCAL : constant Unsigned_64 := 60;
   TLS_Policy_PID : Unsigned_64 := 0;
   TLS_Grant : CuBit.Memory_Grants.Grant_Reference;
   TLS_Policy_Ready : Boolean := False;

   --  OP_LAUNCH: the validated launch block of the request being served
   --  (procmgr's own copy, never the requester's grant), installed into
   --  each child Spawn_Attempt creates for it. Zero bytes: no block.
   launchBlock : CuBit.Launch_Arguments.Block
     (1 .. CuBit.Launch_Arguments.Maximum_Block_Bytes) := [others => 0];
   pendingArgumentBytes : CuBit.Launch_Arguments.Block_Length := 0;
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
   MAX_LAUNCH_PID : constant := 255;
   launchStates : array (Unsigned_64 range 1 .. MAX_LAUNCH_PID) of Launch_State;
   --  The OP_LAUNCH requester whose authority bounds the child being
   --  spawned (0: none), and whether the child asked for more.
   attenuateFor : Unsigned_64 := 0;
   attenuationRefused : Boolean := False;
   --  The launched child could not read the working directory its launch
   --  block names; refused like an attenuation failure.
   directoryRefused : Boolean := False;
   --  The launched child's generation (CuBit.Child_Exits), for the reply.
   launchedGeneration : Unsigned_64 := 0;

   --  Return the ring grants procmgr acquired to derive pid's port rings.
   procedure releaseRings (pid : Unsigned_64) is
      Returned : Boolean;
   begin
      if pid in launchStates'Range then
         for P of launchStates (pid).Ring_Parents (1 .. launchStates (pid).Ring_Parent_Count) loop
            CuBit.Memory_Grants.Return_Acquisition (P, Returned);
         end loop;
         launchStates (pid).Ring_Parent_Count := 0;
      end if;
   end releaseRings;

   procedure resetLaunchState (pid : Unsigned_64) is
   begin
      if pid in launchStates'Range then
         launchStates (pid).Table_Length := 0;
         launchStates (pid).Services := 0;
         launchStates (pid).Scope_Count := 0;
      end if;
   end resetLaunchState;

   ---------------------------------------------------------------------------
   --  printDec - print a small unsigned number in decimal
   ---------------------------------------------------------------------------
   procedure printDec (val : Unsigned_32) is
      buf : String (1 .. 10);
      pos : Natural := buf'Last;
      v   : Unsigned_32 := val;
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

   ---------------------------------------------------------------------------
   --  sendReply - send a reply message to a waiting sender
   ---------------------------------------------------------------------------
   procedure sendReply
     (dest  : ProcessID;
      label : Unsigned_32;
      word0 : Unsigned_64)
   is
      replyMsg : Message := NULL_MESSAGE;
      ignore   : Unsigned_64;
   begin
      replyMsg.tag := (label  => label,
                       length => 1,
                       flags  => 0,
                       reserved  => 0);
      replyMsg.words := [0 => word0, others => 0];
      ignore := reply (dest, replyMsg);
   end sendReply;

   procedure recordAuthority
     (pid         : Unsigned_64;
      slot        : Unsigned_64;
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
      --  A slot has one launch-time explanation.  Replace an earlier record
      --  when later launch policy intentionally writes the same slot.
      for i in authorityRecords'Range loop
         if authorityRecords (i).valid and then
            authorityRecords (i).pid = pid and then
            authorityRecords (i).slot = Unsigned_8 (slot and 16#FF#)
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
         slot        => Unsigned_8 (slot and 16#FF#),
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
   procedure releaseNetworkOwner (owner : Unsigned_64) is
      request : Message := NULL_MESSAGE;
      ignore : MessageTag;
   begin
      if owner = 0 then
         return;
      end if;
      request.tag.label := CuBit.Network_Authority.OP_RELEASE_OWNER;
      request.tag.length := 1;
      request.words (0) := owner;
      ignore := capCall (CuBit.Network_Authority.Policy_Capability_Slot, request);
   end releaseNetworkOwner;

   --  filesystem.svc releases what Owner held: handles (buffered writes
   --  harvested first), its request queue and grants, its access profile
   --  (CuBit.Filesystems.OP_RELEASE_OWNER).
   procedure releaseFilesystemOwner (owner : Unsigned_64) is
      request : Message := NULL_MESSAGE;
      ignore : MessageTag;
   begin
      if owner = 0 then
         return;
      end if;
      request.tag.label := CuBit.Filesystems.OP_RELEASE_OWNER;
      request.tag.length := 1;
      request.words (0) := owner;
      ignore := capCall (CAP_SLOT_FS_LOCAL, request);
   end releaseFilesystemOwner;

   --  The kernel's process list still holds pid.
   Process_List_Bytes : constant := 8_192;
   Process_Entry_Bytes : constant := 32;
   processList : array (0 .. Process_List_Bytes - 1) of Unsigned_8 := [others => 0];
   function processListed (pid : Unsigned_64) return Boolean is
      count : constant Unsigned_64 :=
        syscall (SYSCALL_PROCLIST, Unsigned_64 (To_Integer (processList'Address)),
                 Process_List_Bytes);
   begin
      if count = Unsigned_64'Last then
         return True;   --  unknown: do not act on the claim
      end if;
      for i in 0 .. Natural (Unsigned_64'Min (count, Process_List_Bytes / Process_Entry_Bytes)) - 1 loop
         if (Unsigned_64 (processList (i * Process_Entry_Bytes)) or
             Shift_Left (Unsigned_64 (processList (i * Process_Entry_Bytes + 1)), 8)) = pid
         then
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
      Launcher : Unsigned_64 := 0;
      Started : Unsigned_64 := 0;
   end record;
   MAX_RECORDED_PID : constant := 255;
   processRecords : array (Unsigned_64 range 1 .. MAX_RECORDED_PID) of Process_Record;
   --  Observer issuances never wrap: exhaustion denies the role.
   Next_Process_Issuance : Unsigned_32 := 1;

   procedure noteProcess (pid, launcher : Unsigned_64; identity : String) is
      Length : constant Natural := Natural'Min (identity'Length, PO.Identity_Bytes);
   begin
      if pid not in processRecords'Range then return; end if;
      processRecords (pid) := (Valid => True, Identity => [others => ' '], Identity_Length => Length,
                               Launcher => launcher, Started => syscall (SYSCALL_GETTIME));
      processRecords (pid).Identity (1 .. Length) := identity (identity'First .. identity'First + Length - 1);
   end noteProcess;

   --  List: the kernel's table joined with processRecords, written into one
   --  page the caller lends. Only a holder of the observer tag may ask.
   procedure handleProcessList (sender : ProcessID; msg : Message) is
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped : System.Address;
      Ok, Returned : Boolean;
      Count, Written, Total : Natural := 0;
      replyMsg : Message := NULL_MESSAGE;
      ignore : Unsigned_64;
      procedure Answer (Result : PO.Status) is
      begin
         replyMsg.tag := (label => Unsigned_32 (PO.Status'Enum_Rep (Result)), length => 3, flags => 0, reserved => 0);
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
        (Reference, Unsigned_64 (sender), 0, PO.Page_Bytes, CuBit.Memory_Grants.Write_Access, Mapped, Ok);
      if not Ok then
         Answer (PO.Invalid_Request);
         return;
      end if;
      declare
         Page : array (0 .. PO.Page_Bytes - 1) of Unsigned_8 with Import, Address => Mapped;
         Listed : constant Unsigned_64 :=
           syscall (SYSCALL_PROCLIST, Unsigned_64 (To_Integer (processList'Address)), Process_List_Bytes);
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
               E : constant Natural := I * Process_Entry_Bytes;
               Pid : constant Unsigned_64 :=
                 Unsigned_64 (processList (E)) or Shift_Left (Unsigned_64 (processList (E + 1)), 8);
               Name_Length : Natural := 0;
               SUSPENDED_STATE : constant := 10;
            begin
               for J in 0 .. PO.Name_Bytes - 1 loop
                  if processList (E + 8 + J) not in 0 | 32 then Name_Length := J + 1; end if;
               end loop;
               --  Reserved slots are suspended and never named.
               if not (Name_Length = 0 and then processList (E + 2) = SUSPENDED_STATE) then
                  Total := Total + 1;
                  if Written < PO.Page_Records then
                     declare
                        R : constant Natural := Written * PO.Record_Bytes;
                     begin
                        Put (R + PO.Pid_Offset, Pid, 4);
                        Page (R + PO.State_Offset) := processList (E + 2);
                        Page (R + PO.CPU_Offset) := processList (E + 3);
                        Page (R + PO.Priority_Offset) := processList (E + 4);
                        Page (R + PO.Priority_Offset + 1) := processList (E + 5);
                        for J in 0 .. 3 loop
                           Page (R + PO.Frames_Offset + J) := processList (E + 24 + J);
                        end loop;
                        Page (R + PO.Name_Length_Offset) := Unsigned_8 (Name_Length);
                        for J in 0 .. Name_Length - 1 loop
                           Page (R + PO.Name_Offset + J) := processList (E + 8 + J);
                        end loop;
                        if Pid in processRecords'Range and then processRecords (Pid).Valid then
                           declare
                              Known : Process_Record renames processRecords (Pid);
                           begin
                              Put (R + PO.Launcher_Offset, Known.Launcher, 4);
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

   procedure clearAuthorityForPID (pid : Unsigned_64) is
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
           (slot => CAP_SLOT_FS_LOCAL,
            localAddr => elfBuf + Storage_Offset (Offset),
            numPages => (Count + PAGE_SIZE - 1) / PAGE_SIZE,
            readWrite => True, reference => Loan, success => Ok);
         if not Ok then
            debugPrint ("procmgr: file window grant failed" & LF);
            return;
         end if;
         msg := CuBit.Filesystems.Read_Request (handle, Loan, Unsigned_64 (Count));
         tag := capCall (CAP_SLOT_FS_LOCAL, msg);
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
      tag := capCall (CAP_SLOT_FS_LOCAL, msg);

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
      tag := capCall (CAP_SLOT_FS_LOCAL, msg);

      return (if Complete and then tag.label = REPLY_OK then fileSize else 0);
   end readFileFromFS;

   ---------------------------------------------------------------------------
   --  Manifest constants
   ---------------------------------------------------------------------------
   MANIFEST_MAGIC   : constant Unsigned_32 := 16#43424954#;  -- "CBIT" LE
   ID_MAGIC         : constant Unsigned_32 := 16#44494243#;  -- "CBID" LE

   --  Manifest request types
   REQ_FRAMEBUFFER  : constant Unsigned_8 := 1;
   REQ_SERVICE      : constant Unsigned_8 := 2;
   REQ_IOPORT       : constant Unsigned_8 := 3;
   REQ_NOTIFICATION : constant Unsigned_8 := 7;
   REQ_RESOURCE     : constant Unsigned_8 := 9;

   --  Stream notification label
   OP_STREAM_AVAILABLE : constant Unsigned_32 := 16#0706#;

   --  Config store IPC label
   OP_CONFIG_GET : constant Unsigned_32 := 16#0600#;

   --  Well-known slot for config-based resource quota
   RESOURCE_CAP_SLOT : constant Unsigned_64 := 62;

   --  ELF section header type for PROGBITS
   SHT_PROGBITS     : constant Unsigned_32 := 1;

   --  Capability type position values (must match kernel CapabilityType enum)
   CAP_TYPE_ENDPOINT      : constant Unsigned_64 := 1;
   CAP_TYPE_NOTIFICATION  : constant Unsigned_64 := 2;
   CAP_TYPE_IOPORT        : constant Unsigned_64 := 4;
   CAP_TYPE_PROCESS       : constant Unsigned_64 := 6;
   CAP_TYPE_DEVICE_MEM    : constant Unsigned_64 := 7;
   CAP_TYPE_RESOURCE      : constant Unsigned_64 := 9;

   CAP_SLOT_SERVICE_REG   : constant Unsigned_64 := 7;

   procedure mintRecorded
     (childPID   : Unsigned_64;
      capType    : Unsigned_64;
      objectRef  : Unsigned_64;
      objectParam : Unsigned_64;
      rights     : Unsigned_64;
      slot       : Unsigned_64;
      source     : Unsigned_8;
      reason     : Unsigned_8;
      requested  : Boolean;
      result     : out Unsigned_64)
   is
   begin
      result := syscall
        (SYSCALL_POLICY_MINT_CAPABILITY, childPID, capType, objectRef,
         objectParam, rights, slot);
      recordAuthority
        (pid         => childPID,
         slot        => slot,
         source      => source,
         reason      => (if result = Unsigned_64'Last
                         then AUTH_REASON_MINT_FAILED else reason),
         capType     => capType,
         requested   => requested,
         granted     => result /= Unsigned_64'Last,
         rights      => rights,
         objectRef   => objectRef,
         objectParam => objectParam);
   end mintRecorded;

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
   --  parseAndGrantManifest
   --  Parse the .cubit.caps section from the ELF in elfBuf and mint
   --  capabilities into the child process via SYSCALL_POLICY_MINT_CAPABILITY.
   ---------------------------------------------------------------------------
   --  Whether the launcher being attenuated for holds what this manifest
   --  request asks: the same service role, or the child's own streams and
   --  resource limits. Devices, notifications, framebuffer, render and
   --  network are never passed to a launched child.
   function launcherHolds (reqType : Unsigned_8; role : Unsigned_32)
     return Boolean is
   begin
      if reqType = REQ_RESOURCE then
         return True;
      elsif reqType = REQ_SERVICE and then role < TRACKED_SERVICE_ROLES then
         return (launchStates (attenuateFor).Services and
                 Shift_Left (Unsigned_64'(1), Natural (role))) /= 0;
      end if;
      return False;
   end launcherHolds;

   procedure noteService (pid : Unsigned_64; role : Unsigned_32) is
   begin
      if pid in launchStates'Range and then role < TRACKED_SERVICE_ROLES then
         launchStates (pid).Services := launchStates (pid).Services or
           Shift_Left (Unsigned_64'(1), Natural (role));
      end if;
   end noteService;

   procedure parseAndGrantManifest
     (childPID      : Unsigned_64;
      elfSize       : Unsigned_64;
      render : in out Render_Request;
      approveNetwork : Network_Approval := No_Network;
      systemStartup : Boolean := False;
      approveLogViewer : Boolean := False;
      approveProcessViewer : Boolean := False;
      approveLogControl : Boolean := False)
   is
      --  ELF64 header field offsets
      e_shoff_off     : constant := 40;  -- Section header table offset
      e_shentsize_off : constant := 58;  -- Section header entry size
      e_shnum_off     : constant := 60;  -- Number of section headers

      e_shoff     : Unsigned_64;
      e_shentsize : Unsigned_16;
      e_shnum     : Unsigned_16;
   begin
      if elfSize < 64 then
         return;
      end if;

      e_shoff     := readU64 (e_shoff_off);
      e_shentsize := readU16 (e_shentsize_off);
      e_shnum     := readU16 (e_shnum_off);

      --  Validate section header table
      if e_shentsize /= 64 then
         debugPrint ("procmgr: unexpected shentsize" & LF);
         return;
      end if;

      if e_shoff = 0 or e_shnum = 0 then
         return;
      end if;

      if e_shoff + Unsigned_64 (e_shnum) * 64 > elfSize then
         debugPrint ("procmgr: section headers beyond ELF" & LF);
         return;
      end if;

      --  Scan section headers for .cubit.caps (PROGBITS with magic match)
      for i in 0 .. Unsigned_16'(e_shnum - 1) loop
         declare
            shBase  : constant Unsigned_64 :=
               e_shoff + Unsigned_64 (i) * 64;
            sh_type : constant Unsigned_32 := readU32 (shBase + 4);
            sh_offset : Unsigned_64;
            sh_size   : Unsigned_64;
         begin
            if sh_type = SHT_PROGBITS then
               sh_offset := readU64 (shBase + 24);
               sh_size   := readU64 (shBase + 32);

               --  Need at least 8 bytes for header (magic + version + count)
               if sh_size >= 8 and then
                  sh_offset + sh_size <= elfSize and then
                  readU32 (sh_offset) = MANIFEST_MAGIC
               then
                  --  Found manifest section
                  declare
                     version : constant Unsigned_16 :=
                        readU16 (sh_offset + 4);
                     count   : constant Unsigned_16 :=
                        readU16 (sh_offset + 6);
                     entryBase     : Unsigned_64;
                     reqType       : Unsigned_8;
                     rights        : Unsigned_8;
                     slotNum       : Unsigned_8;
                     param0        : Unsigned_32;
                     param1        : Unsigned_64;
                     rightsMask    : Unsigned_64;
                     ignore        : Unsigned_64;
                  begin
                     if version /= 1 then
                        debugPrint ("procmgr: unknown manifest v" & LF);
                        return;
                     end if;

                     --  Validate all entries fit
                     if sh_size < 8 + Unsigned_64 (count) * 16 then
                        debugPrint ("procmgr: manifest truncated" & LF);
                        return;
                     end if;

                     debugPrint ("procmgr: manifest has ");
                     printDec (Unsigned_32 (count));
                     debugPrint (" cap entries" & LF);

                     --  Do not form count - 1 when the manifest is empty.
                     --  With userspace range checks suppressed, underflow
                     --  would scan unrelated ELF bytes as capability requests
                     --  and could mint authority that was never declared.
                     if count > 0 then
                     for j in 0 .. Unsigned_16'(count - 1) loop
                        entryBase := sh_offset + 8 +
                           Unsigned_64 (j) * 16;
                        reqType := readU8 (entryBase);
                        rights  := readU8 (entryBase + 1);
                        slotNum := readU8 (entryBase + 2);
                        param0  := readU32 (entryBase + 4);
                        param1  := readU64 (entryBase + 8);

                        rightsMask := Unsigned_64 (rights);

                        if attenuateFor /= 0
                          and then not launcherHolds (reqType, param0)
                        then
                           attenuationRefused := True;
                           debugPrint ("procmgr: launched child requests " &
                             "authority its launcher does not hold" & LF);
                        elsif reqType = CuBit.Render_Authority.Manifest_Request then
                           -- A render endpoint is admitted by devmgr, never
                           -- minted by the generic service-capability path.
                           render.Valid := not render.Requested and then
                             rightsMask = 3 and then slotNum in 1 .. 62 and then
                             param0 in 0 .. 1 and then param1 = 0 and then
                             readU8 (entryBase + 3) = 0;
                           render.Requested := True;
                           if render.Valid then
                              render.Destination := CapabilitySlot (slotNum);
                              render.Demand := (if param0 = 1 then CuBit.Render_Startup.Optional
                                                else CuBit.Render_Startup.Required);
                           end if;
                        else
                        case reqType is
                           when CuBit.Network_Authority.Manifest_Request =>
                              declare
                                 scope : CuBit.Network_Authority.Scope;
                                 valid : Boolean;
                                 request : Message := NULL_MESSAGE;
                                 resultTag : MessageTag;
                                 networkPID : constant Unsigned_64 := getInfo
                                   (SYSINFO_REGISTERED_DRIVER, DRIVER_NETSTACK);
                              begin
                                 CuBit.Network_Authority.Decode
                                   (Unsigned_64 (param0), param1, scope, valid);
                                 if valid and then Allows (approveNetwork, scope) and then
                                   rightsMask = 3 and then slotNum in 1 .. 62 and then
                                   networkPID /= 0 and then networkPID /= Unsigned_64'Last
                                 then
                                    request.tag.label := CuBit.Network_Authority.OP_INSTALL_SCOPE;
                                    request.tag.length := 3;
                                    request.words (0) := childPID;
                                    request.words (1) := Unsigned_64 (scope.Network);
                                    request.words (2) := CuBit.Network_Authority.Descriptor (scope);
                                    resultTag := capCall
                                      (CuBit.Network_Authority.Policy_Capability_Slot, request);
                                    if resultTag.label = REPLY_OK and then
                                      request.words (0) in
                                        CuBit.Network_Authority.First_Grant_Tag ..
                                        CuBit.Network_Authority.Last_Grant_Tag
                                    then
                                       declare
                                          authorityTag : constant Unsigned_64 := request.words (0);
                                       begin
                                          mintRecorded
                                            (childPID, CAP_TYPE_ENDPOINT, networkPID,
                                             authorityTag, rightsMask, Unsigned_64 (slotNum),
                                             AUTH_SOURCE_CONFIG_POLICY,
                                             AUTH_REASON_MANIFEST_REQUEST, True, ignore);
                                          if ignore = Unsigned_64'Last then
                                             request := NULL_MESSAGE;
                                             request.tag.label := CuBit.Network_Authority.OP_RELEASE_SCOPE;
                                             request.tag.length := 2;
                                             request.words (0) := childPID;
                                             request.words (1) := authorityTag;
                                             resultTag := capCall
                                               (CuBit.Network_Authority.Policy_Capability_Slot, request);
                                          end if;
                                       end;
                                    end if;
                                 else
                                    debugPrint ("procmgr: network scope denied (not approved or invalid)" & LF);
                                 end if;
                              end;

                           when REQ_FRAMEBUFFER =>
                              --  CAP_DEVICE_MEM, ref=0, param=0x1000_0000
                              mintRecorded
                                (childPID, CAP_TYPE_DEVICE_MEM, 0,
                                 16#1000_0000#, rightsMask,
                                 Unsigned_64 (slotNum), AUTH_SOURCE_MANIFEST,
                                 AUTH_REASON_MANIFEST_REQUEST, True, ignore);
                              debugPrint ("procmgr: minted FB cap" & LF);

                           when REQ_IOPORT =>
                              --  CAP_IOPORT. param0 is the base I/O port.
                              --  param1 low 16 bits optionally gives the port
                              --  count; older manifests leave it zero and get
                              --  the original one-port grant.
                              if param1 = 0 then
                                 param1 := 1;
                              end if;
                              debugPrint ("procmgr: i/o port request base=");
                              printDec (param0 and 16#FFFF#);
                              debugPrint (" count=");
                              printDec (Unsigned_32 (param1 and 16#FFFF#));
                              debugPrint (" slot=");
                              printDec (Unsigned_32 (slotNum));
                              debugPrint ("" & LF);
                              mintRecorded
                                (childPID, CAP_TYPE_IOPORT,
                                 Unsigned_64 (param0 and 16#FFFF#),
                                 param1 and 16#FFFF#, rightsMask,
                                 Unsigned_64 (slotNum), AUTH_SOURCE_MANIFEST,
                                 AUTH_REASON_MANIFEST_REQUEST, True, ignore);
                              if ignore = Unsigned_64'Last then
                                 debugPrint ("procmgr: i/o port cap mint failed" & LF);
                              else
                                 debugPrint ("procmgr: minted i/o port cap" & LF);
                              end if;

                           when REQ_SERVICE =>
                              --  Look up driver PID via sysinfo,
                              --  retrying briefly if not yet registered.
                              declare
                                 driverPID : Unsigned_64 := 0;
                                 MAX_RETRIES : constant := 20;
                                 isAudioControl : constant Boolean :=
                                   Unsigned_64 (param0) = CuBit.Audio_Control.Service_Role;
                                 isLogObserver : constant Boolean :=
                                   Unsigned_64 (param0) = CuBit.Log_Protocol.Observer_Service_Role;
                                 isLogPublisher : constant Boolean :=
                                   Unsigned_64 (param0) = DRIVER_LOGSTORE;
                                 isClockControl : constant Boolean :=
                                   Unsigned_64 (param0) = CuBit.Clock_Control.Service_Role;
                                 isMetricObserver : constant Boolean :=
                                   Unsigned_64 (param0) = CuBit.Metric_Protocol.Observer_Service_Role;
                                 isMetricPublisher : constant Boolean :=
                                   Unsigned_64 (param0) = CuBit.Metric_Protocol.Publisher_Service_Role;
                                 isProcessObserver : constant Boolean :=
                                   Unsigned_64 (param0) = CuBit.Process_Observer.Observer_Service_Role;
                                 isLogControl : constant Boolean :=
                                   Unsigned_64 (param0) = CuBit.Log_Protocol.Control_Service_Role;
                                 use CuBit.Authority_Policy;
                                 Authority : constant Bootstrap_Authority :=
                                   (if isAudioControl then Master_Audio
                                    elsif isClockControl then Clock_Adjustment
                                    elsif isLogObserver then Log_Observation
                                    elsif isMetricObserver then Metric_Observation
                                    elsif isMetricPublisher then Metric_Publication
                                    elsif isProcessObserver then Process_Observation
                                    elsif isLogControl then Log_Control
                                    else Log_Publication);
                                 Approval : constant Decision := Evaluate
                                   (Requested => True,
                                    Installation_Approved => Bootstrap_Approves
                                      (Authority, systemStartup) or else
                                      (isLogObserver and then approveLogViewer) or else
                                      (isProcessObserver and then approveProcessViewer) or else
                                      (isLogControl and then approveLogControl),
                                    Session_Approved => True,
                                    Issuer_Allowed => True);
                                 Issued_Tag : Unsigned_64 := 0;
                              begin
                                 -- Startup approval or the trusted Desktop's
                                 -- narrowly selected system log viewer. This
                                 -- exception grants observation only, not the
                                 -- other startup-only authorities.
                                 if (isAudioControl or isLogObserver or isClockControl
                                     or isMetricObserver or isProcessObserver or isLogControl)
                                   and then Approval /= Approved
                                 then
                                    recordAuthority
                                      (childPID, Unsigned_64 (slotNum),
                                       AUTH_SOURCE_MANIFEST,
                                       AUTH_REASON_STARTUP_REQUIRED,
                                       CAP_TYPE_ENDPOINT, True, False,
                                       rightsMask, Unsigned_64 (param0), 0);
                                    debugPrint ("procmgr: startup-only authority denied" & LF);
                                 else
                                 for attempt in 1 .. MAX_RETRIES loop
                                    driverPID := getInfo (
                                       SYSINFO_REGISTERED_DRIVER,
                                       (if isAudioControl then DRIVER_MIXER
                                        elsif isClockControl then DRIVER_CLOCK
                                        elsif isLogObserver or isLogControl then DRIVER_LOGSTORE
                                        elsif isMetricObserver then
                                          CuBit.Metric_Protocol.Publisher_Service_Role
                                        elsif isProcessObserver then DRIVER_PROCMGR
                                        else Unsigned_64 (param0)));
                                    exit when driverPID /= 0;
                                    ignore := syscall (
                                       SYSCALL_SLEEP, 50);
                                 end loop;

                                 if isAudioControl then
                                    Issued_Tag := CuBit.Audio_Control.Authority_Tag;
                                 elsif isClockControl then
                                    Issued_Tag := CuBit.Clock_Control.Authority_Tag;
                                 elsif (isLogObserver or isLogPublisher or isLogControl)
                                   and then Next_Log_Issuance /= 0
                                 then
                                    if isLogObserver then
                                       Issued_Tag := CuBit.Log_Protocol.Observer_Tag_Base +
                                         Unsigned_64 (Next_Log_Issuance);
                                    elsif isLogControl then
                                       Issued_Tag := CuBit.Log_Protocol.Control_Tag
                                         (Unsigned_64 (Next_Log_Issuance));
                                    else
                                       Issued_Tag := CuBit.Log_Protocol.Publisher_Tag
                                         (CuBit.Log_Protocol.Bootstrap_Budget,
                                          Unsigned_64 (Next_Log_Issuance));
                                    end if;
                                    if Next_Log_Issuance = Unsigned_32
                                      (CuBit.Log_Protocol.Publication_Issuance'Last)
                                    then
                                       Next_Log_Issuance := 0;
                                    else
                                       Next_Log_Issuance := Next_Log_Issuance + 1;
                                    end if;
                                 end if;
                                 if (isMetricObserver or isMetricPublisher)
                                   and then Next_Metric_Issuance /= 0
                                 then
                                    Issued_Tag :=
                                      (if isMetricObserver
                                       then CuBit.Metric_Protocol.Observer_Tag
                                         (Unsigned_64 (Next_Metric_Issuance))
                                       else CuBit.Metric_Protocol.Publisher_Tag
                                         (Unsigned_64 (Next_Metric_Issuance)));
                                    --  Never wrap: exhaustion denies issuance.
                                    Next_Metric_Issuance :=
                                      (if Next_Metric_Issuance = Unsigned_32'Last
                                       then 0 else Next_Metric_Issuance + 1);
                                 end if;
                                 if isProcessObserver and then Next_Process_Issuance /= 0 then
                                    Issued_Tag := CuBit.Process_Observer.Observer_Tag (Next_Process_Issuance);
                                    Next_Process_Issuance :=
                                      (if Next_Process_Issuance = Unsigned_32'Last
                                       then 0 else Next_Process_Issuance + 1);
                                 end if;
                                 if (isLogObserver or isLogPublisher or isLogControl
                                     or isMetricObserver or isMetricPublisher or isProcessObserver)
                                   and then Issued_Tag = 0
                                 then
                                    recordAuthority
                                      (childPID, Unsigned_64 (slotNum),
                                       AUTH_SOURCE_MANIFEST,
                                       AUTH_REASON_MINT_FAILED,
                                       CAP_TYPE_ENDPOINT, True, False,
                                       rightsMask, Unsigned_64 (param0), 0);
                                    debugPrint ("procmgr: telemetry issuance exhausted" & LF);
                                 elsif driverPID /= 0 then
                                    mintRecorded
                                      (childPID, CAP_TYPE_ENDPOINT, driverPID,
                                       Issued_Tag, rightsMask, Unsigned_64 (slotNum),
                                       AUTH_SOURCE_MANIFEST,
                                       AUTH_REASON_MANIFEST_REQUEST, True,
                                       ignore);
                                    if ignore /= Unsigned_64'Last then
                                       noteService (childPID, param0);
                                    end if;
                                    debugPrint (
                                       "procmgr: minted svc cap" & LF);
                                 else
                                    recordAuthority
                                      (childPID, Unsigned_64 (slotNum),
                                       AUTH_SOURCE_MANIFEST,
                                       AUTH_REASON_SERVICE_MISSING,
                                       CAP_TYPE_ENDPOINT, True, False,
                                       rightsMask, Unsigned_64 (param0), 0);
                                    debugPrint (
                                       "procmgr: driver not found" & LF);
                                 end if;
                                 end if;
                              end;

                           when REQ_NOTIFICATION =>
                              -- Registration/focus authority for one declared
                              -- service role. This replaces the former ambient
                              -- keyboard/mouse grants. Manifest admission is
                              -- still subject to the policy layer.
                              if param0 >= 1 and then param0 <= 17 then
                                 mintRecorded
                                   (childPID, CAP_TYPE_NOTIFICATION,
                                    Unsigned_64 (param0), 0, rightsMask,
                                    Unsigned_64 (slotNum),
                                    AUTH_SOURCE_MANIFEST,
                                    AUTH_REASON_MANIFEST_REQUEST, True,
                                    ignore);
                                 debugPrint (
                                   "procmgr: minted notification cap" & LF);
                              else
                                 debugPrint (
                                   "procmgr: invalid notification role" & LF);
                              end if;

                           when REQ_RESOURCE =>
                              --  param0 = maxFrames
                              --  param1 = cpuQuotaUs(lo32)|cpuPeriodUs(hi32)
                              declare
                                 param1 : constant Unsigned_64 :=
                                    readU64 (entryBase + 8);
                              begin
                                 mintRecorded
                                   (childPID, CAP_TYPE_RESOURCE,
                                    Unsigned_64 (param0), param1, rightsMask,
                                    Unsigned_64 (slotNum), AUTH_SOURCE_MANIFEST,
                                    AUTH_REASON_MANIFEST_REQUEST, True, ignore);
                                 debugPrint (
                                    "procmgr: minted resource cap" &
                                    LF);
                              end;

                           when others =>
                              debugPrint (
                                 "procmgr: unknown req type" & LF);
                        end case;
                        end if;
                     end loop;
                     end if;

                  end;

                  --  Only process first matching manifest section
                  return;
               end if;
            end if;
         end;
      end loop;
   end parseAndGrantManifest;

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
   --  Once tls.svc has registered, mint procmgr's own Policy_Tag endpoint to
   --  it and lend it elfBuf for scope lists. False while tls.svc is absent:
   --  TLS scopes are then not installed, so clients are denied, never
   --  broadened. A restarted tls.svc (new PID) is not yet re-bound.
   ---------------------------------------------------------------------------
   function ensureTLSPolicy return Boolean is
      TLS_PID : constant Unsigned_64 :=
        getInfo (SYSINFO_REGISTERED_DRIVER, CuBit.TLS_Protocol.Service_Role);
      Minted : Unsigned_64;
   begin
      if TLS_Policy_Ready then
         return TLS_PID = TLS_Policy_PID;
      end if;
      if TLS_PID = 0 or else TLS_PID = Unsigned_64'Last then
         return False;
      end if;
      mintRecorded
        (syscall (SYSCALL_GETPID), CAP_TYPE_ENDPOINT, TLS_PID,
         CuBit.TLS_Protocol.Policy_Tag, 3, CAP_SLOT_TLS_LOCAL,
         AUTH_SOURCE_KERNEL_BOOTSTRAP, AUTH_REASON_SELF_BOOTSTRAP, False,
         Minted);
      if Minted = Unsigned_64'Last then
         debugPrint ("procmgr: tls policy endpoint mint failed" & LF);
         return False;
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (CAP_SLOT_TLS_LOCAL, elfBuf, INITIAL_BUF_PAGES, True,
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
     (childPID : Unsigned_64; batch : System.Address; batchEntries : Natural;
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
      if pendingGrantBytes = 0 or else attenuateFor not in launchStates'Range then
         ok := pendingGrantBytes = 0;
         return;
      end if;
      for g in 1 .. delegatedCount loop
         LG.Next (pendingGrants (1 .. pendingGrantBytes), position, rights, first, last);
         declare
            name : constant String :=
              [for k in first .. last => Character'Val (pendingGrants (k))];
            Launcher : Launch_State renames launchStates (attenuateFor);
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
            if childPID in launchStates'Range then
               declare
                  Held : Launch_State renames launchStates (childPID);
                  prefix : String (1 .. LAuth.Maximum_Prefix_Bytes) := [others => ' '];
               begin
                  prefix (1 .. name'Length) := name;
                  if Held.Scope_Count < Held.Scopes'Last then
                     Held.Scope_Count := Held.Scope_Count + 1;
                     Held.Scopes (Held.Scope_Count) :=
                       (Service => SERVICE_FS, Rights => rights,
                        Length => name'Length, Prefix => prefix);
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
     (childPID        : Unsigned_64;
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
                           if attenuateFor /= 0 then
                              declare
                                 Launcher : Launch_State renames
                                   launchStates (attenuateFor);
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
                           if childPID in launchStates'Range then
                              declare
                                 Held : Launch_State renames
                                   launchStates (childPID);
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
                                               locals (j).prefix'Length => ' ']);
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
                                             flags  => 0,
                                             reserved  => 0);
                              aclMsg.words := [0 => childPID,
                                               1 => Unsigned_64 (fsCount + delegatedCount),
                                               2 => fsGrant.slot,
                                               3 => fsGrant.generation];
                              aclTag := capCall (
                                 CAP_SLOT_FS_LOCAL, aclMsg);
                              if aclTag.label /= REPLY_OK or else aclTag.length /= 1 or else
                                aclTag.flags /= 0 or else aclTag.reserved /= 0
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
                                             flags  => 0,
                                             reserved  => 0);
                              aclMsg.words := [
                                 0 => childPID,
                                 1 => Unsigned_64 (configCount),
                                 2 => Config_Grant.slot,
                                 3 => Config_Grant.generation];
                              aclTag := capCall (
                                 CAP_SLOT_CONFIG_LOCAL, aclMsg);
                              if aclTag.label /= REPLY_OK or else aclTag.length /= 1 or else
                                aclTag.flags /= 0 or else aclTag.reserved /= 0
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
                                    length => 4, flags => 0, reserved => 0);
                                 tlsMsg.words :=
                                   [childPID, Unsigned_64 (tlsCount),
                                    TLS_Grant.slot, TLS_Grant.generation];
                                 tlsTag := capCall (CAP_SLOT_TLS_LOCAL, tlsMsg);
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
            aclMsg.tag := (label => OP_SET_ACL, length => 4, flags => 0, reserved => 0);
            aclMsg.words := [0 => childPID, 1 => Unsigned_64 (delegatedCount),
                             2 => fsGrant.slot, 3 => fsGrant.generation];
            aclTag := capCall (CAP_SLOT_FS_LOCAL, aclMsg);
            if aclTag.label /= REPLY_OK or else aclTag.length /= 1 or else
              aclTag.flags /= 0 or else aclTag.reserved /= 0
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
   function launchDirectoryVisible (childPID : Unsigned_64) return Boolean is
      package LA renames CuBit.Launch_Arguments;
      use type LA.Validation;
      Length : constant LA.Block_Length := pendingArgumentBytes;
      Read_Only : constant CuBit.File_Access.Rights_Set :=
        [CuBit.File_Access.Read_Objects => True, others => False];
   begin
      if attenuateFor = 0 or else Length = 0 then
         return True;
      elsif LA.Validate (launchBlock (1 .. Length)) /= LA.Valid then
         return False;
      elsif LA.Directory_Declared (launchBlock (1 .. Length)) = 0 then
         return True;
      elsif childPID not in launchStates'Range then
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
            Held : Launch_State renames launchStates (childPID);
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
                        flags  => 0,
                        reserved  => 0);
         cfgMsg.words (0) := Config_Grant.slot;
         cfgMsg.words (1) := Config_Grant.generation;
         cfgMsg.words (2) := Unsigned_64 (totalLen);
         cfgMsg.tag := capCall (CAP_SLOT_CONFIG_LOCAL, cfgMsg);

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
   --  Returns PID on success, 0 on failure.
   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------
   --  loadLaunchTable
   --  Keep pid's .cubit.launch table (from the ELF in elfBuf) if it is well
   --  formed; without one, the process may start nothing with OP_LAUNCH.
   ---------------------------------------------------------------------------
   procedure loadLaunchTable (elfSize : Unsigned_64; pid : Unsigned_64) is
      e_shoff : Unsigned_64;
      e_shnum : Unsigned_16;
   begin
      if pid not in launchStates'Range or else elfSize < 64 then
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
                     launchStates (pid).Table (1 .. Length) := Source;
                     launchStates (pid).Table_Length := Length;
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
   ---------------------------------------------------------------------------
   function deriveRings
     (childPID : Unsigned_64; Description : CuBit.Launch_Arguments.Block)
      return CuBit.Outlet_Rings.Table
   is
      package PD renames CuBit.Program_Descriptions;
      use type PD.Connector_Direction;
      Result : CuBit.Outlet_Rings.Table;
      S : PD.Signature;
      Decoded : Boolean;
      Copy : PD.Bytes (1 .. Description'Length);
      Minted : Unsigned_64;
   begin
      if pendingRings.Count = 0 or else attenuateFor = 0
        or else childPID not in launchStates'Range
        or else Description'Length > PD.Maximum_Descriptor_Bytes
      then
         if attenuateFor /= 0 then
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
      Minted := syscall (SYSCALL_POLICY_MINT_CAPABILITY, syscall (SYSCALL_GETPID),
        CAP_TYPE_ENDPOINT, childPID, 0, 3, Unsigned_64 (Ring_Recipient_Slot));
      if Minted = Unsigned_64'Last then
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
               Pages : constant Natural := S.Connectors (E.Outlet).Pages;
               Mapped : System.Address;
               Child : CuBit.Memory_Grants.Grant_Reference;
               Ok : Boolean;
               State : Launch_State renames launchStates (childPID);
            begin
               CuBit.Memory_Grants.Acquire
                 (Parent, attenuateFor, 0, Unsigned_64 (Pages) * PAGE_SIZE,
                  CuBit.Memory_Grants.Write_Access, Mapped, Ok);
               if not Ok then
                  debugPrint ("procmgr: port ring not acquired from the launcher" & LF);
               end if;
               if Ok then
                  CuBit.Memory_Grants.Derive_Via_Capability
                    (Ring_Recipient_Slot, Parent, 0, Pages, True, Child, Ok);
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
      Prior_Incarnation : Unsigned_64;
      Retry : out Boolean;
      Identity : out Unsigned_64;
      name        : String;
      priority    : Unsigned_64;
      requester   : Unsigned_64 := 0;
      sandboxMode : Unsigned_8 := SANDBOX_NONE;
      cwd         : String := "";
      approveNetwork : Network_Approval := No_Network;
      approveRender : Boolean := False;
      systemStartup : Boolean := False;
      startupRole : CCL.Configurations.Startup_Role := CCL.Configurations.Application)
      return Unsigned_64
   is
      elfSize       : Unsigned_64;
      newPID        : Unsigned_64;
      pri           : Unsigned_64 := priority;
      t0, t1        : Unsigned_64;
      pkgId         : String (1 .. 128);
      pkgIdLen      : Natural := 0;
      streamBitmask : Unsigned_64 := 0;
      Render : Render_Request;
      Captured_Child : GPU_Grants.Recipient;
      Stop_Accepted : Boolean := False;
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
        and then requester /= 0 and then
        sandboxMode = SANDBOX_NONE and then cwd'Length = 0 and then
        requester = getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP);
      --  The CCL Console's and Workbench's :ps / (proc.list) see what runs,
      --  under the same narrow desktop-launched exception.
      Process_Viewer_Approved : constant Boolean :=
        (name = "ccl-workbench.app" or else name = "ccl-console.app" or else name = "logs.app")
        and then requester /= 0 and then
        sandboxMode = SANDBOX_NONE and then cwd'Length = 0 and then
        requester = getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP);
      --  Logs, the console and the Workbench may change what logstore keeps
      --  (log-control), under the same desktop-launched exception.
      Log_Control_Approved : constant Boolean := Process_Viewer_Approved;
      use type CCL.Configurations.Startup_Role;
      procedure Stop_Suspended_Child is
         Result : Unsigned_64;
      begin
         -- Use existing explicit policy authority to obtain WRITE only for
         -- this rejected child, not a wildcard process-kill capability.
         -- The kernel stamps the referenced process generation; a retained
         -- stale slot cannot authorize killing a later occupant of its PID.
         Stop_Accepted := False;
         if GPU_Grants.Valid (Captured_Child) then
            -- The control capability was captured before admission. Never
            -- remint it by PID after waiting: the PID may have been reused.
            if GPU_Grants.Incarnation (GPU_Grants.Capture (Failed_Launch_Control_Slot)) /=
              GPU_Grants.Incarnation (Captured_Child)
            then
               debugPrint ("procmgr: failed launch process cleanup rejected" & LF);
               return;
            end if;
            Result := 0;
         else
            Result := syscall (SYSCALL_POLICY_MINT_CAPABILITY,
              syscall (SYSCALL_GETPID), CAP_TYPE_PROCESS, newPID, 0, 2,
              Unsigned_64 (Failed_Launch_Control_Slot));
         end if;
         if Result = Unsigned_64'Last then
            debugPrint ("procmgr: failed launch process cleanup rejected" & LF);
            return;
         end if;
         Result := syscall (SYSCALL_KILL, newPID);
         if Result /= 0 then
            debugPrint ("procmgr: failed launch process cleanup rejected" & LF);
         else
            Stop_Accepted := True;
            debugPrint ("procmgr: failed launch child stop requested" & LF);
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
                    length => 1, flags => 0, reserved => 0);
                  Cleanup.words (0) := newPID;
                  Tag := capCall
                    ((if Service = Files then CAP_SLOT_FS_LOCAL else CAP_SLOT_CONFIG_LOCAL), Cleanup);
                  if Tag.label /= REPLY_OK then
                     debugPrint ("procmgr: failed launch policy cleanup rejected" & LF);
                  end if;
               end;
            end if;
         end loop;
         Stop_Suspended_Child;
      end Discard_Authorized_Child;
   begin
      Retry := False;
      Identity := 0;
      if startupRole = CCL.Configurations.Config_Storage and then
        (not systemStartup or Config_Storage_Selected)
      then
         debugPrint ("procmgr: Config storage role not available" & LF);
         return 0;
      end if;
      debugPrint ("procmgr: spawn: ");
      debugPrint (name);
      debugPrint ("" & LF);

      t0 := syscall (SYSCALL_GETTIME);
      elfSize := readFileFromFS (name);
      t1 := syscall (SYSCALL_GETTIME);

      if elfSize = 0 then
         debugPrint ("procmgr: file read failed" & LF);
         return 0;
      end if;

      debugPrint ("procmgr: read ");
      printDec (Unsigned_32 (elfSize));
      debugPrint (" bytes in ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);

      if pri = 0 or pri > 10 then
         pri := 5;
      end if;

      t0 := syscall (SYSCALL_GETTIME);
      declare
         function toNum is new Ada.Unchecked_Conversion
            (System.Address, Unsigned_64);
         --  NUL-terminated copy of name for kernel to read
         nameBuf : String (1 .. 17) := (others => Character'Val (0));
         nameLen : Natural := name'Length;
      begin
         if nameLen > 16 then
            nameLen := 16;
         end if;
         for i in 0 .. nameLen - 1 loop
            nameBuf (i + 1) := name (name'First + i);
         end loop;

         newPID := syscall (SYSCALL_SPAWN,
                            toNum (elfBuf),
                            elfSize,
                            pri,
                            toNum (nameBuf'Address),
                            0,          -- arg4: auto-assign PID
                            requester); -- arg5: ppid = who asked for spawn
      end;
      t1 := syscall (SYSCALL_GETTIME);

      if newPID = Unsigned_64'Last then
         debugPrint ("procmgr: spawn syscall failed" & LF);
         return 0;
      end if;

      debugPrint ("procmgr: SYSCALL_SPAWN took ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);

      --  Launch arguments go in before anything else, while the child has
      --  never run; the kernel accepts them only then, and only once. A
      --  program with a description (its ports and descriptor map) gets it
      --  attached to its block, so it learns its ports at start; it gets a
      --  block with just its name when its launcher gave none. A malformed
      --  description refuses the launch.
      declare
         package LA renames CuBit.Launch_Arguments;
         Description_At : Unsigned_64;
         Description_Length : CuBit.Program_Descriptions.Descriptor_Length;
         Description_Valid : Boolean;
         Block_Length : LA.Block_Length := pendingArgumentBytes;
         Accepted : Boolean := True;
      begin
         findDescription
           (elfSize, Description_At, Description_Length, Description_Valid, streamBitmask);
         if Description_Length > 0 and then not Description_Valid then
            debugPrint ("procmgr: invalid program description" & LF);
            Stop_Suspended_Child;
            return 0;
         end if;
         if Description_Length > 0 then
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
                    deriveRings (newPID, Source);
                  Ring_Table : CuBit.Outlet_Rings.Bytes (1 .. CuBit.Outlet_Rings.Maximum_Bytes);
                  Ring_Length : CuBit.Outlet_Rings.Table_Length;
                  Length : LA.Present_Length := Block_Length;
               begin
                  CuBit.Outlet_Rings.Encode (Rings, Ring_Table, Ring_Length);
                  declare
                     Trailer : LA.Block (1 .. Ring_Length + Description_Length);
                  begin
                     for K in 1 .. Ring_Length loop
                        Trailer (K) := Ring_Table (K);
                     end loop;
                     Trailer (Ring_Length + 1 .. Trailer'Last) := Source;
                     LA.Attach_Description (launchBlock, Length, Trailer, Accepted);
                  end;
                  Block_Length := Length;
               end;
            end if;
            if not Accepted then
               debugPrint ("procmgr: program description does not fit its launch block" & LF);
               Stop_Suspended_Child;
               return 0;
            end if;
         end if;
         if Block_Length > 0 then
            declare
               Installed : constant Unsigned_64 := syscall
                 (SYSCALL_INSTALL_LAUNCH_ARGUMENTS, newPID,
                  Unsigned_64 (To_Integer (launchBlock'Address)),
                  Unsigned_64 (Block_Length));
            begin
               if Installed /= 0 then
                  debugPrint ("procmgr: launch arguments not installed" & LF);
                  Stop_Suspended_Child;
                  return 0;
               end if;
            end;
         end if;
      end;
      resetLaunchState (newPID);
      loadLaunchTable (elfSize, newPID);
      if attenuateFor /= 0 then
         --  Capture the child's incarnation now, before any policy step:
         --  the launcher matches its exit event by PID and generation.
         declare
            Minted : constant Unsigned_64 := syscall
              (SYSCALL_POLICY_MINT_CAPABILITY, syscall (SYSCALL_GETPID),
               CAP_TYPE_PROCESS, newPID, 0, 3,
               Unsigned_64 (Failed_Launch_Control_Slot));
         begin
            if Minted /= Unsigned_64'Last then
               Captured_Child := GPU_Grants.Capture (Failed_Launch_Control_Slot);
            end if;
            if not GPU_Grants.Valid (Captured_Child)
              or else GPU_Grants.Process_ID (Captured_Child) /= newPID
            then
               debugPrint ("procmgr: launched child identity unavailable" & LF);
               Stop_Suspended_Child;
               return 0;
            end if;
            launchedGeneration :=
              GPU_Grants.Incarnation (Captured_Child) / 2 ** 32;
         end;
      end if;

      --  PIDs are reusable.  Never attribute records retained from an earlier
      --  process generation to the new child.
      clearAuthorityForPID (newPID);
      --  Nothing a previous holder of this PID had in netstack survives.
      releaseNetworkOwner (newPID);

      --  SYSCALL_SPAWN installs these kernel bootstrap capabilities before
      --  procmgr applies the ELF manifest.  Record their origin explicitly.
      recordAuthority
        (newPID, 0, AUTH_SOURCE_KERNEL_BOOTSTRAP,
         AUTH_REASON_SELF_BOOTSTRAP, CAP_TYPE_ENDPOINT, False, True,
         3, newPID, 0);
      recordAuthority
        (newPID, 3, AUTH_SOURCE_KERNEL_BOOTSTRAP,
         AUTH_REASON_SELF_BOOTSTRAP, CAP_TYPE_PROCESS, False, True,
         3, newPID, 0);

      --  Parse .cubit.id section for package identity
      parseIdSection (elfSize, pkgId, pkgIdLen);

      --  Parse .cubit.caps manifest
      t0 := syscall (SYSCALL_GETTIME);
      noteProcess (newPID, requester, pkgId (1 .. pkgIdLen));
      parseAndGrantManifest
        (newPID, elfSize, Render, approveNetwork, systemStartup,
         approveLogViewer => Log_Viewer_Approved,
         approveProcessViewer => Process_Viewer_Approved,
         approveLogControl => Log_Control_Approved);
      t1 := syscall (SYSCALL_GETTIME);

      debugPrint ("procmgr: manifest took ");
      printDec (Unsigned_32 (t1 - t0));
      debugPrint ("ms" & LF);

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
         return 0;
      end if;

      --  Mint CAP_NOTIFICATION for logstore driver registration
      if pkgIdLen = 18 then
         declare
            LOGSTORE_ID : constant String := "com.cubit.logstore";
            match : Boolean := True;
            ignore : Unsigned_64;
         begin
            for c in 0 .. 17 loop
               if pkgId (1 + c) /= LOGSTORE_ID (1 + c) then
                  match := False;
                  exit;
               end if;
            end loop;
            if match then
               mintRecorded
                 (newPID, CAP_TYPE_NOTIFICATION, DRIVER_LOGSTORE, 0, 2, 7,
                  AUTH_SOURCE_IDENTITY_POLICY, AUTH_REASON_PACKAGE_ID,
                  False, ignore);
               debugPrint ("procmgr: minted logstore ntf cap" & LF);
            end if;
         end;
      end if;

      --  Mint CAP_NOTIFICATION for desktop.svc registration.
      --  The desktop endpoint is intentionally not granted to every process:
      --  only the process whose package identity is com.cubit.desktop should
      --  be able to claim DRIVER_DESKTOP via registerDriver().
      if pkgIdLen = 17 then
         declare
            DESKTOP_ID : constant String := "com.cubit.desktop";
            match : Boolean := True;
            ignore : Unsigned_64;
         begin
            for c in 0 .. 16 loop
               if pkgId (1 + c) /= DESKTOP_ID (1 + c) then
                  match := False;
                  exit;
               end if;
            end loop;
            if match then
               mintRecorded
                 (newPID, CAP_TYPE_NOTIFICATION, DRIVER_DESKTOP, 0, 2,
                  CAP_SLOT_SERVICE_REG, AUTH_SOURCE_IDENTITY_POLICY,
                  AUTH_REASON_PACKAGE_ID, False, ignore);
               debugPrint ("procmgr: minted desktop ntf cap" & LF);
            end if;
         end;
      end if;

      --  Mint CAP_NOTIFICATION for display.svc registration. display.svc is
      --  the sole userspace owner of scanout framebuffer authority.
      if pkgIdLen = 17 then
         declare
            DISPLAY_ID : constant String := "com.cubit.display";
            match : Boolean := True;
            ignore : Unsigned_64;
         begin
            for c in 0 .. 16 loop
               if pkgId (1 + c) /= DISPLAY_ID (1 + c) then
                  match := False;
                  exit;
               end if;
            end loop;
            if match then
               mintRecorded
                 (newPID, CAP_TYPE_NOTIFICATION, DRIVER_DISPLAY, 0, 2,
                  CAP_SLOT_SERVICE_REG, AUTH_SOURCE_IDENTITY_POLICY,
                  AUTH_REASON_PACKAGE_ID, False, ignore);
               debugPrint ("procmgr: minted display ntf cap" & LF);
            end if;
         end;
      end if;

      --  Mint CAP_NOTIFICATION for the headless IPC regression server.
      if pkgIdLen = 24 or else pkgIdLen = 26 then
         declare
            IPCTEST_SERVER_ID : constant String :=
               "com.cubit.ipctest.server";
            BENCH_IPC_SERVER_ID : constant String :=
               "com.cubit.bench.ipc.server";
            match : Boolean := True;
            ignore : Unsigned_64;
         begin
            if pkgIdLen = 24 then
               for c in 0 .. 23 loop
                  if pkgId (1 + c) /= IPCTEST_SERVER_ID (1 + c) then
                     match := False;
                     exit;
                  end if;
               end loop;
            else
               for c in 0 .. 25 loop
                  if pkgId (1 + c) /= BENCH_IPC_SERVER_ID (1 + c) then
                     match := False;
                     exit;
                  end if;
               end loop;
            end if;

            if match then
               mintRecorded
                 (newPID, CAP_TYPE_NOTIFICATION, DRIVER_IPCTEST, 0, 2, 7,
                  AUTH_SOURCE_IDENTITY_POLICY, AUTH_REASON_PACKAGE_ID,
                  False, ignore);
               debugPrint ("procmgr: minted ipctest ntf cap" & LF);
            end if;
         end;
      end if;

      --  Permit only the identity of the CCL host-import regression service
      --  to register the test endpoint. The registration capability itself
      --  remains kernel-enforced and is not inherited by its clients.
      if pkgIdLen = 23 then
         declare
            CCL_TEST_HOST_ID : constant String := "com.cubit.ccl-test-host";
            match : Boolean := True;
            ignore : Unsigned_64;
         begin
            for c in 0 .. 22 loop
               if pkgId (1 + c) /= CCL_TEST_HOST_ID (1 + c) then
                  match := False;
                  exit;
               end if;
            end loop;
            if match then
               mintRecorded
                 (newPID, CAP_TYPE_NOTIFICATION, DRIVER_CCL_TEST, 0, 2,
                  CAP_SLOT_SERVICE_REG, AUTH_SOURCE_IDENTITY_POLICY,
                  AUTH_REASON_PACKAGE_ID, False, ignore);
               debugPrint ("procmgr: minted ccl-test-host ntf cap" & LF);
            end if;
         end;
      end if;

      --  The clock service alone may register the system clock endpoint.
      --  Clients receive separate service handles through their manifests.
      if pkgIdLen = 15 then
         declare
            CLOCK_ID : constant String := "com.cubit.clock";
            match : Boolean := True;
            ignore : Unsigned_64;
         begin
            for c in 0 .. 14 loop
               if pkgId (1 + c) /= CLOCK_ID (1 + c) then
                  match := False;
                  exit;
               end if;
            end loop;
            if match then
               mintRecorded
                 (newPID, CAP_TYPE_NOTIFICATION, DRIVER_CLOCK, 0, 2,
                  CAP_SLOT_SERVICE_REG, AUTH_SOURCE_IDENTITY_POLICY,
                  AUTH_REASON_PACKAGE_ID, False, ignore);
               debugPrint ("procmgr: minted clock ntf cap" & LF);
            end if;
         end;
      end if;

      --  Registration as tls.svc. Package identity is only self-declared,
      --  so this also requires the trusted startup plan: an impostor TLS
      --  service would see every client's plaintext.
      if pkgIdLen = 13 and then systemStartup and then
        pkgId (1 .. 13) = "com.cubit.tls"
      then
         declare
            ignore : Unsigned_64;
         begin
            mintRecorded
              (newPID, CAP_TYPE_NOTIFICATION, CuBit.TLS_Protocol.Service_Role,
               0, 2, CAP_SLOT_SERVICE_REG, AUTH_SOURCE_IDENTITY_POLICY,
               AUTH_REASON_PACKAGE_ID, False, ignore);
            debugPrint ("procmgr: minted tls ntf cap" & LF);
         end;
      end if;

      --  Metrics registration authority belongs only to the trusted startup
      --  service. Self-declared package identity alone cannot replace it.
      if systemStartup and then pkgIdLen = 17 and then
        pkgId (1 .. 17) = "com.cubit.metrics"
      then
         declare
            ignore : Unsigned_64;
         begin
            mintRecorded
              (newPID, CAP_TYPE_NOTIFICATION,
               CuBit.Metric_Protocol.Publisher_Service_Role,
               0, 2, CAP_SLOT_SERVICE_REG, AUTH_SOURCE_IDENTITY_POLICY,
               AUTH_REASON_PACKAGE_ID, False, ignore);
            debugPrint ("procmgr: minted metrics ntf cap" & LF);
         end;
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
            flags => 0, reserved => 0);
         resetMessage.words (0) := newPID;
         resetTag := capCall (CAP_SLOT_FS_LOCAL, resetMessage);
         if resetTag.label /= CuBit.Filesystems.REPLY_OK then
            debugPrint ("procmgr: filesystem policy reset failed" & LF);
            Stop_Suspended_Child;
            return 0;
         end if;
         -- Config scopes are also keyed by PID. Reset even when the new ELF
         -- declares no Config scopes; absence must mean deny, not inheritance.
         declare
            Config_PID : constant Unsigned_64 :=
              getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_CONFIG);
            Config_Reset : Message := NULL_MESSAGE;
         begin
            if Config_PID /= 0 and Config_PID /= Unsigned_64'Last then
               Config_Reset.tag :=
                 (label => CuBit.Filesystems.OP_REVOKE_ACL, length => 1,
                  flags => 0, reserved => 0);
               Config_Reset.words (0) := newPID;
               resetTag := capCall (CAP_SLOT_CONFIG_LOCAL, Config_Reset);
               if resetTag.label /= CuBit.Filesystems.REPLY_OK then
                  debugPrint ("procmgr: config policy reset failed" & LF);
                  Stop_Suspended_Child;
                  return 0;
               end if;
            end if;
         end;
         --  TLS scopes are keyed by PID too: a reused PID must not inherit
         --  another program's names.
         if ensureTLSPolicy then
            declare
               TLS_Reset : Message := NULL_MESSAGE;
            begin
               TLS_Reset.tag :=
                 (label => CuBit.TLS_Protocol.Revoke_Operation, length => 1,
                  flags => 0, reserved => 0);
               TLS_Reset.words (0) := newPID;
               resetTag := capCall (CAP_SLOT_TLS_LOCAL, TLS_Reset);
               if resetTag.label /= REPLY_OK then
                  debugPrint ("procmgr: tls policy reset failed" & LF);
                  Stop_Suspended_Child;
                  return 0;
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
            Discard_Authorized_Child;
            return 0;
         end if;
      end;

      --  Query config store for resource quotas (overwrites elfBuf)
      declare
         maxFrames   : Unsigned_32 := 0;
         cpuQuotaUs  : Unsigned_32 := 0;
         cpuPeriodUs : Unsigned_32 := 0;
         ignore      : Unsigned_64;
      begin
         if pkgIdLen > 0 then
            queryConfigQuota (
               pkgId (1 .. pkgIdLen),
               maxFrames, cpuQuotaUs, cpuPeriodUs);
         else
            queryConfigQuota (
               name, maxFrames, cpuQuotaUs, cpuPeriodUs);
         end if;

         if maxFrames /= 0 or cpuQuotaUs /= 0 then
            declare
               param1 : constant Unsigned_64 :=
                  Unsigned_64 (cpuQuotaUs) or
                  Shift_Left (Unsigned_64 (cpuPeriodUs), 32);
            begin
               mintRecorded
                 (newPID, CAP_TYPE_RESOURCE, Unsigned_64 (maxFrames), param1,
                  1, RESOURCE_CAP_SLOT, AUTH_SOURCE_CONFIG_POLICY,
                  AUTH_REASON_CONFIG_QUOTA, False, ignore);
               debugPrint (
                  "procmgr: minted config resource cap" & LF);
            end;
         end if;
      end;

      if startupRole = CCL.Configurations.Config_Storage then
         declare
            Config_PID : constant Unsigned_64 := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_CONFIG);
            Minted : Unsigned_64;
            Attachment : Message := NULL_MESSAGE;
         begin
            --  Selected by trusted init.ccl, not by executable identity or a
            --  public spawn request. Never overwrite an installed backend.
            Config_Storage_Selected := True;
            if Config_PID = 0 or Config_PID = Unsigned_64'Last then
               Discard_Authorized_Child;
               return 0;
            end if;
            mintRecorded (Config_PID, CAP_TYPE_ENDPOINT, newPID, 0, 3,
              Unsigned_64 (Config_Worker_Startup.Worker_Endpoint),
              AUTH_SOURCE_KERNEL_BOOTSTRAP, AUTH_REASON_STARTUP_REQUIRED, False, Minted);
            if Minted = Unsigned_64'Last then
               Discard_Authorized_Child;
               return 0;
            end if;
            Attachment.tag := (label => Config_Worker_Startup.Operation'Enum_Rep
              (Config_Worker_Startup.Attach_Worker), length => 1, flags => 0, reserved => 0);
            Attachment.words (0) := newPID;
            Attachment.tag := capCall (CAP_SLOT_CONFIG_LOCAL, Attachment);
            if Attachment.tag.label /= REPLY_OK or Attachment.tag.length /= 1
              or Attachment.tag.flags /= 0 or Attachment.tag.reserved /= 0
              or Attachment.words /= [0, 0, 0, 0]
            then
               debugPrint ("procmgr: Config storage attachment failed" & LF);
               Discard_Authorized_Child;
               return 0;
            end if;
         end;
      end if;

      if Render.Requested then
         declare
            use type Render_Launch.Phase, CuBit.Render_Startup.Decision;
            ID : Render_Launch.Ticket := 0;
            Receipt : aliased CompletionEntry;
            Consumed, Accepted : Boolean := False;
            Now : Unsigned_64 := syscall (SYSCALL_GETTIME);
            Deadline : Unsigned_64 := Now;
            Ignore : Unsigned_64;
            Activity : Activity_Result;
            type Inspection_Words is array (0 .. 5) of Unsigned_64;
            Data : aliased Inspection_Words := [others => 0];
            Mode : constant CuBit.Render_Startup.Attempt :=
              (if Force_Software then CuBit.Render_Startup.Software_Only else
               CuBit.Render_Startup.Initial_Attempt
                 (Render.Demand, systemStartup and approveRender));
            Next : CuBit.Render_Startup.Decision;
            Slot_State : CuBit.Render_Startup.Capability := CuBit.Render_Startup.Unknown;
         begin
            -- Capture the child and its stop authority before any broker
            -- request. The software retry is a distinct captured incarnation.
            Ignore := syscall (SYSCALL_POLICY_MINT_CAPABILITY,
              syscall (SYSCALL_GETPID), CAP_TYPE_PROCESS, newPID, 0, 3,
              Unsigned_64 (Failed_Launch_Control_Slot));
            if Ignore = Unsigned_64'Last then
               Discard_Authorized_Child;
               return 0;
            end if;
            Captured_Child := GPU_Grants.Capture (Failed_Launch_Control_Slot);
            Identity := GPU_Grants.Incarnation (Captured_Child);
            if not GPU_Grants.Valid (Captured_Child) or else
              GPU_Grants.Process_ID (Captured_Child) /= newPID or else
              (Force_Software and then
               not CuBit.Render_Startup.Fresh_Retry (Prior_Incarnation, Identity))
            then
               Discard_Authorized_Child;
               return 0;
            end if;
            debugPrint ("procmgr: render attempt incarnation=" & Unsigned_64'Image (Identity) &
              " software=" & Boolean'Image (Mode = CuBit.Render_Startup.Software_Only) & LF);
            -- Only a trusted startup plan may supply this approval today.
            -- Public spawn requests never propagate it. Destinations occupied
            -- by another manifest grant are rejected by kernel delegation.
            if Mode = CuBit.Render_Startup.With_Render and then
              systemStartup and then approveRender and then Render.Valid and then
              Now <= Unsigned_64'Last - 6_000
            then
               Deadline := Now + 6_000;
               Ignore := syscall (SYSCALL_POLICY_MINT_CAPABILITY,
                 syscall (SYSCALL_GETPID), CAP_TYPE_ENDPOINT, newPID, 0, 9,
                 Unsigned_64 (Render_Source_Slot));
               if Ignore /= Unsigned_64'Last then
                  Render_Launch.Start (Render_Launcher, True, Captured_Child,
                    Render_Source_Slot, Render.Destination, ID);
               end if;
               if Render_Launch.State (Render_Launcher, ID) = Render_Launch.Pending then
                  debugPrint ("procmgr: render admission submitted; child suspended" & LF);
               end if;
               -- Startup already executes sequential policy handshakes. Do
               -- not resume this child while its render admission is pending.
               -- This is the sole completion-token owner in procmgr today.
               while Render_Launch.State (Render_Launcher, ID) = Render_Launch.Pending loop
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
               Accepted := Render_Launch.State (Render_Launcher, ID) = Render_Launch.Admitted;
               if Accepted then
                  -- Record actual installed kernel authority, not a guessed
                  -- GPU PID or a tag reconstructed from application input.
                  Ignore := syscall (SYSCALL_INSPECT_CAPABILITY, newPID,
                    Unsigned_64 (Render.Destination),
                    Unsigned_64 (To_Integer (Data'Address)));
                  Accepted := Ignore = 1 and then Data (0) = CAP_TYPE_ENDPOINT and then
                    Data (1) = 3 and then Data (3) /= 0;
               end if;
            end if;
            recordAuthority (newPID, Unsigned_64 (Render.Destination),
              AUTH_SOURCE_CONFIG_POLICY, AUTH_REASON_MANIFEST_REQUEST,
              CAP_TYPE_ENDPOINT, True, Accepted, 3, Data (3), Data (2));
            if Mode = CuBit.Render_Startup.Software_Only then
               Ignore := syscall (SYSCALL_INSPECT_CAPABILITY, newPID,
                 Unsigned_64 (Render.Destination), Unsigned_64 (To_Integer (Data'Address)));
               if Ignore = 1 then
                  Slot_State := (if Data (0) = 0 then CuBit.Render_Startup.Empty
                                 else CuBit.Render_Startup.Other);
               end if;
            elsif Accepted then
               Slot_State := CuBit.Render_Startup.Render_Endpoint;
            end if;
            Next := CuBit.Render_Startup.Decide
              (Render.Demand, Mode,
               systemStartup and approveRender and Render.Valid,
               (case Render_Launch.State (Render_Launcher, ID) is
                  when Render_Launch.Absent => CuBit.Render_Startup.Not_Requested,
                  when Render_Launch.Pending => CuBit.Render_Startup.Pending,
                  when Render_Launch.Rejected => CuBit.Render_Startup.Rejected,
                  when Render_Launch.Admitted => CuBit.Render_Startup.Admitted,
                  when Render_Launch.Uncertain => CuBit.Render_Startup.Uncertain),
               Slot_State);
            if Next not in CuBit.Render_Startup.Resume_Render | CuBit.Render_Startup.Resume_Software then
               -- Timeout is NOT remote cancellation or retirement. The
               -- launcher/broker keep their slots and tokens; this child is
               -- discarded and a late success can never resume it here.
               debugPrint ("procmgr: render admission denied; child not resumed" & LF);
               Discard_Authorized_Child;
               Retry := CuBit.Render_Startup.Retry_Software (Next, Stop_Accepted);
               return 0;
            end if;
            if Next = CuBit.Render_Startup.Resume_Render then
               debugPrint ("procmgr: render admission complete" & LF);
            else
               debugPrint ("procmgr: render software-only admitted" & LF);
            end if;
         end;
      end if;

      declare
         ignore : Unsigned_64;
      begin
         ignore := syscall (SYSCALL_RESUME, newPID);
         if ignore /= 0 then
            debugPrint ("procmgr: child resume rejected" & LF);
            Discard_Authorized_Child;
            return 0;
         end if;
      end;

      debugPrint ("procmgr: resumed PID ");
      printDec (Unsigned_32 (newPID));
      debugPrint ("" & LF);

      --  Notify requester about declared streams (after resume).
      if streamBitmask /= 0 then
         declare
            evMsg : Message := NULL_MESSAGE;
         begin
            evMsg.tag := (
               label  => OP_STREAM_AVAILABLE,
               length => 2,
               flags  => 0,
               reserved  => 0);
            evMsg.words (0) := newPID;
            evMsg.words (1) := streamBitmask;

            if requester /= 0 then
               sendEvent (ProcessID (requester), evMsg);
               debugPrint ("procmgr: sent stream available" & LF);
            end if;

         end;
      end if;

      return newPID;
   end Spawn_Attempt;

   function spawnByName
     (name        : String;
      priority    : Unsigned_64;
      requester   : Unsigned_64 := 0;
      sandboxMode : Unsigned_8 := SANDBOX_NONE;
      cwd         : String := "";
      approveNetwork : Network_Approval := No_Network;
      approveRender : Boolean := False;
      systemStartup : Boolean := False;
      startupRole : CCL.Configurations.Startup_Role := CCL.Configurations.Application)
      return Unsigned_64
   is
      Retry : Boolean;
      Identity, Retry_Identity : Unsigned_64;
      Result : Unsigned_64;
   begin
      Result := Spawn_Attempt
        (False, 0, Retry, Identity, name, priority, requester, sandboxMode,
         cwd, approveNetwork, approveRender, systemStartup, startupRole);
      if Result = 0 and then Retry then
         -- Exactly one fresh attempt, with no broker request. The failed
         -- attempt's launcher resources stay retained independently.
         debugPrint ("procmgr: render retry software with fresh child" & LF);
         Result := Spawn_Attempt
           (True, Identity, Retry, Retry_Identity, name, priority, requester,
            sandboxMode, cwd, approveNetwork, approveRender, systemStartup, startupRole);
      end if;
      return Result;
   end spawnByName;

   ---------------------------------------------------------------------------
   --  handleLaunch
   --  OP_LAUNCH (CuBit.Launch_Arguments): spawn a program with launch
   --  arguments. The name and block are copied out of the requester's grant
   --  first, and the block is validated in procmgr's copy; arguments are
   --  data and never change what authority the child receives.
   ---------------------------------------------------------------------------
   procedure handleLaunch (sender : ProcessID; msg : Message) is
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
      newPID    : Unsigned_64;

      procedure Fail (Reason : LA.Launch_Failure) is
      begin
         sendReply (sender, REPLY_ERR, LA.Launch_Failure'Enum_Rep (Reason));
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
      if Unsigned_64 (sender) not in launchStates'Range then
         Fail (LA.Malformed_Request);
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (Reference, Unsigned_64 (sender), 0,
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
            Launcher : Launch_State renames launchStates (Unsigned_64 (sender));
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
         attenuateFor := Unsigned_64 (sender);
         attenuationRefused := False;
         directoryRefused := False;
         newPID := spawnByName (Name, Priority, Unsigned_64 (sender));
         pendingArgumentBytes := 0;
         pendingGrantBytes := 0;
         pendingRings := (others => <>);
         attenuateFor := 0;
      end;

      if newPID = 0 and then directoryRefused then
         debugPrint ("procmgr: " & CuBit.Failures.Explain
           ("Starting a program",
            CuBit.Failures.Failed
              (CuBit.Failures.Not_Granted,
               "its working directory is outside what it may read",
               "start it in a directory its manifest grants, or grant " &
               "that directory")) & LF);
         directoryRefused := False;
         Fail (LA.Not_Granted);
      elsif newPID = 0 and then attenuationRefused then
         debugPrint ("procmgr: " & CuBit.Failures.Explain
           ("Starting a program",
            CuBit.Failures.Failed
              (CuBit.Failures.Not_Granted,
               "it asks for authority its launcher does not hold",
               "grant the launcher that authority, or remove the request " &
               "from the program's manifest")) & LF);
         attenuationRefused := False;
         Fail (LA.Not_Granted);
      elsif newPID = 0 then
         Fail (LA.Spawn_Failed);
      else
         declare
            Reply_Message : Message := NULL_MESSAGE;
            Ignore : Unsigned_64;
         begin
            Reply_Message.tag :=
              (label => REPLY_OK, length => 2, flags => 0, reserved => 0);
            Reply_Message.words := [0 => newPID, 1 => launchedGeneration,
                                    others => 0];
            Ignore := reply (sender, Reply_Message);
         end;
      end if;
   end handleLaunch;

   ---------------------------------------------------------------------------
   --  handleLaunchTable
   --  OP_LAUNCH_TABLE (CuBit.Launch_Authority): the requester's own launch
   --  table, written into the grant it lends.
   ---------------------------------------------------------------------------
   procedure handleLaunchTable (sender : ProcessID; msg : Message) is
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Mapped    : System.Address;
      Ok, Returned : Boolean;
   begin
      if msg.tag.length /= LAuth.Table_Request_Words
        or else not CuBit.Grant_References.Valid_Wire (msg.words (0))
        or else Unsigned_64 (sender) not in launchStates'Range
      then
         sendReply (sender, REPLY_ERR, CuBit.Launch_Arguments.Launch_Failure'Enum_Rep
                      (CuBit.Launch_Arguments.Malformed_Request));
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (Reference, Unsigned_64 (sender), 0, LAuth.Maximum_Table_Bytes,
         CuBit.Memory_Grants.Write_Access, Mapped, Ok);
      if not Ok then
         sendReply (sender, REPLY_ERR, CuBit.Launch_Arguments.Launch_Failure'Enum_Rep
                      (CuBit.Launch_Arguments.Grant_Unavailable));
         return;
      end if;
      declare
         Launcher : Launch_State renames launchStates (Unsigned_64 (sender));
         Target : LAuth.Table_Bytes (1 .. LAuth.Maximum_Table_Bytes)
           with Import, Address => Mapped;
      begin
         Target (1 .. Launcher.Table_Length) := Launcher.Table (1 .. Launcher.Table_Length);
         CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
         sendReply (sender, REPLY_OK, Unsigned_64 (Launcher.Table_Length));
      end;
   end handleLaunchTable;

   ---------------------------------------------------------------------------
   --  handleProgramDescription
   --  OP_PROGRAM_DESCRIPTION (CuBit.Program_Descriptions): a program's
   --  parameters, for a requester that may launch it. The descriptor is
   --  validated with Decode before it is written back after the name.
   ---------------------------------------------------------------------------
   procedure handleProgramDescription (sender : ProcessID; msg : Message) is
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
        or else Unsigned_64 (sender) not in launchStates'Range
      then
         Fail (LA.Malformed_Request);
         return;
      end if;
      Reference := CuBit.Grant_References.Decode (msg.words (0));
      CuBit.Memory_Grants.Acquire
        (Reference, Unsigned_64 (sender), 0,
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
         Launcher : Launch_State renames launchStates (Unsigned_64 (sender));
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
   ---------------------------------------------------------------------------
   procedure handleSpawn (sender : ProcessID; msg : Message) is
      nameLen     : constant Natural := Natural (msg.tag.length);
      grantId     : constant Unsigned_64 := msg.words (0);
      priority    : constant Unsigned_64 := msg.words (1);
      spawnFlags  : constant Unsigned_64 := msg.words (2);
      cwdLen      : Natural := Natural (msg.words (3) and 16#FF#);
      sandboxMode : constant Unsigned_8 :=
         Unsigned_8 (spawnFlags and 16#03#);
      grantAddr   : constant System.Address :=
         To_Address (Integer_Address (
            GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE));
      newPID : Unsigned_64;
   begin
      if nameLen = 0 or nameLen > 255 then
         sendReply (sender, REPLY_ERR, 0);
         return;
      end if;

      --  Clamp cwdLen to fit in grant buffer after filename
      if cwdLen > 128 then
         cwdLen := 128;
      end if;

      declare
         name : String (1 .. nameLen) with
            Import, Address => grantAddr;
         approval : constant Network_Approval := Desktop_Approval
           (name, Unsigned_64 (sender),
            getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP));
      begin
         if approval = Browser_Outbound then
            debugPrint ("procmgr: desktop browser outbound approval: " & name & LF);
         end if;
         if cwdLen > 0 then
            declare
               cwdAddr : constant System.Address :=
                  To_Address (Integer_Address (
                     GRANT_REGION_BASE + grantId * GRANT_SLOT_SIZE +
                     Unsigned_64 (nameLen)));
               cwdStr : String (1 .. cwdLen) with
                  Import, Address => cwdAddr;
            begin
               newPID := spawnByName (name, priority,
                                      Unsigned_64 (sender),
                                      sandboxMode, cwdStr, approval);
            end;
         else
            newPID := spawnByName (name, priority,
                                   Unsigned_64 (sender),
                                   sandboxMode, approveNetwork => approval);
         end if;
      end;

      if newPID = 0 then
         sendReply (sender, REPLY_ERR, 0);
      else
         sendReply (sender, REPLY_OK, newPID);
      end if;
   end handleSpawn;

   ---------------------------------------------------------------------------
   --  processInitCCL
   --  Read init.ccl from the filesystem, parse entries, and spawn each.
   --  Format: one filename per line, optional "pri=N" suffix.
   --  Lines starting with '#' and blank lines are skipped.
   ---------------------------------------------------------------------------
   procedure processInitCCL is
      use CCL.Configurations;
      Plan : Compilation_Result;
      Source_Size : Unsigned_64;
   begin
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
         Compile (Source, Plan);
      end;
      if not Plan.Success or else Plan.Plan.Kind /= Startup_Profile then
         debugPrint ("procmgr: init.ccl rejected at" & Plan.Position'Image &
                     ": " & Diagnostic_Name (Plan.Diagnostic) &
                     " (expected startup profile)" & LF);
         return;
      end if;
      --  The entire plan owns its strings before spawnByName overwrites elfBuf.
      debugPrint ("procmgr: init.ccl: ");
      printDec (Unsigned_32 (Plan.Plan.Launch_Count));
      debugPrint (" entries" & LF);
      declare
         Entry_Number : Natural := 0;
      begin
      for Item of Plan.Plan.Launches (1 .. Plan.Plan.Launch_Count) loop
         declare
            Name : String renames
              Item.Executable.Data (1 .. Item.Executable.Length);
            PID : Unsigned_64;
         begin
            Entry_Number := Entry_Number + 1;
            --  These bounded milestones are also retained by the firmware
            --  boot panel until display.svc takes ownership.  They make a
            --  physical boot stall attributable to one launch boundary,
            --  without turning the panel into an unbounded serial log.
            debugPrint ("procmgr: init launch ");
            printDec (Unsigned_32 (Entry_Number));
            debugPrint ("/" & Unsigned_32'Image (Unsigned_32 (Plan.Plan.Launch_Count)) & ": " & Name & LF);
            PID := spawnByName
              (Name, Unsigned_64 (Item.Priority), systemStartup => True, startupRole => Item.Role,
               approveRender => Item.Approve_Render,
               approveNetwork =>
                 (if Item.Approval = Approve_Declared then Declared_Network
                  else No_Network));
            if PID = 0 then
               debugPrint ("procmgr: init spawn failed: " & Name & LF);
            else
               debugPrint ("procmgr: init launched: " & Name & LF);
            end if;
         end;
      end loop;
      end;
   end processInitCCL;

   ---------------------------------------------------------------------------
   --  Main variables
   ---------------------------------------------------------------------------
   sender : ProcessID;
   msg    : Message;

begin
   debugPrint ("procmgr: starting..." & LF);

   --  Register as DRIVER_PROCMGR
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_PROCMGR);
   end;

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      ignore : MessageTag;
   begin
      --  This is intentionally before the synchronous ready handoff.  The
      --  firmware diagnostic panel retains the bounded bootstrap line, so a
      --  headless physical boot distinguishes "blocked in handoff" from
      --  "resumed and failed during bootstrap" without relying on serial.
      debugPrint ("procmgr: bootstrap handoff sent" & LF);
      ignore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, reserved => 0),
          authorityTag => 0,
          words    => [others => 0]));
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

   --  Grant elfBuf directly to FS server for zero-copy reads.
   --  Use createGrantViaCap with our FS endpoint cap (slot 1) to avoid
   --  hardcoding the FS server PID.
   debugPrint ("procmgr: bootstrap 2/4 creating filesystem grant" & LF);
   declare
      ok : Boolean;
   begin
      CuBit.Memory_Grants.Create_Via_Capability (
         slot      => CAP_SLOT_FS_LOCAL,
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
     (CAP_SLOT_CONFIG_LOCAL, elfBuf, INITIAL_BUF_PAGES, True,
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

   --  Main service loop
   loop
      receive (sender, msg);

      case msg.tag.label is
         when OP_SPAWN =>
            handleSpawn (sender, msg);
         when OP_LAUNCH =>
            handleLaunch (sender, msg);
         when LAuth.Table_Operation =>
            handleLaunchTable (sender, msg);
         when CuBit.Program_Descriptions.Description_Operation =>
            handleProgramDescription (sender, msg);
         when CuBit.Process_Observer.List_Label =>
            handleProcessList (sender, msg);
         when EVENT_CHILD_EXIT =>
            --  A process retired (the kernel tells procmgr of each). Events
            --  are not unforgeable, so act only if it is really gone.
            if CuBit.Child_Exits.Valid
                 (msg.tag.length, msg.words (0), msg.words (1), msg.words (2))
              and then not processListed (msg.words (0))
            then
               releaseRings (msg.words (0));
               resetLaunchState (msg.words (0));
               if msg.words (0) in processRecords'Range then
                  processRecords (msg.words (0)) := (others => <>);
               end if;
               clearAuthorityForPID (msg.words (0));
               releaseNetworkOwner (msg.words (0));
               releaseFilesystemOwner (msg.words (0));
            end if;
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
   end loop;
end main;
