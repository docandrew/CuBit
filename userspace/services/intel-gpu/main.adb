with Intel_GPU_ADLN_L3;
with Intel_GPU_Record_Store;
with Intel_GPU_Table_Provenance.Backing;
with Intel_GPU_Table_Allocations;
with Intel_GPU_Table_Provenance.Retirement;
with Intel_GPU_Metadata_Platform;
with Intel_GPU_Metadata_Bundle;
with Intel_GPU_Record_Growth;
with Intel_GPU_Table_Provenance.Retirement.Dispatcher;
with Intel_GPU_Update_Storage;
with Intel_GPU_VM_Image.Removal;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Image.Growth.Backing;
with Intel_GPU_VM_Image.Growth.Backing.Writer;
with Intel_GPU_Table_Provenance.IO;
with Intel_GPU_Deferred_Retirement;
with CuBit.Capability_Grants;
with Intel_GPU_Probe_Export;
with Native_GPU_Probe_Protocol;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Boot;
with Intel_GPU_Device_Query;
with Intel_GPU_Memory_Admission;
with Intel_GPU_Diagnostics;
with CuBit.Log_Records;
with Intel_GPU_Display_Presence;
with Intel_GPU_ADS_Buffer;
with Intel_GPU_ADS_System_Info;
with Intel_GPU_PCI_Interrupts;
with Intel_GPU_Resources; use Intel_GPU_Resources;
with Intel_GPU_Observation;
with Intel_GPU_Probe;
with Intel_GPU_Forcewake;
with Intel_GPU_Firmware_File;
with Intel_GPU_Firmware_Buffer;
with Intel_GPU_Submission_Backing;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Budget_Query;
with Intel_GPU_Budget_Protocol;
with Intel_GPU_Buffer_Memory;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Application_State;
with Intel_GPU_Application_Submit;
with Intel_GPU_Application_Image;
with Intel_GPU_Application_Lifetime;
with Intel_GPU_PPGTT_Scratch;
with Intel_GPU_Application_Image.Publication;
with Intel_GPU_Application_Image.Retirement;
with Intel_GPU_Retirement_Invalidate;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Contexts;
with Intel_GPU_Buffer_Requests.Closed_Tables;
with Intel_GPU_Buffer_Requests.Sharing;
with Intel_GPU_Buffer_Requests.Binding;
with Intel_GPU_Render_Control;
with Intel_GPU_Render_Sessions;
with Intel_GPU_Submission_Image;
with Intel_GPU_Submission_Buffer;
with Intel_GPU_Submission_Buffer.Updates;
with Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Native_Initial_Ring;
with Intel_GPU_Native_Live_Ring;
with Intel_GPU_Native_TLB_IO;
with Intel_GPU_ADLN_TLB_Invalidate;
with Intel_GPU_Initial_Completion;
with Intel_GPU_Firmware;
with Intel_GPU_GGTT;
with Intel_GPU_GGTT_Layout;
with Intel_GPU_GGTT_Reservations;
with Intel_GPU_GGTT_Publish;
with Intel_GPU_Native_GGTT;
with Intel_GPU_GGTT_Invalidate;
with Intel_GPU_GuC_Parameters;
with Intel_GPU_GuC_Upload;
with Intel_GPU_GuC_Status;
with Intel_GPU_GuC_CT_Setup;
with Intel_GPU_GuC_CT_Register;
with Intel_GPU_GuC_MMIO;
with Intel_GPU_Native_GuC_Mailbox;
with Intel_GPU_Native_CT_Receive;
with Intel_GPU_GuC_CT_Receive;
with Intel_GPU_Native_CT_Send;
with Intel_GPU_GuC_CT_Send;
with Intel_GPU_GuC_CT_Roundtrip;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
with Intel_GPU_GuC_Fast_Fences;
with Intel_GPU_Context_Table;
with Intel_GPU_Context_Table.Waiting;
with Intel_GPU_Context_Table.Draining;
with Intel_GPU_VM_Update;
with Intel_GPU_VM_Image.Snapshots;
with Intel_GPU_Application_Image.Updates;
with Intel_GPU_Native_Context_Read;
with Intel_GPU_ADLN_Context_Init;
with Intel_GPU_ADLN_Barrier;
with Intel_GPU_Native_GuC_IO;
with Intel_GPU_DMA_Cache;
with Intel_GPU_GGTT_Mapping;
with Intel_GPU_ADLN_WOPCM;
with Intel_GPU_ADLN_PAT;
with Intel_GPU_Native_PAT;
with Intel_GPU_Native_MOCS;
with Intel_GPU_MOCS_Configure;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_ADLN_Steering;
with Intel_GPU_ADLN_Forcewake;
with System;
with System.Storage_Elements;
with System.Machine_Code;
with CuBit.Monotonic;
with Intel_GPU_Reset_Pages;
with Intel_GPU_Display_Mapping;
with Intel_GPU_Native_Parent;
with Intel_GPU_Native_Pipe;
with Intel_GPU_Native_Plane;
with Intel_GPU_Plane_Registers;
with Intel_GPU_Native_Engine_Settings;
with Intel_GPU_Engine_Configure;
with Intel_GPU_Native_GT_Settings;
with Intel_GPU_GT_Configure;
with Intel_GPU_RCS_Start;
with Intel_GPU_Native_RCS_Start;
with Intel_GPU_Native_Cursor;
with Intel_GPU_Scanout_Inventory;
with Intel_GPU_DC_State;
with Intel_GPU_Native_DC_State;
with Intel_GPU_Native_DC;
with Intel_GPU_PHY_Mapping;
with Intel_GPU_Native_Combo_State;
with Intel_GPU_Combo_PHY;
with Intel_GPU_Display_Topology;
with Intel_GPU_Native_Reset;
with Intel_GPU_Native_GuC_Invalidate;
procedure Main is
   package Parent_One is new Intel_GPU_Native_Parent (Intel_GPU_Display_Topology.PW1);
   package Parent_Two is new Intel_GPU_Native_Parent (Intel_GPU_Display_Topology.PW2);
   package DC_Snapshot is new Intel_GPU_Native_DC_State (Parent_One.Held);
   package PHY_Snapshot is new Intel_GPU_Native_Combo_State (Parent_One.Held);
   function DC_Now_Us return Unsigned_64 is
      Stamp : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      return (if Stamp.Available then Stamp.Microseconds else Unsigned_64'Last);
   end DC_Now_Us;
   package Native_DC is new Intel_GPU_Native_DC
     (Parent_One.Held, Intel_GPU_Display_Mapping.Ready,
      Intel_GPU_PHY_Mapping.Ready, DC_Now_Us);
   package Pipe_A is new Intel_GPU_Native_Pipe (Intel_GPU_Display_Topology.A);
   package Pipe_B is new Intel_GPU_Native_Pipe (Intel_GPU_Display_Topology.B);
   package Pipe_C is new Intel_GPU_Native_Pipe (Intel_GPU_Display_Topology.C);
   package Pipe_D is new Intel_GPU_Native_Pipe (Intel_GPU_Display_Topology.D);
   package Plane_A is new Intel_GPU_Native_Plane (Intel_GPU_Display_Topology.A, Pipe_A.Held);
   package Plane_B is new Intel_GPU_Native_Plane (Intel_GPU_Display_Topology.B, Pipe_B.Held);
   package Plane_C is new Intel_GPU_Native_Plane (Intel_GPU_Display_Topology.C, Pipe_C.Held);
   package Plane_D is new Intel_GPU_Native_Plane (Intel_GPU_Display_Topology.D, Pipe_D.Held);
   package Cursor_A is new Intel_GPU_Native_Cursor (Intel_GPU_Display_Topology.A, Pipe_A.Held);
   package Cursor_B is new Intel_GPU_Native_Cursor (Intel_GPU_Display_Topology.B, Pipe_B.Held);
   package Cursor_C is new Intel_GPU_Native_Cursor (Intel_GPU_Display_Topology.C, Pipe_C.Held);
   package Cursor_D is new Intel_GPU_Native_Cursor (Intel_GPU_Display_Topology.D, Pipe_D.Held);
   Scanout_Planes : Intel_GPU_Scanout_Inventory.Planes;
   Scanout_Cursors : Intel_GPU_Scanout_Inventory.Cursors;
   Scanout : Intel_GPU_Scanout_Inventory.Inventory;
   package DP renames Intel_GPU_Display_Presence;
   Display_Presence : DP.Snapshot;
   function Pipe_Present (P : DP.Pipe) return Boolean is
     (Display_Presence.Known and then
       DP."=" (Display_Presence.Pipes (P), DP.Present));
   Address_Layout : Intel_GPU_GGTT_Layout.Layout;
   Selected_WOPCM : Intel_GPU_ADLN_WOPCM.Layout;
   use type Intel_GPU_Firmware_File.Load_Status;
   Sender : ProcessID;
   Request : Message;
   Plan : Mapping_Plan;
   PCI_Device : Unsigned_16 := 0;
   PCI_Revision : Unsigned_8 := 0;
   Startup_Parameters : Intel_GPU_GuC_Parameters.Parameter_Block;
   Result : Unsigned_64;
   Register_Virtual_Base : constant Unsigned_64 := 16#6000_0000#;
   Observation : Intel_GPU_Observation.Snapshot;
   GGC : Unsigned_16;
   Table_Bytes : Unsigned_64;
   GGTT_Inspection_Mapped : Boolean := False;
   Engine_Inventory : Intel_GPU_ADLN_Inventory.Inventory;
   Media_Fuse : Unsigned_32 := Unsigned_32'Last;
   Steering_First, Steering_Second : Intel_GPU_ADLN_Steering.Fuse_Snapshot;
   Steering : Intel_GPU_ADLN_Steering.Topology;
   Doorbell_First, Doorbell_Second : Unsigned_32 := Unsigned_32'Last;
   ADS_Info : Intel_GPU_ADS_System_Info.System_Info;
   Reset_Write_Base : constant Unsigned_64 := Intel_GPU_Reset_Pages.Virtual_Base;
   Reset_Pages_Mapped : Boolean := False;
   Display_Power_Owned : Boolean := False;
   PCI_IRQ_Attempted : Boolean := False;
   Selected_Upload_Bytes : Unsigned_64 := 0;
   -- Initial-boot bounded GT reset after one-shot authorization. Firmware
   -- execution is gated separately on reset, PAT and mapping readiness.
   Enable_Native_Reset : constant Boolean := True;
   Upload_Ledger : Intel_GPU_GGTT_Reservations.Ledger;
   Upload_Backing : Intel_GPU_Firmware_Buffer.Prepared_Buffer;
   Upload_Bound : Boolean := False;
   -- One-shot boot takeover. No modesetting/submission peer is admitted;
   -- the device grant, completed reset and frozen scanout inventory remain
   -- prerequisites on every write. This is not authority to free old RAM.
   GGTT_Takeover_Held : Boolean := False;
   Upload_GPU_Start : Unsigned_64 := 0;
   Upload_Region : constant Unsigned_64 := Intel_GPU_Firmware_Buffer.Firmware_Region_Bytes;
   PAT_Ready, MOCS_Ready, GT_Settings_Ready, Render_Settings_Ready : Boolean := False;
   function Publication_Owner_Base return Boolean is
     (Display_Power_Owned and then Reset_Pages_Mapped and then
      Intel_GPU_Native_Reset.Last_Succeeded and then Native_DC.Held and then
      Parent_One.Held and then Parent_Two.Held and then
      DP.Required_Power_Held (Display_Presence,
        [DP.A => Pipe_A.Held, DP.B => Pipe_B.Held,
         DP.C => Pipe_C.Held, DP.D => Pipe_D.Held]) and then
      Intel_GPU_GGTT_Mapping.Ready and then Intel_GPU_GGTT_Mapping.Bytes = Table_Bytes);
   function Publication_Owner_Ready return Boolean is
     (PAT_Ready and then MOCS_Ready and then GT_Settings_Ready and then
      Render_Settings_Ready and then Publication_Owner_Base);
   function Upload_Range_Allowed (First, Bytes : Unsigned_64) return Boolean is
     (GGTT_Takeover_Held and then Publication_Owner_Ready and then Address_Layout.Valid and then Bytes > 0 and then
      First >= Address_Layout.Upload_First and then First < Address_Layout.Upload_Limit and then
      Bytes <= Address_Layout.Upload_Limit - First and then
      Intel_GPU_Scanout_Inventory.No_Scanout_Overlap (Scanout, (True, First, Bytes)));
   function Upload_Write_Allowed (Index, Value : Unsigned_64) return Boolean is
   begin
      if not Upload_Bound or else not Upload_Backing.Ready or else
        Index < Upload_GPU_Start / 4096 or else
        Index - Upload_GPU_Start / 4096 >= Upload_Region / 4096
      then return False; end if;
      return Upload_Range_Allowed (Index * 4096, 4096) and then
        Value = Intel_GPU_GGTT.Encode_System_Page
          (Upload_Backing.DMA_Address + (Index - Upload_GPU_Start / 4096) * 4096);
   end Upload_Write_Allowed;
   package Upload_IO is new Intel_GPU_Native_GGTT
     (Publication_Owner_Ready, Intel_GPU_GGTT_Mapping.Bytes, Upload_Write_Allowed);
   procedure Prepare_Upload (First, Bytes : Unsigned_64; Success : out Boolean) is
      Current : constant Intel_GPU_Firmware_Buffer.Prepared_Buffer := Intel_GPU_Firmware_Buffer.Prepared;
   begin
      Success := False;
      if Upload_Bound or else not Upload_Backing.Ready or else not Current.Ready or else
        Bytes /= Upload_Region or else not Upload_Range_Allowed (First, Bytes) or else
        Current.DMA_Address /= Upload_Backing.DMA_Address or else
        Current.CPU_Address /= Upload_Backing.CPU_Address or else
        Current.Allocation_Bytes /= Upload_Backing.Allocation_Bytes or else
        Current.Content_Bytes /= Upload_Backing.Content_Bytes or else
        Current.Content_Bytes = 0 or else Current.Content_Bytes > Bytes or else
        Current.Allocation_Bytes < Bytes
      then return; end if;
      if not Intel_GPU_DMA_Cache.Flush_Range (Current.CPU_Address, Bytes) then return; end if;
      Upload_GPU_Start := First;
      Upload_Bound := True;
      Success := True;
   end Prepare_Upload;
   procedure Invalidate_Upload (Success : out Boolean) is
   begin
      Success := Publication_Owner_Ready and then
        Intel_GPU_GGTT_Invalidate.Issue (Reset_Pages_Mapped);
   end Invalidate_Upload;
   package Upload_Publication is new Intel_GPU_GGTT_Publish
     (Upload_Range_Allowed, Prepare_Upload, Upload_IO.Read_PTE,
      Upload_IO.Write_PTE, Invalidate_Upload, Upload_Region);
   Upload_Attempt : Upload_Publication.Attempt;
   use type Intel_GPU_Scanout_Inventory.Outcome;
   Firmware_Mapped, ADS_Mapped, Log_Mapped, CT_Mapped : Boolean := False;
   PAT_Active : Boolean := False;
   function PAT_Owner_Ready return Boolean is
     (PAT_Active and then not Firmware_Mapped and then not ADS_Mapped and then
      not Log_Mapped and then not CT_Mapped and then PCI_Device = 16#46D2# and then
      Publication_Owner_Base);
   package PAT_IO is new Intel_GPU_Native_PAT (PAT_Owner_Ready);
   package Native_PAT is new Intel_GPU_ADLN_PAT
     (PAT_Owner_Ready, PAT_IO.Read32, PAT_IO.Write32);
   PAT_Attempt : Native_PAT.Attempt;
   MOCS_Active : Boolean := False;
   function MOCS_Owner_Ready return Boolean is
     (MOCS_Active and then PAT_Ready and then not Firmware_Mapped and then
      not ADS_Mapped and then not Log_Mapped and then not CT_Mapped and then
      PCI_Device = 16#46D2# and then Publication_Owner_Base);
   package MOCS_IO is new Intel_GPU_Native_MOCS (MOCS_Owner_Ready);
   package Native_MOCS is new Intel_GPU_MOCS_Configure
     (MOCS_Owner_Ready, MOCS_IO.Read32, MOCS_IO.Write32);
   MOCS_Attempt : Native_MOCS.Attempt;
   GT_Settings_Active : Boolean := False;
   function GT_Settings_Owner return Boolean is
     (GT_Settings_Active and then PAT_Ready and then MOCS_Ready and then
      not Firmware_Mapped and then not ADS_Mapped and then not Log_Mapped and then
      not CT_Mapped and then PCI_Device = 16#46D2# and then
      Intel_GPU_Native_Reset.ADS_Observed and then Publication_Owner_Base);
   package GT_Settings_IO is new Intel_GPU_Native_GT_Settings
     (GT_Settings_Owner, Intel_GPU_Native_Reset.ADS_Inventory,
      Intel_GPU_Native_Reset.ADS_Topology);
   package GT_Settings is new Intel_GPU_GT_Configure
     (GT_Settings_Owner, GT_Settings_IO.Read32, GT_Settings_IO.Write32);
   GT_Settings_Attempt : GT_Settings.Attempt;
   Render_Settings_Active : Boolean := False;
   function Render_Settings_Owner return Boolean is
     (Render_Settings_Active and then PAT_Ready and then MOCS_Ready and then GT_Settings_Ready and then
      not Firmware_Mapped and then not ADS_Mapped and then not Log_Mapped and then
      not CT_Mapped and then PCI_Device = 16#46D2# and then
      Intel_GPU_Native_Reset.ADS_Observed and then Publication_Owner_Base);
   package Render_Settings_IO is new Intel_GPU_Native_Engine_Settings
     (Intel_GPU_ADLN_Inventory.Render, Render_Settings_Owner,
      Intel_GPU_Native_Reset.ADS_Inventory, Intel_GPU_Native_Reset.ADS_Topology);
   package Render_Settings is new Intel_GPU_Engine_Configure
     (Render_Settings_Owner, Render_Settings_IO.Read32, Render_Settings_IO.Write32);
   Render_Settings_Attempt : Render_Settings.Attempt;
   ADS_GPU_Start, Log_GPU_Start, CT_GPU_Start : Unsigned_64 := 0;
   Submission_GPU_Start, Engine_Status_GPU_Start : Unsigned_64 := 0;
   package Submission_Buffers is new Intel_GPU_Submission_Buffer (Publication_Owner_Ready);
   Submission_State : Submission_Buffers.Buffer_State;
   -- Not exported yet. Never substitute the complete private allocation when
   -- wiring read-only presentation; it also contains command and table pages.
   Completed_Probe_Pixels : Intel_GPU_Buffer_Reply.Backing;
   -- The diagnostic uses allocator slot1; GuC context IDs are a separate
   -- namespace. Keep this allocation retained even if publication fails.
   package Buffer_Memory is new Intel_GPU_Buffer_Memory
     (Publication_Owner_Ready, Intel_GPU_Metadata_Platform.Storage);
   Buffer_Pool : Buffer_Memory.Pool;
   Budget_Query : Intel_GPU_Budget_Query.Query;
   Budget_Logged : Boolean := False;
   Budget_Reply_Pending : Boolean := False;
   Budget_Reply_Slot : constant CapabilitySlot := 61;
   Runtime_Admitted, Runtime_Fault : Boolean := False;
   CPU_Coherence_Checked, GPU_Coherence_Checked : Boolean := False;
   Submission_Slot : constant Intel_GPU_Buffer_Backing.Slot := 1;
   Submission_Bytes : constant Unsigned_64 :=
     Intel_GPU_Submission_Backing.After_Last - Intel_GPU_Submission_Backing.First;
   Submission_CPU : constant Unsigned_64 := Intel_GPU_Buffer_Backing.CPU_Base;
   -- This is the first allocation in the arena. Verify that identity before
   -- using the fixed mapping instances below; never infer it from the slot ID.
   Submission_Allocation : Intel_GPU_Buffer_Reply.Backing;
   -- Bound only after authenticated startup. Admission additionally requires
   -- current backend readiness and reciprocal recipient-slot installation.
   Render_Admission : Intel_GPU_Render_Control.Controller;
   function Application_Session (From, Stamp : Unsigned_64) return Unsigned_64 is
     (Intel_GPU_Render_Control.Resolve (Render_Admission, From, Stamp));
   package Application_Buffers is new Intel_GPU_Buffer_Requests
     (Application_Session, Publication_Owner_Ready, First_Slot => 2);
   package Context_Tickets is new Application_Buffers.Contexts;
   package Closed_Table_Tickets is new Application_Buffers.Closed_Tables;
   Application_Buffer_State : Application_Buffers.Service;
   Boot_Update_Allocation : Intel_GPU_Buffer_Reply.Backing;
   -- Device-lifetime zero page for eventual GGTT scratch replacement. Never
   -- register as an application BO or release with an individual session.
   Retirement_Scratch : Intel_GPU_Buffer_Reply.Backing;
   Boot_Update_Candidate : Submission_Buffers.VM.Image;
   procedure Application_Recipient
     (From, Stamp : Unsigned_64; Slot : out CapabilitySlot;
      Identity : out Unsigned_64) is
      Session : constant Unsigned_64 := Application_Session (From, Stamp);
      Recorded_Slot : constant Unsigned_64 :=
        Intel_GPU_Render_Control.Stored_Recipient_Slot (Render_Admission, Session);
   begin
      Slot := 0;
      Identity := 0;
      if Recorded_Slot = 0 then return; end if;
      -- Startup layout: one immutable recipient endpoint per session
      -- in40..55, after hardware pages32..39. No capability is minted here.
      -- The broker populates this range before activation and retains slots
      -- through retirement. Activation checks the kernel recipient identity.
      Slot := CapabilitySlot (Recorded_Slot);
      Identity := Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, From, Stamp);
   end Application_Recipient;
   package Application_Maps is new Application_Buffers.Sharing (Application_Recipient);
   Application_Map_State : Application_Maps.Mapping_Table;
   Buffer_Retirement_Pending : Application_Buffers.Ticket := 0;
   -- The pending ticket remains the group anchor/admission gate; this ticket
   -- identifies the one physical allocation currently submitted for release.
   Table_Release_Ticket : Application_Buffers.Ticket := 0;
   Buffer_Retirement_Has_Reply : Boolean := False;
   Buffer_Retirement_Is_Private : Boolean := False;
   Buffer_Retirement_Is_Context : Boolean := False;
   procedure Finish_Context_Retirement;
   Buffer_Retirement_Is_Closed_Table : Boolean := False;
   procedure Finish_Closed_Table_Retirement;
   Buffer_Retirement_Is_Teardown : Boolean := False;
   procedure Finish_Teardown_Buffer_Retirement;
   Buffer_Retirement_Session, Buffer_Retirement_Sender, Buffer_Retirement_Stamp : Unsigned_64 := 0;
   package Deferred_Retirement renames Intel_GPU_Deferred_Retirement;
   Deferred_Closes : Deferred_Retirement.Queue;
   function Application_Work_Drained (Session : Unsigned_64) return Boolean;
   procedure Report_Closed_Buffer (Session, ID : Unsigned_64);
   function Try_Retire_Closed_Buffer
     (From : ProcessID; Msg : Message; With_Reply : Boolean := True) return Boolean;
   procedure Handle_Application_Map (From : ProcessID; Msg : Message) is
      Response : Application_Buffers.Words;
      Created : Application_Maps.Mapping_ID;
      Reply_Message : Message := NULL_MESSAGE;
      Delivery : Unsigned_64;
   begin
      if Msg.tag.length = 4 and then Msg.tag.flags = 0 and then Msg.tag.reserved = 0 and then
        (Msg.words (0) and 16#FFFF_FFFF#) = Application_Buffers.Version and then
        Shift_Right (Msg.words (0), 32) = Application_Maps.Map_Presentation and then
        not Application_Work_Drained
          (Application_Session (Unsigned_64 (From), Msg.authorityTag))
      then
         Reply_Message.tag := (Application_Maps.Map_Label, 4, 0, 0);
         Reply_Message.words := [Application_Buffers.Denied, Application_Buffers.Version, 0, 0];
         Delivery := reply (From, Reply_Message);
         return;
      end if;
      Application_Maps.Handle
        (Application_Buffer_State, Application_Map_State,
         Unsigned_64 (From), Msg.authorityTag, Msg.tag.label, Msg.tag.length,
         Msg.tag.flags, Msg.tag.reserved,
         [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Response, Created);
      Reply_Message.tag := (Application_Maps.Map_Label, 4, 0, 0);
      Reply_Message.words := [Response (0), Response (1), Response (2), Response (3)];
      Delivery := reply (From, Reply_Message);
      if Delivery /= 1 then
         Application_Maps.Reject_Delivery (Application_Buffer_State, Application_Map_State, Created);
      end if;
   end Handle_Application_Map;
   Application_Pending : Application_Buffers.Ticket := 0;
   -- Private parents are retained even after admission is revoked. They are
   -- never passed to Application_Buffers.Complete or exported as BO handles.
   package Application_State renames Intel_GPU_Application_State;
   package Application_Lifetime renames Intel_GPU_Application_Lifetime;
   use type Application_Lifetime.Phase;
   package Application_VM renames Application_State.VM;
   package Application_Topology is new Application_VM.Growth;
   package Application_Binding is new Application_Buffers.Binding (Application_VM);
   Private_Contexts : Application_State.Context_Array renames Application_State.Items;
   Private_Pending : Application_Buffers.Ticket := 0;
   Update_Pending : Application_Buffers.Ticket := 0;
   Update_Table_Pages : Natural range 0 .. Application_State.Table_Pages := 0;
   In_Place_Active : Boolean := False;
   In_Place_Inserting : Boolean := False;
   type Replacement_Record is record
      Ticket : Application_Buffers.Ticket := 0;
      Session, Sender, Stamp, Revision, Root : Unsigned_64 := 0;
      Superseded : Boolean := False;
   end record;
   package Replacement_Records is new Intel_GPU_Record_Store
     (Replacement_Record, (others => <>));
   Replacement_Tables : Replacement_Records.Store;
   function Table_Allocation_Admitted
     (Session, Ticket : Unsigned_64; Slot : Positive) return Boolean;
   function Table_Allocation_Retired (Session, Ticket : Unsigned_64) return Boolean;
   package Table_Allocations is new Intel_GPU_Table_Allocations
     (Table_Allocation_Admitted, Table_Allocation_Retired);
   Table_Backing_Registry : Table_Allocations.Registry;
   procedure Publish_Snapshot (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information);
   type Metadata_Table is (Tickets, Handles, Backing, Replacements, Retirement, Updates, Table_Backing);
   function Metadata_Capacity (Table : Metadata_Table) return Natural is
     (case Table is
         when Tickets => Application_Buffers.Record_Capacity (Application_Buffer_State),
         when Handles => Application_Buffers.Handle_Capacity (Application_Buffer_State),
         when Backing => Buffer_Memory.Record_Capacity (Buffer_Pool),
         when Replacements => Replacement_Records.Capacity (Replacement_Tables),
         when Table_Backing => Table_Allocations.Capacity (Table_Backing_Registry),
         when Retirement => Deferred_Retirement.Capacity (Deferred_Closes),
         when Updates => Application_State.Update_Capacity);
   procedure Extend_Metadata
     (Table : Metadata_Table; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      case Table is
         when Tickets => Application_Buffers.Extend_Tickets (Application_Buffer_State, Base, Bytes, Accepted);
         when Handles => Application_Buffers.Extend_Handles (Application_Buffer_State, Base, Bytes, Accepted);
         when Backing => Buffer_Memory.Extend_Records (Buffer_Pool, Base, Bytes, Accepted);
         when Replacements => Replacement_Records.Extend (Replacement_Tables, Base, Bytes, Accepted);
         when Table_Backing => Table_Allocations.Extend (Table_Backing_Registry, Base, Bytes, Accepted);
         when Retirement => Deferred_Retirement.Extend_Storage (Deferred_Closes, Base, Bytes, Accepted);
         when Updates => Application_State.Extend_Update_Index (Base, Bytes, Accepted);
      end case;
   end Extend_Metadata;
   procedure Admit_Metadata (Count : Positive; Accepted : out Boolean) is
   begin
      Application_Buffers.Admit_Slots
        (Application_Buffer_State, Count,
         (Backing => Metadata_Capacity (Backing),
          Replacements => Natural'Min (Metadata_Capacity (Replacements), Metadata_Capacity (Table_Backing)),
          Retirement => Metadata_Capacity (Retirement),
          Update_Index => Metadata_Capacity (Updates)), Accepted);
   end Admit_Metadata;
   package Metadata_Growth is new Intel_GPU_Metadata_Bundle
     (Metadata_Table, Intel_GPU_Metadata_Platform.Storage, Publication_Owner_Ready,
      Metadata_Capacity, Extend_Metadata, Admit_Metadata);
   Metadata : Metadata_Growth.Bundle;
   use type Metadata_Growth.Phase;
   function Metadata_Busy return Boolean is
     (Metadata_Growth.State (Metadata) not in Metadata_Growth.Idle | Metadata_Growth.Failed);
   function Map_Capacity return Positive is
     (Application_Maps.Record_Capacity (Application_Map_State));
   procedure Publish_Map_Metadata
     (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Application_Maps.Extend_Storage (Application_Map_State, Base, Bytes, Accepted);
   end Publish_Map_Metadata;
   package Map_Growth is new Intel_GPU_Record_Growth
     (Intel_GPU_Metadata_Platform.Storage, Map_Capacity, Publish_Map_Metadata);
   Map_Metadata : Map_Growth.Controller;
   Map_Growth_Disabled : Boolean := False;
   function Map_Metadata_Busy return Boolean is
     (not Map_Growth_Disabled and then Map_Growth.Snapshot (Map_Metadata).State not in
        Map_Growth.Empty | Map_Growth.Idle | Map_Growth.Failed);
   procedure Grow_Map_Metadata is
      Quota : constant Positive := 1_048_576;
      Accepted : Boolean;
      use type Map_Growth.Phase;
   begin
      if Map_Growth_Disabled then return; end if;
      if not Publication_Owner_Ready then
         -- Retain committed metadata after ownership loss; do not wedge
         -- service retirement behind an unfinished growth controller.
         if Map_Metadata_Busy then Map_Growth_Disabled := True; end if;
         return;
      end if;
      if Metadata_Busy or else Application_Pending /= 0 or else Private_Pending /= 0 or else
        Update_Pending /= 0 or else In_Place_Active or else Buffer_Retirement_Pending /= 0 or else
        Buffer_Memory.Pending (Buffer_Pool) then return; end if;
      case Map_Growth.Snapshot (Map_Metadata).State is
         when Map_Growth.Empty =>
            Map_Growth.Configure (Map_Metadata, 64 * 1024 * 1024, Quota, Accepted);
            Map_Growth_Disabled := not Accepted;
         when Map_Growth.Idle =>
            if Application_Maps.Needs_Growth (Application_Map_State) and then Map_Capacity < Quota then
               Map_Growth.Request (Map_Metadata, Positive'Min (Quota, Map_Capacity * 2), Accepted);
               Map_Growth_Disabled := not Accepted;
               if Accepted then Publish_Snapshot ("intel-gpu: CPU grant metadata growth beginning"); end if;
            end if;
         when Map_Growth.Failed =>
            Map_Growth_Disabled := True;
         when others =>
            Map_Growth.Step (Map_Metadata);
            if Map_Growth.Snapshot (Map_Metadata).State = Map_Growth.Idle then
               Publish_Snapshot ("intel-gpu: CPU grant metadata ready slots=" & Positive'Image (Map_Capacity));
            elsif Map_Growth.Snapshot (Map_Metadata).State = Map_Growth.Failed then
               Map_Growth_Disabled := True;
               Publish_Snapshot ("intel-gpu: CPU grant metadata growth failed; prior prefix retained");
            end if;
      end case;
   end Grow_Map_Metadata;
   procedure Grow_Metadata is
      Quota : constant Positive := 1_048_576;
      Available : constant Positive := Application_Buffers.Committed_Slots (Application_Buffer_State);
      Next_Slot : constant Natural := Application_Buffers.Next_Fresh_Slot (Application_Buffer_State);
      Accepted : Boolean;
   begin
      if Application_Pending /= 0 or else Private_Pending /= 0 or else
        Update_Pending /= 0 or else In_Place_Active or else Buffer_Retirement_Pending /= 0 or else
        Buffer_Memory.Pending (Buffer_Pool) then return; end if;
      if Metadata_Growth.State (Metadata) = Metadata_Growth.Idle and then
        Next_Slot > Available and then Available < Quota
      then
         Metadata_Growth.Request (Metadata, Positive'Min (Quota, Available * 2),
           Quota, 64 * 1024 * 1024, Accepted);
         if Accepted then Publish_Snapshot ("intel-gpu: allocation metadata growth beginning"); end if;
         return;
      end if;
      if Metadata_Busy then
         Metadata_Growth.Step (Metadata);
         if Metadata_Growth.State (Metadata) = Metadata_Growth.Idle then
            Publish_Snapshot ("intel-gpu: allocation metadata ready slots=" &
              Positive'Image (Application_Buffers.Committed_Slots (Application_Buffer_State)));
         elsif Metadata_Growth.State (Metadata) = Metadata_Growth.Failed then
            Publish_Snapshot ("intel-gpu: allocation metadata growth failed; prior prefix retained");
         end if;
      end if;
   end Grow_Metadata;
   Current_Table_Ticket : array (Private_Contexts'Range) of Application_Buffers.Ticket := [others => 0];
   Next_Table_Retirement : Intel_GPU_Buffer_Backing.Slot := Intel_GPU_Buffer_Backing.Slot'First;
   type Table_Retirement_Checkpoint is
     (No_Candidate, Readiness, Identity, Backing_Validation, Current_VM,
      Contexts_Running, Alias_Check, Retirement_Queued, Retirement_Submitted, Retirement_Rejected);
   Last_Table_Retirement : Table_Retirement_Checkpoint := No_Candidate;
   Last_Table_Retirement_Slot : Natural := 0;
   Private_Session, Private_Identity : Unsigned_64 := 0;
   -- Separate from budget61 and application62; reserved through completion.
   Activation_Reply_Slot : constant CapabilitySlot := 60;
   Activation_Reply_Pending : Boolean := False;
   Activation_Session, Activation_Identity : Unsigned_64 := 0;
   function Session_Healthy (Session : Unsigned_64) return Boolean;
   function Render_Backend_Ready return Boolean;
   -- Initial bounded VM budget:64 tables, four private scratch pages and context.
   -- No application data is placed in this allocation.
   -- Physical bootstrap is separate from CPU mirror capacity. Sparse binds
   -- acquire further owned table pages through the deferred offline/live paths.
   Private_Table_Pages : constant := 4;
   Private_Pages : constant Intel_GPU_Buffer_Backing.Page_Count :=
     Intel_GPU_Submission_Image.Byte_Count / 4096 + Private_Table_Pages + 4;
   function Table_Ledger_Busy return Boolean;
   procedure Start_Table_Ledger (Index : Positive; Session : Unsigned_64);
   function Initial_Table_Mapping (Index : Positive; Session : Unsigned_64;
      Page : Positive) return Intel_GPU_Table_Provenance.Mapping;
   procedure Finish_Private_Context (Backing : Intel_GPU_Buffer_Reply.Backing) is
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Private_Session);
      Completed_Ticket : constant Application_Buffers.Ticket := Private_Pending;
      Consumed : Boolean;
   begin
      Application_Buffers.Finish_Private
        (Application_Buffer_State, Private_Pending, Consumed);
      if not Consumed then return; end if;
      Private_Pending := 0;
      if Stored = 0 then return; end if;
      declare
         Index : constant Positive := Stored;
         Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages;
         Initialized, Eligible : Boolean;
      begin
         if Completed_Ticket = 0 or else
           Private_Contexts (Index).Parent_Ticket /= Completed_Ticket or else
           Application_Buffers.Ticket_Session (Application_Buffer_State, Completed_Ticket) /= Private_Session
         then
            Runtime_Fault := True;
            Private_Contexts (Index).Life := Application_Lifetime.Retired;
            return;
         end if;
         Private_Contexts (Index).Parent := Backing;
         Eligible := Backing.Ready and then
           Backing.Bytes = Unsigned_64 (Private_Pages) * 4096 and then
           Publication_Owner_Ready and then
           Application_Session (Private_Identity and 16#FFFF_FFFF#,
                                Private_Session) = Private_Session and then
           Intel_GPU_Render_Control.Recipient_Identity
             (Render_Admission, Private_Identity and 16#FFFF_FFFF#,
              Private_Session) = Private_Identity;
         if not Eligible then
            Private_Contexts (Index).Life := Application_Lifetime.Retired;
            return;
         end if;
         Private_Contexts (Index).Context := Intel_GPU_Buffer_Reply.Slice
           (Backing, 0, Intel_GPU_Submission_Image.Byte_Count);
         Private_Contexts (Index).Tables := Intel_GPU_Buffer_Reply.Slice
           (Backing, Intel_GPU_Submission_Image.Byte_Count, Private_Table_Pages * 4096);
         Private_Contexts (Index).Scratch := Intel_GPU_Buffer_Reply.Slice
           (Backing, Intel_GPU_Submission_Image.Byte_Count + Private_Table_Pages * 4096, 4 * 4096);
         if not Private_Contexts (Index).Context.Ready or else
           not Private_Contexts (Index).Tables.Ready or else
           not Private_Contexts (Index).Scratch.Ready then
            Private_Contexts (Index).Life := Application_Lifetime.Retired;
            return;
         end if;
         for L in Scratch'Range loop
            Scratch (L) := Intel_GPU_Buffer_Reply.Page_Address
              (Private_Contexts (Index).Scratch, Unsigned_64 (L) * 4096);
         end loop;
         declare
            function Read_Table (Page : Application_VM.Page_Number) return Unsigned_64 is
              (Intel_GPU_Buffer_Reply.Page_Address
                (Private_Contexts (Index).Tables, Unsigned_64 (Page - 1) * 4096));
            procedure Initialize_Tables is new Application_VM.Initialize_From_Pages (Read_Table);
         begin
            Initialize_Tables (Private_Contexts (Index).Source,
              Backing_Count => Private_Table_Pages, Accepted => Initialized, Scratch => Scratch);
         end;
         Private_Contexts (Index).Life := Application_Lifetime.Allocate
           (Private_Contexts (Index).Life, Initialized);
         if Initialized then Start_Table_Ledger (Index, Private_Session); end if;
      end;
   end Finish_Private_Context;
   procedure Start_Private_Context
     (Session, Identity : Unsigned_64; Started : out Boolean) is
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
   begin
      Started := False;
      if Stored = 0
        or else Private_Pending /= 0 or else Table_Ledger_Busy or else
        Application_Session (Identity and 16#FFFF_FFFF#, Session) /= Session or else
        Intel_GPU_Render_Control.Recipient_Identity
          (Render_Admission, Identity and 16#FFFF_FFFF#, Session) /= Identity
      then return; end if;
      declare
         Index : constant Positive := Stored;
      begin
         if Private_Contexts (Index).Attempted then return; end if;
         Context_Tickets.Reserve (Application_Buffer_State, Session, Private_Pending,
           Pages => Private_Pages);
         if Private_Pending = 0 then return; end if;
         Private_Contexts (Index).Attempted := True;
         Private_Contexts (Index).Parent_Ticket := Private_Pending;
         Private_Session := Session;
         Private_Identity := Identity;
         Buffer_Memory.Start
           (Buffer_Pool, Application_Buffers.Ticket_Slot (Private_Pending),
            Private_Pages, Started);
         if not Started then Finish_Private_Context ((Ready => False)); end if;
      end;
   end Start_Private_Context;
   procedure Retire_Application_Resources (Session : Unsigned_64);
   function Try_Offline_Bind_Growth (From : ProcessID; Msg : Message) return Boolean;
   procedure Complete_Render_Activation is
      package Control renames Intel_GPU_Render_Control;
      Recorded_Slot : constant Unsigned_64 :=
        Control.Stored_Recipient_Slot (Render_Admission, Activation_Session);
      Response : Message := NULL_MESSAGE;
      Accepted : Boolean;
      Delivery : Unsigned_64;
   begin
      if not Activation_Reply_Pending or else Private_Pending /= 0 or else
        Table_Ledger_Busy then return; end if;
      Accepted := Recorded_Slot /= 0 and then Control.Recipient_Identity (Render_Admission,
          Activation_Identity and 16#FFFF_FFFF#, Activation_Session) = Activation_Identity
        and then CuBit.Capability_Grants.Endpoint_Matches
          (CapabilitySlot (Recorded_Slot),
           Activation_Identity)
        and then Session_Healthy (Activation_Session);
      if not Accepted then
         Control.Reject_Delivery (Render_Admission, Activation_Identity, Activation_Session);
         Retire_Application_Resources (Activation_Session);
      end if;
      Response.tag := (Control.Label, 4, 0, 0);
      Response.words := [(if Accepted then Control.OK else Control.Unavailable),
                        Control.Version, Activation_Session, 0];
      Activation_Reply_Pending := False;
      Delivery := replyCap (Activation_Reply_Slot, Response);
      if Delivery /= 1 and then Accepted then
         Control.Reject_Delivery (Render_Admission, Activation_Identity, Activation_Session);
         Retire_Application_Resources (Activation_Session);
      end if;
   end Complete_Render_Activation;
   procedure Handle_Application_Bind (From : ProcessID; Msg : Message) is
      Session : constant Unsigned_64 := Application_Session (Unsigned_64 (From), Msg.authorityTag);
      Identity : constant Unsigned_64 := Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, Unsigned_64 (From), Msg.authorityTag);
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
      Response : Application_Buffers.Words := [Application_Buffers.Denied, 1, 0, 0];
      Reply_Message : Message := NULL_MESSAGE;
      Delivery : Unsigned_64;
   begin
      if Try_Offline_Bind_Growth (From, Msg) then return; end if;
      if Stored /= 0 then
         declare Index : constant Positive := Positive (Stored); begin
            Response (0) := Application_Buffers.Unavailable;
            if Update_Pending = 0 and then not In_Place_Active and then not Table_Ledger_Busy and then
              Private_Contexts (Index).Life = Application_Lifetime.Offline then
               Application_Binding.Handle
                 (Application_Buffer_State, Private_Contexts (Index).Source, Session,
                  Unsigned_64 (From), Msg.authorityTag, Msg.tag.label, Msg.tag.length,
                  Msg.tag.flags, Msg.tag.reserved,
                  [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Response);
            end if;
         end;
      end if;
      Reply_Message.tag := (Application_Binding.Bind_Label, 4, 0, 0);
      Reply_Message.words := [Response (0), Response (1), Response (2), Response (3)];
      Delivery := reply (From, Reply_Message);
      if Delivery /= 1 and then Response (0) = Application_Buffers.OK then
         -- A committed mapping with an undelivered reply cannot safely be
         -- treated as an allocation failure by Mesa followed by VA reuse.
         -- Close admission using the captured incarnation BEFORE resource
         -- retirement. Preserve the VM and backing; this is not rollback or
         -- hardware completion. No later PID lookup or capability minting.
         Intel_GPU_Render_Control.Reject_Delivery (Render_Admission, Identity, Session);
         Retire_Application_Resources (Session);
      end if;
   end Handle_Application_Bind;
   -- Dedicated empty deferred-reply slot. saveReplyCap fails without replacing
   -- an occupied slot; no incoming request may select this capability slot.
   Application_Reply_Slot : constant CapabilitySlot := 62;
   procedure Submit_Budget_Query is
      Token : Unsigned_64;
      Outgoing : Message := NULL_MESSAGE;
   begin
      Intel_GPU_Budget_Query.Start
        (Budget_Query, syscall (SYSCALL_GETTIME),
         Publication_Owner_Ready and then not Runtime_Fault, Token);
      if Token /= 0 then
         Outgoing.tag := (Intel_GPU_Buffer_Backing.Budget_Request_Label, 4, 0, 0);
         Outgoing.words := [Intel_GPU_Buffer_Backing.Budget_Version, 0, 0, 0];
         if not capSubmit (15, Outgoing, Token) then
            Intel_GPU_Budget_Query.Cancel (Budget_Query);
         end if;
      end if;
   end Submit_Budget_Query;
   procedure Handle_Budget_Query (From : ProcessID; Msg : Message) is
      package P renames Intel_GPU_Budget_Protocol;
      Response : Message := NULL_MESSAGE;
      Code : Unsigned_64 := P.Bad_Request;
      Delivered : Unsigned_64;
      pragma Unreferenced (Delivered);
   begin
      if P.Valid_Request (Msg.tag.label, Msg.tag.length, Msg.tag.flags,
                          Msg.tag.reserved, Intel_GPU_Buffer_Backing.Budget_Words (Msg.words)) then
         Code := P.Unavailable;
         if Budget_Reply_Pending or else Intel_GPU_Budget_Query.Pending (Budget_Query) then
            Code := P.Busy;
         elsif Publication_Owner_Ready and then not Runtime_Fault then
            -- Capture kernel reply authority, never defer by reusable PID.
            -- A failed save does not replace a previously occupied slot.
            if saveReplyCap (Unsigned_64 (Budget_Reply_Slot)) = 1 then
               Budget_Reply_Pending := True;
               Submit_Budget_Query;
               return;
            end if;
         end if;
      end if;
      Response.tag := (P.Label, 4, 0, 0);
      Response.words := [Code, 0, 0, 0];
      Delivered := reply (From, Response);
   end Handle_Budget_Query;
   procedure Finish_Application_Buffer (Backing : Intel_GPU_Buffer_Reply.Backing) is
      Ticket : constant Application_Buffers.Ticket := Application_Pending;
      Response : Application_Buffers.Words;
      Consumed : Boolean;
      Reply_Message : Message := NULL_MESSAGE;
      Delivery : Unsigned_64;
   begin
      Application_Buffers.Complete
        (Application_Buffer_State, Application_Pending, Backing, Response, Consumed);
      if not Consumed then return; end if;
      Application_Pending := 0;
      if Response (0) = Application_Buffers.Unavailable then
         Publish_Snapshot ("intel-gpu: allocation unavailable reason=" &
           Application_Buffers.Allocation_Outcome'Image
             (Application_Buffers.Last_Allocation (Application_Buffer_State)));
         Publish_Snapshot ("intel-gpu: allocation backing stage=" &
           Buffer_Memory.Allocation_Stage'Image (Buffer_Memory.Last_Stage (Buffer_Pool)));
      end if;
      Reply_Message.tag := (Application_Buffers.Label, 4, 0, 0);
      Reply_Message.words := [Response (0), Response (1), Response (2), Response (3)];
      Delivery := replyCap (Application_Reply_Slot, Reply_Message);
      if Delivery /= 1 then
         Application_Buffers.Reject_Delivery (Application_Buffer_State, Ticket);
      end if;
   end Finish_Application_Buffer;
   procedure Handle_Application_Buffer (From : ProcessID; Msg : Message) is
      Response : Application_Buffers.Words;
      Deferred : Application_Buffers.Ticket;
      Started, Consumed : Boolean;
      Reply_Message : Message := NULL_MESSAGE;
      Ignored : Unsigned_64;
      pragma Unreferenced (Ignored);
   begin
      if Buffer_Retirement_Pending /= 0 then
         Publish_Snapshot ("intel-gpu: allocation unavailable reason=RETIREMENT_PENDING");
         Reply_Message.tag := (Application_Buffers.Label, 4, 0, 0);
         Reply_Message.words := [Application_Buffers.Unavailable, 1, 0, 0];
         Ignored := reply (From, Reply_Message);
         return;
      end if;
      Application_Buffers.Handle
        (Application_Buffer_State, Unsigned_64 (From), Msg.authorityTag,
         Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
         [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Response, Deferred);
      if Deferred = 0 and then Response (0) = Application_Buffers.Unavailable then
         Publish_Snapshot ("intel-gpu: allocation unavailable reason=" &
           Application_Buffers.Allocation_Outcome'Image
             (Application_Buffers.Last_Allocation (Application_Buffer_State)));
      end if;
      if Msg.words (1) = Application_Buffers.Close and then
        Response (0) = Application_Buffers.Denied
      then
         Publish_Snapshot ("intel-gpu: close rejected handle=" &
           Unsigned_64'Image (Msg.words (2)) & " reason=" &
           Application_Buffers.Close_Diagnostic
             (Application_Buffer_State, Unsigned_64 (From), Msg.authorityTag,
              Msg.words (2))'Image);
      end if;
      if Deferred /= 0 then
         if saveReplyCap (Unsigned_64 (Application_Reply_Slot)) = 1 then
            Application_Pending := Deferred;
            Buffer_Memory.Start
              (Buffer_Pool, Application_Buffers.Ticket_Slot (Deferred),
               Intel_GPU_Buffer_Backing.Page_Count (Msg.words (2) / 4096), Started);
            if not Started then Finish_Application_Buffer ((Ready => False)); end if;
            return;
         end if;
         -- No allocation was submitted. Retire the ticket and use only the
         -- current thread's reply authority; never fall back to a saved PID.
         Application_Buffers.Complete
           (Application_Buffer_State, Deferred, (Ready => False), Response, Consumed);
         Publish_Snapshot
           ("intel-gpu: allocation unavailable reason=REPLY_CAP_UNAVAILABLE");
      end if;
      Reply_Message.tag := (Application_Buffers.Label, 4, 0, 0);
      Reply_Message.words := [Response (0), Response (1), Response (2), Response (3)];
      if Msg.words (1) = Application_Buffers.Close and then
        Response (0) = Application_Buffers.OK
      then
         Report_Closed_Buffer
           (Application_Session (Unsigned_64 (From), Msg.authorityTag), Msg.words (2));
         if Try_Retire_Closed_Buffer (From, Msg) then return; end if;
         for Slot in 1 .. Application_Buffers.Committed_Slots (Application_Buffer_State) loop
            declare
               Candidate : constant Application_Buffers.Closed_Allocation :=
                 Application_Buffers.Closed_At (Application_Buffer_State, Slot);
            begin
               if Candidate.Ready and then
                 Candidate.Session = Application_Session (Unsigned_64 (From), Msg.authorityTag) and then
                 Unsigned_64 (Candidate.Handle) = Msg.words (2)
               then
                  Deferred_Retirement.Remember
                    (Deferred_Closes, Slot,
                     (Candidate.ID, Candidate.Session, Unsigned_64 (From),
                      Msg.authorityTag, Unsigned_64 (Candidate.Handle)));
               end if;
            end;
         end loop;
      end if;
      Ignored := reply (From, Reply_Message);
   end Handle_Application_Buffer;
   procedure Retire_Application_Context (Session : Unsigned_64);
   procedure Retire_Application_Resources (Session : Unsigned_64) is
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
   begin
      if Stored /= 0 then
         Private_Contexts (Positive (Stored)).Life := Application_Lifetime.Retired;
      end if;
      Retire_Application_Context (Session);
      Application_Buffers.Retire_Session (Application_Buffer_State, Session);
      Application_Maps.Retire_Session (Application_Buffer_State, Application_Map_State, Session);
   end Retire_Application_Resources;
   procedure Handle_Close_Own (From : ProcessID; Msg : Message) is
      package Control renames Intel_GPU_Render_Control;
      Response : Control.Words;
      Reply_Message : Message := NULL_MESSAGE;
      Delivery : Unsigned_64;
   begin
      Control.Close_Own (Render_Admission, Unsigned_64 (From), Msg.authorityTag,
        Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
        [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Response);
      if Response (0) = Control.OK then
         -- Admission is already closed. Resource retirement may be pending;
         -- this reply does not acknowledge hardware/grant drain completion.
         Retire_Application_Resources (Response (2));
      end if;
      Reply_Message.tag := (Control.Close_Own_Label, 4, 0, 0);
      Reply_Message.words := [Response (0), Response (1), Response (2), Response (3)];
      Delivery := reply (From, Reply_Message);
   end Handle_Close_Own;
   procedure Handle_Render_Control (From : ProcessID; Msg : Message) is
      package Control renames Intel_GPU_Render_Control;
      Response : Control.Words;
      Reply_Message : Message := NULL_MESSAGE;
      Delivery : Unsigned_64;
      Started : Boolean;
      Recipient : constant Unsigned_64 := Control.Activation_Identity
        (Render_Admission, Unsigned_64 (From), Msg.authorityTag,
         Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
         [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)]);
      Recorded_Slot : constant Unsigned_64 :=
        Control.Stored_Recipient_Slot (Render_Admission, Msg.words (2));
      Recipient_Ready : Boolean := False;
      Backend_Ready : Boolean := False;
   begin
      if Recipient /= 0 and then Recorded_Slot /= 0 then
         -- Same immutable slots as Application_Recipient. Inspection does
         -- not authorize replacement: bootstrap must pin through retirement.
         Recipient_Ready := CuBit.Capability_Grants.Endpoint_Matches
           (CapabilitySlot (Recorded_Slot),
            Recipient);
      end if;
      -- Authenticate before probing backend registers. Abort requires no
      -- hardware readiness and remains possible after ownership loss.
      if Control.Is_Broker (Render_Admission, Unsigned_64 (From), Msg.authorityTag)
        and then Msg.tag = (Control.Label, 4, 0, 0)
        and then Msg.words (0) = Control.Version
        and then Msg.words (3) in Control.Reserve | Control.Activate
      then
         Backend_Ready := Render_Backend_Ready;
      end if;
      Control.Handle (Render_Admission, Unsigned_64 (From), Msg.authorityTag,
        Backend_Ready, Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
        [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Response,
        Recipient_Ready => Recipient_Ready);
      if Response (0) = Control.OK and then Msg.words (3) = Control.Abort_Session then
         -- Handle has closed admission before handles/grants are drained.
         Retire_Application_Resources (Response (2));
      end if;
      if Response (0) = Control.OK and then Msg.words (3) = Control.Activate then
         if not Activation_Reply_Pending and then Private_Pending = 0 and then
           saveReplyCap (Unsigned_64 (Activation_Reply_Slot)) = 1
         then
            Activation_Reply_Pending := True;
            Activation_Session := Response (2);
            Activation_Identity := Msg.words (1);
            Start_Private_Context (Activation_Session, Activation_Identity, Started);
            if not Started then
               Control.Reject_Delivery (Render_Admission, Activation_Identity, Activation_Session);
               Retire_Application_Resources (Activation_Session);
            end if;
            -- Allocation is asynchronous. Only the retained reply path may
            -- acknowledge activation after the private VM is actually usable.
            Complete_Render_Activation;
            return;
         end if;
         Control.Reject_Delivery (Render_Admission, Msg.words (1), Response (2));
         Retire_Application_Resources (Response (2));
         Response (0) := Control.Unavailable;
      end if;
      Reply_Message.tag := (Control.Label, 4, 0, 0);
      Reply_Message.words := [Response (0), Response (1), Response (2), Response (3)];
      Delivery := reply (From, Reply_Message);
      if Delivery /= 1 and then Response (0) = Control.OK then
         Control.Reject_Delivery (Render_Admission, Msg.words (1), Response (2));
         Retire_Application_Resources (Response (2));
      end if;
   end Handle_Render_Control;
   type Submission_View (Ready : Boolean := False) is record
      case Ready is
         when True => DMA_Address, CPU_Address, Bytes : Unsigned_64;
         when False => null;
      end case;
   end record;
   function Submission_Region (Part : Intel_GPU_Submission_Backing.Region)
     return Submission_View is
      Offset : constant Unsigned_64 := Intel_GPU_Submission_Backing.Offsets (Part) -
        Intel_GPU_Submission_Backing.First;
      Bytes : constant Unsigned_64 := Intel_GPU_Submission_Backing.Sizes (Part);
   begin
      if not Submission_Allocation.Ready or else
        not Intel_GPU_Submission_Backing.Valid_Layout or else
        Offset > Submission_Allocation.Bytes or else
        Bytes > Submission_Allocation.Bytes - Offset
      then return (Ready => False); end if;
      return (True, Intel_GPU_Buffer_Reply.Page_Address (Submission_Allocation, Offset),
        Submission_Allocation.CPU_Address + Offset, Bytes);
   end Submission_Region;
   Submission_Mapped, Engine_Status_Mapped : Boolean := False;
   CT_Backing_View : Intel_GPU_Firmware_Buffer.Prepared_CT_Buffer;
   Runtime_Ledger : Intel_GPU_GGTT_Reservations.Ledger;
   type Runtime_Kind is (No_Buffer, ADS_Buffer, Log_Buffer, CT_Buffer,
                         Submission_Buffer, Engine_Status_Buffer);
   Active_Runtime : Runtime_Kind := No_Buffer;
   Runtime_DMA, Runtime_CPU, Runtime_Bytes, Runtime_GPU : Unsigned_64 := 0;
   Runtime_Bound : Boolean := False;
   function Runtime_Page (Base, Offset : Unsigned_64) return Unsigned_64 is
   begin
      if Base /= Runtime_DMA or else Offset mod 4096 /= 0 or else
        Offset >= Runtime_Bytes or else Runtime_Bytes - Offset < 4096
      then return 0; end if;
      case Active_Runtime is
         when Submission_Buffer | Engine_Status_Buffer =>
            if not Submission_Allocation.Ready or else
              Runtime_CPU < Submission_Allocation.CPU_Address or else
              Runtime_CPU - Submission_Allocation.CPU_Address >= Submission_Allocation.Bytes or else
              Offset >= Submission_Allocation.Bytes -
                (Runtime_CPU - Submission_Allocation.CPU_Address)
            then return 0; end if;
            return Intel_GPU_Buffer_Reply.Page_Address
              (Submission_Allocation,
               Runtime_CPU - Submission_Allocation.CPU_Address + Offset);
         when ADS_Buffer | Log_Buffer | CT_Buffer =>
            return Intel_GPU_GGTT.Linear_Page (Base, Offset);
         when No_Buffer => return 0;
      end case;
   end Runtime_Page;
   function Runtime_Range_Allowed (First, Bytes : Unsigned_64) return Boolean is
     (GGTT_Takeover_Held and then Firmware_Mapped and then Publication_Owner_Ready and then Address_Layout.Valid and then
      Bytes > 0 and then First >= Address_Layout.Runtime_First and then
      First < Address_Layout.Runtime_Limit and then Bytes <= Address_Layout.Runtime_Limit - First and then
      Intel_GPU_Scanout_Inventory.No_Scanout_Overlap (Scanout, (True, First, Bytes)));
   function Runtime_Write_Allowed (Index, Value : Unsigned_64) return Boolean is
   begin
      if not Runtime_Bound or else Active_Runtime = No_Buffer or else
        Index < Runtime_GPU / 4096 or else
        Index - Runtime_GPU / 4096 >= Runtime_Bytes / 4096
      then return False; end if;
      return Runtime_Range_Allowed (Index * 4096, 4096) and then
        Value = Intel_GPU_GGTT.Encode_System_Page
          (Runtime_Page (Runtime_DMA, (Index - Runtime_GPU / 4096) * 4096));
   end Runtime_Write_Allowed;
   package Runtime_IO is new Intel_GPU_Native_GGTT
     (Publication_Owner_Ready, Intel_GPU_GGTT_Mapping.Bytes, Runtime_Write_Allowed);
   procedure Prepare_Runtime (First, Bytes : Unsigned_64; Success : out Boolean) is
   begin
      Success := False;
      if Runtime_Bound or else Bytes /= Runtime_Bytes or else
        not Runtime_Range_Allowed (First, Bytes)
      then return; end if;
      case Active_Runtime is
         when ADS_Buffer =>
            declare Current : constant Intel_GPU_ADS_Buffer.Prepared_Backing := Intel_GPU_ADS_Buffer.Prepared; begin
               if not Current.Ready or else Current.DMA_Address /= Runtime_DMA or else
                 Current.CPU_Address /= Runtime_CPU or else Current.Capacity /= Bytes
               then return; end if;
               Intel_GPU_ADS_Buffer.Initialize (First, Bytes, Success);
            end;
         when Log_Buffer =>
            declare Current : constant Intel_GPU_Firmware_Buffer.Prepared_Log_Buffer := Intel_GPU_Firmware_Buffer.Prepared_Log; begin
               if not Current.Ready or else Current.DMA_Address /= Runtime_DMA or else
                 Current.CPU_Address /= Runtime_CPU or else Current.Region_Bytes /= Bytes
               then return; end if;
               Success := Intel_GPU_DMA_Cache.Flush_Range (Runtime_CPU, Bytes);
            end;
         when CT_Buffer =>
            declare Current : constant Intel_GPU_Firmware_Buffer.Prepared_CT_Buffer := Intel_GPU_Firmware_Buffer.Prepared_CT; begin
               if not Current.Ready or else Current.DMA_Address /= Runtime_DMA or else
                 Current.CPU_Address /= Runtime_CPU or else Current.Region_Bytes /= Bytes
               then return; end if;
               Success := Intel_GPU_DMA_Cache.Flush_Range (Runtime_CPU, Bytes);
            end;
         when Submission_Buffer =>
            declare
               Current : constant Submission_View :=
                 Submission_Region
                   (Intel_GPU_Submission_Backing.Context_Image);
            begin
               if not Current.Ready or else Current.DMA_Address /= Runtime_DMA or else
                 Current.CPU_Address /= Runtime_CPU or else
                 Current.Bytes /= Intel_GPU_Submission_Backing.Sizes
                   (Intel_GPU_Submission_Backing.Context_Image) or else
                 Bytes /= Intel_GPU_Submission_Image.GGTT_Bytes
               then return; end if;
               Submission_Buffers.Initialize
                 (Submission_State, Submission_Allocation, First, Bytes, Success);
            end;
         when Engine_Status_Buffer =>
            declare
               Current : constant Submission_View :=
                 Submission_Region
                   (Intel_GPU_Submission_Backing.Engine_Status_Page);
            begin
               -- The earlier context preparation initialized the entire tail.
               -- Never zero it again after publishing any of its GGTT aliases.
               if not Submission_Mapped or else Engine_Status_Mapped or else
                 Submission_GPU_Start = 0 or else
                 Submission_Buffers.Initialized_GPU_Start (Submission_State) /= Submission_GPU_Start or else
                 not Current.Ready or else Current.DMA_Address /= Runtime_DMA or else
                 Current.CPU_Address /= Runtime_CPU or else
                 Current.Bytes /= Bytes or else Bytes /= 4096
               then return; end if;
               Success := Intel_GPU_DMA_Cache.Flush_Range (Runtime_CPU, Bytes);
            end;
         when No_Buffer => return;
      end case;
      if Success then Runtime_GPU := First; Runtime_Bound := True; end if;
   end Prepare_Runtime;
   package Runtime_Publication is new Intel_GPU_GGTT_Publish
     (Runtime_Range_Allowed, Prepare_Runtime, Runtime_IO.Read_PTE,
      Runtime_IO.Write_PTE, Invalidate_Upload, 16 * 1024 * 1024,
      Resolve_Page => Runtime_Page);
   ADS_Attempt, Log_Attempt, CT_Attempt, Submission_Attempt : Runtime_Publication.Attempt;
   Engine_Status_Attempt : Runtime_Publication.Attempt;
   Startup_Active, Startup_Access_Fault : Boolean := False;
   function Startup_Owner_Ready return Boolean is
     (Startup_Active and then not Startup_Access_Fault and then
      Firmware_Mapped and then ADS_Mapped and then Log_Mapped and then
      Publication_Owner_Ready and then Selected_WOPCM.Valid and then
      Upload_Backing.Ready and then
      Intel_GPU_GuC_Parameters.Valid (Startup_Parameters) and then
      Intel_GPU_ADS_Buffer.Initialized_GPU_Start = ADS_GPU_Start);
   package GuC_IO is new Intel_GPU_Native_GuC_IO (Startup_Owner_Ready);
   function GuC_Read (Offset : Unsigned_32) return Unsigned_32 is
      Value : constant Unsigned_32 := GuC_IO.Read32 (Offset);
   begin
      if Value = Unsigned_32'Last then Startup_Access_Fault := True; end if;
      return Value;
   end GuC_Read;
   procedure GuC_Write (Offset, Value : Unsigned_32; Success : out Boolean) is
   begin
      GuC_IO.Write32 (Offset, Value, Success);
      if not Success then Startup_Access_Fault := True; end if;
   end GuC_Write;
   procedure GuC_Byte (Offset : Unsigned_64; Value : out Unsigned_8;
                       Success : out Boolean) is
      Current : constant Intel_GPU_Firmware_Buffer.Prepared_Buffer := Intel_GPU_Firmware_Buffer.Prepared;
      use System.Storage_Elements;
   begin
      Value := 0; Success := False;
      if not Startup_Owner_Ready or else not Current.Ready or else
        Current.CPU_Address /= Upload_Backing.CPU_Address or else
        Current.DMA_Address /= Upload_Backing.DMA_Address or else
        Current.Content_Bytes /= Upload_Backing.Content_Bytes or else
        Current.Allocation_Bytes /= Upload_Backing.Allocation_Bytes or else
        Offset >= Current.Content_Bytes or else Offset >= Upload_Region or else
        Current.CPU_Address > Unsigned_64'Last - Offset
      then Startup_Access_Fault := True; return; end if;
      declare
         Byte : Unsigned_8 with Import, Volatile,
           Address => To_Address (Integer_Address (Current.CPU_Address + Offset));
      begin Value := Byte; end;
      Success := Startup_Owner_Ready;
      if not Success then Startup_Access_Fault := True; end if;
   end GuC_Byte;
   function GuC_Now return Unsigned_64 is
     (if Startup_Owner_Ready then DC_Now_Us else Unsigned_64'Last);
   procedure GuC_Pause is
   begin
      System.Machine_Code.Asm ("pause", Volatile => True);
   end GuC_Pause;
   package Native_GuC is new Intel_GPU_GuC_Upload
     (GuC_Read, GuC_Write, GuC_Byte, GuC_Now, GuC_Pause);
   -- Optional supervisor-installed diagnostic recipient. Unlike render
   -- admission, this endpoint can only obtain the completed boot pixels.
   Probe_Recipient_Slot : constant CapabilitySlot := Native_GPU_Probe_Protocol.Driver_Recipient_Slot;
   Probe_Stamp : constant Unsigned_64 := Native_GPU_Probe_Protocol.Viewer_Tag;
   Probe_Identity : Unsigned_64 := 0;
   procedure Capture_Probe_Recipient is
      Endpoint_Kind : constant Unsigned_64 := 1;
      Read_Right : constant Unsigned_64 := 1;
      Data : aliased array (0 .. 5) of Unsigned_64 := [others => 0];
      Result : Unsigned_64;
   begin
      Result := syscall (SYSCALL_INSPECT_CAPABILITY, syscall (SYSCALL_GETPID),
        Unsigned_64 (Probe_Recipient_Slot),
        Unsigned_64 (System.Storage_Elements.To_Integer (Data'Address)));
      if Result = 1 and then Data (0) = Endpoint_Kind and then
        (Data (1) and Read_Right) /= 0 and then
        Data (3) in 1 .. Unsigned_64 (Unsigned_32'Last) and then
        Data (5) in 1 .. Unsigned_64 (Unsigned_32'Last)
      then
         Probe_Identity := Shift_Left (Data (5), 32) or Data (3);
      end if;
   end Capture_Probe_Recipient;
   procedure Probe_Recipient
     (From, Stamp : Unsigned_64; Slot : out CapabilitySlot;
      Identity : out Unsigned_64) is
   begin
      Slot := Probe_Recipient_Slot;
      Identity := 0;
      if Probe_Identity /= 0 and then Stamp = Probe_Stamp and then
        From = (Probe_Identity and 16#FFFF_FFFF#) and then
        CuBit.Capability_Grants.Endpoint_Matches (Slot, Probe_Identity)
      then Identity := Probe_Identity; end if;
   end Probe_Recipient;
   function Probe_Pixels return Intel_GPU_Buffer_Reply.Backing is
     (if Runtime_Fault then (Ready => False) else Completed_Probe_Pixels);
   package Probe_Export is new Intel_GPU_Probe_Export (Probe_Pixels, Probe_Recipient);
   Probe_State : Probe_Export.Export_State;
   procedure Handle_Probe (From : ProcessID; Msg : Message) is
      Response : Native_GPU_Probe_Protocol.Words;
      Reply_Message : Message := NULL_MESSAGE;
      Delivery : Unsigned_64;
   begin
      Probe_Export.Handle (Probe_State, Unsigned_64 (From), Msg.authorityTag,
        Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
        Native_GPU_Probe_Protocol.Words (Msg.words), Response);
      Reply_Message.tag := (Native_GPU_Probe_Protocol.Label, 4, 0, 0);
      Reply_Message.words := MessageWords (Response);
      Delivery := reply (From, Reply_Message);
      if Delivery /= 1 and then Response (0) = 0 then
         Probe_Export.Reject_Delivery (Probe_State);
      end if;
      -- Read requests are one-shot diagnostics. Keep their result in the
      -- collector, not only serial stdout: the physical testbench may have
      -- neither a serial cable nor a functioning viewer. Do not log repeated
      -- retirement polling, which could crowd out the original failure.
      if Msg.words (1) = 0 then
         Publish_Snapshot ("intel-gpu: viewer target request status=" &
           Unsigned_64'Image (Response (0)) & " delivered=" &
           Boolean'Image (Delivery = 1));
         Publish_Snapshot ("intel-gpu: viewer target backing=" &
           Boolean'Image (Completed_Probe_Pixels.Ready) & " runtime-fault=" &
           Boolean'Image (Runtime_Fault) & " recipient=" &
           Boolean'Image (Probe_Identity /= 0));
      end if;
   end Handle_Probe;
   function Runtime_Owner_Base return Boolean is
      Current : constant Intel_GPU_Firmware_Buffer.Prepared_CT_Buffer :=
        Intel_GPU_Firmware_Buffer.Prepared_CT;
   begin
      return Runtime_Admitted and then not Runtime_Fault and then
        not Startup_Access_Fault and then CT_Mapped and then
        Firmware_Mapped and then ADS_Mapped and then Log_Mapped and then
        Publication_Owner_Ready and then CT_Backing_View.Ready and then
        Current.Ready and then Current.DMA_Address = CT_Backing_View.DMA_Address and then
        Current.CPU_Address = CT_Backing_View.CPU_Address and then
        Current.Region_Bytes = CT_Backing_View.Region_Bytes and then
        Intel_GPU_ADS_Buffer.Initialized_GPU_Start = ADS_GPU_Start;
   end Runtime_Owner_Base;
   package Runtime_Status_IO is new Intel_GPU_Native_GuC_IO (Runtime_Owner_Base);
   function Runtime_Owner_Ready return Boolean is
      use type Intel_GPU_GuC_Status.State;
   begin
      if not Runtime_Owner_Base then return False; end if;
      if Intel_GPU_GuC_Status.Decode (Runtime_Status_IO.Read32 (16#C000#)) /=
        Intel_GPU_GuC_Status.Ready
      then Runtime_Fault := True; return False; end if;
      return Runtime_Owner_Base;
   end Runtime_Owner_Ready;
   package Runtime_Mailbox is new Intel_GPU_Native_GuC_Mailbox (Runtime_Owner_Ready);
   function Runtime_Now return Unsigned_64 is
     (if Runtime_Owner_Ready then DC_Now_Us else Unsigned_64'Last);
   package Runtime_MMIO is new Intel_GPU_GuC_MMIO
     (Runtime_Owner_Ready, Runtime_Mailbox.Read_Word, Runtime_Mailbox.Write_Word,
      Runtime_Mailbox.Notify, Runtime_Now, GuC_Pause);
   Runtime_Channel : Runtime_MMIO.Channel;
   Runtime_Last_Result : Runtime_MMIO.Result := Runtime_MMIO.Rejected;
   procedure Runtime_Exchange (Request : Intel_GPU_GuC_CT_Setup.Request;
                              Reply : out Unsigned_32; Success : out Boolean) is
      use type Runtime_MMIO.Result;
   begin
      Runtime_MMIO.Exchange (Runtime_Channel, Request, 1_000_000,
                             Reply, Runtime_Last_Result);
      Success := Runtime_Last_Result = Runtime_MMIO.Complete;
      if not Success then Runtime_Fault := True; end if;
   end Runtime_Exchange;
   package Native_CT is new Intel_GPU_GuC_CT_Register
     (Runtime_Owner_Ready, Runtime_Exchange);
   CT_Registration : Native_CT.Registration;
   RCS_Start_Active, RCS_Ready : Boolean := False;
   function RCS_Status_GPU return Unsigned_64 is (Engine_Status_GPU_Start);
   function RCS_Page_Ready (GPU : Unsigned_64) return Boolean is
     (Engine_Status_Mapped and then GPU /= 0 and then GPU = Engine_Status_GPU_Start and then
      Submission_Mapped and then Submission_GPU_Start /= 0 and then
      Submission_Buffers.Initialized_GPU_Start (Submission_State) = Submission_GPU_Start);
   function RCS_Start_Owner return Boolean is
     (RCS_Start_Active and then Publication_Owner_Ready and then
      Native_CT.Enabled (CT_Registration) and then RCS_Page_Ready (Engine_Status_GPU_Start) and then
      Runtime_Owner_Ready);
   package RCS_IO is new Intel_GPU_Native_RCS_Start (RCS_Start_Owner, RCS_Status_GPU);
   package RCS_Startup is new Intel_GPU_RCS_Start
     (RCS_Start_Owner, RCS_Page_Ready, RCS_IO.Write32, RCS_IO.Read32);
   RCS_Attempt : RCS_Startup.Attempt;
   CT_CPU_Base : constant Unsigned_64 :=
     16#6100_0000# + Intel_GPU_Firmware_Buffer.CT_Region_Offset;
   function CT_Receive_Owner_Ready return Boolean is
     (CT_Backing_View.Ready and then CT_Backing_View.CPU_Address = CT_CPU_Base and then
      CT_Backing_View.Region_Bytes = 32 * 1024 and then
      Native_CT.Enabled (CT_Registration));
   package CT_Receive_IO is new Intel_GPU_Native_CT_Receive
     (CT_CPU_Base, CT_Receive_Owner_Ready);
   package CT_Receive is new Intel_GPU_GuC_CT_Receive
     (CT_Receive_IO.Read_Descriptor, CT_Receive_IO.Read_Word,
      CT_Receive_IO.Finish_Reads, CT_Receive_IO.Write_Head, CT_Receive_IO.Make_Visible);
   Receive_Channel : CT_Receive.Channel;
   -- Retain owned copies until an HXG dispatcher is implemented; never treat
   -- arbitrary firmware payloads as callbacks, addresses or completed work.
   Initial_CT_Messages : array (Positive range 1 .. 8) of CT_Receive.Message;
   Initial_CT_Count : Natural range 0 .. 8 := 0;
   package CT_Send_IO is new Intel_GPU_Native_CT_Send
     (CT_CPU_Base, CT_Receive_Owner_Ready);
   package CT_Send is new Intel_GPU_GuC_CT_Send
     (CT_Send_IO.Read_Descriptor, CT_Send_IO.Write_Word, CT_Send_IO.Make_Visible,
      CT_Send_IO.Write_Tail, Runtime_Mailbox.Notify);
   Send_Channel : CT_Send.Channel;
   CT_Events : array (Positive range 1 .. 8) of CT_Receive.Message;
   CT_Event_Count : Natural range 0 .. 8 := 0;
   -- Context controls share the transport's diagnostic FAST_REQUEST ID stream.

   procedure Queue_Log_Control (Fence : Unsigned_16; Success : out Boolean) is
      Send_Status : CT_Send.Result;
      use type CT_Send.Result;
   begin
      -- One outstanding request, one single-word response (two CT DWORDs).
      -- The 4096-word G2H ring is exclusively serviced by this startup path;
      -- unsolicited events are bounded and retained separately below.
      -- Linux guc_action_control_log: 0x40, control=0 disables debug logging.
      -- This is a supported CT operation, not a made-up ping or engine job.
      CT_Send.Send (Send_Channel, [16#40#, 0], Fence, Send_Status);
      Success := Send_Status = CT_Send.Queued;
   end Queue_Log_Control;
   procedure Poll_CT_Reply
     (Output : out CT_Receive.Message; Status : out CT_Receive.Result) is
   begin
      CT_Receive.Poll (Receive_Channel, Output, Status);
   end Poll_CT_Reply;
   procedure Retain_CT_Event (Item : CT_Receive.Message; Success : out Boolean) is
   begin
      Success := False;
      if CT_Event_Count = CT_Events'Length then return; end if;
      CT_Event_Count := CT_Event_Count + 1;
      CT_Events (CT_Event_Count) := Item;
      Success := True;
   end Retain_CT_Event;
   package CT_Roundtrip is new Intel_GPU_GuC_CT_Roundtrip
     (CT_Receive, CT_Receive_Owner_Ready, Queue_Log_Control, Poll_CT_Reply,
      Retain_CT_Event, Runtime_Now, GuC_Pause);
   CT_Probe_Attempt : CT_Roundtrip.Attempt;
   package Context_Life renames Intel_GPU_GuC_Context_Lifecycle;
   use type Context_Life.Phase;
   package Context_Event renames Intel_GPU_GuC_Context_Event;
   package Fast_Fences renames Intel_GPU_GuC_Fast_Fences;
   Fast_Stream : Fast_Fences.Stream;
   function Context_Owner return Boolean is
     (not Fast_Fences.Failed (Fast_Stream) and then
      RCS_Ready and then RCS_Page_Ready (Engine_Status_GPU_Start) and then
      CT_Receive_Owner_Ready and then Runtime_Owner_Ready);
   package Context_Input is new Intel_GPU_Native_Context_Read
     (Context_Owner, Intel_GPU_Native_Reset.ADS_Topology);
   Context_Init : Intel_GPU_ADLN_Context_Init.Segment;
   procedure Queue_Context
     (Payload : Context_Event.Words;
      Result : out Context_Life.Send_Result) is
      Status : CT_Send.Result;
      Wire_Fence : Unsigned_16;
      Accepted : Boolean;
      use type CT_Send.Result;
   begin
      Result := Context_Life.Uncertain;
      -- Context admission is checked by the session/table before this call.
      -- Wire IDs are transport diagnostics, not context authority. The boot
      -- synchronous probe uses low-half fence 42.
      if not Context_Owner then return; end if;
      Fast_Fences.Prepare (Fast_Stream, Wire_Fence, Accepted);
      if not Accepted then return; end if;
      CT_Send.Send (Send_Channel, CT_Send.Words (Payload), Wire_Fence, Status);
      Fast_Fences.Sent (Fast_Stream,
        (if Status = CT_Send.Queued then Fast_Fences.Published
         elsif Status = CT_Send.Would_Block then Fast_Fences.Not_Published
         else Fast_Fences.Uncertain));
      if not Context_Owner then Fast_Fences.Fail (Fast_Stream); return; end if;
      Result := (if Status = CT_Send.Queued then Context_Life.Queued
                 elsif Status = CT_Send.Would_Block then Context_Life.Backpressure
                 else Context_Life.Uncertain);
   end Queue_Context;
   procedure Retain_Context_Event
     (Payload : Context_Event.Words; Fence : Unsigned_16; Success : out Boolean) is
      Item : CT_Receive.Message;
   begin
      Success := False;
      if Payload'Length not in 1 .. 255 then return; end if;
      Item.Length := Payload'Length; Item.Fence := Fence;
      for Index in 1 .. Item.Length loop
         Item.Payload (Index) := Payload (Payload'First + Index - 1);
      end loop;
      Retain_CT_Event (Item, Success);
   end Retain_Context_Event;
   package Context_Driver is new Intel_GPU_GuC_Context_Session
     (Context_Owner, Queue_Context, Retain_Context_Event);
   First_Context_ID : constant Unsigned_32 := 7;
   package Context_Pool is new Intel_GPU_Context_Table
     (16, Context_Driver, Context_Owner, Retain_Context_Event,
      First_ID => First_Context_ID);
   Contexts : Context_Pool.Table;
   package Context_Drain is new Context_Pool.Draining
     (Runtime_Now, Application_Work_Drained);
   Drain_State : Context_Drain.Drain_State;
   Render_Context_ID : Unsigned_32 := Context_Pool.No_Context;
   procedure Retire_Application_Context (Session : Unsigned_64) is
      Retained_ID : Unsigned_32;
   begin
      -- Close context lookup/new work before retiring application handles.
      -- No reclamation: retirement is not a hardware-disable acknowledgement.
      -- The service-loop drain observes retained retirement flags; no blocking
      -- disable wait or backing release occurs in the IPC handler.
      Context_Pool.Retire_Session (Contexts, Session, Retained_ID);
   end Retire_Application_Context;
   procedure Dispatch_Context_Event
     (Payload : Context_Event.Words; Fence : Unsigned_16;
      Status : out Context_Driver.Result) is
      ID : Unsigned_32;
      Delivery : Context_Pool.Dispatch_Result;
   begin
      Context_Pool.Dispatch (Contexts, Payload, Fence, ID, Delivery);
      Status := (case Delivery is
        when Context_Pool.Delivered => Context_Driver.Handled,
        when Context_Pool.Retained => Context_Driver.Retained,
        when Context_Pool.Context_Fault =>
          (if ID = Render_Context_ID then Context_Driver.Faulted else Context_Driver.Handled),
        when Context_Pool.Transport_Fault => Context_Driver.Faulted);
   end Dispatch_Context_Event;
   package Context_Wait is new Context_Pool.Waiting
     (CT_Receive, Poll_CT_Reply, Runtime_Now, GuC_Pause);
   Context_Started : Boolean := False;
   function Initial_Backing_Owner return Boolean is
     (Context_Owner and then Submission_GPU_Start /= 0 and then
      Submission_Buffers.Initialized_GPU_Start (Submission_State) = Submission_GPU_Start and then
      Submission_Allocation.Ready and then
      Submission_Allocation.CPU_Address = Submission_CPU and then
      Submission_Allocation.Bytes = Submission_Bytes);
   function Initial_Exclusive return Boolean is
     (Context_Pool.State (Contexts, Render_Context_ID) = Context_Life.Fresh);
   package Initial_Ring is new Intel_GPU_Native_Initial_Ring
     (Submission_CPU, Submission_Bytes, Initial_Backing_Owner, Initial_Exclusive);
   function Live_Backing_Owner return Boolean is
     (Initial_Backing_Owner and then
      Context_Pool.Work_Allowed (Contexts, Render_Context_ID));
   function Live_Coherent_Ready return Boolean is
     (PCI_Device = 16#46D2# and then Live_Backing_Owner);
   -- ADL-N LLC system-memory/WB contract only, not a cross-platform default.
   function Live_CPU_Base return Unsigned_64 is (Submission_CPU);
   function Live_Bytes return Unsigned_64 is (81920);
   package Live_Ring is new Intel_GPU_Native_Live_Ring
     (Live_CPU_Base, Live_Bytes, Live_Backing_Owner, Live_Coherent_Ready,
      Initial_Ring.Read_Marker);
   Live_Channel : Live_Ring.Channel;
   function Service_Initial_Events return Boolean is
      Item : CT_Receive.Message;
      Received : CT_Receive.Result;
      Status : Context_Driver.Result;
      use type CT_Receive.Result;
      use type Context_Driver.Result;
   begin
      if not Context_Owner then return False; end if;
      for Index in 1 .. 8 loop
         Poll_CT_Reply (Item, Received);
         if Received = CT_Receive.Empty then return Context_Owner; end if;
         if Received /= CT_Receive.Received or else Item.Length = 0 then
            Context_Pool.Fail (Contexts); return False;
         end if;
         Dispatch_Context_Event (
           Context_Event.Words (Item.Payload (1 .. Item.Length)), Item.Fence, Status);
         if Status in Context_Driver.Faulted | Context_Driver.Rejected then return False; end if;
      end loop;
      return Context_Owner;
   end Service_Initial_Events;
   package Initial_Completion is new Intel_GPU_Initial_Completion
     (Initial_Backing_Owner, Initial_Ring.Read_Marker, Service_Initial_Events,
      Runtime_Now, GuC_Pause);
   Initial_Attempt : Initial_Completion.Attempt;
   function Render_Backend_Ready return Boolean is
      use type Initial_Completion.Phase;
   begin
      -- Require more than a boot marker: acknowledged disable, retained
      -- backing, exclusive GGTT bookkeeping and current GuC/engine ownership.
      -- Allocation still checks capacity; this observation reserves nothing.
      return not Runtime_Fault and then not Context_Pool.Failed (Contexts)
        and then Private_Pending = 0 and then Application_Pending = 0
        and then Update_Pending = 0 and then not In_Place_Active and then not Activation_Reply_Pending
        and then not Buffer_Memory.Pending (Buffer_Pool)
        and then Retirement_Scratch.Ready and then Submission_Allocation.Ready
        and then GGTT_Takeover_Held and then Address_Layout.Valid
        and then Intel_GPU_GGTT_Reservations.Table_Size (Runtime_Ledger) = Table_Bytes
        and then Table_Bytes /= 0
        and then Initial_Completion.State (Initial_Attempt) = Initial_Completion.Observed
        and then Render_Context_ID /= Context_Pool.No_Context
        and then Context_Pool.State (Contexts, Render_Context_ID) = Context_Life.Disabled
        and then Context_Owner;
   end Render_Backend_Ready;
   function Request_Authorization (Label : Unsigned_32) return Boolean is
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Token : constant Unsigned_64 := 16#4947_1000# + Unsigned_64 (Label);
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Now : Unsigned_64;
      Previous : Unsigned_64 := Started;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      if Label not in 16#022E# | 16#022F# |
        Intel_GPU_PCI_Interrupts.Disable_Request_Label
      then return False; end if;
      if Label = Intel_GPU_PCI_Interrupts.Disable_Request_Label then
         if PCI_IRQ_Attempted then return False; end if;
         PCI_IRQ_Attempted := True;
         if not Display_Power_Owned or else not Reset_Pages_Mapped then
            return False;
         end if;
      end if;
      if Label = 16#022E# and then not Reset_Pages_Mapped then return False; end if;
      if Started = Unsigned_64'Last then return False; end if;
      Msg.tag := (Label, 0, 0, 0);
      if not capSubmit (15, Msg, Token) then return False; end if;
      for Poll in 1 .. 30_000 loop
         Now := syscall (SYSCALL_GETTIME);
         if Now = Unsigned_64'Last or else Now < Previous or else
           Now - Started >= 30_000
         then return False; end if;
         Previous := Now;
         if Intel_GPU_Diagnostics.Poll_Driver (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               if not capSubmit (15, Msg, Token) then return False; end if;
            else
               return Receipt.status = COMPLETION_OK and then
                 Receipt.msg.tag = (16#F000#, 0, 0, 0) and then
                 Receipt.msg.words = [0, 0, 0, 0];
            end if;
         end if;
         Activity := Wait_For_Activity_Until
           (if Now < Unsigned_64'Last then Now + 1 else Now);
      end loop;
      return False;
   end Request_Authorization;
   function Map_Reset_Pages return String is
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Now : Unsigned_64;
      Granted : Boolean;
      Activity : Activity_Result;
      Clock : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
      pragma Unreferenced (Activity);
   begin
      if Reset_Pages_Mapped then return "already-mapped (NOT reset)"; end if;
      if not Engine_Inventory.Valid then return "inventory-unavailable"; end if;
      if not Clock.Available then return "clock-unavailable"; end if;
      for Index in Intel_GPU_Reset_Pages.Page_Index loop
         declare
            Token : constant Unsigned_64 := 16#4947_0010# + Unsigned_64 (Index);
            Offset : constant Unsigned_64 := Intel_GPU_Reset_Pages.Offset (Index);
         begin
            Msg.tag := (16#022D#, 1, 0, 0);
            Msg.words := [Unsigned_64 (Index), 0, 0, 0];
            if not capSubmit (15, Msg, Token) then return "submit-failed"; end if;
            Granted := False;
            loop
               Now := syscall (SYSCALL_GETTIME);
               if Now < Started or else Now - Started >= 30_000 then return "grant-timeout"; end if;
               if Intel_GPU_Diagnostics.Poll_Driver (Receipt'Address) /= 0 and then Receipt.token = Token then
                  if Receipt.status = COMPLETION_OK and then
                    Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
                    Receipt.msg.words = [0, 0, 0, 0]
                  then
                     if not capSubmit (15, Msg, Token) then return "retry-failed"; end if;
                  else
                     Granted := Receipt.status = COMPLETION_OK and then
                       Receipt.msg.tag = (16#F000#, 0, 0, 0) and then
                       Receipt.msg.words = [0, 0, 0, 0];
                     exit;
                  end if;
               end if;
               Activity := Wait_For_Activity_Until
                 (if Now < Unsigned_64'Last then Now + 1 else Now);
            end loop;
            if not Granted then return "grant-denied"; end if;
            if Offset > Unsigned_64'Last - Plan.Physical_Base then return "address-overflow"; end if;
            if syscall (SYSCALL_MAP_DEVICE, Plan.Physical_Base + Offset,
                        Reset_Write_Base + Unsigned_64 (Index) * 4096, 1, 0) /= 0
            then return "map-denied"; end if;
         end;
      end loop;
      Reset_Pages_Mapped := True;
      return "ready (NOT reset)";
   end Map_Reset_Pages;
   procedure Publish_Snapshot (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information) is
   begin
      Intel_GPU_Diagnostics.Capture (Text, Level);
      Intel_GPU_Diagnostics.Tick;
   end Publish_Snapshot;
   function Hex (Value : Unsigned_32) return String;
   -- One-shot offline-bind -> prepared context transition. Admission remains
   -- disabled at Handle_Render_Control until the complete backend is usable.
   Prepare_Context_Label : constant Unsigned_32 := 16#0A25#;
   Preparing_Index : Natural range 0 .. Intel_GPU_Render_Sessions.Capacity := 0;
   Preparing_Identity, Preparing_Session : Unsigned_64 := 0;
   function Application_Image_Owner return Boolean is
     (Preparing_Index /= 0 and then not Runtime_Fault and then Publication_Owner_Ready and then
      Application_Lifetime.Backing_Usable (Private_Contexts (Preparing_Index).Life) and then
      Application_Session (Preparing_Identity and 16#FFFF_FFFF#, Preparing_Session) =
        Preparing_Session and then
      Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, Preparing_Identity and 16#FFFF_FFFF#, Preparing_Session) =
          Preparing_Identity);
   function Flush_Application_Page (CPU : Unsigned_64) return Boolean is
     (Application_Image_Owner and then Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096)
      and then Application_Image_Owner);
   package Application_Images is new Intel_GPU_Application_Image
     (Application_VM, Application_Image_Owner, Flush_Application_Page);
   Application_Images_State : array (Private_Contexts'Range) of Application_Images.State;
   function Application_PTE_Allowed (Index, Value : Unsigned_64) return Boolean is
      First : Unsigned_64;
   begin
      if not Application_Image_Owner then return False; end if;
      First := Application_Images.GPU_Start (Application_Images_State (Preparing_Index));
      return First /= 0 and then Runtime_Range_Allowed (First, Intel_GPU_Submission_Image.GGTT_Bytes)
        and then Index >= First / 4096 and then
        Index - First / 4096 < Intel_GPU_Submission_Image.GGTT_Bytes / 4096 and then
        Value = Intel_GPU_GGTT.Encode_System_Page
          (Intel_GPU_Buffer_Reply.Page_Address
             (Private_Contexts (Preparing_Index).Context,
              (Index - First / 4096) * 4096));
   end Application_PTE_Allowed;
   package Application_GGTT_IO is new Intel_GPU_Native_GGTT
     (Application_Image_Owner, Intel_GPU_GGTT_Mapping.Bytes, Application_PTE_Allowed);
   package Application_Publication is new Application_Images.Publication
     (Runtime_Range_Allowed, Application_GGTT_IO.Read_PTE,
      Application_GGTT_IO.Write_PTE, Invalidate_Upload);
   procedure Handle_Context_Preparation (From : ProcessID; Msg : Message) is
      Session : constant Unsigned_64 := Application_Session (Unsigned_64 (From), Msg.authorityTag);
      Identity : constant Unsigned_64 := Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, Unsigned_64 (From), Msg.authorityTag);
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
      Code : Unsigned_64 := Application_Buffers.Denied;
      Prepared : Boolean := False;
      Scratch : Application_Images.Tables.Scratch_Mappings;
      Status : Application_Publication.Result;
      use type Application_Publication.Result;
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      if Update_Pending /= 0 or else In_Place_Active or else Table_Ledger_Busy then
         Code := Application_Buffers.Unavailable;
      elsif Stored /= 0 then
         Code := Application_Buffers.Bad_Request;
         if Msg.tag.length = 4 and then Msg.tag.flags = 0 and then Msg.tag.reserved = 0 and then
           Msg.words (0) = 1 and then Msg.words (1) = 0 and then
           Msg.words (2) = 0 and then Msg.words (3) = 0
         then
            Code := Application_Buffers.Unavailable;
            Preparing_Index := Stored;
            Preparing_Identity := Identity; Preparing_Session := Session;
            if Application_Image_Owner and then
              Private_Contexts (Preparing_Index).Life = Application_Lifetime.Offline then
               Private_Contexts (Preparing_Index).Life := Application_Lifetime.Begin_Preparation
                 (Private_Contexts (Preparing_Index).Life);
               Application_VM.Seal (Private_Contexts (Preparing_Index).Source, Prepared);
               if Prepared then
                 declare
                  Generation : constant Unsigned_64 := Private_Contexts (Stored).Table_Generation;
                  function Held return Boolean is
                    (Application_Image_Owner and then Preparing_Index = Stored
                     and then Preparing_Session = Session and then Preparing_Identity = Identity
                     and then Private_Contexts (Stored).Table_Generation = Generation);
                  function Table_Page (Ordinal : Positive) return Application_Images.Tables.Page_Mapping is
                     M : Intel_GPU_Table_Provenance.Mapping;
                  begin
                     if not Held then return (0, 0); end if;
                     M := Initial_Table_Mapping (Stored, Session, Ordinal);
                     if not Held or else M.Ticket = 0 then return (0, 0); end if;
                     return (M.CPU, M.DMA);
                  end Table_Page;
                  procedure Publish_Tables is new Application_Publication.Publish_From_Mappings (Table_Page);
                 begin
                  for L in Scratch'Range loop
                     Scratch (L) :=
                       (Private_Contexts (Preparing_Index).Scratch.CPU_Address + Unsigned_64 (L) * 4096,
                        Intel_GPU_Buffer_Reply.Page_Address
                          (Private_Contexts (Preparing_Index).Scratch, Unsigned_64 (L) * 4096));
                  end loop;
                  if Prepared then Publish_Tables
                    (Application_Images_State (Preparing_Index),
                     Private_Contexts (Preparing_Index).Source,
                     Application_VM.Used (Private_Contexts (Preparing_Index).Source),
                     Private_Contexts (Preparing_Index).Context, Runtime_Ledger, Status, Scratch);
                  Prepared := Status = Application_Publication.Published;
                  end if;
                 end;
               end if;
               -- No later offline binds or second publication attempt. The
               -- retained source is sealed; future live binds need VM_Update.
               Private_Contexts (Preparing_Index).Life := Application_Lifetime.Finish_Preparation
                 (Private_Contexts (Preparing_Index).Life, Prepared);
               if Prepared then Code := Application_Buffers.OK;
               else
                  Intel_GPU_Render_Control.Reject_Delivery (Render_Admission, Identity, Session);
                  Retire_Application_Resources (Session);
               end if;
            end if;
            Preparing_Index := 0; Preparing_Identity := 0; Preparing_Session := 0;
         end if;
      end if;
      Response.tag := (Prepare_Context_Label, 4, 0, 0);
      Response.words := [Code, 1, 0, 0];
      Delivered := reply (From, Response);
      if Delivered /= 1 and then Prepared then
         Intel_GPU_Render_Control.Reject_Delivery (Render_Admission, Identity, Session);
         Retire_Application_Resources (Session);
      end if;
   end Handle_Context_Preparation;
   Register_Context_Label : constant Unsigned_32 := 16#0A26#;
   Application_Registration_Attempted : array (Private_Contexts'Range) of Boolean := [others => False];
   Application_Setup_Complete : array (Private_Contexts'Range) of Boolean := [others => False];
   procedure Handle_Context_Registration (From : ProcessID; Msg : Message) is
      Session : constant Unsigned_64 := Application_Session (Unsigned_64 (From), Msg.authorityTag);
      Identity : constant Unsigned_64 := Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, Unsigned_64 (From), Msg.authorityTag);
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
      Code : Unsigned_64 := Application_Buffers.Denied;
      ID : Unsigned_32 := Context_Pool.No_Context;
      Accepted : Boolean := False;
      Status : Context_Driver.Result;
      use type Context_Driver.Result;
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      if Update_Pending /= 0 or else In_Place_Active then
         Code := Application_Buffers.Unavailable;
      elsif Stored /= 0 then
         Code := Application_Buffers.Bad_Request;
         if Msg.tag.length = 4 and then Msg.tag.flags = 0 and then Msg.tag.reserved = 0 and then
           Msg.words (0) = 1 and then Msg.words (1) = 0 and then
           Msg.words (2) = 0 and then Msg.words (3) = 0
         then
            Code := Application_Buffers.Unavailable;
            -- Generic actuals are evaluated during elaboration, before
            -- Ring_Owner can reject a missing allocation.
            if Private_Contexts (Positive (Stored)).Context.Ready then
               declare
                  Index : constant Positive := Positive (Stored);
                  GPU : constant Unsigned_64 :=
                    Application_Publication.GPU_Address (Application_Images_State (Index));
                  function Ring_Owner return Boolean is
                    (not Runtime_Fault and then Context_Owner and then
                     Application_Session (Unsigned_64 (From), Msg.authorityTag) = Session and then
                     Intel_GPU_Render_Control.Recipient_Identity
                       (Render_Admission, Unsigned_64 (From), Msg.authorityTag) = Identity and then
                     Private_Contexts (Index).Context.Ready and then GPU /= 0 and then
                     Application_Publication.GPU_Address (Application_Images_State (Index)) = GPU);
                  function Never_Registered return Boolean is
                    (Context_Pool.Session_Context (Contexts, Session) = Context_Pool.No_Context);
                  -- Instance is used once, under the retained per-session attempt
                  -- flag. No caller-supplied CPU address or ring contents.
                  package App_Initial_Ring is new Intel_GPU_Native_Initial_Ring
                    (Private_Contexts (Index).Context.CPU_Address,
                     Private_Contexts (Index).Context.Bytes, Ring_Owner, Never_Registered);
                  function Setup_Owner return Boolean is
                    (Ring_Owner and then
                     (ID = Context_Pool.No_Context or else
                      Context_Pool.State (Contexts, ID) /= Context_Life.Quarantined));
                  package App_Completion is new Intel_GPU_Initial_Completion
                    (Setup_Owner, App_Initial_Ring.Read_Marker, Service_Initial_Events,
                     Runtime_Now, GuC_Pause);
                  Completion_Attempt : App_Completion.Attempt;
                  Completion_Status : App_Completion.Result;
                  Wait_Status : Context_Wait.Result;
                  use type App_Completion.Result;
                  use type Context_Wait.Result;
                  Setup : Intel_GPU_ADLN_Context_Init.Segment;
                  WM : Unsigned_32;
               begin
                  if GPU /= 0 and then not Application_Registration_Attempted (Index) and then
                    not Runtime_Fault and then Context_Owner
                  then
                     Application_Registration_Attempted (Index) := True;
                     WM := Context_Input.Read_WM_Chicken2;
                     Setup := Intel_GPU_ADLN_Context_Init.Build_Setup
                       (Ring_Owner and then WM /= Unsigned_32'Last, WM);
                     -- Arm against the untouched marker before ANY GPU publication.
                     App_Completion.Arm (Completion_Attempt, Completion_Status);
                     if Completion_Status = App_Completion.Ready then
                        App_Initial_Ring.Publish (Setup, Accepted);
                     end if;
                     if Accepted then
                        Context_Pool.Open
                          (Contexts, GPU, Address_Layout.Runtime_First, 1000, 500_000, True,
                           ID, Accepted, Session => Session);
                     end if;
                     if Accepted then
                        for Action in Context_Life.Register_Context .. Context_Life.Set_Policy loop
                           Context_Pool.Submit (Contexts, ID, Action, Status);
                           if Status /= Context_Driver.Queued then Accepted := False; exit; end if;
                        end loop;
                     end if;
                     if Accepted then
                        Context_Wait.Execute
                          (Contexts, ID, Context_Life.Enable, 1_000_000, Wait_Status);
                        Accepted := Wait_Status = Context_Wait.Complete;
                     end if;
                     if Accepted then
                        App_Completion.Wait
                          (Completion_Attempt, 1_000_000, Completion_Status);
                        Accepted := Completion_Status = App_Completion.Complete;
                     end if;
                     if Accepted then
                        Context_Wait.Execute
                          (Contexts, ID, Context_Life.Disable, 1_000_000, Wait_Status);
                        Accepted := Wait_Status = Context_Wait.Complete;
                     end if;
                     -- Success means the driver-owned setup marker was observed
                     -- and scheduling disable acknowledged. No application batch
                     -- is present, and backing remains retained on every path.
                     if Accepted and then Setup_Owner and then
                       Context_Pool.State (Contexts, ID) = Context_Life.Disabled
                     then
                        Application_Setup_Complete (Index) := True;
                        Code := Application_Buffers.OK;
                     else
                        Intel_GPU_Render_Control.Reject_Delivery (Render_Admission, Identity, Session);
                        Retire_Application_Resources (Session);
                     end if;
                  end if;
               end;
            end if;
         end if;
      end if;
      Response.tag := (Register_Context_Label, 4, 0, 0);
      Response.words := [Code, 1, 0, 0];
      Delivered := reply (From, Response);
      if Delivered /= 1 and then Code = Application_Buffers.OK then
         Intel_GPU_Render_Control.Reject_Delivery (Render_Admission, Identity, Session);
         Retire_Application_Resources (Session);
      end if;
   end Handle_Context_Registration;
   -- Selection belongs to the serialized service loop, never to request-supplied
   -- context IDs or CPU/DMA addresses. Per-session channels retain their tails.
   -- Public render admission remains CLOSED: wiring this transport is not proof
   -- of command isolation, bounded hostile-work recovery or safe address reuse.
   Submit_Label : constant Unsigned_32 := 16#0A27#;
   Selected_Index : Natural := 0;
   Selected_Session, Selected_Identity, Selected_Sender, Selected_Stamp : Unsigned_64 := 0;
   Selected_CPU, Selected_Bytes, Selected_DMA, Selected_GPU : Unsigned_64 := 0;
   Selected_Context : Unsigned_32 := Context_Pool.No_Context;
   function Submission_Owner return Boolean is
     (Selected_Index in Private_Contexts'Range and then
      Buffer_Retirement_Pending = 0 and then
      not Application_Maps.Presentation_Held (Application_Map_State, Selected_Session) and then
      not Application_Buffers.Image_Writes_Held (Application_Buffer_State, Selected_Session) and then
      not Runtime_Fault and then Context_Owner and then PCI_Device = 16#46D2# and then
      Application_Setup_Complete (Selected_Index) and then
      Application_Session (Selected_Sender, Selected_Stamp) = Selected_Session and then
      Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, Selected_Sender, Selected_Stamp) = Selected_Identity and then
      Selected_Context /= Context_Pool.No_Context and then
      Context_Pool.Session_Context (Contexts, Selected_Session) = Selected_Context and then
      Context_Pool.State (Contexts, Selected_Context) /= Context_Life.Quarantined and then
      Private_Contexts (Selected_Index).Life = Application_Lifetime.Published and then
      Private_Contexts (Selected_Index).Context.Ready and then
      Private_Contexts (Selected_Index).Context.CPU_Address = Selected_CPU and then
      Private_Contexts (Selected_Index).Context.Bytes = Selected_Bytes and then
      Intel_GPU_Buffer_Reply.Page_Address (Private_Contexts (Selected_Index).Context, 0) = Selected_DMA and then
      Selected_GPU /= 0 and then Application_Publication.GPU_Address
        (Application_Images_State (Selected_Index)) = Selected_GPU);
   function Submission_Work_Owner return Boolean is
     (Update_Pending = 0 and then not In_Place_Active and then Submission_Owner and then
      Context_Pool.Work_Allowed (Contexts, Selected_Context));
   function Selected_CPU_Base return Unsigned_64 is (Selected_CPU);
   function Selected_Backing_Bytes return Unsigned_64 is (Selected_Bytes);
   procedure Read_Submission_Marker (Value : out Unsigned_64; OK : out Boolean) is
      function Never_Publish return Boolean is (False);
      package Reader is new Intel_GPU_Native_Initial_Ring
        (Selected_CPU, Selected_Bytes, Submission_Owner, Never_Publish);
   begin
      Reader.Read_Marker (Value, OK);
   end Read_Submission_Marker;
   package Application_Ring is new Intel_GPU_Native_Live_Ring
     (Selected_CPU_Base, Selected_Backing_Bytes, Submission_Work_Owner,
      Submission_Work_Owner, Read_Submission_Marker);
   Application_Channels : array (Private_Contexts'Range) of Application_Ring.Channel;
   package Application_Completion is new Intel_GPU_Initial_Completion
     (Submission_Owner, Read_Submission_Marker, Service_Initial_Events,
      Runtime_Now, GuC_Pause);
   function Submission_Batch (Handle, GPU, Offset, Bytes : Unsigned_64) return Boolean is
     (Submission_Owner and then Application_Binding.Batch_Mapped
       (Application_Buffer_State, Private_Contexts (Selected_Index).Source,
        Selected_Session, Selected_Sender, Selected_Stamp, Handle, GPU, Offset, Bytes));
   procedure Arm_Submission
     (Attempt : in out Application_Completion.Attempt;
      Previous, Expected : Unsigned_32; OK : out Boolean) is
      Status : Application_Completion.Result;
      use type Application_Completion.Result;
   begin
      Application_Completion.Arm (Attempt, Status, Previous, Expected);
      OK := Status = Application_Completion.Ready;
   end Arm_Submission;
   procedure Enable_Submission (OK : out Boolean) is
      Status : Context_Wait.Result;
      use type Context_Wait.Result;
   begin
      Context_Wait.Execute (Contexts, Selected_Context, Context_Life.Enable, 1_000_000, Status);
      OK := Status = Context_Wait.Complete;
      if not OK then
         Publish_Snapshot ("intel-gpu: submission enable=" & Context_Wait.Result'Image (Status) &
           " context=" & Unsigned_32'Image (Selected_Context) &
           " state=" & Context_Life.Phase'Image (Context_Pool.State (Contexts, Selected_Context)));
      end if;
   end Enable_Submission;
   procedure Publish_Submission (GPU : Unsigned_64; Sequence : Unsigned_32; OK : out Boolean) is
      WM : Unsigned_32;
      Previous_Tail : Unsigned_32;
      Segment : Intel_GPU_ADLN_Context_Init.Segment;
   begin
      OK := False;
      if not Submission_Work_Owner then return; end if;
      WM := Context_Input.Read_WM_Chicken2;
      Segment := Intel_GPU_ADLN_Context_Init.Build_Batch
        (Submission_Work_Owner and then WM /= Unsigned_32'Last, WM, Sequence, GPU);
      Previous_Tail := Application_Ring.Tail (Application_Channels (Selected_Index));
      Application_Ring.Append (Application_Channels (Selected_Index), Segment, OK);
      if not OK then
         Publish_Snapshot ("intel-gpu: submission ring append failed tail=" &
           Unsigned_32'Image (Application_Ring.Tail (Application_Channels (Selected_Index))) &
           " sequence=" & Unsigned_32'Image
             (Application_Ring.Sequence (Application_Channels (Selected_Index))));
      elsif Application_Ring.Tail (Application_Channels (Selected_Index)) < Previous_Tail then
         Publish_Snapshot ("intel-gpu: submission ring wrapped sequence=" &
           Unsigned_32'Image (Sequence) & " tail=" &
           Unsigned_32'Image (Application_Ring.Tail (Application_Channels (Selected_Index))));
      end if;
   end Publish_Submission;
   procedure Notify_Submission (OK : out Boolean) is
      Status : Context_Driver.Result;
      use type Context_Driver.Result;
   begin
      Context_Pool.Notify_Work (Contexts, Selected_Context, True, Status);
      OK := Status = Context_Driver.Queued;
      if not OK then
         Publish_Snapshot ("intel-gpu: submission notify=" & Context_Driver.Result'Image (Status) &
           " context=" & Unsigned_32'Image (Selected_Context) &
           " state=" & Context_Life.Phase'Image (Context_Pool.State (Contexts, Selected_Context)));
      end if;
   end Notify_Submission;
   procedure Wait_Submission (Attempt : in out Application_Completion.Attempt; OK : out Boolean) is
      Status : Application_Completion.Result;
      use type Application_Completion.Result;
   begin
      Application_Completion.Wait (Attempt, 1_000_000, Status);
      OK := Status = Application_Completion.Complete;
   end Wait_Submission;
   procedure Disable_Submission (OK : out Boolean) is
      Status : Context_Wait.Result;
      use type Context_Wait.Result;
   begin
      Context_Wait.Execute (Contexts, Selected_Context, Context_Life.Disable, 1_000_000, Status);
      OK := Status = Context_Wait.Complete;
   end Disable_Submission;
   procedure Quarantine_Submission is
   begin
      Intel_GPU_Render_Control.Reject_Delivery
        (Render_Admission, Selected_Identity, Selected_Session);
      Retire_Application_Resources (Selected_Session);
   end Quarantine_Submission;
   package Application_Submission is new Intel_GPU_Application_Submit
     (Submission_Owner, Submission_Batch, Application_Completion.Attempt,
      Arm_Submission, Enable_Submission, Publish_Submission, Notify_Submission,
      Wait_Submission, Disable_Submission, Quarantine_Submission);
   Application_Submissions : array (Private_Contexts'Range) of Application_Submission.State;
   function Application_Work_Drained (Session : Unsigned_64) return Boolean is
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
      use type Application_Submission.Phase;
   begin
      -- Only called between handlers by the serialized service-loop drain.
      -- Setup success includes marker observation and scheduling disable.
      -- Idle includes completion of the last batch AND disable; Failed must
      -- never be mistaken for idle just because the synchronous call returned.
      -- Conservatively block on ANY deferred publisher, even another session.
      if Runtime_Fault or else Buffer_Retirement_Pending /= 0 or else not Context_Owner or else
        Stored = 0 or else
        Selected_Index /= 0 or else Preparing_Index /= 0 or else
        Application_Pending /= 0 or else Private_Pending /= 0 or else Update_Pending /= 0 or else
        In_Place_Active or else Table_Ledger_Busy
      then return False; end if;
      declare Index : constant Positive := Stored; begin
         return Application_Setup_Complete (Index) and then
           Application_Submission.Current (Application_Submissions (Index)) in
             Application_Submission.Uninitialized | Application_Submission.Idle;
      end;
   end Application_Work_Drained;
   procedure Handle_Application_Submission (From : ProcessID; Msg : Message) is
      Session : constant Unsigned_64 := Application_Session (Unsigned_64 (From), Msg.authorityTag);
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
      Code : Unsigned_64 := Application_Buffers.Denied;
      Completion : Unsigned_32 := 0;
      Status : Application_Submission.Result;
      Receipt : Application_Submission.Completion_Receipt;
      use type Application_Submission.Result;
      use type Application_Submission.Phase;
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      Selected_Index := 0;
      if Stored /= 0 and then
        Buffer_Retirement_Pending = 0 and then
        not Application_Maps.Presentation_Held (Application_Map_State, Session) and then
        not Application_Buffers.Image_Writes_Held (Application_Buffer_State, Session)
      then
         Code := Application_Buffers.Bad_Request;
         -- [version | (byte offset << 32), BO handle, raw48 GPU address, bytes]
         if Msg.tag.length = 4 and then Msg.tag.flags = 0 and then Msg.tag.reserved = 0 and then
           (Msg.words (0) and 16#FFFF_FFFF#) = 1
         then
            Selected_Index := Stored;
            Selected_Session := Session; Selected_Sender := Unsigned_64 (From);
            Selected_Stamp := Msg.authorityTag;
            Selected_Identity := Intel_GPU_Render_Control.Recipient_Identity
              (Render_Admission, Selected_Sender, Selected_Stamp);
            Selected_Context := Context_Pool.Session_Context (Contexts, Session);
            Code := Application_Buffers.Unavailable;
            -- Do not evaluate fields of the Ready=True variant until the
            -- backing discriminant is established. Submission_Owner runs
            -- later and cannot protect these selection-time reads.
            if Private_Contexts (Selected_Index).Context.Ready then
               Selected_CPU := Private_Contexts (Selected_Index).Context.CPU_Address;
               Selected_Bytes := Private_Contexts (Selected_Index).Context.Bytes;
               Selected_DMA := Intel_GPU_Buffer_Reply.Page_Address
                 (Private_Contexts (Selected_Index).Context, 0);
               Selected_GPU := Application_Publication.GPU_Address (Application_Images_State (Selected_Index));
               if Update_Pending = 0 and then not In_Place_Active and then Submission_Owner and then
                 Context_Pool.Can_Run_And_Retire (Contexts, Selected_Context)
               then
                  if Application_Submission.Current (Application_Submissions (Selected_Index)) =
                    Application_Submission.Uninitialized
                  then
                     Application_Submission.Initialize (Application_Submissions (Selected_Index),
                       Application_Setup_Complete (Selected_Index));
                  end if;
                  Application_Submission.Execute_With_Receipt (Application_Submissions (Selected_Index),
                    Msg.words (1), Msg.words (2), Shift_Right (Msg.words (0), 32), Msg.words (3),
                    Receipt, Status);
                  if Status = Application_Submission.Complete then
                     Completion := Application_Submission.Receipt_Sequence
                       (Application_Submissions (Selected_Index), Receipt);
                     if Completion = 0 then
                        Quarantine_Submission;
                        Status := Application_Submission.Faulted;
                     end if;
                  end if;
                  Code := (case Status is
                    when Application_Submission.Complete => Application_Buffers.OK,
                    when Application_Submission.Rejected => Application_Buffers.Bad_Request,
                    when Application_Submission.Batch_Denied => Application_Buffers.Denied,
                    when others => Application_Buffers.Unavailable);
               end if;
            end if;
         end if;
      end if;
      Response.tag := (Submit_Label, 4, 0, 0);
      Response.words := [Code, 1, Unsigned_64 (Completion), 0];
      Delivered := reply (From, Response);
      if Delivered /= 1 and then Code = Application_Buffers.OK then
         -- Completed work cannot be replayed safely after a lost reply.
         Quarantine_Submission;
      end if;
      Selected_Index := 0;
   end Handle_Application_Submission;
   Update_Index : Natural := 0;
   Update_Session, Update_Identity, Update_Sender, Update_Stamp : Unsigned_64 := 0;
   Update_Request : Message := NULL_MESSAGE;
   Update_Context : Unsigned_32 := Context_Pool.No_Context;
   Update_Held : Boolean := False;
   function Update_Owner return Boolean is
     (Update_Index in Private_Contexts'Range and then not Runtime_Fault and then
      Context_Owner and then PCI_Device = 16#46D2# and then Reset_Pages_Mapped and then
      Intel_GPU_Native_Reset.Last_Succeeded and then
      Application_Setup_Complete (Update_Index) and then
      Private_Contexts (Update_Index).Life = Application_Lifetime.Published and then
      Application_Session (Update_Sender, Update_Stamp) = Update_Session and then
      Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, Update_Sender, Update_Stamp) = Update_Identity and then
      Context_Pool.Session_Context (Contexts, Update_Session) = Update_Context and then
      Update_Context /= Context_Pool.No_Context and then not Context_Pool.Failed (Contexts));
   function Update_Exclusive return Boolean is
   begin
      if not Update_Owner or else not Update_Held or else
        (Update_Pending = 0 and then not In_Place_Active) then
         return False;
      end if;
      -- Current native backend is RCS-only, with synchronous completed/flush
      -- markers and disable acknowledgments between batches. No OA admission.
      -- Require acknowledged scheduling stop for every retained context,
      -- including tombstones whose deregistration has completed.
      for I in 1 .. Context_Pool.Count (Contexts) loop
         if not Context_Life.Scheduling_Stopped
           (Context_Pool.State (Contexts, First_Context_ID + Unsigned_32 (I - 1)))
         then return False; end if;
      end loop;
      return True;
   end Update_Exclusive;
   package Update_Storage is new Intel_GPU_Update_Storage
     (Intel_GPU_Metadata_Platform.Storage, Update_Exclusive);
   Update_Images : Update_Storage.Pool;
   Update_Image_Pending : Boolean := False;
   package Live_TLB_IO is new Intel_GPU_Native_TLB_IO (Update_Exclusive);
   procedure Update_Clock (Value : out Unsigned_64; OK : out Boolean) is
   begin
      Value := Runtime_Now;
      OK := Value /= Unsigned_64'Last and then Update_Exclusive;
   end Update_Clock;
   package Live_TLB is new Intel_GPU_ADLN_TLB_Invalidate
     (Update_Exclusive, Live_TLB_IO.Write_Register, Live_TLB_IO.Read_Register, Update_Clock);
   package Live_Images is new Application_Images.Updates (Update_Exclusive);
   package Live_Snapshots is new Application_VM.Snapshots;
   Removal_Backing : Intel_GPU_Buffer_Reply.Backing;
   type Mapping_Capture is record
      Ready : Boolean := False;
      Index : Natural := 0;
      Session, Identity, Ticket, Root, Revision, Generation : Unsigned_64 := 0;
   end record;
   Removal_Capture : Mapping_Capture;
   Removal_GPU, Removal_Offset, Removal_Bytes, Removal_Revision : Unsigned_64 := 0;
   Removal_Invalidated : Boolean := False;
   -- Ticket-relative authority boundary shared by current capture and future
   -- per-table provenance. Never accept a client CPU/DMA address here.
   function Table_Ticket_Session (Ticket : Unsigned_64) return Unsigned_64 is
     (Application_Buffers.Ticket_Session (Application_Buffer_State, Ticket));
   function Table_Allocation_Admitted
     (Session, Ticket : Unsigned_64; Slot : Positive) return Boolean is
   begin
      if not Publication_Owner_Ready or else Session = 0 or else Ticket = 0 or else
        Application_Buffers.Ticket_Slot (Ticket) /= Slot or else
        Table_Ticket_Session (Ticket) /= Session or else
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session) = 0
      then return False; end if;
      -- Incremental allocations are authenticated private table-only tickets;
      -- they do not own a replacement image. Backing must separately have been
      -- installed from the retained supervisor allocation into the registry.
      if Application_Buffers.Is_Table_Allocation
        (Application_Buffer_State, Session, Ticket, Application_Buffers.Incremental_Tables)
      then return True; end if;
      if not Application_Buffers.Is_Table_Allocation
        (Application_Buffer_State, Session, Ticket, Application_Buffers.Replacement_Tables) or else
        not Application_State.Has_Update (Slot)
      then return False; end if;
      if Ticket = Update_Pending and then Session = Update_Session then
         return Update_Exclusive;
      end if;
      if Slot > Replacement_Records.Capacity (Replacement_Tables) then return False; end if;
      declare
         Saved : constant Replacement_Record := Replacement_Records.Get (Replacement_Tables, Slot);
      begin
         return Saved.Session = Session and Saved.Ticket = Ticket and not Saved.Superseded;
      end;
   end Table_Allocation_Admitted;
   function Table_Allocation_Retired (Session, Ticket : Unsigned_64) return Boolean is
     (not Runtime_Fault and then Context_Owner and then Session /= 0 and then Ticket /= 0 and then
      Session = Buffer_Retirement_Session and then Buffer_Retirement_Pending /= 0 and then
      Ticket = Table_Release_Ticket and then
      (Buffer_Retirement_Is_Private or else Buffer_Retirement_Is_Closed_Table or else
       Buffer_Retirement_Is_Context) and then
      Buffer_Memory.Retirement_Confirmed (Buffer_Pool, Application_Buffers.Ticket_Slot (Ticket),
        Application_Buffers.Ticket_Generation (Ticket)));
   procedure Select_Table_Slice
     (Session, Ticket : Unsigned_64;
      Selected : out Intel_GPU_Buffer_Reply.Backing;
      Allocation_Offset : out Unsigned_64; Accepted : out Boolean)
   is
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
   begin
      Selected := (Ready => False); Allocation_Offset := 0; Accepted := False;
      if not Publication_Owner_Ready or else Session = 0 or else Stored = 0
        or else Ticket = 0
        or else Application_Buffers.Ticket_Session (Application_Buffer_State, Ticket) /= Session
      then return; end if;
      if Private_Contexts (Positive (Stored)).Parent_Ticket = Ticket then
         -- Initial allocation also contains context/ring and scratch. Expose
         -- only its table slice, retaining absolute allocation-relative offsets.
         Allocation_Offset := Intel_GPU_Submission_Image.Byte_Count;
         Selected := Private_Contexts (Positive (Stored)).Tables;
      else
         Table_Allocations.Lookup (Table_Backing_Registry, Application_Buffers.Ticket_Slot (Ticket),
           Session, Ticket,
           (if Application_Buffers.Is_Table_Allocation
              (Application_Buffer_State, Session, Ticket, Application_Buffers.Incremental_Tables)
            then Table_Allocations.Incremental_Tables else Table_Allocations.Replacement_Image),
           Selected, Accepted);
         return;
      end if;
      Accepted := True;
   end Select_Table_Slice;
   package Table_Resolver is new Intel_GPU_Table_Provenance.Backing
     (Publication_Owner_Ready, Table_Ticket_Session, Select_Table_Slice);
   procedure Resolve_Table_Page
     (Session, Ticket, Offset : Unsigned_64;
      CPU, DMA : out Unsigned_64; Accepted : out Boolean)
     renames Table_Resolver.Resolve_Owned_Page;
   package Table_Authority is new Intel_GPU_Table_Provenance.Authority (Resolve_Table_Page);
   Table_Appends : array (Private_Contexts'Range) of Table_Authority.Append_State;
   use type Table_Authority.Append_Phase;
   Ledger_Index : Natural range 0 .. Intel_GPU_Render_Sessions.Capacity := 0;
   Ledger_Session : Unsigned_64 := 0;
   function Table_Ledger_Busy return Boolean is (Ledger_Index /= 0);
   function Table_Ledger_Owner return Boolean is
     (Ledger_Index in Private_Contexts'Range and then Publication_Owner_Ready and then
      Private_Contexts (Ledger_Index).Life = Application_Lifetime.Offline and then
      Intel_GPU_Render_Control.Storage_Index (Render_Admission, Ledger_Session) = Ledger_Index);
   procedure Stop_Table_Ledger (Reason : String) is
   begin
      if Ledger_Index in Private_Contexts'Range then
         Private_Contexts (Ledger_Index).Life := Application_Lifetime.Retired;
      end if;
      Publish_Snapshot (Reason);
      Ledger_Index := 0; Ledger_Session := 0;
   end Stop_Table_Ledger;
   function Offline_Metadata_Owner return Boolean;
   Live_Metadata_Pending : Boolean := False;
   Live_Metadata_Epoch : Unsigned_64 := 0;
   function Live_Metadata_Owner return Boolean is
     (Live_Metadata_Pending and then In_Place_Active and then not Update_Held and then
      Update_Pending = 0 and then Update_Owner and then
      Application_VM.Revision (Private_Contexts (Update_Index).Source) = Live_Metadata_Epoch);
   function Context_Metadata_Index return Natural is
     (if Table_Ledger_Busy then Ledger_Index else Update_Index);
   function Context_Metadata_Owner return Boolean is
     (if Table_Ledger_Busy then Table_Ledger_Owner
      elsif Live_Metadata_Pending then Live_Metadata_Owner else Offline_Metadata_Owner);
   function Ledger_Capacity return Positive is
     (if Context_Metadata_Index = 0 then 1 else
      Intel_GPU_Table_Provenance.Capacity (Private_Contexts (Context_Metadata_Index).Table_Owners));
   procedure Extend_Ledger (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Context_Metadata_Owner then
         Intel_GPU_Table_Provenance.Extend
           (Private_Contexts (Context_Metadata_Index).Table_Owners, Base, Bytes, Accepted);
      end if;
   end Extend_Ledger;
   function Context_Mirror_Capacity return Positive is
     (if Context_Metadata_Index = 0 then 1 else
      Application_VM.Mirror_Capacity (Private_Contexts (Context_Metadata_Index).Source));
   procedure Extend_Context_Mirror (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Context_Metadata_Owner then
         Application_VM.Extend_Metadata
           (Private_Contexts (Context_Metadata_Index).Source, Base, Bytes, Accepted);
      end if;
   end Extend_Context_Mirror;
   function Context_Descriptor_Capacity return Positive is
     (if Context_Metadata_Index = 0 then 1 else
      Application_VM.Descriptor_Capacity (Private_Contexts (Context_Metadata_Index).Source));
   procedure Extend_Context_Descriptors (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Context_Metadata_Owner then
         Application_VM.Extend_Descriptors
           (Private_Contexts (Context_Metadata_Index).Source, Base, Bytes, Accepted);
      end if;
   end Extend_Context_Descriptors;
   function Context_Reference_Capacity return Positive is
     (if Context_Metadata_Index = 0 then 1 else Application_State.Table_References.Capacity
       (Private_Contexts (Context_Metadata_Index).Table_IDs));
   procedure Extend_Context_References (Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Context_Metadata_Owner then
         Application_State.Table_References.Extend
           (Private_Contexts (Context_Metadata_Index).Table_IDs, Base, Bytes, Accepted);
      end if;
   end Extend_Context_References;
   type Context_Metadata_Table is (References, Descriptors, Mirrors, Provenance);
   type Context_Metadata_Needs is array (Context_Metadata_Table) of Natural;
   Context_Metadata_Targets : array (Private_Contexts'Range) of Context_Metadata_Needs :=
     [others => [others => 0]];
   function Context_Metadata_Capacity (Table : Context_Metadata_Table) return Natural is
     (case Table is
        when References => Context_Reference_Capacity,
        when Descriptors => Context_Descriptor_Capacity,
        when Mirrors => Context_Mirror_Capacity,
        when Provenance => Ledger_Capacity);
   procedure Extend_Context_Metadata
     (Table : Context_Metadata_Table; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      case Table is
         when References => Extend_Context_References (Base, Bytes, Accepted);
         when Descriptors => Extend_Context_Descriptors (Base, Bytes, Accepted);
         when Mirrors => Extend_Context_Mirror (Base, Bytes, Accepted);
         when Provenance => Extend_Ledger (Base, Bytes, Accepted);
      end case;
   end Extend_Context_Metadata;
   procedure Admit_Context_Metadata (Count : Positive; Accepted : out Boolean) is
   begin
      Accepted := Context_Metadata_Owner and then Count <= Application_State.Table_Pages
        and then Context_Metadata_Targets (Context_Metadata_Index) (References) = Count
        and then (for all T in Context_Metadata_Table => Context_Metadata_Capacity (T) >=
          Context_Metadata_Targets (Context_Metadata_Index) (T));
   end Admit_Context_Metadata;
   package Context_Metadata_Growth is new Intel_GPU_Metadata_Bundle
     (Context_Metadata_Table, Intel_GPU_Metadata_Platform.Storage,
      Context_Metadata_Owner, Context_Metadata_Capacity, Extend_Context_Metadata, Admit_Context_Metadata);
   Context_Metadata : array (Private_Contexts'Range) of Context_Metadata_Growth.Bundle;
   use type Context_Metadata_Growth.Phase;
   procedure Request_Context_Metadata
     (Index, Tables, Records : Positive; OK : out Boolean) is
   begin
      OK := False;
      if not Context_Metadata_Owner or else Context_Metadata_Index /= Index then return; end if;
      Context_Metadata_Growth.Request
        (Context_Metadata (Index), Tables,
         [References => (Tables, Application_State.Table_Pages,
                         Application_State.Table_References.Metadata_Bytes),
          Descriptors => (Tables, Application_State.Table_Pages,
                          Application_VM.Descriptor_Metadata_Bytes),
          Mirrors => (Tables, Application_State.Table_Pages,
                      Unsigned_64 (Application_State.Table_Pages -
                        Application_State.Bootstrap_Table_Mirrors) * 4096),
          Provenance => (Records, 32768, 1024 * 1024)], OK);
      if OK then
         Context_Metadata_Targets (Index) :=
           [References | Descriptors | Mirrors => Tables, Provenance => Records];
      end if;
   end Request_Context_Metadata;
   procedure Start_Table_Ledger (Index : Positive; Session : Unsigned_64) is
      OK : Boolean;
   begin
      if Table_Ledger_Busy or else Index not in Private_Contexts'Range then
         Runtime_Fault := True; return;
      end if;
      Ledger_Index := Index; Ledger_Session := Session;
      if not Table_Ledger_Owner then
         Stop_Table_Ledger ("intel-gpu: initial metadata owner unavailable; backing retained");
         return;
      end if;
      -- Stable per-store byte reservations, not BO backing or GPU VA.
      Request_Context_Metadata
        (Index, Private_Table_Pages, Private_Table_Pages, OK);
      if not OK then
         Private_Contexts (Index).Life := Application_Lifetime.Retired;
         Publish_Snapshot ("intel-gpu: initial table provenance request rejected; backing retained");
         Ledger_Index := 0; Ledger_Session := 0;
      end if;
   end Start_Table_Ledger;
   procedure Grow_Table_Ledger is
      OK : Boolean := False;
   begin
      if not Table_Ledger_Busy then return; end if;
      if Table_Ledger_Owner then
         Context_Metadata_Growth.Step (Context_Metadata (Ledger_Index));
         if not Table_Ledger_Owner then
            Stop_Table_Ledger ("intel-gpu: table metadata owner lost; backing retained"); return;
         end if;
         case Context_Metadata_Growth.State (Context_Metadata (Ledger_Index)) is
            when Context_Metadata_Growth.Idle =>
               if Table_Authority.Status (Table_Appends (Ledger_Index)) = Table_Authority.Unused then
                  Table_Authority.Begin_Append
                    (Table_Appends (Ledger_Index), Private_Contexts (Ledger_Index).Table_Owners,
                     Ledger_Session, Private_Contexts (Ledger_Index).Table_Generation,
                     Private_Contexts (Ledger_Index).Parent_Ticket,
                     Intel_GPU_Submission_Image.Byte_Count, Private_Table_Pages, OK);
                  if not Table_Ledger_Owner then
                     Stop_Table_Ledger ("intel-gpu: table append owner lost; backing retained"); return;
                  end if;
                  if OK then return; end if;
               else
                  Table_Authority.Step
                    (Table_Appends (Ledger_Index), Private_Contexts (Ledger_Index).Table_Owners);
                  if not Table_Ledger_Owner then
                     Stop_Table_Ledger ("intel-gpu: table append owner lost; backing retained"); return;
                  end if;
                  if Table_Authority.Status (Table_Appends (Ledger_Index)) = Table_Authority.Appending
                  then return; end if;
                  OK := Table_Authority.Status (Table_Appends (Ledger_Index)) = Table_Authority.Appended;
                  if OK then
                     for Page in 1 .. Private_Table_Pages loop
                        Application_State.Table_References.Put
                          (Private_Contexts (Ledger_Index).Table_IDs,
                           Private_Contexts (Ledger_Index).Table_Generation, Page,
                           Table_Authority.First_ID (Table_Appends (Ledger_Index)) + Page - 1, OK);
                        exit when not OK;
                     end loop;
                  end if;
               end if;
            when Context_Metadata_Growth.Failed => null;
            when others => return;
         end case;
      end if;
      if not OK then Private_Contexts (Ledger_Index).Life := Application_Lifetime.Retired; end if;
      Publish_Snapshot ("intel-gpu: initial table provenance ready=" & Boolean'Image (OK) &
        " records=" & Natural'Image (Intel_GPU_Table_Provenance.Count
          (Private_Contexts (Ledger_Index).Table_Owners)) &
        " growth=" & Context_Metadata_Growth.Phase'Image
          (Context_Metadata_Growth.State (Context_Metadata (Ledger_Index))));
      Ledger_Index := 0; Ledger_Session := 0;
   end Grow_Table_Ledger;
   function Initial_Table_Mapping (Index : Positive; Session : Unsigned_64;
      Page : Positive) return Intel_GPU_Table_Provenance.Mapping is
   begin
      if Index not in Private_Contexts'Range or else
        Page not in Application_VM.Page_Number or else
        Application_State.Table_References.Get (Private_Contexts (Index).Table_IDs,
          Private_Contexts (Index).Table_Generation, Page) = 0
      then return (others => 0); end if;
      return Table_Authority.Lookup (Private_Contexts (Index).Table_Owners, Session,
        Private_Contexts (Index).Table_Generation,
        Application_State.Table_References.Get (Private_Contexts (Index).Table_IDs,
          Private_Contexts (Index).Table_Generation, Page));
   end Initial_Table_Mapping;
   function Replacement_Table_Mapping
     (Slot : Intel_GPU_Buffer_Backing.Slot; Session : Unsigned_64;
      Page : Positive) return Intel_GPU_Table_Provenance.Mapping is
   begin
      if not Application_State.Has_Update (Slot) or else
        Page not in Application_VM.Page_Number
      then return (others => 0); end if;
      declare
         Item : Application_State.Update_Record renames Application_State.Updates (Slot).all;
      begin
         if Application_State.Table_References.Get (Item.Table_IDs, Item.Table_Generation, Page) = 0
         then return (others => 0); end if;
         return Table_Authority.Lookup (Item.Table_Owners, Session,
           Item.Table_Generation, Application_State.Table_References.Get
             (Item.Table_IDs, Item.Table_Generation, Page));
      end;
   end Replacement_Table_Mapping;
   -- Initial-root incremental allocations retain independent tickets in the
   -- context ledger. New image ordinals are installed only after directory
   -- publication and its own TLB receipt; leaf binding uses a second receipt.
   type Directory_Phase is
     (No_Directory_Update, Allocate_Directories, Register_Directories,
      Start_Directories, Publish_Directories, Invalidate_Directories,
      Commit_Directories);
   Directory_Update : Directory_Phase := No_Directory_Update;
   Directory_First_ID, Directory_Previous_Used : Natural := 0;
   Directory_Invalidated : Boolean := False;
   function Directory_Exclusive return Boolean is
     (Directory_Update /= No_Directory_Update and then Update_Exclusive and then
      Current_Table_Ticket (Update_Index) = 0 and then
      Private_Contexts (Update_Index).Table_Generation =
        Intel_GPU_Table_Provenance.Generation (Private_Contexts (Update_Index).Table_Owners));
   function Directory_Table_ID (DMA : Unsigned_64) return Natural is
      M : Intel_GPU_Table_Provenance.Mapping;
   begin
      if DMA = 0 or else not Directory_Exclusive then return 0; end if;
      for P in 1 .. Application_VM.Used (Private_Contexts (Update_Index).Source) loop
         M := Initial_Table_Mapping (Update_Index, Update_Session, P);
         if M.Ticket /= 0 and then M.DMA = DMA then
            return Application_State.Table_References.Get
              (Private_Contexts (Update_Index).Table_IDs,
               Private_Contexts (Update_Index).Table_Generation, P);
         end if;
      end loop;
      if Directory_First_ID /= 0 and then Update_Table_Pages /= 0 then
         for P in 0 .. Update_Table_Pages - 1 loop
            M := Table_Authority.Lookup (Private_Contexts (Update_Index).Table_Owners,
              Update_Session, Private_Contexts (Update_Index).Table_Generation,
              Directory_First_ID + P);
            if M.Ticket = Update_Pending and then M.DMA = DMA then
               return Directory_First_ID + P;
            end if;
         end loop;
      end if;
      return 0;
   end Directory_Table_ID;
   function Directory_Owned (DMA : Unsigned_64) return Boolean is
     (Directory_Table_ID (DMA) /= 0);
   function Directory_Flush_CPU (CPU : Unsigned_64) return Boolean is
     (Directory_Exclusive and then Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096)
      and then Directory_Exclusive);
   package Directory_IO is new Intel_GPU_Table_Provenance.IO
     (Table_Authority, Directory_Exclusive, Directory_Flush_CPU);
   procedure Read_Directory_Word
     (DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Value : out Unsigned_64; OK : out Boolean) is
      ID : constant Natural := Directory_Table_ID (DMA);
   begin
      Value := 0; OK := False;
      if ID = 0 then return; end if;
      Directory_IO.Read_Word (Private_Contexts (Update_Index).Table_Owners,
        Update_Session, Private_Contexts (Update_Index).Table_Generation,
        ID, DMA, Index, Value, OK);
   end Read_Directory_Word;
   procedure Write_Directory_Word
     (DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Value : Unsigned_64; OK : out Boolean) is
      ID : constant Natural := Directory_Table_ID (DMA);
   begin
      OK := False;
      if ID = 0 then return; end if;
      Directory_IO.Write_Word (Private_Contexts (Update_Index).Table_Owners,
        Update_Session, Private_Contexts (Update_Index).Table_Generation,
        ID, DMA, Index, Value, OK);
   end Write_Directory_Word;
   function Flush_Directory_Page (DMA : Unsigned_64) return Boolean is
      ID : constant Natural := Directory_Table_ID (DMA);
   begin
      return ID /= 0 and then Directory_IO.Flush
        (Private_Contexts (Update_Index).Table_Owners, Update_Session,
         Private_Contexts (Update_Index).Table_Generation, ID, DMA);
   end Flush_Directory_Page;
   function Directory_TLB_Confirmed return Boolean is
     (Directory_Invalidated and then Directory_Exclusive);
   package Directory_Backing is new Application_Topology.Backing (Directory_Owned);
   package Directory_Writer is new Directory_Backing.Writer
     (Directory_Exclusive, Read_Directory_Word, Write_Directory_Word,
      Flush_Directory_Page, Directory_TLB_Confirmed);
   type Offline_Bind_Phase is
     (No_Offline_Bind, Grow_Offline_Metadata, Allocate_Offline, Register_Offline, Commit_Offline);
   Offline_Bind_State : Offline_Bind_Phase := No_Offline_Bind;
   Offline_Epoch : Unsigned_64 := 0;
   function Offline_Bind_Owner return Boolean is
      Status : Application_Binding.Preparation_Result;
      use type Application_Binding.Preparation_Result;
   begin
      if Offline_Bind_State = No_Offline_Bind or else Update_Pending = 0 or else
        Update_Index not in Private_Contexts'Range or else Runtime_Fault or else
        not Publication_Owner_Ready or else Table_Ledger_Busy or else
        Private_Contexts (Update_Index).Life /= Application_Lifetime.Offline or else
        Application_Setup_Complete (Update_Index) or else
        Current_Table_Ticket (Update_Index) /= 0 or else
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Update_Session) /= Update_Index or else
        Intel_GPU_Render_Control.Recipient_Identity
          (Render_Admission, Update_Sender, Update_Stamp) /= Update_Identity or else
        Private_Contexts (Update_Index).Table_Generation /=
          Intel_GPU_Table_Provenance.Generation (Private_Contexts (Update_Index).Table_Owners)
      then return False; end if;
      Application_Binding.Check_Offline_Bind_Request
        (Application_Buffer_State, Private_Contexts (Update_Index).Source,
         Update_Session, Offline_Epoch, Update_Sender, Update_Stamp,
         Update_Request.tag.label, Update_Request.tag.length,
         Update_Request.tag.flags, Update_Request.tag.reserved,
         [Update_Request.words (0), Update_Request.words (1),
          Update_Request.words (2), Update_Request.words (3)], Status);
      return Status = Application_Binding.Eligible;
   end Offline_Bind_Owner;
   function Offline_Metadata_Owner return Boolean is
     (Offline_Bind_State = Grow_Offline_Metadata and then Offline_Bind_Owner);
   procedure Reject_Offline_Bind is
   begin
      Intel_GPU_Render_Control.Reject_Delivery (Render_Admission, Update_Identity, Update_Session);
      Retire_Application_Resources (Update_Session);
   end Reject_Offline_Bind;
   procedure Finish_Offline_Bind (Backing : Intel_GPU_Buffer_Reply.Backing) is
      OK, Consumed : Boolean := False;
      Words : Application_Buffers.Words := [Application_Buffers.Unavailable, 1, 0, 0];
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
      First, ID : Natural := 0;
      function Appended_Page (Ordinal : Positive) return Unsigned_64 is
         M : Intel_GPU_Table_Provenance.Mapping;
      begin
         if not Offline_Bind_Owner or else ID = 0 or else
           Ordinal > Update_Table_Pages or else ID > Natural'Last - (Ordinal - 1)
         then return 0; end if;
         M := Table_Authority.Lookup
           (Private_Contexts (Update_Index).Table_Owners, Update_Session,
            Private_Contexts (Update_Index).Table_Generation, ID + (Ordinal - 1));
         if not Offline_Bind_Owner or else M.Ticket /= Update_Pending
         then return 0; end if;
         return M.DMA;
      end Appended_Page;
      procedure Append_Stream is new Application_VM.Append_Offline_From_Pages
        (Appended_Page, Offline_Bind_Owner);
   begin
      if Offline_Bind_State = No_Offline_Bind then return; end if;
      if Offline_Bind_State = Grow_Offline_Metadata and then Offline_Bind_Owner then
         -- No supervisor backing operation is active in this phase. The event
         -- loop supplies its old result; never interpret it as this ticket's
         -- backing. Advance one metadata phase and revalidate the captured BO,
         -- requester, epoch and topology before starting physical allocation.
         Context_Metadata_Growth.Step (Context_Metadata (Update_Index));
         if Offline_Bind_Owner then
            case Context_Metadata_Growth.State (Context_Metadata (Update_Index)) is
               when Context_Metadata_Growth.Idle =>
                  declare
                     Needed : constant Application_Topology.Offline_Requirements :=
                       Application_Topology.Inspect_Offline
                         (Private_Contexts (Update_Index).Source,
                          Update_Request.words (2), Update_Request.words (3));
                     use type Application_Topology.Plan_Status;
                  begin
                     if Needed.Topology.Status = Application_Topology.Ready and then
                       Needed.Topology.Fits_Reserved and then
                       Needed.Additional_Backing = Update_Table_Pages and then
                       Update_Table_Pages <= Ledger_Capacity -
                         Intel_GPU_Table_Provenance.Count (Private_Contexts (Update_Index).Table_Owners)
                     then
                        Offline_Bind_State := Allocate_Offline;
                        Buffer_Memory.Start (Buffer_Pool,
                          Application_Buffers.Ticket_Slot (Update_Pending), Update_Table_Pages, OK);
                        if OK then return; end if;
                     end if;
                  end;
               when Context_Metadata_Growth.Failed => null;
               when others => return;
            end case;
         end if;
         -- Fall through to terminal failure, not stale backing processing.
         Offline_Bind_State := Grow_Offline_Metadata;
      end if;
      if Offline_Bind_Owner and then Intel_GPU_Buffer_Reply.Valid (Backing) and then
        Update_Table_Pages /= 0 and then Backing.Bytes = Unsigned_64 (Update_Table_Pages) * 4096
      then
         case Offline_Bind_State is
            when Allocate_Offline =>
               Table_Allocations.Install (Table_Backing_Registry,
                 Application_Buffers.Ticket_Slot (Update_Pending), Update_Session,
                 Update_Pending, Table_Allocations.Incremental_Tables, Backing, OK);
               if OK then Table_Authority.Rearm (Table_Appends (Update_Index),
                 Private_Contexts (Update_Index).Table_Owners, OK); end if;
               if OK then Table_Authority.Begin_Append (Table_Appends (Update_Index),
                 Private_Contexts (Update_Index).Table_Owners, Update_Session,
                 Private_Contexts (Update_Index).Table_Generation,
                 Update_Pending, 0, Update_Table_Pages, OK); end if;
               if OK then Offline_Bind_State := Register_Offline; return; end if;
            when Register_Offline =>
               Table_Authority.Step (Table_Appends (Update_Index), Private_Contexts (Update_Index).Table_Owners);
               if Table_Authority.Status (Table_Appends (Update_Index)) = Table_Authority.Appending
               then return; end if;
               if Table_Authority.Status (Table_Appends (Update_Index)) = Table_Authority.Appended then
                  Offline_Bind_State := Commit_Offline; return;
               end if;
            when Commit_Offline =>
               declare begin
                  ID := Table_Authority.First_ID (Table_Appends (Update_Index));
                  OK := ID /= 0;
                  if OK and then Offline_Bind_Owner then
                     Append_Stream
                       (Private_Contexts (Update_Index).Source, Update_Table_Pages, First, OK);
                     if OK then
                        for P in 1 .. Update_Table_Pages loop
                           Application_State.Table_References.Put
                             (Private_Contexts (Update_Index).Table_IDs,
                              Private_Contexts (Update_Index).Table_Generation,
                              First + P - 1, ID + P - 1, OK);
                           exit when not OK;
                        end loop;
                        -- This sole serialized mutation advances the expected
                        -- epoch. It does not authorize a different requester.
                        if OK then
                        Offline_Epoch := Application_VM.Revision (Private_Contexts (Update_Index).Source);
                        Application_Binding.Handle (Application_Buffer_State,
                          Private_Contexts (Update_Index).Source, Update_Session,
                          Update_Sender, Update_Stamp, Update_Request.tag.label,
                          Update_Request.tag.length, Update_Request.tag.flags, Update_Request.tag.reserved,
                          [Update_Request.words (0), Update_Request.words (1),
                           Update_Request.words (2), Update_Request.words (3)], Words);
                        end if;
                     end if;
                  end if;
               end;
            when No_Offline_Bind | Grow_Offline_Metadata => null;
         end case;
      end if;
      Application_Buffers.Finish_Private (Application_Buffer_State, Update_Pending, Consumed);
      if not Consumed then Words := [Application_Buffers.Unavailable, 1, 0, 0]; end if;
      if Words (0) /= Application_Buffers.OK then
         Publish_Snapshot ("intel-gpu: offline backing growth failed; backing retained");
         Reject_Offline_Bind;
      end if;
      Response.tag := (Application_Binding.Bind_Label, 4, 0, 0);
      Response.words := [Words (0), Words (1), Words (2), Words (3)];
      Delivered := replyCap (Application_Reply_Slot, Response);
      if Delivered /= 1 then Reject_Offline_Bind; end if;
      Offline_Bind_State := No_Offline_Bind;
      Update_Pending := 0; Update_Table_Pages := 0; Update_Index := 0;
   end Finish_Offline_Bind;
   function Try_Offline_Bind_Growth (From : ProcessID; Msg : Message) return Boolean is
      Session : constant Unsigned_64 := Application_Session (Unsigned_64 (From), Msg.authorityTag);
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
      Status : Application_Binding.Preparation_Result;
      Needed : Application_Topology.Offline_Requirements;
      Started, Consumed : Boolean;
      use type Application_Binding.Preparation_Result, Application_Topology.Plan_Status;
   begin
      if Stored = 0 or else Update_Pending /= 0 or else In_Place_Active or else
        Application_Pending /= 0 or else Private_Pending /= 0 or else Buffer_Retirement_Pending /= 0 or else
        Table_Ledger_Busy or else Buffer_Memory.Pending (Buffer_Pool) or else
        Private_Contexts (Stored).Life /= Application_Lifetime.Offline
      then return False; end if;
      Application_Binding.Check_Offline_Bind_Request
        (Application_Buffer_State, Private_Contexts (Stored).Source, Session,
         Application_VM.Revision (Private_Contexts (Stored).Source), Unsigned_64 (From), Msg.authorityTag,
         Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
         [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Status);
      if Status /= Application_Binding.Eligible then return False; end if;
      Needed := Application_Topology.Inspect_Offline
        (Private_Contexts (Stored).Source, Msg.words (2), Msg.words (3));
      if Needed.Topology.Status /= Application_Topology.Ready or else
        not Needed.Topology.Fits_Quota or else Needed.Additional_Backing = 0 or else
        Intel_GPU_Table_Provenance.Count (Private_Contexts (Stored).Table_Owners) > 32768 or else
        Needed.Additional_Backing > 32768 -
          Intel_GPU_Table_Provenance.Count (Private_Contexts (Stored).Table_Owners)
      then return False; end if;
      Update_Index := Stored; Update_Session := Session; Update_Sender := Unsigned_64 (From);
      Update_Stamp := Msg.authorityTag; Update_Request := Msg;
      Update_Identity := Intel_GPU_Render_Control.Recipient_Identity (Render_Admission, Update_Sender, Update_Stamp);
      Offline_Epoch := Application_VM.Revision (Private_Contexts (Stored).Source);
      Update_Table_Pages := Needed.Additional_Backing;
      Application_Buffers.Reserve_Private (Application_Buffer_State, Session, Update_Pending,
        Reclaimable => True, Kind => Application_Buffers.Incremental_Tables,
        Pages => Update_Table_Pages);
      if Update_Pending = 0 then Update_Index := 0; Update_Table_Pages := 0; return False; end if;
      Offline_Bind_State := Grow_Offline_Metadata;
      if not Offline_Bind_Owner or else saveReplyCap (Unsigned_64 (Application_Reply_Slot)) /= 1 then
         Reject_Offline_Bind;
         Application_Buffers.Finish_Private (Application_Buffer_State, Update_Pending, Consumed);
         Offline_Bind_State := No_Offline_Bind; Update_Pending := 0;
         Update_Index := 0; Update_Table_Pages := 0;
         return False; -- caller still owns and answers the unsaved request
      end if;
      Request_Context_Metadata
        (Stored, Needed.Topology.Required_Tables,
         Intel_GPU_Table_Provenance.Count (Private_Contexts (Stored).Table_Owners) +
           Needed.Additional_Backing, Started);
      if not Started then
         Offline_Bind_State := Allocate_Offline;
         Finish_Offline_Bind ((Ready => False));
      end if;
      return True;
   end Try_Offline_Bind_Growth;
   Recycle_Slot : Intel_GPU_Buffer_Backing.Slot := 1;
   Recycle_Session, Recycle_Ticket : Unsigned_64 := 0;
   Recycle_Context_Index : Natural := 0;
   function Context_Group_Exclusion (Owner : Unsigned_64) return Boolean;
   function Recycle_Exclusion_Ready (Owner : Unsigned_64) return Boolean is
   begin
      if Buffer_Retirement_Is_Context then
         if not Context_Group_Exclusion (Owner) then return False; end if;
      else
      if not (Owner = Recycle_Session and then Owner /= 0 and then not Runtime_Fault and then Context_Owner
      and then Recycle_Ticket /= 0 and then Recycle_Ticket = Buffer_Retirement_Pending
      and then Owner = Buffer_Retirement_Session
      and then (Buffer_Retirement_Is_Private or else Buffer_Retirement_Is_Closed_Table)
      and then Application_Buffers.Ticket_Slot (Recycle_Ticket) = Recycle_Slot
      and then Application_State.Has_Update (Recycle_Slot)
      and then Application_Buffers.Is_Table_Allocation
        (Application_Buffer_State, Owner, Recycle_Ticket, Application_Buffers.Replacement_Tables))
      then return False; end if;
      declare
         Saved : constant Replacement_Record := Replacement_Records.Get (Replacement_Tables, Recycle_Slot);
      begin
         if Saved.Session /= Owner or else Saved.Ticket /= Recycle_Ticket or else
           (not Live_Snapshots.Retired (Application_State.Updates (Recycle_Slot).Candidate) and then
            (not Application_VM.Sealed (Application_State.Updates (Recycle_Slot).Candidate) or else
             Application_VM.Revision (Application_State.Updates (Recycle_Slot).Candidate) /= Saved.Revision or else
             Application_VM.Root_DMA (Application_State.Updates (Recycle_Slot).Candidate) /= Saved.Root))
         then return False; end if;
      end;
      end if;
      -- The preflight's alias proofs remain stable behind the global pending
      -- gate. Still recheck scheduling exclusion across every dispatcher yield.
      for I in 1 .. Context_Pool.Count (Contexts) loop
         if not Context_Life.Scheduling_Stopped
           (Context_Pool.State (Contexts, First_Context_ID + Unsigned_32 (I - 1)))
         then return False; end if;
      end loop;
      return True;
   end Recycle_Exclusion_Ready;
   function Recycle_Receipt_Ready (Owner : Unsigned_64) return Boolean is
     (Recycle_Exclusion_Ready (Owner) and then Buffer_Memory.Retirement_Confirmed
        (Buffer_Pool, Recycle_Slot, Application_Buffers.Ticket_Generation (Recycle_Ticket)));
   -- BEGIN GROUPED TABLE TRANSPORT
   function Recycle_May_Release (Owner, ID : Unsigned_64) return Boolean is
      use type Table_Allocations.Allocation_Role;
      Item : Table_Allocations.Retained_Allocation;
   begin
      if ID = 0 or else not Recycle_Exclusion_Ready (Owner) then return False; end if;
      if Buffer_Retirement_Is_Context and then ID = Recycle_Ticket then
         -- Combined context/ring/scratch parent is not a table-only registry
         -- entry. The group exclusion must establish its consumer retirement;
         -- authenticate its exact context ticket before requesting release.
         return Context_Tickets.Can_Retire (Application_Buffer_State, Owner, ID);
      end if;
      Item := Table_Allocations.Retained_At
        (Table_Backing_Registry, Application_Buffers.Ticket_Slot (ID));
      if not Item.Present or else Item.Session /= Owner or else Item.Ticket /= ID
      then return False; end if;
      if ID = Recycle_Ticket then
         return Item.Role = Table_Allocations.Replacement_Image;
      end if;
      return Item.Role = Table_Allocations.Incremental_Tables and then
        Application_Buffers.Is_Table_Allocation
          (Application_Buffer_State, Owner, ID, Application_Buffers.Incremental_Tables) and then
        (not Buffer_Retirement_Is_Context or else
         Closed_Table_Tickets.Can_Retire (Application_Buffer_State, Owner, ID));
   end Recycle_May_Release;
   function Recycle_Exact_Receipt (Owner, ID : Unsigned_64) return Boolean is
     (ID /= 0 and then ID = Table_Release_Ticket and then Recycle_May_Release (Owner, ID) and then
      Buffer_Memory.Retirement_Confirmed (Buffer_Pool, Application_Buffers.Ticket_Slot (ID),
        Application_Buffers.Ticket_Generation (ID)));
   package Table_Recycling is new Intel_GPU_Table_Provenance.Retirement
     (Recycle_Exclusion_Ready, Recycle_May_Release, Recycle_Exact_Receipt);
   procedure Submit_Table_Retirement (Owner, ID : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Table_Release_Ticket /= 0 or else not Recycle_May_Release (Owner, ID) or else
        Buffer_Memory.Pending (Buffer_Pool) then return; end if;
      Table_Release_Ticket := ID;
      Buffer_Memory.Retire (Buffer_Pool, Application_Buffers.Ticket_Slot (ID),
        Application_Buffers.Ticket_Generation (ID), True, Accepted);
      Last_Table_Retirement := (if Accepted then Retirement_Submitted else Retirement_Rejected);
   end Submit_Table_Retirement;
   procedure Poll_Table_Receipt (Owner, ID : Unsigned_64; Complete, Failed : out Boolean) is
   begin
      Complete := Recycle_Exact_Receipt (Owner, ID);
      Failed := not Recycle_May_Release (Owner, ID) or else
        (not Complete and then not Buffer_Memory.Pending (Buffer_Pool));
   end Poll_Table_Receipt;
   procedure Finalize_Table_Receipt (Owner, ID : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Recycle_Exact_Receipt (Owner, ID) then return; end if;
      if Buffer_Retirement_Is_Context and then ID = Recycle_Ticket then
         -- Final context acknowledgment belongs to the parent finalizer after
         -- the entire ledger has completed. There is no registry row to erase.
         Accepted := True;
      else
         Table_Allocations.Retire
           (Table_Backing_Registry, Application_Buffers.Ticket_Slot (ID), Owner, ID, Accepted);
      end if;
      if Accepted and then ID /= Recycle_Ticket then
         -- References for this ticket have been swept by the dispatcher. The
         -- anchor remains held until the complete group retires and reopens.
         if Buffer_Retirement_Is_Closed_Table or else Buffer_Retirement_Is_Context then
            Closed_Table_Tickets.Acknowledge (Application_Buffer_State, Owner, ID, True, Accepted);
         else
            Application_Buffers.Acknowledge_Private_Retirement
              (Application_Buffer_State, Owner, ID, True, Accepted);
         end if;
      end if;
      if Accepted then Table_Release_Ticket := 0; end if;
   end Finalize_Table_Receipt;
   -- END GROUPED TABLE TRANSPORT
   procedure Prepare_Table_Retirement
     (Owner, ID : Unsigned_64; Complete, Failed : out Boolean);
   package Table_Recycle_Dispatch is new Table_Recycling.Dispatcher
     (Prepare_Table_Retirement, Submit_Table_Retirement, Poll_Table_Receipt, Finalize_Table_Receipt);
   Table_Recycle_Control : Table_Recycle_Dispatch.Controller;
   use type Table_Recycle_Dispatch.State;
   function Recycle_In_Progress return Boolean is
     (Table_Recycle_Dispatch.Status (Table_Recycle_Control) = Table_Recycle_Dispatch.Running);
   procedure Begin_Table_Recycling
     (Slot : Intel_GPU_Buffer_Backing.Slot; Session, Ticket : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Table_Recycle_Dispatch.Status (Table_Recycle_Control) /= Table_Recycle_Dispatch.Unused then return; end if;
      Recycle_Context_Index := 0;
      Recycle_Slot := Slot; Recycle_Session := Session; Recycle_Ticket := Ticket;
      if Table_Release_Ticket /= 0 then return; end if;
      if not Recycle_Exclusion_Ready (Session) then return; end if;
      Table_Recycle_Dispatch.Start (Table_Recycle_Control, Application_State.Updates (Slot).Table_Owners,
        Session, Application_State.Updates (Slot).Table_Generation, Accepted,
        Last_Ticket => Ticket);
      -- Buffer_Memory exposes the current retirement receipt. Keep the group's
      -- anchor last so the finalizer can validate that exact slot/generation,
      -- never a preceding child's receipt. Child allocations additionally
      -- require incremental role/identity and the per-ticket alias preflight.
   end Begin_Table_Recycling;
   procedure Advance_Table_Recycling is
   begin
      if Recycle_In_Progress then
         if Recycle_Context_Index in Private_Contexts'Range then
            Table_Recycle_Dispatch.Step
              (Table_Recycle_Control, Private_Contexts (Recycle_Context_Index).Table_Owners);
         else
            Table_Recycle_Dispatch.Step (Table_Recycle_Control, Application_State.Updates (Recycle_Slot).Table_Owners);
         end if;
      end if;
   end Advance_Table_Recycling;
   procedure Recycle_Table_Ledger
     (Slot : Intel_GPU_Buffer_Backing.Slot; Session, Ticket : Unsigned_64;
      Accepted, Pending : out Boolean)
   is
      OK : Boolean;
   begin
      Accepted := False; Pending := False;
      if Slot /= Recycle_Slot or else Session /= Recycle_Session or else Ticket /= Recycle_Ticket
        or else not Recycle_Receipt_Ready (Session) then return; end if;
      declare
         Item : Application_State.Update_Record renames Application_State.Updates (Slot).all;
      begin
         Pending := Recycle_In_Progress;
         if Pending or else Table_Recycle_Dispatch.Status (Table_Recycle_Control) /= Table_Recycle_Dispatch.Done
         then return; end if;
         Table_Recycle_Dispatch.Reopen (Table_Recycle_Control, Item.Table_Owners, OK);
         if not OK then return; end if;
         Application_State.Table_References.Reopen (Item.Table_IDs, Item.Table_Generation,
           Intel_GPU_Table_Provenance.Generation (Item.Table_Owners), OK);
         if not OK then return; end if;
         Item.Table_Generation := Intel_GPU_Table_Provenance.Generation (Item.Table_Owners);
         Recycle_Session := 0; Recycle_Ticket := 0;
         Accepted := True;
      end;
   end Recycle_Table_Ledger;
   procedure Invalidate_Update (OK : out Boolean);
   function Captured_Mapping_Ready return Boolean is
      Ticket : Application_Buffers.Ticket;
   begin
      if not Removal_Capture.Ready or else not In_Place_Active or else
        not Update_Exclusive or else Removal_Capture.Index /= Update_Index or else
        Removal_Capture.Session /= Update_Session or else
        Removal_Capture.Identity /= Update_Identity or else
        not Application_VM.Sealed (Private_Contexts (Update_Index).Source) or else
        Removal_Capture.Root /= Application_VM.Root_DMA (Private_Contexts (Update_Index).Source) or else
        Removal_Capture.Revision /= Application_VM.Revision (Private_Contexts (Update_Index).Source)
      then return False; end if;
      Ticket := Current_Table_Ticket (Update_Index);
      if Ticket /= Removal_Capture.Ticket then return False; end if;
      if Ticket = 0 then
         return Removal_Capture.Generation = Private_Contexts (Update_Index).Table_Generation;
      end if;
      declare
         Slot : constant Intel_GPU_Buffer_Backing.Slot := Application_Buffers.Ticket_Slot (Ticket);
         Saved : constant Replacement_Record := Replacement_Records.Get (Replacement_Tables, Slot);
      begin
         return Saved.Ticket = Ticket and then Saved.Session = Update_Session and then
           not Saved.Superseded and then Application_State.Has_Update (Slot) and then
           Saved.Root = Removal_Capture.Root and then
           Application_State.Updates (Slot).Table_Generation = Removal_Capture.Generation;
      end;
   end Captured_Mapping_Ready;
   function Captured_Table_Mapping (Page : Positive)
     return Intel_GPU_Table_Provenance.Mapping is
      M : Intel_GPU_Table_Provenance.Mapping;
   begin
      if not Captured_Mapping_Ready or else
        Page > Application_VM.Used (Private_Contexts (Update_Index).Source)
      then return (others => 0); end if;
      if Removal_Capture.Ticket = 0 then
         M := Initial_Table_Mapping (Update_Index, Update_Session, Page);
      else
         M := Replacement_Table_Mapping
           (Application_Buffers.Ticket_Slot (Removal_Capture.Ticket), Update_Session, Page);
      end if;
      -- A resolver can lose authority. Recheck the captured incarnation and
      -- generation after it returns, not merely the expected physical address.
      if M.Ticket = 0 or else not Captured_Mapping_Ready or else
        M.DMA /= Application_VM.Page_DMA (Private_Contexts (Update_Index).Source, Page)
      then return (others => 0); end if;
      return M;
   end Captured_Table_Mapping;
   function Captured_Leaf_Mapping (Table_DMA : Unsigned_64)
     return Intel_GPU_Table_Provenance.Mapping is
   begin
      if not Captured_Mapping_Ready then return (others => 0); end if;
      for P in 2 .. Application_VM.Used (Private_Contexts (Update_Index).Source) loop
         if Application_VM.Page_DMA (Private_Contexts (Update_Index).Source, P) = Table_DMA and then
           Application_VM.Leaf_Table (Private_Contexts (Update_Index).Source, P)
         then return Captured_Table_Mapping (P); end if;
      end loop;
      return (others => 0);
   end Captured_Leaf_Mapping;
   procedure Remove_Leaf
     (Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean) is
      M : Intel_GPU_Table_Provenance.Mapping;
   begin
      Success := False;
      if not In_Place_Active or else not Update_Exclusive then return; end if;
      M := Captured_Leaf_Mapping (Table_DMA);
      if M.Ticket = 0 then return; end if;
      Preparing_Index := Update_Index;
      Preparing_Session := Update_Session; Preparing_Identity := Update_Identity;
      Live_Images.Remove_Mapped_Leaf
        (Application_Images_State (Update_Index), Private_Contexts (Update_Index).Source,
         (M.CPU, M.DMA), Table_DMA, Index, Expected, Replacement, Success);
      Preparing_Index := 0; Preparing_Session := 0; Preparing_Identity := 0;
   end Remove_Leaf;
   package Live_Removal is new Application_VM.Removal
     (Update_Exclusive, Remove_Leaf, Invalidate_Update);
   Removal_States : array (Private_Contexts'Range) of Live_Removal.Controller;
   function Captured_Data_Page (Ordinal : Positive) return Unsigned_64 is
      Delta_Bytes : constant Unsigned_64 := Unsigned_64 (Ordinal - 1) * 4096;
   begin
      if not Captured_Mapping_Ready or else
        Unsigned_64 (Ordinal) > Removal_Bytes / 4096 or else
        Removal_Offset > Unsigned_64'Last - Delta_Bytes
      then return 0; end if;
      return Intel_GPU_Buffer_Reply.Page_Address
        (Removal_Backing, Removal_Offset + Delta_Bytes);
   end Captured_Data_Page;
   procedure Start_Removal_Stream is new Live_Removal.Start_From_Pages (Captured_Data_Page);
   procedure Insert_Leaf
     (Table_DMA : Unsigned_64; Index : Intel_GPU_ADLN_PPGTT.Table_Index;
      Expected, Replacement : Unsigned_64; Success : out Boolean) is
      M : Intel_GPU_Table_Provenance.Mapping;
   begin
      Success := False;
      if not In_Place_Active or else not In_Place_Inserting or else
        not Update_Exclusive then return; end if;
      M := Captured_Leaf_Mapping (Table_DMA);
      if M.Ticket = 0 then return; end if;
      Preparing_Index := Update_Index;
      Preparing_Session := Update_Session; Preparing_Identity := Update_Identity;
      Live_Images.Insert_Mapped_Leaf
        (Application_Images_State (Update_Index), Private_Contexts (Update_Index).Source,
         (M.CPU, M.DMA), Table_DMA, Index, Expected, Replacement, Success);
      Preparing_Index := 0; Preparing_Session := 0; Preparing_Identity := 0;
   end Insert_Leaf;
   package Live_Insertion is new Application_VM.Insertion
     (Update_Exclusive, Insert_Leaf, Invalidate_Update);
   function Insertion_Can_Reuse_Stream is new Live_Insertion.Can_Reuse_From_Pages (Captured_Data_Page);
   procedure Start_Insertion_Stream is new Live_Insertion.Start_From_Pages (Captured_Data_Page);
   -- One serialized publication receipt, not a per-BO or per-context arena.
   Insertion_State : Live_Insertion.Controller renames Application_State.Insertion;
   Insertion_Metadata_Pending : Boolean := False;
   Insertion_Metadata_Epoch : Unsigned_64 := 0;
   function Insertion_Word_Capacity return Positive is
     (Application_VM.Insertion_Capacity (Insertion_State));
   procedure Extend_Insertion_Words (Base, Bytes : Unsigned_64; OK : out Boolean) is
   begin
      OK := False;
      if Insertion_Metadata_Pending and then In_Place_Active and then
        not Update_Held and then Update_Owner
      then
         Application_VM.Extend_Insertion_Metadata (Insertion_State, Base, Bytes, OK);
      end if;
   end Extend_Insertion_Words;
   package Insertion_Metadata_Growth is new Intel_GPU_Record_Growth
     (Intel_GPU_Metadata_Platform.Storage, Insertion_Word_Capacity, Extend_Insertion_Words);
   Insertion_Metadata : Insertion_Metadata_Growth.Controller;
   use type Insertion_Metadata_Growth.Phase;
   procedure Advance_Insertion_Metadata;
   Directory_Metadata_Pending : Boolean := False;
   Directory_Metadata_Epoch : Unsigned_64 := 0;
   Directory_Metadata_Target : Positive := 1;
   function Directory_Link_Capacity return Positive is
     (if Update_Index not in Private_Contexts'Range then 1 else
      Application_VM.Growth_Capacity (Private_Contexts (Update_Index).Growth));
   procedure Extend_Directory_Links (Base, Bytes : Unsigned_64; OK : out Boolean) is
   begin
      OK := False;
      if Directory_Metadata_Pending and then In_Place_Active and then
        not Update_Held and then Update_Owner and then
        Application_VM.Revision (Private_Contexts (Update_Index).Source) = Directory_Metadata_Epoch
      then
         Application_VM.Extend_Growth_Metadata
           (Private_Contexts (Update_Index).Growth, Base, Bytes, OK);
      end if;
   end Extend_Directory_Links;
   package Directory_Metadata_Growth is new Intel_GPU_Record_Growth
     (Intel_GPU_Metadata_Platform.Storage, Directory_Link_Capacity, Extend_Directory_Links);
   -- Each context owns its stable CPU reservation for its retained receipt.
   -- Never reuse one context's metadata backing for another context's plan.
   Directory_Metadata : array (Private_Contexts'Range) of Directory_Metadata_Growth.Controller;
   use type Directory_Metadata_Growth.Phase;
   procedure Advance_Directory_Metadata;
   procedure Advance_Live_Metadata;
   Leaf_Publication_Started : Boolean := False;
   procedure Capture_Removal
     (Backing : Intel_GPU_Buffer_Reply.Backing;
      GPU, Offset, Bytes, Revision : Unsigned_64; Accepted : out Boolean)
   is
      Ticket : Application_Buffers.Ticket;
   begin
      Accepted := False;
      Removal_Capture := (others => <>);
      if not In_Place_Active or else not Update_Exclusive then return; end if;
      if not Application_VM.Sealed (Private_Contexts (Update_Index).Source) or else
        Application_VM.Used (Private_Contexts (Update_Index).Source) = 0
      then return; end if;
      Ticket := Current_Table_Ticket (Update_Index);
      Removal_Capture :=
        (Ready => True, Index => Update_Index, Session => Update_Session,
         Identity => Update_Identity, Ticket => Ticket,
         Root => Application_VM.Root_DMA (Private_Contexts (Update_Index).Source),
         Revision => Revision, Generation => Private_Contexts (Update_Index).Table_Generation);
      if Ticket /= 0 then
         declare
            Slot : constant Intel_GPU_Buffer_Backing.Slot := Application_Buffers.Ticket_Slot (Ticket);
            Saved : constant Replacement_Record := Replacement_Records.Get (Replacement_Tables, Slot);
         begin
            if Saved.Ticket /= Ticket or else Saved.Session /= Update_Session or else
              Saved.Superseded or else not Application_State.Has_Update (Slot) or else
              Saved.Root /= Application_VM.Root_DMA (Private_Contexts (Update_Index).Source)
            then return; end if;
            Removal_Capture.Generation := Application_State.Updates (Slot).Table_Generation;
         end;
      end if;
      -- Each ledger lookup reauthenticates its exact table-only allocation
      -- slice. A live tree may span several tickets; a single aggregate
      -- backing descriptor is neither necessary nor sufficient authority.
      -- Preflight every live ordinal, but retain no quota-sized address array.
      -- The exact mapping is authenticated again immediately before each write.
      for P in 1 .. Application_VM.Used (Private_Contexts (Update_Index).Source) loop
         declare
            M : constant Intel_GPU_Table_Provenance.Mapping := Captured_Table_Mapping (P);
         begin
            if M.Ticket = 0 then Removal_Capture.Ready := False; return; end if;
         end;
      end loop;
      Removal_Backing := Backing;
      Removal_GPU := GPU; Removal_Offset := Offset; Removal_Bytes := Bytes;
      Removal_Revision := Revision; Removal_Invalidated := False;
      Leaf_Publication_Started := False;
      if In_Place_Inserting then
         if not Insertion_Can_Reuse_Stream (Insertion_State,
           Private_Contexts (Update_Index).Source, Revision, GPU, Natural (Bytes / 4096),
           Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write)
         then Removal_Capture.Ready := False; return; end if;
      end if;
      Accepted := Captured_Mapping_Ready;
   end Capture_Removal;
   procedure Drain_Update (OK : out Boolean) is
   begin
      -- The hold was acquired before asynchronous allocation. The service
      -- blocks all new submissions until finish; previous submits are bounded
      -- synchronous transactions with completion and scheduling disable.
      OK := Update_Exclusive;
   end Drain_Update;
   procedure Publish_Update (OK : out Boolean) is
   begin
      OK := False;
      if not Update_Exclusive then return; end if;
      if In_Place_Active then
         if not In_Place_Inserting then
            Start_Removal_Stream (Removal_States (Update_Index),
              Private_Contexts (Update_Index).Source, Removal_Revision, Removal_GPU,
              Natural (Removal_Bytes / 4096), OK);
            if not OK then return; end if;
            while Live_Removal.Publishing (Removal_States (Update_Index)) loop
               Live_Removal.Step (Removal_States (Update_Index), Private_Contexts (Update_Index).Source);
            end loop;
            OK := Live_Removal.Published (Removal_States (Update_Index));
            return;
         end if;
         Start_Insertion_Stream (Insertion_State,
           Private_Contexts (Update_Index).Source, Removal_Revision, Removal_GPU,
           Natural (Removal_Bytes / 4096), Intel_GPU_ADLN_PPGTT.Write_Back,
           Intel_GPU_ADLN_PPGTT.Read_Write, OK);
         if not OK then return; end if;
         while Live_Insertion.Publishing (Insertion_State) loop
            Live_Insertion.Step (Insertion_State, Private_Contexts (Update_Index).Source);
         end loop;
         OK := Live_Insertion.Published (Insertion_State);
         return;
      end if;
      declare
         Tables : Intel_GPU_Buffer_Reply.Backing renames
           Application_State.Updates (Application_Buffers.Ticket_Slot (Update_Pending)).Tables;
         Ticket : constant Application_Buffers.Ticket := Update_Pending;
         Slot : constant Intel_GPU_Buffer_Backing.Slot := Application_Buffers.Ticket_Slot (Ticket);
         Index : constant Positive := Update_Index;
         Session : constant Unsigned_64 := Update_Session;
         Identity : constant Unsigned_64 := Update_Identity;
         Generation : constant Unsigned_64 := Application_State.Updates (Slot).Table_Generation;
         function Held return Boolean is
           (Update_Exclusive and then Update_Pending = Ticket and then Ticket /= 0
            and then Update_Index = Index and then Update_Session = Session
            and then Update_Identity = Identity and then Application_State.Has_Update (Slot)
            and then Application_State.Updates (Slot).Table_Generation = Generation);
         function Table_Page (Ordinal : Positive) return Application_Images.Tables.Page_Mapping is
            M : Intel_GPU_Table_Provenance.Mapping;
         begin
            if not Held then return (0, 0); end if;
            M := Replacement_Table_Mapping (Slot, Session, Ordinal);
            if not Held or else M.Ticket /= Ticket then return (0, 0); end if;
            return (M.CPU, M.DMA);
         end Table_Page;
         procedure Publish_Tables is new Live_Images.Publish_Tables_From_Mappings (Table_Page);
      begin
         if not Tables.Ready then return; end if;
         Preparing_Index := Update_Index;
         Preparing_Session := Update_Session; Preparing_Identity := Update_Identity;
         Publish_Tables
           (Application_Images_State (Update_Index), Private_Contexts (Update_Index).Source,
            Application_State.Updates (Slot).Candidate,
            Application_VM.Used (Application_State.Updates (Slot).Candidate), OK);
         Preparing_Index := 0; Preparing_Session := 0; Preparing_Identity := 0;
      end;
   end Publish_Update;
   procedure Advance_Update_Publication (Finished, Success : out Boolean) is
   begin
      Finished := True; Success := False;
      if not Update_Exclusive then return; end if;
      if not In_Place_Active then
         Publish_Update (Success);
         return;
      end if;
      if not Leaf_Publication_Started then
         Leaf_Publication_Started := True;
         if not In_Place_Inserting then
            Start_Removal_Stream (Removal_States (Update_Index),
              Private_Contexts (Update_Index).Source, Removal_Revision, Removal_GPU,
              Natural (Removal_Bytes / 4096), Success);
            Finished := not Success;
            return;
         end if;
         Start_Insertion_Stream (Insertion_State,
           Private_Contexts (Update_Index).Source, Removal_Revision, Removal_GPU,
           Natural (Removal_Bytes / 4096), Intel_GPU_ADLN_PPGTT.Write_Back,
           Intel_GPU_ADLN_PPGTT.Read_Write, Success);
         Finished := not Success;
         return;
      end if;
      if In_Place_Inserting then
         Live_Insertion.Step (Insertion_State, Private_Contexts (Update_Index).Source);
         Finished := not Live_Insertion.Publishing (Insertion_State);
         Success := not Finished or else Live_Insertion.Published (Insertion_State);
      else
         Live_Removal.Step (Removal_States (Update_Index), Private_Contexts (Update_Index).Source);
         Finished := not Live_Removal.Publishing (Removal_States (Update_Index));
         Success := not Finished or else Live_Removal.Published (Removal_States (Update_Index));
      end if;
   end Advance_Update_Publication;
   procedure Invalidate_Update (OK : out Boolean) is
      Attempt : Live_TLB.Attempt;
      Status : Live_TLB.Result;
      use type Live_TLB.Result;
   begin
      Live_TLB.Execute (Attempt, Status);
      OK := Status = Live_TLB.Complete;
      if In_Place_Active then Removal_Invalidated := OK; end if;
   end Invalidate_Update;
   procedure Resume_Update (OK : out Boolean) is
   begin
      -- Submit-on-demand: preserve acknowledged disabled state. Release the
      -- software hold only after the coordinator commits and image is adopted.
      OK := Update_Exclusive;
      if OK and then In_Place_Active then
         if In_Place_Inserting then
            Live_Insertion.Commit (Insertion_State,
              Private_Contexts (Update_Index).Source, Removal_Invalidated, OK);
         else
            Live_Removal.Commit (Removal_States (Update_Index),
              Private_Contexts (Update_Index).Source, Removal_Invalidated, OK);
         end if;
      end if;
   end Resume_Update;
   package Live_VM is new Intel_GPU_VM_Update
     (Update_Owner, Drain_Update, Publish_Update, Invalidate_Update, Resume_Update);
   procedure Advance_Live_Update is new Live_VM.Advance (Advance_Update_Publication);
   Live_VM_States : array (Private_Contexts'Range) of Live_VM.State;
   procedure Report_Closed_Buffer (Session, ID : Unsigned_64) is
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
      Drained : constant Boolean := Application_Work_Drained (Session);
      CPU_State : constant Application_Maps.Retirement_State :=
        Application_Maps.Observe_Buffer_Retirement (Application_Map_State, Session, ID);
      Checked, Disjoint : Boolean := False;
      Generation : Unsigned_64 := 0;
   begin
      if Drained and then Stored /= 0
      then
         declare Index : constant Positive := Positive (Stored); begin
            Checked := Live_VM.Can_Submit (Live_VM_States (Index));
            if Checked then
               Generation := Live_VM.Generation (Live_VM_States (Index));
               -- Finish_VM_Update adopts this image only after publication,
               -- invalidation and commit. Deferred/failed updates are excluded
               -- by Work_Drained and the coordinator state above.
               Disjoint := Application_Binding.Closed_Buffer_Disjoint
                 (Application_Buffer_State, Private_Contexts (Index).Source, Session, ID);
            end if;
         end;
      end if;
      -- Read-only evidence before the reclamation coordinator runs. These
      -- observations alone do not release storage or recycle IDs.
      Publish_Snapshot ("intel-gpu: closed BO=" & Unsigned_64'Image (ID) &
        " CPU=" & Application_Maps.Retirement_State'Image (CPU_State) &
        " GPU-drained=" & Boolean'Image (Drained), CuBit.Log_Records.Debug);
      Publish_Snapshot ("intel-gpu: closed BO VM checked=" & Boolean'Image (Checked) &
        " generation=" & Unsigned_64'Image (Generation) &
        " disjoint=" & Boolean'Image (Disjoint) & " (BACKING RETAINED)",
        CuBit.Log_Records.Debug);
   end Report_Closed_Buffer;
   procedure Finish_Buffer_Retirement is
      Accepted : Boolean := False;
      Recycling : Boolean := False;
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      if Buffer_Retirement_Pending = 0 then return; end if;
      if Buffer_Retirement_Is_Context then
         Finish_Context_Retirement;
         return;
      end if;
      if Buffer_Retirement_Is_Closed_Table then
         Finish_Closed_Table_Retirement;
         return;
      end if;
      if Buffer_Retirement_Is_Teardown then
         Finish_Teardown_Buffer_Retirement;
         return;
      end if;
      if Buffer_Memory.Retirement_Confirmed
        (Buffer_Pool, Application_Buffers.Ticket_Slot (Buffer_Retirement_Pending),
         Application_Buffers.Ticket_Generation (Buffer_Retirement_Pending)) and then
        not Runtime_Fault and then
        Application_Session (Buffer_Retirement_Sender, Buffer_Retirement_Stamp) =
          Buffer_Retirement_Session
      then
         if Buffer_Retirement_Is_Private then
            declare
               Slot : constant Intel_GPU_Buffer_Backing.Slot :=
                 Application_Buffers.Ticket_Slot (Buffer_Retirement_Pending);
               Saved : constant Replacement_Record :=
                 Replacement_Records.Get (Replacement_Tables, Slot);
            begin
               if Saved.Ticket = Buffer_Retirement_Pending and then Saved.Superseded and then
                 Saved.Session = Buffer_Retirement_Session
               then
                  if Recycle_In_Progress then
                     Accepted := True;
                  else
                     Live_Snapshots.Forget_Retired
                       (Application_State.Updates (Slot).Candidate, Saved.Revision, Saved.Root, True, Accepted);
                  end if;
                  if Accepted then
                     Recycle_Table_Ledger (Slot, Saved.Session, Saved.Ticket, Accepted, Recycling);
                     if Recycling then return; end if;
                  end if;
                  if Accepted then
                     Application_Buffers.Acknowledge_Private_Retirement
                       (Application_Buffer_State, Saved.Session, Saved.Ticket, True, Accepted);
                  end if;
                  if Accepted then
                     Application_State.Updates (Slot).Tables := (Ready => False);
                     Replacement_Records.Put (Replacement_Tables, Slot, (others => <>));
                  end if;
               end if;
            end;
         else
            Application_Buffers.Acknowledge_Retirement
              (Application_Buffer_State, Buffer_Retirement_Session,
               Buffer_Retirement_Pending, True, Accepted);
         end if;
      end if;
      if not Accepted then
         Runtime_Fault := True;
         Buffer_Memory.Cancel (Buffer_Pool);
         Application_Buffers.Quarantine (Application_Buffer_State);
      end if;
      Response.tag := (Application_Buffers.Label, 4, 0, 0);
      Response.words := [(if Accepted then Application_Buffers.OK else Application_Buffers.Unavailable), 1, 0, 0];
      if Buffer_Retirement_Has_Reply then
         Delivered := replyCap (Application_Reply_Slot, Response);
         if Delivered /= 1 then
            -- This reply contains only close status, not a new handle/grant.
            -- A departed caller cannot undo an exact supervisor retirement
            -- acknowledgement or quarantine other sessions. replyCap consumes
            -- its one-use slot even on delivery failure; never replay it.
            -- Uncertain retirement was already quarantined above.
            Publish_Snapshot ("intel-gpu: closed BO reply unavailable; no replay");
         end if;
      end if;
      Publish_Snapshot ((if Buffer_Retirement_Is_Private then "intel-gpu: private table retirement acknowledged="
                        else "intel-gpu: closed BO retirement acknowledged=") & Boolean'Image (Accepted) &
        " ticket=" & Unsigned_64'Image (Buffer_Retirement_Pending),
        (if Accepted then CuBit.Log_Records.Debug else CuBit.Log_Records.Error));
      Buffer_Retirement_Pending := 0;
      Buffer_Retirement_Has_Reply := False;
      Buffer_Retirement_Is_Private := False;
   end Finish_Buffer_Retirement;
   function Try_Retire_Closed_Buffer
     (From : ProcessID; Msg : Message; With_Reply : Boolean := True) return Boolean is
      Session : constant Unsigned_64 := Application_Session (Unsigned_64 (From), Msg.authorityTag);
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
      Started : Boolean;
      use type Application_Maps.Retirement_State;
   begin
      if not Render_Backend_Ready or else not Application_Work_Drained (Session) or else
        Stored = 0 or else
        Application_Maps.Observe_Buffer_Retirement
          (Application_Map_State, Session, Msg.words (2)) /= Application_Maps.Clear
      then return False; end if;
      for I in 1 .. Context_Pool.Count (Contexts) loop
         if not Context_Life.Scheduling_Stopped
           (Context_Pool.State (Contexts, First_Context_ID + Unsigned_32 (I - 1)))
         then return False; end if;
      end loop;
      declare Index : constant Positive := Positive (Stored); begin
         if not Live_VM.Can_Submit (Live_VM_States (Index)) or else
           not Application_Binding.Closed_Buffer_Disjoint
             (Application_Buffer_State, Private_Contexts (Index).Source, Session, Msg.words (2))
         then return False; end if;
      end;
      for Slot in 1 .. Application_Buffers.Committed_Slots (Application_Buffer_State) loop
         declare
            Candidate : constant Application_Buffers.Closed_Allocation :=
              Application_Buffers.Closed_At (Application_Buffer_State, Slot);
         begin
            if Candidate.Ready and then Candidate.Session = Session and then
              Unsigned_64 (Candidate.Handle) = Msg.words (2) and then
              Application_Buffers.Can_Retire (Application_Buffer_State, Session, Candidate.ID)
            then
               if With_Reply and then
                 saveReplyCap (Unsigned_64 (Application_Reply_Slot)) /= 1 then return False; end if;
               Buffer_Retirement_Has_Reply := With_Reply;
               Buffer_Retirement_Is_Private := False;
               Buffer_Retirement_Pending := Candidate.ID;
               Buffer_Retirement_Session := Session;
               Buffer_Retirement_Sender := Unsigned_64 (From);
               Buffer_Retirement_Stamp := Msg.authorityTag;
               Buffer_Memory.Retire (Buffer_Pool, Slot, Candidate.Generation, True, Started);
               if not Started then
                  Buffer_Memory.Cancel (Buffer_Pool);
                  Finish_Buffer_Retirement;
               end if;
               return True;
            end if;
         end;
      end loop;
      return False;
   end Try_Retire_Closed_Buffer;
   function Revisit_Deferred_Close
     (Slot : Deferred_Retirement.Slot; Saved : Deferred_Retirement.Candidate)
      return Deferred_Retirement.Outcome is
      Candidate : constant Application_Buffers.Closed_Allocation :=
        Application_Buffers.Closed_At (Application_Buffer_State, Slot);
      Envelope : Message := NULL_MESSAGE;
   begin
      if Application_Session (Saved.Sender, Saved.Stamp) /= Saved.Session then
         return Deferred_Retirement.Discarded; -- retain backing for session teardown
      end if;
      -- Closed_At can be temporarily unavailable during another allocation.
      if not Candidate.Ready then return Deferred_Retirement.Waiting; end if;
      if Candidate.ID /= Saved.Ticket or else Candidate.Session /= Saved.Session or else
        Unsigned_64 (Candidate.Handle) /= Saved.Handle
      then
         return Deferred_Retirement.Discarded;
      end if;
      -- Internal preflight input, never a received IPC or reply authority.
      Envelope.authorityTag := Saved.Stamp;
      Envelope.words (2) := Saved.Handle;
      if Try_Retire_Closed_Buffer (ProcessID (Saved.Sender), Envelope, With_Reply => False) then
         return Deferred_Retirement.Submitted;
      end if;
      return Deferred_Retirement.Waiting;
   end Revisit_Deferred_Close;
   procedure Poll_Deferred_Closes is new Deferred_Retirement.Poll (Revisit_Deferred_Close);
   procedure Poll_Table_Retirement is
      Slot : constant Intel_GPU_Buffer_Backing.Slot := Next_Table_Retirement;
      Saved : constant Replacement_Record :=
        Replacement_Records.Get (Replacement_Tables, Slot);
      Backing : constant Intel_GPU_Buffer_Reply.Backing :=
        (if Application_State.Has_Update (Slot) then Application_State.Updates (Slot).Tables
         else (Ready => False));
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Saved.Session);
      Started : Boolean;
   begin
      Next_Table_Retirement :=
        (if Slot = Positive'Min
          (Application_Buffers.Committed_Slots (Application_Buffer_State),
           Replacement_Records.Capacity (Replacement_Tables)) then 1 else Slot + 1);
      if Saved.Ticket = 0 or else not Saved.Superseded then return; end if;
      -- Diagnostic observation only: retain the last actual superseded
      -- candidate rather than overwriting it on irrelevant/empty slots.
      Last_Table_Retirement_Slot := Slot;
      Last_Table_Retirement := Readiness;
      if not Render_Backend_Ready or else not Application_Work_Drained (Saved.Session)
      then return; end if;
      Last_Table_Retirement := Identity;
      if Stored = 0 or else
        Application_Session (Saved.Sender, Saved.Stamp) /= Saved.Session or else
        Application_Buffers.Ticket_Session (Application_Buffer_State, Saved.Ticket) /= Saved.Session
      then return; end if;
      Last_Table_Retirement := Backing_Validation;
      if not Intel_GPU_Buffer_Reply.Valid (Backing) or else
        Application_VM.Revision (Application_State.Updates (Slot).Candidate) /= Saved.Revision or else
        Application_VM.Root_DMA (Application_State.Updates (Slot).Candidate) /= Saved.Root or else
        not Application_VM.Sealed (Application_State.Updates (Slot).Candidate)
      then return; end if;
      Last_Table_Retirement := Current_VM;
      if Current_Table_Ticket (Positive (Stored)) = Saved.Ticket or else
        not Live_VM.Can_Submit (Live_VM_States (Positive (Stored)))
      then return; end if;
      Last_Table_Retirement := Contexts_Running;
      for I in 1 .. Context_Pool.Count (Contexts) loop
         if not Context_Life.Scheduling_Stopped
           (Context_Pool.State (Contexts, First_Context_ID + Unsigned_32 (I - 1)))
         then return; end if;
      end loop;
      -- Private tables are never exported as CPU grants/app BO handles. Check
      -- every initialized current VM for physical aliases, not only its owner.
      -- Only exact, acknowledged offline receipt disposal can exempt a source.
      -- Failed or merely unsealed preparation must still block retirement.
      Last_Table_Retirement := Alias_Check;
      -- Per-image alias checks now run in Prepare_Table_Retirement after the
      -- pending gate closes admission, one retained image per service turn.
      Buffer_Retirement_Pending := Saved.Ticket;
      Buffer_Retirement_Session := Saved.Session;
      Buffer_Retirement_Sender := Saved.Sender;
      Buffer_Retirement_Stamp := Saved.Stamp;
      Buffer_Retirement_Has_Reply := False;
      Buffer_Retirement_Is_Private := True;
      Begin_Table_Recycling (Slot, Saved.Session, Saved.Ticket, Started);
      Last_Table_Retirement :=
        (if Started then Retirement_Queued else Retirement_Rejected);
      if not Started then
         Buffer_Memory.Cancel (Buffer_Pool);
         Finish_Buffer_Retirement;
      end if;
   end Poll_Table_Retirement;
   function Read_Replacement_Page (Page : Application_VM.Page_Number) return Unsigned_64 is
      M : Intel_GPU_Table_Provenance.Mapping;
   begin
      if not Update_Exclusive or else Update_Pending = 0 or else
        Page > Update_Table_Pages then return 0; end if;
      M := Replacement_Table_Mapping
        (Application_Buffers.Ticket_Slot (Update_Pending), Update_Session, Page);
      return (if Update_Exclusive and then M.Ticket /= 0 then M.DMA else 0);
   end Read_Replacement_Page;
   procedure Execute_Update is new Application_Binding.Handle_Update_From_Pages
     (Read_Replacement_Page, Live_VM);
   procedure Begin_Removal is new Application_Binding.Begin_In_Place (Live_VM, True, Capture_Removal);
   procedure Begin_Insertion is new Application_Binding.Begin_In_Place (Live_VM, False, Capture_Removal);
   procedure Finish_In_Place_Request is new Application_Binding.Finish_In_Place (Live_VM);
   procedure Fail_Update is
   begin
      if Update_Index in Private_Contexts'Range then
         Live_VM.Fail (Live_VM_States (Update_Index));
         Intel_GPU_Render_Control.Reject_Delivery
           (Render_Admission, Update_Identity, Update_Session);
         Retire_Application_Resources (Update_Session);
      end if;
   end Fail_Update;
   procedure Reply_In_Place (Words : in out Application_Buffers.Words) is
      Released : Boolean := False;
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      if Words (0) = Application_Buffers.OK and then Update_Exclusive then
         Context_Pool.Release_Work (Contexts, Update_Context, Released, Keep_Disabled => True);
      end if;
      if not Released then
         Words := [Application_Buffers.Unavailable, 1, 0, 0];
         Publish_Snapshot ("intel-gpu: in-place " &
           (if In_Place_Inserting then "bind" else "unbind") & " failed; backing retained");
         Fail_Update;
      end if;
      Response.tag := (Application_Binding.Update_Label, 4, 0, 0);
      Response.words := [Words (0), Words (1), Words (2), Words (3)];
      Delivered := replyCap (Application_Reply_Slot, Response);
      if Delivered /= 1 then Fail_Update; end if;
      In_Place_Active := False; Update_Held := False; Update_Index := 0;
   end Reply_In_Place;
   procedure Advance_In_Place is
      Finished : Boolean;
      Status : Live_VM.Result;
      Words : Application_Buffers.Words := [Application_Buffers.Unavailable, 1, 0, 0];
      use type Live_VM.Result;
   begin
      if not In_Place_Active then return; end if;
      if Live_Metadata_Pending then Advance_Live_Metadata; return; end if;
      if Insertion_Metadata_Pending then Advance_Insertion_Metadata; return; end if;
      if Directory_Metadata_Pending then Advance_Directory_Metadata; return; end if;
      if Update_Index not in Private_Contexts'Range then Reply_In_Place (Words); return; end if;
      Advance_Live_Update (Live_VM_States (Update_Index), Finished, Status);
      if not Finished then return; end if;
      if Status = Live_VM.Complete then
         Finish_In_Place_Request (Application_Buffer_State, Live_VM_States (Update_Index),
           Update_Session, Update_Sender, Update_Stamp, Shift_Right (Update_Request.words (0), 32), Words);
      end if;
      Reply_In_Place (Words);
   end Advance_In_Place;
   procedure Finish_Directory_Update (Backing : Intel_GPU_Buffer_Reply.Backing) is
      OK, Consumed, Started : Boolean := False;
      Words : Application_Buffers.Words := [Application_Buffers.Unavailable, 1, 0, 0];
      At_Phase : constant Directory_Phase := Directory_Update;
      function New_Directory_Page (Ordinal : Positive) return Unsigned_64 is
         M : Intel_GPU_Table_Provenance.Mapping;
      begin
         if not Directory_Exclusive or else Directory_First_ID = 0 or else
           Ordinal > Update_Table_Pages or else
           Directory_First_ID > Natural'Last - (Ordinal - 1)
         then return 0; end if;
         M := Table_Authority.Lookup
           (Private_Contexts (Update_Index).Table_Owners, Update_Session,
            Private_Contexts (Update_Index).Table_Generation,
            Directory_First_ID + (Ordinal - 1));
         if not Directory_Exclusive or else M.Ticket /= Update_Pending
         then return 0; end if;
         return M.DMA;
      end New_Directory_Page;
      procedure Start_Directory_Stream is new Directory_Writer.Start_From_Pages (New_Directory_Page);
   begin
      if Directory_Update = No_Directory_Update then return; end if;
      if Directory_Exclusive and then Intel_GPU_Buffer_Reply.Valid (Backing) and then
        Update_Table_Pages /= 0 and then
        Backing.Bytes = Unsigned_64 (Update_Table_Pages) * 4096
      then
         case Directory_Update is
            when Allocate_Directories =>
               Table_Allocations.Install (Table_Backing_Registry,
                 Application_Buffers.Ticket_Slot (Update_Pending), Update_Session,
                 Update_Pending, Table_Allocations.Incremental_Tables, Backing, OK);
               if OK then
                  Table_Authority.Rearm (Table_Appends (Update_Index),
                    Private_Contexts (Update_Index).Table_Owners, OK);
               end if;
               if OK then
                  Table_Authority.Begin_Append (Table_Appends (Update_Index),
                    Private_Contexts (Update_Index).Table_Owners, Update_Session,
                    Private_Contexts (Update_Index).Table_Generation,
                    Update_Pending, 0, Update_Table_Pages, OK);
               end if;
               if OK then Directory_Update := Register_Directories; return; end if;
            when Register_Directories =>
               Table_Authority.Step (Table_Appends (Update_Index),
                 Private_Contexts (Update_Index).Table_Owners);
               if Table_Authority.Status (Table_Appends (Update_Index)) = Table_Authority.Appending
               then return; end if;
               if Table_Authority.Status (Table_Appends (Update_Index)) = Table_Authority.Appended then
                  Directory_First_ID := Table_Authority.First_ID (Table_Appends (Update_Index));
                  Directory_Update := Start_Directories; return;
               end if;
            when Start_Directories =>
               declare
                  Source : Application_VM.Image renames Private_Contexts (Update_Index).Source;
                  Receipt : Directory_Writer.State renames Private_Contexts (Update_Index).Growth;
                  Root : constant Application_Images.Tables.Page_Mapping :=
                    Application_Images.Retained_Root (Application_Images_State (Update_Index));
               begin
                  OK := Root.DMA = Application_VM.Root_DMA (Source) and then Directory_Owned (Root.DMA);
                  if OK and then Directory_Writer.Attempted (Receipt) then
                     Directory_Writer.Rearm (Receipt, Source, Root.DMA, OK);
                  end if;
                  if OK then
                     Start_Directory_Stream (Receipt, Source,
                       Update_Request.words (2), Update_Request.words (3), Root.DMA,
                       Update_Table_Pages, OK);
                  end if;
                  if OK then Directory_Update := Publish_Directories; return; end if;
               end;
            when Publish_Directories =>
               Directory_Writer.Step (Private_Contexts (Update_Index).Growth,
                 Private_Contexts (Update_Index).Source);
               if Directory_Writer.Pending (Private_Contexts (Update_Index).Growth) then return; end if;
               if Directory_Writer.Published (Private_Contexts (Update_Index).Growth) then
                  Directory_Update := Invalidate_Directories; return;
               end if;
            when Invalidate_Directories =>
               -- Fresh, transaction-local receipt; never inherit the leaf
               -- invalidation bit or a previous directory transaction's bit.
               Invalidate_Update (Directory_Invalidated);
               if Directory_Invalidated then Directory_Update := Commit_Directories; return; end if;
            when Commit_Directories =>
               Directory_Writer.Commit (Private_Contexts (Update_Index).Growth,
                 Private_Contexts (Update_Index).Source, OK);
               if OK then
                  for P in 1 .. Update_Table_Pages loop
                     Application_State.Table_References.Put
                       (Private_Contexts (Update_Index).Table_IDs,
                        Private_Contexts (Update_Index).Table_Generation,
                        Directory_Previous_Used + P, Directory_First_ID + P - 1, OK);
                     exit when not OK;
                  end loop;
                  if OK then
                  Application_Buffers.Finish_Private (Application_Buffer_State, Update_Pending, Consumed);
                  if Consumed then
                     Publish_Snapshot ("intel-gpu: incremental directories committed pages=" &
                       Natural'Image (Update_Table_Pages) & " (leaf bind pending)");
                     -- Keep held work, request and reply. The public VM update
                     -- generation advances once, after the separate leaf bind.
                     In_Place_Active := True;
                     Directory_Update := No_Directory_Update;
                     Update_Pending := 0; Update_Table_Pages := 0;
                     Begin_Insertion (Application_Buffer_State,
                       Private_Contexts (Update_Index).Source, Live_VM_States (Update_Index),
                       Update_Session, Update_Sender, Update_Stamp, Update_Request.tag.label,
                       Update_Request.tag.length, Update_Request.tag.flags, Update_Request.tag.reserved,
                       [Update_Request.words (0), Update_Request.words (1),
                        Update_Request.words (2), Update_Request.words (3)], Words, Started);
                     if not Started then Reply_In_Place (Words); end if;
                     return;
                  end if;
                  end if;
               end if;
            when No_Directory_Update => null;
         end case;
      end if;
      Publish_Snapshot ("intel-gpu: incremental directories failed stage=" &
        Directory_Phase'Image (At_Phase) & "; backing retained");
      -- No rollback, allocation reuse, or replacement fallback after failure.
      -- Partial ledger installation or uncertain publication quarantines the
      -- session; only confirmed full consumer retirement can release backing.
      Application_Buffers.Finish_Private (Application_Buffer_State, Update_Pending, Consumed);
      Fail_Update;
      Directory_Update := No_Directory_Update;
      Update_Pending := 0; Update_Table_Pages := 0;
      Reply_In_Place (Words);
   end Finish_Directory_Update;
   procedure Finish_VM_Update (Backing : Intel_GPU_Buffer_Reply.Backing) is
      Response : Application_Buffers.Words := [Application_Buffers.Unavailable, 1, 0, 0];
      Message_Out : Message := NULL_MESSAGE;
      OK, Consumed : Boolean;
      Delivered : Unsigned_64;
      type Finish_Stage is (Storage_Check, Execute, Adopt, Release_Work, Finish_Ticket);
      At_Stage : Finish_Stage := Storage_Check;
   begin
      if Update_Pending = 0 then return; end if;
      if Offline_Bind_State /= No_Offline_Bind then
         Finish_Offline_Bind (Backing); return;
      end if;
      if Directory_Update /= No_Directory_Update then
         Finish_Directory_Update (Backing); return;
      end if;
      if Application_State.Has_Update (Application_Buffers.Ticket_Slot (Update_Pending)) then
         Application_State.Updates (Application_Buffers.Ticket_Slot (Update_Pending)).Tables := Backing;
      end if;
      if Application_State.Has_Update (Application_Buffers.Ticket_Slot (Update_Pending)) and then
        Backing.Ready and then Update_Table_Pages /= 0 and then
        Backing.Bytes = Unsigned_64 (Update_Table_Pages) * 4096 and then
        Update_Exclusive
      then
         declare
            Item : Application_State.Update_Record renames Application_State.Updates
              (Application_Buffers.Ticket_Slot (Update_Pending)).all;
         begin
            -- BEGIN STEPPED REPLACEMENT REGISTRATION
            OK := Intel_GPU_Table_Provenance.Count (Item.Table_Owners) <= Update_Table_Pages;
            if OK and then Intel_GPU_Table_Provenance.Count (Item.Table_Owners) = 0 then
               Table_Allocations.Install (Table_Backing_Registry,
                 Application_Buffers.Ticket_Slot (Update_Pending), Update_Session, Update_Pending,
                 Table_Allocations.Replacement_Image, Backing, OK);
            end if;
            if OK and then Intel_GPU_Table_Provenance.Count (Item.Table_Owners) < Update_Table_Pages then
               declare
                  P : constant Positive := Intel_GPU_Table_Provenance.Count (Item.Table_Owners) + 1;
               begin
                  Table_Authority.Install (Item.Table_Owners, Update_Session,
                    Item.Table_Generation, P, Update_Pending, Unsigned_64 (P - 1) * 4096, OK);
                  if OK then
                     Application_State.Table_References.Put
                       (Item.Table_IDs, Item.Table_Generation, P, P, OK);
                     -- Keep Update_Pending, held work and reply authority.
                     -- The retained Buffer_Memory result is revisited next
                     -- turn; no GPU publication occurs during registration.
                     if OK then return; end if;
                  end if;
               end;
            end if;
            -- END STEPPED REPLACEMENT REGISTRATION
         end;
         if OK then
         At_Stage := Execute;
         Execute_Update
           (Application_Buffer_State, Private_Contexts (Update_Index).Source,
            Application_State.Updates (Application_Buffers.Ticket_Slot (Update_Pending)).Candidate, Update_Table_Pages,
            Live_VM_States (Update_Index), Update_Session, Update_Sender, Update_Stamp,
            Update_Request.tag.label, Update_Request.tag.length, Update_Request.tag.flags,
            Update_Request.tag.reserved,
            [Update_Request.words (0), Update_Request.words (1),
             Update_Request.words (2), Update_Request.words (3)], Response);
         if Response (0) = Application_Buffers.OK then
            At_Stage := Adopt;
            Live_Snapshots.Adopt_Committed
              (Private_Contexts (Update_Index).Source,
               Application_State.Updates (Application_Buffers.Ticket_Slot (Update_Pending)).Candidate, OK);
            if OK and then Update_Exclusive then
               At_Stage := Release_Work;
               Context_Pool.Release_Work (Contexts, Update_Context, OK, Keep_Disabled => True);
            else OK := False; end if;
            if not OK then Response := [Application_Buffers.Unavailable, 1, 0, 0]; end if;
         end if;
         end if;
      end if;
      if Response (0) /= Application_Buffers.OK then
         Publish_Snapshot ("intel-gpu: VM update failed stage=" & Finish_Stage'Image (At_Stage) &
           " backing=" & Boolean'Image (Backing.Ready) &
           " handle=" & Unsigned_64'Image (Update_Request.words (1) and 16#FFFF_FFFF#));
         if not Backing.Ready then
            -- Last observed allocator stage, not by itself a denial reason:
            -- Start can refuse before changing the stage, and image-storage
            -- failure can prevent Start altogether.
            Publish_Snapshot ("intel-gpu: VM backing pool-stage=" &
              Buffer_Memory.Allocation_Stage'Image (Buffer_Memory.Last_Stage (Buffer_Pool)) &
              " slot=" & Positive'Image (Application_Buffers.Ticket_Slot (Update_Pending)) &
              " records=" & Positive'Image (Buffer_Memory.Record_Capacity (Buffer_Pool)));
            Publish_Snapshot ("intel-gpu: last table retirement checkpoint=" &
              Table_Retirement_Checkpoint'Image (Last_Table_Retirement) &
              " slot=" & Natural'Image (Last_Table_Retirement_Slot) &
              " (observation; NOT retirement confirmation)");
         end if;
         Fail_Update;
      end if;
      Application_Buffers.Finish_Private (Application_Buffer_State, Update_Pending, Consumed);
      if not Consumed then
         Publish_Snapshot ("intel-gpu: VM update failed stage=FINISH_TICKET");
         Fail_Update; Response := [Application_Buffers.Unavailable, 1, 0, 0];
      end if;
      Message_Out.tag := (Application_Binding.Update_Label, 4, 0, 0);
      Message_Out.words := [Response (0), Response (1), Response (2), Response (3)];
      Delivered := replyCap (Application_Reply_Slot, Message_Out);
      if Delivered /= 1 then Fail_Update; end if;
      if Delivered = 1 and then Response (0) = Application_Buffers.OK then
         declare
            Slot : constant Intel_GPU_Buffer_Backing.Slot := Application_Buffers.Ticket_Slot (Update_Pending);
            Previous : constant Application_Buffers.Ticket := Current_Table_Ticket (Update_Index);
         begin
            if Previous /= 0 then
               declare
                  Old_Slot : constant Intel_GPU_Buffer_Backing.Slot :=
                    Application_Buffers.Ticket_Slot (Previous);
                  Old : constant Replacement_Record :=
                    Replacement_Records.Get (Replacement_Tables, Old_Slot);
               begin
                  if Old.Ticket /= Previous or else Old.Session /= Update_Session then
                     Fail_Update; Runtime_Fault := True;
                  else
                     Table_Allocations.Revoke
                       (Table_Backing_Registry, Old_Slot, Old.Session, Old.Ticket, OK);
                     if OK then
                        Replacement_Records.Put
                          (Replacement_Tables, Old_Slot, (Old with delta Superseded => True));
                     else
                        Fail_Update; Runtime_Fault := True;
                     end if;
                  end if;
               end;
            end if;
            Replacement_Records.Put (Replacement_Tables, Slot,
              (Update_Pending, Update_Session, Update_Sender, Update_Stamp,
               Application_VM.Revision (Application_State.Updates (Slot).Candidate),
               Application_VM.Root_DMA (Application_State.Updates (Slot).Candidate), False));
            Current_Table_Ticket (Update_Index) := Update_Pending;
         end;
      end if;
      Update_Pending := 0; Update_Table_Pages := 0; Update_Held := False; Update_Index := 0;
   end Finish_VM_Update;
   procedure Handle_VM_Update (From : ProcessID; Msg : Message; Saved_Reply : Boolean := False) is
      Session : constant Unsigned_64 := Application_Session (Unsigned_64 (From), Msg.authorityTag);
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
      Response : Message := NULL_MESSAGE;
      Code : Unsigned_64 := Application_Buffers.Denied;
      Status : Application_Binding.Preparation_Result;
      use type Application_Binding.Preparation_Result;
      Started, Consumed : Boolean;
      Delivered : Unsigned_64;
      type Admission_Stage is (Session_Check, Busy_Check, Owner_Check,
        Request_Check, Reserve_Tables, Hold_Context, Save_Reply);
      At_Stage : Admission_Stage := Session_Check;
      Reply_Saved : Boolean := Saved_Reply;
      function Save_Update_Reply return Boolean is
      begin
         if not Reply_Saved then
            Reply_Saved := saveReplyCap (Unsigned_64 (Application_Reply_Slot)) = 1;
         end if;
         return Reply_Saved;
      end Save_Update_Reply;
      function Send_Update_Reply (Value : Message) return Unsigned_64 is
        (if Reply_Saved then replyCap (Application_Reply_Slot, Value) else reply (From, Value));
   begin
      if Stored /= 0 then
         At_Stage := Busy_Check;
         Code := Application_Buffers.Unavailable;
         if Buffer_Retirement_Pending = 0 and then Update_Pending = 0 and then
           not In_Place_Active and then Application_Pending = 0 and then Private_Pending = 0 then
            Update_Index := Stored; Update_Session := Session;
            Update_Sender := Unsigned_64 (From); Update_Stamp := Msg.authorityTag;
            Update_Identity := Intel_GPU_Render_Control.Recipient_Identity
              (Render_Admission, Update_Sender, Update_Stamp);
            Update_Context := Context_Pool.Session_Context (Contexts, Session);
            At_Stage := Owner_Check;
            if Update_Owner and then Live_VM.Can_Submit (Live_VM_States (Update_Index)) then
               At_Stage := Request_Check;
               Application_Binding.Check_Update_Request
                 (Application_Buffer_State, Private_Contexts (Update_Index).Source,
                  Session, Live_VM.Generation (Live_VM_States (Update_Index)),
                  Update_Sender, Update_Stamp, Msg.tag.label, Msg.tag.length,
                  Msg.tag.flags, Msg.tag.reserved,
                  [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Status);
               Code := (case Status is
                 when Application_Binding.Malformed => Application_Buffers.Bad_Request,
                 when Application_Binding.Request_Denied => Application_Buffers.Denied,
                 when others => Application_Buffers.Unavailable);
               if Status = Application_Binding.Eligible then
                  In_Place_Inserting := (Shift_Right (Msg.words (0), 16) and 16#FFFF#) = 0;
                  if In_Place_Inserting and then
                    Msg.words (3) / 4096 > Unsigned_64 (Insertion_Word_Capacity)
                  then
                     Started := not Saved_Reply;
                     if Started and then Insertion_Metadata_Growth.Snapshot (Insertion_Metadata).State =
                       Insertion_Metadata_Growth.Empty
                     then
                        Insertion_Metadata_Growth.Configure (Insertion_Metadata,
                          Unsigned_64 (Application_State.Table_Pages) * 4096,
                          Application_State.Table_Pages * 512, Started);
                     end if;
                     -- Preserve reply authority before making growth non-idle.
                     -- A failed save leaves the configured controller reusable;
                     -- a later request rejection replies through the saved cap.
                     if Started then Started := Save_Update_Reply; end if;
                     if Started then
                        Insertion_Metadata_Growth.Request (Insertion_Metadata,
                          Positive (Msg.words (3) / 4096), Started);
                     end if;
                     if Started then
                        Update_Request := Msg;
                        Insertion_Metadata_Epoch := Application_VM.Revision (Private_Contexts (Stored).Source);
                        Update_Held := False;
                        In_Place_Active := True; Insertion_Metadata_Pending := True;
                        return;
                     end if;
                     -- No GPU work started. Do not enter insertion without storage.
                     Response.tag := (Application_Binding.Update_Label, 4, 0, 0);
                     Response.words := [Application_Buffers.Unavailable, 1, 0, 0];
                     Delivered := Send_Update_Reply (Response);
                     Update_Index := 0;
                     return;
                  end if;
                  if not In_Place_Inserting or else Live_Insertion.Range_Reusable
                    (Private_Contexts (Update_Index).Source, Msg.words (2), Msg.words (3)) then
                     -- Edit retained leaves: no replacement
                     -- table ticket, supervisor allocation or candidate image.
                     In_Place_Active := True;
                     At_Stage := Hold_Context;
                     Context_Pool.Hold_Work (Contexts, Update_Context, Update_Held);
                     if Update_Exclusive then At_Stage := Save_Reply; end if;
                     if Update_Exclusive and then Save_Update_Reply then
                        declare
                           Words : Application_Buffers.Words;
                        begin
                           Update_Request := Msg;
                           if In_Place_Inserting then
                              Begin_Insertion (Application_Buffer_State,
                                Private_Contexts (Update_Index).Source, Live_VM_States (Update_Index),
                                Session, Update_Sender, Update_Stamp, Msg.tag.label,
                                Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
                                [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Words, Started);
                           else
                              Begin_Removal (Application_Buffer_State,
                             Private_Contexts (Update_Index).Source, Live_VM_States (Update_Index),
                             Session, Update_Sender, Update_Stamp, Msg.tag.label,
                             Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
                             [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Words, Started);
                           end if;
                           if not Started then Reply_In_Place (Words); end if;
                        end;
                        -- Hold, reply capability and captured request survive
                        -- event-loop turns until commit or terminal failure.
                        return;
                     end if;
                     Fail_Update;
                     In_Place_Active := False; Update_Held := False; Update_Index := 0;
                     Response.tag := (Application_Binding.Update_Label, 4, 0, 0);
                     Response.words := [Application_Buffers.Unavailable, 1, 0, 0];
                     Delivered := Send_Update_Reply (Response);
                     return;
                  end if;
                  At_Stage := Reserve_Tables;
                  declare
                     Needed : constant Application_Topology.Requirements :=
                       Application_Topology.Inspect (Private_Contexts (Update_Index).Source,
                                                    Msg.words (2), Msg.words (3));
                     -- Replacement pages have their own ledger. Only an
                     -- incremental initial-root update appends to this one.
                     Provenance_Demand : constant Natural :=
                       Intel_GPU_Table_Provenance.Count (Private_Contexts (Stored).Table_Owners) +
                       (if Current_Table_Ticket (Update_Index) = 0 then Needed.Additional_Tables else 0);
                     use type Application_Topology.Plan_Status;
                  begin
                     Update_Table_Pages := 0;
                     Directory_Update := No_Directory_Update;
                     Directory_First_ID := 0; Directory_Invalidated := False;
                     if Needed.Status = Application_Topology.Ready and then Needed.Fits_Quota and then
                       Needed.Additional_Tables > 0 and then
                       (not Needed.Fits_Reserved or else
                        Needed.Required_Tables > Application_State.Table_References.Capacity
                          (Private_Contexts (Stored).Table_IDs) or else
                        (Current_Table_Ticket (Update_Index) = 0 and then
                         Needed.Additional_Tables > Intel_GPU_Table_Provenance.Capacity
                          (Private_Contexts (Stored).Table_Owners) -
                            Intel_GPU_Table_Provenance.Count (Private_Contexts (Stored).Table_Owners)))
                     then
                        Started := Provenance_Demand <= 32768;
                        if Started then Started := Save_Update_Reply; end if;
                        if Started then
                           Update_Request := Msg;
                           Live_Metadata_Epoch := Application_VM.Revision (Private_Contexts (Stored).Source);
                           Update_Held := False; In_Place_Active := True; Live_Metadata_Pending := True;
                           Request_Context_Metadata
                             (Stored, Needed.Required_Tables, Positive'Max (1, Provenance_Demand), Started);
                           if Started then return; end if;
                           Live_Metadata_Pending := False; In_Place_Active := False;
                        end if;
                        Response.tag := (Application_Binding.Update_Label, 4, 0, 0);
                        Response.words := [Application_Buffers.Unavailable, 1, 0, 0];
                        Delivered := Send_Update_Reply (Response);
                        Update_Index := 0;
                        return;
                     end if;
                     if Needed.Status = Application_Topology.Ready and then Needed.Fits_Reserved then
                        Directory_Previous_Used := Application_VM.Used
                          (Private_Contexts (Update_Index).Source);
                        if Current_Table_Ticket (Update_Index) = 0 and then
                          Needed.Additional_Tables > 0 and then
                          Needed.Additional_Tables <= Intel_GPU_Table_Provenance.Capacity
                            (Private_Contexts (Update_Index).Table_Owners) -
                              Intel_GPU_Table_Provenance.Count (Private_Contexts (Update_Index).Table_Owners)
                        then
                           if Needed.Additional_Tables > Directory_Link_Capacity then
                              Started := True;
                              if Directory_Metadata_Growth.Snapshot (Directory_Metadata (Stored)).State =
                                Directory_Metadata_Growth.Empty
                              then
                                 Directory_Metadata_Growth.Configure (Directory_Metadata (Stored),
                                   Unsigned_64 (Application_State.Table_Pages) * 4096,
                                   Application_State.Table_Pages, Started);
                              end if;
                              -- Insertion metadata may already have saved this reply.
                              -- Save_Update_Reply preserves that original authority.
                              if Started then Started := Save_Update_Reply; end if;
                              if Started then
                                 Directory_Metadata_Growth.Request (Directory_Metadata (Stored),
                                   Needed.Additional_Tables, Started);
                              end if;
                              if Started then
                                 Update_Request := Msg;
                                 Directory_Metadata_Epoch := Application_VM.Revision
                                   (Private_Contexts (Stored).Source);
                                 Directory_Metadata_Target := Needed.Additional_Tables;
                                 Update_Held := False;
                                 In_Place_Active := True; Directory_Metadata_Pending := True;
                                 return;
                              end if;
                              Response.tag := (Application_Binding.Update_Label, 4, 0, 0);
                              Response.words := [Application_Buffers.Unavailable, 1, 0, 0];
                              Delivered := Send_Update_Reply (Response);
                              Update_Index := 0;
                              return;
                           end if;
                           Update_Table_Pages := Needed.Additional_Tables;
                           Directory_Update := Allocate_Directories;
                        else
                           Update_Table_Pages := Needed.Required_Tables;
                        end if;
                        if Update_Table_Pages /= 0 then
                           Application_Buffers.Reserve_Private
                             (Application_Buffer_State, Update_Session, Update_Pending, Reclaimable => True,
                              Kind => (if Directory_Update = Allocate_Directories then
                                Application_Buffers.Incremental_Tables else Application_Buffers.Replacement_Tables),
                              Pages => Update_Table_Pages);
                        end if;
                     end if;
                  end;
                  if Update_Pending /= 0 then
                     At_Stage := Hold_Context;
                     Context_Pool.Hold_Work (Contexts, Update_Context, Update_Held);
                     if Update_Exclusive then At_Stage := Save_Reply; end if;
                     if Update_Exclusive and then Save_Update_Reply then
                        Update_Request := Msg;
                        if Directory_Update = Allocate_Directories then
                           Buffer_Memory.Start (Buffer_Pool,
                             Application_Buffers.Ticket_Slot (Update_Pending), Update_Table_Pages, Started);
                           if not Started then Finish_Directory_Update ((Ready => False)); end if;
                           return;
                        end if;
                        -- The replacement mirrors need the planned topology,
                        -- not eager backing for the entire table quota.
                        Started := False;
                        if Update_Table_Pages /= 0 then
                           Update_Storage.Request (Update_Images,
                             Application_Buffers.Ticket_Slot (Update_Pending),
                             256 * 1024 * 1024, Started,
                             Tables => Update_Table_Pages);
                        end if;
                        Update_Image_Pending := Started;
                        if not Started then Finish_VM_Update ((Ready => False)); end if;
                        return;
                     end if;
                     -- No deferred reply/allocation exists. Keep backing/slot
                     -- retained and retire instead of guessing a safe resume.
                     Fail_Update;
                     Application_Buffers.Finish_Private (Application_Buffer_State, Update_Pending, Consumed);
                     Update_Pending := 0; Update_Held := False;
                  end if;
               end if;
            end if;
            Update_Index := 0;
         end if;
      end if;
      Publish_Snapshot ("intel-gpu: VM update rejected stage=" & Admission_Stage'Image (At_Stage) &
        " status=" & Unsigned_64'Image (Code) &
        " handle=" & Unsigned_64'Image (Msg.words (1) and 16#FFFF_FFFF#));
      Response.tag := (Application_Binding.Update_Label, 4, 0, 0);
      Response.words := [Code, 1, 0, 0];
      Delivered := Send_Update_Reply (Response);
   end Handle_VM_Update;
   procedure Advance_Live_Metadata is
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      if not Live_Metadata_Pending then return; end if;
      if Live_Metadata_Owner then
         Context_Metadata_Growth.Step (Context_Metadata (Update_Index));
         if not Live_Metadata_Owner then null;
         elsif Context_Metadata_Growth.State (Context_Metadata (Update_Index)) = Context_Metadata_Growth.Idle then
            -- Growth only prepares CPU bookkeeping. Re-run normal admission
            -- with the original reply; GPU hold/publication/TLB gates remain
            -- downstream and no table ticket exists during this wait.
            Live_Metadata_Pending := False; In_Place_Active := False;
            Handle_VM_Update (ProcessID (Update_Sender), Update_Request, Saved_Reply => True);
            return;
         elsif Context_Metadata_Growth.State (Context_Metadata (Update_Index)) /= Context_Metadata_Growth.Failed then
            return;
         end if;
      end if;
      Response.tag := (Application_Binding.Update_Label, 4, 0, 0);
      Response.words := [Application_Buffers.Unavailable, 1, 0, 0];
      Fail_Update;
      Delivered := replyCap (Application_Reply_Slot, Response);
      Live_Metadata_Pending := False; In_Place_Active := False;
      Update_Held := False; Update_Index := 0;
   end Advance_Live_Metadata;
   procedure Advance_Directory_Metadata is
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      if not Directory_Metadata_Pending then return; end if;
      if Update_Owner and then not Update_Held and then
        Application_VM.Revision (Private_Contexts (Update_Index).Source) = Directory_Metadata_Epoch
      then
         Directory_Metadata_Growth.Step (Directory_Metadata (Update_Index));
         if not Update_Owner or else Update_Held or else
           Application_VM.Revision (Private_Contexts (Update_Index).Source) /= Directory_Metadata_Epoch
         then null;
         elsif Directory_Metadata_Growth.Snapshot (Directory_Metadata (Update_Index)).State =
           Directory_Metadata_Growth.Idle
         then
            if Directory_Link_Capacity >= Directory_Metadata_Target then
               Directory_Metadata_Pending := False; In_Place_Active := False;
               Handle_VM_Update (ProcessID (Update_Sender), Update_Request, Saved_Reply => True);
               return;
            end if;
         elsif Directory_Metadata_Growth.Snapshot (Directory_Metadata (Update_Index)).State /=
           Directory_Metadata_Growth.Failed
         then return;
         end if;
      end if;
      -- No table ticket or GPU hold was acquired. Retain CPU metadata and
      -- quarantine on lost authority; never publish from a stale saved request.
      Response.tag := (Application_Binding.Update_Label, 4, 0, 0);
      Response.words := [Application_Buffers.Unavailable, 1, 0, 0];
      Fail_Update;
      Delivered := replyCap (Application_Reply_Slot, Response);
      Directory_Metadata_Pending := False; In_Place_Active := False;
      Update_Held := False; Update_Index := 0;
   end Advance_Directory_Metadata;
   procedure Advance_Insertion_Metadata is
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      if not Insertion_Metadata_Pending then return; end if;
      if Update_Owner and then not Update_Held and then
        Application_VM.Revision (Private_Contexts (Update_Index).Source) = Insertion_Metadata_Epoch
      then
         Insertion_Metadata_Growth.Step (Insertion_Metadata);
         if Insertion_Metadata_Growth.Snapshot (Insertion_Metadata).State = Insertion_Metadata_Growth.Idle then
            Insertion_Metadata_Pending := False; In_Place_Active := False;
            -- Re-run all admission checks, but consume the original saved reply
            -- exactly once; never attempt another kernel reply-capability save.
            Handle_VM_Update (ProcessID (Update_Sender), Update_Request, Saved_Reply => True);
            return;
         elsif Insertion_Metadata_Growth.Snapshot (Insertion_Metadata).State /= Insertion_Metadata_Growth.Failed then
            return;
         end if;
      end if;
      Response.tag := (Application_Binding.Update_Label, 4, 0, 0);
      Response.words := [Application_Buffers.Unavailable, 1, 0, 0];
      Fail_Update;
      Delivered := replyCap (Application_Reply_Slot, Response);
      Insertion_Metadata_Pending := False; In_Place_Active := False;
      Update_Held := False; Update_Index := 0;
   end Advance_Insertion_Metadata;
   -- Cleanup owns its own gate: revoked applications must never regain the
   -- live-session preparation authority merely to detach their mappings.
   Image_Retirement_Index : Natural range 0 .. Intel_GPU_Render_Sessions.Capacity := 0;
   Image_Retirement_First : Unsigned_64 := 0;
   Image_Retirement_Attempted : array (Private_Contexts'Range) of Boolean := [others => False];
   Next_Image_Retirement : Positive := Private_Contexts'First;
   function Image_Retirement_Owner return Boolean is
      Session : Unsigned_64;
      use type Context_Drain.Retirement_State;
      use type Application_Maps.Retirement_State;
   begin
      if Image_Retirement_Index = 0 or else Image_Retirement_First = 0 or else
        PCI_Device /= 16#46D2# or else not Render_Backend_Ready or else
        not Reset_Pages_Mapped or else not Intel_GPU_Native_Reset.Last_Succeeded
      then return False; end if;
      Session := Intel_GPU_Render_Control.Issued_Tag (Render_Admission, Image_Retirement_Index);
      if Session = 0 or else Private_Contexts (Image_Retirement_Index).Parent_Ticket not in
        1 .. Application_Buffers.Ticket'Last or else
        Application_Buffers.Ticket_Session
          (Application_Buffer_State, Private_Contexts (Image_Retirement_Index).Parent_Ticket) /= Session or else
        Private_Contexts (Image_Retirement_Index).Life /= Application_Lifetime.Retired or else
        not Application_Work_Drained (Session) or else
        Context_Drain.Observe (Contexts, Session) /= Context_Drain.Deregistered or else
        Application_Maps.Observe_Retirement (Application_Map_State, Session) /= Application_Maps.Clear or else
        not Runtime_Range_Allowed (Image_Retirement_First, Intel_GPU_Submission_Image.GGTT_Bytes)
      then return False; end if;
      -- Serialized RCS-only backend: no OA admission, no deferred publishers,
      -- and every previously submitted batch completed its flush/disable.
      for I in 1 .. Context_Pool.Count (Contexts) loop
         if not Context_Life.Scheduling_Stopped
           (Context_Pool.State (Contexts, First_Context_ID + Unsigned_32 (I - 1)))
         then return False; end if;
      end loop;
      return True;
   end Image_Retirement_Owner;
   function Image_Retirement_Range (First, Bytes : Unsigned_64) return Boolean is
     (First = Image_Retirement_First and then Bytes = Intel_GPU_Submission_Image.GGTT_Bytes
      and then Image_Retirement_Owner);
   function Image_Retirement_Write (Index, Value : Unsigned_64) return Boolean is
     (Image_Retirement_Owner and then Index >= Image_Retirement_First / 4096 and then
      Index - Image_Retirement_First / 4096 < Intel_GPU_Submission_Image.GGTT_Bytes / 4096 and then
      Value = Intel_GPU_GGTT.Encode_System_Page
        (Intel_GPU_Buffer_Reply.Page_Address (Retirement_Scratch, 0)));
   package Image_Retirement_IO is new Intel_GPU_Native_GGTT
     (Image_Retirement_Owner, Intel_GPU_GGTT_Mapping.Bytes, Image_Retirement_Write);
   procedure Image_Retirement_Clock (Value : out Unsigned_64; OK : out Boolean) is
   begin
      Value := Runtime_Now;
      OK := Value /= Unsigned_64'Last and then Image_Retirement_Owner;
   end Image_Retirement_Clock;
   package Image_Retirement_TLB_IO is new Intel_GPU_Native_TLB_IO (Image_Retirement_Owner);
   package Image_Retirement_GuC is new Intel_GPU_Native_GuC_Invalidate
     (Image_Retirement_Owner, Image_Retirement_Clock);
   package Image_Retirement_Completion is new Intel_GPU_Retirement_Invalidate
     (Image_Retirement_Owner, Image_Retirement_TLB_IO.Write_Register,
      Image_Retirement_TLB_IO.Read_Register, Image_Retirement_GuC.Write_Request,
      Image_Retirement_GuC.Read_Status, Image_Retirement_Clock);
   procedure Invalidate_Retired_Image (OK : out Boolean) is
      Attempt : Image_Retirement_Completion.Attempt;
      Status : Image_Retirement_Completion.Result;
      use type Image_Retirement_Completion.Result;
   begin
      -- ADL-N uses the CEE8 MMIO completion path, not the later-platform
      -- GuC CT TLB-invalidation action. Engine completion alone is insufficient.
      Image_Retirement_Completion.Execute (Attempt, Status);
      OK := Status = Image_Retirement_Completion.Complete;
      if not OK then
         Publish_Snapshot ("intel-gpu: retired context invalidation " &
           Image_Retirement_Completion.Result'Image (Status) & " (BACKING RETAINED)");
      end if;
   end Invalidate_Retired_Image;
   package Image_Retirement is new Application_Images.Retirement
     (Image_Retirement_Range, Image_Retirement_IO.Read_PTE,
      Image_Retirement_IO.Write_PTE, Invalidate_Retired_Image);
   Image_Retirement_Results : array (Private_Contexts'Range) of Image_Retirement.Result :=
     [others => Image_Retirement.Rejected];
   Image_Retirement_Addresses : array (Private_Contexts'Range) of Unsigned_64 := [others => 0];
   procedure Poll_Image_Retirement is
      Index : constant Positive := Next_Image_Retirement;
      Status : Image_Retirement.Result;
      use type Image_Retirement.Result;
   begin
      Next_Image_Retirement := (if Index = Private_Contexts'Last then Private_Contexts'First else Index + 1);
      if Image_Retirement_Attempted (Index) or else
        Private_Contexts (Index).Life /= Application_Lifetime.Retired
      then return; end if;
      Image_Retirement_Index := Index;
      Image_Retirement_First := Application_Publication.GPU_Address (Application_Images_State (Index));
      if Image_Retirement_Owner then
         Image_Retirement_Attempted (Index) := True;
         Image_Retirement_Addresses (Index) := Image_Retirement_First;
         Image_Retirement.Execute (Application_Images_State (Index), Runtime_Ledger,
           Intel_GPU_Buffer_Reply.Page_Address (Retirement_Scratch, 0), Status);
         Image_Retirement_Results (Index) := Status;
         if Status = Image_Retirement.Quarantined then Runtime_Fault := True; end if;
         Publish_Snapshot ("intel-gpu: retired context GGTT " & Image_Retirement.Result'Image (Status) &
           " session=" & Unsigned_64'Image (Intel_GPU_Render_Control.Issued_Tag (Render_Admission, Index)) &
           (if Status = Image_Retirement.Address_Released then
              " (ADDRESS REUSABLE; BACKING RETAINED)"
            else " (BACKING AND CLAIM RETAINED)"));
      end if;
      Image_Retirement_Index := 0;
      Image_Retirement_First := 0;
   end Poll_Image_Retirement;
   Context_Retirement_Index : Natural := 0;
   Context_Retirement_Root : Application_Images.Tables.Page_Mapping := (0, 0);
   Context_Retirement_Revision : Unsigned_64 := 0;
   Context_Retirement_Attempted : array (Private_Contexts'Range) of Boolean := [others => False];
   Next_Context_Retirement : Positive := Private_Contexts'First;
   type Table_Census_Record is record
      Session, Revision, Generation : Unsigned_64 := 0;
      Records : Natural := 0;
      Limit, Cursor, Ledger_Cursor : Positive := 1;
   end record;
   Parent_Table_Census : array (Private_Contexts'Range) of Table_Census_Record;
   procedure Advance_Context_Table_Census
     (Index : Positive; Session : Unsigned_64; Complete, Accepted : out Boolean)
   is
      use type Table_Allocations.Allocation_Role;
      Item : Table_Allocations.Retained_Allocation;
      Found, OK : Boolean;
      Next_Record : Natural;
   begin
      Complete := False; Accepted := False;
      if Index not in Private_Contexts'Range or else Session = 0 then return; end if;
      declare
         Census : Table_Census_Record renames Parent_Table_Census (Index);
         Ledger : Intel_GPU_Table_Provenance.Ledger renames Private_Contexts (Index).Table_Owners;
         function Current return Boolean is
           (Census.Session = Session and then
            Census.Revision = Table_Allocations.Revision (Table_Backing_Registry) and then
            Census.Limit = Table_Allocations.Capacity (Table_Backing_Registry) and then
            Census.Generation = Intel_GPU_Table_Provenance.Generation (Ledger) and then
            Census.Generation = Private_Contexts (Index).Table_Generation and then
            Census.Records = Intel_GPU_Table_Provenance.Count (Ledger));
      begin
         if Private_Contexts (Index).Table_Generation /= Intel_GPU_Table_Provenance.Generation (Ledger)
         then return; end if;
         if not Current then
            Census := (Session => Session,
              Revision => Table_Allocations.Revision (Table_Backing_Registry),
              Generation => Private_Contexts (Index).Table_Generation,
              Records => Intel_GPU_Table_Provenance.Count (Ledger),
              Limit => Table_Allocations.Capacity (Table_Backing_Registry),
              Cursor => 1, Ledger_Cursor => 1);
         end if;
         Item := Table_Allocations.Retained_At (Table_Backing_Registry, Census.Cursor);
         if Item.Present and then Item.Session = Session then
            -- Observations include revoked allocations. Only exact closed
            -- incremental tickets referenced by THIS ledger may join the group.
            if Item.Role /= Table_Allocations.Incremental_Tables or else
              Application_Buffers.Ticket_Slot (Item.Ticket) /= Census.Cursor or else
              not Closed_Table_Tickets.Can_Retire (Application_Buffer_State, Session, Item.Ticket)
            then Census := (others => <>); return; end if;
            Intel_GPU_Table_Provenance.Scan_Ticket
              (Ledger, Session, Item.Ticket, Census.Ledger_Cursor, Found, Next_Record, OK);
            if not OK or else not Current or else (not Found and then Next_Record = 0)
            then Census := (others => <>); return; end if;
            if not Found then
               Census.Ledger_Cursor := Next_Record;
               Accepted := True; return;
            end if;
         end if;
         if not Current then Census := (others => <>); return; end if;
         Accepted := True;
         Census.Ledger_Cursor := 1;
         if Census.Cursor = Census.Limit then
            Complete := True; Census.Cursor := 1;
         else Census.Cursor := Census.Cursor + 1; end if;
      end;
   end Advance_Context_Table_Census;
   function Context_Group_Exclusion (Owner : Unsigned_64) return Boolean is
      Index : constant Natural := Recycle_Context_Index;
      use type Context_Drain.Retirement_State;
      use type Application_Maps.Retirement_State;
   begin
      return Index in Private_Contexts'Range and then Index = Context_Retirement_Index and then
        not Runtime_Fault and then Context_Owner and then Buffer_Retirement_Is_Context and then
        Owner /= 0 and then Owner = Recycle_Session and then Owner = Buffer_Retirement_Session and then
        Owner = Intel_GPU_Render_Control.Issued_Tag (Render_Admission, Index) and then
        Recycle_Ticket /= 0 and then Recycle_Ticket = Buffer_Retirement_Pending and then
        Recycle_Ticket = Private_Contexts (Index).Parent_Ticket and then
        Application_Buffers.Ticket_Slot (Recycle_Ticket) = Recycle_Slot and then
        Private_Contexts (Index).Life = Application_Lifetime.Retired and then
        Current_Table_Ticket (Index) = 0 and then
        Context_Drain.Observe (Contexts, Owner) = Context_Drain.Deregistered and then
        Application_Maps.Observe_Retirement (Application_Map_State, Owner) = Application_Maps.Clear and then
        Context_Tickets.Can_Retire (Application_Buffer_State, Owner, Recycle_Ticket) and then
        (Live_Snapshots.Retired (Private_Contexts (Index).Source) or else
         (Application_VM.Sealed (Private_Contexts (Index).Source) and then
          Application_VM.Revision (Private_Contexts (Index).Source) = Context_Retirement_Revision and then
          Application_VM.Root_DMA (Private_Contexts (Index).Source) = Context_Retirement_Root.DMA));
   end Context_Group_Exclusion;
   procedure Begin_Context_Recycling (Index : Positive; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Index not in Private_Contexts'Range or else Table_Release_Ticket /= 0 or else
        Table_Recycle_Dispatch.Status (Table_Recycle_Control) /= Table_Recycle_Dispatch.Unused
      then return; end if;
      Recycle_Context_Index := Index;
      Recycle_Session := Buffer_Retirement_Session; Recycle_Ticket := Buffer_Retirement_Pending;
      if Recycle_Ticket = 0 then return; end if;
      Recycle_Slot := Application_Buffers.Ticket_Slot (Recycle_Ticket);
      if not Recycle_Exclusion_Ready (Recycle_Session) then return; end if;
      Table_Recycle_Dispatch.Start (Table_Recycle_Control, Private_Contexts (Index).Table_Owners,
        Recycle_Session, Private_Contexts (Index).Table_Generation, Accepted,
        Last_Ticket => Recycle_Ticket);
   end Begin_Context_Recycling;
   procedure Recycle_Context_Ledger (Index : Positive; Accepted : out Boolean) is
      function Released (Session : Unsigned_64) return Boolean is
         use type Context_Drain.Retirement_State;
         use type Application_Maps.Retirement_State;
      begin
         return Index in Private_Contexts'Range and then
           Index = Context_Retirement_Index and then not Runtime_Fault and then Context_Owner and then
           Buffer_Retirement_Is_Context and then Session /= 0 and then
           Session = Buffer_Retirement_Session and then
           Session = Intel_GPU_Render_Control.Issued_Tag (Render_Admission, Index) and then
           Private_Contexts (Index).Life = Application_Lifetime.Retired and then
           Context_Drain.Observe (Contexts, Session) = Context_Drain.Deregistered and then
           Application_Maps.Observe_Retirement (Application_Map_State, Session) = Application_Maps.Clear;
      end Released;
      function Confirmed (Session, Ticket : Unsigned_64) return Boolean is
        (Released (Session) and then Ticket /= 0 and then
         Ticket = Buffer_Retirement_Pending and then
         Ticket = Private_Contexts (Index).Parent_Ticket and then
         Context_Tickets.Can_Retire (Application_Buffer_State, Session, Ticket) and then
         Buffer_Memory.Retirement_Confirmed (Buffer_Pool,
           Application_Buffers.Ticket_Slot (Ticket), Application_Buffers.Ticket_Generation (Ticket)));
   begin
      Accepted := False;
      if Index not in Private_Contexts'Range or else Index /= Recycle_Context_Index or else
        not Confirmed (Buffer_Retirement_Session, Buffer_Retirement_Pending) or else
        Table_Recycle_Dispatch.Status (Table_Recycle_Control) /= Table_Recycle_Dispatch.Done
      then return; end if;
      Table_Recycle_Dispatch.Reopen
        (Table_Recycle_Control, Private_Contexts (Index).Table_Owners, Accepted);
      if Accepted then
         Application_State.Table_References.Reopen
           (Private_Contexts (Index).Table_IDs, Private_Contexts (Index).Table_Generation,
            Intel_GPU_Table_Provenance.Generation (Private_Contexts (Index).Table_Owners), Accepted);
         if not Accepted then return; end if;
         Private_Contexts (Index).Table_Generation := Intel_GPU_Table_Provenance.Generation
           (Private_Contexts (Index).Table_Owners);
         Recycle_Context_Index := 0; Recycle_Session := 0; Recycle_Ticket := 0;
      end if;
   end Recycle_Context_Ledger;
   procedure Finish_Context_Retirement is
      Index : constant Natural := Context_Retirement_Index;
      Accepted : Boolean := False;
      use type Context_Drain.Retirement_State;
      use type Application_Maps.Retirement_State;
   begin
      -- No application identity is consulted: this is cleanup of a revoked
      -- session. Client dispatch is excluded while this exact request is pending.
      if Index in Private_Contexts'Range and then not Runtime_Fault and then
        Context_Owner and then
        Private_Contexts (Index).Life = Application_Lifetime.Retired and then
        Private_Contexts (Index).Parent_Ticket = Buffer_Retirement_Pending and then
        Buffer_Retirement_Session /= 0 and then
        Buffer_Retirement_Session = Intel_GPU_Render_Control.Issued_Tag (Render_Admission, Index) and then
        Context_Drain.Observe (Contexts, Buffer_Retirement_Session) = Context_Drain.Deregistered and then
        Application_Maps.Observe_Retirement (Application_Map_State, Buffer_Retirement_Session) = Application_Maps.Clear and then
        Context_Tickets.Can_Retire (Application_Buffer_State,
          Buffer_Retirement_Session, Buffer_Retirement_Pending) and then
        Buffer_Memory.Retirement_Confirmed (Buffer_Pool,
          Application_Buffers.Ticket_Slot (Buffer_Retirement_Pending),
          Application_Buffers.Ticket_Generation (Buffer_Retirement_Pending))
      then
         Accepted := Live_Snapshots.Retired (Private_Contexts (Index).Source);
         if not Accepted then
            Live_Snapshots.Forget_Retired (Private_Contexts (Index).Source,
              Context_Retirement_Revision, Context_Retirement_Root.DMA, True, Accepted);
         end if;
         if Accepted then
            Image_Retirement.Forget_Backing_Receipt (Application_Images_State (Index),
              Image_Retirement_Addresses (Index), Context_Retirement_Root, True, Accepted);
         end if;
         if Accepted then
            Recycle_Context_Ledger (Index, Accepted);
         end if;
         if Accepted then
            Context_Tickets.Acknowledge (Application_Buffer_State,
              Buffer_Retirement_Session, Buffer_Retirement_Pending, True, Accepted);
         end if;
         if Accepted then
            Private_Contexts (Index).Parent := (Ready => False);
            Private_Contexts (Index).Context := (Ready => False);
            Private_Contexts (Index).Tables := (Ready => False);
            Private_Contexts (Index).Scratch := (Ready => False);
         end if;
      end if;
      if not Accepted then
         Runtime_Fault := True;
         Buffer_Memory.Cancel (Buffer_Pool);
         Application_Buffers.Quarantine (Application_Buffer_State);
      end if;
      Publish_Snapshot ("intel-gpu: context parent slice retirement acknowledged=" &
        Boolean'Image (Accepted) & " ticket=" & Unsigned_64'Image (Buffer_Retirement_Pending) &
        " (PHYSICAL ARENA RETAINED)");
      Buffer_Retirement_Pending := 0;
      Buffer_Retirement_Is_Context := False;
      Context_Retirement_Index := 0;
   end Finish_Context_Retirement;
   procedure Poll_Context_Retirement is
      Index : constant Positive := Next_Context_Retirement;
      Session : constant Unsigned_64 :=
        Intel_GPU_Render_Control.Issued_Tag (Render_Admission, Index);
      Parent : Intel_GPU_Buffer_Reply.Backing renames Private_Contexts (Index).Parent;
      Root : constant Application_Images.Tables.Page_Mapping :=
        Application_Images.Retained_Root (Application_Images_State (Index));
      Started : Boolean;
      Table_Revision : constant Unsigned_64 := Table_Allocations.Revision (Table_Backing_Registry);
      Census_Complete, Census_OK : Boolean;
      Census : Table_Census_Record renames Parent_Table_Census (Index);
      use type Image_Retirement.Result;
      function Conflicts (Page : Unsigned_64) return Boolean is
        (Intel_GPU_Buffer_Reply.Overlaps_DMA (Parent, Page, 4096));
      function Disjoint is new Application_VM.Backing_Disjoint (Conflicts);
   begin
      Next_Context_Retirement := (if Index = Private_Contexts'Last then Private_Contexts'First else Index + 1);
      if Session = 0 or else Context_Retirement_Attempted (Index) or else
        Image_Retirement_Results (Index) /= Image_Retirement.Address_Released or else
        not Intel_GPU_Buffer_Reply.Valid (Parent) or else
        not Context_Tickets.Can_Retire (Application_Buffer_State, Session, Private_Contexts (Index).Parent_Ticket) or else
        not Application_Work_Drained (Session) or else Current_Table_Ticket (Index) /= 0 or else
        Root.CPU = 0 or else Root.DMA = 0 or else
        (not Live_Snapshots.Retired (Private_Contexts (Index).Source) and then
         (not Application_VM.Sealed (Private_Contexts (Index).Source) or else
          Root.DMA /= Application_VM.Root_DMA (Private_Contexts (Index).Source)))
      then return; end if;
      Advance_Context_Table_Census (Index, Session, Census_Complete, Census_OK);
      if not Census_OK or else not Census_Complete then return; end if;
      -- Updated VMs need grouped replacement retirement; do not free their
      -- initial scratch/root parent while retained candidates still refer to it.
      for Slot in 1 .. Replacement_Records.Capacity (Replacement_Tables) loop
         if Replacement_Records.Get (Replacement_Tables, Slot).Session = Session then return; end if;
         if Application_State.Has_Update (Slot) and then Application_State.Updates (Slot).Tables.Ready and then
           (not Application_VM.Sealed (Application_State.Updates (Slot).Candidate) or else
            not Disjoint (Application_State.Updates (Slot).Candidate))
         then return; end if;
      end loop;
      for I in Private_Contexts'Range loop
         if I /= Index and then Private_Contexts (I).Attempted and then
           not Live_Snapshots.Retired (Private_Contexts (I).Source) and then
           (not Application_VM.Sealed (Private_Contexts (I).Source) or else
            not Disjoint (Private_Contexts (I).Source))
         then return; end if;
      end loop;
      Image_Retirement_Index := Index;
      Image_Retirement_First := Image_Retirement_Addresses (Index);
      Started := Image_Retirement_Owner;
      Image_Retirement_Index := 0; Image_Retirement_First := 0;
      if not Started or else Table_Allocations.Revision (Table_Backing_Registry) /= Table_Revision or else
        Intel_GPU_Table_Provenance.Generation (Private_Contexts (Index).Table_Owners) /= Census.Generation or else
        Intel_GPU_Table_Provenance.Count (Private_Contexts (Index).Table_Owners) /= Census.Records
      then
         return;
      end if;
      Context_Retirement_Attempted (Index) := True;
      Context_Retirement_Index := Index;
      Context_Retirement_Root := Root;
      Context_Retirement_Revision := Application_VM.Revision (Private_Contexts (Index).Source);
      Buffer_Retirement_Pending := Private_Contexts (Index).Parent_Ticket;
      Buffer_Retirement_Session := Session;
      Buffer_Retirement_Has_Reply := False;
      Buffer_Retirement_Is_Private := False;
      Buffer_Retirement_Is_Context := True;
      Begin_Context_Recycling (Index, Started);
      if not Started then
         Buffer_Memory.Cancel (Buffer_Pool);
         Finish_Context_Retirement;
      end if;
   end Poll_Context_Retirement;
   Next_Closed_Table_Retirement : Intel_GPU_Buffer_Backing.Slot := 1;
   Closed_Table_Index : Natural := 0;
   Closed_Table_Source_Revision : Unsigned_64 := 0;
   procedure Finish_Closed_Table_Retirement is
      Slot : constant Intel_GPU_Buffer_Backing.Slot :=
        Application_Buffers.Ticket_Slot (Buffer_Retirement_Pending);
      Saved : constant Replacement_Record := Replacement_Records.Get (Replacement_Tables, Slot);
      Index : constant Natural := Closed_Table_Index;
      Accepted : Boolean := False;
      Recycling : Boolean := False;
      use type Context_Drain.Retirement_State;
      use type Application_Maps.Retirement_State;
   begin
      if Index in Private_Contexts'Range and then not Runtime_Fault and then Context_Owner and then
        Saved.Ticket = Buffer_Retirement_Pending and then Saved.Session = Buffer_Retirement_Session and then
        Saved.Session /= 0 and then
        Saved.Session = Intel_GPU_Render_Control.Issued_Tag (Render_Admission, Index) and then
        Private_Contexts (Index).Life = Application_Lifetime.Retired and then
        Context_Drain.Observe (Contexts, Saved.Session) = Context_Drain.Deregistered and then
        Application_Maps.Observe_Retirement (Application_Map_State, Saved.Session) = Application_Maps.Clear and then
        Closed_Table_Tickets.Can_Retire (Application_Buffer_State, Saved.Session, Saved.Ticket) and then
        Buffer_Memory.Retirement_Confirmed (Buffer_Pool, Slot,
          Application_Buffers.Ticket_Generation (Saved.Ticket))
      then
         if Recycle_In_Progress then
            Accepted := True;
         else
            Live_Snapshots.Forget_Retired (Application_State.Updates (Slot).Candidate,
              Saved.Revision, Saved.Root, True, Accepted);
         end if;
         if Accepted and then not Recycle_In_Progress and then Closed_Table_Source_Revision /= 0 then
            Accepted := Current_Table_Ticket (Index) = Saved.Ticket;
            if Accepted then
               -- Adopted Source has its own epoch, not Candidate's epoch.
               Live_Snapshots.Forget_Retired (Private_Contexts (Index).Source,
                 Closed_Table_Source_Revision, Saved.Root, True, Accepted);
            end if;
         end if;
         if Accepted then
            Recycle_Table_Ledger (Slot, Saved.Session, Saved.Ticket, Accepted, Recycling);
            if Recycling then return; end if;
         end if;
         if Accepted then
            Closed_Table_Tickets.Acknowledge (Application_Buffer_State,
              Saved.Session, Saved.Ticket, True, Accepted);
         end if;
         if Accepted then
            if Closed_Table_Source_Revision /= 0 then Current_Table_Ticket (Index) := 0; end if;
            Application_State.Updates (Slot).Tables := (Ready => False);
            Replacement_Records.Put (Replacement_Tables, Slot, (others => <>));
         end if;
      end if;
      if not Accepted then
         Runtime_Fault := True;
         Buffer_Memory.Cancel (Buffer_Pool);
         Application_Buffers.Quarantine (Application_Buffer_State);
      end if;
      Publish_Snapshot ("intel-gpu: closed context table retirement acknowledged=" &
        Boolean'Image (Accepted) & " ticket=" & Unsigned_64'Image (Buffer_Retirement_Pending));
      Buffer_Retirement_Pending := 0;
      Buffer_Retirement_Is_Closed_Table := False;
      Closed_Table_Index := 0; Closed_Table_Source_Revision := 0;
   end Finish_Closed_Table_Retirement;
   procedure Poll_Closed_Table_Retirement is
      Slot : constant Intel_GPU_Buffer_Backing.Slot := Next_Closed_Table_Retirement;
      Saved : constant Replacement_Record := Replacement_Records.Get (Replacement_Tables, Slot);
      Backing : constant Intel_GPU_Buffer_Reply.Backing :=
        (if Application_State.Has_Update (Slot) then Application_State.Updates (Slot).Tables
         else (Ready => False));
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Saved.Session);
      Index : Positive;
      Source_Revision : Unsigned_64 := 0;
      Started : Boolean;
      use type Image_Retirement.Result;
   begin
      Next_Closed_Table_Retirement :=
        (if Slot = Replacement_Records.Capacity (Replacement_Tables) then 1 else Slot + 1);
      if Saved.Ticket = 0 or else Stored = 0 or else
        Application_Buffers.Ticket_Slot (Saved.Ticket) /= Slot or else
        not Application_Work_Drained (Saved.Session) or else
        not Closed_Table_Tickets.Can_Retire (Application_Buffer_State, Saved.Session, Saved.Ticket) or else
        not Intel_GPU_Buffer_Reply.Valid (Backing) or else
        not Application_VM.Sealed (Application_State.Updates (Slot).Candidate) or else
        Application_VM.Revision (Application_State.Updates (Slot).Candidate) /= Saved.Revision or else
        Application_VM.Root_DMA (Application_State.Updates (Slot).Candidate) /= Saved.Root
      then return; end if;
      Index := Positive (Stored);
      if Image_Retirement_Results (Index) /= Image_Retirement.Address_Released then return; end if;
      if Current_Table_Ticket (Index) = Saved.Ticket then
         if not Application_VM.Sealed (Private_Contexts (Index).Source) or else
           Application_VM.Root_DMA (Private_Contexts (Index).Source) /= Saved.Root
         then return; end if;
         Source_Revision := Application_VM.Revision (Private_Contexts (Index).Source);
      end if;
      Image_Retirement_Index := Index;
      Image_Retirement_First := Image_Retirement_Addresses (Index);
      Started := Image_Retirement_Owner;
      Image_Retirement_Index := 0; Image_Retirement_First := 0;
      if not Started then return; end if;
      Closed_Table_Index := Index;
      Closed_Table_Source_Revision := Source_Revision;
      Buffer_Retirement_Pending := Saved.Ticket;
      Buffer_Retirement_Session := Saved.Session;
      Buffer_Retirement_Has_Reply := False;
      Buffer_Retirement_Is_Private := False;
      Buffer_Retirement_Is_Context := False;
      Buffer_Retirement_Is_Closed_Table := True;
      Begin_Table_Recycling (Slot, Saved.Session, Saved.Ticket, Started);
      if not Started then
         Buffer_Memory.Cancel (Buffer_Pool);
         Finish_Closed_Table_Retirement;
      end if;
   end Poll_Closed_Table_Retirement;
   type Recycle_Preparation_Phase is (Check_Contexts, Check_Images, Prepared);
   Recycle_Preparation : Recycle_Preparation_Phase := Check_Contexts;
   Recycle_Preparation_Ticket : Unsigned_64 := 0;
   Recycle_Preparation_Revision : Unsigned_64 := 0;
   Recycle_Preparation_Cursor, Recycle_Preparation_Limit : Positive := 1;
   procedure Prepare_Table_Retirement
     (Owner, ID : Unsigned_64; Complete, Failed : out Boolean) is
   begin
      Complete := False; Failed := True;
      if not Recycle_May_Release (Owner, ID) then return; end if;
      if Recycle_Preparation_Ticket /= ID then
         Recycle_Preparation_Ticket := ID;
         Recycle_Preparation_Revision := Table_Allocations.Revision (Table_Backing_Registry);
         Recycle_Preparation := Check_Contexts;
         Recycle_Preparation_Cursor := 1;
         Recycle_Preparation_Limit := Replacement_Records.Capacity (Replacement_Tables);
      end if;
      if Recycle_Preparation_Limit /= Replacement_Records.Capacity (Replacement_Tables) or else
        Recycle_Preparation_Revision /= Table_Allocations.Revision (Table_Backing_Registry)
      then return; end if;
      declare
         Backing : constant Intel_GPU_Buffer_Reply.Backing :=
           (if Buffer_Retirement_Is_Context then Private_Contexts (Recycle_Context_Index).Parent
            else Application_State.Updates (Recycle_Slot).Tables);
         Saved : constant Replacement_Record :=
           (if Buffer_Retirement_Is_Context then
              (Revision => Context_Retirement_Revision, Root => Context_Retirement_Root.DMA, others => <>)
            else Replacement_Records.Get (Replacement_Tables, Recycle_Slot));
         I : constant Positive := Recycle_Preparation_Cursor;
         function Conflicts (Page : Unsigned_64) return Boolean is
            Overlaps, Known : Boolean;
         begin
            if Buffer_Retirement_Is_Context and then ID = Recycle_Ticket then
               return Intel_GPU_Buffer_Reply.Overlaps_DMA (Backing, Page, 4096);
            end if;
            Table_Allocations.Check_Retained_Range
              (Table_Backing_Registry, Application_Buffers.Ticket_Slot (ID),
               Owner, ID, Recycle_Preparation_Revision, Page, 4096, Overlaps, Known);
            return not Known or else Overlaps;
         end Conflicts;
         function Disjoint is new Application_VM.Backing_Disjoint (Conflicts);
      begin
         if not Intel_GPU_Buffer_Reply.Valid (Backing) then return; end if;
         case Recycle_Preparation is
            when Check_Contexts =>
               if Private_Contexts (I).Attempted and then
                 not Live_Snapshots.Retired (Private_Contexts (I).Source)
               then
                  if (Buffer_Retirement_Is_Context and then I = Recycle_Context_Index) or else
                    (Buffer_Retirement_Is_Closed_Table and then I = Closed_Table_Index and then
                     Current_Table_Ticket (I) = Recycle_Ticket)
                  then
                     if not Application_VM.Sealed (Private_Contexts (I).Source) or else
                       Application_VM.Root_DMA (Private_Contexts (I).Source) /= Saved.Root or else
                       Application_VM.Revision (Private_Contexts (I).Source) /=
                         (if Buffer_Retirement_Is_Context then Context_Retirement_Revision
                          else Closed_Table_Source_Revision)
                     then return; end if;
                  elsif not Application_VM.Sealed (Private_Contexts (I).Source) or else
                    not Disjoint (Private_Contexts (I).Source)
                  then return; end if;
               end if;
               if I = Private_Contexts'Last then
                  Recycle_Preparation := Check_Images; Recycle_Preparation_Cursor := 1;
               else Recycle_Preparation_Cursor := I + 1; end if;
            when Check_Images =>
               if (Buffer_Retirement_Is_Context or else I /= Recycle_Slot) and then Application_State.Has_Update (I) and then
                 Application_State.Updates (I).Tables.Ready and then
                 (not Application_VM.Sealed (Application_State.Updates (I).Candidate) or else
                  not Disjoint (Application_State.Updates (I).Candidate))
               then return; end if;
               if I = Recycle_Preparation_Limit then Recycle_Preparation := Prepared;
               else Recycle_Preparation_Cursor := I + 1; end if;
            when Prepared => Complete := True;
         end case;
         -- Buffer_Retirement_Pending excludes new work across all these turns.
         -- Recheck hardware/context exclusion before accepting any progress.
         Failed := not Recycle_May_Release (Owner, ID);
      end;
   end Prepare_Table_Retirement;
   -- Cleanup authority never resolves or reopens an application capability.
   -- This first path covers fully dismantled contexts only. Failed startup or
   -- incomplete table retirement remains retained for a separate recovery path.
   function Teardown_Buffer_Owner (Session, Ticket : Unsigned_64) return Boolean is
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
      use type Context_Drain.Retirement_State;
      use type Application_Maps.Retirement_State;
      use type Image_Retirement.Result;
   begin
      if Runtime_Fault or else not Context_Owner or else not Render_Backend_Ready or else
        Stored = 0 or else Session = 0 or else
        Intel_GPU_Render_Control.Issued_Tag (Render_Admission, Stored) /= Session or else
        Private_Contexts (Stored).Life /= Application_Lifetime.Retired or else
        Private_Contexts (Stored).Parent.Ready or else
        not Context_Retirement_Attempted (Stored) or else
        not Live_Snapshots.Retired (Private_Contexts (Stored).Source) or else
        Current_Table_Ticket (Stored) /= 0 or else
        Image_Retirement_Results (Stored) /= Image_Retirement.Address_Released or else
        Context_Drain.Observe (Contexts, Session) /= Context_Drain.Deregistered or else
        Application_Maps.Observe_Retirement (Application_Map_State, Session) /= Application_Maps.Clear
      then return False; end if;
      -- App BO bindings are session-owned; the session's source has now been
      -- forgotten only after its context/table receipts completed. Outstanding
      -- exported CPU references are independently rejected by Can_Retire.
      return Application_Buffers.Can_Retire (Application_Buffer_State, Session, Ticket);
   end Teardown_Buffer_Owner;
   procedure Finish_Teardown_Buffer_Retirement is
      Accepted : Boolean := False;
   begin
      if Teardown_Buffer_Owner (Buffer_Retirement_Session, Buffer_Retirement_Pending) and then
        Buffer_Memory.Retirement_Confirmed (Buffer_Pool,
          Application_Buffers.Ticket_Slot (Buffer_Retirement_Pending),
          Application_Buffers.Ticket_Generation (Buffer_Retirement_Pending))
      then
         Application_Buffers.Acknowledge_Retirement (Application_Buffer_State,
           Buffer_Retirement_Session, Buffer_Retirement_Pending, True, Accepted);
      end if;
      if not Accepted then
         Runtime_Fault := True;
         Buffer_Memory.Cancel (Buffer_Pool);
         Application_Buffers.Quarantine (Application_Buffer_State);
      end if;
      Publish_Snapshot ("intel-gpu: teardown BO retirement acknowledged=" &
        Boolean'Image (Accepted) & " ticket=" & Unsigned_64'Image (Buffer_Retirement_Pending),
        (if Accepted then CuBit.Log_Records.Debug else CuBit.Log_Records.Error));
      Buffer_Retirement_Pending := 0;
      Buffer_Retirement_Is_Teardown := False;
   end Finish_Teardown_Buffer_Retirement;
   Next_Teardown_Buffer : Intel_GPU_Buffer_Backing.Slot := 1;
   procedure Poll_Teardown_Buffers is
      Slot : constant Intel_GPU_Buffer_Backing.Slot := Next_Teardown_Buffer;
      Candidate : constant Application_Buffers.Closed_Allocation :=
        Application_Buffers.Closed_At (Application_Buffer_State, Slot);
      Started : Boolean;
   begin
      Next_Teardown_Buffer :=
        (if Slot >= Application_Buffers.Committed_Slots (Application_Buffer_State)
         then 1 else Slot + 1);
      if not Candidate.Ready or else not Application_Work_Drained (Candidate.Session) or else
        not Teardown_Buffer_Owner (Candidate.Session, Candidate.ID)
      then return; end if;
      Buffer_Retirement_Pending := Candidate.ID;
      Buffer_Retirement_Session := Candidate.Session;
      Buffer_Retirement_Has_Reply := False;
      Buffer_Retirement_Is_Private := False;
      Buffer_Retirement_Is_Context := False;
      Buffer_Retirement_Is_Closed_Table := False;
      Buffer_Retirement_Is_Teardown := True;
      Buffer_Memory.Retire (Buffer_Pool, Slot, Candidate.Generation, True, Started);
      if not Started then
         Buffer_Memory.Cancel (Buffer_Pool);
         Finish_Teardown_Buffer_Retirement;
      end if;
   end Poll_Teardown_Buffers;
   procedure Handle_Retirement_Query (From : ProcessID; Msg : Message) is
      package Control renames Intel_GPU_Render_Control;
      use type Context_Drain.Retirement_State;
      use type Application_Maps.Retirement_State;
      use type Image_Retirement.Result;
      Session : constant Unsigned_64 := Control.Resolve_Retired
        (Render_Admission, Unsigned_64 (From), Msg.authorityTag);
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Control.Storage_Index (Render_Admission, Session);
      Facts : Control.Drain_Facts;
      GPU : Context_Drain.Retirement_State;
      Grants : Application_Maps.Retirement_State;
      Code : Unsigned_64 := Control.Denied;
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      if Session /= 0 and then Stored /= 0 then
         Code := Control.Bad_Request;
         if Msg.tag = (Control.Retirement_Label, 4, 0, 0) and then
           Msg.words = [Control.Version, 0, 0, 0]
         then
            GPU := Context_Drain.Observe (Contexts, Session);
            Grants := Application_Maps.Observe_Retirement (Application_Map_State, Session);
            Facts.Uncertain := Runtime_Fault or else not Publication_Owner_Ready or else
              GPU in Context_Drain.Uncertain | Context_Drain.Admission_Open or else
              Grants = Application_Maps.Uncertain;
            -- Serialized dispatcher: synchronous registration/submission cannot
            -- overlap this handler. Deferred allocation/VM work still can.
            Facts.Work_Pending := Application_Buffers.Pending_For
              (Application_Buffer_State, Session) or else
              (Preparing_Index /= 0 and then Preparing_Session = Session);
            declare
               Index : constant Positive := Stored;
            begin
               if Application_Registration_Attempted (Index) then
                  -- A deregistration event stops scheduling, but does not
                  -- remove the context's GGTT aliases. Wait for the exact
                  -- scratch-remap and both translation completion domains.
                  Facts.Work_Pending := Facts.Work_Pending or else
                    not Image_Retirement_Attempted (Index);
                  Facts.Uncertain := Facts.Uncertain or else
                    (Image_Retirement_Attempted (Index) and then
                     Image_Retirement_Results (Index) /= Image_Retirement.Address_Released);
               end if;
            end;
            -- Disable can still precede outstanding deregistration or a
            -- deferred VM hold. Only GuC's completion closes this milestone.
            -- This remains quiescence, NOT permission to reuse any backing.
            Facts.GPU_Stopped := GPU = Context_Drain.Deregistered or else
              (GPU = Context_Drain.No_Context and then not
               Application_Registration_Attempted
                 (Stored));
            if GPU = Context_Drain.No_Context and then not Facts.GPU_Stopped then
               -- An attempted registration without an observable context is
               -- uncertainty, not evidence that no hardware work exists.
               Facts.Uncertain := True;
            end if;
            Facts.Grants_Retired := Grants = Application_Maps.Clear;
            Code := Control.Drain_Status (Facts);
         end if;
      end if;
      Response.tag := (Control.Retirement_Label, 4, 0, 0);
      Response.words := [Code, Control.Version, 0, 0];
      Delivered := reply (From, Response);
   end Handle_Retirement_Query;
   function Session_Healthy (Session : Unsigned_64) return Boolean is
      ID : Unsigned_32;
      Stored : constant Intel_GPU_Render_Sessions.Slot_Index :=
        Intel_GPU_Render_Control.Storage_Index (Render_Admission, Session);
   begin
      if Stored = 0 then return False; end if;
      -- Resolve supplies the bounded active-session tag, never request data.
      if not Application_Lifetime.Admission_Ready
        (Private_Contexts (Stored).Life,
         not Runtime_Fault and then Context_Owner,
         True, -- Caller separately resolves the active identity/session.
         (Private_Pending /= 0 and then Private_Session = Session) or else
         (Table_Ledger_Busy and then Ledger_Session = Session))
      then return False; end if;
      ID := Context_Pool.Session_Context (Contexts, Session);
      return ID = Context_Pool.No_Context or else
        Context_Pool.State (Contexts, ID) /= Context_Life.Quarantined;
   end Session_Healthy;
   procedure Handle_Session_Status (From : ProcessID; Msg : Message) is
      package Control renames Intel_GPU_Render_Control;
      Session : constant Unsigned_64 := Control.Resolve
        (Render_Admission, Unsigned_64 (From), Msg.authorityTag);
      Healthy : Boolean := False;
      Response : Message := NULL_MESSAGE;
      Data : Control.Words;
      Delivered : Unsigned_64;
   begin
      -- Do not touch device registers on behalf of an unauthenticated caller.
      if Session /= 0 and then
        Msg.tag = (Control.Status_Label, 4, 0, 0) and then
        Msg.words = [Control.Version, 0, 0, 0]
      then
         Healthy := Session_Healthy (Session);
      end if;
      Data := Control.Session_Status
        (Render_Admission, Unsigned_64 (From), Msg.authorityTag, Healthy,
         Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
         [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)]);
      Response.tag := (Control.Status_Label, 4, 0, 0);
      Response.words := [Data (0), Data (1), Data (2), Data (3)];
      Delivered := reply (From, Response);
   end Handle_Session_Status;
   function VM_Query_Policy (From : ProcessID; Msg : Message)
      return Intel_GPU_Device_Query.VM_Contract
   is
      package DQ renames Intel_GPU_Device_Query;
      Session : Unsigned_64;
   begin
      if Msg.tag /= (DQ.Label, 4, 0, 0) or else
        Msg.words /= [DQ.Version, DQ.Virtual_Memory, 0, 0]
      then return DQ.VM_Unavailable; end if;
      Session := Intel_GPU_Render_Control.Resolve
        (Render_Admission, Unsigned_64 (From), Msg.authorityTag);
      -- Same authenticated health gate as active session status. Private
      -- roots/scratch and raw48 bindings are the Application_State VM
      -- contract, not a capability guessed from the PCI model or GGTT size.
      return (if Session_Healthy (Session) then DQ.Private_PPGTT_48
              else DQ.VM_Unavailable);
   end VM_Query_Policy;
   function Memory_Query_Policy (From : ProcessID; Msg : Message)
      return Intel_GPU_Device_Query.Memory_Contract
   is
      package DQ renames Intel_GPU_Device_Query;
      Session : Unsigned_64;
   begin
      if Msg.tag /= (DQ.Label, 4, 0, 0) or else
        Msg.words /= [DQ.Version, DQ.Memory, 0, 0]
      then return DQ.Not_Admitted; end if;
      Session := Intel_GPU_Render_Control.Resolve
        (Render_Admission, Unsigned_64 (From), Msg.authorityTag);
      -- Owned CPU-WB/GPU-PAT0 arena only. Platform contract and boot
      -- regression checks are distinct from current session/owner authority.
      -- Never infer admission from the historical probe result alone.
      return Intel_GPU_Memory_Admission.Policy
        ((Device => PCI_Device, Runtime_Admitted => Runtime_Admitted,
          Owner_Held => Publication_Owner_Ready and then Context_Owner,
          Session_Healthy => Session_Healthy (Session), Faulted => Runtime_Fault,
          CPU_To_GPU_Checked => CPU_Coherence_Checked,
          GPU_To_CPU_Checked => GPU_Coherence_Checked));
   end Memory_Query_Policy;
   procedure Start_Render_Context is
      function Completion_Name (Value : Initial_Completion.Result) return String is
        (case Value is
           when Initial_Completion.Rejected => "REJECTED",
           when Initial_Completion.Ready => "READY",
           when Initial_Completion.Complete => "COMPLETE",
           when Initial_Completion.Ownership_Lost => "OWNERSHIP_LOST",
           when Initial_Completion.Read_Failed => "READ_FAILED",
           when Initial_Completion.Unexpected_Marker => "UNEXPECTED_MARKER",
           when Initial_Completion.Invalid_Clock => "INVALID_CLOCK",
           when Initial_Completion.Timed_Out => "TIMED_OUT",
           when Initial_Completion.Event_Failed => "EVENT_FAILED");
      function Wait_Name (Value : Context_Wait.Result) return String is
        (case Value is
           when Context_Wait.Rejected => "REJECTED",
           when Context_Wait.Complete => "COMPLETE",
           when Context_Wait.Ownership_Lost => "OWNERSHIP_LOST",
           when Context_Wait.Invalid_Clock => "INVALID_CLOCK",
           when Context_Wait.Timed_Out => "TIMED_OUT",
           when Context_Wait.Send_Failed => "SEND_FAILED",
           when Context_Wait.Receive_Failed => "RECEIVE_FAILED",
           when Context_Wait.Context_Failed => "CONTEXT_FAILED");
      function Notify_Name (Value : Context_Driver.Result) return String is
        (case Value is
           when Context_Driver.Rejected => "REJECTED",
           when Context_Driver.Backpressure => "BACKPRESSURE",
           when Context_Driver.Queued => "QUEUED",
           when Context_Driver.Handled => "HANDLED",
           when Context_Driver.Retained => "RETAINED",
           when Context_Driver.Faulted => "FAULTED");
      Status : Context_Driver.Result;
      Wait_Status : Context_Wait.Result;
      use type Context_Driver.Result;
      use type Context_Wait.Result;
      Completed : Initial_Completion.Result;
      Published : Boolean;
      Repeat_Attempt : Initial_Completion.Attempt;
      Repeat_Completed : Initial_Completion.Result := Initial_Completion.Rejected;
      Repeat_Notify : Context_Driver.Result := Context_Driver.Rejected;
      Repeat_Published : Boolean := False;
      Repeat_Segment : Intel_GPU_ADLN_Context_Init.Segment;
      Repeat_Tail : Unsigned_32 := 0;
      Barrier_Attempt : Initial_Completion.Attempt;
      Barrier_Completed : Initial_Completion.Result := Initial_Completion.Rejected;
      Barrier_Notify : Context_Driver.Result := Context_Driver.Rejected;
      Barrier_Published : Boolean := False;
      L3_Attempt : Initial_Completion.Attempt;
      L3_Completed : Initial_Completion.Result := Initial_Completion.Rejected;
      L3_Notify : Context_Driver.Result := Context_Driver.Rejected;
      L3_Published, L3_Read, L3_Fresh, L3_Matches : Boolean := False;
      L3_Value : Unsigned_32 := Unsigned_32'Last;
      L3_Parameters : Unsigned_32 := Unsigned_32'Last;
      L3_URB_KiB : Natural := 0;
      Draw_Attempt : Initial_Completion.Attempt;
      Draw_Completed : Initial_Completion.Result := Initial_Completion.Rejected;
      Draw_Notify : Context_Driver.Result := Context_Driver.Rejected;
      Draw_Published, Draw_Target_Clear, Pixels_Read, Pixels_Match : Boolean := False;
      Pixels : Initial_Ring.Pixel_Samples := [others => Unsigned_32'Last];
      Unflushed_Pixels : Initial_Ring.Pixel_Samples := [others => Unsigned_32'Last];
      Unflushed_Read, Unflushed_Match : Boolean := False;
      Image : Initial_Ring.Target_Image;
      Image_Read : Boolean := False;
      Image_Nonzero : Natural := 0;
      Image_Hash : Unsigned_32 := 16#811C9DC5#;
      Saved_Head, Saved_Tail, H2G_Head, H2G_Tail, H2G_Status : Unsigned_32 := 0;
      Saved_OK, H2G_OK : Boolean := False;
      Batch_Value : Unsigned_64 := Unsigned_64'Last;
      Batch_Read : Boolean := False;
      Copy_Prepared, Copy_Read : Boolean := False;
      Copy_Value : Unsigned_32 := Unsigned_32'Last;
      use type Initial_Completion.Result;
      TLB_Armed : Boolean := False;
      Update_Held, Update_Released : Boolean := False;
      function TLB_Probe_Owner return Boolean is
        (TLB_Armed and then Update_Held and then PCI_Device = 16#46D2# and then
         Initial_Backing_Owner and then not Runtime_Fault and then
         Reset_Pages_Mapped and then Intel_GPU_Native_Reset.Last_Succeeded and then
         Context_Pool.Count (Contexts) = 1 and then
         not Context_Pool.Failed (Contexts) and then
         Context_Pool.State (Contexts, Render_Context_ID) = Context_Life.Disabled);
      -- One serialized bootstrap owner, no application or OA admission.
      -- Successful native reset retains forcewake; only this RCS probe has
      -- executed since that reset. This is NOT a general multi-engine gate.
      package TLB_IO is new Intel_GPU_Native_TLB_IO (TLB_Probe_Owner);
      procedure TLB_Clock (Value : out Unsigned_64; OK : out Boolean) is
      begin
         Value := Runtime_Now;
         OK := Value /= Unsigned_64'Last and then TLB_Probe_Owner;
      end TLB_Clock;
      package TLB_Probe is new Intel_GPU_ADLN_TLB_Invalidate
        (TLB_Probe_Owner, TLB_IO.Write_Register, TLB_IO.Read_Register, TLB_Clock);
      package GuC_Native_Wait is new Intel_GPU_Native_GuC_Invalidate
        (TLB_Probe_Owner, TLB_Clock);
      package GuC_Wait renames GuC_Native_Wait.Completion;
      GuC_Attempt : GuC_Wait.Attempt;
      GuC_Status : GuC_Wait.Result := GuC_Wait.Rejected;
      use type GuC_Wait.Result;
      TLB_Attempt : TLB_Probe.Attempt;
      TLB_Status : TLB_Probe.Result := TLB_Probe.Rejected;
      use type TLB_Probe.Result;
      function Flush_Update_Page (CPU : Unsigned_64) return Boolean is
        (TLB_Probe_Owner and then Intel_GPU_DMA_Cache.Flush_Range (CPU, 4096)
         and then TLB_Probe_Owner);
      package Boot_Updates is new Submission_Buffers.Updates
        (TLB_Probe_Owner, Flush_Update_Page);
      Update_Tables : Submission_Buffers.VM.Backing_Pages;
      Update_Mappings : Boot_Updates.Tables.Mappings;
      Update_Ready : Boolean := False;
      type Update_Stage is (Prechecks, Allocation, Ownership, Prepare,
                            Map_Alias, Seal, Publish, Invalidate, Finished);
      Update_At : Update_Stage := Prechecks;
      Resume_Status, Final_Disable : Context_Wait.Result := Context_Wait.Rejected;
      Alias_Attempt : Initial_Completion.Attempt;
      Alias_Completed : Initial_Completion.Result := Initial_Completion.Rejected;
      Alias_Published : Boolean := False;
      Alias_Notify : Context_Driver.Result := Context_Driver.Rejected;
      function Pixel_Read_Owner return Boolean is
        (not Runtime_Fault and then Context_Owner and then
         Draw_Completed = Initial_Completion.Complete and then
         Pixels_Match and then Image_Read and then
         Barrier_Completed = Initial_Completion.Complete and then
         Alias_Completed = Initial_Completion.Complete and then
         Final_Disable = Context_Wait.Complete and then
         Context_Pool.State (Contexts, Render_Context_ID) = Context_Life.Disabled);
      function Pixel_View is new Submission_Buffers.Completed_Pixel_View (Pixel_Read_Owner);
   begin
      if Context_Started or else not Context_Owner then return; end if;
      CPU_Coherence_Checked := False;
      GPU_Coherence_Checked := False;
      Context_Started := True;
      declare
         WM : constant Unsigned_32 := Context_Input.Read_WM_Chicken2;
      begin
         Context_Init := Intel_GPU_ADLN_Context_Init.Build
           (Context_Owner and then WM /= Unsigned_32'Last, WM);
         if not Context_Init.Valid then
            Context_Pool.Fail (Contexts); Runtime_Fault := True;
            Publish_Snapshot ("intel-gpu: context initialization input unavailable; backing retained");
            return;
         end if;
      end;
      Initial_Completion.Arm (Initial_Attempt, Completed);
      if Completed /= Initial_Completion.Ready then
         Context_Pool.Fail (Contexts); Runtime_Fault := True;
         Publish_Snapshot ("intel-gpu: initialization marker arm " &
           Initial_Completion.Result'Image (Completed) & "; backing retained");
         return;
      end if;
      Initial_Ring.Publish (Context_Init, Published);
      if not Published then
         Initial_Completion.Fail (Initial_Attempt);
         Context_Pool.Fail (Contexts); Runtime_Fault := True;
         Publish_Snapshot ("intel-gpu: initialization ring publication failed; backing retained");
         return;
      end if;
      -- One retained RCS0 context; transport assigns diagnostic wire IDs.
      Initial_Ring.Prepare_Copy_Source (Copy_Prepared);
      -- No completion-page maintenance until the first batch has finished.
      -- Only one four-word scheduling response is outstanding at a time.
      -- This bring-up policy explicitly requests preempt-to-idle on quantum
      -- expiry; it is a driver policy choice, not a probed hardware property.
      Context_Pool.Open (Contexts, Submission_GPU_Start,
        Address_Layout.Runtime_First, 1000, 500_000, True,
        Render_Context_ID, Published);
      if not Published then
         Context_Pool.Fail (Contexts); Runtime_Fault := True;
         Publish_Snapshot ("intel-gpu: context table allocation failed; backing retained");
         return;
      end if;
      for Action in Context_Life.Register_Context .. Context_Life.Set_Policy loop
         Context_Pool.Submit (Contexts, Render_Context_ID, Action, Status);
         if Status /= Context_Driver.Queued then
            Context_Pool.Fail (Contexts); Runtime_Fault := True;
            Publish_Snapshot ("intel-gpu: context " & Context_Life.Operation'Image (Action) &
              " " & Context_Driver.Result'Image (Status) & "; backing retained");
            return;
         end if;
      end loop;
      Context_Wait.Execute (Contexts, Render_Context_ID, Context_Life.Enable, 1_000_000, Wait_Status);
      if Wait_Status /= Context_Wait.Complete then
         Runtime_Fault := True;
         Publish_Snapshot ("intel-gpu: context enable " & Context_Wait.Result'Image (Wait_Status));
         return;
      end if;
      Initial_Completion.Wait (Initial_Attempt, 1_000_000, Completed);
      if Completed = Initial_Completion.Complete and then Copy_Prepared then
         Initial_Ring.Read_Copy_Result (Copy_Value, Copy_Read);
      end if;
      if Completed = Initial_Completion.Complete and then Live_Coherent_Ready then
         -- Reuse the validated command encoding/settings, changing only the
         -- final completion immediate. Never overwrite the first segment or
         -- clear the GPU marker on the CPU. This is still a fixed probe, not
         -- an application-controlled batch submission interface.
         Repeat_Segment := Context_Init;
         Repeat_Segment.Words (90) := 2;
         Initial_Completion.Arm (Repeat_Attempt, Repeat_Completed, 1, 2);
         if Repeat_Completed = Initial_Completion.Ready then
            Live_Ring.Append (Live_Channel, Repeat_Segment, Repeat_Published);
            if Repeat_Published then
               Context_Pool.Notify_Work (Contexts, Render_Context_ID, True, Repeat_Notify);
               if Repeat_Notify = Context_Driver.Queued then
                  Initial_Completion.Wait (Repeat_Attempt, 1_000_000, Repeat_Completed);
               else
                  Initial_Completion.Fail (Repeat_Attempt);
                  Repeat_Completed := Initial_Completion.Event_Failed;
               end if;
            else
               Initial_Completion.Fail (Repeat_Attempt);
               Repeat_Completed := Initial_Completion.Rejected;
            end if;
         end if;
      end if;
      Repeat_Tail := Live_Ring.Tail (Live_Channel);
      if Repeat_Completed = Initial_Completion.Complete and then Live_Coherent_Ready then
         -- This dedicated slot must still contain its initialized value.
         -- Never clear it on the CPU after GPU publication.
         Initial_Ring.Read_L3_Result (L3_Value, L3_Parameters, L3_Read);
         L3_Fresh := L3_Read and then L3_Value = 0 and then L3_Parameters = 0;
         L3_Read := False;
         if L3_Fresh then
            Initial_Completion.Arm (L3_Attempt, L3_Completed, 2, 3);
            if L3_Completed = Initial_Completion.Ready then
               Live_Ring.Append (Live_Channel, Intel_GPU_ADLN_Context_Init.Build_L3 (3), L3_Published);
               if L3_Published then
                  Context_Pool.Notify_Work (Contexts, Render_Context_ID, True, L3_Notify);
                  if L3_Notify = Context_Driver.Queued then
                     Initial_Completion.Wait (L3_Attempt, 1_000_000, L3_Completed);
                  else
                     Initial_Completion.Fail (L3_Attempt);
                     L3_Completed := Initial_Completion.Event_Failed;
                  end if;
               else
                  Initial_Completion.Fail (L3_Attempt);
                  L3_Completed := Initial_Completion.Rejected;
               end if;
            end if;
         end if;
      end if;
      -- No pending work remains after sequence3. Read GPU-produced samples
      -- before terminal disable; the immutable drawing batch is already
      -- published, so no CPU rewrite or context re-enable is needed.
      if L3_Completed = Initial_Completion.Complete then
         Initial_Ring.Read_L3_Result (L3_Value, L3_Parameters, L3_Read);
         L3_Matches := L3_Read and then Intel_GPU_ADLN_L3.Render_Allocation_Matches
           (Intel_GPU_ADLN_L3.Decode_Allocation (L3_Value));
         L3_URB_KiB := Intel_GPU_ADLN_L3.Probe_URB_KiB
           (L3_Matches and then L3_Fresh and then Context_Owner and then
              PCI_Device = 16#46D2# and then Steering.Valid,
            Intel_GPU_ADLN_L3.Decode_Fuse (Steering_First.L3_Disable),
            Intel_GPU_ADLN_L3.Decode_Parameters (L3_Parameters),
            Intel_GPU_ADLN_L3.Decode_Allocation (L3_Value));
         Initial_Ring.Read_Batch_Result (Batch_Value, Batch_Read);
         if L3_URB_KiB = Intel_GPU_Submission_Image.Draw_URB_KiB and then
           Batch_Read and then Batch_Value =
             Unsigned_64 (Intel_GPU_Submission_Image.Batch_Probe_Value) and then
           Live_Coherent_Ready and then Context_Owner
         then
            Initial_Ring.Read_Pixels (Pixels, Pixels_Read);
            Draw_Target_Clear := Pixels_Read and then (for all P of Pixels => P = 0);
            Pixels_Read := False;
            if Draw_Target_Clear then
               declare
                  WM : constant Unsigned_32 := Context_Input.Read_WM_Chicken2;
                  Segment : constant Intel_GPU_ADLN_Context_Init.Segment :=
                    Intel_GPU_ADLN_Context_Init.Build_Draw
                      (Context_Owner and then WM /= Unsigned_32'Last, WM, 4);
               begin
                  if Segment.Valid then
                     Initial_Completion.Arm (Draw_Attempt, Draw_Completed, 3, 4);
                     if Draw_Completed = Initial_Completion.Ready then
                        Live_Ring.Append (Live_Channel, Segment, Draw_Published);
                        if Draw_Published then
                           Context_Pool.Notify_Work (Contexts, Render_Context_ID, True, Draw_Notify);
                           if Draw_Notify = Context_Driver.Queued then
                              Initial_Completion.Wait (Draw_Attempt, 1_000_000, Draw_Completed);
                           else
                              Initial_Completion.Fail (Draw_Attempt);
                              Draw_Completed := Initial_Completion.Event_Failed;
                           end if;
                        else
                           Initial_Completion.Fail (Draw_Attempt);
                           Draw_Completed := Initial_Completion.Rejected;
                        end if;
                     end if;
                  end if;
               end;
            end if;
         end if;
      end if;
      -- Preserve the draw sequence (4); exercise the standalone barrier as 5.
      -- Completion here does not authorize page-table updates or memory reuse.
      if Draw_Completed = Initial_Completion.Complete and then Live_Coherent_Ready then
         Initial_Completion.Arm (Barrier_Attempt, Barrier_Completed, 4, 5);
         if Barrier_Completed = Initial_Completion.Ready then
            Live_Ring.Append (Live_Channel, Intel_GPU_ADLN_Barrier.Build (5), Barrier_Published);
            if Barrier_Published then
               Context_Pool.Notify_Work (Contexts, Render_Context_ID, True, Barrier_Notify);
               if Barrier_Notify = Context_Driver.Queued then
                  Initial_Completion.Wait (Barrier_Attempt, 1_000_000, Barrier_Completed);
               else
                  Initial_Completion.Fail (Barrier_Attempt);
                  Barrier_Completed := Initial_Completion.Event_Failed;
               end if;
            else
               Initial_Completion.Fail (Barrier_Attempt);
               Barrier_Completed := Initial_Completion.Rejected;
            end if;
         end if;
      end if;
      -- Capture before scheduling-disable adds another CT message or changes
      -- context state. Saved pointers may lag; neither snapshot is atomic.
      Live_Ring.Read_Saved_Pointers (Live_Channel, Saved_Head, Saved_Tail, Saved_OK);
      CT_Send_IO.Read_Descriptor (H2G_Head, H2G_Tail, H2G_Status, H2G_OK);
      -- Preserve the healthy writer across a controlled VM update. This hold
      -- prevents both tail publication and notification; it is not a drain.
      Context_Pool.Hold_Work (Contexts, Render_Context_ID, Update_Held);
      if not Update_Held then Live_Ring.Fail (Live_Channel); end if;
      -- Even after a marker timeout, try to disable a still-healthy enabled
      -- context before slow logging. A faulted transport cannot be trusted;
      -- in every case backing stays retained and no further work is submitted.
      Context_Wait.Execute (Contexts, Render_Context_ID, Context_Life.Disable, 1_000_000, Wait_Status);
      if Wait_Status = Context_Wait.Complete and Completed = Initial_Completion.Complete then
         Initial_Ring.Read_Batch_Result (Batch_Value, Batch_Read);
         if Draw_Completed = Initial_Completion.Complete then
            -- The pre-draw clear check loaded these CPU cache lines. Sample
            -- before any post-draw target CLFLUSH; later maintenance remains
            -- mandatory. This supplements the documented platform contract;
            -- the sample alone never establishes HOST_COHERENT admission.
            Initial_Ring.Sample_Pixels_No_Flush (Unflushed_Pixels, Unflushed_Read);
            Initial_Ring.Read_Pixels (Pixels, Pixels_Read);
            Unflushed_Match := Unflushed_Read and then Pixels_Read and then
              (for all I in Pixels'Range => Unflushed_Pixels (I) = Pixels (I));
            Initial_Ring.Read_Image (Image, Image_Read);
            if Image_Read then
               for Pixel of Image loop
                  if Pixel /= 0 then Image_Nonzero := Image_Nonzero + 1; end if;
                  Image_Hash := (Image_Hash xor Pixel) * 16#01000193#;
               end loop;
            end if;
            -- Linear B8G8R8A8_UNORM target, opaque red fragment shader.
            Pixels_Match := Pixels_Read and then Pixels (0) = 16#FFFF0000# and then
              (for all I in 1 .. 4 => Pixels (I) = 0);
         end if;
      end if;
      if not Update_Held or Wait_Status /= Context_Wait.Complete or Completed /= Initial_Completion.Complete or
        Repeat_Completed /= Initial_Completion.Complete or
        L3_Completed /= Initial_Completion.Complete or not L3_Matches or L3_URB_KiB = 0 or
        Draw_Completed /= Initial_Completion.Complete or not Pixels_Match or not Image_Read or
        Barrier_Completed /= Initial_Completion.Complete or
        not Batch_Read or Batch_Value /=
          Unsigned_64 (Intel_GPU_Submission_Image.Batch_Probe_Value)
      then
         Context_Pool.Fail (Contexts); Runtime_Fault := True;
      end if;
      if not Runtime_Fault then
         -- Marker5 is the completed flush/invalidate barrier, not merely a
         -- scheduling ACK. New work is held until publication, invalidation
         -- and acknowledged enable all succeed. All generations are retained.
         TLB_Armed := True;
         Update_At := Allocation;
         if Boot_Update_Allocation.Ready and then
           Boot_Update_Allocation.Bytes = 4 * 4096
         then Update_At := Ownership; end if;
         if Boot_Update_Allocation.Ready and then
           Boot_Update_Allocation.Bytes = 4 * 4096 and then TLB_Probe_Owner
         then
            for P in Submission_Buffers.VM.Page_Number loop
               Update_Tables (P) := Intel_GPU_Buffer_Reply.Page_Address
                 (Boot_Update_Allocation, Unsigned_64 (P - 1) * 4096);
               Update_Mappings (P) :=
                 (Boot_Update_Allocation.CPU_Address + Unsigned_64 (P - 1) * 4096,
                  Update_Tables (P));
            end loop;
            Update_At := Prepare;
            Submission_Buffers.Prepare_Boot_Update
              (Submission_State, Boot_Update_Candidate, Update_Tables, Update_Ready);
            if Update_Ready then
               -- New private VA alias of the already-authorized batch
               -- page. No device or firmware memory is added to this VM.
               Update_At := Map_Alias;
               Submission_Buffers.VM.Map_Page
                 (Boot_Update_Candidate, 16#208000#,
                  Intel_GPU_Buffer_Reply.Page_Address (Submission_Allocation,
                    Intel_GPU_Submission_Backing.Offsets
                      (Intel_GPU_Submission_Backing.Batch_Buffer) -
                    Intel_GPU_Submission_Backing.First),
                  Intel_GPU_ADLN_PPGTT.Write_Back, Intel_GPU_ADLN_PPGTT.Read_Write,
                  Update_Ready);
            end if;
            if Update_Ready then
               Update_At := Seal;
               Submission_Buffers.VM.Seal (Boot_Update_Candidate, Update_Ready);
            end if;
            if Update_Ready then
               Update_At := Publish;
               Publish_Snapshot ("intel-gpu: stable-root update beginning; context disabled");
               Boot_Updates.Publish_Boot_Tables
                 (Submission_State, Boot_Update_Candidate, Update_Mappings, Update_Ready);
            end if;
         end if;
         Publish_Snapshot ("intel-gpu: stable-root update published=" &
           Boolean'Image (Update_Ready) & "; alias=00208000; backing retained");
         if Update_Ready then
            Update_At := Invalidate;
            Publish_Snapshot ("intel-gpu: native TLB invalidation beginning (PPGTT updated)");
            TLB_Probe.Execute (TLB_Attempt, TLB_Status);
            if TLB_Status = TLB_Probe.Complete then
               -- The flush marker completed, scheduling is disabled and the
               -- work hold remains owned. Exercise the GuC MMIO completion
               -- path without replacing any GGTT PTE or releasing backing.
               Publish_Snapshot ("intel-gpu: GuC MMIO invalidation wait beginning; context held");
               GuC_Wait.Execute (GuC_Attempt, GuC_Status);
               if GuC_Status = GuC_Wait.Complete then Update_At := Finished; end if;
            end if;
         end if;
         TLB_Armed := False;
         if TLB_Status /= TLB_Probe.Complete or else GuC_Status /= GuC_Wait.Complete then
            Context_Pool.Fail (Contexts); Runtime_Fault := True;
         end if;
      end if;
      Publish_Snapshot ("intel-gpu: boot VM update stage=" & Update_Stage'Image (Update_At));
      Publish_Snapshot ("intel-gpu: GuC MMIO invalidation wait " &
        GuC_Wait.Result'Image (GuC_Status) & "; backing retained");
      if Update_At = Prechecks then
         Publish_Snapshot ("intel-gpu: boot VM gates held=" & Boolean'Image (Update_Held) &
           " disable=" & Wait_Name (Wait_Status) & " barrier=" & Completion_Name (Barrier_Completed));
         Publish_Snapshot ("intel-gpu: boot VM gates pixels=" & Boolean'Image (Pixels_Match) &
           " image=" & Boolean'Image (Image_Read) & " L3=" & Boolean'Image (L3_Matches) &
           " URB=" & Natural'Image (L3_URB_KiB));
      end if;
      Publish_Snapshot ("intel-gpu: native TLB invalidation " &
        (case TLB_Status is
           when TLB_Probe.Rejected => "REJECTED",
           when TLB_Probe.Complete => "COMPLETE",
           when TLB_Probe.Ownership_Lost => "OWNERSHIP-LOST",
           when TLB_Probe.Write_Failed => "WRITE-FAILED",
           when TLB_Probe.Read_Failed => "READ-FAILED",
           when TLB_Probe.Invalid_Clock => "INVALID-CLOCK",
           when TLB_Probe.Timed_Out => "TIMED-OUT") & "; backing retained");
      if not Runtime_Fault and then Update_Ready and then TLB_Status = TLB_Probe.Complete then
         Context_Wait.Execute
           (Contexts, Render_Context_ID, Context_Life.Enable, 1_000_000, Resume_Status);
         if Resume_Status = Context_Wait.Complete then
            Context_Pool.Release_Work (Contexts, Render_Context_ID, Update_Released);
            if Update_Released then
               Update_Held := False;
               Initial_Completion.Arm (Alias_Attempt, Alias_Completed, 5, 6);
               if Alias_Completed = Initial_Completion.Ready then
                  declare
                     WM : constant Unsigned_32 := Context_Input.Read_WM_Chicken2;
                     Segment : constant Intel_GPU_ADLN_Context_Init.Segment :=
                       Intel_GPU_ADLN_Context_Init.Build_Batch
                         (Live_Backing_Owner and then WM /= Unsigned_32'Last,
                          WM, 6, 16#208000#);
                  begin
                     Live_Ring.Append (Live_Channel, Segment, Alias_Published);
                  end;
                  if Alias_Published then
                     Context_Pool.Notify_Work (Contexts, Render_Context_ID, True, Alias_Notify);
                     if Alias_Notify = Context_Driver.Queued then
                        Initial_Completion.Wait (Alias_Attempt, 1_000_000, Alias_Completed);
                     end if;
                  end if;
               end if;
            end if;
         end if;
         -- Best effort disable even when the resumed work failed. No backing
         -- is recycled, and failure permanently closes the context below.
         Context_Wait.Execute
           (Contexts, Render_Context_ID, Context_Life.Disable, 1_000_000, Final_Disable);
         if Alias_Completed /= Initial_Completion.Complete or else
           Final_Disable /= Context_Wait.Complete
         then Context_Pool.Fail (Contexts); Runtime_Fault := True; end if;
      end if;
      Live_Ring.Fail (Live_Channel);
      -- Retain only a fully completed, disabled diagnostic run. A later
      -- query still requires the current owner and authenticated live session.
      if not Runtime_Fault and then Publication_Owner_Ready and then Context_Owner
        and then Final_Disable = Context_Wait.Complete
        and then Context_Pool.State (Contexts, Render_Context_ID) = Context_Life.Disabled
      then
         CPU_Coherence_Checked := Completed = Initial_Completion.Complete and then
           Copy_Prepared and then Copy_Read and then
           Copy_Value = Intel_GPU_Submission_Image.Copy_Probe_Value;
         GPU_Coherence_Checked := Draw_Completed = Initial_Completion.Complete and then
           Draw_Target_Clear and then Pixels_Match and then Unflushed_Match;
      end if;
      Publish_Snapshot ("intel-gpu: owned-WB qualification CPU-to-GPU=" &
        Boolean'Image (CPU_Coherence_Checked) & " GPU-to-CPU=" &
        Boolean'Image (GPU_Coherence_Checked) & " (live session/owner still required)");
      Completed_Probe_Pixels := Pixel_View (Submission_State);
      Publish_Snapshot ("intel-gpu: completed pixel view ready=" &
        Boolean'Image (Completed_Probe_Pixels.Ready) & " (NOT exported)");
      Publish_Snapshot ("intel-gpu: updated-VM batch published=" & Boolean'Image (Alias_Published) &
        " completion=" & Completion_Name (Alias_Completed) &
        " disable=" & Wait_Name (Final_Disable) & "; alias=00208000");
      Publish_Snapshot ("intel-gpu: L3 ring fresh=" & Boolean'Image (L3_Fresh) &
        " published=" & Boolean'Image (L3_Published) &
        " notify=" & Notify_Name (L3_Notify) & " completion=" & Completion_Name (L3_Completed));
      Publish_Snapshot ("intel-gpu: L3 allocation read=" & Boolean'Image (L3_Read) &
        " raw=" & Hex (L3_Value) & " fields-match=" & Boolean'Image (L3_Matches) &
        " (before draw)");
      Publish_Snapshot ("intel-gpu: L3 parameters=" & Hex (L3_Parameters) &
        " admitted URB KiB=" & Natural'Image (L3_URB_KiB));
      Publish_Snapshot ("intel-gpu: draw target-clear=" & Boolean'Image (Draw_Target_Clear) &
        " published=" & Boolean'Image (Draw_Published) &
        " notify=" & Notify_Name (Draw_Notify) & " completion=" & Completion_Name (Draw_Completed));
      Publish_Snapshot ("intel-gpu: draw pixels read=" & Boolean'Image (Pixels_Read) &
        " center=" & Hex (Pixels (0)) & " match=" & Boolean'Image (Pixels_Match));
      Publish_Snapshot ("intel-gpu: draw no-CPU-flush read=" & Boolean'Image (Unflushed_Read) &
        " center=" & Hex (Unflushed_Pixels (0)) &
        " matches-flushed=" & Boolean'Image (Unflushed_Match));
      Publish_Snapshot ("intel-gpu: draw corners=" & Hex (Pixels (1)) & "/" &
        Hex (Pixels (2)) & "/" & Hex (Pixels (3)) & "/" & Hex (Pixels (4)));
      Publish_Snapshot ("intel-gpu: draw image read=" & Boolean'Image (Image_Read) &
        " nonzero=" & Natural'Image (Image_Nonzero) & " word-hash=" & Hex (Image_Hash));
      Publish_Snapshot ("intel-gpu: private batch read=" & Boolean'Image (Batch_Read) &
        " value=" & Unsigned_64'Image (Batch_Value) &
        "; expected=" & Unsigned_32'Image (Intel_GPU_Submission_Image.Batch_Probe_Value));
      Publish_Snapshot ("intel-gpu: CPU-no-flush copy prepared=" & Boolean'Image (Copy_Prepared) &
        " read=" & Boolean'Image (Copy_Read) & " value=" & Hex (Copy_Value) &
        " match=" & Boolean'Image (Copy_Read and then
          Copy_Value = Intel_GPU_Submission_Image.Copy_Probe_Value));
      Publish_Snapshot ("intel-gpu: initialization GPU marker " &
        Completion_Name (Completed) & "; disable " &
        Wait_Name (Wait_Status));
      Publish_Snapshot ("intel-gpu: initialization marker value=" &
        Unsigned_64'Image (Initial_Completion.Last_Marker (Initial_Attempt)) & " reads=" &
        Natural'Image (Initial_Completion.Marker_Reads (Initial_Attempt)));
      Publish_Snapshot ("intel-gpu: repeated ring published=" & Boolean'Image (Repeat_Published) &
        " notify=" & Notify_Name (Repeat_Notify) &
        " completion=" & Completion_Name (Repeat_Completed));
      Publish_Snapshot ("intel-gpu: repeated marker value=" &
        Unsigned_64'Image (Initial_Completion.Last_Marker (Repeat_Attempt)) &
        " reads=" & Natural'Image (Initial_Completion.Marker_Reads (Repeat_Attempt)) &
        " tail=" & Unsigned_32'Image (Repeat_Tail));
      Publish_Snapshot ("intel-gpu: standalone barrier published=" & Boolean'Image (Barrier_Published) &
        " notify=" & Notify_Name (Barrier_Notify) &
        " completion=" & Completion_Name (Barrier_Completed));
      Publish_Snapshot ("intel-gpu: standalone barrier marker=" &
        Unsigned_64'Image (Initial_Completion.Last_Marker (Barrier_Attempt)) &
        " reads=" & Natural'Image (Initial_Completion.Marker_Reads (Barrier_Attempt)) &
        " tail=" & Unsigned_32'Image (Live_Ring.Tail (Live_Channel)) & " (before PPGTT update)");
      Publish_Snapshot ("intel-gpu: repeat saved-context sampled=" & Boolean'Image (Saved_OK) &
        " head=" & Hex (Saved_Head) & " tail=" & Hex (Saved_Tail) & " (NOT live engine pointers)");
      Publish_Snapshot ("intel-gpu: repeat H2G sampled=" & Boolean'Image (H2G_OK) &
        " head=" & Hex (H2G_Head) & " tail=" & Hex (H2G_Tail) &
        " status=" & Hex (H2G_Status) & " (before disable)");
   end Start_Render_Context;
   procedure Service_Context_Events is
      Item : CT_Receive.Message;
      Status : CT_Receive.Result;
      Dispatched : Context_Driver.Result;
      use type CT_Receive.Result;
      use type Context_Driver.Result;
   begin
      if not Context_Started or else Runtime_Fault then return; end if;
      if not Context_Owner then
         Context_Pool.Fail (Contexts); Runtime_Fault := True;
         Publish_Snapshot ("intel-gpu: context owner lost; backing retained");
         return;
      end if;
      for Index in 1 .. 8 loop
         Poll_CT_Reply (Item, Status);
         exit when Status = CT_Receive.Empty;
         if Status /= CT_Receive.Received or else Item.Length = 0 then
            Context_Pool.Fail (Contexts); Runtime_Fault := True;
            Publish_Snapshot ("intel-gpu: context receive failed; backing retained");
            return;
         end if;
         Dispatch_Context_Event (
           Context_Event.Words (Item.Payload (1 .. Item.Length)), Item.Fence, Dispatched);
         if Dispatched in Context_Driver.Faulted | Context_Driver.Rejected then
            Runtime_Fault := True;
            Publish_Snapshot ("intel-gpu: late context event fault; backing retained");
            return;
         end if;
      end loop;
   end Service_Context_Events;
   function Read_Register (Address : Unsigned_64) return Unsigned_32 is
      Value : Unsigned_32 with Import, Volatile_Full_Access,
        Address => System.Storage_Elements.To_Address
          (System.Storage_Elements.Integer_Address (Address));
   begin
      return Value;
   end Read_Register;
   procedure Capture is new Intel_GPU_Observation.Capture (Read_Register);
   function Probe_Forcewake return String is
      Write_Base : constant Unsigned_64 := 16#6020_0000#;
      Token : constant Unsigned_64 := 16#4947_0003#;
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Now : Unsigned_64;
      Activity : Activity_Result;
      Granted : Boolean := False;
      pragma Unreferenced (Activity);
      function Clock return Unsigned_64 is (syscall (SYSCALL_GETTIME));
      function Read_Ack (Offset : Unsigned_32) return Unsigned_32 is
         Allowed : Boolean := False;
      begin
         for D in Intel_GPU_ADLN_Inventory.Domain loop
            Allowed := Allowed or Offset = Intel_GPU_ADLN_Inventory.Ack_Register (D);
         end loop;
         if not Allowed then return Unsigned_32'Last; end if;
         return Read_Register (Register_Virtual_Base + Unsigned_64 (Offset));
      end Read_Ack;
      procedure Write_Request (Offset, Value : Unsigned_32) is
         Register : Unsigned_32 with Import, Volatile_Full_Access,
           Address => System.Storage_Elements.To_Address
             (System.Storage_Elements.Integer_Address (Write_Base + Unsigned_64 (Offset mod 4096)));
         Allowed : Boolean := False;
      begin
         for D in Intel_GPU_ADLN_Inventory.Domain loop
            Allowed := Allowed or Offset = Intel_GPU_ADLN_Inventory.Request_Register (D);
         end loop;
         if not Allowed or else
           (Value /= 16#10001# and Value /= 16#10000#)
         then raise Program_Error; end if;
         Register := Value;
      end Write_Request;
      procedure Pause is
         Ignored : Unsigned_64;
         pragma Unreferenced (Ignored);
      begin
         Ignored := syscall (SYSCALL_SLEEP, 1);
      end Pause;
      package FW is new Intel_GPU_Forcewake (Read_Ack, Write_Request, Pause, Clock,
         Intel_GPU_ADLN_Inventory.Request_Register (Intel_GPU_ADLN_Inventory.GT),
         Intel_GPU_ADLN_Inventory.Ack_Register (Intel_GPU_ADLN_Inventory.GT));
      Object : FW.Lease;
      package All_FW is new Intel_GPU_ADLN_Forcewake (Read_Ack, Write_Request, Pause, Clock);
      All_OK : Boolean;
      Status : FW.Result;
      First_Fuse, Second_Fuse : Unsigned_32;
      function Read_Steering return Intel_GPU_ADLN_Steering.Fuse_Snapshot is
         Sample : Intel_GPU_ADLN_Steering.Fuse_Snapshot;
      begin
         Sample.Slice_Enable := Read_Register (Register_Virtual_Base +
           Unsigned_64 (Intel_GPU_ADLN_Steering.Slice_Register));
         Sample.DSS_Enable := Read_Register (Register_Virtual_Base +
           Unsigned_64 (Intel_GPU_ADLN_Steering.DSS_Register));
         Sample.L3_Disable := Read_Register (Register_Virtual_Base +
           Unsigned_64 (Intel_GPU_ADLN_Steering.L3_Register));
         return Sample;
      end Read_Steering;
      use type FW.Result;
      function Describe (Value : FW.Result) return String is
      begin
         case Value is
            when FW.Ready => return "ready";
            when FW.Timed_Out => return "deadline-expired";
            when FW.Poll_Exhausted => return "poll-budget-exhausted";
            when FW.Invalid_MMIO => return "invalid-MMIO";
            when FW.Invalid_Clock => return "invalid-clock";
            when FW.Invalid_State => return "invalid-state";
         end case;
      end Describe;
   begin
      Msg.tag := (16#022A#, 0, 0, 0);
      if not capSubmit (15, Msg, Token) then return "grant-submit-failed"; end if;
      loop
         Now := Clock;
         if Now < Started or else Now - Started >= 30_000 then
            return "grant-timeout";
         end if;
         if Intel_GPU_Diagnostics.Poll_Driver (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               --  Devmgr is still collecting other drivers' ready messages.
               --  This completed request minted nothing; retry within budget.
               if not capSubmit (15, Msg, Token) then
                  return "grant-retry-failed";
               end if;
            else
            Granted := Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F000#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0];
            exit;
            end if;
         end if;
         Activity := Wait_For_Activity_Until
           (if Now < Unsigned_64'Last then Now + 1 else Now);
      end loop;
      if not Granted then return "grant-denied"; end if;
      --  Separate alias: retain the original read-only register mapping.
      if syscall (SYSCALL_MAP_DEVICE, Plan.Physical_Base + 16#A000#,
                  Write_Base, 1, 0) /= 0
      then return "write-map-denied"; end if;
      FW.Acquire (Object, 100, Status);
      if Status /= FW.Ready then return "acquire-" & Describe (Status); end if;
      -- Fuse register lies in the existing read-only mapping. Hold GT awake
      -- for both reads, and do not admit an inventory after a failed release.
      First_Fuse := Read_Register (Register_Virtual_Base +
        Unsigned_64 (Intel_GPU_ADLN_Inventory.Fuse_Register));
      Second_Fuse := Read_Register (Register_Virtual_Base +
        Unsigned_64 (Intel_GPU_ADLN_Inventory.Fuse_Register));
      -- Same existing read-only fuse page. No MCR selector writes. Admit the
      -- topology only after successful release and validated ADL-N identity.
      Steering_First := Read_Steering;
      Steering_Second := Read_Steering;
      FW.Release (Object, 100, Status);
      if Status = FW.Ready and First_Fuse = Second_Fuse then
         Media_Fuse := First_Fuse;
         Engine_Inventory := Intel_GPU_ADLN_Inventory.Decode
           (Unsigned_16 (Request.words (1) and 16#FFFF#),
            Unsigned_16 (Shift_Right (Request.words (1), 16) and 16#FFFF#),
            Media_Fuse);
         if Engine_Inventory.Valid then
            Steering := Intel_GPU_ADLN_Steering.Decode_Stable
              (Steering_First, Steering_Second);
         end if;
      end if;
      if Status = FW.Ready and Engine_Inventory.Valid then
         All_FW.Acquire
           (Unsigned_16 (Request.words (1) and 16#FFFF#),
            Unsigned_16 (Shift_Right (Request.words (1), 16) and 16#FFFF#),
            Media_Fuse, All_OK);
         if not All_OK then return "release-ready; domains-acquire-failed"; end if;
         Doorbell_First := Read_Register (Register_Virtual_Base +
           Unsigned_64 (Intel_GPU_ADS_System_Info.Doorbell_Register));
         Doorbell_Second := Read_Register (Register_Virtual_Base +
           Unsigned_64 (Intel_GPU_ADS_System_Info.Doorbell_Register));
         All_FW.Release (All_OK);
         if not All_OK then return "release-ready; domains-release-failed"; end if;
         ADS_Info := Intel_GPU_ADS_System_Info.Build
           (Engine_Inventory, Steering, Doorbell_First, Doorbell_Second);
         return "release-ready; domains-release-ready";
      end if;
      return "release-" & Describe (Status);
   end Probe_Forcewake;
   function Hex (Value : Unsigned_32) return String is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Text : String (1 .. 8);
   begin
      for Index in Text'Range loop
         Text (Index) := Hex_Digits
           (Natural (Shift_Right (Value, (8 - Index) * 4) and 15) + 1);
      end loop;
      return Text;
   end Hex;
   function Hex64 (Value : Unsigned_64) return String is
     (Hex (Unsigned_32 (Shift_Right (Value, 32))) &
      Hex (Unsigned_32 (Value and 16#FFFF_FFFF#)));
   function Inspect_GGTT return String is
      Token : constant Unsigned_64 := 16#4947_0004#;
      Virtual : constant Unsigned_64 := 16#6040_0000#;
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Start : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Now, Physical : Unsigned_64;
      Activity : Activity_Result;
      Low, High : Unsigned_32;
      Present : Natural := 0;
      pragma Unreferenced (Activity);
   begin
      if Table_Bytes = 0 then return "unavailable"; end if;
      Msg.tag := (16#022B#, 0, 0, 0);
      if not capSubmit (15, Msg, Token) then return "submit-failed"; end if;
      loop
         Now := syscall (SYSCALL_GETTIME);
         if Now < Start or else Now - Start >= 30_000 then return "timeout"; end if;
         if Intel_GPU_Diagnostics.Poll_Driver (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (16#F002#, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               if not capSubmit (15, Msg, Token) then return "retry-failed"; end if;
            else
               if Receipt.status /= COMPLETION_OK or else
                 Receipt.msg.tag /= (16#F000#, 2, 0, 0) or else
                 Receipt.msg.words (1 .. 3) /= [Table_Bytes, 0, 0]
               then return "grant-denied"; end if;
               Physical := Receipt.msg.words (0);
               exit;
            end if;
         end if;
         Activity := Wait_For_Activity_Until
           (if Now < Unsigned_64'Last then Now + 1 else Now);
      end loop;
      if Physical = 0 or else Physical mod 4096 /= 0 or else
        Physical > Unsigned_64'Last - Table_Bytes
      then return "map-denied"; end if;
      -- Every supported table is a multiple of 2MiB. Map disjoint chunks
      -- below MAP_DEVICE's per-call limit; keep every alias read-only.
      -- A partial failure leaves only process-lived read-only aliases and
      -- returns before inspecting any table entry. No retry/remap here.
      for Chunk in Unsigned_64 range 0 .. Table_Bytes / (2 * 1024 * 1024) - 1 loop
         if syscall (SYSCALL_MAP_DEVICE,
           Physical + Chunk * (2 * 1024 * 1024),
           Virtual + Chunk * (2 * 1024 * 1024), 512, 1) /= 0
         then return "map-denied"; end if;
      end loop;
      -- Diagnostic sampling, not an atomic table snapshot or a free-space map.
      GGTT_Inspection_Mapped := True;
      Low := Read_Register (Virtual);
      High := Read_Register (Virtual + 4);
      if Low = Unsigned_32'Last and then High = Unsigned_32'Last then
         return "invalid-MMIO";
      end if;
      for Index in Unsigned_64 range 0 .. Table_Bytes / 8 - 1 loop
         declare
            Entry_Low : constant Unsigned_32 := Read_Register (Virtual + Index * 8);
         begin
            -- A failed MMIO read can return all ones anywhere in the table,
            -- not just at entry zero. Do not count that as a present mapping.
            -- Read the high word only for this diagnostic sentinel; ordinary
            -- entries retain the existing single-read sampling cost.
            if Entry_Low = Unsigned_32'Last and then
              Read_Register (Virtual + Index * 8 + 4) = Unsigned_32'Last
            then return "invalid-MMIO entry=" & Unsigned_64'Image (Index); end if;
            if (Entry_Low and 1) /= 0 then
               Present := Present + 1;
            end if;
         end;
      end loop;
      return "first=" & Hex (High) & Hex (Low) & " present=" & Natural'Image (Present) &
        " scanned=" & Unsigned_64'Image (Table_Bytes / 8);
   end Inspect_GGTT;
begin
   debugPrint ("intel-gpu: awaiting device-scoped bootstrap" & ASCII.LF);
   declare
      First : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
      Last : CuBit.Monotonic.Reading;
      Moving : Boolean := False;
   begin
      if First.Available then
         for Poll in 1 .. 10_000 loop
            Last := CuBit.Monotonic.Read;
            exit when not Last.Available;
            exit when Last.Microseconds < First.Microseconds;
            if Last.Microseconds > First.Microseconds then Moving := True; exit; end if;
         end loop;
      end if;
      debugPrint ("intel-gpu: monotonic microsecond progress=" & Boolean'Image (Moving) & ASCII.LF);
   end;
   receive (Sender, Request);
   if Sender = 0 or else
     Sender /= getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DEVMGR) or else
     Request.authorityTag /= Intel_GPU_Boot.Broker_Tag or else
     Request.tag.label /= Intel_GPU_Boot.Configure_Label or else
     Request.tag.length /= 4 or else Request.tag.flags /= 0 or else
     Request.tag.reserved /= 0
   then
      debugPrint ("intel-gpu: bootstrap rejected" & ASCII.LF);
      return;
   end if;
   Plan := Intel_GPU_Boot.Decode
     ([Request.words (0), Request.words (1), Request.words (2), Request.words (3)]);
   GGC := Unsigned_16 (Shift_Right (Request.words (3), 16) and 16#FFFF#);
   Table_Bytes := Intel_GPU_GGTT.Table_Size (GGC);
   if Plan.Status /= Admitted then
      debugPrint ("intel-gpu: resource rejected " & Admission_Status'Image (Plan.Status) & ASCII.LF);
      return;
   end if;
   Intel_GPU_Render_Control.Bind
     (Render_Admission, Unsigned_64 (Sender), Intel_GPU_Boot.Broker_Tag);
   -- Capture identity only after sender/protocol/resource admission. Later
   -- IPC requests must not become the source of firmware startup identity.
   Capture_Probe_Recipient;
   PCI_Device := Unsigned_16 (Shift_Right (Request.words (1), 16) and 16#FFFF#);
   PCI_Revision := Unsigned_8 (Shift_Right (Request.words (1), 32) and 16#FF#);
   debugPrint ("intel-gpu: GGC=" & Hex (Unsigned_32 (GGC)) &
     " GGTT table bytes" & Unsigned_64'Image (Table_Bytes) & ASCII.LF);
   Result := syscall (SYSCALL_MAP_DEVICE, Plan.Physical_Base,
                      Register_Virtual_Base, Plan.Bytes / 4096, 1);
   if Result /= 0 then
      debugPrint ("intel-gpu: read-only mapping denied" & ASCII.LF);
      return;
   end if;
   debugPrint ("intel-gpu: read-only register mapping ready; firmware scanout retained" & ASCII.LF);
   Display_Power_Owned := Request_Authorization (16#022F#);
   Publish_Snapshot
     (if Display_Power_Owned then
        "intel-gpu: display power owner designated (NO power writes/reference yet)"
      else "intel-gpu: display power ownership unavailable");
   declare
      IRQ : constant Intel_GPU_PCI_Interrupts.Snapshot :=
        Intel_GPU_PCI_Interrupts.Unpack
          (Unsigned_8 (Shift_Right (Request.words (3), 32) and 16#7F#));
      function Flag (Value : Boolean) return String is
        (if Value then "yes" else "no");
   begin
      if not IRQ.Valid then
         Publish_Snapshot ("intel-gpu: PCI IRQ snapshot unavailable/invalid");
      else
         Publish_Snapshot ("intel-gpu: PCI INTx disabled=" & Flag (IRQ.INTx_Disabled));
         Publish_Snapshot ("intel-gpu: PCI MSI present=" & Flag (IRQ.MSI_Present) &
           " enabled=" & Flag (IRQ.MSI_Enabled));
         Publish_Snapshot ("intel-gpu: PCI MSI-X present=" & Flag (IRQ.MSIX_Present) &
           " enabled=" & Flag (IRQ.MSIX_Enabled) & " masked=" & Flag (IRQ.MSIX_Masked));
      end if;
   end;
   -- Decode v4 admits only ADLN and authenticated devmgr D0 evidence.
   -- This is observation, not a power reference or scanout takeover. No writes.
   Capture (Intel_GPU_Probe.Alder_Lake_N, True, Register_Virtual_Base,
            Plan.Bytes, Observation);
   if Observation.Captured then
      for Name in Intel_GPU_Observation.Register_Name loop
         debugPrint ("intel-gpu: snapshot " &
           (case Name is
              when Intel_GPU_Observation.Firmware_Power_Control => "FIRMWARE_POWER_CONTROL",
              when Intel_GPU_Observation.Driver_Power_Control => "DRIVER_POWER_CONTROL",
              when Intel_GPU_Observation.Display_DC_Control => "DC_STATE_EN",
              when Intel_GPU_Observation.Display_Fuse_Status => "DISPLAY_FUSE_STATUS") & "=" &
           Hex (Observation.Values (Name)) & ASCII.LF);
      end loop;
      declare
         Forcewake_Result : constant String := Probe_Forcewake;
         GGTT_Result : constant String := Inspect_GGTT;
         Firmware_Address : System.Address;
         Firmware_Bytes : Unsigned_64;
         Firmware_Plan : Intel_GPU_Firmware.Layout;
         Firmware_Status : Intel_GPU_Firmware_File.Load_Status;
      begin
      debugPrint ("intel-gpu: forcewake " & Forcewake_Result & ASCII.LF);
      debugPrint ("intel-gpu: GGTT read-only " & GGTT_Result & ASCII.LF);
      Intel_GPU_Firmware_File.Load
        (6, Firmware_Address, Firmware_Bytes, Firmware_Plan, Firmware_Status);
      debugPrint ("intel-gpu: firmware file " &
        Intel_GPU_Firmware_File.Name (Firmware_Status) &
        " bytes" & Unsigned_64'Image (Firmware_Bytes) &
        " code" & Unsigned_64'Image (Firmware_Plan.Code_Bytes) &
        " (NOT authenticated or uploaded)" & ASCII.LF);
      Publish_Snapshot ("intel-gpu: read-only snapshot; firmware=" &
        Hex (Observation.Values (Intel_GPU_Observation.Firmware_Power_Control)) &
        " driver=" & Hex (Observation.Values (Intel_GPU_Observation.Driver_Power_Control)) &
        "; scanout retained");
      Publish_Snapshot ("intel-gpu: display DC=" &
        Hex (Observation.Values (Intel_GPU_Observation.Display_DC_Control)) &
        " fuses=" & Hex (Observation.Values (Intel_GPU_Observation.Display_Fuse_Status)) &
        " (read-only; NOT a power reference)");
      declare
         Bits : String (1 .. 4) := (others => '-');
      begin
         for Pipe in Intel_GPU_Observation.Display_Pipe loop
            if Intel_GPU_Observation.Pipe_Request_State_Set
              (Observation.Values (Intel_GPU_Observation.Driver_Power_Control), Pipe)
            then Bits (Intel_GPU_Observation.Display_Pipe'Pos (Pipe) + 1) := 'Y'; end if;
         end loop;
         Publish_Snapshot
           ("intel-gpu: display PW request+state ABCD=" & Bits & " (snapshot only)");
      end;
      Publish_Snapshot ("intel-gpu: forcewake=" & Forcewake_Result);
      Publish_Snapshot ("intel-gpu: clock startup=" &
        Unsigned_64'Image (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 0)) &
        " HPET id=" & Hex (Unsigned_32 (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 1) and 16#FFFF_FFFF#)) &
        " period-fs=" & Unsigned_64'Image (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 2)));
      Publish_Snapshot ("intel-gpu: clock timer offset=" &
        Hex (Unsigned_32 (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 3) and 16#FFFF_FFFF#)) &
        " before=" & Hex (Unsigned_32 (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 4) and 16#FFFF_FFFF#)) &
        " after=" & Hex (Unsigned_32 (getInfo (SYSINFO_MONOTONIC_DIAGNOSTIC, 5) and 16#FFFF_FFFF#)));
      if Forcewake_Result = "release-ready; domains-release-ready" then
         declare
            Mapping_Result : constant String := Map_Reset_Pages;
         begin
            debugPrint ("intel-gpu: reset pages " & Mapping_Result & ASCII.LF);
            Publish_Snapshot ("intel-gpu: reset pages " & Mapping_Result);
         end;
      end if;
      Publish_Snapshot ("intel-gpu: media fuse=" & Hex (Media_Fuse) &
        " inventory-valid=" & Boolean'Image (Engine_Inventory.Valid));
      Publish_Snapshot ("intel-gpu: topology first slice=" & Hex (Steering_First.Slice_Enable) &
        " dss=" & Hex (Steering_First.DSS_Enable) & " l3-disable=" & Hex (Steering_First.L3_Disable));
      Publish_Snapshot ("intel-gpu: topology second slice=" & Hex (Steering_Second.Slice_Enable) &
        " dss=" & Hex (Steering_Second.DSS_Enable) & " l3-disable=" & Hex (Steering_Second.L3_Disable));
      if Steering.Valid then
         Publish_Snapshot ("intel-gpu: steering valid group=0 default=" &
           Natural'Image (Steering.Default_Instance) & " l3=" &
           Natural'Image (Intel_GPU_ADLN_Steering.Instance (Steering, 16#B100#)));
      else
         Publish_Snapshot ("intel-gpu: steering unavailable; not admitted");
      end if;
      if Engine_Inventory.Valid then
         for D in Intel_GPU_ADLN_Inventory.Domain loop
            if Engine_Inventory.Domains (D) then
               Publish_Snapshot ("intel-gpu: required forcewake " &
                 Intel_GPU_ADLN_Inventory.Domain'Image (D));
            end if;
         end loop;
      end if;
      Publish_Snapshot ("intel-gpu: doorbells first=" & Hex (Doorbell_First) &
        " second=" & Hex (Doorbell_Second) & " ADS-info-valid=" & Boolean'Image (ADS_Info.Valid));
      Publish_Snapshot ("intel-gpu: file=" &
        Intel_GPU_Firmware_File.Name (Firmware_Status) &
        " bytes=" & Unsigned_64'Image (Firmware_Bytes));
      Publish_Snapshot ("intel-gpu: ggtt=" &
        Unsigned_64'Image (Table_Bytes) & "; " & GGTT_Result);
      if Firmware_Status = Intel_GPU_Firmware_File.Loaded then
         declare
            Prepared : constant String := Intel_GPU_Firmware_Buffer.Prepare
              (Firmware_Address, Firmware_Bytes);
         begin
            debugPrint ("intel-gpu: firmware buffer " & Prepared & ASCII.LF);
            Publish_Snapshot ("intel-gpu: firmware buffer " & Prepared);
         end;
      end if;
      declare
         Buffer_View : constant Intel_GPU_Firmware_Buffer.Prepared_Buffer :=
           Intel_GPU_Firmware_Buffer.Prepared;
      begin
         if Buffer_View.Ready then
            if Buffer_View.DMA_Address = 0 or else
              Buffer_View.DMA_Address mod 4096 /= 0 or else
              Buffer_View.Allocation_Bytes /= 1024 * 1024 or else
              Buffer_View.DMA_Address > 2 ** 32 - Buffer_View.Allocation_Bytes or else
              Buffer_View.CPU_Address /= 16#6100_0000# or else
              Buffer_View.Content_Bytes /= Firmware_Bytes or else
              Firmware_Status /= Intel_GPU_Firmware_File.Loaded
            then
               debugPrint ("intel-gpu: firmware descriptor inconsistent" & ASCII.LF);
               return;
            end if;
            Publish_Snapshot ("intel-gpu: firmware descriptor ready; bytes=" &
              Unsigned_64'Image (Buffer_View.Content_Bytes) & " capacity=" &
              Unsigned_64'Image (Buffer_View.Allocation_Bytes));
            if Firmware_Plan.Valid then
               Selected_Upload_Bytes := Firmware_Plan.Code_Bytes + 128;
            end if;
            declare
               Log_View : constant Intel_GPU_Firmware_Buffer.Prepared_Log_Buffer :=
                 Intel_GPU_Firmware_Buffer.Prepared_Log;
            begin
               if Log_View.Ready then
                  Publish_Snapshot ("intel-gpu: log storage zeroed-retained; bytes=" &
                    Unsigned_64'Image (Log_View.Region_Bytes) & " (NOT GPU-published)");
               end if;
            end;
         else
            Publish_Snapshot ("intel-gpu: firmware descriptor unavailable");
         end if;
      end;
      end;
   else
      debugPrint ("intel-gpu: snapshot unavailable" & ASCII.LF);
   end if;
   Publish_Snapshot ("intel-gpu: display power pages " &
     Intel_GPU_Display_Mapping.Prepare (Display_Power_Owned, Plan.Physical_Base));
   Publish_Snapshot ("intel-gpu: parent PW1 " &
     Parent_One.Acquire (Display_Power_Owned, False));
   Publish_Snapshot ("intel-gpu: parent PW2 " &
     Parent_Two.Acquire (Display_Power_Owned, Parent_One.Held));
   if Display_Power_Owned and then Parent_One.Held and then
     Intel_GPU_Probe.Contains_Register (Register_Virtual_Base, Plan.Bytes, 16#51000#)
   then
      declare
         First : constant Unsigned_32 := Read_Register (Register_Virtual_Base + 16#51000#);
         Second : constant Unsigned_32 := Read_Register (Register_Virtual_Base + 16#51000#);
      begin
         Display_Presence := DP.Decode (16#8086#, PCI_Device, 3, First, Second);
         Publish_Snapshot ("intel-gpu: DFSM first=" & Hex (First) & " second=" & Hex (Second));
      end;
   end if;
   for P in DP.Pipe loop
      Publish_Snapshot ("intel-gpu: pipe " & DP.Pipe'Image (P) & " presence " &
        (case Display_Presence.Pipes (P) is
           when DP.Unknown => "unknown", when DP.Present => "present", when DP.Absent => "absent"));
   end loop;
   Publish_Snapshot ("intel-gpu: PHY pages " &
     Intel_GPU_PHY_Mapping.Prepare (Display_Power_Owned, Plan.Physical_Base));
   for Port in Intel_GPU_Combo_PHY.PHY loop
      declare
         use Intel_GPU_Combo_PHY;
         use type PHY_Snapshot.Outcome;
         Sample : constant PHY_Snapshot.Observation :=
           PHY_Snapshot.Capture (Display_Power_Owned, Port);
      begin
         Publish_Snapshot ("intel-gpu: PHY " & PHY'Image (Port) & " snapshot " &
           PHY_Snapshot.Outcome'Image (Sample.Status));
         if Sample.Status = PHY_Snapshot.Collected then
            Publish_Snapshot ("intel-gpu: PHY " & PHY'Image (Port) &
              " proc=" & Hex (Sample.Values (Comp_3)) &
              " state=" & Intel_GPU_Combo_PHY.Outcome'Image (Prepare (Port, Sample.Values).Status) &
              " (NOT restored)");
         end if;
      end;
   end loop;
   declare
      use Intel_GPU_DC_State;
      use type DC_Snapshot.Outcome;
      DC : constant DC_Snapshot.Observation := DC_Snapshot.Capture (Display_Power_Owned);
   begin
      Publish_Snapshot ("intel-gpu: retained DC snapshot " &
        DC_Snapshot.Outcome'Image (DC.Status));
      if DC.Status = DC_Snapshot.Collected then
         Publish_Snapshot ("intel-gpu: CDCLK=" & Hex (DC.Values (Clock_Control)) &
           " PLL=" & Hex (DC.Values (PLL)) & " reference=" & Hex (DC.Values (Reference)));
         Publish_Snapshot ("intel-gpu: DBUF0=" & Hex (DC.Values (Buffer_0)) &
           " DBUF1=" & Hex (DC.Values (Buffer_1)));
         Publish_Snapshot ("intel-gpu: DBUF2=" & Hex (DC.Values (Buffer_2)) &
           " DBUF3=" & Hex (DC.Values (Buffer_3)));
         -- A self-check reports whether this sample is settled; it does not
         -- establish preservation across a transition that has not occurred.
         Publish_Snapshot ("intel-gpu: DC baseline check " &
           Outcome'Image (Check (DC.Values, DC.Values)) & " (NOT DC-exit)");
      end if;
   end;
   if Enable_Native_Reset and then Request_Authorization (16#022E#) then
      if Request_Authorization (Intel_GPU_PCI_Interrupts.Disable_Request_Label) then
         -- Only verified PCI control state: not a CPU IRQ/handler drain fence.
         Publish_Snapshot ("intel-gpu: PCI IRQ sources disabled (NOT handler drain)");
         Publish_Snapshot ("intel-gpu: native reset beginning; scanout retained");
         declare
            Reset_Result : constant String := Intel_GPU_Native_Reset.Execute (Media_Fuse);
         begin
            debugPrint ("intel-gpu: native reset " & Reset_Result & ASCII.LF);
            Publish_Snapshot ("intel-gpu: native reset " & Reset_Result);
            Publish_Snapshot ("intel-gpu: CS timestamp Hz=" &
              Unsigned_32'Image (Intel_GPU_Native_Reset.Timestamp_Hz) &
              " (0=unavailable; read-only observation)");
            if Intel_GPU_Native_Reset.Last_Succeeded then
               -- Initial boot only: the static display claim excludes other
               -- software writers. Keep firmware scanout, clocks and buffers;
               -- never report a DC-off reference from the enable mask alone.
               Publish_Snapshot ("intel-gpu: native DC transition beginning; scanout retained");
               Publish_Snapshot ("intel-gpu: native DC transition " &
                 Native_DC.Execute (Display_Power_Owned));
               Publish_Snapshot ("intel-gpu: " & Native_DC.PHY_Diagnostic);
               declare
                  use Intel_GPU_Display_Topology;
                  Parents : constant Unsigned_64 :=
                    (if Parent_One.Held then Bit (PW1) else 0) or
                    (if Native_DC.Held then Bit (DC_Off) else 0) or
                    (if Parent_Two.Held then Bit (PW2) else 0);
               begin
                  -- Initial boot only: this process has no IRQ grant or
                  -- registered GPU handler, and the broker has disabled PCI
                  -- sources above. This is NOT a runtime handler-drain API.
                  Publish_Snapshot ("intel-gpu: pipe A power " &
                    (if Pipe_Present (DP.A) then Pipe_A.Acquire
                      (Display_Power_Owned, Intel_GPU_Display_Mapping.Ready, True, Parents)
                     else "skipped: absent or unknown presence"));
                  Publish_Snapshot ("intel-gpu: pipe B power " &
                    (if Pipe_Present (DP.B) then Pipe_B.Acquire
                      (Display_Power_Owned, Intel_GPU_Display_Mapping.Ready, True, Parents)
                     else "skipped: absent or unknown presence"));
                  -- C/D additionally require DC_Off in Ancestors. The bit
                  -- above is supplied only by the completed native transition.
                  Publish_Snapshot ("intel-gpu: pipe C power " &
                    (if Pipe_Present (DP.C) then Pipe_C.Acquire
                      (Display_Power_Owned, Intel_GPU_Display_Mapping.Ready, True, Parents)
                     else "skipped: absent or unknown presence"));
                  Publish_Snapshot ("intel-gpu: pipe D power " &
                    (if Pipe_Present (DP.D) then Pipe_D.Acquire
                      (Display_Power_Owned, Intel_GPU_Display_Mapping.Ready, True, Parents)
                     else "skipped: absent or unknown presence"));
                  for Number in Intel_GPU_Plane_Registers.Plane_Number loop
                  declare
                     A : constant Plane_A.Observation :=
                       (if Pipe_Present (DP.A) then Plane_A.Inspect (Display_Power_Owned, Table_Bytes, Number)
                        else (others => <>));
                     B : constant Plane_B.Observation :=
                       (if Pipe_Present (DP.B) then Plane_B.Inspect (Display_Power_Owned, Table_Bytes, Number)
                        else (others => <>));
                     C : constant Plane_C.Observation :=
                       (if Pipe_Present (DP.C) then Plane_C.Inspect (Display_Power_Owned, Table_Bytes, Number)
                        else (others => <>));
                     D : constant Plane_D.Observation :=
                       (if Pipe_Present (DP.D) then Plane_D.Inspect (Display_Power_Owned, Table_Bytes, Number)
                        else (others => <>));
                     Label : constant String := " plane" & Positive'Image (Number);
                  begin
                     Scanout_Planes (Number) := (A.Collected, A.Before, A.After);
                     Scanout_Planes (5 + Number) := (B.Collected, B.Before, B.After);
                     Scanout_Planes (10 + Number) := (C.Collected, C.Before, C.After);
                     Scanout_Planes (15 + Number) := (D.Collected, D.Before, D.After);
                     Publish_Snapshot ("intel-gpu: A" & Label & " " & Plane_A.Diagnostic (A));
                     if A.Collected then
                     Publish_Snapshot ("intel-gpu: A" & Label & " ctl=" & Hex (A.Before.Control) &
                       " surface=" & Hex (A.Before.Surface) & " live=" & Hex (A.Before.Live_Surface));
                     end if;
                     Publish_Snapshot ("intel-gpu: B" & Label & " " & Plane_B.Diagnostic (B));
                     if B.Collected then
                     Publish_Snapshot ("intel-gpu: B" & Label & " ctl=" & Hex (B.Before.Control) &
                       " surface=" & Hex (B.Before.Surface) & " live=" & Hex (B.Before.Live_Surface));
                     end if;
                     Publish_Snapshot ("intel-gpu: C" & Label & " " & Plane_C.Diagnostic (C));
                     if C.Collected then
                     Publish_Snapshot ("intel-gpu: C" & Label & " ctl=" & Hex (C.Before.Control) &
                       " surface=" & Hex (C.Before.Surface) & " live=" & Hex (C.Before.Live_Surface));
                     end if;
                     Publish_Snapshot ("intel-gpu: D" & Label & " " & Plane_D.Diagnostic (D));
                     if D.Collected then
                     Publish_Snapshot ("intel-gpu: D" & Label & " ctl=" & Hex (D.Before.Control) &
                       " surface=" & Hex (D.Before.Surface) & " live=" & Hex (D.Before.Live_Surface));
                     end if;
                  end;
                  end loop;
                  declare
                     A : constant Cursor_A.Observation :=
                       (if Pipe_Present (DP.A) then Cursor_A.Inspect (Display_Power_Owned, Table_Bytes)
                        else (others => <>));
                     B : constant Cursor_B.Observation :=
                       (if Pipe_Present (DP.B) then Cursor_B.Inspect (Display_Power_Owned, Table_Bytes)
                        else (others => <>));
                     C : constant Cursor_C.Observation :=
                       (if Pipe_Present (DP.C) then Cursor_C.Inspect (Display_Power_Owned, Table_Bytes)
                        else (others => <>));
                     D : constant Cursor_D.Observation :=
                       (if Pipe_Present (DP.D) then Cursor_D.Inspect (Display_Power_Owned, Table_Bytes)
                        else (others => <>));
                  begin
                     Scanout_Cursors := [(A.Collected, A.Before, A.After),
                       (B.Collected, B.Before, B.After), (C.Collected, C.Before, C.After),
                       (D.Collected, D.Before, D.After)];
                     Publish_Snapshot ("intel-gpu: cursor A " & Cursor_A.Diagnostic (A));
                     if A.Collected then
                     Publish_Snapshot ("intel-gpu: cursor A ctl=" & Hex (A.Before.Control) &
                       " base=" & Hex (A.Before.Base) & " live=" & Hex (A.Before.Live_Base));
                     end if;
                     Publish_Snapshot ("intel-gpu: cursor B " & Cursor_B.Diagnostic (B));
                     if B.Collected then
                     Publish_Snapshot ("intel-gpu: cursor B ctl=" & Hex (B.Before.Control) &
                       " base=" & Hex (B.Before.Base) & " live=" & Hex (B.Before.Live_Base));
                     end if;
                     Publish_Snapshot ("intel-gpu: cursor C " & Cursor_C.Diagnostic (C));
                     if C.Collected then
                     Publish_Snapshot ("intel-gpu: cursor C ctl=" & Hex (C.Before.Control) &
                       " base=" & Hex (C.Before.Base) & " live=" & Hex (C.Before.Live_Base));
                     end if;
                     Publish_Snapshot ("intel-gpu: cursor D " & Cursor_D.Diagnostic (D));
                     if D.Collected then
                     Publish_Snapshot ("intel-gpu: cursor D ctl=" & Hex (D.Before.Control) &
                       " base=" & Hex (D.Before.Base) & " live=" & Hex (D.Before.Live_Base));
                     end if;
                  end;
                  Scanout := Intel_GPU_Scanout_Inventory.Collect
                    (Scanout_Planes, Scanout_Cursors, Table_Bytes, Display_Presence);
                  Publish_Snapshot ("intel-gpu: scanout inventory " &
                    (case Scanout.Status is
                       when Intel_GPU_Scanout_Inventory.Complete => "COMPLETE",
                       when Intel_GPU_Scanout_Inventory.Incomplete => "INCOMPLETE",
                       when Intel_GPU_Scanout_Inventory.Unsupported_Plane => "UNSUPPORTED-PLANE",
                       when Intel_GPU_Scanout_Inventory.Unsupported_Cursor => "UNSUPPORTED-CURSOR") &
                    " ranges=" & Natural'Image (Scanout.Count) & " (NOT GGTT ownership)");
               end;
               declare
                  Size_First : constant Unsigned_32 := Read_Register (Register_Virtual_Base + 16#C050#);
                  Base_First : constant Unsigned_32 := Read_Register (Register_Virtual_Base + 16#C340#);
                  Size_Second : constant Unsigned_32 := Read_Register (Register_Virtual_Base + 16#C050#);
                  Base_Second : constant Unsigned_32 := Read_Register (Register_Virtual_Base + 16#C340#);
                  WOPCM : constant Intel_GPU_ADLN_WOPCM.Layout :=
                    Intel_GPU_ADLN_WOPCM.Select_Layout
                      (Size_First, Base_First, Size_Second, Base_Second, Selected_Upload_Bytes);
               begin
                  Publish_Snapshot ("intel-gpu: WOPCM size=" & Hex (Size_First) &
                    " base=" & Hex (Base_First) & " stable=" &
                    Boolean'Image (Size_First = Size_Second and Base_First = Base_Second));
                  Publish_Snapshot ("intel-gpu: WOPCM plan valid=" & Boolean'Image (WOPCM.Valid) &
                    " locked=" & Boolean'Image (WOPCM.Locked) &
                    " pin-bias=" & Unsigned_64'Image (WOPCM.Pin_Bias));
                  if WOPCM.Valid then
                     Selected_WOPCM := WOPCM;
                     Address_Layout := Intel_GPU_GGTT_Layout.Plan (Table_Bytes, WOPCM.Pin_Bias);
                  end if;
                  if Address_Layout.Valid then
                     Publish_Snapshot ("intel-gpu: GGTT runtime first=" & Hex64 (Address_Layout.Runtime_First) &
                       " limit=" & Hex64 (Address_Layout.Runtime_Limit));
                     Publish_Snapshot ("intel-gpu: GGTT upload first=" & Hex64 (Address_Layout.Upload_First) &
                       " limit=" & Hex64 (Address_Layout.Upload_Limit));
                     Publish_Snapshot ("intel-gpu: GGTT end guard=" & Hex64 (Address_Layout.Guard_First));
                     -- Fixed first1MiB upload proposal. A clear scanout test
                     -- does not establish ownership or prove hardware PTEs
                     -- empty. No ledger admission or publication occurs here.
                     Publish_Snapshot ("intel-gpu: upload scanout-clear=" & Boolean'Image
                       (Intel_GPU_Scanout_Inventory.No_Scanout_Overlap
                         (Scanout, (True, Address_Layout.Upload_First, 1_048_576))) &
                       " (NOT GPU-published)");
                  else
                     Publish_Snapshot ("intel-gpu: GGTT layout rejected; no address allocation");
                  end if;
               end;
               Publish_Snapshot ("intel-gpu: ADS hardware observation valid=" &
                 Boolean'Image (Intel_GPU_Native_Reset.ADS_Observed) &
                 " doorbell=" & Hex (Intel_GPU_Native_Reset.ADS_Doorbell));
               if Intel_GPU_Native_Reset.ADS_Observed then
                  Publish_Snapshot ("intel-gpu: measured EU mask=" &
                    Hex (Unsigned_32 (Intel_GPU_Native_Reset.ADS_Execution_Units.EU_Mask)) &
                    " total=" & Natural'Image
                      (Intel_GPU_Native_Reset.ADS_Execution_Units.Total_EUs));
                  Publish_Snapshot ("intel-gpu: ADS topology DSS=" &
                    Hex (Unsigned_32 (Intel_GPU_Native_Reset.ADS_Topology.DSS_Mask)) &
                    " L3=" &
                    Hex (Unsigned_32 (Intel_GPU_Native_Reset.ADS_Topology.L3_Mask)));
               end if;
               Publish_Snapshot ("intel-gpu: ADS backing " & Intel_GPU_ADS_Buffer.Prepare);
               -- CPU access only: publication still requires a reserved GPU
               -- range, preserved scanout mappings and the invalidate path.
               Publish_Snapshot ("intel-gpu: GGTT write mapping " &
                 Intel_GPU_GGTT_Mapping.Prepare
                   (Display_Power_Owned, Intel_GPU_Native_Reset.Last_Succeeded,
                    Plan.Physical_Base, Table_Bytes));
               declare
                  Status : Native_PAT.Result;
                  use type Native_PAT.Result;
               begin
                  PAT_Active := True;
                  Native_PAT.Configure (PAT_Attempt, Status);
                  PAT_Ready := Status = Native_PAT.Ready and then PAT_Owner_Ready;
                  PAT_Active := False;
                  Publish_Snapshot ("intel-gpu: PAT setup " & Native_PAT.Result_Name (Status) &
                    " index=" & Hex (Unsigned_32 (Native_PAT.Last_Index (PAT_Attempt))) &
                    " raw=" & Hex (Native_PAT.Last_Raw (PAT_Attempt)));
               end;
               declare
                  Status : Native_MOCS.Result;
                  use type Native_MOCS.Result;
               begin
                  MOCS_Active := True;
                  Native_MOCS.Configure (MOCS_Attempt, Status);
                  MOCS_Ready := Status = Native_MOCS.Ready and then MOCS_Owner_Ready;
                  MOCS_Active := False;
                  Publish_Snapshot ("intel-gpu: MOCS setup " & Native_MOCS.Result_Name (Status) &
                    " index=" & Hex (Unsigned_32 (Native_MOCS.Last_Index (MOCS_Attempt))) &
                    " raw=" & Hex (Native_MOCS.Last_Raw (MOCS_Attempt)));
               end;
               declare
                  Status : GT_Settings.Result;
                  use type GT_Settings.Result;
               begin
                  GT_Settings_Active := True;
                  GT_Settings.Configure
                    (GT_Settings_Attempt, Intel_GPU_Native_Reset.ADS_Inventory,
                     Intel_GPU_Native_Reset.ADS_Topology, Status);
                  GT_Settings_Ready := Status in GT_Settings.Ready | GT_Settings.Ready_With_Firmware_Override
                    and then GT_Settings_Owner;
                  GT_Settings_Active := False;
                  Publish_Snapshot ("intel-gpu: GT settings " &
                    (case Status is
                       when GT_Settings.Rejected => "REJECTED",
                       when GT_Settings.Ownership_Lost => "OWNERSHIP-LOST",
                       when GT_Settings.Read_Failed => "READ-FAILED",
                       when GT_Settings.Write_Failed => "WRITE-FAILED",
                       when GT_Settings.Readback_Failed => "READBACK-FAILED",
                       when GT_Settings.Ready => "READY",
                       when GT_Settings.Ready_With_Firmware_Override => "READY-FIRMWARE-OVERRIDE") &
                    " offset=" & Hex (GT_Settings.Last_Offset (GT_Settings_Attempt)) &
                    " raw=" & Hex (GT_Settings.Last_Readback (GT_Settings_Attempt)));
               end;
               declare
                  Status : Render_Settings.Result;
                  use type Render_Settings.Result;
               begin
                  Render_Settings_Active := True;
                  Render_Settings.Configure
                    (Render_Settings_Attempt, Intel_GPU_Native_Reset.ADS_Inventory,
                     Intel_GPU_ADLN_Inventory.Render, Status);
                  Render_Settings_Ready := Status = Render_Settings.Ready and then Render_Settings_Owner;
                  Render_Settings_Active := False;
                  Publish_Snapshot ("intel-gpu: render engine settings " &
                    (case Status is
                       when Render_Settings.Rejected => "REJECTED",
                       when Render_Settings.Ownership_Lost => "OWNERSHIP-LOST",
                       when Render_Settings.Read_Failed => "READ-FAILED",
                       when Render_Settings.Write_Failed => "WRITE-FAILED",
                       when Render_Settings.Readback_Failed => "READBACK-FAILED",
                       when Render_Settings.Ready => "READY") & " (NOT engine-started)");
               end;
               -- Initial boot: frozen exclusive claim, no other GPU modeset
               -- or submission owner. Replace inherited PTEs only within a
               -- retained allocation; scanout and the guard remain untouched. The
               -- top reservation is driver-owned upload space, not runtime
               -- GuC space; scanout exclusions apply even in this interval.
               Upload_Backing := Intel_GPU_Firmware_Buffer.Prepared;
               if Publication_Owner_Ready and then Address_Layout.Valid and then
                 Upload_Backing.Ready and then
                 Scanout.Status = Intel_GPU_Scanout_Inventory.Complete
               then
                  GGTT_Takeover_Held := True;
                  Publish_Snapshot ("intel-gpu: GGTT bounded takeover admitted; scanout retained");
                  declare
                     Admitted : Boolean;
                     Selected : Unsigned_64;
                     Status : Upload_Publication.Result;
                     use type Upload_Publication.Result;
                  begin
                     Intel_GPU_GGTT_Reservations.Admit
                       (Upload_Ledger, Table_Bytes, Address_Layout.Upload_First,
                        Address_Layout.Upload_Limit - Address_Layout.Upload_First, Admitted);
                     if Admitted then
                        Upload_Publication.Publish_Available
                          (Upload_Attempt, Upload_Ledger, Upload_Backing.DMA_Address,
                           Upload_Region, 4096, Selected, Status);
                        Firmware_Mapped := Status = Upload_Publication.Published;
                        Publish_Snapshot ("intel-gpu: firmware mapping " &
                          (case Status is
                             when Upload_Publication.Published => "PUBLISHED",
                             when Upload_Publication.Quarantined => "QUARANTINED",
                             when Upload_Publication.Protected_Range => "PROTECTED-RANGE",
                             when Upload_Publication.Reservation_Failed => "NO-FREE-UPLOAD-RANGE",
                             when Upload_Publication.Read_Failed => "READ-FAILED",
                             when Upload_Publication.Prepare_Failed => "PREPARE-FAILED",
                             when Upload_Publication.Rejected => "REJECTED") &
                          " gpu=" & Hex64 (Selected) & " (NOT executing)");
                        declare
                           Detail : constant Upload_Publication.Search_Evidence :=
                             Upload_Publication.Search_Detail (Upload_Attempt);
                        begin
                           Publish_Snapshot ("intel-gpu: upload allocation " &
                             Upload_Publication.Search_Name (Detail.Outcome) &
                             " reads=" & Unsigned_64'Image (Detail.Reads) &
                             " blocked=" & Unsigned_64'Image (Detail.Blocked) &
                             " nonzero=" & Unsigned_64'Image (Detail.Nonzero));
                           if Detail.Nonzero /= 0 then
                              Publish_Snapshot ("intel-gpu: upload first nonzero index=" &
                                Hex64 (Detail.First_Nonzero_Index) & " pte=" &
                                Hex64 (Detail.First_Nonzero_Value));
                              declare
                                 procedure Probe (Index : Unsigned_64) is
                                    Address : Unsigned_64;
                                    Low, High_Before, High_After, RO_Low, RO_High : Unsigned_32;
                                    Again : Unsigned_64;
                                    Valid : Boolean;
                                 begin
                                    if not Publication_Owner_Ready or else
                                      Index >= Intel_GPU_GGTT_Mapping.Bytes / 8
                                    then return; end if;
                                    Address := Intel_GPU_GGTT_Mapping.Virtual_Base + Index * 8;
                                    High_Before := Read_Register (Address + 4);
                                    Low := Read_Register (Address);
                                    High_After := Read_Register (Address + 4);
                                    Upload_IO.Read_PTE (Index, Again, Valid);
                                    Publish_Snapshot ("intel-gpu: PTE probe index=" & Hex64 (Index) &
                                      " high=" & Hex (High_Before) & " low=" & Hex (Low));
                                    Publish_Snapshot ("intel-gpu: PTE probe high-again=" & Hex (High_After) &
                                      " qword=" & Hex64 (Again) & " read-ok=" & Boolean'Image (Valid));
                                    if GGTT_Inspection_Mapped and then Index < Table_Bytes / 8 then
                                       RO_Low := Read_Register (16#6040_0000# + Index * 8);
                                       RO_High := Read_Register (16#6040_0000# + Index * 8 + 4);
                                       Publish_Snapshot ("intel-gpu: PTE probe RO high=" & Hex (RO_High) &
                                         " low=" & Hex (RO_Low));
                                    end if;
                                 end Probe;
                              begin
                                 -- Observations only, not PTE validity/ownership
                                 -- evidence. No writes, allocation or retries.
                                 Probe (0);
                                 Probe (Detail.First_Nonzero_Index);
                              end;
                           end if;
                        end;
                        Upload_Bound := False;
                     end if;
                  end;
               else
                  Publish_Snapshot ("intel-gpu: firmware mapping prerequisites unavailable; backing retained");
                  declare
                     procedure Require (Name : String; Ready : Boolean) is
                     begin
                        if not Ready then
                           Publish_Snapshot ("intel-gpu: upload missing " & Name);
                        end if;
                     end Require;
                     Held : constant DP.Held_Set :=
                       [DP.A => Pipe_A.Held, DP.B => Pipe_B.Held,
                        DP.C => Pipe_C.Held, DP.D => Pipe_D.Held];
                  begin
                     -- Retained state only: no extra MMIO, writes or retries.
                     Require ("display ownership", Display_Power_Owned);
                     Require ("reset pages", Reset_Pages_Mapped);
                     Require ("native reset", Intel_GPU_Native_Reset.Last_Succeeded);
                     Require ("DC power", Native_DC.Held);
                     Require ("parent power 1", Parent_One.Held);
                     Require ("parent power 2", Parent_Two.Held);
                     Require ("pipe presence", Display_Presence.Known);
                     for P in DP.Pipe loop
                        Require ("pipe " & Character'Val (Character'Pos ('A') + DP.Pipe'Pos (P)),
                          DP."=" (Display_Presence.Pipes (P), DP.Absent) or else
                            (DP."=" (Display_Presence.Pipes (P), DP.Present) and then Held (P)));
                     end loop;
                     Require ("GGTT write mapping", Intel_GPU_GGTT_Mapping.Ready);
                     Require ("GGTT mapping size", Intel_GPU_GGTT_Mapping.Bytes = Table_Bytes);
                     Require ("PAT", PAT_Ready);
                     Require ("MOCS", MOCS_Ready);
                     Require ("GT settings", GT_Settings_Ready);
                     Require ("render settings", Render_Settings_Ready);
                     Require ("address layout", Address_Layout.Valid);
                     Require ("firmware backing", Upload_Backing.Ready);
                     Require ("scanout inventory", Scanout.Status = Intel_GPU_Scanout_Inventory.Complete);
                  end;
               end if;
               if Firmware_Mapped then
                  declare
                     Policy : constant Intel_GPU_Buffer_Backing.Heap_Policy :=
                       Intel_GPU_Buffer_Backing.Native_System_Heap
                         (getInfo (Intel_GPU_Buffer_Backing.Managed_RAM_Query));
                     Accepted : Boolean;
                  begin
                     Buffer_Memory.Configure_Heap
                       (Buffer_Pool, Policy.Byte_Quota, Policy.DMA_Limit,
                        Policy.Metadata_Bytes, Accepted);
                     if Accepted then
                        Application_Buffers.Configure_Client_Budgets
                          (Application_Buffer_State, Policy.Byte_Quota / 8192 * 4096, Accepted);
                     end if;
                     Publish_Snapshot ("intel-gpu: system backing policy accepted=" &
                       Boolean'Image (Accepted) & " quota=" & Unsigned_64'Image (Policy.Byte_Quota) &
                       " (NOT reserved)");
                     if not Accepted then Firmware_Mapped := False; end if;
                  end;
               end if;
               if Firmware_Mapped then
                  -- Bootstrap is serialized: no non-logger IPC request remains
                  -- in flight here. Allocate before preparing any context PTEs.
                  Submission_Allocation := Buffer_Memory.Acquire
                    (Buffer_Pool, Submission_Slot,
                     Intel_GPU_Buffer_Backing.Page_Count (Submission_Bytes / 4096));
                  if Submission_Allocation.Ready and then
                    (Submission_Allocation.CPU_Address /= Submission_CPU or else
                     Submission_Allocation.Bytes /= Submission_Bytes)
                  then
                     -- Unexpected layout is retained but never published.
                     Submission_Allocation := (Ready => False);
                  end if;
                  Publish_Snapshot ("intel-gpu: context buffer zeroed-retained=" &
                    Boolean'Image (Submission_Allocation.Ready));
                  Publish_Snapshot ("intel-gpu: context allocation stage=" &
                    Buffer_Memory.Allocation_Stage'Image (Buffer_Memory.Last_Stage (Buffer_Pool)));
                  if Submission_Allocation.Ready then
                     declare
                        Ticket : Application_Buffers.Ticket;
                        Consumed : Boolean;
                     begin
                        Application_Buffers.Reserve_Private (Application_Buffer_State, 0, Ticket, Pages => 4);
                        if Ticket /= 0 then
                           Boot_Update_Allocation := Buffer_Memory.Acquire
                             (Buffer_Pool, Application_Buffers.Ticket_Slot (Ticket), 4);
                           Application_Buffers.Finish_Private
                             (Application_Buffer_State, Ticket, Consumed);
                           if not Consumed then Boot_Update_Allocation := (Ready => False); end if;
                        end if;
                        Publish_Snapshot ("intel-gpu: update tables zeroed-retained=" &
                          Boolean'Image (Boot_Update_Allocation.Ready));
                        if Boot_Update_Allocation.Ready then
                           Application_Buffers.Reserve_Private
                             (Application_Buffer_State, 0, Ticket, Pages => 1);
                           if Ticket /= 0 then
                              Retirement_Scratch := Buffer_Memory.Acquire
                                (Buffer_Pool, Application_Buffers.Ticket_Slot (Ticket), 1);
                              Application_Buffers.Finish_Private
                                (Application_Buffer_State, Ticket, Consumed);
                              if not Consumed then Retirement_Scratch := (Ready => False); end if;
                           end if;
                        end if;
                        Publish_Snapshot ("intel-gpu: GGTT retirement scratch zeroed-retained=" &
                          Boolean'Image (Retirement_Scratch.Ready) & " (NOT GPU-published)");
                     end;
                  end if;
                  declare
                     Backing : constant Intel_GPU_ADS_Buffer.Prepared_Backing := Intel_GPU_ADS_Buffer.Prepared;
                     Log_Backing : constant Intel_GPU_Firmware_Buffer.Prepared_Log_Buffer := Intel_GPU_Firmware_Buffer.Prepared_Log;
                     CT_Backing : constant Intel_GPU_Firmware_Buffer.Prepared_CT_Buffer := Intel_GPU_Firmware_Buffer.Prepared_CT;
                     Submission_Backing : constant Submission_View :=
                       Submission_Region
                         (Intel_GPU_Submission_Backing.Context_Image);
                     Admitted : Boolean;
                     Selected : Unsigned_64;
                     Status : Runtime_Publication.Result;
                     use type Runtime_Publication.Result;
                     function Describe (Value : Runtime_Publication.Result) return String is
                       (case Value is
                          when Runtime_Publication.Published => "PUBLISHED",
                          when Runtime_Publication.Quarantined => "QUARANTINED",
                          when Runtime_Publication.Protected_Range => "PROTECTED-RANGE",
                          when Runtime_Publication.Reservation_Failed => "NO-FREE-RUNTIME-RANGE",
                          when Runtime_Publication.Read_Failed => "READ-FAILED",
                          when Runtime_Publication.Prepare_Failed => "PREPARE-FAILED",
                          when Runtime_Publication.Rejected => "REJECTED");
                  begin
                     Intel_GPU_GGTT_Reservations.Admit
                       (Runtime_Ledger, Table_Bytes, Address_Layout.Runtime_First,
                        Address_Layout.Runtime_Limit - Address_Layout.Runtime_First, Admitted);
                     if Admitted and then Backing.Ready and then Log_Backing.Ready then
                        Active_Runtime := ADS_Buffer;
                        Runtime_DMA := Backing.DMA_Address; Runtime_CPU := Backing.CPU_Address;
                        Runtime_Bytes := Backing.Capacity; Runtime_Bound := False;
                        Runtime_Publication.Publish_Available
                          (ADS_Attempt, Runtime_Ledger, Runtime_DMA, Runtime_Bytes,
                           4096, Selected, Status);
                        ADS_Mapped := Status = Runtime_Publication.Published;
                        if ADS_Mapped then ADS_GPU_Start := Selected; end if;
                        Publish_Snapshot ("intel-gpu: ADS mapping " & Describe (Status) &
                          " gpu=" & Hex64 (Selected));
                        if ADS_Mapped then
                           Active_Runtime := Log_Buffer;
                           Runtime_DMA := Log_Backing.DMA_Address; Runtime_CPU := Log_Backing.CPU_Address;
                           Runtime_Bytes := Log_Backing.Region_Bytes; Runtime_Bound := False;
                           Runtime_Publication.Publish_Available
                             (Log_Attempt, Runtime_Ledger, Runtime_DMA, Runtime_Bytes,
                              4096, Selected, Status);
                           Log_Mapped := Status = Runtime_Publication.Published;
                           if Log_Mapped then Log_GPU_Start := Selected; end if;
                           Publish_Snapshot ("intel-gpu: log mapping " & Describe (Status) &
                             " gpu=" & Hex64 (Selected));
                        end if;
                        if ADS_Mapped and then Log_Mapped and then CT_Backing.Ready then
                           Active_Runtime := CT_Buffer;
                           Runtime_DMA := CT_Backing.DMA_Address; Runtime_CPU := CT_Backing.CPU_Address;
                           Runtime_Bytes := CT_Backing.Region_Bytes; Runtime_Bound := False;
                           Runtime_Publication.Publish_Available
                             (CT_Attempt, Runtime_Ledger, Runtime_DMA, Runtime_Bytes,
                              4096, Selected, Status);
                           CT_Mapped := Status = Runtime_Publication.Published;
                           if CT_Mapped then
                              CT_GPU_Start := Selected;
                              CT_Backing_View := CT_Backing;
                           end if;
                           Publish_Snapshot ("intel-gpu: CT mapping " & Describe (Status) &
                             " gpu=" & Hex64 (CT_GPU_Start) & " (NOT registered)");
                        end if;
                        if CT_Mapped and then Submission_Backing.Ready then
                           -- Only context+ring receive GGTT aliases. The
                           -- preparation callback writes/flushes the private
                           -- VM tables and batch too, before any PTE store.
                           Active_Runtime := Submission_Buffer;
                           Runtime_DMA := Submission_Backing.DMA_Address;
                           Runtime_CPU := Submission_Backing.CPU_Address;
                           Runtime_Bytes := Intel_GPU_Submission_Image.GGTT_Bytes;
                           Runtime_Bound := False;
                           Runtime_Publication.Publish_Available
                             (Submission_Attempt, Runtime_Ledger, Runtime_DMA,
                              Runtime_Bytes, 4096, Selected, Status);
                           Submission_Mapped := Status = Runtime_Publication.Published;
                           if Submission_Mapped then Submission_GPU_Start := Selected; end if;
                           Publish_Snapshot ("intel-gpu: context/ring mapping " & Describe (Status) &
                             " gpu=" & Hex64 (Selected) & " (NOT registered/submitted)");
                        end if;
                        if Submission_Mapped then
                           declare
                              Page : constant Submission_View :=
                                Submission_Region
                                  (Intel_GPU_Submission_Backing.Engine_Status_Page);
                           begin
                              if Page.Ready then
                                 Active_Runtime := Engine_Status_Buffer;
                                 Runtime_DMA := Page.DMA_Address;
                                 Runtime_CPU := Page.CPU_Address;
                                 Runtime_Bytes := Page.Bytes;
                                 Runtime_Bound := False;
                                 Runtime_Publication.Publish_Available
                                   (Engine_Status_Attempt, Runtime_Ledger, Runtime_DMA,
                                    Runtime_Bytes, 4096, Selected, Status);
                                 Engine_Status_Mapped := Status = Runtime_Publication.Published;
                                 if Engine_Status_Mapped then Engine_Status_GPU_Start := Selected; end if;
                                 Publish_Snapshot ("intel-gpu: engine HWSP mapping " & Describe (Status) &
                                   " gpu=" & Hex64 (Engine_Status_GPU_Start) & " (NOT engine-started)");
                              end if;
                           end;
                        end if;
                        -- Claims/backing remain retained; revoke the local
                        -- write binding now that these transactions have ended.
                        Active_Runtime := No_Buffer; Runtime_Bound := False;
                        if ADS_Mapped and then Log_Mapped and then
                          Publication_Owner_Ready and then
                          Intel_GPU_ADS_Buffer.Initialized_GPU_Start = ADS_GPU_Start
                        then
                           -- Loaded firmware is metadata-admitted70.49.4;
                           -- native inventory admits ADL-N46D2/IP12.0 only.
                           -- i915 selects PRE_PARSER, POLLCS and (>=70.7.0)
                           -- TSC_CHECK_ON_RC6, independently of PCI revision.
                           Startup_Parameters := Intel_GPU_GuC_Parameters.Encode_ADLN
                             ((Pin_Bias => Address_Layout.Runtime_First,
                               ADS => Intel_GPU_GuC_Parameters.Address (ADS_GPU_Start),
                               ADS_Backing_Bytes => Backing.Capacity,
                               Log =>
                                 (Base => Intel_GPU_GuC_Parameters.Address (Log_GPU_Start),
                                  Backing_Bytes => Log_Backing.Region_Bytes,
                                  Notify_Half_Full => False,
                                  Capture_Megabyte_Units => False,
                                  Log_Megabyte_Units => False,
                                  Crash_Count => 0, Capture_Count => 0,
                                  Debug_Pages_Count => 0),
                               Device => PCI_Device, Revision => PCI_Revision,
                               Scheduler_Enabled => True, SLPC_Enabled => False,
                               PXP_Enabled => False, Logging_Enabled => True,
                               Verbosity => 1, Workarounds => 16#0044_4000#));
                           Publish_Snapshot ("intel-gpu: GuC parameters " &
                             (if Intel_GPU_GuC_Parameters.Valid (Startup_Parameters)
                              then "READY" else "REJECTED") &
                             " identity=" & Hex
                               (Intel_GPU_GuC_Parameters.Words (Startup_Parameters) (5)) &
                             " (NOT written/executing)");
                        else
                           Publish_Snapshot ("intel-gpu: GuC parameters withheld; mapping/owner incomplete");
                        end if;
                     else
                        Publish_Snapshot ("intel-gpu: runtime mapping prerequisites unavailable; backing retained");
                     end if;
                  end;
               end if;
            end if;
         end;
      else
         Publish_Snapshot ("intel-gpu: PCI IRQ disable unavailable; reset skipped; resources retained");
      end if;
   elsif Reset_Pages_Mapped and then Enable_Native_Reset then
      Publish_Snapshot ("intel-gpu: native reset authorization unavailable");
   end if;
   -- Retain device, backing and forcewake on success or uncertain failure.
   if Firmware_Mapped and then ADS_Mapped and then Log_Mapped and then
     Intel_GPU_GuC_Parameters.Valid (Startup_Parameters) and then
     Selected_WOPCM.Valid and then Publication_Owner_Ready
   then
      declare
         Header : Intel_GPU_Firmware.CSS_Header := [others => 0];
         OK : Boolean;
         Status : Native_GuC.Result;
         use type Native_GuC.Result;
      begin
         Publish_Snapshot ("intel-gpu: native GuC upload beginning; backing/scanout retained");
         Startup_Active := True;
         for Index in Header'Range loop
            GuC_Byte (Unsigned_64 (Index), Header (Index), OK);
            exit when not OK;
         end loop;
         if Startup_Owner_Ready then
            Native_GuC.Execute
              (Header, Startup_Parameters, Upload_Backing.Content_Bytes,
               Upload_GPU_Start, Selected_WOPCM.Capacity, Selected_WOPCM.Base,
               Selected_WOPCM.Bytes, 1_000_000, Status);
            Startup_Active := False;
            Publish_Snapshot ("intel-gpu: GuC startup " &
              (case Status is
                 when Native_GuC.Rejected => "REJECTED",
                 when Native_GuC.Parameters_Failed => "PARAMETERS-FAILED",
                 when Native_GuC.WOPCM_Failed => "WOPCM-FAILED",
                 when Native_GuC.Preparation_Failed => "PREPARATION-FAILED",
                 when Native_GuC.Signature_Failed => "SIGNATURE-FAILED",
                 when Native_GuC.Transfer_Failed => "TRANSFER-FAILED",
                 when Native_GuC.Startup_Failed => "STARTUP-FAILED",
                 when Native_GuC.Firmware_Ready => "FIRMWARE-READY (NOT submission-ready)") &
              " raw=" & Hex (Native_GuC.Last_Startup_Raw));
            Publish_Snapshot ("intel-gpu: GuC transfer " & Native_GuC.Last_Transfer_Detail);
            if Status = Native_GuC.Firmware_Ready and then CT_Mapped then
               declare
                  CT_Status : Native_CT.Result;
                  use type Native_CT.Result;
               begin
                  Runtime_Admitted := True;
                  Publish_Snapshot ("intel-gpu: CT registration beginning; backing retained");
                  Native_CT.Execute
                    (CT_Registration, CT_GPU_Start, CT_Backing_View.Region_Bytes,
                     Address_Layout.Runtime_First, CT_Status);
                  if CT_Status /= Native_CT.Complete then Runtime_Fault := True; end if;
                  Publish_Snapshot ("intel-gpu: CT registration " &
                    Native_CT.Result'Image (CT_Status) & " step=" &
                    Hex (Unsigned_32 (Native_CT.Last_Step (CT_Registration))));
                  Publish_Snapshot ("intel-gpu: CT reply=" & Hex (Native_CT.Last_Reply (CT_Registration)) &
                    " transport=" & Runtime_MMIO.Result'Image (Runtime_Last_Result));
                  if CT_Status = Native_CT.Complete then
                     Publish_Snapshot ("intel-gpu: CT enabled (NOT engine submission-ready)");
                     CT_Receive.Initialize (Receive_Channel,
                       CT_Receive_Owner_Ready, 4096, 0);
                     declare
                        Receive_Status : CT_Receive.Result;
                        use type CT_Receive.Result;
                     begin
                        for Index in Initial_CT_Messages'Range loop
                           CT_Receive.Poll (Receive_Channel,
                             Initial_CT_Messages (Index), Receive_Status);
                           if Receive_Status = CT_Receive.Empty then
                              Publish_Snapshot ("intel-gpu: CT receive empty (NOT a firmware round-trip)");
                              exit;
                           elsif Receive_Status /= CT_Receive.Received then
                              Runtime_Fault := True;
                              Publish_Snapshot ("intel-gpu: CT receive " &
                                CT_Receive.Result'Image (Receive_Status) & "; backing retained");
                              exit;
                           end if;
                           Initial_CT_Count := Index;
                           Publish_Snapshot ("intel-gpu: CT captured len=" &
                             Natural'Image (Initial_CT_Messages (Index).Length) &
                             " fence=" & Hex (Unsigned_32 (Initial_CT_Messages (Index).Fence)) &
                             " hxg=" & Hex (Initial_CT_Messages (Index).Payload (1)));
                        end loop;
                        Publish_Snapshot ("intel-gpu: CT initial capture count=" &
                          Natural'Image (Initial_CT_Count) & " (NOT dispatched)");
                     end;
                     if CT_Receive_Owner_Ready then
                        CT_Send.Initialize (Send_Channel, True, 1024, 0);
                        declare
                           Probe_Status : CT_Roundtrip.Result;
                           Probe_Reply : Unsigned_32;
                           use type CT_Roundtrip.Result;
                        begin
                           Publish_Snapshot ("intel-gpu: CT logging-control roundtrip beginning");
                           CT_Roundtrip.Execute (CT_Probe_Attempt, 42, 1_000_000,
                                                 Probe_Reply, Probe_Status);
                           if Probe_Status /= CT_Roundtrip.Complete then
                              Runtime_Fault := True;
                           end if;
                           Publish_Snapshot ("intel-gpu: CT roundtrip " &
                             CT_Roundtrip.Result'Image (Probe_Status) &
                             " reply=" & Hex (Probe_Reply));
                           Publish_Snapshot ("intel-gpu: CT retained events=" &
                             Natural'Image (CT_Event_Count) & " (NOT engine submission-ready)");
                           if Probe_Status = CT_Roundtrip.Complete then
                              declare
                                 Engine_Result : RCS_Startup.Result;
                                 use type RCS_Startup.Result;
                              begin
                                 RCS_Start_Active := True;
                                 Publish_Snapshot ("intel-gpu: RCS preflight hwsp=" &
                                   Hex64 (Engine_Status_GPU_Start) & " mapped=" &
                                   Boolean'Image (Engine_Status_Mapped));
                                 Publish_Snapshot ("intel-gpu: RCS context mapped=" &
                                   Boolean'Image (Submission_Mapped) & " gpu=" &
                                   Hex64 (Submission_GPU_Start));
                                 RCS_Startup.Start (RCS_Attempt, Engine_Status_GPU_Start, Engine_Result);
                                 RCS_Ready := Engine_Result = RCS_Startup.Ready and then RCS_Start_Owner;
                                 RCS_Start_Active := False;
                                 if not RCS_Ready then Runtime_Fault := True; end if;
                                 Publish_Snapshot ("intel-gpu: RCS startup " &
                                   (case Engine_Result is
                                      when RCS_Startup.Rejected => "REJECTED",
                                      when RCS_Startup.Ownership_Lost => "OWNERSHIP-LOST",
                                      when RCS_Startup.Write_Failed => "WRITE-FAILED",
                                      when RCS_Startup.Readback_Failed => "READBACK-FAILED",
                                      when RCS_Startup.Ready => "READY") &
                                   " (NOT context-registered/submitted)");
                                 if Engine_Result = RCS_Startup.Rejected then
                                    Publish_Snapshot ("intel-gpu: RCS rejection=" &
                                      RCS_Startup.Rejection_Reason'Image
                                        (RCS_Startup.Rejection (RCS_Attempt)));
                                 end if;
                                 if RCS_Ready then Start_Render_Context; end if;
                              end;
                           end if;
                        end;
                     end if;
                  end if;
               end;
            elsif Status = Native_GuC.Firmware_Ready then
               Publish_Snapshot ("intel-gpu: CT backing unavailable; registration skipped");
            end if;
         else
            Startup_Active := False;
            Publish_Snapshot ("intel-gpu: GuC source/owner rejected before upload");
         end if;
      end;
   else
      Publish_Snapshot ("intel-gpu: native GuC startup prerequisites unavailable");
   end if;
   Submit_Budget_Query;
   loop
      Service_Context_Events;
      declare
         New_Fault : Boolean;
      begin
         Context_Drain.Tick (Contexts, Drain_State, New_Fault);
         if New_Fault then
            Publish_Snapshot ("intel-gpu: retired context quarantined; backing retained",
              CuBit.Log_Records.Error);
         end if;
      end;
      declare
         Receipt : aliased CompletionEntry;
         Unused : Unsigned_64;
         Found : Boolean;
         Activity : Activity_Result;
         Consumed : Boolean;
         Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
         pragma Unreferenced (Activity);
      begin
         Unused := Intel_GPU_Diagnostics.Poll_Driver (Receipt'Address);
         Consumed := False;
         if Unused /= 0 then
            Intel_GPU_Budget_Query.Complete
              (Budget_Query, Receipt.token, Now,
               Publication_Owner_Ready and then not Runtime_Fault,
               Receipt.status = COMPLETION_OK, Receipt.msg.tag.label,
               Receipt.msg.tag.length, Receipt.msg.tag.flags, Receipt.msg.tag.reserved,
               Intel_GPU_Buffer_Backing.Budget_Words (Receipt.msg.words), Consumed);
         end if;
         Intel_GPU_Budget_Query.Tick
           (Budget_Query, Now, Publication_Owner_Ready and then not Runtime_Fault);
         if Budget_Reply_Pending and then not Intel_GPU_Budget_Query.Pending (Budget_Query) then
            declare
               Response : Message := NULL_MESSAGE;
               Data : constant Intel_GPU_Buffer_Backing.Budget_Words :=
                 Intel_GPU_Budget_Protocol.Response (Intel_GPU_Budget_Query.Result (Budget_Query));
               Delivered : Unsigned_64;
               pragma Unreferenced (Delivered);
            begin
               Response.tag := (Intel_GPU_Budget_Protocol.Label, 4, 0, 0);
               Response.words := [Data (0), Data (1), Data (2), Data (3)];
               Budget_Reply_Pending := False;
               -- A lost read-only reply requires no resource rollback/replay.
               Delivered := replyCap (Budget_Reply_Slot, Response);
            end;
         end if;
         if not Budget_Logged and then not Intel_GPU_Budget_Query.Pending (Budget_Query) then
            declare
               Budget : constant Intel_GPU_Buffer_Backing.Budget_Snapshot :=
                 Intel_GPU_Budget_Query.Result (Budget_Query);
            begin
               Budget_Logged := True;
               Publish_Snapshot ("intel-gpu: backing budget known=" & Boolean'Image (Budget.Known) &
                 " retained=" & Unsigned_64'Image (Budget.Retained_Bytes) &
                 " free=" & Unsigned_64'Image (Budget.Free_Bytes));
               Publish_Snapshot ("intel-gpu: backing budget slots=" & Natural'Image (Budget.Unused_Slots) &
                 " max-allocation=" & Unsigned_64'Image (Budget.Maximum_Allocation) &
                 " (snapshot; NOT reserved)");
            end;
         end if;
         if Unused /= 0 and then not Consumed and then
           (Application_Pending /= 0 or Private_Pending /= 0 or Update_Pending /= 0 or Buffer_Retirement_Pending /= 0) then
            Buffer_Memory.Complete (Buffer_Pool, Receipt, Consumed);
         end if;
         if Update_Image_Pending then
            Update_Storage.Step (Update_Images);
            if not Update_Storage.Pending (Update_Images) then
               declare
                  Started : Boolean := False;
               begin
                  Update_Image_Pending := False;
                  if Update_Storage.Ready (Update_Images) and then Update_Table_Pages /= 0 then
                     Buffer_Memory.Start (Buffer_Pool,
                       Application_Buffers.Ticket_Slot (Update_Pending),
                       Update_Table_Pages, Started);
                  end if;
                  if not Started then Finish_VM_Update ((Ready => False)); end if;
               end;
            end if;
         end if;
         if not Update_Image_Pending and then
           (Application_Pending /= 0 or Private_Pending /= 0 or Update_Pending /= 0 or Buffer_Retirement_Pending /= 0) then
            Buffer_Memory.Tick (Buffer_Pool);
            Advance_Table_Recycling;
            if not Buffer_Memory.Pending (Buffer_Pool) and then not Recycle_In_Progress then
               if Buffer_Retirement_Pending /= 0 then
                  Finish_Buffer_Retirement;
               elsif Update_Pending /= 0 then
                  Finish_VM_Update (Buffer_Memory.Result (Buffer_Pool));
               elsif Private_Pending /= 0 then
                  Finish_Private_Context (Buffer_Memory.Result (Buffer_Pool));
               else
                  Finish_Application_Buffer (Buffer_Memory.Result (Buffer_Pool));
               end if;
            end if;
         end if;
         Advance_In_Place;
         Grow_Table_Ledger;
         Complete_Render_Activation;
         Intel_GPU_Diagnostics.Tick;
         Application_Maps.Poll (Application_Buffer_State, Application_Map_State);
         -- One candidate from each bounded queue before admission: sustained
         -- client traffic must not starve reclamation. While a retirement is
         -- in flight, leave requests (and reply authority) in the kernel.
         -- Completion polling above drains a separate completion queue, so
         -- a queued client cannot obstruct the supervisor acknowledgement.
         Grow_Map_Metadata;
         if not Map_Metadata_Busy then Grow_Metadata; end if;
         if not Metadata_Busy and then not Map_Metadata_Busy and then not In_Place_Active then
            Poll_Deferred_Closes (Deferred_Closes);
            Poll_Table_Retirement;
            Poll_Image_Retirement;
            Poll_Closed_Table_Retirement;
            Poll_Context_Retirement;
            Poll_Teardown_Buffers;
         end if;
         Found := False;
         if Buffer_Retirement_Pending = 0 and then not Metadata_Busy and then not Map_Metadata_Busy
           and then not In_Place_Active then
            Poll_Service_Request (Sender, Request, Found);
         end if;
         if Found and then Request.tag.label = Intel_GPU_Budget_Protocol.Label then
            Handle_Budget_Query (Sender, Request);
         elsif Found and then Request.tag.label = Native_GPU_Probe_Protocol.Label then
            Handle_Probe (Sender, Request);
         elsif Found and then Request.tag.label = Application_Buffers.Label then
            Handle_Application_Buffer (Sender, Request);
         elsif Found and then Request.tag.label = Application_Maps.Map_Label then
            Handle_Application_Map (Sender, Request);
         elsif Found and then Request.tag.label = Application_Binding.Bind_Label then
            Handle_Application_Bind (Sender, Request);
         elsif Found and then Request.tag.label = Prepare_Context_Label then
            Handle_Context_Preparation (Sender, Request);
         elsif Found and then Request.tag.label = Register_Context_Label then
            Handle_Context_Registration (Sender, Request);
         elsif Found and then Request.tag.label = Submit_Label then
            Handle_Application_Submission (Sender, Request);
         elsif Found and then Request.tag.label = Application_Binding.Update_Label then
            Handle_VM_Update (Sender, Request);
         elsif Found and then Request.tag.label = Intel_GPU_Render_Control.Retirement_Label then
            Handle_Retirement_Query (Sender, Request);
         elsif Found and then Request.tag.label = Intel_GPU_Render_Control.Status_Label then
            Handle_Session_Status (Sender, Request);
         elsif Found and then Request.tag.label = Intel_GPU_Render_Control.Close_Own_Label then
            Handle_Close_Own (Sender, Request);
         elsif Found and then Request.tag.label = Intel_GPU_Render_Control.Label then
            Handle_Render_Control (Sender, Request);
         elsif Found then
            declare
               package DQ renames Intel_GPU_Device_Query;
               Data : constant DQ.Snapshot :=
                 (Device => PCI_Device, Revision => PCI_Revision,
                  Topology_Observed => Intel_GPU_Native_Reset.ADS_Observed and then
                    Intel_GPU_Native_Reset.ADS_Execution_Units.Valid,
                  DSS_Mask => Intel_GPU_Native_Reset.ADS_Execution_Units.DSS_Mask,
                  EU_Mask => Intel_GPU_Native_Reset.ADS_Execution_Units.EU_Mask);
               Response : constant DQ.Words := DQ.Respond
                 (Data, Request.tag.label, Request.tag.length, Request.tag.flags,
                  Request.tag.reserved,
                  [Request.words (0), Request.words (1),
                   Request.words (2), Request.words (3)],
                  Timestamp_Hz => Intel_GPU_Native_Reset.Timestamp_Hz,
                  Memory_Policy => Memory_Query_Policy (Sender, Request),
                  VM_Policy => VM_Query_Policy (Sender, Request));
               Reply_Message : Message := NULL_MESSAGE;
            begin
               Reply_Message.tag := (Request.tag.label, 4, 0, 0);
               Reply_Message.words :=
                 [Response (0), Response (1), Response (2), Response (3)];
               Unused := reply (Sender, Reply_Message);
            end;
         end if;
         if not Metadata_Busy and then not Update_Image_Pending and then not In_Place_Active
           and then not Buffer_Memory.Local_Work_Pending (Buffer_Pool) then
            Activity := Wait_For_Activity_Until
              (if Now < Unsigned_64'Last - 10 then Now + 10 else Now);
         end if;
      end;
   end loop;
end Main;
