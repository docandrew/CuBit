with Intel_GPU_ADLN_L3;
with CuBit.Capability_Grants;
with Intel_GPU_Probe_Export;
with Native_GPU_Probe_Protocol;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_Boot;
with Intel_GPU_Device_Query;
with Intel_GPU_Diagnostics;
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
with Intel_GPU_Buffer_Memory;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Application_State;
with Intel_GPU_Application_Submit;
with Intel_GPU_Application_Image;
with Intel_GPU_Application_Image.Publication;
with Intel_GPU_Buffer_Requests;
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
   package Buffer_Memory is new Intel_GPU_Buffer_Memory (Publication_Owner_Ready);
   Buffer_Pool : Buffer_Memory.Pool;
   Submission_Slot : constant Intel_GPU_Buffer_Backing.Slot := 1;
   Submission_Bytes : constant Unsigned_64 :=
     Intel_GPU_Submission_Backing.After_Last - Intel_GPU_Submission_Backing.First;
   Submission_CPU : constant Unsigned_64 := Intel_GPU_Buffer_Backing.CPU_Base;
   -- This is the first allocation in the arena. Verify that identity before
   -- using the fixed mapping instances below; never infer it from the slot ID.
   Submission_Allocation : Intel_GPU_Buffer_Reply.Backing;
   -- Startup must bind the admission controller through trusted bootstrap.
   -- It deliberately stays unbound until that supervisor integration exists.
   Render_Admission : Intel_GPU_Render_Control.Controller;
   function Application_Session (From, Stamp : Unsigned_64) return Unsigned_64 is
     (Intel_GPU_Render_Control.Resolve (Render_Admission, From, Stamp));
   package Application_Buffers is new Intel_GPU_Buffer_Requests
     (Application_Session, Publication_Owner_Ready, First_Slot => 2);
   Application_Buffer_State : Application_Buffers.Service;
   Boot_Update_Allocation : Intel_GPU_Buffer_Reply.Backing;
   Boot_Update_Candidate : Submission_Buffers.VM.Image;
   procedure Application_Recipient
     (From, Stamp : Unsigned_64; Slot : out CapabilitySlot;
      Identity : out Unsigned_64) is
      Session : constant Unsigned_64 := Application_Session (From, Stamp);
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
   begin
      Slot := 0;
      Identity := 0;
      if Session <= Base or else
        Session > Base + Intel_GPU_Render_Sessions.Capacity then return; end if;
      -- Proposed startup layout: one immutable recipient endpoint per session
      -- in40..55, after hardware pages32..39. No capability is minted here.
      -- Admission MUST stay unbound until startup reserves and populates this
      -- range and guarantees slot stability through sharing/retirement.
      Slot := CapabilitySlot (39 + Session - Base);
      Identity := Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, From, Stamp);
   end Application_Recipient;
   package Application_Maps is new Application_Buffers.Sharing (Application_Recipient);
   Application_Map_State : Application_Maps.Mapping_Table;
   procedure Handle_Application_Map (From : ProcessID; Msg : Message) is
      Response : Application_Buffers.Words;
      Created : Application_Maps.Mapping_ID;
      Reply_Message : Message := NULL_MESSAGE;
      Delivery : Unsigned_64;
   begin
      Application_Maps.Handle
        (Application_Buffer_State, Application_Map_State,
         Unsigned_64 (From), Msg.authorityTag, Msg.tag.label, Msg.tag.length,
         Msg.tag.flags, Msg.tag.reserved,
         [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Response, Created);
      Reply_Message.tag := (Application_Maps.Map_Label, 4, 0, 0);
      Reply_Message.words := [Response (0), Response (1), Response (2), Response (3)];
      Delivery := reply (From, Reply_Message);
      if Delivery /= 1 then
         Application_Maps.Reject_Delivery (Application_Map_State, Created);
      end if;
   end Handle_Application_Map;
   Application_Pending : Application_Buffers.Ticket := 0;
   -- Private parents are retained even after admission is revoked. They are
   -- never passed to Application_Buffers.Complete or exported as BO handles.
   package Application_State renames Intel_GPU_Application_State;
   package Application_VM renames Application_State.VM;
   package Application_Binding is new Application_Buffers.Binding (Application_VM);
   Private_Contexts : Application_State.Context_Array renames Application_State.Items;
   Private_Pending : Application_Buffers.Ticket := 0;
   Update_Pending : Application_Buffers.Ticket := 0;
   Private_Session, Private_Identity : Unsigned_64 := 0;
   -- Initial bounded VM budget:64 table pages plus the private context image.
   -- No application data is placed in this allocation.
   Private_Table_Pages : constant := Application_State.Table_Pages;
   Private_Pages : constant Intel_GPU_Buffer_Backing.Page_Count :=
     Intel_GPU_Submission_Image.Byte_Count / 4096 + Private_Table_Pages;
   procedure Finish_Private_Context (Backing : Intel_GPU_Buffer_Reply.Backing) is
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
      Consumed : Boolean;
   begin
      Application_Buffers.Finish_Private
        (Application_Buffer_State, Private_Pending, Consumed);
      if not Consumed then return; end if;
      Private_Pending := 0;
      if Private_Session <= Base or else Private_Session > Base +
        Intel_GPU_Render_Sessions.Capacity then return; end if;
      declare
         Index : constant Positive := Positive (Private_Session - Base);
         Pages : Application_VM.Backing_Pages;
         Initialized : Boolean;
      begin
         Private_Contexts (Index).Parent := Backing;
         Private_Contexts (Index).Ready := Backing.Ready and then
           Backing.Bytes = Unsigned_64 (Private_Pages) * 4096 and then
           Publication_Owner_Ready and then
           Application_Session (Private_Identity and 16#FFFF_FFFF#,
                                Private_Session) = Private_Session and then
           Intel_GPU_Render_Control.Recipient_Identity
             (Render_Admission, Private_Identity and 16#FFFF_FFFF#,
              Private_Session) = Private_Identity;
         if not Private_Contexts (Index).Ready then return; end if;
         Private_Contexts (Index).Ready := False;
         Private_Contexts (Index).Context := Intel_GPU_Buffer_Reply.Slice
           (Backing, 0, Intel_GPU_Submission_Image.Byte_Count);
         Private_Contexts (Index).Tables := Intel_GPU_Buffer_Reply.Slice
           (Backing, Intel_GPU_Submission_Image.Byte_Count, Private_Table_Pages * 4096);
         if not Private_Contexts (Index).Context.Ready or else
           not Private_Contexts (Index).Tables.Ready then return; end if;
         for P in Application_VM.Page_Number loop
            Pages (P) := Intel_GPU_Buffer_Reply.Page_Address
              (Private_Contexts (Index).Tables, Unsigned_64 (P - 1) * 4096);
         end loop;
         Application_VM.Initialize (Private_Contexts (Index).Source, Pages, Initialized);
         Private_Contexts (Index).Ready := Initialized;
      end;
   end Finish_Private_Context;
   procedure Start_Private_Context
     (Session, Identity : Unsigned_64; Started : out Boolean) is
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
   begin
      Started := False;
      if Session <= Base or else Session > Base + Intel_GPU_Render_Sessions.Capacity
        or else Private_Pending /= 0 or else
        Application_Session (Identity and 16#FFFF_FFFF#, Session) /= Session or else
        Intel_GPU_Render_Control.Recipient_Identity
          (Render_Admission, Identity and 16#FFFF_FFFF#, Session) /= Identity
      then return; end if;
      declare
         Index : constant Positive := Positive (Session - Base);
      begin
         if Private_Contexts (Index).Attempted then return; end if;
         Application_Buffers.Reserve_Private (Application_Buffer_State, Private_Pending);
         if Private_Pending = 0 then return; end if;
         Private_Contexts (Index).Attempted := True;
         Private_Session := Session;
         Private_Identity := Identity;
         Buffer_Memory.Start
           (Buffer_Pool, Intel_GPU_Buffer_Backing.Slot (Private_Pending),
            Private_Pages, Started);
         if not Started then Finish_Private_Context ((Ready => False)); end if;
      end;
   end Start_Private_Context;
   procedure Retire_Application_Resources (Session : Unsigned_64);
   procedure Handle_Application_Bind (From : ProcessID; Msg : Message) is
      Session : constant Unsigned_64 := Application_Session (Unsigned_64 (From), Msg.authorityTag);
      Identity : constant Unsigned_64 := Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, Unsigned_64 (From), Msg.authorityTag);
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
      Response : Application_Buffers.Words := [Application_Buffers.Denied, 1, 0, 0];
      Reply_Message : Message := NULL_MESSAGE;
      Delivery : Unsigned_64;
   begin
      if Session > Base and then Session <= Base + Intel_GPU_Render_Sessions.Capacity then
         declare Index : constant Positive := Positive (Session - Base); begin
            Response (0) := Application_Buffers.Unavailable;
            if Private_Contexts (Index).Ready then
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
      Application_Buffers.Handle
        (Application_Buffer_State, Unsigned_64 (From), Msg.authorityTag,
         Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
         [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Response, Deferred);
      if Deferred /= 0 then
         if saveReplyCap (Unsigned_64 (Application_Reply_Slot)) = 1 then
            Application_Pending := Deferred;
            Buffer_Memory.Start
              (Buffer_Pool, Intel_GPU_Buffer_Backing.Slot (Deferred),
               Intel_GPU_Buffer_Backing.Page_Count (Msg.words (2) / 4096), Started);
            if not Started then Finish_Application_Buffer ((Ready => False)); end if;
            return;
         end if;
         -- No allocation was submitted. Retire the ticket and use only the
         -- current thread's reply authority; never fall back to a saved PID.
         Application_Buffers.Complete
           (Application_Buffer_State, Deferred, (Ready => False), Response, Consumed);
      end if;
      Reply_Message.tag := (Application_Buffers.Label, 4, 0, 0);
      Reply_Message.words := [Response (0), Response (1), Response (2), Response (3)];
      Ignored := reply (From, Reply_Message);
   end Handle_Application_Buffer;
   procedure Retire_Application_Context (Session : Unsigned_64);
   procedure Retire_Application_Resources (Session : Unsigned_64) is
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
   begin
      if Session > Base and then Session <= Base + Intel_GPU_Render_Sessions.Capacity then
         Private_Contexts (Positive (Session - Base)).Ready := False;
      end if;
      Retire_Application_Context (Session);
      Application_Buffers.Retire_Session (Application_Buffer_State, Session);
      Application_Maps.Retire_Session (Application_Map_State, Session);
   end Retire_Application_Resources;
   procedure Handle_Render_Control (From : ProcessID; Msg : Message) is
      package Control renames Intel_GPU_Render_Control;
      Response : Control.Words;
      Reply_Message : Message := NULL_MESSAGE;
      Delivery : Unsigned_64;
      Started : Boolean;
   begin
      -- Only abort can succeed with Ready=False. Enabling reserve/activate
      -- requires the startup slot contract AND a usable render backend; raw
      -- GuC/owner readiness is not sufficient. Bootstrap remains unbound.
      Control.Handle (Render_Admission, Unsigned_64 (From), Msg.authorityTag,
        False, Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
        [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Response);
      if Response (0) = Control.OK and then Msg.words (3) = Control.Abort_Session then
         -- Handle has closed admission before handles/grants are drained.
         Retire_Application_Resources (Response (2));
      end if;
      if Response (0) = Control.OK and then Msg.words (3) = Control.Activate then
         Start_Private_Context (Response (2), Msg.words (1), Started);
         if not Started then
            Control.Reject_Delivery (Render_Admission, Msg.words (1), Response (2));
            Retire_Application_Resources (Response (2));
            Response (0) := Control.Unavailable;
         end if;
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
   Runtime_Admitted, Runtime_Fault : Boolean := False;
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
   procedure Publish_Snapshot (Text : String);
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
   -- Context table owns all post-probe fence allocations.
   function Context_Fence_Allowed (Fence : Unsigned_16) return Boolean;

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
   function Context_Owner return Boolean is
     (RCS_Ready and then RCS_Page_Ready (Engine_Status_GPU_Start) and then
      CT_Receive_Owner_Ready and then Runtime_Owner_Ready);
   package Context_Input is new Intel_GPU_Native_Context_Read
     (Context_Owner, Intel_GPU_Native_Reset.ADS_Topology);
   Context_Init : Intel_GPU_ADLN_Context_Init.Segment;
   procedure Queue_Context
     (Payload : Context_Event.Words; Fence : Unsigned_16;
      Result : out Context_Life.Send_Result) is
      Status : CT_Send.Result;
      use type CT_Send.Result;
   begin
      Result := Context_Life.Uncertain;
      -- Only controls/notifications with a retained table-owned fence may
      -- enter the shared transport, across all registered contexts.
      if not Context_Owner or else not Context_Fence_Allowed (Fence) then return; end if;
      CT_Send.Send (Send_Channel, CT_Send.Words (Payload), Fence, Status);
      if not Context_Owner then return; end if;
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
     (16, 100, 65535, Context_Driver, Context_Owner, Retain_Context_Event,
      First_ID => First_Context_ID);
   Contexts : Context_Pool.Table;
   package Context_Drain is new Context_Pool.Draining (Runtime_Now);
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
   function Context_Fence_Allowed (Fence : Unsigned_16) return Boolean is
     (Context_Pool.Owns_Fence (Contexts, Fence));
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
   procedure Publish_Snapshot (Text : String) is
   begin
      Intel_GPU_Diagnostics.Capture (Text);
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
      Private_Contexts (Preparing_Index).Ready and then
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
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
      Code : Unsigned_64 := Application_Buffers.Denied;
      Prepared : Boolean := False;
      Backing : Application_Images.Tables.Mappings;
      Status : Application_Publication.Result;
      use type Application_Publication.Result;
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      if Update_Pending /= 0 then
         Code := Application_Buffers.Unavailable;
      elsif Session > Base and then Session <= Base + Intel_GPU_Render_Sessions.Capacity then
         Code := Application_Buffers.Bad_Request;
         if Msg.tag.length = 4 and then Msg.tag.flags = 0 and then Msg.tag.reserved = 0 and then
           Msg.words (0) = 1 and then Msg.words (1) = 0 and then
           Msg.words (2) = 0 and then Msg.words (3) = 0
         then
            Code := Application_Buffers.Unavailable;
            Preparing_Index := Natural (Session - Base);
            Preparing_Identity := Identity; Preparing_Session := Session;
            if Application_Image_Owner then
               Application_VM.Seal (Private_Contexts (Preparing_Index).Source, Prepared);
               if Prepared then
                  for P in Application_VM.Page_Number loop
                     Backing (P) :=
                       (Private_Contexts (Preparing_Index).Tables.CPU_Address + Unsigned_64 (P - 1) * 4096,
                        Intel_GPU_Buffer_Reply.Page_Address
                          (Private_Contexts (Preparing_Index).Tables, Unsigned_64 (P - 1) * 4096));
                  end loop;
                  Application_Publication.Publish
                    (Application_Images_State (Preparing_Index),
                     Private_Contexts (Preparing_Index).Source, Backing,
                     Private_Contexts (Preparing_Index).Context, Runtime_Ledger, Status);
                  Prepared := Status = Application_Publication.Published;
               end if;
               -- No later offline binds or second publication attempt. The
               -- retained source is sealed; future live binds need VM_Update.
               Private_Contexts (Preparing_Index).Ready := False;
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
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
      Code : Unsigned_64 := Application_Buffers.Denied;
      ID : Unsigned_32 := Context_Pool.No_Context;
      Accepted : Boolean := False;
      Status : Context_Driver.Result;
      use type Context_Driver.Result;
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      if Update_Pending /= 0 then
         Code := Application_Buffers.Unavailable;
      elsif Session > Base and then Session <= Base + Intel_GPU_Render_Sessions.Capacity then
         Code := Application_Buffers.Bad_Request;
         if Msg.tag.length = 4 and then Msg.tag.flags = 0 and then Msg.tag.reserved = 0 and then
           Msg.words (0) = 1 and then Msg.words (1) = 0 and then
           Msg.words (2) = 0 and then Msg.words (3) = 0
         then
            Code := Application_Buffers.Unavailable;
            -- Generic actuals are evaluated during elaboration, before
            -- Ring_Owner can reject a missing allocation.
            if Private_Contexts (Positive (Session - Base)).Context.Ready then
               declare
                  Index : constant Positive := Positive (Session - Base);
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
                          (Contexts, GPU, Address_Layout.Runtime_First, 256, 1000, 500_000, True,
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
      not Runtime_Fault and then Context_Owner and then PCI_Device = 16#46D2# and then
      Application_Setup_Complete (Selected_Index) and then
      Application_Session (Selected_Sender, Selected_Stamp) = Selected_Session and then
      Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, Selected_Sender, Selected_Stamp) = Selected_Identity and then
      Selected_Context /= Context_Pool.No_Context and then
      Context_Pool.Session_Context (Contexts, Selected_Session) = Selected_Context and then
      Context_Pool.State (Contexts, Selected_Context) /= Context_Life.Quarantined and then
      Private_Contexts (Selected_Index).Ready and then
      Private_Contexts (Selected_Index).Context.Ready and then
      Private_Contexts (Selected_Index).Context.CPU_Address = Selected_CPU and then
      Private_Contexts (Selected_Index).Context.Bytes = Selected_Bytes and then
      Intel_GPU_Buffer_Reply.Page_Address (Private_Contexts (Selected_Index).Context, 0) = Selected_DMA and then
      Selected_GPU /= 0 and then Application_Publication.GPU_Address
        (Application_Images_State (Selected_Index)) = Selected_GPU);
   function Submission_Work_Owner return Boolean is
     (Update_Pending = 0 and then Submission_Owner and then
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
   end Enable_Submission;
   procedure Publish_Submission (GPU : Unsigned_64; Sequence : Unsigned_32; OK : out Boolean) is
      WM : Unsigned_32;
      Segment : Intel_GPU_ADLN_Context_Init.Segment;
   begin
      OK := False;
      if not Submission_Work_Owner then return; end if;
      WM := Context_Input.Read_WM_Chicken2;
      Segment := Intel_GPU_ADLN_Context_Init.Build_Batch
        (Submission_Work_Owner and then WM /= Unsigned_32'Last, WM, Sequence, GPU);
      Application_Ring.Append (Application_Channels (Selected_Index), Segment, OK);
   end Publish_Submission;
   procedure Notify_Submission (OK : out Boolean) is
      Status : Context_Driver.Result;
      use type Context_Driver.Result;
   begin
      Context_Pool.Notify_Work (Contexts, Selected_Context, True, Status);
      OK := Status = Context_Driver.Queued;
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
   procedure Handle_Application_Submission (From : ProcessID; Msg : Message) is
      Session : constant Unsigned_64 := Application_Session (Unsigned_64 (From), Msg.authorityTag);
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
      Code : Unsigned_64 := Application_Buffers.Denied;
      Completion : Unsigned_32 := 0;
      Status : Application_Submission.Result;
      use type Application_Submission.Result;
      use type Application_Submission.Phase;
      Response : Message := NULL_MESSAGE;
      Delivered : Unsigned_64;
   begin
      Selected_Index := 0;
      if Session > Base and then Session <= Base + Intel_GPU_Render_Sessions.Capacity then
         Code := Application_Buffers.Bad_Request;
         -- [version | (byte offset << 32), BO handle, raw48 GPU address, bytes]
         if Msg.tag.length = 4 and then Msg.tag.flags = 0 and then Msg.tag.reserved = 0 and then
           (Msg.words (0) and 16#FFFF_FFFF#) = 1
         then
            Selected_Index := Natural (Session - Base);
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
               if Update_Pending = 0 and then Submission_Owner and then
                 Context_Pool.State (Contexts, Selected_Context) = Context_Life.Disabled
               then
                  if Application_Submission.Current (Application_Submissions (Selected_Index)) =
                    Application_Submission.Uninitialized
                  then
                     Application_Submission.Initialize (Application_Submissions (Selected_Index),
                       Application_Setup_Complete (Selected_Index));
                  end if;
                  Application_Submission.Execute (Application_Submissions (Selected_Index),
                    Msg.words (1), Msg.words (2), Shift_Right (Msg.words (0), 32), Msg.words (3),
                    Status, Completion);
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
      Private_Contexts (Update_Index).Ready and then
      Application_Session (Update_Sender, Update_Stamp) = Update_Session and then
      Intel_GPU_Render_Control.Recipient_Identity
        (Render_Admission, Update_Sender, Update_Stamp) = Update_Identity and then
      Context_Pool.Session_Context (Contexts, Update_Session) = Update_Context and then
      Update_Context /= Context_Pool.No_Context and then not Context_Pool.Failed (Contexts));
   function Update_Exclusive return Boolean is
   begin
      if not Update_Owner or else not Update_Held or else Update_Pending = 0 then
         return False;
      end if;
      -- Current native backend is RCS-only, with synchronous completed/flush
      -- markers and disable acknowledgments between batches. No OA admission.
      -- Refuse publication while ANY retained context is not disabled.
      for I in 1 .. Context_Pool.Count (Contexts) loop
         if Context_Pool.State (Contexts, First_Context_ID + Unsigned_32 (I - 1)) /=
           Context_Life.Disabled then return False; end if;
      end loop;
      return True;
   end Update_Exclusive;
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
   procedure Drain_Update (OK : out Boolean) is
   begin
      -- The hold was acquired before asynchronous allocation. The service
      -- blocks all new submissions until finish; previous submits are bounded
      -- synchronous transactions with completion and scheduling disable.
      OK := Update_Exclusive;
   end Drain_Update;
   procedure Publish_Update (OK : out Boolean) is
      Mappings : Application_Images.Tables.Mappings;
   begin
      OK := False;
      if not Update_Exclusive then return; end if;
      declare
         Tables : Intel_GPU_Buffer_Reply.Backing renames
           Application_State.Updates (Update_Pending).Tables;
      begin
         if not Tables.Ready then return; end if;
         for P in Application_VM.Page_Number loop
            Mappings (P) :=
              (Tables.CPU_Address + Unsigned_64 (P - 1) * 4096,
               Intel_GPU_Buffer_Reply.Page_Address (Tables, Unsigned_64 (P - 1) * 4096));
         end loop;
         Preparing_Index := Update_Index;
         Preparing_Session := Update_Session; Preparing_Identity := Update_Identity;
         Live_Images.Publish_Tables
           (Application_Images_State (Update_Index), Private_Contexts (Update_Index).Source,
            Application_State.Updates (Update_Pending).Candidate, Mappings, OK);
         Preparing_Index := 0; Preparing_Session := 0; Preparing_Identity := 0;
      end;
   end Publish_Update;
   procedure Invalidate_Update (OK : out Boolean) is
      Attempt : Live_TLB.Attempt;
      Status : Live_TLB.Result;
      use type Live_TLB.Result;
   begin
      Live_TLB.Execute (Attempt, Status);
      OK := Status = Live_TLB.Complete;
   end Invalidate_Update;
   procedure Resume_Update (OK : out Boolean) is
   begin
      -- Submit-on-demand: preserve acknowledged disabled state. Release the
      -- software hold only after the coordinator commits and image is adopted.
      OK := Update_Exclusive;
   end Resume_Update;
   package Live_VM is new Intel_GPU_VM_Update
     (Update_Owner, Drain_Update, Publish_Update, Invalidate_Update, Resume_Update);
   Live_VM_States : array (Private_Contexts'Range) of Live_VM.State;
   procedure Execute_Update is new Application_Binding.Handle_Update (Live_VM);
   procedure Fail_Update is
   begin
      if Update_Index in Private_Contexts'Range then
         Live_VM.Fail (Live_VM_States (Update_Index));
         Intel_GPU_Render_Control.Reject_Delivery
           (Render_Admission, Update_Identity, Update_Session);
         Retire_Application_Resources (Update_Session);
      end if;
   end Fail_Update;
   procedure Finish_VM_Update (Backing : Intel_GPU_Buffer_Reply.Backing) is
      Tables : Application_VM.Backing_Pages;
      Response : Application_Buffers.Words := [Application_Buffers.Unavailable, 1, 0, 0];
      Message_Out : Message := NULL_MESSAGE;
      OK, Consumed : Boolean;
      Delivered : Unsigned_64;
   begin
      if Update_Pending = 0 then return; end if;
      Application_State.Updates (Update_Pending).Tables := Backing;
      if Backing.Ready and then Backing.Bytes = Private_Table_Pages * 4096 and then
        Update_Exclusive
      then
         for P in Application_VM.Page_Number loop
            Tables (P) := Intel_GPU_Buffer_Reply.Page_Address (Backing, Unsigned_64 (P - 1) * 4096);
         end loop;
         Execute_Update
           (Application_Buffer_State, Private_Contexts (Update_Index).Source,
            Application_State.Updates (Update_Pending).Candidate, Tables,
            Live_VM_States (Update_Index), Update_Session, Update_Sender, Update_Stamp,
            Update_Request.tag.label, Update_Request.tag.length, Update_Request.tag.flags,
            Update_Request.tag.reserved,
            [Update_Request.words (0), Update_Request.words (1),
             Update_Request.words (2), Update_Request.words (3)], Response);
         if Response (0) = Application_Buffers.OK then
            Live_Snapshots.Adopt_Committed
              (Private_Contexts (Update_Index).Source,
               Application_State.Updates (Update_Pending).Candidate, OK);
            if OK and then Update_Exclusive then
               Context_Pool.Release_Work (Contexts, Update_Context, OK, Keep_Disabled => True);
            else OK := False; end if;
            if not OK then Response := [Application_Buffers.Unavailable, 1, 0, 0]; end if;
         end if;
      end if;
      if Response (0) /= Application_Buffers.OK then Fail_Update; end if;
      Application_Buffers.Finish_Private (Application_Buffer_State, Update_Pending, Consumed);
      if not Consumed then Fail_Update; Response := [Application_Buffers.Unavailable, 1, 0, 0]; end if;
      Message_Out.tag := (Application_Binding.Update_Label, 4, 0, 0);
      Message_Out.words := [Response (0), Response (1), Response (2), Response (3)];
      Delivered := replyCap (Application_Reply_Slot, Message_Out);
      if Delivered /= 1 then Fail_Update; end if;
      Update_Pending := 0; Update_Held := False; Update_Index := 0;
   end Finish_VM_Update;
   procedure Handle_VM_Update (From : ProcessID; Msg : Message) is
      Session : constant Unsigned_64 := Application_Session (Unsigned_64 (From), Msg.authorityTag);
      Base : constant Unsigned_64 := Intel_GPU_Render_Sessions.Tag_Base;
      Response : Message := NULL_MESSAGE;
      Code : Unsigned_64 := Application_Buffers.Denied;
      Status : Application_Binding.Preparation_Result;
      use type Application_Binding.Preparation_Result;
      Started, Consumed : Boolean;
      Delivered : Unsigned_64;
   begin
      if Session > Base and then Session <= Base + Intel_GPU_Render_Sessions.Capacity then
         Code := Application_Buffers.Unavailable;
         if Update_Pending = 0 and then Application_Pending = 0 and then Private_Pending = 0 then
            Update_Index := Natural (Session - Base); Update_Session := Session;
            Update_Sender := Unsigned_64 (From); Update_Stamp := Msg.authorityTag;
            Update_Identity := Intel_GPU_Render_Control.Recipient_Identity
              (Render_Admission, Update_Sender, Update_Stamp);
            Update_Context := Context_Pool.Session_Context (Contexts, Session);
            if Update_Owner and then Live_VM.Can_Submit (Live_VM_States (Update_Index)) then
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
                  Application_Buffers.Reserve_Private (Application_Buffer_State, Update_Pending);
                  if Update_Pending /= 0 then
                     Context_Pool.Hold_Work (Contexts, Update_Context, Update_Held);
                     if Update_Exclusive and then saveReplyCap (Unsigned_64 (Application_Reply_Slot)) = 1 then
                        Update_Request := Msg;
                        Buffer_Memory.Start (Buffer_Pool, Intel_GPU_Buffer_Backing.Slot (Update_Pending),
                                             Private_Table_Pages, Started);
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
      Response.tag := (Application_Binding.Update_Label, 4, 0, 0);
      Response.words := [Code, 1, 0, 0];
      Delivered := reply (From, Response);
   end Handle_VM_Update;
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
      Image : Initial_Ring.Target_Image;
      Image_Read : Boolean := False;
      Image_Nonzero : Natural := 0;
      Image_Hash : Unsigned_32 := 16#811C9DC5#;
      Saved_Head, Saved_Tail, H2G_Head, H2G_Tail, H2G_Status : Unsigned_32 := 0;
      Saved_OK, H2G_OK : Boolean := False;
      Batch_Value : Unsigned_64 := Unsigned_64'Last;
      Batch_Read : Boolean := False;
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
      -- One retained RCS0 context, with a disjoint retained fence interval.
      -- Only one four-word scheduling response is outstanding at a time.
      -- This bring-up policy explicitly requests preempt-to-idle on quantum
      -- expiry; it is a driver policy choice, not a probed hardware property.
      Context_Pool.Open (Contexts, Submission_GPU_Start,
        Address_Layout.Runtime_First, 256, 1000, 500_000, True,
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
            Initial_Ring.Read_Pixels (Pixels, Pixels_Read);
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
            if TLB_Status = TLB_Probe.Complete then Update_At := Finished; end if;
         end if;
         TLB_Armed := False;
         if TLB_Status /= TLB_Probe.Complete then
            Context_Pool.Fail (Contexts); Runtime_Fault := True;
         end if;
      end if;
      Publish_Snapshot ("intel-gpu: boot VM update stage=" & Update_Stage'Image (Update_At));
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
      Publish_Snapshot ("intel-gpu: draw corners=" & Hex (Pixels (1)) & "/" &
        Hex (Pixels (2)) & "/" & Hex (Pixels (3)) & "/" & Hex (Pixels (4)));
      Publish_Snapshot ("intel-gpu: draw image read=" & Boolean'Image (Image_Read) &
        " nonzero=" & Natural'Image (Image_Nonzero) & " word-hash=" & Hex (Image_Hash));
      Publish_Snapshot ("intel-gpu: private batch read=" & Boolean'Image (Batch_Read) &
        " value=" & Unsigned_64'Image (Batch_Value) &
        "; expected=" & Unsigned_32'Image (Intel_GPU_Submission_Image.Batch_Probe_Value));
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
                        Application_Buffers.Reserve_Private (Application_Buffer_State, Ticket);
                        if Ticket /= 0 then
                           Boot_Update_Allocation := Buffer_Memory.Acquire
                             (Buffer_Pool, Intel_GPU_Buffer_Backing.Slot (Ticket), 4);
                           Application_Buffers.Finish_Private
                             (Application_Buffer_State, Ticket, Consumed);
                           if not Consumed then Boot_Update_Allocation := (Ready => False); end if;
                        end if;
                        Publish_Snapshot ("intel-gpu: update tables zeroed-retained=" &
                          Boolean'Image (Boot_Update_Allocation.Ready));
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
   loop
      Service_Context_Events;
      declare
         New_Fault : Boolean;
      begin
         Context_Drain.Tick (Contexts, Drain_State, New_Fault);
         if New_Fault then
            Publish_Snapshot ("intel-gpu: retired context quarantined; backing retained");
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
         if Unused /= 0 and then
           (Application_Pending /= 0 or Private_Pending /= 0 or Update_Pending /= 0) then
            Buffer_Memory.Complete (Buffer_Pool, Receipt, Consumed);
         end if;
         if Application_Pending /= 0 or Private_Pending /= 0 or Update_Pending /= 0 then
            Buffer_Memory.Tick (Buffer_Pool);
            if not Buffer_Memory.Pending (Buffer_Pool) then
               if Update_Pending /= 0 then
                  Finish_VM_Update (Buffer_Memory.Result (Buffer_Pool));
               elsif Private_Pending /= 0 then
                  Finish_Private_Context (Buffer_Memory.Result (Buffer_Pool));
               else
                  Finish_Application_Buffer (Buffer_Memory.Result (Buffer_Pool));
               end if;
            end if;
         end if;
         Intel_GPU_Diagnostics.Tick;
         Application_Maps.Poll (Application_Map_State);
         Poll_Service_Request (Sender, Request, Found);
         if Found and then Request.tag.label = Native_GPU_Probe_Protocol.Label then
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
                  Timestamp_Hz => Intel_GPU_Native_Reset.Timestamp_Hz);
               Reply_Message : Message := NULL_MESSAGE;
            begin
               Reply_Message.tag := (Request.tag.label, 4, 0, 0);
               Reply_Message.words :=
                 [Response (0), Response (1), Response (2), Response (3)];
               Unused := reply (Sender, Reply_Message);
            end;
         end if;
         Activity := Wait_For_Activity_Until
           (if Now < Unsigned_64'Last - 10 then Now + 10 else Now);
      end;
   end loop;
end Main;
