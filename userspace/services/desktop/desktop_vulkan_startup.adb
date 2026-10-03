with Vulkan_Upload_Recording;
with Vulkan_Device_Upload_FFI;
with Vulkan_Owned_Source;
with Vulkan_Device_Source_FFI;
with Vulkan_Device_Pipeline_FFI;
with Compositor_Target_Damage;
with Vulkan_Scene_Recording;
with Vulkan_Device_Targets_FFI;
with System;
with Vulkan_Context_Owner;
with Vulkan_Submission;
package body Desktop_Vulkan_Startup with SPARK_Mode,
  Refined_State => (Engine => (Device, Context, Submission, Targets, Budget, Pool, Damage, Configured, Stopping, Pipeline, Pipeline_Ticket, Backings, Upload, Coverage, Extents, Writer, Active_Write, Write_Plan, Write_Discard, Glyph_Sources, Source_Readers, Reader_Sequence)) is
   package CP renames Upload_Progress;
   package GS renames Vulkan_Glyph_Sources;
   Glyph_Sources : GS.State;
   package G renames Compositor_Upload;
   type Coverage_Array is array (Backing_Slot) of CP.State;
   Coverage : Coverage_Array;
   type Extent_Record is record
      Width, Height : G.Edge := 0;
      Kind : G.Pixel_Format := G.BGRA8;
   end record;
   type Extent_Array is array (Backing_Slot) of Extent_Record;
   Extents : Extent_Array;
   type Writer_Phase is (Available, Producing, Transferring, Unsafe);
   Writer : Writer_Phase := Available;
   Active_Write : Write_Ticket := No_Write;
   Write_Plan : G.Plan;
   Write_Discard : Boolean := False;
   use type CP.Phase, CP.Ticket, G.Pixel_Format;
   package U renames Vulkan_Upload_Owner;
   Upload : U.State;
   use type U.Phase;
   package S renames Vulkan_Owned_Source;
   type Backing_Array is array (Backing_Slot) of S.State;
   Backings : Backing_Array;
   package D renames Vulkan_Device_Owner;
   package O renames Vulkan_Owned_Targets;
   package F renames Vulkan_Frame;
   package TD renames Compositor_Target_Damage;
   package V renames Vulkan_Submission;
   package P renames O.P;
   type Reader_Record is record
      Source : V.Source_Ticket := V.No_Source;
      Serial : Interfaces.Unsigned_64 := 0;
   end record;
   type Reader_Array is array (Reader_Index) of Reader_Record;
   Source_Readers : Reader_Array;
   Reader_Sequence : Interfaces.Unsigned_64 := 0;
   use type Interfaces.Unsigned_64;
   Damage : TD.State := TD.Open (1, 1);
   Configured, Stopping : Boolean := False;
   use type V.Phase, V.Source_Slot, V.Source_Ticket, P.Ticket, P.Slot, F.Admission, F.Completion,
     Vulkan_Scene_Recording.Outcome, Vulkan_Scene.Phase, Interfaces.Unsigned_32;
   Targets : O.State;
   Budget : O.A.State := O.A.Open (0);
   Pool : O.P.State := O.P.Open (1);
   type Pipeline_Phase is (Fresh, Live, Closed, Quarantined);
   Pipeline : Pipeline_Phase := Fresh;
   Pipeline_Ticket : Vulkan_Context_Owner.Child := Vulkan_Context_Owner.No_Child;
   Device : D.State;
   Context : Vulkan_Context_Owner.State;
   Submission : Vulkan_Submission.State := Vulkan_Submission.Open (System.Null_Address);
   use type Vulkan_Context_Owner.Phase, Vulkan_Context_Owner.Child, System.Address;
   function Valid return Boolean is
     (O.A.Valid (Budget) and then
      (for all Index in Backing_Slot => CP.Valid (Coverage (Index)) and
         (if CP.Current (Coverage (Index)) in CP.Writing | CP.Pending then
            Writer /= Available and Index = Active_Write.Index)) and then
      (if Writer = Producing then V.Current (Submission) = V.Idle and
         Write_Active (Active_Write)) and then
      (if Writer = Transferring then V.Current (Submission) = V.Pending and
         CP.Current (Coverage (Active_Write.Index)) = CP.Pending and
         CP.Active (Coverage (Active_Write.Index), Active_Write.Chunk)) and then P.Valid (Pool) and then TD.Valid (Damage) and then (if D.Current (Device) = D.Fresh then
        Writer = Available and Vulkan_Context_Owner.Current (Context) = Vulkan_Context_Owner.Fresh and
        Vulkan_Context_Owner.Empty (Context) and
        Vulkan_Submission.Owner_Context (Submission) = System.Null_Address and
        Vulkan_Submission.Can_Destroy (Submission)));
   function Current return D.Phase is (D.Current (Device));
   procedure Initialize (Admitted_Slot : Interfaces.Unsigned_64) is
   begin
      -- Guard here also establishes the fresh-context precondition when the
      -- package has already progressed to live/retired/quarantined state.
      if D.Current (Device) = D.Fresh then
         D.Start (Device, Context, Submission, Admitted_Slot);
      end if;
   end Initialize;
   procedure Check_Health (Usable : out Boolean) is
   begin D.Check_Health (Device, Usable); end Check_Health;
   function Target_Phase return O.Phase is (O.Current (Targets));
   function Charged_Bytes return Natural is (O.A.Charged (Budget));
   function Configured_Limit return Natural is (O.A.Limit (Budget));
   function Can_Prepare_Targets return Boolean is
     (D.Current (Device) = D.Ready and not Stopping and O.Untouched (Targets));
   procedure Prepare_Targets
     (Requests : Vulkan_Frame.Targets; Description : System.Address;
      Epoch : O.P.Live_ID; Byte_Limit : Natural;
      Allowed_Types : Interfaces.Unsigned_32; Ready : out Boolean) is
   begin
      Ready := False;
      if not Can_Prepare_Targets then
         return;
      end if;
      Budget := O.A.Open (Byte_Limit);
      Pool := O.P.Open (Epoch);
      O.Initialize (Targets, Context, Requests, Description, Epoch, Submission,
                    Budget, Allowed_Types);
      Ready := O.Ready (Targets);
   end Prepare_Targets;
   procedure Configure_Targets
     (Width, Height : Interfaces.Unsigned_32;
      Epoch : O.P.Live_ID; Byte_Limit : Natural; Ready : out Boolean) is
      Description : System.Address;
      Requests : Vulkan_Frame.Targets;
      Allowed : Interfaces.Unsigned_32;
   begin
      Ready := False;
      if not Can_Prepare_Targets then return; end if;
      Vulkan_Device_Targets_FFI.Prepare (Width, Height, Description, Requests, Allowed);
      Prepare_Targets (Requests, Description, Epoch, Byte_Limit, Allowed, Ready);
      if Ready and then Width in 1 .. 65535 and then Height in 1 .. 65535 then
         Damage := TD.Open (TD.Extent (Width), TD.Extent (Height));
         Configured := True;
      end if;
   end Configure_Targets;
   function Pipeline_Ready return Boolean is (Pipeline = Live);
   procedure Prepare_Pipeline (Ready : out Boolean) is
      Result : Interfaces.Unsigned_32;
   begin
      Ready := False;
      if Stopping or else D.Current (Device) /= D.Ready or else Pipeline /= Fresh then return; end if;
      Vulkan_Context_Owner.Register_Child (Context, Pipeline_Ticket);
      if Pipeline_Ticket = Vulkan_Context_Owner.No_Child then Pipeline := Closed; return; end if;
      Vulkan_Device_Pipeline_FFI.Create (Result);
      if Result = 0 then Pipeline := Live; Ready := True;
      elsif Result = 1 then
         Pipeline := Closed;
         Vulkan_Context_Owner.Retire_Child (Context, Pipeline_Ticket, True);
      else Pipeline := Quarantined;
      end if;
   end Prepare_Pipeline;
   function Upload_Capacity return Natural is (U.Capacity (Upload));
   function Upload_Phase return U.Phase is (U.Current (Upload));
   procedure Configure_Upload (Size : U.Capacity_Range; Ready : out Boolean) is
      Request : System.Address;
   begin
      Ready := False;
      if Stopping or else D.Current (Device) /= D.Ready or else Pipeline /= Live or else
         not O.Ready (Targets) or else not Configured or else P.Faulted (Pool) or else
         TD.Faulted (Damage) or else V.Current (Submission) /= V.Idle or else
         Writer /= Available or else U.Current (Upload) not in U.Fresh | U.Closed then return; end if;
      Request := Vulkan_Device_Upload_FFI.Prepare;
      if Request = System.Null_Address then return; end if;
      U.Initialize (Upload, Context, Submission, Request, Size, Budget, Ready);
   end Configure_Upload;
   procedure Release_Upload (Released : out Boolean) is
   begin
      Released := False;
      if Writer /= Available or else D.Current (Device) /= D.Ready or else V.Current (Submission) /= V.Idle then return; end if;
      U.Close (Upload, Context, Budget, True, Released);
   end Release_Upload;
   function Backing_Phase (Index : Backing_Slot) return O.I.Phase is
     (S.Current (Backings (Index)));
   function Backing_Lease (Index : Backing_Slot) return O.A.Ticket is
     (S.Lease (Backings (Index)));
   procedure Allocate_Backing (Index : Backing_Slot;
      Width, Height : Interfaces.Unsigned_32; Mask : Boolean;
      Lease : out O.A.Ticket; Result : out Source_Result) is
      Request : System.Address;
      Allowed : Interfaces.Unsigned_32;
      Accepted : Boolean;
   begin
      Lease := O.A.No_Ticket; Result := Source_Rejected;
      if D.Current (Device) = D.Quarantined or else V.Current (Submission) = V.Quarantined or else
         Backing_Phase (Index) = O.I.Quarantined then Result := Source_Unsafe; return; end if;
      if Stopping or else D.Current (Device) /= D.Ready or else Pipeline /= Live or else
         not O.Ready (Targets) or else not Configured or else P.Faulted (Pool) or else
         TD.Faulted (Damage) then return; end if;
      if Writer /= Available or else V.Current (Submission) /= V.Idle then Result := Source_Busy; return; end if;
      if Backing_Phase (Index) not in O.I.Fresh | O.I.Closed or else
         V.Source_Present (Submission, Index) then return; end if;
      if Width not in 1 .. 65535 or else Height not in 1 .. 65535 then return; end if;
      Vulkan_Device_Source_FFI.Prepare (Vulkan_Device_Source_FFI.Slot (Index),
         Width, Height, Mask, Request, Allowed);
      if Request = System.Null_Address then return; end if;
      pragma Assert (CP.Valid (Coverage (Index)));
      S.Initialize (Backings (Index), Context, Submission, Request, Budget, Allowed, Accepted);
      pragma Assert (CP.Valid (Coverage (Index)));
      if Accepted then
         Extents (Index) := (G.Edge (Width), G.Edge (Height), (if Mask then G.R8 else G.BGRA8));
         CP.Begin_Image (Coverage (Index), O.A.Identity (Backing_Lease (Index)),
            G.Edge (Width), G.Edge (Height), Extents (Index).Kind, Accepted);
         if Accepted then Lease := Backing_Lease (Index); Result := Source_Accepted; end if;
      elsif Backing_Phase (Index) = O.I.Quarantined then Result := Source_Unsafe;
      end if;
   end Allocate_Backing;
   procedure Release_Backing (Index : Backing_Slot; Lease : O.A.Ticket;
      Released : out Boolean) is
   begin
      Released := False;
      if Writer /= Available or else D.Current (Device) /= D.Ready or else V.Current (Submission) /= V.Idle or else
         Lease = O.A.No_Ticket or else Lease /= Backing_Lease (Index) or else
         V.Source_Present (Submission, Index) then return; end if;
      S.Close (Backings (Index), Context, Budget, True, Released);
   end Release_Backing;
   function Glyph_Source (Key : GS.Key) return V.Source_Ticket is
     (GS.Resolve (Glyph_Sources, Submission, Key));
   procedure Bind_Glyph (Index : Backing_Slot; Lease : O.A.Ticket;
      Key : GS.Key; Source : V.Source_Ticket; Accepted : out Boolean) is
      Layout : constant GS.L.Layout := GS.L.Plan (Key.Scale);
   begin
      Accepted := False;
      if Stopping or else D.Current (Device) /= D.Ready or else Writer /= Available or else
         V.Current (Submission) /= V.Idle or else Natural (Index) >= GS.Slot'Last or else
         Lease = O.A.No_Ticket or else Lease /= Backing_Lease (Index) or else
         Backing_Phase (Index) /= O.I.Live or else Extents (Index).Kind /= G.R8 or else
         Extents (Index).Width /= Layout.Width or else Extents (Index).Height /= Layout.Height or else
         not CP.Publishable (Coverage (Index), O.A.Identity (Lease)) or else
         not V.Source_Valid (Submission, Source) or else V.Source_At (Submission, Index) /= Source
      then return; end if;
      GS.Bind (Glyph_Sources, Submission, GS.Slot (Natural (Index) + 1), Key, Source, Layout, Accepted);
   end Bind_Glyph;
   procedure Capture_Glyph (Scene : in out Vulkan_Scene.State;
      Key : GS.Key; Cell : Vulkan_Scene.A.G.Logical_Rectangle;
      Tint : Vulkan_Scene.A.Word; Accepted : out Boolean) is
   begin
      Vulkan_Scene.Append_Glyph (Scene, Submission, Glyph_Sources, Key, Cell, Tint, Accepted);
   end Capture_Glyph;
   function Can_Retire_Readers return Boolean is
     (D.Current (Device) = D.Ready and Pipeline = Live and Writer = Available and
      V.Current (Submission) = V.Idle and not P.Faulted (Pool) and not TD.Faulted (Damage));
   function Source_Held (Ticket : V.Source_Ticket) return Boolean is
     (V.Source_Valid (Submission, Ticket));
   function Reader_Held (Reader : Source_Reader) return Boolean is
     (Reader.Serial /= 0 and then Source_Readers (Reader.Index).Serial = Reader.Serial);
   function Source_Pinned (Ticket : V.Source_Ticket) return Boolean is
     (Ticket /= V.No_Source and then
        (for some I in Reader_Index => Source_Readers (I).Serial /= 0 and Source_Readers (I).Source = Ticket));
   procedure Pin_Source (Ticket : V.Source_Ticket; Reader : out Source_Reader) is
   begin
      Reader := No_Source_Reader;
      if Stopping or else not Can_Retire_Readers or else not Source_Held (Ticket) or else
         Reader_Sequence = Interfaces.Unsigned_64'Last then return; end if;
      for I in Reader_Index loop
         if Source_Readers (I).Serial = 0 then
            Reader_Sequence := Reader_Sequence + 1;
            Source_Readers (I) := (Ticket, Reader_Sequence);
            Reader := (I, Reader_Sequence); return;
         end if;
      end loop;
   end Pin_Source;
   procedure Unpin_Source (Reader : Source_Reader; CPU_Retired : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not CPU_Retired or else not Can_Retire_Readers or else not Reader_Held (Reader) then return; end if;
      Source_Readers (Reader.Index) := (others => <>); Accepted := True;
   end Unpin_Source;
   procedure Import_Prepared_Source (Index : V.Source_Slot;
      Description : System.Address; Ticket : out V.Source_Ticket; Result : out Source_Result)
     with Pre => Valid, Post => Valid and (if Ticket /= V.No_Source then Source_Held (Ticket));
   procedure Import_Prepared_Source (Index : V.Source_Slot;
      Description : System.Address; Ticket : out V.Source_Ticket;
      Result : out Source_Result) is
   begin
      Ticket := V.No_Source; Result := Source_Rejected;
      if D.Current (Device) = D.Quarantined or else V.Current (Submission) = V.Quarantined then
         Result := Source_Unsafe; return;
      end if;
      if Stopping or else D.Current (Device) /= D.Ready or else Pipeline /= Live or else
         P.Faulted (Pool) or else TD.Faulted (Damage) then return; end if;
      if Writer /= Available or else V.Current (Submission) /= V.Idle then Result := Source_Busy; return; end if;
      V.Import_Source (Submission, Index, Description, Ticket);
      if Ticket /= V.No_Source then Result := Source_Accepted;
      elsif V.Current (Submission) = V.Quarantined then Result := Source_Unsafe;
      end if;
   end Import_Prepared_Source;
   procedure Import_Source (Index : V.Source_Slot;
      Description : System.Address; Ticket : out V.Source_Ticket; Result : out Source_Result) is
   begin
      Ticket := V.No_Source; Result := Source_Rejected;
      if Backing_Phase (Index) not in O.I.Fresh | O.I.Closed then return; end if;
      Import_Prepared_Source (Index, Description, Ticket, Result);
   end Import_Source;
   procedure Import_Owned_Source (Index : V.Source_Slot;
      Image : System.Address; Ticket : out V.Source_Ticket; Result : out Source_Result) is
      Description : System.Address;
   begin
      Ticket := V.No_Source; Result := Source_Rejected;
      if D.Current (Device) = D.Quarantined or else V.Current (Submission) = V.Quarantined then
         Result := Source_Unsafe; return;
      end if;
      if Stopping or else D.Current (Device) /= D.Ready or else Pipeline /= Live or else
         P.Faulted (Pool) or else TD.Faulted (Damage) then return; end if;
      if Writer /= Available or else V.Current (Submission) /= V.Idle then Result := Source_Busy; return; end if;
      if Backing_Phase (Index) not in O.I.Fresh | O.I.Closed or else
         V.Source_Present (Submission, Index) then return; end if;
      Description := Vulkan_Device_Pipeline_FFI.Source_Request (Interfaces.Unsigned_32 (Index), Image);
      if Description /= System.Null_Address then Import_Source (Index, Description, Ticket, Result); end if;
   end Import_Owned_Source;
   procedure Release_Source (Ticket : V.Source_Ticket; Released : out System.Address) is
   begin
      Released := System.Null_Address;
      if Writer /= Available or else D.Current (Device) /= D.Ready or else Pipeline /= Live then return; end if;
      if Source_Pinned (Ticket) then return; end if;
      V.Release_Source (Submission, Ticket, Released);
   end Release_Source;
   function Write_Active (Ticket : Write_Ticket) return Boolean is
     (Writer = Producing and Ticket /= No_Write and Ticket = Active_Write and
      CP.Can_Write (Coverage (Ticket.Index), Ticket.Chunk));
   function Can_Produce (Index : Backing_Slot; Lease : O.A.Ticket) return Boolean is
     (not Stopping and D.Current (Device) = D.Ready and Pipeline = Live and
      O.Ready (Targets) and Configured and not P.Faulted (Pool) and not TD.Faulted (Damage) and
      Writer = Available and V.Current (Submission) = V.Idle and
      Lease /= O.A.No_Ticket and Lease = Backing_Lease (Index) and Backing_Phase (Index) = O.I.Live and
      not V.Source_Present (Submission, Index));
   procedure Restart_Content (Index : Backing_Slot; Lease : O.A.Ticket; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Can_Produce (Index, Lease) then return; end if;
      CP.Begin_Image (Coverage (Index), O.A.Identity (Lease), Extents (Index).Width,
         Extents (Index).Height, Extents (Index).Kind, Accepted);
   end Restart_Content;
   procedure Begin_Write (Index : Backing_Slot; Lease : O.A.Ticket;
      Ticket : out Write_Ticket; Plan : out G.Plan; Mapping : out System.Address;
      Result : out Source_Result; Row_Pixels : Compositor_Upload.Edge := 0) is
      Chunk : CP.Ticket;
      Discard, Accepted : Boolean;
      Empty : G.Plan;
   begin
      Ticket := No_Write; Plan := Empty; Mapping := System.Null_Address; Result := Source_Rejected;
      if Writer = Unsafe or else D.Current (Device) = D.Quarantined or else V.Current (Submission) = V.Quarantined then
         Result := Source_Unsafe; return;
      end if;
      if Writer /= Available or else V.Current (Submission) /= V.Idle then Result := Source_Busy; return; end if;
      if not Can_Produce (Index, Lease) then return; end if;
      if U.Current (Upload) /= U.Live or else U.Capacity (Upload) not in 1 .. G.Byte_Count'Last or else
         U.Mapping (Upload) = System.Null_Address then return; end if;
      CP.Begin_Write (Coverage (Index), G.Byte_Count (U.Capacity (Upload)), Plan, Chunk, Discard, Accepted, Row_Pixels);
      if not Accepted then return; end if;
      Ticket := (Index, Chunk); Active_Write := Ticket; Write_Plan := Plan; Write_Discard := Discard;
      Writer := Producing; Mapping := U.Mapping (Upload); Result := Source_Accepted;
   end Begin_Write;
   procedure Cancel_Write (Ticket : Write_Ticket; Producer_Retired : Boolean; Cancelled : out Boolean) is
   begin
      Cancelled := False;
      if Writer /= Producing or else Ticket /= Active_Write or else Ticket = No_Write then return; end if;
      CP.Cancel (Coverage (Ticket.Index), Ticket.Chunk, Producer_Retired);
      Writer := (if Producer_Retired then Available else Unsafe);
      Cancelled := Producer_Retired;
   end Cancel_Write;
   procedure Submit_Write (Ticket : Write_Ticket; Producer_Retired : Boolean; Result : out Source_Result) is
      Accepted, Cancelled : Boolean;
   begin
      Result := Source_Rejected;
      if Writer /= Producing or else Ticket /= Active_Write or else Ticket = No_Write or else
         not CP.Can_Write (Coverage (Ticket.Index), Ticket.Chunk) then return; end if;
      if not Producer_Retired or else D.Current (Device) /= D.Ready or else
         V.Current (Submission) /= V.Idle then
         CP.Cancel (Coverage (Ticket.Index), Ticket.Chunk, False); Writer := Unsafe; Result := Source_Unsafe; return;
      end if;
      -- A stopped producer may cancel but cannot enqueue new GPU work at shutdown.
      if Stopping then Cancel_Write (Ticket, True, Cancelled); return; end if;
      V.Begin_Record (Submission, Accepted);
      if Accepted then
         Vulkan_Upload_Recording.Record_Transfer (Submission, Context, Upload,
            Backings (Ticket.Index), Ticket.Index, Write_Plan, Write_Discard, Accepted);
         if not Accepted then
            V.Cancel (Submission, Cancelled);
            CP.Cancel (Coverage (Ticket.Index), Ticket.Chunk, Cancelled);
            Writer := (if Cancelled then Available else Unsafe);
            Result := (if Cancelled then Source_Rejected else Source_Unsafe); return;
         end if;
         V.Seal_Transfer (Submission, Accepted);
         if Accepted then V.Submit (Submission, Accepted); end if;
      end if;
      if not Accepted then
         CP.Cancel (Coverage (Ticket.Index), Ticket.Chunk, False); Writer := Unsafe; Result := Source_Unsafe; return;
      end if;
      CP.Submitted (Coverage (Ticket.Index), Ticket.Chunk, Accepted);
      Writer := (if Accepted then Transferring else Unsafe);
      Result := (if Accepted then Source_Accepted else Source_Unsafe);
   end Submit_Write;
   procedure Poll_Upload (Result : out Poll_Result) is
      Observation : V.Observation;
   begin
      Result := Idle;
      if Writer = Unsafe then Result := GPU_Failed; return; end if;
      if Writer /= Transferring then return; end if;
      if D.Current (Device) /= D.Ready or else V.Current (Submission) /= V.Pending then
         Writer := Unsafe; Result := GPU_Failed; return;
      end if;
      V.Poll (Submission, Observation);
      CP.Observe (Coverage (Active_Write.Index), Active_Write.Chunk,
         (case Observation is when V.Still_Pending => CP.Still_Pending,
            when V.Finished => CP.Completed, when V.Uncertain => CP.Uncertain));
      case Observation is
         when V.Still_Pending => Result := Pending;
         when V.Finished => Writer := Available; Result := Completed;
         when V.Uncertain => Writer := Unsafe; Result := GPU_Failed;
      end case;
   end Poll_Upload;
   procedure Import_Backing (Index : Backing_Slot; Lease : O.A.Ticket;
      Ticket : out V.Source_Ticket; Result : out Source_Result) is
      Description : System.Address;
   begin
      Ticket := V.No_Source; Result := Source_Rejected;
      if not Can_Produce (Index, Lease) or else
         not CP.Publishable (Coverage (Index), O.A.Identity (Lease)) then return; end if;
      Description := Vulkan_Device_Pipeline_FFI.Source_Request
         (Interfaces.Unsigned_32 (Index), S.Description (Backings (Index)));
      if Description /= System.Null_Address then
         Import_Prepared_Source (Index, Description, Ticket, Result);
      end if;
   end Import_Backing;
   function Upload_Pending return Boolean is (Writer = Transferring);
   function Frame_Pending return Boolean is (V.Current (Submission) = V.Pending and Writer /= Transferring);
   procedure Damage_Output (Region : Compositor_Damage.Box; Accepted : out Boolean) is
   begin
      Accepted := Configured and then not TD.Faulted (Damage) and then
        Compositor_Damage.Contains (TD.Bounds (Damage), Region);
      if Accepted then TD.Change (Damage, Region); end if;
   end Damage_Output;
   function Admit_Capture (Screen : Vulkan_Scene.A.G.Output) return Capture_Admission is
      use type O.Phase;
   begin
      if D.Current (Device) = D.Quarantined or else Pipeline = Quarantined or else
         Writer = Unsafe or else V.Current (Submission) = V.Quarantined or else
         O.Current (Targets) = O.Quarantined or else P.Faulted (Pool) or else TD.Faulted (Damage)
      then return Capture_Uncertain; end if;
      if Stopping or else D.Current (Device) /= D.Ready or else Pipeline /= Live or else
         not Configured or else not O.Ready (Targets) or else
         Natural (Screen.Width) /= TD.Bounds (Damage).Right or else
         Natural (Screen.Height) /= TD.Bounds (Damage).Bottom
      then return Capture_Unavailable; end if;
      if Writer /= Available or else V.Current (Submission) /= V.Idle or else
         Frame_Pending or else TD.Active (Damage) /= 0 or else P.Writer (Pool) /= P.None or else
         (not P.Has_Free (Pool) and then P.Ready (Pool) = P.None)
      then return Capture_Busy; end if;
      return Capture_Allowed;
   end Admit_Capture;
   procedure Render (Scene : Vulkan_Scene.State; Result : out Frame_Result) is
      Admission : F.Admission;
      Recorded : Vulkan_Scene_Recording.Outcome;
      OK : Boolean;
   begin
      Result := Rejected;
      if Stopping or else D.Current (Device) /= D.Ready or else not O.Ready (Targets) or else
         not Configured or else P.Faulted (Pool) or else TD.Faulted (Damage)
      then return; end if;
      if Writer /= Available or else Frame_Pending then Result := Deferred; return; end if;
      if V.Current (Submission) /= V.Idle or else TD.Active (Damage) /= 0 or else
         Vulkan_Scene.Current (Scene) /= Vulkan_Scene.Sealed
      then return; end if;
      F.Begin_Record (Submission, Pool, Damage, Admission, Replace_Ready => True);
      if Admission = F.Deferred then Result := Deferred; return;
      elsif Admission = F.Failed then Result := Failed; return;
      end if;
      Vulkan_Scene_Recording.Record_Scene (Scene, Targets, Submission, Pool, Damage, Recorded);
      if Recorded = Vulkan_Scene_Recording.Cancelled then return;
      elsif Recorded = Vulkan_Scene_Recording.Quarantined then Result := Failed; return;
      end if;
      F.Submit (Submission, Pool, OK);
      if OK then Result := Submitted;
      else TD.Finish (Damage, TD.Unknown); Result := Failed;
      end if;
   end Render;
   procedure Poll_Frame (Result : out Poll_Result) is
      Completion : F.Completion;
   begin
      Result := Idle;
      if D.Current (Device) /= D.Ready or else P.Faulted (Pool) or else
         TD.Faulted (Damage) or else V.Current (Submission) = V.Quarantined
      then Result := GPU_Failed; return; end if;
      if not Frame_Pending then return; end if;
      if not P.Rendering (Pool) or else TD.Active (Damage) = 0 or else
         TD.Active (Damage) /= P.Writer (Pool).Buffer
      then Result := GPU_Failed; return; end if;
      F.Poll (Submission, Pool, Damage, Completion);
      Result := (case Completion is when F.Still_Pending => Pending,
        when F.Ready => Completed, when F.Uncertain => GPU_Failed);
   end Poll_Frame;
   function Presentation_Pending return Presentation_Ticket is (P.Displayed (Pool));
   function Presentation_Front return Presentation_Ticket is (P.Front (Pool));
   function Presentation_Faulted return Boolean is (P.Faulted (Pool));
   procedure Take_Presentation (Ticket : out Presentation_Ticket) is
   begin
      Ticket := No_Presentation;
      if Stopping or else D.Current (Device) /= D.Ready or else not O.Ready (Targets) then return; end if;
      P.Present (Pool, Ticket);
   end Take_Presentation;
   procedure Confirm_Presentation
     (Ticket, Previous : Presentation_Ticket; Confirmed : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if D.Current (Device) /= D.Ready or else not O.Ready (Targets) then return; end if;
      P.Latch_Display (Pool, Ticket, Previous, Confirmed);
      Accepted := not P.Faulted (Pool) and P.Front (Pool) = Ticket and P.Displayed (Pool) = P.None;
   end Confirm_Presentation;
   procedure Cancel_Presentation
     (Ticket : Presentation_Ticket; Quiescent : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if D.Current (Device) /= D.Ready or else not O.Ready (Targets) then return; end if;
      P.Retire_Display (Pool, Ticket, Quiescent);
      Accepted := not P.Faulted (Pool) and P.Displayed (Pool) = P.None;
   end Cancel_Presentation;
   procedure Retire_Presentation
     (Ticket : Presentation_Ticket; Confirmed : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if D.Current (Device) /= D.Ready or else not O.Ready (Targets) then return; end if;
      P.Retire_Front (Pool, Ticket, Confirmed);
      Accepted := not P.Faulted (Pool) and P.Front (Pool) = P.None;
   end Retire_Presentation;
   procedure Stop is
      Released : Boolean;
      Pipeline_Result : Interfaces.Unsigned_32;
   begin
      -- A failed health observation forbids additional child FFI operations.
      -- The device/context retain uncertain children and accounting forever.
      if D.Current (Device) = D.Ready then
         Stopping := True;
         if Writer /= Available or else
           (for some I in Reader_Index => Source_Readers (I).Serial /= 0) then return; end if;
         if not P.Faulted (Pool) and then P.Ready (Pool) /= P.None then
            P.Discard_Ready (Pool, P.Ready (Pool));
         end if;
         for Index in Backing_Slot loop
            pragma Loop_Invariant (Valid);
            pragma Loop_Invariant (Configured_Limit = O.A.Limit (Budget'Loop_Entry));
            Release_Backing (Index, Backing_Lease (Index), Released);
         end loop;
         Release_Upload (Released);
         O.Close (Targets, Context, Submission, Pool, Budget, Released);
         if Pipeline = Live and then V.Can_Destroy (Submission) then
            Vulkan_Device_Pipeline_FFI.Close (Pipeline_Result);
            if Pipeline_Result = 0 then
               Pipeline := Closed;
               Vulkan_Context_Owner.Retire_Child (Context, Pipeline_Ticket, True);
            else Pipeline := Quarantined;
            end if;
         end if;
      end if;
      D.Close (Device, Context, Submission);
   end Stop;
end Desktop_Vulkan_Startup;
