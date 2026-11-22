with Compositor_Upload_Progress;
with Compositor_Upload;
with Vulkan_Upload_Recording;
with Vulkan_Upload_Owner;
with Vulkan_Device_Source_FFI;
with Vulkan_Owned_Source;
with Vulkan_Device_Pipeline_FFI;
with Vulkan_Device_Targets_FFI;
with Vulkan_Owned_Targets; with Vulkan_Frame;
with Vulkan_Scene; with Compositor_Target_Damage; with Compositor_Affine;
with Vulkan_Scene_Recording;
package body Vulkan_Target_Bundle_Bridge is
   package O renames Vulkan_Owned_Targets;
   package P renames O.P;
   package CO renames O.C;
   Context : CO.State;
   Device_Description : System.Address := System.Null_Address;
   Device_Requests : Vulkan_Frame.Targets := (others => System.Null_Address);
   use type System.Address;
   use type O.Phase, Interfaces.C.int, P.Ticket;
   Partial, Settling, Textured, Source_Mask : Boolean := False;
   Source_Image : Vulkan_Owned_Source.State;
   Upload : Vulkan_Upload_Owner.State;
   package Progress is new Compositor_Upload_Progress;
   Content : Progress.State;
   Write_Plan : Compositor_Upload.Plan;
   Write_Ticket : Progress.Ticket;
   Write_Discard : Boolean := False;
   use type Progress.Phase;

   Source_Ticket : O.V.Source_Ticket;
   use type O.I.Phase, O.V.Source_Ticket;
   Owner : O.State;
   Budget : O.A.State := O.A.Open (16 * 1024 * 1024);
   Pool : P.State := P.Open (1);
   Session : O.V.State;
   Damage : Compositor_Target_Damage.State := Compositor_Target_Damage.Open (32, 24);
   function Fill (Rotation, N, D, L, T, R, B : Interfaces.C.int) return Interfaces.C.int is
      package G renames Compositor_Affine.G;
      use type G.Logical_Coordinate;
      Screen : constant G.Output := (32, 24, G.Orientation'Val (Rotation), (G.Scale_Component (N), G.Scale_Component (D)), -3, 7);
      Scene : Vulkan_Scene.State := Vulkan_Scene.Open (Screen, 16#204060#);
      Accepted : Boolean;
      Admission : Vulkan_Frame.Admission;
      Recorded : Vulkan_Scene_Recording.Outcome;
      use type Vulkan_Frame.Admission, Vulkan_Scene_Recording.Outcome;
   begin
      if Textured then
         Vulkan_Scene.Append (Scene,
           (Source_Ticket, (-3, 7, 29, 31), False, Source_Mask, 16#FFFF0000#, Vulkan_Scene.Textured), Accepted);
         if not Accepted then return 0; end if;
      end if;
      Vulkan_Scene.Append_Physical_Fill (Scene,
        (G.Pixel_Edge (L), G.Pixel_Edge (T), G.Pixel_Edge (R), G.Pixel_Edge (B)), 16#00A0E0#, Accepted);
      if not Accepted then return 0; end if;
      Vulkan_Scene.Seal (Scene, Accepted); if not Accepted then return 0; end if;
      if not Settling then
         Compositor_Target_Damage.Change (Damage, (if Partial then (1, 1, 4, 3) else (0, 0, 32, 24)));
      end if;
      Vulkan_Frame.Begin_Record (Session, Pool, Damage, Admission);
      if Admission /= Vulkan_Frame.Started then return 0; end if;
      if Partial then
         pragma Assert (Compositor_Target_Damage.Initialized (Damage, P.Writer (Pool).Buffer));
         pragma Assert (not Compositor_Target_Damage.D.Covers
            (Compositor_Target_Damage.Painting (Damage), Compositor_Target_Damage.Bounds (Damage)));
      end if;
      Vulkan_Scene_Recording.Record_Scene (Scene, Owner, Session, Pool, Damage, Recorded);
      if Recorded /= Vulkan_Scene_Recording.Recorded then return 0; end if;
      pragma Assert (not O.V.Quiescent (Session) and P.Ready (Pool) /= P.Writer (Pool));
      Vulkan_Frame.Submit (Session, Pool, Accepted);
      return (if Accepted then Interfaces.C.int (P.Writer (Pool).Buffer) else 0);
   end Fill;
   function Upload_Open (Request : System.Address; Size : Interfaces.Unsigned_32) return Interfaces.C.int is
      Accepted : Boolean;
   begin
      Vulkan_Upload_Owner.Initialize (Upload, Context, Session, Request,
         Vulkan_Upload_Owner.Capacity_Range (Size), Budget, Accepted);
      return (if Accepted then 0 else 1);
   end Upload_Open;
   function Upload_Begin return Interfaces.C.int is
      Plan : Compositor_Upload.Plan;
      Ticket : Progress.Ticket;
      Discard, Accepted : Boolean;
   begin
      if not O.V.Quiescent (Session) or else O.V.Source_Present (Session, 0) then return 0; end if;
      Progress.Begin_Write (Content, Compositor_Upload.Byte_Count (Vulkan_Upload_Owner.Capacity (Upload)),
         Plan, Ticket, Discard, Accepted);
      if not Accepted then return 0; end if;
      Write_Plan := Plan; Write_Ticket := Ticket; Write_Discard := Discard;
      return Interfaces.C.int (Compositor_Upload.Area (Plan).Height);
   end Upload_Begin;
   function Upload_First_Row return Interfaces.Unsigned_32 is
     (Interfaces.Unsigned_32 (Compositor_Upload.Area (Write_Plan).Y));
   function Upload_Submit return Interfaces.C.int is
      Accepted, Cancelled : Boolean;
   begin
      if not O.V.Quiescent (Session) or else not Progress.Can_Write (Content, Write_Ticket) then return 1; end if;
      O.V.Begin_Record (Session, Accepted);
      if not Accepted then Progress.Cancel (Content, Write_Ticket, False); return 2; end if;
      Vulkan_Upload_Recording.Record_Transfer (Session, Context, Upload, Source_Image, 0, Write_Plan, Write_Discard, Accepted);
      if not Accepted then
         O.V.Cancel (Session, Cancelled); Progress.Cancel (Content, Write_Ticket, Cancelled); return 3;
      end if;
      O.V.Seal_Transfer (Session, Accepted);
      if not Accepted then Progress.Cancel (Content, Write_Ticket, False); return 4; end if;
      O.V.Submit (Session, Accepted);
      if not Accepted then Progress.Cancel (Content, Write_Ticket, False); return 5; end if;
      Progress.Submitted (Content, Write_Ticket, Accepted);
      return (if Accepted then 0 else 6);
   end Upload_Submit;
   function Upload_Finish return Interfaces.C.int is
      Result : O.V.Observation;
      use type O.V.Phase, O.V.Observation;
   begin
      if O.V.Current (Session) /= O.V.Pending then return 1; end if;
      O.V.Poll (Session, Result);
      Progress.Observe (Content, Write_Ticket,
         (case Result is when O.V.Still_Pending => Progress.Still_Pending,
            when O.V.Finished => Progress.Completed, when O.V.Uncertain => Progress.Uncertain));
      return (if Result = O.V.Finished then 0 else 2);
   end Upload_Finish;
   function Upload_Mapping return System.Address is
     (if O.V.Quiescent (Session) and then Progress.Can_Write (Content, Write_Ticket)
      then Vulkan_Upload_Owner.Mapping (Upload) else System.Null_Address);
   function Upload_Close return Interfaces.C.int is
      Released : Boolean;
   begin
      Vulkan_Upload_Owner.Close (Upload, Context, Budget,
         O.V.Quiescent (Session) and then Progress.Current (Content) not in Progress.Writing | Progress.Pending | Progress.Quarantined,
         Released);
      return (if Released then 0 else 1);
   end Upload_Close;
   function Source_Configure (Width, Height, Mask : Interfaces.Unsigned_32) return Interfaces.C.int is
      Request : System.Address;
      Allowed : Interfaces.Unsigned_32;
      Accepted : Boolean;
      use type Interfaces.Unsigned_32;
   begin
      if Vulkan_Owned_Source.Current (Source_Image) not in O.I.Fresh | O.I.Closed or else
         Mask > 1 then return 1; end if;
      Vulkan_Device_Source_FFI.Prepare (0, Width, Height, Mask = 1, Request, Allowed);
      if Request = System.Null_Address then return 1; end if;
      Vulkan_Owned_Source.Initialize (Source_Image, Context, Session, Request,
         Budget, Allowed, Accepted);
      if Accepted then
         Source_Mask := Mask = 1;
         Progress.Begin_Image (Content, O.A.Identity (Vulkan_Owned_Source.Lease (Source_Image)),
            Compositor_Upload.Edge (Width), Compositor_Upload.Edge (Height),
            (if Source_Mask then Compositor_Upload.R8 else Compositor_Upload.BGRA8), Accepted);
      end if;
      return (if Accepted then 0 else 2);
   end Source_Configure;
   function Source_Image_Request return System.Address is
     (Vulkan_Owned_Source.Description (Source_Image));
   function Source_Restart (Width, Height, Mask : Interfaces.Unsigned_32) return Interfaces.C.int is
      Accepted : Boolean;
      use type Interfaces.Unsigned_32;
   begin
      if not O.V.Quiescent (Session) or else O.V.Source_Present (Session, 0) or else
         Vulkan_Owned_Source.Current (Source_Image) /= O.I.Live or else Mask > 1 then return 1; end if;
      Progress.Begin_Image (Content, O.A.Identity (Vulkan_Owned_Source.Lease (Source_Image)),
         Compositor_Upload.Edge (Width), Compositor_Upload.Edge (Height),
         (if Mask = 1 then Compositor_Upload.R8 else Compositor_Upload.BGRA8), Accepted);
      return (if Accepted then 0 else 2);
   end Source_Restart;
   function Source_Detach return Interfaces.C.int is
      Released : System.Address;
   begin
      O.V.Release_Source (Session, Source_Ticket, Released);
      return (if Released /= System.Null_Address then 0 else 1);
   end Source_Detach;
   function Source_Import (Request : System.Address) return Interfaces.C.int is
      Description : System.Address;
   begin
      if Request = System.Null_Address or else Request /= Vulkan_Owned_Source.Description (Source_Image) or else
         not Progress.Publishable (Content, O.A.Identity (Vulkan_Owned_Source.Lease (Source_Image))) then return 1; end if;
      Description := Vulkan_Device_Pipeline_FFI.Source_Request (0, Request);
      if Description = System.Null_Address then return 1; end if;
      O.V.Import_Source (Session, 0, Description, Source_Ticket);
      return (if Source_Ticket /= O.V.No_Source then 0 else 2);
   end Source_Import;
   function Source_Release return Interfaces.C.int is
      Released : System.Address;
      Closed : Boolean;
   begin
      O.V.Release_Source (Session, Source_Ticket, Released);
      if Released = System.Null_Address then return 1; end if;
      Vulkan_Owned_Source.Close (Source_Image, Context, Budget, True, Closed);
      return (if Closed then 0 else 2);
   end Source_Release;
   function Textured_Fill return Interfaces.C.int is
      Result : Interfaces.C.int;
   begin
      Textured := True; Result := Fill (0, 1, 1, 1, 1, 4, 3); Textured := False;
      return Result;
   end Textured_Fill;
   function Settle_Fill return Interfaces.C.int is
      Result : Interfaces.C.int;
   begin
      Settling := True; Result := Fill (0, 1, 1, 32, 24, 32, 24); Settling := False;
      return Result;
   end Settle_Fill;
   function Partial_Fill return Interfaces.C.int is
      Result : Interfaces.C.int;
   begin
      Partial := True; Result := Fill (0, 1, 1, 1, 1, 4, 3); Partial := False;
      return Result;
   end Partial_Fill;
   function Cancel_First return Interfaces.C.int is
      Admission : Vulkan_Frame.Admission;
      OK : Boolean;
      use type Vulkan_Frame.Admission;
      Slot : P.Live_Slot;
   begin
      Vulkan_Frame.Begin_Record (Session, Pool, Damage, Admission);
      if Admission /= Vulkan_Frame.Started then return 1; end if;
      Slot := P.Writer (Pool).Buffer;
      pragma Assert (not Compositor_Target_Damage.Initialized (Damage, Slot));
      O.Prepare_Frame (Owner, Session, Pool, Damage, OK);
      if not OK then return 2; end if;
      Vulkan_Frame.Cancel (Session, Pool, OK);
      if not OK then return 3; end if;
      Compositor_Target_Damage.Finish (Damage, Compositor_Target_Damage.Cancelled);
      pragma Assert (not Compositor_Target_Damage.Initialized (Damage, Slot));
      return 0;
   end Cancel_First;
   function Finish return Interfaces.C.int is
      Observation : Vulkan_Frame.Completion;
      use type Vulkan_Frame.Completion;
   begin
      Vulkan_Frame.Poll (Session, Pool, Damage, Observation);
      return (if Observation = Vulkan_Frame.Ready then 0 else 1);
   end Finish;
   function Context_Open (Description : System.Address) return System.Address is
      OK : Boolean;
   begin
      CO.Initialize (Context, Description, OK);
      return (if OK then CO.Context (Context) else System.Null_Address);
   end Context_Open;
   function Context_Close return Interfaces.C.int is
      Released : Boolean;
   begin
      CO.Close (Context, Session, Released);
      return (if Released then 0 else 2);
   end Context_Close;
   function Open (Description, A, B, C, Submission : System.Address; Allowed : Interfaces.Unsigned_32) return Interfaces.C.int is
   begin
      if Submission /= CO.Context (Context) then return 1; end if;
      Session := O.V.Open (Submission);
      O.Initialize (Owner, Context, Vulkan_Frame.Targets'(A, B, C), Description, 1, Session, Budget, Allowed);
      pragma Assert (O.Parent_Held (Owner, Context));
      return (if O.Ready (Owner) then 0 else 1);
   end Open;
   function Open_Device_Targets (Width, Height : Interfaces.Unsigned_32) return Interfaces.C.int is
      Allowed : Interfaces.Unsigned_32;
   begin
      Vulkan_Device_Targets_FFI.Prepare (Width, Height, Device_Description, Device_Requests, Allowed);
      return Open (Device_Description, Device_Requests (1), Device_Requests (2),
                   Device_Requests (3), CO.Context (Context), Allowed);
   end Open_Device_Targets;
   function Device_Request (Index : Interfaces.C.int) return System.Address is
   begin
      if Index = 0 then return Device_Description;
      elsif Index in 1 .. 3 then return Device_Requests (P.Live_Slot (Index));
      else return System.Null_Address;
      end if;
   end Device_Request;
   function Close (Hold : Interfaces.C.int) return Interfaces.C.int is
      Ticket, Previous : P.Ticket; Released : Boolean;
      -- The C oracle has already waited the actual queue fence. This test
      -- supplies that GPU fact; display retirement is explicitly simulated.
   begin
      if Hold /= 0 then
         P.Acquire (Pool, Ticket); P.Start_Render (Pool, Ticket);
         P.Finish_Render (Pool, Ticket, P.Completed); P.Present (Pool, Ticket);
         Previous := P.Front (Pool); P.Latch_Display (Pool, Ticket, Previous, True);
      elsif P.Front (Pool) /= P.None then P.Retire_Front (Pool, P.Front (Pool), True);
      end if;
      O.Close (Owner, Context, Session, Pool, Budget, Released);
      if Released then pragma Assert (O.A.Charged (Budget) = 0); return 0; end if;
      pragma Assert (O.A.Charged (Budget) > 0);return 2;
   end Close;
end Vulkan_Target_Bundle_Bridge;
