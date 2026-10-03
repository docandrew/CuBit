with Compositor_Image_Sampling;
with Vulkan_Glyph_Sources;
with Vulkan_Submission;
with Vulkan_Scene;
with Vulkan_Frame;
with Compositor_Target_Damage;
with Compositor_Damage;
with Vulkan_Target_Owner;
with Compositor_Pool;
with Vulkan_Affine_Binding;
package body Vulkan_Submission_Test_Bridge is
   package B renames Vulkan_Affine_Binding;
   package G renames B.G;
   package V renames Vulkan_Submission;
   package P renames Compositor_Pool;
   package F renames Vulkan_Frame;
   package TD renames Compositor_Target_Damage;
   package CD renames Compositor_Damage;
   Damage : TD.State;
   Captured : Vulkan_Scene.State;
   package GS renames Vulkan_Glyph_Sources;
   Glyph_Sources : GS.State;
   package O renames Vulkan_Target_Owner;
   use type F.Targets;
   Target_Owner : O.State;
   use type F.Admission, F.Completion;
   use type System.Address, V.Source_Ticket, V.Phase, V.Observation, P.Ticket, B.Outcome, Interfaces.C.int;
   Session : V.State;
   Pool : P.State;
   Ticket : P.Ticket;
   Active : Boolean := False;
   Direct : Boolean := False;
   Bindings : F.Targets;
   Latches, Replacements : Natural := 0;
   procedure Damage_Region (Left, Top, Right, Bottom : Interfaces.C.int) is
   begin TD.Change (Damage, (Natural (Left), Natural (Top), Natural (Right), Natural (Bottom))); end Damage_Region;
   function Repaint_Count return Interfaces.C.int is (Interfaces.C.int (CD.Count (TD.Painting (Damage))));
   procedure Repaint_Box (Index : Interfaces.C.int; Left, Top, Right, Bottom : out Interfaces.C.int) is
      R : constant CD.Box := CD.Item (TD.Painting (Damage), CD.Index (Index));
   begin
      Left := Interfaces.C.int (R.Left); Top := Interfaces.C.int (R.Top);
      Right := Interfaces.C.int (R.Right); Bottom := Interfaces.C.int (R.Bottom);
   end Repaint_Box;
   function Initialize_Targets (Description : System.Address) return Interfaces.C.int is
      Accepted : Boolean;
   begin
      O.Initialize (Target_Owner, Description, 1, Accepted);
      return (if Accepted then 0 else 2);
   end Initialize_Targets;
   function Close_Targets return Interfaces.C.int is
      Released : Boolean;
   begin
      O.Close (Target_Owner, Session, Pool, Released);
      return (if Released then 0 else 2);
   end Close_Targets;
   procedure Set_Targets (A, B, C : System.Address) is
   begin
      pragma Assert (V.Quiescent (Session) and P.Front (Pool) = P.None and P.Displayed (Pool) = P.None);
      pragma Assert (A /= B and A /= C and B /= C and A /= System.Null_Address and B /= System.Null_Address and C /= System.Null_Address);
      Bindings := O.Bindings (Target_Owner);
      pragma Assert (Bindings = F.Targets'(A, B, C)); Direct := True;
   end Set_Targets;
   function Target_Index return Interfaces.C.int is (Interfaces.C.int (P.Writer (Pool).Buffer));
   procedure Display_Tick (Latch_Now : Interfaces.C.int) is
      Offered, Previous : P.Ticket;
   begin
      pragma Assert (Direct and V.Quiescent (Session));
      P.Present (Pool, Offered);
      if Latch_Now /= 0 and P.Displayed (Pool) /= P.None then
         Offered := P.Displayed (Pool); Previous := P.Front (Pool);
         -- Hosted model evidence only; no physical scanout exists here.
         P.Latch_Display (Pool, Offered, Previous, True); Latches := Latches + 1;
         P.Present (Pool, Offered);
      end if;
      pragma Assert (P.Valid (Pool) and not P.Faulted (Pool));
   end Display_Tick;
   procedure Finish_Display is
   begin
      if P.Ready (Pool) /= P.None then P.Discard_Ready (Pool, P.Ready (Pool)); end if;
      Display_Tick (1);
      if P.Front (Pool) /= P.None then P.Retire_Front (Pool, P.Front (Pool), True); end if;
      pragma Assert (not P.Faulted (Pool) and P.Front (Pool) = P.None and P.Displayed (Pool) = P.None and P.Ready (Pool) = P.None);
      pragma Assert (Latches > 50 and Replacements > 100);
      Direct := False;
   end Finish_Display;
   Source : V.Source_Ticket := V.No_Source;
   Source_Context : System.Address := System.Null_Address;
   Managed : Boolean := False;
   Mask_Source : V.Source_Ticket := V.No_Source;
   Mask_Context : System.Address := System.Null_Address;
   function Import_Mask (Description, Expected_Context : System.Address) return Interfaces.C.int is
      Registered : V.Source_Ticket;
   begin
      V.Import_Source (Session, 1, Description, Registered);
      if Registered = V.No_Source then return 2; end if;
      Mask_Source := Registered; Mask_Context := Expected_Context;
      return 0;
   end Import_Mask;
   function Release_Mask return System.Address is
      Released : System.Address;
   begin
      V.Release_Source (Session, Mask_Source, Released);
      if Released /= System.Null_Address then
         pragma Assert (Released = Mask_Context and not V.Source_Valid (Session, Mask_Source));
         Mask_Source := V.No_Source; Mask_Context := System.Null_Address;
      end if;
      return Released;
   end Release_Mask;
   function Import_Source (Description, Expected_Context : System.Address) return Interfaces.C.int is
      Registered : V.Source_Ticket;
   begin
      V.Import_Source (Session, 0, Description, Registered);
      if Registered = V.No_Source then return 2; end if;
      Source := Registered; Source_Context := Expected_Context; Managed := True;
      return 0;
   end Import_Source;
   function Register_Source (Context : System.Address) return Interfaces.C.int is
      Registered : V.Source_Ticket;
   begin
      V.Install_Source (Session, 0, Context, Registered);
      if Registered = V.No_Source then return 2; end if;
      Source := Registered; Source_Context := Context; Managed := False;
      return 0;
   end Register_Source;
   function Release_Source return System.Address is
      Released : System.Address;
   begin
      if Managed then V.Release_Source (Session, Source, Released);
      else V.Remove_Source (Session, Source, Released); end if;
      if Released /= System.Null_Address then
         pragma Assert (Released = Source_Context and not V.Source_Valid (Session, Source));
         Source := V.No_Source; Source_Context := System.Null_Address;
      end if;
      return Released;
   end Release_Source;
   procedure Check_Retained is
      Released : System.Address;
   begin
      if Source /= V.No_Source then
         Released := Release_Source;
         pragma Assert (Released = System.Null_Address and V.Source_Valid (Session, Source));
      end if;
      if Mask_Source /= V.No_Source then
         Released := Release_Mask;
         pragma Assert (Released = System.Null_Address and V.Source_Valid (Session, Mask_Source));
      end if;
   end Check_Retained;
   procedure Open (Context : System.Address) is
   begin
      Session := V.Open (Context); Pool := P.Open (1); Damage := TD.Open (32, 24);
   end Open;
   function Releasable return Interfaces.C.int is
     (if V.Quiescent (Session) and not P.Rendering (Pool) then 1 else 0);
   function Start return Interfaces.C.int is
      Outcome : F.Admission;
      OK : Boolean;
   begin
      pragma Assert (V.Current (Session) = V.Idle);
      if Direct and not P.Has_Free (Pool) and P.Ready (Pool) /= P.None then Replacements := Replacements + 1; end if;
      F.Begin_Record (Session, Pool, Damage, Outcome, Replace_Ready => Direct);
      OK := Outcome = F.Started; Ticket := P.Writer (Pool);
      pragma Assert (Ticket /= P.None);
      Active := OK;
      pragma Assert (P.Rendering (Pool) and not V.Quiescent (Session));
      return (if OK then 0 else 2);
   end Start;
   function Begin_Scene (Pass : System.Address; Width, Height : Interfaces.Unsigned_32)
     return Interfaces.C.int is
      OK : Boolean;
   begin
      if Direct then
         pragma Assert (Pass = Bindings (P.Writer (Pool).Buffer));
         F.Begin_Scene (Session, Pool, Bindings, G.Physical_Extent (Width), G.Physical_Extent (Height), OK);
      else
         F.Begin_Scene (Session, Pool, (others => Pass), G.Physical_Extent (Width), G.Physical_Extent (Height), OK);
      end if;
      pragma Assert (not V.Quiescent (Session) and P.Rendering (Pool));
      return (if OK then 0 else 2);
   end Begin_Scene;
   function End_Scene return Interfaces.C.int is
      OK : Boolean;
   begin
      F.End_Scene (Session, Pool, OK);
      pragma Assert (not V.Quiescent (Session) and P.Rendering (Pool));
      return (if OK then 0 else 2);
   end End_Scene;
   function Cancel return Interfaces.C.int is
      OK : Boolean;
   begin
      F.Cancel (Session, Pool, Damage, OK);
      if not OK then return 2; end if;
      Active := False;
      pragma Assert (V.Quiescent (Session) and not P.Rendering (Pool) and not P.Faulted (Pool));
      return 0;
   end Cancel;
   function Finish return Interfaces.C.int is
      OK : Boolean;
   begin
      F.Submit (Session, Pool, OK); Active := False;
      Check_Retained;
      pragma Assert (not V.Quiescent (Session) and P.Rendering (Pool));
      return (if OK then 0 else 2);
   end Finish;
   function Poll return Interfaces.C.int is
      R : F.Completion;
      Presented : P.Ticket;
   begin
      F.Poll (Session, Pool, Damage, R);
      if R = F.Still_Pending then
         Check_Retained;
         pragma Assert (P.Rendering (Pool) and not P.Writable (Pool, Ticket) and not V.Quiescent (Session));
         return 1;
      elsif R = F.Uncertain then
         pragma Assert (P.Faulted (Pool) and not V.Quiescent (Session));
         return 2;
      end if;
      pragma Assert (P.Ready (Pool) = Ticket and not P.Rendering (Pool));
      -- No scanout exists in this hosted fixture. Release the completed frame
      -- after the GPU's test-only readback has completed in the same submission.
      if not Direct then
         P.Present (Pool, Presented); P.Retire_Display (Pool, Presented, True);
      end if;
      pragma Assert (V.Quiescent (Session) and not P.Faulted (Pool));
      return 0;
   end Poll;
   function Budget return Interfaces.C.int is
      OK : Boolean;
      Ignore : Interfaces.C.int;
   begin
      Ignore := Start;
      pragma Assert (Ignore = 0);
      for I in 1 .. V.Maximum_Draws loop
         V.Admit_Draw (Session, OK); pragma Assert (OK);
      end loop;
      V.Admit_Draw (Session, OK);
      pragma Assert (not OK and not V.Complete_Frame (Session) and V.Draws (Session) = V.Maximum_Draws);
      F.Cancel (Session, Pool, Damage, OK); pragma Assert (OK and V.Quiescent (Session));
      Active := False;
      pragma Assert (not P.Faulted (Pool) and not P.Rendering (Pool));
      return 0;
   end Budget;
   procedure Capture_Begin (V : access constant Input) is
      Screen : constant G.Output :=
        (G.Physical_Extent (V.W), G.Physical_Extent (V.H), G.Orientation'Val (V.Rotation),
         (G.Scale_Component (V.N), G.Scale_Component (V.D)), G.Output_Origin (V.X), G.Output_Origin (V.Y));
   begin
      Captured := Vulkan_Scene.Open (Screen, 16#204060#);
   end Capture_Begin;
   function Capture (V : access constant Input) return Interfaces.C.int is
      OK : Boolean;
   begin
      if V.Mask = 4 then
         declare
            Key : constant GS.Key := (0, 63, (G.Scale_Component (V.N), G.Scale_Component (V.D)));
         begin
            if GS.Resolve (Glyph_Sources, Session, Key) /= Mask_Source then
               GS.Bind (Glyph_Sources, Session, 1, Key, Mask_Source, GS.L.Plan (Key.Scale), OK);
               if not OK then return 2; end if;
            end if;
            Vulkan_Scene.Append_Glyph (Captured, Session, Glyph_Sources, Key,
              (G.Logical_Coordinate (V.L), G.Logical_Coordinate (V.T), G.Logical_Coordinate (V.R), G.Logical_Coordinate (V.B)),
              V.Tint, OK);
            return (if OK then 0 else 2);
         end;
      end if;
      Vulkan_Scene.Append (Captured,
        ((if V.Mask /= 0 then Mask_Source else Source),
         (G.Logical_Coordinate (V.L), G.Logical_Coordinate (V.T), G.Logical_Coordinate (V.R), G.Logical_Coordinate (V.B)),
         V.Over /= 0, V.Mask /= 0, V.Tint,
         (if V.Over = 2 then Vulkan_Scene.Straight_Textured else Vulkan_Scene.Textured)), OK);
      return (if OK then 0 else 2);
   end Capture;
   function Capture_Backdrop (V : access constant Input; Mode : Interfaces.C.int) return Interfaces.C.int is
      OK : Boolean;
   begin
      Vulkan_Scene.Append_Backdrop (Captured, Source, G.Physical_Extent (V.W), G.Physical_Extent (V.H),
        Compositor_Image_Sampling.Placement'Val (Mode), OK);
      return (if OK then 0 else 2);
   end Capture_Backdrop;
   function Capture_Fill (V : access constant Input) return Interfaces.C.int is
      OK : Boolean;
   begin
      Vulkan_Scene.Append (Captured,
        (Vulkan_Submission.No_Source,
         (G.Logical_Coordinate (V.L), G.Logical_Coordinate (V.T), G.Logical_Coordinate (V.R), G.Logical_Coordinate (V.B)),
         False, False, V.Tint, Vulkan_Scene.Solid), OK);
      return (if OK then 0 else 2);
   end Capture_Fill;
   function Capture_Gradient (V : access constant Input; Bottom : Interfaces.Unsigned_32) return Interfaces.C.int is
      OK : Boolean;
   begin
      Vulkan_Scene.Append_Gradient (Captured,
        (G.Logical_Coordinate (V.L), G.Logical_Coordinate (V.T), G.Logical_Coordinate (V.R), G.Logical_Coordinate (V.B)),
        V.Tint, Bottom, OK);
      return (if OK then 0 else 2);
   end Capture_Gradient;
   function Capture_Clip (V : access constant Input; Reset : Interfaces.C.int) return Interfaces.C.int is
      use type Interfaces.C.int;
      OK : Boolean;
   begin
      Vulkan_Scene.Append (Captured,
        (Vulkan_Submission.No_Source,
         (G.Logical_Coordinate (V.L), G.Logical_Coordinate (V.T), G.Logical_Coordinate (V.R), G.Logical_Coordinate (V.B)),
         False, False, 0, (if Reset /= 0 then Vulkan_Scene.Reset_Clip else Vulkan_Scene.Set_Clip)), OK);
      return (if OK then 0 else 2);
   end Capture_Clip;
   function Capture_End return Interfaces.C.int is
      OK : Boolean;
   begin
      Vulkan_Scene.Seal (Captured, OK);
      return (if OK then 0 else 2);
   end Capture_End;
   function Replay return Interfaces.C.int is
      OK : Boolean;
   begin
      Vulkan_Scene.Replay (Captured, Session, Damage, OK);
      return (if OK then 0 else 2);
   end Replay;
   function Draw (Borrowed : System.Address; V : access constant Input)
     return Interfaces.C.int is
      use type Interfaces.C.int;
      Screen : constant G.Output :=
        (G.Physical_Extent (V.W), G.Physical_Extent (V.H), G.Orientation'Val (V.Rotation),
         (G.Scale_Component (V.N), G.Scale_Component (V.D)), G.Output_Origin (V.X), G.Output_Origin (V.Y));
      Result : B.Outcome;
   begin
      pragma Assert (Input'Size = 72 * 8);
      if Active then
         pragma Assert (Borrowed = Source_Context and Vulkan_Submission.Source_Valid (Session, Source));
         Vulkan_Submission.Draw_Output
           (Session, Source, Screen,
         (G.Logical_Coordinate (V.L), G.Logical_Coordinate (V.T), G.Logical_Coordinate (V.R), G.Logical_Coordinate (V.B)),
         (G.Pixel_Edge (V.DL), G.Pixel_Edge (V.DT), G.Pixel_Edge (V.DR), G.Pixel_Edge (V.DB)),
         V.Over /= 0, V.Mask /= 0, V.Tint, Result, Straight_Alpha => V.Over = 2);
      else
         B.Draw_Output (Borrowed, Screen,
         (G.Logical_Coordinate (V.L), G.Logical_Coordinate (V.T), G.Logical_Coordinate (V.R), G.Logical_Coordinate (V.B)),
         (G.Pixel_Edge (V.DL), G.Pixel_Edge (V.DT), G.Pixel_Edge (V.DR), G.Pixel_Edge (V.DB)),
         V.Over /= 0, V.Mask /= 0, V.Tint, Result, Straight_Alpha => V.Over = 2);
      end if;
      return B.Outcome'Pos (Result);
   end Draw;
end Vulkan_Submission_Test_Bridge;
