with System;
with Interfaces;
with Compositor_Affine;
with Vulkan_Affine_Binding;
package Vulkan_Submission with SPARK_Mode is
   use type Vulkan_Affine_Binding.Outcome, System.Address, Interfaces.Unsigned_64;
   type Phase is (Idle, Recording, Sealed, Pending, Quarantined);
   Maximum_Draws : constant := 4096;
   subtype Draw_Count is Natural range 0 .. Maximum_Draws;
   -- Fixed descriptor metadata, independent of the configured pixel-byte budget.
   type Source_Slot is range 0 .. 139;
   subtype Glyph_Slot is Source_Slot range 0 .. 127;
   subtype Backdrop_Slot is Source_Slot range 128 .. 129;
   subtype Icon_Atlas_Slot is Source_Slot range 130 .. 131;
   subtype Client_Slot is Source_Slot range 132 .. 139;
   type Source_Ticket is private;
   No_Source : constant Source_Ticket;
   type State is private;
   function Current (S : State) return Phase;
   function Source_Present (S : State; Index : Source_Slot) return Boolean;
   function Source_Valid (S : State; Ticket : Source_Ticket) return Boolean;
   function Same_Sources (Left, Right : State) return Boolean;
   function Same_Source (Left, Right : State; Index : Source_Slot) return Boolean;
   function Source_At (S : State; Index : Source_Slot) return Source_Ticket;
   function Source_Sequence (S : State) return Interfaces.Unsigned_64 with Ghost;
   function Source_Generation (Ticket : Source_Ticket) return Interfaces.Unsigned_64 with Ghost;
   -- Borrowed immutable draw context and every referenced image/view/descriptor
   -- remain provider-owned and alive until Remove_Source returns that context.
   -- Aliased underlying resources require provider-level shared ownership.
   -- Invalid calls return without touching foreign resources or the table.
   procedure Install_Source
     (S : in out State; Index : Source_Slot; Borrowed_Draw : System.Address;
      Ticket : out Source_Ticket)
     with Post => Current (S) = Current (S'Old) and
       (if Ticket /= No_Source then Source_Valid (S, Ticket) and Source_Present (S, Index) and
          Source_Sequence (S) > Source_Sequence (S'Old) and
          Source_Generation (Ticket) = Source_Sequence (S)
        else Same_Sources (S, S'Old)) and
       (for all J in Source_Slot => (if J /= Index then Same_Source (S, S'Old, J))) and
       (if Current (S'Old) /= Idle then Same_Sources (S, S'Old) and Ticket = No_Source);
   procedure Remove_Source
     (S : in out State; Ticket : Source_Ticket; Released : out System.Address)
     with Post => Source_Sequence (S) = Source_Sequence (S'Old) and
       Current (S) = Current (S'Old) and
       (if Released = System.Null_Address then Same_Sources (S, S'Old)
        else Current (S'Old) = Idle and Source_Valid (S'Old, Ticket) and not Source_Valid (S, Ticket)) and
       (for all J in Source_Slot =>
         (if Source_At (S'Old, J) /= Ticket then Same_Source (S, S'Old, J))) and
       (if Current (S'Old) /= Idle then Same_Sources (S, S'Old) and Released = System.Null_Address);
   -- Managed provider path. Idle-only; unknown foreign outcomes quarantine
   -- without releasing registrations or the caller's attempted image lease.
   procedure Import_Source
     (S : in out State; Index : Source_Slot; Description : System.Address;
      Ticket : out Source_Ticket)
     with Post => Current (S) in Current (S'Old) | Quarantined and
       (if Ticket /= No_Source then Current (S) = Idle and Source_Valid (S, Ticket) and
          Source_Sequence (S) > Source_Sequence (S'Old) and Source_Generation (Ticket) = Source_Sequence (S)
        else Same_Sources (S, S'Old)) and
       (if Current (S'Old) /= Idle then S = S'Old and Ticket = No_Source) and
       (for all J in Source_Slot => (if J /= Index then Same_Source (S, S'Old, J)));
   procedure Release_Source
     (S : in out State; Ticket : Source_Ticket; Released : out System.Address)
     with Post => Current (S) in Current (S'Old) | Quarantined and
       Source_Sequence (S) = Source_Sequence (S'Old) and
       (if Released = System.Null_Address then Same_Sources (S, S'Old)
        else Current (S) = Idle and Source_Valid (S'Old, Ticket) and not Source_Valid (S, Ticket)) and
       (for all J in Source_Slot =>
         (if Source_At (S'Old, J) /= Ticket then Same_Source (S, S'Old, J))) and
       (if Current (S'Old) /= Idle then S = S'Old and Released = System.Null_Address);
   function Draws (S : State) return Draw_Count;
   function Complete_Frame (S : State) return Boolean;
   function Pass_Started (S : State) return Boolean;
   function Pass_Active (S : State) return Boolean;
   function Pass_Finished (S : State) return Boolean;
   -- GPU/command quiescence alone does not release registered source imports.
   function Can_Destroy (S : State) return Boolean is
     (Current (S) = Idle and (for all I in Source_Slot => not Source_Present (S, I)));
   function Quiescent (S : State) return Boolean is (Current (S) = Idle);
   -- Stable private native context identity. Owners must not copy/reset a live
   -- controller or recycle its context address while any resource is attached.
   function Owner_Context (S : State) return System.Address;
   -- Fresh, exclusively owned native context, with no old work/references.
   -- Do not use Open to recover a quarantined context or rebind live objects.
   function Open (Borrowed : System.Address) return State
     with Post => Current (Open'Result) = Idle and Draws (Open'Result) = 0 and not Pass_Started (Open'Result);
   procedure Begin_Record (S : in out State; Accepted : out Boolean)
     with Pre => Current (S) = Idle,
       Post => Same_Sources (S, S'Old) and Accepted = (Current (S) = Recording) and
         Current (S) in Recording | Quarantined and Draws (S) = 0 and
         Complete_Frame (S) = Accepted and
         not Quiescent (S) and not Pass_Started (S);
   -- One render pass per frame. Before/after the pass, callers may record
   -- required transfer/barrier commands through their audited adapters.
   procedure Begin_Scene
     (S : in out State; Borrowed_Pass : System.Address;
      Width, Height : Compositor_Affine.G.Physical_Extent; Accepted : out Boolean)
     with Pre => Current (S) = Recording and not Pass_Started (S) and Complete_Frame (S),
       Post => Same_Sources (S, S'Old) and Accepted = (Current (S) = Recording) and
         Current (S) in Recording | Quarantined and
         Pass_Active (S) = Accepted and not Pass_Finished (S) and
         Draws (S) = Draws (S'Old) and Complete_Frame (S) = Complete_Frame (S'Old);
   procedure End_Scene (S : in out State; Accepted : out Boolean)
     with Pre => Current (S) = Recording and Pass_Active (S),
       Post => Same_Sources (S, S'Old) and Accepted = (Current (S) = Recording) and
         Current (S) in Recording | Quarantined and
         Pass_Finished (S) = Accepted and Pass_Active (S) = not Accepted and
         Draws (S) = Draws (S'Old) and Complete_Frame (S) = Complete_Frame (S'Old);
   -- Charge BEFORE invoking a draw adapter, including attempts that are empty
   -- or rejected. Exhaustion invalidates this frame; it must be cancelled.
   procedure Admit_Draw (S : in out State; Accepted : out Boolean)
     with Pre => Current (S) = Recording,
       Post => Same_Sources (S, S'Old) and Current (S) = Recording and
         Pass_Active (S) = Pass_Active (S'Old) and Pass_Finished (S) = Pass_Finished (S'Old) and
         Accepted = (Complete_Frame (S'Old) and Draws (S'Old) < Maximum_Draws) and
         Draws (S) = Draws (S'Old) + (if Accepted then 1 else 0) and
         Complete_Frame (S) = Accepted;
   procedure Reject_Frame (S : in out State)
     with Pre => Current (S) = Recording,
       Post => Same_Sources (S, S'Old) and Current (S) = Recording and not Complete_Frame (S) and
         Pass_Active (S) = Pass_Active (S'Old) and Pass_Finished (S) = Pass_Finished (S'Old) and
         Draws (S) = Draws (S'Old);
   -- Draw context must reference this submission's command buffer/device.
   -- Rejection or admission exhaustion invalidates the whole candidate frame.
   procedure Draw_Output
     (S : in out State; Source : Source_Ticket;
      Screen : Compositor_Affine.G.Output;
      Surface : Compositor_Affine.G.Logical_Rectangle;
      Damage : Compositor_Affine.G.Physical_Rectangle;
      Over, Mask : Boolean; Tint : Compositor_Affine.Word;
      Result : out Vulkan_Affine_Binding.Outcome;
      Raster_Glyph : Boolean := False; Straight_Alpha : Boolean := False)
     with Pre => Current (S) = Recording and Pass_Active (S),
       Post => Same_Sources (S, S'Old) and Current (S) = Recording and Pass_Active (S) and
         Draws (S) = Draws (S'Old) +
           (if Complete_Frame (S'Old) and Draws (S'Old) < Maximum_Draws then 1 else 0) and
         (if Result = Vulkan_Affine_Binding.Rejected then not Complete_Frame (S)
          else Complete_Frame (S) and Draws (S) = Draws (S'Old) + 1);
   -- Opaque RGB fill. Empty/inverted rectangles consume one bounded attempt
   -- without recording; out-of-target geometry or foreign failure rejects.
   procedure Fill_Output
     (S : in out State; Area : Compositor_Affine.G.Physical_Rectangle;
      RGB : Compositor_Affine.Word; Accepted : out Boolean)
     with Pre => Current (S) = Recording and Pass_Active (S),
       Post => Same_Sources (S, S'Old) and Current (S) = Recording and Pass_Active (S) and
         Draws (S) = Draws (S'Old) +
           (if Complete_Frame (S'Old) and Draws (S'Old) < Maximum_Draws then 1 else 0) and
         Accepted = Complete_Frame (S);
   -- Caller has recorded final dependency transitions after End_Scene.
   procedure Seal (S : in out State; Accepted : out Boolean)
     with Pre => Current (S) = Recording and Complete_Frame (S) and Pass_Finished (S),
       Post => Same_Sources (S, S'Old) and Accepted = (Current (S) = Sealed) and
         Current (S) in Sealed | Quarantined and not Quiescent (S) and
         Complete_Frame (S) = Complete_Frame (S'Old) and Draws (S) = Draws (S'Old);
   -- Transfer-only batch: at least one admitted operation, no render pass.
   -- Transfer recording never makes an output frame or an image publishable.
   procedure Seal_Transfer (S : in out State; Accepted : out Boolean)
     with Pre => Current (S) = Recording and Complete_Frame (S) and
                 not Pass_Started (S) and Draws (S) > 0,
       Post => Same_Sources (S, S'Old) and Accepted = (Current (S) = Sealed) and
         Current (S) in Sealed | Quarantined and not Quiescent (S) and not Pass_Started (S) and
         Complete_Frame (S) = Complete_Frame (S'Old) and Draws (S) = Draws (S'Old);
   procedure Submit (S : in out State; Accepted : out Boolean)
     with Pre => Current (S) = Sealed,
       Post => Same_Sources (S, S'Old) and Accepted = (Current (S) = Pending) and
         Current (S) in Pending | Quarantined and not Quiescent (S) and
         Complete_Frame (S) = Complete_Frame (S'Old) and Draws (S) = Draws (S'Old);
   type Observation is (Still_Pending, Finished, Uncertain);
   procedure Poll (S : in out State; Result : out Observation)
     with Pre => Current (S) = Pending,
       Post => Same_Sources (S, S'Old) and (case Result is
         when Still_Pending => S = S'Old,
         when Finished => Current (S) = Idle and Quiescent (S),
         when Uncertain => Current (S) = Quarantined and not Quiescent (S));
   -- Only unsubmitted command buffers may be discarded. Failure is sticky.
   procedure Cancel (S : in out State; Accepted : out Boolean)
     with Pre => Current (S) in Recording | Sealed,
       Post => Same_Sources (S, S'Old) and (Accepted = Quiescent (S)) and Current (S) in Idle | Quarantined and
         (if Accepted then not Pass_Started (S));
private
   subtype Serial is Interfaces.Unsigned_64;
   use type Serial;
   type Source_Ticket is record
      Index : Source_Slot := Source_Slot'First;
      Generation : Serial := 0;
   end record;
   No_Source : constant Source_Ticket := (others => <>);
   type Source_Entry is record
      Context : System.Address := System.Null_Address;
      Managed : Boolean := False;
      Generation : Serial := 0;
   end record;
   type Source_Table is array (Source_Slot) of Source_Entry;
   type Pass_Phase is (Before_Pass, Inside_Pass, After_Pass);
   type State is record
      Context : System.Address := System.Null_Address;
      Sources : Source_Table;
      Last_Source : Serial := 0;
      Stage : Phase := Idle;
      Count : Draw_Count := 0;
      Whole : Boolean := True;
      Pass_State : Pass_Phase := Before_Pass;
      Target_Width, Target_Height : Compositor_Affine.G.Physical_Extent := 1;
   end record;
   function Source_Sequence (S : State) return Interfaces.Unsigned_64 is (S.Last_Source);
   function Source_Generation (Ticket : Source_Ticket) return Interfaces.Unsigned_64 is (Ticket.Generation);
   function Source_Present (S : State; Index : Source_Slot) return Boolean is
     (S.Sources (Index).Context /= System.Null_Address);
   function Source_Valid (S : State; Ticket : Source_Ticket) return Boolean is
     (Ticket.Generation /= 0 and then Source_Present (S, Ticket.Index) and then
      S.Sources (Ticket.Index).Generation = Ticket.Generation);
   function Same_Source (Left, Right : State; Index : Source_Slot) return Boolean is
     (Left.Sources (Index) = Right.Sources (Index));
   function Source_At (S : State; Index : Source_Slot) return Source_Ticket is
     (if Source_Present (S, Index) then (Index, S.Sources (Index).Generation) else No_Source);
   function Same_Sources (Left, Right : State) return Boolean is
     (Left.Sources = Right.Sources and Left.Last_Source = Right.Last_Source);
   function Current (S : State) return Phase is (S.Stage);
   function Owner_Context (S : State) return System.Address is (S.Context);
   function Draws (S : State) return Draw_Count is (S.Count);
   function Complete_Frame (S : State) return Boolean is (S.Whole);
   function Pass_Started (S : State) return Boolean is (S.Pass_State /= Before_Pass);
   function Pass_Active (S : State) return Boolean is (S.Pass_State = Inside_Pass);
   function Pass_Finished (S : State) return Boolean is (S.Pass_State = After_Pass);
   function Open (Borrowed : System.Address) return State is
     ((Context => Borrowed, others => <>));
end Vulkan_Submission;
