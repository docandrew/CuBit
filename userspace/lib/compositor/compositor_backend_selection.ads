-- Policy only: the startup adapter authenticates every readiness observation.
-- No observation here grants authority or proves GPU/consumer retirement.
with Interfaces;
package Compositor_Backend_Selection with SPARK_Mode, Pure is
   type Mode is (Unselected, Software, GPU);
   type Readiness is record
      Admitted, Device, Targets, Pipeline, Upload, Readback, Configuration : Boolean := False;
   end record;
   function Ready (Evidence : Readiness) return Boolean is
     (Evidence.Admitted and Evidence.Device and Evidence.Targets and
      Evidence.Pipeline and Evidence.Upload and Evidence.Readback and Evidence.Configuration);
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   type Recovery_Key is record
      Output, Epoch, Frame, Buffer : Word := 0;
   end record;
   function Valid (Key : Recovery_Key) return Boolean is
     (Key.Epoch /= 0 and Key.Frame /= 0 and Key.Buffer in 1 .. 3);
   type Recovery_Phase is (Idle, Draining, Recovered, Quarantined);
   type Drain_Evidence is record
      Renderer, Sources, Readback, Output_Writer, Full_Repaint : Boolean := False;
      Uncertain : Boolean := False;
   end record;
   function Drained (Evidence : Drain_Evidence) return Boolean is
     (Evidence.Renderer and Evidence.Sources and Evidence.Readback and
      Evidence.Output_Writer and Evidence.Full_Repaint and not Evidence.Uncertain);
   type State is private;
   function Current (S : State) return Mode;
   procedure Select_Backend (S : in out State; Evidence : Readiness; Accepted : out Boolean)
     with Post => Accepted = (Current (S'Old) = Unselected) and then
       (if Accepted then Current (S) = (if Ready (Evidence) then GPU else Software)
        else S = S'Old);
   -- An early draw closes admission with CPU selected; it cannot activate GPU.
   procedure Begin_Output (S : in out State)
     with Post => Current (S) /= Unselected and then
       (if Current (S'Old) = Unselected then Current (S) = Software else S = S'Old);
   function Recovery (S : State) return Recovery_Phase;
   function Key_Of (S : State) return Recovery_Key;
   function Can_Capture (S : State) return Boolean is
     (Current (S) /= Unselected and Recovery (S) in Idle | Recovered);
   procedure Request_Recovery (S : in out State; Key : Recovery_Key; Accepted : out Boolean)
     with Post => Accepted = (Current (S'Old) = GPU and Recovery (S'Old) = Idle and Valid (Key))
       and then Current (S) = Current (S'Old) and then
       (if Accepted then Recovery (S) = Draining and Key_Of (S) = Key
        else S = S'Old);
   -- Adapter supplies evidence for THIS writer/output, after closing admission.
   -- Full_Repaint means queued invalidation, not completion of the next CPU frame.
   procedure Observe_Recovery (S : in out State; Key : Recovery_Key;
      Evidence : Drain_Evidence; Switched : out Boolean)
     with Post =>
       Switched = (Recovery (S'Old) = Draining and Key = Key_Of (S'Old) and Drained (Evidence))
       and then Key_Of (S) = Key_Of (S'Old) and then
       (if Recovery (S'Old) /= Draining or Key /= Key_Of (S'Old) then S = S'Old
        elsif Evidence.Uncertain then Recovery (S) = Quarantined and Current (S) = Current (S'Old)
        elsif Drained (Evidence) then Recovery (S) = Recovered and Current (S) = Software
        else S = S'Old);
private
   type State is record
      Selected : Mode := Unselected;
      Recovering : Recovery_Phase := Idle;
      Key : Recovery_Key;
   end record;
   function Current (S : State) return Mode is (S.Selected);
   function Recovery (S : State) return Recovery_Phase is (S.Recovering);
   function Key_Of (S : State) return Recovery_Key is (S.Key);
end Compositor_Backend_Selection;
