with System;
with Vulkan_Frame;
with Vulkan_Submission;
with Compositor_Pool;
package Vulkan_Target_Owner with SPARK_Mode is
   package V renames Vulkan_Submission;
   package P renames Compositor_Pool;
   use type P.Ticket, P.ID;
   type Phase is (Fresh, Live, Closed, Quarantined);
   type State is private;
   function Current (S : State) return Phase;
   function Output_Epoch (S : State) return P.ID;
   function Bindings (S : State) return Vulkan_Frame.Targets with Pre => Current (S) = Live;
   -- One attempt per fresh output incarnation. Unknown retains every requested
   -- lease even if nothing could be published. Never reopen quarantined storage.
   procedure Initialize (S : in out State; Description : System.Address; Output_Epoch : P.Live_ID; Accepted : out Boolean)
     with Pre => Current (S) = Fresh,
       Post => Current (S) in Live | Closed | Quarantined and (Accepted = (Current (S) = Live));
   function Can_Close (S : State; Submission : V.State; Pool : P.State) return Boolean;
   procedure Close (S : in out State; Submission : V.State; Pool : P.State; Released : out Boolean)
     with Post =>
       (if Can_Close (S'Old, Submission, Pool) then
          Current (S) in Closed | Quarantined and (Released = (Current (S) = Closed))
        else S = S'Old and not Released);
private
   type State is record
      Mode : Phase := Fresh;
      Epoch : P.ID := 0;
      Request : System.Address := System.Null_Address;
      Views : Vulkan_Frame.Targets := (others => System.Null_Address);
   end record;
   function Can_Close (S : State; Submission : V.State; Pool : P.State) return Boolean is
     (Current (S) = Live and P.Epoch (Pool) = S.Epoch and V.Can_Destroy (Submission) and P.Valid (Pool) and not P.Faulted (Pool) and
      P.Writer (Pool) = P.None and P.Ready (Pool) = P.None and
      P.Displayed (Pool) = P.None and P.Front (Pool) = P.None);
   function Current (S : State) return Phase is (S.Mode);
   function Output_Epoch (S : State) return P.ID is (S.Epoch);
   function Bindings (S : State) return Vulkan_Frame.Targets is (S.Views);
end Vulkan_Target_Owner;
