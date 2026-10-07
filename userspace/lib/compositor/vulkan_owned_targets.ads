with Compositor_Target_Damage;
with Vulkan_Context_Owner;
with System;
with Vulkan_Image_Owner;
with Vulkan_Target_Owner;
with Vulkan_Frame;
with Vulkan_Submission;
with Compositor_Pool;
-- Owns three private backing allocations and their dependent views together.
-- One state per output incarnation; requests/ledger stay private and unchanged
-- except through these owners. It does not grant scanout or external authority.
package Vulkan_Owned_Targets with SPARK_Mode is
   package C renames Vulkan_Context_Owner;
   package I renames Vulkan_Image_Owner;
   package A renames I.Accounting;
   package T renames Vulkan_Target_Owner;
   package P renames Compositor_Pool;
   package V renames Vulkan_Submission;
   use type I.Phase, T.Phase, A.State, System.Address, C.Child, C.Phase;
   type Phase is (Fresh, Backed, Live, Closed, Quarantined);
   type State is private;
   use type V.Phase;
   procedure Record_Readback (S : State; Submission : in out V.State;
      Pool : P.State; Staging : System.Address; Accepted : out Boolean)
     with Pre => P.Valid (Pool) and V.Current (Submission) = V.Recording and
       V.Complete_Frame (Submission) and not V.Pass_Started (Submission),
       Post => V.Current (Submission) = V.Recording and
       not V.Pass_Started (Submission) and Accepted = V.Complete_Frame (Submission) and
       (if Accepted then V.Draws (Submission) > 0);
   function Current (S : State) return Phase;
   function Output_Epoch (S : State) return P.ID;
   function Untouched (S : State) return Boolean;
   function Can_Attach (S : State) return Boolean;
   function Ready (S : State) return Boolean;
   function Same_Submission (S : State; Submission : V.State) return Boolean;
   function Parent_Held (S : State; Context : C.State) return Boolean;
   function Same_Parent (After, Before : State) return Boolean with Ghost;
   -- Production creation path: reserve a context child BEFORE allocating any
   -- image/view. Clean rollback retires it; uncertainty retains it permanently.
   procedure Initialize
     (S : in out State; Context : in out C.State;
      Requests : Vulkan_Frame.Targets; Description : System.Address;
      Epoch : P.Live_ID; Submission : V.State;
      Budget : in out A.State; Allowed_Types : I.U32)
     with Pre => Untouched (S) and A.Valid (Budget),
       Post => A.Valid (Budget) and A.Limit (Budget) = A.Limit (Budget'Old) and Current (S) in Live | Closed | Quarantined and
         (if Current (S) = Live then Ready (S) and Same_Submission (S, Submission) and
            Parent_Held (S, Context));
   -- Retire the output's context child only after backing/views and all pool
   -- readers retire. Foreign contexts cannot tear down or detach this output.
   procedure Close
     (S : in out State; Context : in out C.State;
      Submission : V.State; Pool : P.State;
      Budget : in out A.State; Released : out Boolean)
     with Pre => A.Valid (Budget),
       Post => A.Valid (Budget) and A.Limit (Budget) = A.Limit (Budget'Old) and
         (if Released then Current (S) = Closed and not Parent_Held (S, Context));
   -- Record a layout transition only for the acquired writer. Cold targets
   -- require full repaint; only confirmed GPU completion warms damage history.
   procedure Prepare_Frame (S : State; Submission : V.State; Pool : P.State;
      Damage : Compositor_Target_Damage.State; Accepted : out Boolean)
     with Pre => Compositor_Target_Damage.Valid (Damage);
   -- Low-level backing/view operations. Callers using Initialize must use the
   -- context-aware Close above to retire the parent registration as well.
   procedure Allocate (S : in out State; Requests : Vulkan_Frame.Targets;
                       Budget : in out A.State; Allowed_Types : I.U32)
     with Pre => Untouched (S) and A.Valid (Budget),
       Post => A.Valid (Budget) and A.Limit (Budget) = A.Limit (Budget'Old) and Same_Parent (S, S'Old) and Current (S) in Backed | Closed | Quarantined and
         (if Current (S) = Backed then Can_Attach (S));
   procedure Attach (S : in out State; Description : System.Address;
                     Epoch : P.Live_ID; Submission : V.State; Budget : in out A.State)
     with Pre => Can_Attach (S) and A.Valid (Budget),
       Post => A.Valid (Budget) and A.Limit (Budget) = A.Limit (Budget'Old) and Same_Parent (S, S'Old) and Current (S) in Live | Closed | Quarantined and
         (if Current (S) = Live then Ready (S) and Same_Submission (S, Submission));
   function Bindings (S : State) return Vulkan_Frame.Targets
     with Pre => Ready (S);
   function Can_Close (S : State; Submission : V.State; Pool : P.State) return Boolean;
   procedure Close (S : in out State; Submission : V.State; Pool : P.State;
                    Budget : in out A.State; Released : out Boolean)
     with Pre => A.Valid (Budget),
       Post => A.Valid (Budget) and A.Limit (Budget) = A.Limit (Budget'Old) and Same_Parent (S, S'Old) and (if Released then Current (S) = Closed) and
         (if not Can_Close (S'Old, Submission, Pool) then
             S = S'Old and Budget = Budget'Old and not Released);
private
   type Images is array (P.Live_Slot) of I.State;
   type State is record
      Mode : Phase := Fresh;
      Parent_Ticket : C.Child := C.No_Child;
      Backing : Images;
      Requests : Vulkan_Frame.Targets := (others => System.Null_Address);
      Views : T.State;
      Context, Description : System.Address := System.Null_Address;
   end record;
   function Same_Parent (After, Before : State) return Boolean is
     (After.Parent_Ticket = Before.Parent_Ticket);
   function Parent_Held (S : State; Context : C.State) return Boolean is
     (C.Held (Context, S.Parent_Ticket));
   function Current (S : State) return Phase is (S.Mode);
   function Output_Epoch (S : State) return P.ID is (T.Output_Epoch (S.Views));
   function Untouched (S : State) return Boolean is
     (S.Mode = Fresh and T.Current (S.Views) = T.Fresh and
      (for all N in P.Live_Slot => I.Status (S.Backing (N)) = I.Fresh));
   function Can_Attach (S : State) return Boolean is
     (S.Mode = Backed and T.Current (S.Views) = T.Fresh and
      (for all N in P.Live_Slot => I.Status (S.Backing (N)) = I.Live));
   function Ready (S : State) return Boolean is
     (S.Mode = Live and T.Current (S.Views) = T.Live and S.Context /= System.Null_Address);
   function Same_Submission (S : State; Submission : V.State) return Boolean is
     (S.Context /= System.Null_Address and S.Context = V.Owner_Context (Submission));
   function Can_Close (S : State; Submission : V.State; Pool : P.State) return Boolean is
     (S.Mode = Backed or else (S.Mode = Live and then Same_Submission (S, Submission) and then
                              T.Can_Close (S.Views, Submission, Pool)));
   function Bindings (S : State) return Vulkan_Frame.Targets is (T.Bindings (S.Views));
end Vulkan_Owned_Targets;
