with Interfaces;
with System;
with Vulkan_Context_Owner;
with Vulkan_Submission;
package Vulkan_Device_Owner with SPARK_Mode is
   package C renames Vulkan_Context_Owner;
   package V renames Vulkan_Submission;
   use type System.Address, C.Phase, C.State, V.State;
   type Phase is (Fresh, Software, Ready, Retiring, Retired, Quarantined);
   type State is private;
   function Current (S : State) return Phase;
   function Owns_Device (S : State) return Boolean;
   -- Slot=0 means no admitted render capability: do not call Mesa. Otherwise
   -- the caller supplies its manifest-bound, already admitted endpoint slot.
   -- Context/submission and S must be exclusively owned and never reset/copied.
   procedure Start
     (S : in out State; Context : in out C.State;
      Submission : in out V.State; Slot : Interfaces.Unsigned_64)
     with Pre => (if Current (S) = Fresh then
       C.Current (Context) = C.Fresh and C.Empty (Context) and
       V.Owner_Context (Submission) = System.Null_Address and V.Can_Destroy (Submission)),
       Post => Current (S) /= Fresh and
         (if Current (S'Old) = Fresh and Current (S) = Ready then Owns_Device (S) and
            C.Current (Context) = C.Live and
            V.Owner_Context (Submission) = C.Context (Context)) and
         (if Current (S'Old) /= Fresh then
            S = S'Old and Context = Context'Old and Submission = Submission'Old);
   -- Observe existing Mesa session health before admitting further GPU work.
   -- Success is not a fence/lease. Failure is sticky and retains every owner;
   -- never infer that a lost device or its display readers have retired.
   -- Performs at most one foreign status operation, which can block in IPC.
   -- Integrators must schedule it outside input dispatch, not per primitive.
   procedure Check_Health (S : in out State; Usable : out Boolean)
     with Post => Owns_Device (S) = Owns_Device (S'Old) and
       Usable = (Current (S) = Ready) and
       (if Current (S'Old) = Ready then Current (S) in Ready | Quarantined
        else S = S'Old and not Usable);
   -- One bounded retirement pass. Never closes Mesa while a registered child,
   -- source or GPU command remains. Pending keeps authority; unsafe is sticky.
   -- External display retirement must precede retirement of its child token.
   procedure Close
     (S : in out State; Context : in out C.State; Submission : V.State)
     with Post =>
       (if Current (S'Old) /= Fresh then Current (S) /= Fresh) and
       (if Current (S'Old) in Fresh | Retired | Quarantined then
          S = S'Old and Context = Context'Old) and
       (if not Owns_Device (S) and Owns_Device (S'Old) then
          Current (S) = Retired and C.Current (Context) in C.Fresh | C.Closed);
private
   type State is record
      Mode : Phase := Fresh;
      Held, Context_Attempted : Boolean := False;
      Identity : System.Address := System.Null_Address;
   end record;
   function Current (S : State) return Phase is (S.Mode);
   function Owns_Device (S : State) return Boolean is (S.Held);
end Vulkan_Device_Owner;
