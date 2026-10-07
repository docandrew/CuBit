with System;
with Interfaces;
with Vulkan_Submission;
package Vulkan_Context_Owner with SPARK_Mode is
   package V renames Vulkan_Submission;
   subtype Serial is Interfaces.Unsigned_64;
   use type Serial, System.Address;
   type Phase is (Fresh, Live, Closed, Quarantined);
   type State is private;
   type Child is private;
   No_Child : constant Child;
   -- One target bundle, one pipeline, all sources and upload/readback staging.
   -- Derive from the bounded scene inventory, not the general driver BO limit.
   Maximum_Children : constant := 1 + 1 +
     (V.Source_Slot'Pos (V.Source_Slot'Last) - V.Source_Slot'Pos (V.Source_Slot'First) + 1) + 2;
   function Current (S : State) return Phase;
   function Context (S : State) return System.Address;
   function Sequence (S : State) return Serial;
   function Held (S : State; Ticket : Child) return Boolean;
   function Empty (S : State) return Boolean;
   function Preserves_Children (After, Before : State) return Boolean with Ghost;
   function Preserves_Other_Children (After, Before : State; Ticket : Child) return Boolean with Ghost;
   -- Private request, zeroed native storage and admitted device must outlive
   -- this owner. Do not copy/reset owner or recycle any context address.
   procedure Initialize (S : in out State; Description : System.Address; Accepted : out Boolean)
     with Pre => Current (S) = Fresh and Empty (S),
       Post => Current (S) in Live | Closed | Quarantined and Empty (S) and
         Accepted = (Current (S) = Live) and
         (if Accepted then Context (S) /= System.Null_Address);
   -- Reserve BEFORE creating target views/pipelines/other dependent objects.
   -- A rejected registration permits no native creation. Each token binds
   -- this context and a nonwrapping incarnation; capacity stays fixed.
   procedure Register_Child (S : in out State; Ticket : out Child)
     with Post => Current (S) = Current (S'Old) and Context (S) = Context (S'Old) and
       Preserves_Children (S, S'Old) and
       (if Ticket /= No_Child then Held (S, Ticket) and Sequence (S) > Sequence (S'Old)
        else S = S'Old);
   -- Native_Retired must come from confirmed child-owner release or known-
   -- clean creation rollback, never mere request cancellation/device health.
   -- Uncertain children retain their token indefinitely. This observation is
   -- a trusted boundary; policy proves only how it affects the registry.
   procedure Retire_Child (S : in out State; Ticket : Child; Native_Retired : Boolean)
     with Post => Current (S) = Current (S'Old) and Context (S) = Context (S'Old) and
       Sequence (S) = Sequence (S'Old) and Preserves_Other_Children (S, S'Old, Ticket) and
       (if not Native_Retired or not Held (S'Old, Ticket) then S = S'Old
        else not Held (S, Ticket));
   function Can_Close (S : State; Submission : V.State) return Boolean;
   procedure Close (S : in out State; Submission : V.State; Released : out Boolean)
     with Post =>
       (if Can_Close (S'Old, Submission) then
          Current (S) in Closed | Quarantined and (Released = (Current (S) = Closed))
        else S = S'Old and not Released);
private
   subtype Slot is Positive range 1 .. Maximum_Children;
   type Child is record
      Owner : System.Address := System.Null_Address;
      Index : Slot := 1;
      Generation : Serial := 0;
   end record;
   No_Child : constant Child := (others => <>);
   type Registry is array (Slot) of Serial;
   type State is record
      Mode : Phase := Fresh;
      Request, Borrowed : System.Address := System.Null_Address;
      Last : Serial := 0;
      Children : Registry := (others => 0);
   end record;
   function Current (S : State) return Phase is (S.Mode);
   function Context (S : State) return System.Address is (S.Borrowed);
   function Sequence (S : State) return Serial is (S.Last);
   function Empty (S : State) return Boolean is (for all N in Slot => S.Children (N) = 0);
   function Held (S : State; Ticket : Child) return Boolean is
     (S.Mode = Live and Ticket.Owner = S.Borrowed and Ticket.Generation /= 0 and
      S.Children (Ticket.Index) = Ticket.Generation);
   function Preserves_Children (After, Before : State) return Boolean is
     (for all N in Slot => (if Before.Children (N) /= 0 then After.Children (N) = Before.Children (N)));
   function Preserves_Other_Children (After, Before : State; Ticket : Child) return Boolean is
     (for all N in Slot => (if N /= Ticket.Index then After.Children (N) = Before.Children (N)));
   function Can_Close (S : State; Submission : V.State) return Boolean is
     (S.Mode = Live and S.Borrowed /= System.Null_Address and
      V.Owner_Context (Submission) = S.Borrowed and V.Can_Destroy (Submission) and Empty (S));
end Vulkan_Context_Owner;
