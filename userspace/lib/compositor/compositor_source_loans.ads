with Interfaces;
-- Metadata policy only. Caller retains the actual grant/address in the matching
-- slot from reservation until Released. Callback evidence is not inferred here.
generic
   Capacity : Positive;
package Compositor_Source_Loans with SPARK_Mode, Pure is
   subtype Slot is Positive range 1 .. Capacity;
   subtype Serial is Interfaces.Unsigned_64;
   use type Serial;
   type Ticket is private;
   No_Ticket : constant Ticket;
   type Phase is (Unused, Reserved, Attached, Renderer_Pending, Grant_Pending,
                  Released, Quarantined);
   type Renderer_Result is (Retired, Busy, Uncertain);
   type State is private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Index (T : Ticket) return Slot with Pre => T /= No_Ticket;
   function Current (S : State; T : Ticket) return Phase;
   function At_Slot (S : State; I : Slot) return Ticket;
   function Last_Serial (S : State) return Serial;
   function Other_Slots_Unchanged (After, Before : State; Changed : Slot)
      return Boolean with Ghost;
   procedure Reserve (S : in out State; T : out Ticket)
     with Pre => Valid (S), Post => Valid (S) and
       (if T = No_Ticket then S = S'Old
        else Current (S, T) = Reserved and Last_Serial (S) = Last_Serial (S'Old) + 1 and
          Other_Slots_Unchanged (S, S'Old, Index (T)));
   -- Failed acquisition: no grant was acquired. Only a reservation may cancel.
   procedure Cancel (S : in out State; T : Ticket)
     with Pre => Valid (S) and Current (S, T) = Reserved,
       Post => Valid (S) and Last_Serial (S) = Last_Serial (S'Old) and
         At_Slot (S, Index (T)) = T and Other_Slots_Unchanged (S, S'Old, Index (T)) and
         Current (S, T) = Released;
   procedure Activate (S : in out State; T : Ticket)
     with Pre => Valid (S) and Current (S, T) = Reserved,
       Post => Valid (S) and Last_Serial (S) = Last_Serial (S'Old) and
         At_Slot (S, Index (T)) = T and Other_Slots_Unchanged (S, S'Old, Index (T)) and
         Current (S, T) = Attached;
   procedure Retire (S : in out State; T : Ticket)
     with Pre => Valid (S) and Current (S, T) in Attached | Renderer_Pending | Grant_Pending,
       Post => Valid (S) and Last_Serial (S) = Last_Serial (S'Old) and
         At_Slot (S, Index (T)) = T and Other_Slots_Unchanged (S, S'Old, Index (T)) and
         Current (S, T) =
         (if Current (S'Old, T) = Attached then Renderer_Pending else Current (S'Old, T));
   procedure Observe_Renderer (S : in out State; T : Ticket; Result : Renderer_Result)
     with Pre => Valid (S) and Current (S, T) = Renderer_Pending,
       Post => Valid (S) and Last_Serial (S) = Last_Serial (S'Old) and
         At_Slot (S, Index (T)) = T and Other_Slots_Unchanged (S, S'Old, Index (T)) and
         Current (S, T) =
         (case Result is when Retired => Grant_Pending,
          when Busy => Renderer_Pending, when Uncertain => Quarantined) and
         (if Result = Busy then S = S'Old);
   procedure Observe_Grant (S : in out State; T : Ticket; Confirmed : Boolean)
     with Pre => Valid (S) and Current (S, T) = Grant_Pending,
       Post => Valid (S) and Last_Serial (S) = Last_Serial (S'Old) and
         At_Slot (S, Index (T)) = T and Other_Slots_Unchanged (S, S'Old, Index (T)) and
         Current (S, T) =
         (if Confirmed then Released else Quarantined);
private
   type Ticket is record
      Position : Slot := Slot'First;
      Identity : Serial := 0;
   end record;
   No_Ticket : constant Ticket := (Slot'First, 0);
   type Entry_State is record
      Identity : Serial := 0;
      Status : Phase := Released;
   end record;
   type Entries is array (Slot) of Entry_State;
   type State is record
      Items : Entries;
      Last : Serial := 0;
   end record;
   function Valid (S : State) return Boolean is
     (for all I in Slot => S.Items (I).Identity <= S.Last and
       S.Items (I).Status /= Unused and
       (if S.Items (I).Status /= Released then S.Items (I).Identity /= 0));
   function Other_Slots_Unchanged (After, Before : State; Changed : Slot)
      return Boolean is
     (for all I in Slot => (if I /= Changed then After.Items (I) = Before.Items (I)));
   function Index (T : Ticket) return Slot is (T.Position);
   -- Old opaque tickets can observe completed retirement after a slot is
   -- reused, but can never mutate its new generation through the preconditions.
   function Current (S : State; T : Ticket) return Phase is
     (if T.Identity = 0 or T.Identity > S.Items (T.Position).Identity then Unused
      elsif T.Identity < S.Items (T.Position).Identity then Released
      else S.Items (T.Position).Status);
   function At_Slot (S : State; I : Slot) return Ticket is
     (if S.Items (I).Identity = 0 then No_Ticket else (I, S.Items (I).Identity));
   function Last_Serial (S : State) return Serial is (S.Last);
end Compositor_Source_Loans;
