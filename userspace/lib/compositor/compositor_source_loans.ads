with Interfaces;
-- Metadata policy only. Caller retains the actual grant/address in the matching
-- slot from reservation until Released. Callback evidence is not inferred here.
-- The table is sized by its owner (Capacity) and grows by copying into a
-- larger one (Grow): a slot keeps its position, so tickets stay meaningful.
package Compositor_Source_Loans with SPARK_Mode, Pure is
   subtype Slot is Positive;
   subtype Serial is Interfaces.Unsigned_64;
   use type Serial;
   type Ticket is private;
   No_Ticket : constant Ticket;
   type Phase is (Unused, Reserved, Attached, Renderer_Pending, Grant_Pending,
                  Released, Quarantined);
   type Renderer_Result is (Retired, Busy, Uncertain);
   type State (Capacity : Slot) is private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Index (T : Ticket) return Slot with Pre => T /= No_Ticket;
   -- A ticket from a table never names a slot past it; any other ticket is
   -- Unused there.
   function Current (S : State; T : Ticket) return Phase;
   function At_Slot (S : State; I : Slot) return Ticket with Pre => I <= S.Capacity;
   function Last_Serial (S : State) return Serial;
   function Other_Slots_Unchanged (After, Before : State; Changed : Slot)
      return Boolean with Ghost, Pre => After.Capacity = Before.Capacity;
   -- Into holds every slot of From at the same position, then empty slots.
   function Extends (Into, From : State) return Boolean with Ghost;
   procedure Reserve (S : in out State; T : out Ticket)
     with Pre => Valid (S), Post => Valid (S) and then
       (if T = No_Ticket then S = S'Old
        else Index (T) <= S.Capacity and then
          Current (S, T) = Reserved and then Last_Serial (S) = Last_Serial (S'Old) + 1 and then
          Other_Slots_Unchanged (S, S'Old, Index (T)));
   -- Failed acquisition: no grant was acquired. Only a reservation may cancel.
   procedure Cancel (S : in out State; T : Ticket)
     with Pre => Valid (S) and then Current (S, T) = Reserved,
       Post => Valid (S) and then Last_Serial (S) = Last_Serial (S'Old) and then
         Index (T) <= S.Capacity and then
         At_Slot (S, Index (T)) = T and then Other_Slots_Unchanged (S, S'Old, Index (T)) and then
         Current (S, T) = Released;
   procedure Activate (S : in out State; T : Ticket)
     with Pre => Valid (S) and then Current (S, T) = Reserved,
       Post => Valid (S) and then Last_Serial (S) = Last_Serial (S'Old) and then
         Index (T) <= S.Capacity and then
         At_Slot (S, Index (T)) = T and then Other_Slots_Unchanged (S, S'Old, Index (T)) and then
         Current (S, T) = Attached;
   procedure Retire (S : in out State; T : Ticket)
     with Pre => Valid (S) and then Current (S, T) in Attached | Renderer_Pending | Grant_Pending,
       Post => Valid (S) and then Last_Serial (S) = Last_Serial (S'Old) and then
         Index (T) <= S.Capacity and then
         At_Slot (S, Index (T)) = T and then Other_Slots_Unchanged (S, S'Old, Index (T)) and then
         Current (S, T) =
         (if Current (S'Old, T) = Attached then Renderer_Pending else Current (S'Old, T));
   procedure Observe_Renderer (S : in out State; T : Ticket; Result : Renderer_Result)
     with Pre => Valid (S) and then Current (S, T) = Renderer_Pending,
       Post => Valid (S) and then Last_Serial (S) = Last_Serial (S'Old) and then
         Index (T) <= S.Capacity and then
         At_Slot (S, Index (T)) = T and then Other_Slots_Unchanged (S, S'Old, Index (T)) and then
         Current (S, T) =
         (case Result is when Retired => Grant_Pending,
          when Busy => Renderer_Pending, when Uncertain => Quarantined) and then
         (if Result = Busy then S = S'Old);
   procedure Observe_Grant (S : in out State; T : Ticket; Confirmed : Boolean)
     with Pre => Valid (S) and then Current (S, T) = Grant_Pending,
       Post => Valid (S) and then Last_Serial (S) = Last_Serial (S'Old) and then
         Index (T) <= S.Capacity and then
         At_Slot (S, Index (T)) = T and then Other_Slots_Unchanged (S, S'Old, Index (T)) and then
         Current (S, T) =
         (if Confirmed then Released else Quarantined);
   -- A larger table with the same loans: every ticket has the same phase in
   -- Into as in From, and then the added slots are free.
   procedure Grow (From : State; Into : out State)
     with Pre => Valid (From) and then Into.Capacity >= From.Capacity,
       Post => Valid (Into) and then Last_Serial (Into) = Last_Serial (From) and then
         Extends (Into, From) and then
         (for all I in 1 .. From.Capacity => At_Slot (Into, I) = At_Slot (From, I)) and then
         (for all I in 1 .. Into.Capacity => (if I > From.Capacity then At_Slot (Into, I) = No_Ticket));
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
   type Entries is array (Slot range <>) of Entry_State;
   type State (Capacity : Slot) is record
      Items : Entries (1 .. Capacity);
      Last : Serial := 0;
   end record;
   function Valid (S : State) return Boolean is
     (for all I in 1 .. S.Capacity => S.Items (I).Identity <= S.Last and
       S.Items (I).Status /= Unused and
       (if S.Items (I).Status /= Released then S.Items (I).Identity /= 0));
   function Other_Slots_Unchanged (After, Before : State; Changed : Slot)
      return Boolean is
     (for all I in 1 .. After.Capacity =>
        (if I /= Changed then After.Items (I) = Before.Items (I)));
   function Extends (Into, From : State) return Boolean is
     (Into.Capacity >= From.Capacity and then Into.Last = From.Last and then
      (for all I in 1 .. From.Capacity => Into.Items (I) = From.Items (I)) and then
      (for all I in 1 .. Into.Capacity =>
         (if I > From.Capacity then Into.Items (I) = (Identity => 0, Status => Released))));
   function Index (T : Ticket) return Slot is (T.Position);
   -- Old opaque tickets can observe completed retirement after a slot is
   -- reused, but can never mutate its new generation through the preconditions.
   function Current (S : State; T : Ticket) return Phase is
     (if T.Identity = 0 or else T.Position > S.Capacity or else
         T.Identity > S.Items (T.Position).Identity then Unused
      elsif T.Identity < S.Items (T.Position).Identity then Released
      else S.Items (T.Position).Status);
   function At_Slot (S : State; I : Slot) return Ticket is
     (if S.Items (I).Identity = 0 then No_Ticket else (I, S.Items (I).Identity));
   function Last_Serial (S : State) return Serial is (S.Last);
end Compositor_Source_Loans;
