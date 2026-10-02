-- Producer write eligibility. Foreign protection and authenticated replies
-- are evidence supplied by the narrow adapter, not assumptions of this model.
package Client_Frame_State with SPARK_Mode, Pure is
   subtype Identity is Positive range 1 .. 2 ** 31 - 1;
   type Phase is (Empty, Writable, Sealed, Held, Uncertain);
   type State is record
      Mode : Phase := Empty;
      Epoch, Ticket : Natural := 0;
   end record;
   function Valid (S : State) return Boolean is
     ((if S.Mode = Held then S.Epoch in Identity and S.Ticket in Identity
       elsif S.Mode /= Uncertain then S.Epoch = 0 and S.Ticket = 0));
   procedure Allocate (S : in out State)
     with Pre => Valid (S) and S.Mode = Empty,
       Post => Valid (S) and S = (Writable, 0, 0);
   procedure Seal (S : in out State; Protection_OK : Boolean)
     with Pre => Valid (S) and S.Mode = Writable,
       Post => Valid (S) and S = (if Protection_OK then (Sealed, 0, 0) else (Uncertain, 0, 0));
   procedure Borrow (S : in out State; Epoch, Ticket : Identity)
     with Pre => Valid (S) and S.Mode = Sealed,
       Post => Valid (S) and S = (Held, Epoch, Ticket);
   procedure Retire (S : in out State; Epoch, Ticket : Identity; Confirmed : Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       (if S.Mode'Old = Held and S.Epoch'Old = Epoch and S.Ticket'Old = Ticket and Confirmed
        then S = (Sealed, 0, 0) else S = S'Old);
   procedure Reopen (S : in out State; Protection_OK : Boolean)
     with Pre => Valid (S) and S.Mode = Sealed,
       Post => Valid (S) and S = (if Protection_OK then (Writable, 0, 0) else (Uncertain, 0, 0));
   procedure Quarantine (S : in out State)
     with Pre => Valid (S), Post => Valid (S) and S.Mode = Uncertain and
       S.Epoch = S.Epoch'Old and S.Ticket = S.Ticket'Old;
   procedure Release (S : in out State; Readers_Retired, Released : Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       (if Readers_Retired and Released then S = (Empty, 0, 0)
        else S.Mode = Uncertain and S.Epoch = S.Epoch'Old and S.Ticket = S.Ticket'Old);
end Client_Frame_State;
