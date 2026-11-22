generic
   Maximum_Generation : Positive := Positive'Last;
package Compositor_Surface_State with SPARK_Mode, Pure is
   subtype Generation is Natural range 0 .. Maximum_Generation;
   subtype Slot is Positive range 1 .. 2;
   type Phase is (Empty, Candidate, Visible, Retiring);
   type Buffer_Record is record
      Status : Phase := Empty;
      Epoch : Generation := 0;
      Ticket : Natural := 0;
   end record;
   type Entries is array (Slot) of Buffer_Record;
   type State is record
      Closing : Boolean := False;
      Requested : Generation := 0;
      Issued : Natural := 0;
      Buffers : Entries := (others => (others => <>));
   end record;
   function Valid (S : State) return Boolean is
     ((if S.Closing then
         (for all I in Slot => S.Buffers (I).Status in Empty | Retiring)) and then
      (for all I in Slot =>
         (if S.Buffers (I).Status = Empty then S.Buffers (I).Epoch = 0 and S.Buffers (I).Ticket = 0
          else S.Buffers (I).Epoch in 1 .. S.Requested and S.Buffers (I).Ticket in 1 .. S.Issued)) and then
      (S.Buffers (1).Ticket = 0 or S.Buffers (2).Ticket = 0 or
       S.Buffers (1).Ticket /= S.Buffers (2).Ticket) and then
      not (S.Buffers (1).Status = Visible and S.Buffers (2).Status = Visible) and then
      not (S.Buffers (1).Status = Candidate and S.Buffers (2).Status = Candidate));
   -- Called only for a changed logical extent/density configuration.
   -- Obsolete candidates enter retirement; keep their identities and the
   -- visible frame until actual readers release them. Never free on resize.
   -- Exhaustion is terminal for this surface identity; epochs never wrap.
   procedure Configure (S : in out State; Accepted : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and S.Closing = S.Closing'Old and
       S.Issued = S.Issued'Old and
       Accepted = (not S.Closing'Old and S.Requested'Old < Maximum_Generation) and
       (if Accepted then
          S.Requested = S.Requested'Old + 1 and
          (for all I in Slot =>
             S.Buffers (I).Epoch = S.Buffers'Old (I).Epoch and
             S.Buffers (I).Ticket = S.Buffers'Old (I).Ticket and
             S.Buffers (I).Status =
               (if S.Buffers'Old (I).Status = Candidate then Retiring
                else S.Buffers'Old (I).Status))
        else S = S'Old);
   -- Admission precedes acquisition. Failed acquisition calls Discard; it
   -- must still pass through retirement if foreign ownership is uncertain.
   procedure Stage (S : in out State; I : Slot; Epoch : Generation; Accepted : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and S.Closing = S.Closing'Old and S.Requested = S.Requested'Old and
       Accepted = (not S.Closing'Old and S.Issued'Old < Natural'Last and Epoch > 0 and Epoch = S.Requested'Old and
         S.Buffers'Old (I).Status = Empty and
         (for all J in Slot => S.Buffers'Old (J).Status /= Candidate)) and
       (for all J in Slot => (if J /= I then S.Buffers (J) = S.Buffers'Old (J))) and
       (if not Accepted then S = S'Old else
          S.Issued = S.Issued'Old + 1 and S.Buffers (I).Ticket = S.Issued and
          S.Buffers (I).Status = Candidate and S.Buffers (I).Epoch = Epoch and Epoch = S.Requested and Epoch > 0);
   procedure Present (S : in out State; I : Slot; Epoch : Generation; Ticket : Natural; Accepted : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and S.Closing = S.Closing'Old and S.Requested = S.Requested'Old and
       S.Issued = S.Issued'Old and
       Accepted = (not S.Closing'Old and Epoch > 0 and Epoch = S.Requested'Old and
         S.Buffers'Old (I).Status = Candidate and S.Buffers'Old (I).Epoch = Epoch and
         S.Buffers'Old (I).Ticket = Ticket) and
       (for all J in Slot => S.Buffers (J).Epoch = S.Buffers'Old (J).Epoch and
         S.Buffers (J).Ticket = S.Buffers'Old (J).Ticket) and
       (if not Accepted then S = S'Old else
          (for all J in Slot => (if J /= I then
            S.Buffers (J).Status = (if S.Buffers'Old (J).Status = Visible then Retiring else S.Buffers'Old (J).Status))) and
          S.Buffers (I).Status = Visible and S.Buffers (I).Epoch = Epoch and Epoch = S.Requested and Epoch > 0);
   procedure Discard (S : in out State; I : Slot; Ticket : Natural)
     with Pre => Valid (S), Post => Valid (S) and S.Closing = S.Closing'Old and S.Requested = S.Requested'Old and
       S.Issued = S.Issued'Old and
       (for all J in Slot => S.Buffers (J).Epoch = S.Buffers'Old (J).Epoch and
         S.Buffers (J).Ticket = S.Buffers'Old (J).Ticket) and
       (for all J in Slot => (if J /= I then S.Buffers (J) = S.Buffers'Old (J))) and
       (if S.Buffers'Old (I).Status = Candidate and S.Buffers'Old (I).Ticket = Ticket
        then S.Buffers (I).Status = Retiring else S = S'Old);
   -- Terminal destruction begins by withdrawing all publication eligibility.
   -- Keep identities until external readers and grants are actually retired.
   -- Repeated Close is harmless; this identity can never be configured again.
   procedure Close (S : in out State)
     with Pre => Valid (S), Post => Valid (S) and S.Closing and
       S.Requested = S.Requested'Old and S.Issued = S.Issued'Old and
       (for all I in Slot =>
          S.Buffers (I).Epoch = S.Buffers'Old (I).Epoch and
          S.Buffers (I).Ticket = S.Buffers'Old (I).Ticket and
          S.Buffers (I).Status =
            (if S.Buffers'Old (I).Status = Empty then Empty else Retiring));
   -- The caller supplies authoritative reader/grant retirement confirmation.
   procedure Retire (S : in out State; I : Slot; Ticket : Natural; Readers_Retired : Boolean)
     with Pre => Valid (S), Post => Valid (S) and S.Closing = S.Closing'Old and S.Requested = S.Requested'Old and
       S.Issued = S.Issued'Old and
       (for all J in Slot => (if J /= I then S.Buffers (J) = S.Buffers'Old (J))) and
       (if not Readers_Retired or S.Buffers'Old (I).Status /= Retiring or S.Buffers'Old (I).Ticket /= Ticket then S = S'Old
        else S.Buffers (I) = (Empty, 0, 0));
end Compositor_Surface_State;
