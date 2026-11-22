--  Latest-wins delivery of a position over an asynchronous channel with at
--  most one message in flight (pointer plane moves: Desktop to display.svc,
--  display.svc to the GPU driver). Pure; the caller performs the send.
--
--  Rule: a send is due exactly when nothing is in flight and the newest
--  wanted position differs from the last one sent. Proved below: after any
--  sequence of operations, either a message is in flight, or the newest
--  wanted position has been sent, or a send is due. So as long as the owner
--  sends whenever Due (after each Want, each completion and, for a send the
--  channel refused, its retry turn), the newest position is always
--  delivered eventually; intermediate positions may be skipped, never the
--  last one.
package CuBit.Newest_Wins with SPARK_Mode, Pure is
   type Position is record
      X, Y : Integer := 0;
   end record;

   type State is private;

   function Wanted (S : State) return Position;
   function Sent (S : State) return Position;
   function In_Flight (S : State) return Boolean;
   function Due (S : State) return Boolean is
     (not In_Flight (S) and then Wanted (S) /= Sent (S));
   --  The property: never stuck with an undelivered newest position.
   function Settled_Or_Due (S : State) return Boolean is
     (In_Flight (S) or else Wanted (S) = Sent (S) or else Due (S));

   --  A new position: replaces any position not yet sent.
   procedure Want (S : in out State; P : Position)
     with Post => Wanted (S) = P and then Sent (S) = Sent (S'Old) and then
                  In_Flight (S) = In_Flight (S'Old) and then Settled_Or_Due (S);

   --  The channel accepted a send of Wanted (S).
   procedure Submitted (S : in out State)
     with Pre => Due (S),
          Post => In_Flight (S) and then Sent (S) = Wanted (S) and then
                  Wanted (S) = Wanted (S'Old) and then Settled_Or_Due (S);

   --  The in-flight send completed (any outcome: a failed send is retried
   --  by forcing it due again, Resend).
   procedure Completed (S : in out State)
     with Post => not In_Flight (S) and then Wanted (S) = Wanted (S'Old) and then
                  Sent (S) = Sent (S'Old) and then Settled_Or_Due (S) and then
                  Due (S) = (Wanted (S) /= Sent (S));

   --  Forget what was sent (the receiver lost it, or a new request was
   --  created): the newest position is due again once nothing is in flight.
   procedure Resend (S : in out State)
     with Post => Wanted (S) = Wanted (S'Old) and then In_Flight (S) = In_Flight (S'Old) and then
                  Sent (S) /= Wanted (S) and then Settled_Or_Due (S);
private
   type State is record
      Want_P, Sent_P : Position;
      Flight : Boolean := False;
   end record;
   function Wanted (S : State) return Position is (S.Want_P);
   function Sent (S : State) return Position is (S.Sent_P);
   function In_Flight (S : State) return Boolean is (S.Flight);
end CuBit.Newest_Wins;
