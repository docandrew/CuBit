with Interfaces;
--  Three fixed backing allocations, bound by the caller to a fresh epoch.
--  Evidence of GPU/display quiescence is supplied by audited backend adapters.
package Compositor_Pool with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_64;
   subtype ID is Interfaces.Unsigned_64;
   subtype Live_ID is ID range 1 .. ID'Last;
   subtype Slot is Natural range 0 .. 3;
   subtype Live_Slot is Slot range 1 .. 3;
   type Ticket is record
      Buffer : Slot := 0;
      Epoch, Serial : ID := 0;
   end record;
   None : constant Ticket := (Buffer => 0, Epoch => 0, Serial => 0);
   type State is private;
   function Valid (S : State) return Boolean;
   function Faulted (S : State) return Boolean;
   function Epoch (S : State) return ID;
   function Last_Serial (S : State) return ID;
   function Writer (S : State) return Ticket;
   function Ready (S : State) return Ticket;
   function Displayed (S : State) return Ticket;
   -- Displayed is submitted/pending; Front remains held after a direct latch.
   function Front (S : State) return Ticket;
   -- Completed image held by a transfer/CPU reader, NOT a display latch.
   function Readback (S : State) return Ticket;
   function Has_Free (S : State) return Boolean;
   function Rendering (S : State) return Boolean;
   function Writable (S : State; T : Ticket) return Boolean is
     (not Faulted (S) and T.Buffer /= 0 and T = Writer (S) and not Rendering (S))
     with Post => (if Valid (S) and Writable'Result then
       T.Buffer /= Ready (S).Buffer and T.Buffer /= Displayed (S).Buffer and
       T.Buffer /= Front (S).Buffer and T.Buffer /= Readback (S).Buffer);
   function Open (New_Epoch : Live_ID) return State
     with Post => Valid (Open'Result) and not Faulted (Open'Result) and
       Epoch (Open'Result) = New_Epoch and Last_Serial (Open'Result) = 0 and
       Writer (Open'Result) = None and Ready (Open'Result) = None and
       Displayed (Open'Result) = None and Front (Open'Result) = None and
       Readback (Open'Result) = None;
   -- Busy returns None unchanged. Exhausted/unopened pools become faulted.
   -- Replace_Ready is opt-in only when newer scene work will be rendered.
   -- If all slots are held, reclaim the quiescent ready frame, never front or
   -- pending. Ordinary eager writer acquisition must leave this False.
   procedure Acquire
     (S : in out State; T : out Ticket; Replace_Ready : Boolean := False)
     with Pre => Valid (S), Post => Valid (S) and
       Readback (S) = Readback (S'Old) and
       (T /= None) = (not Faulted (S'Old) and Writer (S'Old) = None and
         Epoch (S'Old) /= 0 and Last_Serial (S'Old) < ID'Last and
         (Has_Free (S'Old) or (Replace_Ready and Ready (S'Old) /= None))) and
       (if T = None and not Faulted (S) then S = S'Old) and
       Ready (S) = (if T /= None and not Has_Free (S'Old) then None else Ready (S'Old)) and
       Displayed (S) = Displayed (S'Old) and
       (if T /= None and not Has_Free (S'Old) then T.Buffer = Ready (S'Old).Buffer) and
       Front (S) = Front (S'Old) and Epoch (S) = Epoch (S'Old) and
       (if T /= None then Writable (S, T) and T.Serial > Last_Serial (S'Old) and
          T.Serial = Last_Serial (S) and T.Epoch = Epoch (S)) and
       (if Faulted (S'Old) then Faulted (S) and T = None) and
       (if not Faulted (S'Old) and Writer (S'Old) = None and
          Epoch (S'Old) /= 0 and Last_Serial (S'Old) < ID'Last and not Has_Free (S'Old) and
          not (Replace_Ready and Ready (S'Old) /= None)
        then S = S'Old);
   procedure Start_Render (S : in out State; T : Ticket)
     with Pre => Valid (S), Post => Valid (S) and
       Readback (S) = Readback (S'Old) and
       Front (S) = Front (S'Old) and Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       Writer (S) = Writer (S'Old) and Ready (S) = Ready (S'Old) and
       Displayed (S) = Displayed (S'Old) and not Writable (S, T) and
       (if Writable (S'Old, T) then Rendering (S) and not Faulted (S)
        else Faulted (S));
   type Render_Outcome is (Completed, Failed_Quiescent, Unknown);
   procedure Finish_Render (S : in out State; T : Ticket; Result : Render_Outcome)
     with Pre => Valid (S), Post => Valid (S) and
       Readback (S) = Readback (S'Old) and
       Front (S) = Front (S'Old) and Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       Displayed (S) = Displayed (S'Old) and
       (if not Faulted (S'Old) and Rendering (S'Old) and
           T = Writer (S'Old) and Result /= Unknown
        then Writer (S) = None and not Rendering (S) and not Faulted (S) and
          Ready (S) = (if Result = Completed then T else Ready (S'Old))
        else Faulted (S) and Writer (S) = Writer (S'Old) and
          Ready (S) = Ready (S'Old));
   -- Drop a completed unpublished candidate, for example during output disable.
   -- This never releases a writer, pending presentation, or visible front.
   procedure Discard_Ready (S : in out State; T : Ticket)
     with Pre => Valid (S), Post => Valid (S) and
       Readback (S) = Readback (S'Old) and
       Front (S) = Front (S'Old) and Displayed (S) = Displayed (S'Old) and
       Writer (S) = Writer (S'Old) and Rendering (S) = Rendering (S'Old) and
       Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       (if not Faulted (S'Old) and T /= None and T = Ready (S'Old)
        then Ready (S) = None and not Faulted (S)
        else Ready (S) = Ready (S'Old) and Faulted (S));
   -- One outstanding display frame. No deep FIFO; newest completed frame wins.
   procedure Present (S : in out State; T : out Ticket)
     with Pre => Valid (S), Post => Valid (S) and
       Readback (S) = Readback (S'Old) and
       Faulted (S) = Faulted (S'Old) and
       Front (S) = Front (S'Old) and Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       Writer (S) = Writer (S'Old) and Rendering (S) = Rendering (S'Old) and
       (if not Faulted (S'Old) and Displayed (S'Old) = None and Ready (S'Old) /= None
        then T = Ready (S'Old) and Displayed (S) = T and Ready (S) = None
        else T = None and Displayed (S) = Displayed (S'Old) and Ready (S) = Ready (S'Old));
   procedure Retire_Display (S : in out State; T : Ticket; Released : Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       Readback (S) = Readback (S'Old) and
       Front (S) = Front (S'Old) and Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       Writer (S) = Writer (S'Old) and Ready (S) = Ready (S'Old) and
       (if not Faulted (S'Old) and T /= None and T = Displayed (S'Old) and Released
        then Displayed (S) = None and not Faulted (S)
        else Displayed (S) = Displayed (S'Old) and Faulted (S));
   -- Adapter evidence must establish both the new latch and retirement of the
   -- exact prior front. Neither command acceptance nor a timeout is sufficient.
   -- A backend with separate signals retains both tickets until it has both.
   procedure Latch_Display
     (S : in out State; T, Previous : Ticket; Confirmed : Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       Readback (S) = Readback (S'Old) and
       Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       Writer (S) = Writer (S'Old) and Ready (S) = Ready (S'Old) and
       Rendering (S) = Rendering (S'Old) and
       (if not Faulted (S'Old) and T /= None and T = Displayed (S'Old) and
           Previous = Front (S'Old) and Confirmed
        then Front (S) = T and Displayed (S) = None and not Faulted (S)
        else Front (S) = Front (S'Old) and Displayed (S) = Displayed (S'Old) and Faulted (S));
   -- Final visible target needs explicit quiescent disable/retirement evidence.
   procedure Retire_Front (S : in out State; T : Ticket; Confirmed : Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       Readback (S) = Readback (S'Old) and
       Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       Writer (S) = Writer (S'Old) and Ready (S) = Ready (S'Old) and
       Displayed (S) = Displayed (S'Old) and Rendering (S) = Rendering (S'Old) and
       (if not Faulted (S'Old) and T /= None and T = Front (S'Old) and Confirmed
        then Front (S) = None and not Faulted (S)
        else Front (S) = Front (S'Old) and Faulted (S));

   -- One outstanding completed-target reader, sharing the same three slots.
   -- Busy leaves state unchanged. This consumes Ready, never Displayed/Front.
   procedure Take_Readback (S : in out State; T : out Ticket)
     with Pre => Valid (S), Post => Valid (S) and
       Faulted (S) = Faulted (S'Old) and
       Writer (S) = Writer (S'Old) and Rendering (S) = Rendering (S'Old) and
       Displayed (S) = Displayed (S'Old) and Front (S) = Front (S'Old) and
       Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       (if not Faulted (S'Old) and Readback (S'Old) = None and Ready (S'Old) /= None
        then T = Ready (S'Old) and Readback (S) = T and Ready (S) = None
        else T = None and S = S'Old);
   -- Evidence belongs to trusted adapters: exact transfer completion and all
   -- CPU readers drained. Neither timeout nor command acceptance is evidence.
   -- Invalid/uncertain retirement faults and retains the target.
   procedure Retire_Readback
     (S : in out State; T : Ticket; Transfer_Complete, CPU_Drained : Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       Writer (S) = Writer (S'Old) and Ready (S) = Ready (S'Old) and
       Rendering (S) = Rendering (S'Old) and
       Displayed (S) = Displayed (S'Old) and Front (S) = Front (S'Old) and
       Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       (if not Faulted (S'Old) and T /= None and T = Readback (S'Old) and
           Transfer_Complete and CPU_Drained
        then Readback (S) = None and not Faulted (S)
        else Readback (S) = Readback (S'Old) and Faulted (S));

private
   type State is record
      Generation, Sequence : ID := 0;
      Failed, GPU_Busy : Boolean := False;
      W, R, D, F, B : Ticket := None;
   end record;
   function Faulted (S : State) return Boolean is (S.Failed);
   function Epoch (S : State) return ID is (S.Generation);
   function Last_Serial (S : State) return ID is (S.Sequence);
   function Writer (S : State) return Ticket is (S.W);
   function Ready (S : State) return Ticket is (S.R);
   function Displayed (S : State) return Ticket is (S.D);
   function Front (S : State) return Ticket is (S.F);
   function Readback (S : State) return Ticket is (S.B);
   function Free (S : State; B : Live_Slot) return Boolean is
     (S.W.Buffer /= B and S.R.Buffer /= B and S.D.Buffer /= B and
      S.F.Buffer /= B and S.B.Buffer /= B);
   function Has_Free (S : State) return Boolean is
     (Free (S, 1) or Free (S, 2) or Free (S, 3));
   function Rendering (S : State) return Boolean is (S.GPU_Busy);
   function Ticket_Valid (S : State; T : Ticket) return Boolean is
     (T = None or else (T.Buffer /= 0 and T.Epoch = S.Generation and
       T.Epoch /= 0 and T.Serial /= 0 and T.Serial <= S.Sequence));
   function Distinct (A, B : Ticket) return Boolean is
     (A = None or B = None or A.Buffer /= B.Buffer);
   function Valid (S : State) return Boolean is
     (Ticket_Valid (S, S.B) and
      Distinct (S.B, S.W) and Distinct (S.B, S.R) and
      Distinct (S.B, S.D) and Distinct (S.B, S.F) and
      Ticket_Valid (S, S.W) and Ticket_Valid (S, S.R) and Ticket_Valid (S, S.D) and Ticket_Valid (S, S.F) and
      Distinct (S.W, S.R) and Distinct (S.W, S.D) and Distinct (S.R, S.D) and
      Distinct (S.W, S.F) and Distinct (S.R, S.F) and Distinct (S.D, S.F) and
      (if S.GPU_Busy then S.W /= None) and
      (if S.W /= None then S.W.Serial = S.Sequence and
         (if S.R /= None then S.R.Serial < S.W.Serial) and
         (if S.D /= None then S.D.Serial < S.W.Serial)) and
      (if S.R /= None and S.D /= None then S.D.Serial < S.R.Serial) and
      (if S.F /= None then
         (if S.W /= None then S.F.Serial < S.W.Serial) and
         (if S.R /= None then S.F.Serial < S.R.Serial) and
         (if S.D /= None then S.F.Serial < S.D.Serial)));
end Compositor_Pool;
