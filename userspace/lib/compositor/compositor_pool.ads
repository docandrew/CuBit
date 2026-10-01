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
   function Rendering (S : State) return Boolean;
   function Writable (S : State; T : Ticket) return Boolean is
     (not Faulted (S) and T.Buffer /= 0 and T = Writer (S) and not Rendering (S))
     with Post => (if Valid (S) and Writable'Result then
       T.Buffer /= Ready (S).Buffer and T.Buffer /= Displayed (S).Buffer);
   function Open (New_Epoch : Live_ID) return State
     with Post => Valid (Open'Result) and not Faulted (Open'Result) and
       Epoch (Open'Result) = New_Epoch and Last_Serial (Open'Result) = 0 and
       Writer (Open'Result) = None and Ready (Open'Result) = None and
       Displayed (Open'Result) = None;
   -- Busy returns None unchanged. Exhausted/unopened pools become faulted.
   procedure Acquire (S : in out State; T : out Ticket)
     with Pre => Valid (S), Post => Valid (S) and
       (T /= None) = (not Faulted (S'Old) and Writer (S'Old) = None and
         Epoch (S'Old) /= 0 and Last_Serial (S'Old) < ID'Last) and
       Ready (S) = Ready (S'Old) and Displayed (S) = Displayed (S'Old) and
       Epoch (S) = Epoch (S'Old) and
       (if T /= None then Writable (S, T) and T.Serial > Last_Serial (S'Old) and
          T.Serial = Last_Serial (S) and T.Epoch = Epoch (S)) and
       (if Faulted (S'Old) then Faulted (S) and T = None);
   procedure Start_Render (S : in out State; T : Ticket)
     with Pre => Valid (S), Post => Valid (S) and
       Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       Writer (S) = Writer (S'Old) and Ready (S) = Ready (S'Old) and
       Displayed (S) = Displayed (S'Old) and not Writable (S, T) and
       (if Writable (S'Old, T) then Rendering (S) and not Faulted (S)
        else Faulted (S));
   type Render_Outcome is (Completed, Failed_Quiescent, Unknown);
   procedure Finish_Render (S : in out State; T : Ticket; Result : Render_Outcome)
     with Pre => Valid (S), Post => Valid (S) and
       Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       Displayed (S) = Displayed (S'Old) and
       (if not Faulted (S'Old) and Rendering (S'Old) and
           T = Writer (S'Old) and Result /= Unknown
        then Writer (S) = None and not Rendering (S) and not Faulted (S) and
          Ready (S) = (if Result = Completed then T else Ready (S'Old))
        else Faulted (S) and Writer (S) = Writer (S'Old) and
          Ready (S) = Ready (S'Old));
   -- One outstanding display frame. No deep FIFO; newest completed frame wins.
   procedure Present (S : in out State; T : out Ticket)
     with Pre => Valid (S), Post => Valid (S) and
       Faulted (S) = Faulted (S'Old) and
       Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       Writer (S) = Writer (S'Old) and Rendering (S) = Rendering (S'Old) and
       (if not Faulted (S'Old) and Displayed (S'Old) = None and Ready (S'Old) /= None
        then T = Ready (S'Old) and Displayed (S) = T and Ready (S) = None
        else T = None and Displayed (S) = Displayed (S'Old) and Ready (S) = Ready (S'Old));
   procedure Retire_Display (S : in out State; T : Ticket; Released : Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       Epoch (S) = Epoch (S'Old) and Last_Serial (S) = Last_Serial (S'Old) and
       Writer (S) = Writer (S'Old) and Ready (S) = Ready (S'Old) and
       (if not Faulted (S'Old) and T /= None and T = Displayed (S'Old) and Released
        then Displayed (S) = None and not Faulted (S)
        else Displayed (S) = Displayed (S'Old) and Faulted (S));
private
   type State is record
      Generation, Sequence : ID := 0;
      Failed, GPU_Busy : Boolean := False;
      W, R, D : Ticket := None;
   end record;
   function Faulted (S : State) return Boolean is (S.Failed);
   function Epoch (S : State) return ID is (S.Generation);
   function Last_Serial (S : State) return ID is (S.Sequence);
   function Writer (S : State) return Ticket is (S.W);
   function Ready (S : State) return Ticket is (S.R);
   function Displayed (S : State) return Ticket is (S.D);
   function Rendering (S : State) return Boolean is (S.GPU_Busy);
   function Ticket_Valid (S : State; T : Ticket) return Boolean is
     (T = None or else (T.Buffer /= 0 and T.Epoch = S.Generation and
       T.Epoch /= 0 and T.Serial /= 0 and T.Serial <= S.Sequence));
   function Distinct (A, B : Ticket) return Boolean is
     (A = None or B = None or A.Buffer /= B.Buffer);
   function Valid (S : State) return Boolean is
     (Ticket_Valid (S, S.W) and Ticket_Valid (S, S.R) and Ticket_Valid (S, S.D) and
      Distinct (S.W, S.R) and Distinct (S.W, S.D) and Distinct (S.R, S.D) and
      (if S.GPU_Busy then S.W /= None) and
      (if S.W /= None then S.W.Serial = S.Sequence and
         (if S.R /= None then S.R.Serial < S.W.Serial) and
         (if S.D /= None then S.D.Serial < S.W.Serial)) and
      (if S.R /= None and S.D /= None then S.D.Serial < S.R.Serial));
end Compositor_Pool;
