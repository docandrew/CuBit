with Interfaces;
-- A handled-input watermark is a lower bound on application state included in
-- a frame, not proof that that input changed pixels or was physically visible.
package Client_Input_Provenance with SPARK_Mode, Pure is
   subtype Serial is Interfaces.Unsigned_64;
   use type Serial;
   type State is private;
   function Handled (S : State) return Serial;
   function Active (S : State) return Serial;
   function Painting (S : State) return Boolean;
   function Frozen (S : State) return Serial;
   function Last_Published (S : State) return Serial;
   procedure Begin_Event (S : in out State; ID : Serial; Accepted : out Boolean)
     with Post => (Accepted =
       (not Painting (S'Old) and Active (S'Old) = 0 and ID > Handled (S'Old))) and
       (if Accepted then Active (S) = ID and Handled (S) = Handled (S'Old) and
          Painting (S) = Painting (S'Old) and Frozen (S) = Frozen (S'Old) and
          Last_Published (S) = Last_Published (S'Old)
        else S = S'Old);
   procedure Finish_Event (S : in out State; ID : Serial; Accepted : out Boolean)
     with Post => (Accepted = (ID /= 0 and Active (S'Old) = ID)) and
       (if Accepted then Handled (S) = ID and Active (S) = 0 and
          Painting (S) = Painting (S'Old) and Frozen (S) = Frozen (S'Old) and
          Last_Published (S) = Last_Published (S'Old)
        else S = S'Old);
   procedure Begin_Paint (S : in out State; Accepted : out Boolean)
     with Post => (Accepted = not Painting (S'Old)) and
       (if Accepted then Painting (S) and Frozen (S) = Handled (S'Old) and
          Handled (S) = Handled (S'Old) and Active (S) = Active (S'Old) and
          Last_Published (S) = Last_Published (S'Old)
        else S = S'Old);
   -- Zero means unknown/no completed input. Close the capture on every result,
   -- including rejection/cancellation/uncertainty; retry starts a fresh paint.
   procedure End_Paint
     (S : in out State; Published : Boolean; Watermark : out Serial)
     with Post => Watermark =
       (if Painting (S'Old) and Published then Frozen (S'Old) else 0) and
       not Painting (S) and Frozen (S) = 0 and
       Handled (S) = Handled (S'Old) and Active (S) = Active (S'Old) and
       Last_Published (S) =
         (if Painting (S'Old) and Published then Frozen (S'Old)
          else Last_Published (S'Old)) and
       Last_Published (S) >= Last_Published (S'Old);
private
   type State is record
      Completed, In_Progress, Captured, Published_Input : Serial := 0;
      Paint_Open : Boolean := False;
   end record with Dynamic_Predicate =>
     Captured <= Completed and Published_Input <= Completed and
     (In_Progress = 0 or In_Progress > Completed) and
     (Paint_Open or Captured = 0) and
     (if Paint_Open then Captured >= Published_Input);
   function Handled (S : State) return Serial is (S.Completed);
   function Active (S : State) return Serial is (S.In_Progress);
   function Painting (S : State) return Boolean is (S.Paint_Open);
   function Frozen (S : State) return Serial is (S.Captured);
   function Last_Published (S : State) return Serial is (S.Published_Input);
end Client_Input_Provenance;
