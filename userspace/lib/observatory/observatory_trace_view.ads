with Compositor_Trace_Framing;
package Observatory_Trace_View with Pure, SPARK_Mode is
   package F renames Compositor_Trace_Framing;
   package A renames F.A;
   subtype Word is A.Word;
   use type Word, F.Phase;
   Capacity : constant := 64;
   subtype Count is Natural range 0 .. Capacity;
   subtype Index is Positive range 1 .. Capacity;
   subtype Page_Number is Natural range 0 .. F.Maximum_Events / Capacity - 1;
   type State is private;
   function Ready (S : State) return Boolean;
   function Length (S : State) return Count;
   function Total (S : State) return F.Count;
   function Page (S : State) return Page_Number;
   function Lossy (S : State) return Boolean;
   function Budget_Limited (S : State) return Boolean;
   function Observer_Failed (S : State) return Boolean;
   function Value_At (S : State; Position : Index) return A.S.Capture
     with Pre => Position <= Length (S), Post => A.Valid (Value_At'Result);
   -- A page becomes visible only after the whole archive validates through EOF.
   procedure Start (S : out State; Header : A.Chunk; Wanted : Page_Number)
     with Post => not Ready (S) and Length (S) = 0 and Page (S) = Wanted;
   procedure Feed (S : in out State; Data : A.Chunk)
     with Post => not Ready (S) and Length (S) = 0 and Page (S) = Page (S'Old);
   procedure Finish (S : in out State; Trailing_Bytes : Natural)
     with Pre => Trailing_Bytes < 256,
       Post => Page (S) = Page (S'Old) and
         (if not Ready (S) then Length (S) = 0);
private
   type Values is array (Index) of A.S.Capture;
   type State is record
      Archive : F.State;
      Rows : Values;
      Used : Count := 0;
      Selected : Page_Number := 0;
      Saw_Loss, Hit_Budget, Saw_Failure, EOF_Seen : Boolean := False;
   end record with Dynamic_Predicate =>
     (for all I in 1 .. Used => A.Valid (Rows (I)));
   function Ready (S : State) return Boolean is (S.EOF_Seen and F.Status (S.Archive) = F.Complete);
   function Length (S : State) return Count is (if Ready (S) then S.Used else 0);
   function Total (S : State) return F.Count is (if Ready (S) then F.Events (S.Archive) else 0);
   function Page (S : State) return Page_Number is (S.Selected);
   function Lossy (S : State) return Boolean is (Ready (S) and S.Saw_Loss);
   function Budget_Limited (S : State) return Boolean is (Ready (S) and S.Hit_Budget);
   function Observer_Failed (S : State) return Boolean is (Ready (S) and S.Saw_Failure);
   function Value_At (S : State; Position : Index) return A.S.Capture is (S.Rows (Position));
end Observatory_Trace_View;
