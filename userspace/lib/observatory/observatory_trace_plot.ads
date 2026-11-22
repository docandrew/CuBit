with Observatory_Trace_View;
with Observatory_History;
package Observatory_Trace_Plot with SPARK_Mode is
   package V renames Observatory_Trace_View;
   package A renames V.A;
   subtype Word is A.Word;
   use type Word;
   subtype Lane is Natural range 0 .. 4;
   type Interval is record
      Row : Lane := 0;
      First, Last : Word := 0;
   end record;
   function Describe (Value : A.S.Capture) return Interval with
     Pre => A.Valid (Value), Post => Describe'Result.First <= Describe'Result.Last;
   type Bounds is record
      First, Last : Word := 0;
   end record;
   function Extent (View : V.State) return Bounds with
     Post => Extent'Result.First <= Extent'Result.Last and then
       (for all I in 1 .. V.Length (View) =>
          Extent'Result.First <= Describe (V.Value_At (View, I)).First and
          Describe (V.Value_At (View, I)).Last <= Extent'Result.Last);
   subtype Width is Observatory_History.Positive_Height;
   type Bar is record
      Left : Natural range 0 .. 511 := 0;
      Pixels : Positive range 1 .. 512 := 1;
   end record;
   function Project (Value : Interval; Window : Bounds; Size : Width) return Bar
     with Pre => Window.First <= Value.First and Value.First <= Value.Last and
                 Value.Last <= Window.Last,
       Post => Project'Result.Left + Project'Result.Pixels <= Size;
end Observatory_Trace_Plot;
