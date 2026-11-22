with Interfaces;
with Observatory_Trace_View;
package Observatory_Archive_Stream with Pure, SPARK_Mode is
   package V renames Observatory_Trace_View;
   package A renames V.A;
   subtype Byte is Interfaces.Unsigned_8;
   Maximum_Bytes : constant := (V.F.Maximum_Events + 2) * 256;
   type State is private;
   function Ready (S : State) return Boolean;
   function Length (S : State) return Natural;
   function Context (S : State) return V.State with
     Post => V.Ready (Context'Result) = Ready (S);
   procedure Start (S : out State; Page : V.Page_Number)
     with Post => not Ready (S) and Length (S) = 0;
   -- Fragment boundaries are irrelevant. A zero-byte read, not a short read,
   -- establishes EOF in the foreign file adapter.
   procedure Append (S : in out State; Value : Byte)
     with Post => not Ready (S) and Length (S) <= Maximum_Bytes;
   procedure Finish (S : in out State);
private
   type State is record
      View : V.State;
      Chunk : A.Chunk := [others => 0];
      Pending : Natural range 0 .. 255 := 0;
      Seen : Natural range 0 .. Maximum_Bytes := 0;
      First : Boolean := True;
      Ended : Boolean := False;
   end record with Dynamic_Predicate => (if V.Ready (View) then Ended);
   function Ready (S : State) return Boolean is (S.Ended and V.Ready (S.View));
   function Length (S : State) return Natural is (S.Seen);
   function Context (S : State) return V.State is (S.View);
end Observatory_Archive_Stream;
