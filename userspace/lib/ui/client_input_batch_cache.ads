with Compositor_Input_Batch_Wire;
with Compositor_Input_Protocol;
package Client_Input_Batch_Cache with SPARK_Mode, Pure is
   package W renames Compositor_Input_Batch_Wire;
   package P renames Compositor_Input_Protocol;
   package DP renames P.DP;
   use type W.Word;
   use type DP.Status_Code;
   type State is private;
   function Remaining (S : State) return W.B.Count;
   function Expected_After (S : State) return W.Word;
   function Bound_Surface (S : State) return W.Word;
   procedure Load
     (S : in out State; Page : W.Snapshot_Words; Receipt : DP.Wire_Message;
      Surface, Request : W.Identity; After : W.Word; Accepted : out Boolean)
   with Post =>
     (if not Accepted then S = S'Old else
        Remaining (S'Old) = 0 and then Bound_Surface (S) = Surface
        and then Expected_After (S) = After);
   procedure Take
     (S : in out State; Surface : W.Identity; After : W.Word;
      Value : out DP.Input_Result)
   with Pre => not Value'Constrained,
     Post =>
     (if Value.Status = DP.Success then
        Remaining (S'Old) > 0 and then Remaining (S) = Remaining (S'Old) - 1
        and then Bound_Surface (S) = Surface and then
        Value.Value.Serial > After and then Expected_After (S) = Value.Value.Serial
      else S = S'Old);
   -- Metadata only: no grant or allocation lifetime is hidden in this cache.
   procedure Clear (S : out State)
     with Post => Remaining (S) = 0 and Expected_After (S) = 0 and Bound_Surface (S) = 0;
private
   type State is record
      Stored : W.B.Batch;
      Used : W.B.Count := 0;
      Surface, After : W.Word := 0;
   end record;
   function Remaining (S : State) return W.B.Count is
     (if S.Used < S.Stored.Length then S.Stored.Length - S.Used else 0);
   function Expected_After (S : State) return W.Word is (S.After);
   function Bound_Surface (S : State) return W.Word is (S.Surface);
end Client_Input_Batch_Cache;
