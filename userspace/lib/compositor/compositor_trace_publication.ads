with Compositor_Trace_Wire;
generic
   Sequence_Limit : Compositor_Trace_Wire.Word := Compositor_Trace_Wire.Word'Last;
package Compositor_Trace_Publication with Pure, SPARK_Mode is
   package W renames Compositor_Trace_Wire;
   use type W.Word, W.Event;
   function Identified (Value : W.Event; ID : W.Word) return W.Event is
     (case Value.Kind is
         when W.Input_Event => (W.Input_Event, ID, Value.Input),
         when W.Source_Event => (W.Source_Event, ID, Value.Source),
         when W.Render_Event => (W.Render_Event, ID, Value.Render),
         when W.Frame_Event => (W.Frame_Event, ID, Value.Frame));
   function Valid_Content (Value : W.Event) return Boolean is
     (W.Valid (Identified (Value, 1)));
   type Prepared is record
      Ready : Boolean := False;
      Value : W.Event;
   end record;
   type State is private;
   function Following (S : State) return W.Word;
   function Invalid (S : State) return W.Word;
   function Refused (S : State) return W.Word;
   function Unsupported (S : State) return W.Word;
   function Increment (Value : W.Word) return W.Word is
     (if Value = W.Word'Last then Value else Value + 1);
   --  Caller-provided Event_ID is ignored. Every valid attempt consumes its
   --  own identity before transport admission, so refusal never reuses an ID.
   --  Sequence_Limit is reserved; exhaustion never wraps or emits another ID.
   procedure Prepare (S : in out State; Value : W.Event; Result : out Prepared)
     with Post =>
       Result.Ready = (Valid_Content (Value) and Following (S'Old) < Sequence_Limit) and then
       Following (S) = (if Result.Ready then Following (S'Old) + 1 else Following (S'Old)) and then
       Invalid (S) = (if Valid_Content (Value) then Invalid (S'Old)
                     else Increment (Invalid (S'Old))) and then
       Refused (S) = (if Valid_Content (Value) and not Result.Ready
                     then Increment (Refused (S'Old)) else Refused (S'Old)) and then
       Unsupported (S) = Unsupported (S'Old) and then
       (if Result.Ready then W.Valid (Result.Value) and then
          Result.Value = Identified (Value, Following (S'Old)));
   procedure Note_Refusal (S : in out State)
     with Post => Refused (S) = Increment (Refused (S'Old)) and
       Following (S) = Following (S'Old) and Invalid (S) = Invalid (S'Old) and
       Unsupported (S) = Unsupported (S'Old);
   procedure Note_Unsupported (S : in out State)
     with Post => Unsupported (S) = Increment (Unsupported (S'Old)) and
       Following (S) = Following (S'Old) and Invalid (S) = Invalid (S'Old) and
       Refused (S) = Refused (S'Old);
private
   type State is record
      Next : W.Word := 1;
      Bad, Lost, Unsupported_Count : W.Word := 0;
   end record with Dynamic_Predicate => State.Next /= 0;
   function Following (S : State) return W.Word is (S.Next);
   function Invalid (S : State) return W.Word is (S.Bad);
   function Refused (S : State) return W.Word is (S.Lost);
   function Unsupported (S : State) return W.Word is (S.Unsupported_Count);
end Compositor_Trace_Publication;
