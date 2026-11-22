with Compositor_Input_Queue;
package Client_Input_Channel_Policy with SPARK_Mode, Pure is
   package IQ renames Compositor_Input_Queue;
   use type IQ.Word;
   type Phase is (Fresh, Preparing, Ready, Disabled);
   type State is private;
   function Mode (S : State) return Phase;
   function Next_Identity (S : State) return IQ.Word;
   function Create (First : IQ.Word := 1) return State
     with Post => Mode (Create'Result) = Fresh and Next_Identity (Create'Result) = First;
   -- Exactly one setup attempt per process-lifetime state; no retry can
   -- allocate another page after failure or uncertain ownership.
   procedure Begin_Setup (S : in out State; Needed : out Boolean)
     with Post => Needed = (Mode (S'Old) = Fresh) and then
       Mode (S) = (if Needed then Preparing else Mode (S'Old)) and then
       Next_Identity (S) = Next_Identity (S'Old);
   procedure Finish_Setup (S : in out State; OK : Boolean)
     with Pre => Mode (S) = Preparing,
       Post => Mode (S) = (if OK then Ready else Disabled) and then
         Next_Identity (S) = Next_Identity (S'Old);
   procedure Disable (S : in out State)
     with Post => Mode (S) = Disabled and Next_Identity (S) = Next_Identity (S'Old);
   procedure Reserve (S : in out State; Identity : out IQ.Word)
     with Post =>
       (if Mode (S'Old) /= Ready then Identity = 0 and S = S'Old
        elsif Next_Identity (S'Old) in 0 | IQ.Word'Last then
          Identity = 0 and Mode (S) = Disabled and Next_Identity (S) = Next_Identity (S'Old)
        else Identity = Next_Identity (S'Old) and Mode (S) = Ready and
          Next_Identity (S) = Next_Identity (S'Old) + 1);
private
   type State is record
      Current : Phase := Fresh;
      Next : IQ.Word := 1;
   end record;
   function Mode (S : State) return Phase is (S.Current);
   function Next_Identity (S : State) return IQ.Word is (S.Next);
   function Create (First : IQ.Word := 1) return State is ((Fresh, First));
end Client_Input_Channel_Policy;
