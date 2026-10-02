with Interfaces;
package CuBit.Backend_Targets with SPARK_Mode, Pure is
   subtype Buffer is Natural range 0 .. 1;
   subtype Identifier is Interfaces.Unsigned_64;
   use type Identifier;
   type Phase is (Unconfigured, Idle, Preparing, In_Flight, Failed);
   type State is private;
   function Current (S : State) return Phase;
   function Active (S : State) return Buffer;
   function Target (S : State) return Buffer is (1 - Active (S));
   function Token (S : State) return Identifier;
   function Valid (S : State) return Boolean;
   function Writable (S : State; B : Buffer) return Boolean is
     (Current (S) = Preparing and B = Target (S))
     with Post => (if Writable'Result then B /= Active (S));
   -- Audited adapter calls only after a successful, quiescent GPU clear.
   -- A failed/uncertain instance cannot be reopened by ordinary clear requests.
   procedure Cleared (S : in out State)
     with Pre => Valid (S), Post => Valid (S) and
       (if Current (S'Old) in Unconfigured | Idle then
          Current (S) = Idle and Active (S) = 0 and Token (S) = 0
        else Current (S) = Failed and Active (S) = Active (S'Old) and Token (S) = Token (S'Old));
   procedure Prepare (S : in out State; Accepted : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       Accepted = (Current (S'Old) = Idle) and Active (S) = Active (S'Old) and
       Token (S) = Token (S'Old) and
       (if Accepted then Current (S) = Preparing and Writable (S, Target (S)) else S = S'Old);
   -- Seal before the foreign submission, so neither target is writable during IO.
   procedure Seal (S : in out State; ID : Identifier)
     with Pre => Valid (S), Post => Valid (S) and Active (S) = Active (S'Old) and
       not Writable (S, 0) and not Writable (S, 1) and
       (if Current (S'Old) = Preparing and ID /= 0 then Current (S) = In_Flight and Token (S) = ID
        else Current (S) = Failed and Token (S) = Token (S'Old));
   -- Published includes authenticated matching backend completion and old-target
   -- quiescence, supplied by the adapter. This does not itself prove a GPU fence.
   procedure Complete (S : in out State; ID : Identifier; Published : Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       (if Current (S'Old) = In_Flight and ID = Token (S'Old) and Published then
          Current (S) = Idle and Active (S) = Target (S'Old) and Token (S) = 0
        else Current (S) = Failed and Active (S) = Active (S'Old) and Token (S) = Token (S'Old));
   procedure Quarantine (S : in out State)
     with Pre => Valid (S), Post => Valid (S) and Current (S) = Failed and
       Active (S) = Active (S'Old) and Token (S) = Token (S'Old);
private
   type State is record
      Stage : Phase := Unconfigured;
      Front : Buffer := 0;
      Pending : Identifier := 0;
   end record;
   function Current (S : State) return Phase is (S.Stage);
   function Active (S : State) return Buffer is (S.Front);
   function Token (S : State) return Identifier is (S.Pending);
   function Valid (S : State) return Boolean is
     (if S.Stage = In_Flight then S.Pending /= 0
      elsif S.Stage /= Failed then S.Pending = 0 else True);
end CuBit.Backend_Targets;
