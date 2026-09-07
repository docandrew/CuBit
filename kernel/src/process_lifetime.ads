-- CPU execution presence is independent of scheduling/IPC queue state.
package Process_Lifetime with SPARK_Mode => On is
    type State is private;
    Initial_State : constant State;
    function Executing (S : State) return Boolean;
    function Closing (S : State) return Boolean;
    function Can_Run (S : State) return Boolean;
    function Can_Reap (S : State) return Boolean;
    function Reaping (S : State) return Boolean;
    function Retired (S : State) return Boolean;

    procedure Enter_CPU (S : in out State; Success : out Boolean)
      with Global => null,
      Post => (Success = Can_Run (S'Old)) and
        (if Success then Executing (S) and not Closing (S) else S = S'Old);
    procedure Leave_CPU (S : in out State; Success : out Boolean)
      with Global => null,
      Post => (Success = Executing (S'Old)) and
        (if Success then
             (if Closing (S'Old) then Can_Reap (S) else Can_Run (S))
         else S = S'Old);
    procedure Request_Stop (S : in out State)
      with Global => null,
      Post => Closing (S) and Executing (S) = Executing (S'Old) and
        (if Reaping (S'Old) or Retired (S'Old) then S = S'Old);
    procedure Claim_Reap (S : in out State; Success : out Boolean)
      with Global => null,
      Post => (Success = Can_Reap (S'Old)) and
        (if Success then Reaping (S) and not Executing (S) else S = S'Old);
    procedure Finish_Reap (S : in out State; Success : out Boolean)
      with Global => null,
      Post => (Success = Reaping (S'Old)) and
        (if Success then Retired (S) else S = S'Old);

    procedure Prove_Stop_Waits_For_CPU with Ghost, Global => null;
private
    -- Zero is the initialized, non-executing live state, including ELF
    -- construction before first dispatch. One atomic word can be sampled.
    type State is (Live_Stopped, Live_Running, Closing_Running,
                   Closing_Stopped, Reap_Claimed, Fully_Retired)
      with Size => 32;
    Initial_State : constant State := Live_Stopped;
    function Executing (S : State) return Boolean is
      (S in Live_Running | Closing_Running);
    function Closing (S : State) return Boolean is
      (S not in Live_Stopped | Live_Running);
    function Can_Run (S : State) return Boolean is (S = Live_Stopped);
    function Can_Reap (S : State) return Boolean is (S = Closing_Stopped);
    function Reaping (S : State) return Boolean is (S = Reap_Claimed);
    function Retired (S : State) return Boolean is (S = Fully_Retired);
end Process_Lifetime;
