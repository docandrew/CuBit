with Interfaces; use Interfaces;
with Config;

-- Production acknowledgment/authorization state. No hardware access. The
-- adapter must publish invalidations before requests, and acknowledge only
-- after the architectural flush. A completed round can be consumed once.
package TLB_Reclamation with SPARK_Mode => On is
    subtype CPU_Index is Natural range 0 .. Config.MAX_SMP_CPUS - 1;
    type Epoch is new Unsigned_64;
    type CPU_Set is array (CPU_Index) of Boolean;
    type Acknowledgments is array (CPU_Index) of Epoch;
    type State is private;
    Initial_State : constant State;

    function Active (S : State) return Boolean;
    function Ticket (S : State) return Epoch;
    function Waiting_For (S : State; CPU : CPU_Index) return Boolean;
    function Can_Reclaim (S : State) return Boolean;

    procedure Begin_Round (S : in out State; Targets : CPU_Set;
                           Success : out Boolean)
      with Global => null,
           Post =>
             (if Active (S'Old) or Ticket (S'Old) = Epoch'Last then
                 not Success and S = S'Old
              else Success and Active (S) and
                   Ticket (S) = Ticket (S'Old) + 1 and
                   (for all C in CPU_Index => Waiting_For (S, C) = Targets (C)));

    procedure Observe (S : in out State; Seen : Acknowledgments)
      with Global => null,
           Post => Active (S) = Active (S'Old) and
                   Ticket (S) = Ticket (S'Old) and
                   (for all C in CPU_Index => Waiting_For (S, C) =
                     (Waiting_For (S'Old, C) and
                      not (Active (S'Old) and Seen (C) = Ticket (S'Old))));

    procedure Take_Completion (S : in out State; Authorized : out Boolean)
      with Global => null,
           Post => Authorized = Can_Reclaim (S'Old) and
                   Ticket (S) = Ticket (S'Old) and
                   (if Authorized then not Active (S) else S = S'Old);

    procedure Prove_Withheld_Ack (CPU : CPU_Index)
      with Ghost, Global => null;

    procedure Prove_Completion_And_Replay
      with Ghost, Global => null;

private
    type State is record
        In_Progress : Boolean := False;
        Generation : Epoch := 0;
        Pending : CPU_Set := (others => False);
    end record;
    Initial_State : constant State := (others => <>);
    function Active (S : State) return Boolean is (S.In_Progress);
    function Ticket (S : State) return Epoch is (S.Generation);
    function Waiting_For (S : State; CPU : CPU_Index) return Boolean is
      (S.Pending (CPU));
    function Can_Reclaim (S : State) return Boolean is
      (S.In_Progress and (for all C in CPU_Index => not S.Pending (C)));
end TLB_Reclamation;
