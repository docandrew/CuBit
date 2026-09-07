package body TLB_Reclamation with SPARK_Mode => On is
    procedure Begin_Round (S : in out State; Targets : CPU_Set;
                           Success : out Boolean) is
    begin
        if S.In_Progress or S.Generation = Epoch'Last then
            Success := False;
        else
            S.Generation := S.Generation + 1;
            S.Pending := Targets;
            S.In_Progress := True;
            Success := True;
        end if;
    end Begin_Round;

    procedure Observe (S : in out State; Seen : Acknowledgments) is
    begin
        for C in CPU_Index loop
            if S.In_Progress and Seen (C) = S.Generation then
                S.Pending (C) := False;
            end if;
            pragma Loop_Invariant
              (for all I in CPU_Index'First .. C => S.Pending (I) =
                (S.Pending'Loop_Entry (I) and
                 not (S.In_Progress and Seen (I) = S.Generation)));
        end loop;
    end Observe;

    procedure Take_Completion (S : in out State; Authorized : out Boolean) is
    begin
        Authorized := Can_Reclaim (S);
        if Authorized then
            S.In_Progress := False;
        end if;
    end Take_Completion;

    procedure Prove_Withheld_Ack (CPU : CPU_Index) is
        S : State := Initial_State;
        Targets : constant CPU_Set := (others => True);
        Seen : Acknowledgments := (others => 1);
        OK : Boolean;
    begin
        Begin_Round (S, Targets, OK);
        pragma Assert (OK);
        Seen (CPU) := 0; -- every other CPU replied; this one is still stale
        Observe (S, Seen);
        Take_Completion (S, OK);
        pragma Assert (not OK and Waiting_For (S, CPU));
    end Prove_Withheld_Ack;

    procedure Prove_Completion_And_Replay is
        S : State := Initial_State;
        Targets : constant CPU_Set := (others => True);
        Seen : constant Acknowledgments := (others => 1);
        OK : Boolean;
    begin
        Begin_Round (S, Targets, OK);
        Observe (S, Seen);
        Take_Completion (S, OK);
        pragma Assert (OK);
        Take_Completion (S, OK);
        pragma Assert (not OK); -- completion is consumed, not reusable
        Begin_Round (S, Targets, OK);
        pragma Assert (OK and Ticket (S) = 2);
        Observe (S, Seen); -- replay acknowledgments from the first round
        Take_Completion (S, OK);
        pragma Assert (not OK);
    end Prove_Completion_And_Replay;
end TLB_Reclamation;
