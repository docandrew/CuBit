package body Process_Lifetime with SPARK_Mode => On is
    procedure Enter_CPU (S : in out State; Success : out Boolean) is
    begin
        Success := Can_Run (S);
        if Success then S := Live_Running; end if;
    end Enter_CPU;
    procedure Leave_CPU (S : in out State; Success : out Boolean) is
    begin
        Success := Executing (S);
        case S is
            when Live_Running => S := Live_Stopped;
            when Closing_Running => S := Closing_Stopped;
            when others => null;
        end case;
    end Leave_CPU;
    procedure Request_Stop (S : in out State) is
    begin
        case S is
            when Live_Stopped => S := Closing_Stopped;
            when Live_Running => S := Closing_Running;
            when others => null;
        end case;
    end Request_Stop;
    procedure Claim_Reap (S : in out State; Success : out Boolean) is
    begin
        Success := Can_Reap (S);
        if Success then S := Reap_Claimed; end if;
    end Claim_Reap;
    procedure Finish_Reap (S : in out State; Success : out Boolean) is
    begin
        Success := Reaping (S);
        if Success then S := Fully_Retired; end if;
    end Finish_Reap;
    procedure Prove_Stop_Waits_For_CPU is
        S : State := Initial_State;
        OK : Boolean;
    begin
        Enter_CPU (S, OK);
        pragma Assert (OK);
        Request_Stop (S);
        Claim_Reap (S, OK);
        pragma Assert (not OK and Executing (S));
        Leave_CPU (S, OK);
        pragma Assert (OK and Can_Reap (S));
        Enter_CPU (S, OK);
        pragma Assert (not OK);
        Claim_Reap (S, OK);
        pragma Assert (OK and Reaping (S));
        Claim_Reap (S, OK);
        pragma Assert (not OK);
        Finish_Reap (S, OK);
        pragma Assert (OK and Retired (S));
        Enter_CPU (S, OK);
        pragma Assert (not OK);
    end Prove_Stop_Waits_For_CPU;
end Process_Lifetime;
