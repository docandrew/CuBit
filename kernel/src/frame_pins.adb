package body Frame_Pins with SPARK_Mode => On is
    function Decode (Raw : Unsigned_8) return State is
      ((Pins => Raw and 127, Pending_Free => Raw >= 128));

    function Encode (S : State) return Unsigned_8 is
      (S.Pins + (if S.Pending_Free then 128 else 0));

    procedure Pin (S : in out State; Success : out Boolean) is
    begin
        if S.Pending_Free or S.Pins = Pin_Count'Last then
            Success := False;
        else
            S.Pins := S.Pins + 1;
            Success := True;
        end if;
    end Pin;

    procedure Request_Free (S : in out State; Action : out Release_Action) is
    begin
        if S.Pins = 0 then
            S.Pending_Free := False;
            Action := Reclaim_Frame;
        else
            S.Pending_Free := True;
            Action := Keep_Frame;
        end if;
    end Request_Free;

    procedure Unpin (S : in out State; Success : out Boolean;
                     Action : out Release_Action) is
    begin
        Action := Keep_Frame;
        if S.Pins = 0 then
            Success := False;
        else
            S.Pins := S.Pins - 1;
            Success := True;
            if S.Pins = 0 and S.Pending_Free then
                S.Pending_Free := False;
                Action := Reclaim_Frame;
            end if;
        end if;
    end Unpin;

    procedure Prove_Pinned_Free (Original : State) is
        S : State := Original;
        Action : Release_Action;
        Success : Boolean;
    begin
        Request_Free (S, Action);
        pragma Assert (Action = Keep_Frame and Retiring (S));
        Pin (S, Success);
        pragma Assert (not Success and Count (S) = Count (Original));
    end Prove_Pinned_Free;
end Frame_Pins;
