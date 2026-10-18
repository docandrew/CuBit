-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
package body Grant_Doorbells with SPARK_Mode is
    pragma Warnings (GNATprove, Off, "is set by", Reason => "ghost lemmas keep only the state");

    procedure Ring (B : in out Bell; Outcome : out Ring_Outcome; Newly : out Boolean) is
    begin
        Newly := False;
        if B.Ended then
            Outcome := Ended_Grant;
        else
            Outcome := Rung;
            Newly := not B.Pending;
            B.Pending := True;
        end if;
    end Ring;

    procedure Take (B : in out Bell; Taken : out Boolean) is
    begin
        Taken := B.Pending;
        B.Pending := False;
    end Take;

    procedure Finish (B : in out Bell; Was_Pending : out Boolean) is
    begin
        Was_Pending := B.Pending;
        B.Pending := False;
        B.Ended := True;
    end Finish;

    procedure Lemma_Ring_Taken_Once (B : in out Bell; Repeats : Natural; First, Second : out Boolean) is
        Outcome : Ring_Outcome;
        Newly : Boolean;
    begin
        Ring (B, Outcome, Newly);
        for K in 1 .. Repeats loop
            Ring (B, Outcome, Newly);
            pragma Loop_Invariant (B.Pending and then not B.Ended);
        end loop;
        Take (B, First);
        Take (B, Second);
    end Lemma_Ring_Taken_Once;

    procedure Lemma_Ended_Refuses (B : in out Bell; Outcome : out Ring_Outcome) is
        Was_Pending, Newly : Boolean;
    begin
        Finish (B, Was_Pending);
        Ring (B, Outcome, Newly);
    end Lemma_Ended_Refuses;

end Grant_Doorbells;
