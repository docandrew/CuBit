package body Interrupt_State with SPARK_Mode => On is
    procedure Enter (S : in out State; Hardware_IF : Boolean; Status : out Result)
    is
    begin
        if S.Level > 0 and Hardware_IF then
            Status := Interrupts_Enabled_While_Nested;
        elsif S.Level = Natural'Last then
            Status := Nesting_Exhausted;
        else
            if S.Level = 0 then
                S.Policy := (if Hardware_IF then Restore_Enabled else Keep_Masked);
            end if;
            S.Level := S.Level + 1;
            Status := Success;
        end if;
    end Enter;

    procedure Leave (S : in out State; Hardware_IF : Boolean;
                     Enable_Interrupts : out Boolean; Status : out Result)
    is
    begin
        Enable_Interrupts := False;
        if Hardware_IF then
            Status := Interrupts_Enabled_While_Nested;
        elsif S.Level = 0 then
            Status := No_Critical_Section;
        else
            S.Level := S.Level - 1;
            Enable_Interrupts := S.Level = 0 and S.Policy = Restore_Enabled;
            Status := Success;
        end if;
    end Leave;

    function Capture (S : State) return Context is ((Policy => S.Policy));

    procedure Resume (S : in out State; Saved : Context) is
    begin
        S.Policy := Saved.Policy;
    end Resume;

    procedure Prove_Handoff (Suspended, Incoming : State) is
        Saved : constant Context := Capture (Suspended);
        Running : State := Incoming;
        Enable : Boolean;
        Status : Result;
    begin
        Resume (Running, Saved);
        pragma Assert (Can_Handoff (Running, False, True));
        Leave (Running, False, Enable, Status);
        pragma Assert (Status = Success and Depth (Running) = 0);
        pragma Assert (Enable = Restores_Interrupts (Suspended));
    end Prove_Handoff;

    procedure Prove_Nested_Exclusion (Idle : State; Hardware_IF : Boolean) is
        Running : State := Idle;
        Enable : Boolean;
        Status : Result;
    begin
        Enter (Running, Hardware_IF, Status);
        pragma Assert (Status = Success and Depth (Running) = 1);
        Enter (Running, False, Status);
        pragma Assert (Status = Success and Depth (Running) = 2);
        Leave (Running, False, Enable, Status);
        pragma Assert (Status = Success and Depth (Running) = 1 and not Enable);
        Leave (Running, False, Enable, Status);
        pragma Assert (Status = Success and Depth (Running) = 0);
        pragma Assert (Enable = Hardware_IF);
    end Prove_Nested_Exclusion;
end Interrupt_State;
