-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
package body Kernel_Reports with
    SPARK_Mode => On
is

    -- Subject's last report was just taken or dropped: release its PID if
    -- that was deferred.
    procedure Settle (T : in out Table; Subject : Process; Released : out Process)
      with Pre  => (for all P in Process =>
                      (if P /= Subject and then T.Subjects (P).Free_Deferred
                       then Any_Unread (T.Subjects (P)))),
           Post => Valid (T) and then
                   T.Recipients = T.Recipients'Old and then
                   (for all P in Process =>
                      (for all K in Report_Kind =>
                         T.Subjects (P).Reports (K) = T.Subjects'Old (P).Reports (K))) and then
                   (if Released /= No_Process then
                      Released = Subject and then
                      not Any_Unread (T.Subjects (Released)) and then
                      not T.Subjects (Released).Free_Deferred)
    is
    begin
        Released := No_Process;
        if T.Subjects (Subject).Free_Deferred and then
           not Any_Unread (T.Subjects (Subject))
        then
            T.Subjects (Subject).Free_Deferred := False;
            if Subject /= No_Process then
                Released := Subject;
            end if;
        end if;
    end Settle;

    procedure Open (T : in out Table; R : Process; Generation : Unsigned_64) is
    begin
        T.Recipients (R) := (Open => True, Generation => Generation);
    end Open;

    procedure Close (T : in out Table; R : Process; Released : out Process) is
    begin
        T.Recipients (R).Open := False;
        Released := No_Process;
        for P in Process loop
            for K in Report_Kind loop
                if T.Subjects (P).Reports (K).Unread and then
                   T.Subjects (P).Reports (K).To.Id = R
                then
                    T.Subjects (P).Reports (K).Unread := False;
                    Settle (T, P, Released);
                    if Released /= No_Process then
                        return;
                    end if;
                end if;
                pragma Loop_Invariant (Valid (T) and then not T.Recipients (R).Open and then
                                   Released = No_Process);
            end loop;
            pragma Loop_Invariant (Valid (T) and then not T.Recipients (R).Open and then
                                   Released = No_Process);
        end loop;
    end Close;

    procedure Put
      (T : in out Table; Subject : Process; Kind : Report_Kind;
       To : Recipient; Value : Report; Kept : out Boolean)
    is
        S : Slot renames T.Subjects (Subject).Reports (Kind);
    begin
        Kept := Accepts (T, To);
        if not Kept then
            return;
        end if;
        if S.Unread then
            -- Only faults reach here (an exit's slot is free): count it.
            if S.Value.Further < Unsigned_16'Last then
                S.Value.Further := S.Value.Further + 1;
            end if;
            S.To := To;
        else
            S := (Unread => True, To => To, Value => Value);
        end if;
    end Put;

    procedure Take
      (T : in out Table; R : Process; Value : out Report; Found : out Boolean;
       Released : out Process)
    is
    begin
        Value := (others => <>);
        Found := False;
        Released := No_Process;
        for P in Process loop
            pragma Loop_Invariant (Valid (T) and then not Found and then
                                   Released = No_Process and then
                                   T = T'Loop_Entry);
            pragma Loop_Invariant
              (for all Q in Process range Process'First .. P - 1 =>
                 (for all K in Report_Kind =>
                    not (T.Subjects (Q).Reports (K).Unread and then
                         T.Subjects (Q).Reports (K).To.Id = R)));
            for K in Report_Kind loop
                pragma Loop_Invariant (T = T'Loop_Entry);
                pragma Loop_Invariant
                  (for all J in Report_Kind =>
                     (if J < K then
                        not (T.Subjects (P).Reports (J).Unread and then
                             T.Subjects (P).Reports (J).To.Id = R)));
                if T.Subjects (P).Reports (K).Unread and then
                   T.Subjects (P).Reports (K).To.Id = R
                then
                    Value := T.Subjects (P).Reports (K).Value;
                    Found := True;
                    T.Subjects (P).Reports (K).Unread := False;
                    Settle (T, P, Released);
                    return;
                end if;
            end loop;
        end loop;
    end Take;

    procedure Request_Free
      (T : in out Table; Subject : Process; Free_Now : out Boolean)
    is
    begin
        Free_Now := not Any_Unread (T.Subjects (Subject));
        T.Subjects (Subject).Free_Deferred := not Free_Now;
    end Request_Free;

end Kernel_Reports;
