-------------------------------------------------------------------------------
-- Tag_Protocol: a proved model of address-space tags. Test-only.
-------------------------------------------------------------------------------
package body Tag_Protocol with SPARK_Mode => On is

    procedure Prove_No_Stale (S : State) is null;

    procedure Begin_Life (S : in out State; T : Slot) is
    begin
        S.Lives (T) := Next_Life (S.Lives (T));
    end Begin_Life;

    procedure Load (S : in out State; C : CPU; T : Slot; Kept : out Boolean) is
    begin
        Kept := Keep (S.Loaded (C, T), S.Lives (T));
        if not Kept then
            -- CR3 bit 63 clear: the tag's translations go.
            S.Cached (C, T) := No_Life;
            S.Removed (C, T) := False;
        end if;
        S.Loaded (C, T) := S.Lives (T);
        S.Active (C) := T;
    end Load;

    procedure Load_Kernel (S : in out State; C : CPU) is
    begin
        S.Active (C) := Kernel_Tag;
    end Load_Kernel;

    procedure Use_Translations (S : in out State; C : CPU) is
    begin
        if S.Active (C) in Slot then
            S.Cached (C, S.Active (C)) := S.Lives (S.Active (C));
        end if;
    end Use_Translations;

    procedure Remove (S : in out State; T : Slot) is
    begin
        for C in CPU loop
            S.Removed (C, T) := S.Cached (C, T) /= No_Life;
            pragma Loop_Invariant (Inv (S));
            pragma Loop_Invariant
              (for all D in CPU'First .. C =>
                 S.Removed (D, T) = (S.Cached (D, T) /= No_Life));
            pragma Loop_Invariant (S.Lives = S.Lives'Loop_Entry and then
                                   S.Active = S.Active'Loop_Entry and then
                                   S.Cached = S.Cached'Loop_Entry);
        end loop;
    end Remove;

    procedure Flush_All (S : in out State; C : CPU) is
    begin
        for T in Slot loop
            S.Cached (C, T) := No_Life;
            S.Removed (C, T) := False;
            pragma Loop_Invariant (Inv (S));
            pragma Loop_Invariant
              (for all U in Slot'First .. T =>
                 S.Cached (C, U) = No_Life and then not S.Removed (C, U));
            pragma Loop_Invariant
              (for all D in CPU =>
                 (if D /= C then
                    (for all U in Slot =>
                       S.Cached (D, U) = S.Cached'Loop_Entry (D, U) and then
                       S.Removed (D, U) = S.Removed'Loop_Entry (D, U))));
            pragma Loop_Invariant (S.Lives = S.Lives'Loop_Entry and then
                                   S.Active = S.Active'Loop_Entry and then
                                   S.Loaded = S.Loaded'Loop_Entry);
        end loop;
    end Flush_All;

    procedure Shootdown (S : in out State) is
    begin
        for C in CPU loop
            Flush_All (S, C);
            pragma Loop_Invariant (Inv (S));
            pragma Loop_Invariant
              (for all D in CPU'First .. C =>
                 (for all T in Slot =>
                    S.Cached (D, T) = No_Life and then not S.Removed (D, T)));
            pragma Loop_Invariant (S.Lives = S.Lives'Loop_Entry and then
                                   S.Active = S.Active'Loop_Entry);
        end loop;
    end Shootdown;

    procedure Prove_Reuse_Drops (S : in out State; C : CPU; T : Slot) is
        Ignore, Kept : Boolean;
    begin
        -- The earlier life runs on C and caches translations there.
        Load (S, C, T, Ignore);
        Use_Translations (S, C);
        -- It ends: every CPU leaves it (contexts stop and switch away).
        for D in CPU loop
            Load_Kernel (S, D);
            pragma Loop_Invariant (Inv (S));
            pragma Loop_Invariant
              (for all E in CPU'First .. D => S.Active (E) = Kernel_Tag);
            pragma Loop_Invariant (S.Lives = S.Lives'Loop_Entry);
        end loop;
        -- The PID is reused: a new life, loaded again on C.
        Begin_Life (S, T);
        Load (S, C, T, Kept);
        pragma Assert (not Kept);
    end Prove_Reuse_Drops;

    procedure Prove_Shootdown_Forgets (S : in out State; T : Slot) is
    begin
        Remove (S, T);
        Shootdown (S);
    end Prove_Shootdown_Forgets;

end Tag_Protocol;
