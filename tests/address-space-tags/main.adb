-------------------------------------------------------------------------------
-- Hosted checks for address-space tags (tests/address-space-tags/README.md):
-- the CR3 encoding at its edges, and a long random walk over Tag_Protocol
-- (with contracts checked at run time) asserting the invariant and that no
-- CPU running a slot ever holds another life's translations.
-------------------------------------------------------------------------------
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Address_Space_Tags; use Address_Space_Tags;
with Tag_Protocol; use Tag_Protocol;

procedure Main is
    Failures : Natural := 0;

    procedure Check (Condition : Boolean; Name : String) is
    begin
        if not Condition then
            Failures := Failures + 1;
            Put_Line ("FAIL " & Name);
        end if;
    end Check;

    procedure CR3_Edges is
        Roots : constant array (1 .. 3) of Unsigned_64 := (0, 4096, Root_Mask);
        Tags  : constant array (1 .. 4) of Tag := (Kernel_Tag, 1, 256, Tag'Last);
    begin
        for R of Roots loop
            for T of Tags loop
                for K in Boolean loop
                    declare
                        V : constant Unsigned_64 := CR3_Value (R, T, K);
                    begin
                        Check (Root_Of (V) = R and then Tag_Of (V) = T and then
                               Keeps (V) = K, "CR3 round trip");
                    end;
                end loop;
            end loop;
        end loop;
        Check (not Valid_Root (4097), "unaligned root refused");
        Check (not Valid_Root (2 ** 52), "root above bit 51 refused");
        Check (Valid_Root (Root_Mask), "highest root accepted");
        Check (not Keep (No_Life, No_Life), "no life is never kept");
        Check (Keep (5, 5) and then not Keep (4, 5) and then not Keep (6, 5),
               "keep only the current life");
        Check (Next_Life (No_Life) /= No_Life, "a first life is a life");
    end CR3_Edges;

    -- A deterministic generator (no Ada.Numerics state to seed).
    Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
    function Pick (Bound : Positive) return Natural is
    begin
        Seed := Seed * 6364136223846793005 + 1442695040888963407;
        return Natural (Shift_Right (Seed, 33) mod Unsigned_64 (Bound));
    end Pick;

    Steps : constant := 1_000_000;
    S : State;
    Kept : Boolean;
    Loads, Keeps_Seen, Lives_Begun, Shootdowns : Natural := 0;
begin
    CR3_Edges;
    for Step in 1 .. Steps loop
        declare
            C : constant CPU := CPU (Pick (Natural (CPU'Last) + 1));
            T : constant Slot := Slot'First + Pick (Slot'Last - Slot'First + 1);
        begin
            case Pick (8) is
                when 0 | 1 =>
                    if S.Lives (T) /= No_Life then
                        declare
                            Before : constant Life := S.Cached (C, T);
                        begin
                            Load (S, C, T, Kept);
                            Loads := Loads + 1;
                            if Kept then
                                Keeps_Seen := Keeps_Seen + 1;
                                Check (Before = No_Life or else Before = S.Lives (T),
                                       "kept translations belong to the current life");
                            end if;
                        end;
                    end if;
                when 2 =>
                    Load_Kernel (S, C);
                when 3 | 4 =>
                    Use_Translations (S, C);
                when 5 =>
                    if Runs_Nowhere (S, T) then
                        Begin_Life (S, T);
                        Lives_Begun := Lives_Begun + 1;
                    end if;
                when 6 =>
                    Remove (S, T);
                    if Pick (2) = 0 then
                        Shootdown (S);
                        Shootdowns := Shootdowns + 1;
                        for D in CPU loop
                            for U in Slot loop
                                Check (not S.Removed (D, U), "removed mapping forgotten");
                            end loop;
                        end loop;
                    end if;
                when others =>
                    Flush_All (S, C);
            end case;
        end;
        Check (Inv (S), "invariant");
        Check (No_Stale (S), "no stale translations");
        exit when Failures > 10;
    end loop;
    Put_Line ("address-space-tags: steps" & Natural'Image (Steps) &
              " loads" & Natural'Image (Loads) & " kept" & Natural'Image (Keeps_Seen) &
              " lives" & Natural'Image (Lives_Begun) &
              " shootdowns" & Natural'Image (Shootdowns));
    if Failures = 0 then
        Put_Line ("address-space-tags: PASS");
    else
        Put_Line ("address-space-tags: FAIL" & Natural'Image (Failures));
    end if;
end Main;
