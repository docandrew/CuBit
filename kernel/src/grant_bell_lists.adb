-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
package body Grant_Bell_Lists is

    procedure Append (L : in out List; S : Slot) is
    begin
        Set_Previous (S, L.Tail);
        Set_Next (S, No_Slot);
        if L.Tail /= No_Slot then
            Set_Next (L.Tail, S);
        else
            L.Head := S;
        end if;
        L.Tail := S;
        L.Count := L.Count + 1;
    end Append;

    procedure Remove (L : in out List; S : Slot) is
        P : constant Slot := Previous (S);
        N : constant Slot := Next (S);
    begin
        if P /= No_Slot then
            Set_Next (P, N);
        else
            L.Head := N;
        end if;
        if N /= No_Slot then
            Set_Previous (N, P);
        else
            L.Tail := P;
        end if;
        Set_Previous (S, No_Slot);
        Set_Next (S, No_Slot);
        L.Count := L.Count - 1;
    end Remove;

    procedure Pop (L : in out List; S : out Slot) is
    begin
        S := L.Head;
        if S /= No_Slot then
            Remove (L, S);
        end if;
    end Pop;

end Grant_Bell_Lists;
