-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
pragma Ada_2022;

package body Thread_Slots with SPARK_Mode => On is

    function Taken_Count (Set : Slot_Set) return Natural is
        Result : Natural := 0;
    begin
        for T in Slot loop
            pragma Loop_Invariant (Result <= Natural (T - Slot'First));
            if Set (T) then
                Result := Result + 1;
            end if;
        end loop;
        return Result;
    end Taken_Count;

    procedure Take (Set : in out Slot_Set; S : out Slot; Taken : out Boolean) is
    begin
        S := Slot'First;
        Taken := False;
        for T in Slot loop
            pragma Loop_Invariant (Set = Set'Loop_Entry);
            pragma Loop_Invariant (for all U in Slot'First .. T => (if U /= T then Set (U)));
            if not Set (T) then
                Set (T) := True;
                S := T;
                Taken := True;
                return;
            end if;
        end loop;
    end Take;

    procedure Give_Back (Set : in out Slot_Set; S : Slot) is
    begin
        Set (S) := False;
    end Give_Back;
end Thread_Slots;
