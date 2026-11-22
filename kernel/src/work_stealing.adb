package body Work_Stealing with SPARK_Mode => On is

    function Min_Age (Ticks_Per_Microsecond : Unsigned_64;
                      Microseconds          : Unsigned_64) return Unsigned_64
    is
    begin
        if Ticks_Per_Microsecond = 0 or else Microseconds = 0 then
            return 0;
        elsif Microseconds <= Unsigned_64'Last / Ticks_Per_Microsecond then
            return Ticks_Per_Microsecond * Microseconds;
        else
            return Unsigned_64'Last;
        end if;
    end Min_Age;

    function Eligible (C                     : Candidate;
                       Now                   : Unsigned_64;
                       Ticks_Per_Microsecond : Unsigned_64;
                       Age_Microseconds      : Unsigned_64) return Boolean
    is
    begin
        return C.Priority >= 0 and then not C.Pinned and then
               not C.Closing and then not C.Executing and then
               Ticks_Per_Microsecond /= 0 and then
               Aged (C.Queued_At, Now,
                     Min_Age (Ticks_Per_Microsecond, Age_Microseconds));
    end Eligible;

    function First_Eligible (E : Eligibility) return Natural is
    begin
        for I in E'Range loop
            pragma Loop_Invariant (for all J in E'First .. I - 1 => not E (J));
            if E (I) then
                return I;
            end if;
        end loop;
        return 0;
    end First_Eligible;

    procedure Prove_First_Is_Best (P : Priorities; E : Eligibility) is
        F : constant Natural := First_Eligible (E);
    begin
        -- Every eligible entry is at or after F, and the list is sorted.
        pragma Assert (for all I in E'Range => (if E (I) then I >= F));
        pragma Assert (for all I in E'Range => (if I >= F then P (F) >= P (I)));
    end Prove_First_Is_Best;

end Work_Stealing;
