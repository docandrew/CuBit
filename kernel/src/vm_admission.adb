pragma Ada_2022;
package body VM_Admission with SPARK_Mode => On is

    function Range_Admission
      (T : Run_Table; Count : Run_Count; Mapped : Page_Total;
       First, Bytes : Unsigned_64) return Range_Verdict
    is
    begin
        if Bytes = 0 or else Bytes mod Page_Bytes /= 0
          or else First mod Page_Bytes /= 0
          or else First >= VM_ABI.Guest_Physical_Limit
          or else Bytes / Page_Bytes > (VM_ABI.Guest_Physical_Limit - First) / Page_Bytes
        then
            return Bad_Range;
        elsif Bytes / Page_Bytes > VM_ABI.Guest_Page_Ceiling - Mapped then
            return Too_Large;
        elsif Count = VM_ABI.RAM_Run_Ceiling then
            return Ledger_Full;
        end if;
        pragma Assert (Run_Ok ((First, Bytes / Page_Bytes)));
        for I in 1 .. Count loop
            if Overlap (T (I), (First, Bytes / Page_Bytes)) then
                return Overlapping;
            end if;
            pragma Loop_Invariant
              (for all J in 1 .. I => not Overlap (T (J), (First, Bytes / Page_Bytes)));
        end loop;
        return Admitted;
    end Range_Admission;

    procedure Lemma_Add_Keeps_Disjoint
      (T : Run_Table; Count : Run_Count; New_Run : Run) is null;
end VM_Admission;
