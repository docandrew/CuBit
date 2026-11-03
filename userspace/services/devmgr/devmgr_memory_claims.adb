pragma Ada_2022;

package body Devmgr_Memory_Claims with SPARK_Mode is

   procedure Claim_Pages
     (Claims : in out Claim_Array; Count : in out Natural;
      Physical, Bytes : Unsigned_64; By : Holder; Result : out Claim_Result)
   is
      Wanted : Claim;
      Same : Boolean := False;
   begin
      Result := Malformed;
      if not In_Limit (Physical, Bytes)
        or else (Is_ACPI (By) and then not Whole_Pages (Physical, Bytes))
      then
         return;
      end if;
      Wanted := Pages_Of (Physical, Bytes, By);
      for K in 1 .. Count loop
         if Claims (K) = Wanted then
            Same := True;
         elsif Conflicts (Claims (K).By, By)
           and then not Disjoint (Claims (K), Physical, Bytes)
         then
            Result := (if Is_ACPI (Claims (K).By) then Overlaps_ACPI else Overlaps_Device);
            return;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. K =>
              Claims (J) = Wanted
              or else not Conflicts (Claims (J).By, By)
              or else Disjoint (Claims (J), Physical, Bytes));
         pragma Loop_Invariant (if Same then (for some J in 1 .. K => Claims (J) = Wanted));
      end loop;
      if Same then
         Result := Granted;
      elsif Count < Claims'Last then
         Count := Count + 1;
         Claims (Count) := Wanted;
         Result := Granted;
      else
         Result := Needs_Room;
      end if;
   end Claim_Pages;

   function Next_Start (Claims : Claim_Array; Count : Natural; Page : Unsigned_64) return Unsigned_64 is
      Next : Unsigned_64 := Page_Limit;
   begin
      for K in 1 .. Count loop
         if Claims (K).First > Page and then Claims (K).First < Next then
            Next := Claims (K).First;
         end if;
         pragma Loop_Invariant
           (Next <= Page_Limit
            and then (for all J in 1 .. K => Claims (J).First <= Page or else Claims (J).First >= Next));
      end loop;
      return Next;
   end Next_Start;

end Devmgr_Memory_Claims;
