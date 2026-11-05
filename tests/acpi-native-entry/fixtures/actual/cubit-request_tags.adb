------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Request_Tags with SPARK_Mode is

   procedure Issue (T : in out Table; Number : out Tag; At_Entry : out Entry_Index;
                    Result : out Issue_Result) is
   begin
      Number := No_Tag;
      At_Entry := Entry_Index'First;
      if T.Next_Tag > Last_Tag then
         Result := Exhausted;
         return;
      end if;
      for I in Entry_Index loop
         if T.Entries (I).State = Free then
            Number := T.Next_Tag;
            At_Entry := I;
            T.Entries (I) := (State => Waiting, Number => Number);
            T.Next_Tag := T.Next_Tag + 1;
            Result := Issued;
            return;
         end if;
         pragma Loop_Invariant (for all J in 1 .. I => T.Entries (J).State /= Free);
      end loop;
      Result := Full;
   end Issue;

   procedure Complete (T : in out Table; Number : Tag; At_Entry : out Entry_Index;
                       Result : out Match) is
   begin
      At_Entry := Entry_Index'First;
      Result := Unknown;
      if Number = No_Tag then
         return;
      end if;
      for I in Entry_Index loop
         if T.Entries (I).State /= Free and then T.Entries (I).Number = Number then
            At_Entry := I;
            if T.Entries (I).State = Waiting then
               T.Entries (I).State := Answered;
               Result := Matched;
            else
               Result := Duplicate;
            end if;
            return;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => T.Entries (J).State = Free or else T.Entries (J).Number /= Number);
      end loop;
   end Complete;

   procedure Fail_All (T : in out Table) is
   begin
      for I in Entry_Index loop
         if T.Entries (I).State = Waiting then
            T.Entries (I).State := Ended;
         end if;
         pragma Loop_Invariant
           (for all J in Entry_Index =>
              T.Entries (J).Number = T.Entries'Loop_Entry (J).Number
              and then T.Entries (J).State =
                (if J <= I and then T.Entries'Loop_Entry (J).State = Waiting then Ended
                 else T.Entries'Loop_Entry (J).State));
      end loop;
   end Fail_All;

   procedure Release (T : in out Table; At_Entry : Entry_Index) is
   begin
      T.Entries (At_Entry) := (State => Free, Number => No_Tag);
   end Release;

end CuBit.Request_Tags;
