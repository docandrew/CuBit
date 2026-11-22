pragma Ada_2022;
with CuBit.Metric_Records;
package body CuBit.Metric_Raw_Validation with SPARK_Mode is
   package R renames CuBit.Metric_Records;
   function Valid (Page : P.Raw_Page; Cursor, Written, Next, Gap : Unsigned_64)
      return Boolean is
      Words : R.Slot_Words;
   begin
      if Cursor = 0 or Written > P.Raw_Rows_Per_Page or Next < Cursor then
         return False;
      end if;
      if Gap > Next - Cursor or else Next - Cursor - Gap /= Written then
         return False;
      end if;
      for I in P.Raw_Row_Index loop
         if Unsigned_64 (I) < Written then
            if Page (I) (0) /= Cursor + Gap + Unsigned_64 (I) or else
               Page (I) (1) = 0 or else not P.Is_Publisher (Page (I) (2)) or else
               Page (I) (3) = 0 or else Page (I) (6) /= 0 or else Page (I) (7) /= 0
            then
               return False;
            end if;
            for W in R.Slot_Word_Index loop
               Words (W) := Page (I) (8 + W);
            end loop;
            if not R.Decode (Words).Success then
               return False;
            end if;
         else
            for W of Page (I) loop
               if W /= 0 then
                  return False;
               end if;
            end loop;
         end if;
      end loop;
      return True;
   end Valid;
end CuBit.Metric_Raw_Validation;
