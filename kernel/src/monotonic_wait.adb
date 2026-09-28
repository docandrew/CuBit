package body Monotonic_Wait is
   use Interfaces;
   procedure At_Least
     (Duration_US, Overstatement_US : Unsigned_64;
      Poll_Limit : Positive; Result : out Outcome)
   is
      First, Previous, Current, Required : Unsigned_64;
      Valid : Boolean;
   begin
      Result := Unrepresentable;
      if Duration_US = 0 then Result := Completed; return; end if;
      if Duration_US > Unsigned_64'Last - Overstatement_US then return; end if;
      Required := Duration_US + Overstatement_US;
      Read (First, Valid);
      if not Valid then Result := Unavailable; return; end if;
      Previous := First;
      for Poll in 1 .. Poll_Limit loop
         Read (Current, Valid);
         if not Valid then Result := Unavailable; return; end if;
         if Current < Previous then Result := Regressed; return; end if;
         if Current - First >= Required then Result := Completed; return; end if;
         Previous := Current;
         if Poll < Poll_Limit then Pause; end if;
      end loop;
      Result := Polls_Exhausted;
   end At_Least;
end Monotonic_Wait;
