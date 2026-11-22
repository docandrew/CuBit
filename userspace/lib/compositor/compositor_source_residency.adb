package body Compositor_Source_Residency with SPARK_Mode is
   function Find (S : State; Key : C.Source_Key) return Slot is
   begin
      for I in Slot loop
         if S.Entries (I).Key = Key then return I; end if;
         pragma Loop_Invariant (for all J in Slot'First .. I => S.Entries (J).Key /= Key);
      end loop;
      raise Program_Error;
   end Find;
   function First_Free (S : State) return Slot is
   begin
      for I in Slot loop
         if S.Entries (I).Key = C.No_Key then return I; end if;
         pragma Loop_Invariant (for all J in Slot'First .. I => S.Entries (J).Key /= C.No_Key);
      end loop;
      raise Program_Error;
   end First_Free;
   function Victim (S : State; Allowed : Candidates) return Slot is
      Best : Slot := Slot'First;
      Found : Boolean := False;
   begin
      for I in Slot loop
         if Allowed (I) and then
           (not Found or else S.Entries (I).Last_Use < S.Entries (Best).Last_Use)
         then
            Best := I; Found := True;
         end if;
         pragma Loop_Invariant (Found = (for some J in Slot'First .. I => Allowed (J)));
         pragma Loop_Invariant (if Found then Allowed (Best));
         pragma Loop_Invariant
           (for all J in Slot'First .. I =>
              (if Allowed (J) then S.Entries (Best).Last_Use <= S.Entries (J).Last_Use));
      end loop;
      pragma Assert (Found);
      return Best;
   end Victim;
   procedure Bind (S : in out State; I : Slot; Key : C.Source_Key) is
   begin
      S.Entries (I).Key := Key;
      S.Entries (I).Stale := C.Empty_Band;
      pragma Assert (S.Entries (I).Key = Key);
   end Bind;
   procedure Unbind (S : in out State; I : Slot) is
   begin
      S.Entries (I).Key := C.No_Key;
      S.Entries (I).Stale := C.Empty_Band;
   end Unbind;
   procedure Touch (S : in out State; I : Slot) is
   begin
      if S.Clock < Use_Stamp'Last then S.Clock := S.Clock + 1; end if;
      S.Entries (I).Last_Use := S.Clock;
   end Touch;
   procedure Note (S : in out State; Key : C.Source_Key; Rows : C.Row_Band) is
      I : Slot;
   begin
      if not Holds (S, Key) then return; end if;
      I := Find (S, Key);
      S.Entries (I).Stale := C.Union (S.Entries (I).Stale, Rows);
   end Note;
   procedure Clear_Stale (S : in out State; I : Slot) is
   begin
      S.Entries (I).Stale := C.Empty_Band;
   end Clear_Stale;
end Compositor_Source_Residency;
