package body Compositor_Input_Queue with SPARK_Mode is
   function Newest (Q : Queue) return Selection is
      Best : Selection := -1;
   begin
      for I in Index loop
         if Q (I).Valid and then
           (Best = -1 or else Q (I).Serial > Q (Best).Serial)
         then Best := I; end if;
         pragma Loop_Invariant
           (if Best = -1 then (for all J in Index'First .. I => not Q (J).Valid)
            else Best <= I and then Q (Best).Valid and then
              (for all J in Index'First .. I =>
                (if Q (J).Valid then Q (J).Serial <= Q (Best).Serial)));
      end loop;
      return Best;
   end Newest;
   function Vacant (Q : Queue) return Selection is
   begin
      for I in Index loop
         if not Q (I).Valid then return I; end if;
         pragma Loop_Invariant (for all J in Index'First .. I => Q (J).Valid);
      end loop;
      return -1;
   end Vacant;
   function Oldest_After (Q : Queue; After : Word) return Selection is
      Best : Selection := -1;
   begin
      for I in Index loop
         if Q (I).Valid and then Q (I).Serial > After and then
           (Best = -1 or else Q (I).Serial < Q (Best).Serial)
         then Best := I; end if;
         pragma Loop_Invariant
           (if Best = -1 then
              (for all J in Index'First .. I => not Q (J).Valid or Q (J).Serial <= After)
            else Best <= I and then Q (Best).Valid and then Q (Best).Serial > After and then
              (for all J in Index'First .. I =>
                (if Q (J).Valid and Q (J).Serial > After then Q (Best).Serial <= Q (J).Serial)));
      end loop;
      return Best;
   end Oldest_After;
   procedure Pop
     (Q : in out Queue; After : Word; Selected : out Selection; Value : out Event)
   is
      Before : constant Queue := Q with Ghost;
   begin
      Selected := Oldest_After (Q, After);
      Value := (if Selected = -1 then (others => <>) else Q (Selected));
      for I in Index loop
         if Q (I).Valid and then (Q (I).Serial <= After or I = Selected) then
            Q (I).Valid := False;
         end if;
         pragma Loop_Invariant
           (for all J in Index => Q (J) =
             (if J <= I and then Before (J).Valid and then
                (Before (J).Serial <= After or J = Selected)
              then Cleared (Before (J)) else Before (J)));
      end loop;
   end Pop;
   procedure Reserve (Next_Serial : in out Word; Serial : out Word) is
   begin
      Serial := 0;
      if Next_Serial /= 0 and Next_Serial /= Word'Last then
         Serial := Next_Serial;
         Next_Serial := Next_Serial + 1;
      end if;
   end Reserve;
   procedure Recover
     (Q : in out Queue; Next_Serial : in out Word; Recovery : Event; Accepted : out Boolean)
   is
      Serial : Word;
   begin
      Reserve (Next_Serial, Serial);
      Accepted := Serial /= 0;
      if Accepted then
         Q := (others => (others => <>));
         Q (Index'First) := Numbered (Recovery, Serial);
      end if;
   end Recover;
   procedure Push
     (Q : in out Queue; Next_Serial : in out Word;
      Incoming, Recovery : Event; Motion_Kind : Word; Result : out Outcome)
   is
      Last : Selection;
      Free : Selection;
   begin
      if Next_Serial = 0 or else Next_Serial = Word'Last then
         Result := Exhausted;
         return;
      end if;
      Last := (if Incoming.Kind = Motion_Kind then Newest (Q) else -1);
      if Incoming.Kind = Motion_Kind and then Last /= -1 and then
        Q (Last).Kind = Motion_Kind
      then
         Q (Last) := Coalesced (Q (Last), Incoming);
         Result := Motion_Replaced;
         return;
      end if;
      Free := Vacant (Q);
      if Free /= -1 then
         Q (Free) := Numbered (Incoming, Next_Serial);
         Result := Appended;
      else
         Q := (others => (others => <>));
         Q (Index'First) := Numbered (Recovery, Next_Serial);
         Result := Resynchronized;
      end if;
      Next_Serial := Next_Serial + 1;
   end Push;
end Compositor_Input_Queue;
