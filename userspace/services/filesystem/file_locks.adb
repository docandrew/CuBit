package body File_Locks with SPARK_Mode is

   procedure Stop_Waiting (T : in out Table; S : Slot)
   with Pre  => Consistent (T),
        Post => Consistent (T) and then not T.Slots (S).Waiting
                and then T.Slots (S).Held = T'Old.Slots (S).Held
                and then T.Slots (S).Object = T'Old.Slots (S).Object
                and then (for all O in Slot => (if O /= S then T.Slots (O) = T'Old.Slots (O)));

   procedure Stop_Waiting (T : in out Table; S : Slot) is
   begin
      if T.Slots (S).Waiting then
         T.Slots (S).Waiting := False;
         if T.Waiting_Count > 0 then
            T.Waiting_Count := T.Waiting_Count - 1;
         end if;
      end if;
   end Stop_Waiting;

   procedure Request
     (T : in out Table; S : Slot; Object : Object_Key; Wanted : Lock_Mode;
      Wait : Boolean; Until_At : Deadline; Result : out Outcome; Released : out Boolean)
   is
      Prior : constant Mode := T.Slots (S).Held;
   begin
      Released := False;
      T.Slots (S).Object := Object;
      if Prior = Wanted then
         Result := Granted;
         return;
      end if;
      if Prior /= Unlocked then
         T.Slots (S).Held := Unlocked;
         Released := True;
      end if;
      if Compatible (T, S, Object, Wanted) and then not Has_Waiter (T, Object) then
         T.Slots (S).Held := Wanted;
         Result := Granted;
      elsif Wait and then T.Next_Order < Arrival'Last and then T.Waiting_Count < Capacity then
         T.Slots (S) := (Object => Object, Held => Unlocked, Waiting => True, Wanted => Wanted,
                         Order => T.Next_Order, Until_At => Until_At);
         T.Next_Order := T.Next_Order + 1;
         T.Waiting_Count := T.Waiting_Count + 1;
         Result := Queued;
      else
         Result := Busy;
      end if;
   end Request;

   procedure Release (T : in out Table; S : Slot; Was_Held, Was_Waiting : out Boolean) is
   begin
      Was_Held := T.Slots (S).Held /= Unlocked;
      Was_Waiting := T.Slots (S).Waiting;
      T.Slots (S).Held := Unlocked;
      Stop_Waiting (T, S);
   end Release;

   procedure Cancel_Wait (T : in out Table; S : Slot; Was_Waiting : out Boolean) is
   begin
      Was_Waiting := T.Slots (S).Waiting;
      Stop_Waiting (T, S);
   end Cancel_Wait;

   procedure Settle
     (T : in out Table; Object : Object_Key; Granted_Slot : out Slot; Found : out Boolean)
   is
      Have : Boolean := False;
      Oldest : Slot := Slot'First;
   begin
      Granted_Slot := Slot'First;
      Found := False;
      for O in Slot loop
         if T.Slots (O).Waiting and then T.Slots (O).Object = Object
           and then (not Have or else T.Slots (O).Order < T.Slots (Oldest).Order)
         then
            Oldest := O;
            Have := True;
         end if;
         pragma Loop_Invariant
           (if Have then T.Slots (Oldest).Waiting and then T.Slots (Oldest).Object = Object);
      end loop;
      if Have and then Compatible (T, Oldest, Object, T.Slots (Oldest).Wanted) then
         T.Slots (Oldest).Held := T.Slots (Oldest).Wanted;
         Stop_Waiting (T, Oldest);
         Granted_Slot := Oldest;
         Found := True;
      end if;
   end Settle;

   procedure Expire
     (T : in out Table; Now : Deadline; Expired_Slot : out Slot; Found : out Boolean) is
   begin
      Expired_Slot := Slot'First;
      Found := False;
      for O in Slot loop
         if T.Slots (O).Waiting and then T.Slots (O).Until_At /= No_Deadline
           and then T.Slots (O).Until_At <= Now
         then
            Stop_Waiting (T, O);
            Expired_Slot := O;
            Found := True;
            return;
         end if;
      end loop;
   end Expire;

   function Earliest_Deadline (T : Table) return Deadline is
      Earliest : Deadline := No_Deadline;
   begin
      for O in Slot loop
         if T.Slots (O).Waiting and then T.Slots (O).Until_At < Earliest then
            Earliest := T.Slots (O).Until_At;
         end if;
      end loop;
      return Earliest;
   end Earliest_Deadline;

end File_Locks;
