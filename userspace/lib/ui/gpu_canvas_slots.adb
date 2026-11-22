package body GPU_Canvas_Slots with SPARK_Mode is

   function Free_Slot (S : State) return Slot_Index is
   begin
      for I in Slot_Index loop
         if S.Phases (I) = Free then
            return I;
         end if;
         pragma Loop_Invariant (for all J in Slot_Index'First .. I => S.Phases (J) /= Free);
      end loop;
      pragma Assert (Count (S, Free) = 0);
      return Slot_Index'First;
   end Free_Slot;

   function Front_Slot (S : State) return Slot_Index is
   begin
      for I in Slot_Index loop
         if S.Phases (I) = Front then
            return I;
         end if;
         pragma Loop_Invariant (for all J in Slot_Index'First .. I => S.Phases (J) /= Front);
      end loop;
      pragma Assert (Count (S, Front) = 0);
      return Slot_Index'First;
   end Front_Slot;

   function Choose (S : State) return Choice is
   begin
      if not S.Visible or else Is_Empty (S.Wanted) then
         return (Hidden, Slot_Index'First);
      elsif S.Wanted /= S.Allocated then
         return (if In_Flight (S) then (Busy, Slot_Index'First)
                 else (Reallocate, Slot_Index'First));
      elsif not S.Front_Painted or else Count (S, Free) = 0 then
         return (Busy, Slot_Index'First);
      else
         return (Grant, Free_Slot (S));
      end if;
   end Choose;

   procedure Acquire (S : in out State; Slot : Slot_Index) is
   begin
      S.Phases (Slot) := Acquired;
   end Acquire;

   procedure Present (S : in out State; Slot : Slot_Index) is
   begin
      S.Phases (Slot) := Copying;
   end Present;

   procedure Complete (S : in out State; Slot : Slot_Index) is
   begin
      for I in Slot_Index loop
         if S.Phases (I) = Front then
            S.Phases (I) := Free;
         end if;
         pragma Loop_Invariant
           (for all J in Slot_Index'First .. I => S.Phases (J) /= Front);
         pragma Loop_Invariant
           (for all J in Slot_Index =>
              (if S.Phases'Loop_Entry (J) = Front and then J <= I then S.Phases (J) = Free
               else S.Phases (J) = S.Phases'Loop_Entry (J) or else J <= I));
         pragma Loop_Invariant
           (for all J in Slot_Index =>
              (if J > I then S.Phases (J) = S.Phases'Loop_Entry (J)));
         pragma Loop_Invariant
           (for all J in Slot_Index =>
              (if S.Phases'Loop_Entry (J) /= Front then S.Phases (J) = S.Phases'Loop_Entry (J)));
      end loop;
      S.Phases (Slot) := Front;
      S.Front_Painted := False;
   end Complete;

   procedure Painted (S : in out State) is
   begin
      S.Front_Painted := True;
   end Painted;

   procedure Reallocated (S : in out State) is
   begin
      S.Phases := [others => Free];
      S.Allocated := S.Wanted;
      S.Front_Painted := True;
      S.Generation := S.Generation + 1;
   end Reallocated;

   procedure Released (S : in out State) is
   begin
      S.Phases := [others => Empty];
      S.Allocated := (0, 0);
      S.Front_Painted := True;
   end Released;

   procedure Cancel (S : in out State; Slot : Slot_Index) is
   begin
      S.Phases (Slot) := Free;
   end Cancel;

end GPU_Canvas_Slots;
