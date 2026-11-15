pragma Ada_2022;

package body Interrupt_Line_Policy with
   SPARK_Mode => On
is

   procedure Route
     (S        : in out Line_State;
      Owner    : Unsigned_64;
      Trigger  : Trigger_Mode;
      Polarity : Pin_Polarity;
      Result   : out Route_Result;
      Action   : out Hardware_Action)
   is
      Free : Owner_Slot := Owner_Slot'First;
      Found_Free : Boolean := False;
   begin
      if Has_Owner (S) and then not Same_Mode (S, Trigger, Polarity) then
         Result := Mode_Conflict;
         Action := No_Change;
         return;
      end if;

      if not Is_Owner (S, Owner) then
         for I in Owner_Slot loop
            if S.Owners (I) = No_Owner then
               Free := I;
               Found_Free := True;
               exit;
            end if;
            pragma Loop_Invariant
              (for all J in Owner_Slot'First .. I => S.Owners (J) /= No_Owner);
         end loop;
         if not Found_Free then
            Result := Owners_Full;
            Action := No_Change;
            return;
         end if;
         pragma Assert (not S.Waiting (Free));
         S.Owners (Free) := Owner;
      end if;

      S.Trigger := Trigger;
      S.Polarity := Polarity;
      --  Masked exactly while an owner still has to acknowledge: a masked
      --  line nobody waits on (its owners left) is opened for the new one.
      S.Held_Masked := Any_Waiting (S);
      Result := Routed;
      Action := (if S.Held_Masked then Program_Masked else Program_Unmasked);
   end Route;

   procedure Deliver
     (S        : in out Line_State;
      Action   : out Hardware_Action;
      Dispatch : out Boolean)
   is
   begin
      S.Delivered := Next_Count (S.Delivered);
      Dispatch := Has_Owner (S);
      if S.Trigger = Level then
         for I in Owner_Slot loop
            S.Waiting (I) := S.Owners (I) /= No_Owner;
            pragma Loop_Invariant
              (for all J in Owner_Slot'First .. I =>
                 S.Waiting (J) = (S.Owners (J) /= No_Owner));
         end loop;
         S.Held_Masked := True;
         Action := Mask_Input;
      else
         Action := No_Change;
      end if;
   end Deliver;

   procedure Acknowledge
     (S        : in out Line_State;
      Owner    : Unsigned_64;
      Accepted : out Boolean;
      Action   : out Hardware_Action)
   is
      Was_Masked : constant Boolean := S.Held_Masked;
   begin
      Accepted := Is_Owner (S, Owner);
      Action := No_Change;
      if not Accepted then
         return;
      end if;
      for I in Owner_Slot loop
         if S.Owners (I) = Owner then
            S.Waiting (I) := False;
         end if;
         pragma Loop_Invariant
           (for all J in Owner_Slot'First .. I =>
              (if S.Owners (J) = Owner then not S.Waiting (J)));
         pragma Loop_Invariant
           (for all J in Owner_Slot =>
              (if S.Owners (J) /= Owner or else J > I
               then S.Waiting (J) = S.Waiting'Loop_Entry (J)));
         pragma Loop_Invariant (S.Owners = S.Owners'Loop_Entry);
      end loop;
      if Was_Masked and then not Any_Waiting (S) then
         S.Held_Masked := False;
         Action := Unmask_Input;
      end if;
   end Acknowledge;

   procedure Release
     (S      : in out Line_State;
      Owner  : Unsigned_64;
      Action : out Hardware_Action)
   is
      Was_Owner  : constant Boolean := Is_Owner (S, Owner);
      Was_Masked : constant Boolean := S.Held_Masked;
   begin
      Action := No_Change;
      if not Was_Owner then
         return;
      end if;
      for I in Owner_Slot loop
         if S.Owners (I) = Owner then
            S.Owners (I) := No_Owner;
            S.Waiting (I) := False;
         end if;
         pragma Loop_Invariant
           (for all J in Owner_Slot'First .. I => S.Owners (J) /= Owner);
         pragma Loop_Invariant
           (for all J in Owner_Slot =>
              (if S.Owners'Loop_Entry (J) /= Owner or else J > I then
                 S.Owners (J) = S.Owners'Loop_Entry (J)
                 and then S.Waiting (J) = S.Waiting'Loop_Entry (J)));
         pragma Loop_Invariant
           (for all J in Owner_Slot =>
              (if S.Waiting (J) then S.Owners (J) /= No_Owner));
         pragma Loop_Invariant
           (for all J in Owner_Slot =>
              (if S.Waiting (J) then S.Waiting'Loop_Entry (J)));
      end loop;
      --  Leaving only ends waits: nobody waited unless the line was held.
      pragma Assert (if Any_Waiting (S) then Was_Masked);
      if not Has_Owner (S) then
         --  Nobody would clear a level source: keep the input masked.
         if S.Trigger = Level then
            S.Held_Masked := True;
            Action := Mask_Input;
         end if;
      elsif Was_Masked and then not Any_Waiting (S) then
         S.Held_Masked := False;
         Action := Unmask_Input;
      end if;
   end Release;

end Interrupt_Line_Policy;
