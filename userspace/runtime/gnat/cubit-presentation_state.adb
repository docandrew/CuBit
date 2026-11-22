package body CuBit.Presentation_State with SPARK_Mode is
   function Allows (Stage : Phase; Action : Event) return Boolean is
     (case Action is
        when Begin_Read => Stage = Queued,
        when Release_Buffer => Stage in Reading | Shown_Held | Dropped_Held,
        when Was_Presented => Stage in Reading | Copied,
        when Discard => Stage in Queued | Reading | Copied,
        when Retire => Stage in Shown_Released | Dropped_Released);

   function Next_Phase (Stage : Phase; Action : Event) return Phase is
     (if not Allows (Stage, Action) then Stage else
        (case Action is
           when Begin_Read => Reading,
           when Release_Buffer =>
             (case Stage is
                when Reading => Copied,
                when Shown_Held => Shown_Released,
                when others => Dropped_Released),
           when Was_Presented =>
             (if Stage = Reading then Shown_Held else Shown_Released),
           when Discard =>
             (if Stage = Copied then Dropped_Released else Dropped_Held),
           when Retire => Idle));

   procedure Submit
     (Item : in out State; Result : out Admission; ID : out Submission_ID)
   is
   begin
      ID := No_Submission;
      if Item.Closing then
         Result := Closed;
      elsif Item.Stage /= Idle then
         Result := Busy;
      elsif Unsigned_64 (Item.Last_ID) >= Maximum_Identifier then
         Result := Exhausted;
      else
         Item.Last_ID := Item.Last_ID + 1;
         Item.Stage := Queued;
         ID := Item.Last_ID;
         Result := Accepted;
      end if;
   end Submit;

   procedure Apply
     (Item : in out State; ID : Submission_ID; Action : Event;
      Applied, Return_Buffer : out Boolean)
   is
   begin
      Applied := ID /= No_Submission and ID = Item.Last_ID and
        Allows (Item.Stage, Action);
      Return_Buffer := Applied and Action = Release_Buffer;
      if Applied then
         Item.Stage := Next_Phase (Item.Stage, Action);
      end if;
   end Apply;

   procedure Close (Item : in out State) is
   begin
      Item.Closing := True;
      case Item.Stage is
         when Queued => Item.Stage := Dropped_Held;
         when others => null;
      end case;
   end Close;
end CuBit.Presentation_State;
