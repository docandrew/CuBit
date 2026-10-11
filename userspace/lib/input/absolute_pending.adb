package body Absolute_Pending with SPARK_Mode is
   procedure Append
     (S : in out State; R : Report; Observed_Ms : Word;
      Outcome : out Append_Outcome)
   is
      Lost : Boolean;
   begin
      if Can_Replace (S, R) then
         S.Newest := Merge (S.Newest, R);
         Input_Pending.Replace_Newest (S.Pending, Encode (S.Newest));
         Outcome := Replaced;
         return;
      end if;
      Input_Pending.Append (S.Pending, Encode (R), Lost, Observed_Ms);
      S.Mergeable := not Lost and then S.Known and then
        R.Buttons = S.Newest.Buttons and then R.Flags = S.Newest.Flags;
      S.Previous := S.Newest;
      S.Newest := R;
      S.Known := True;
      Outcome := (if Lost then Overflowed else Appended);
   end Append;

   procedure Acknowledge (S : in out State) is
   begin
      Input_Pending.Acknowledge (S.Pending);
   end Acknowledge;

   procedure Mark_Lost (S : in out State) is
   begin
      Input_Pending.Require_Recovery (S.Pending);
      S.Mergeable := False;
   end Mark_Lost;

   procedure Reset (S : in out State) is
   begin
      Input_Pending.Reset (S.Pending);
      S.Known := False;
      S.Mergeable := False;
   end Reset;
end Absolute_Pending;
