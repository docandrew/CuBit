package body Config_Fixture is
   use Config_Objects;
   use type Number;
   Token : Number := 0;
   procedure Load_Empty (Object : in out State) is
      Result : Outcome;
      Empty : CCL.Objects.Image;
   begin
      Attach (Object, 1, Result); pragma Assert (Result = Accepted);
      Token := Token + 1;
      Begin_Load (Object, Token, Result); pragma Assert (Result = Accepted);
      Finish_Load (Object, 1, Token, Absent, 0, Empty, Result);
      pragma Assert (Result = Accepted);
   end Load_Empty;
   procedure Commit
     (Object : in out State; Value : CCL.Objects.Image;
      Expected : Number; Result : out Outcome) is
   begin
      Token := Token + 1;
      Begin_Commit (Object, Value, Expected, Token, Result);
      if Result = Accepted then
         Finish_Commit (Object, Session (Object), Token, Committed, Expected + 1, Result);
      end if;
   end Commit;
end Config_Fixture;
