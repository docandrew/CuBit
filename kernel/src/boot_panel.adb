pragma Ada_2022;
package body Boot_Panel with SPARK_Mode is
   function Fit (Text : String) return Line is
      Result : Line := [others => ' '];
      Used : Natural range 0 .. Columns := 0;
   begin
      for C of Text loop
         exit when Used = Columns;
         Used := Used + 1;
         Result (Used) := (if C in ' ' .. '~' then C else '?');
      end loop;
      return Result;
   end Fit;
   procedure Initialize (S : in out State) is
   begin
      if S.Life = Unavailable then
         S.Life := Active;
         S.Text (Heading) := Fit ("CuBit / boot diagnostics");
         S.Text (Current_Step) := Fit ("Initializing kernel");
      end if;
   end Initialize;
   procedure Retire (S : in out State) is
   begin
      S.Life := Retired;
   end Retire;
   procedure Begin_Step (S : in out State; Text : String) is
   begin
      if S.Life = Active and then not S.Stopped then
         S.Text (Current_Step) := Fit (Text);
      end if;
   end Begin_Step;
   procedure Complete_Step (S : in out State; Text : String) is
   begin
      if S.Life = Active and then not S.Stopped then
         S.Text (Last_Completed) := Fit (Text);
      end if;
   end Complete_Step;
   procedure Fail (S : in out State; Text : String) is
   begin
      if S.Life = Active and then not S.Stopped then
         S.Stopped := True;
         S.Text (Current_Step) := Fit ("Boot stopped - first failure retained below");
         S.Text (First_Error) := Fit (Text);
      end if;
   end Fail;
   procedure Append (S : in out State; C : Character; Changed : out Boolean) is
   begin
      Changed := False;
      if S.Life /= Active or else S.Stopped then return; end if;
      if C = ASCII.LF then
         if S.Used > 0 then
            S.Text (Latest_Detail) := S.Pending;
            Changed := True;
         end if;
         S.Pending := [others => ' '];
         S.Used := 0;
      elsif C /= ASCII.CR and then C /= ASCII.NUL and then S.Used < Columns then
         S.Used := S.Used + 1;
         S.Pending (S.Used) := (if C in ' ' .. '~' then C else '?');
      end if;
   end Append;
end Boot_Panel;
