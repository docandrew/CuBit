package body Compositor_Frame_Trace with SPARK_Mode is
   procedure Add (S : in out State; Value : Record_Value) is
   begin
      if not Valid (Value) then
         if S.Bad < Natural'Last then S.Bad := S.Bad + 1; end if;
      elsif S.Used = Maximum_Records then
         if S.Dropped < Natural'Last then S.Dropped := S.Dropped + 1; end if;
      else
         S.Values (S.Used + 1) := Value;
         S.Used := S.Used + 1;
      end if;
   end Add;
   procedure Reset (S : out State) is
   begin
      S := (others => <>);
   end Reset;
end Compositor_Frame_Trace;
