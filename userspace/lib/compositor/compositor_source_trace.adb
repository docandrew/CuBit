package body Compositor_Source_Trace with SPARK_Mode is
   procedure Add (S : in out State; Value : Record_Value) is
   begin
      if not Valid (Value) then
         S.Bad := Increment (S.Bad);
      elsif S.Used = Maximum_Records then
         S.Dropped := Increment (S.Dropped);
      else
         S.Values (S.Used + 1) := Value;
         S.Used := S.Used + 1;
      end if;
   end Add;
   procedure Reset (S : out State) is
   begin
      S := (others => <>);
   end Reset;
end Compositor_Source_Trace;
