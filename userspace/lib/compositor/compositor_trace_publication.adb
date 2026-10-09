package body Compositor_Trace_Publication with SPARK_Mode is
   procedure Prepare (S : in out State; Value : W.Event; Result : out Prepared) is
   begin
      Result := (Ready => False, others => <>);
      if not Valid_Content (Value) then
         S.Bad := Increment (S.Bad);
      elsif S.Next >= Sequence_Limit then
         S.Lost := Increment (S.Lost);
      else
         Result := (True, Identified (Value, S.Next));
         S.Next := S.Next + 1;
      end if;
   end Prepare;
   procedure Note_Refusal (S : in out State) is
   begin
      S.Lost := Increment (S.Lost);
   end Note_Refusal;
   procedure Note_Unsupported (S : in out State) is
   begin
      S.Unsupported_Count := Increment (S.Unsupported_Count);
   end Note_Unsupported;
end Compositor_Trace_Publication;
