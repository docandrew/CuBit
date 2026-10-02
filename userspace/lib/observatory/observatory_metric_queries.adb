package body Observatory_Metric_Queries with SPARK_Mode is
   function Valid_Page
     (Value : Reply; Requested : Cursor; Rows : P.Summary_Page) return Boolean is
   begin
      if not Admitted (Value, Requested) then return False; end if;
      for I in P.Row_Index loop
         if Unsigned_64 (I) < Value.Payload (0) and then
           not Observatory_Metric_Summaries.Valid_Row (Rows (I))
         then return False; end if;
         pragma Loop_Invariant
           (for all J in P.Row_Index'First .. I =>
               (if Unsigned_64 (J) < Value.Payload (0) then
                   Observatory_Metric_Summaries.Valid_Row (Rows (J))));
      end loop;
      return True;
   end Valid_Page;
end Observatory_Metric_Queries;
