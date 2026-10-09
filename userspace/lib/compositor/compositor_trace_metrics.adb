pragma Ada_2022;
package body Compositor_Trace_Metrics with SPARK_Mode is
   function Fragment (Value : W.Event) return R.Trace_Group is
      Data : constant W.Packet := W.Encode (Value);
      Result : R.Trace_Group;
   begin
      for I in R.Trace_Part loop
         Result (I) := (R.Trace, Schema, Value.Event_ID, I,
                       [for J in R.Trace_Part => Data (I * 4 + J)]);
         pragma Loop_Invariant
           (for all J in R.Trace_Part'First .. I =>
              R.Valid (Result (J)) and Result (J).Part = J and
              Result (J).Trace_ID = Value.Event_ID and Result (J).Key = Schema);
         pragma Loop_Invariant
           (for all J in R.Trace_Part'First .. I =>
              Result (J) = (R.Trace, Schema, Value.Event_ID, J,
                            Part_Data (Data, J)));
      end loop;
      return Result;
   end Fragment;

   function Assemble (Rows : Group) return W.Decoded is
      Data : W.Packet := (others => 0);
      Result : W.Decoded;
   begin
      if not Coherent (Rows) then return (Success => False); end if;
      for I in R.Trace_Part loop
         for J in R.Trace_Part loop
            Data (I * 4 + J) := Rows (I).Value.Data (J);
         end loop;
      end loop;
      W.Lemma_Decoded_Valid (Data);
      Result := W.Decode (Data);
      if Result.Success and then Result.Value.Event_ID = Rows (0).Value.Trace_ID then
         return Result;
      else
         return (Success => False);
      end if;
   end Assemble;
end Compositor_Trace_Metrics;
