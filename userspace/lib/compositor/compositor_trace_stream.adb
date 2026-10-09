package body Compositor_Trace_Stream with SPARK_Mode is
   use type R.Record_Kind;
   function Add (A, B : W.Word) return W.Word is
     (if B > W.Word'Last - A then W.Word'Last else A + B);

   procedure Start (S : out State; Incarnation, Requested_Cursor : W.Word) is
   begin
      S := (Endpoint => Incarnation, Next => Requested_Cursor, others => <>);
   end Start;

   procedure Discard_Partial (S : in out State) is
   begin
      if S.Used > 0 then
         S.Stats.Abandoned_Events := Increment (S.Stats.Abandoned_Events);
      end if;
      S.Used := 0;
   end Discard_Partial;

   procedure Feed
     (S : in out State; Incarnation : W.Word; Row : P.Raw_Row;
      Result : out Capture) is
      Words : R.Slot_Words;
      Decoded : R.Decoded_Record;
      Complete : W.Decoded;
   begin
      Result := (Success => False, others => <>);
      if Incarnation = 0 or Incarnation /= S.Endpoint then
         Discard_Partial (S);
         S.Stats.Endpoint_Mismatches := Increment (S.Stats.Endpoint_Mismatches);
         return;
      end if;
      if Row (0) = 0 or Row (0) = W.Word'Last or Row (1) = 0 or
        not P.Is_Publisher (Row (2)) or Row (3) = 0 or
        Row (6) /= 0 or Row (7) /= 0 or Row (0) < S.Next
      then
         Discard_Partial (S);
         S.Stats.Rejected_Rows := Increment (S.Stats.Rejected_Rows);
         return;
      end if;
      if Row (0) > S.Next then
         Discard_Partial (S);
         S.Stats.Skipped_Rows := Add (S.Stats.Skipped_Rows, Row (0) - S.Next);
      end if;
      S.Next := Row (0) + 1;
      for I in R.Slot_Word_Index loop Words (I) := Row (8 + I); end loop;
      Decoded := R.Decode (Words);
      if not Decoded.Success then
         Discard_Partial (S);
         S.Stats.Rejected_Rows := Increment (S.Stats.Rejected_Rows);
         return;
      end if;
      if Decoded.Value.Kind /= R.Trace or else Decoded.Value.Key /= M.Schema then
         Discard_Partial (S);
         return;
      end if;
         if Decoded.Value.Part = 0 then
            Discard_Partial (S);
            S.Rows (0) := (Row (0), Row (1), Row (2), Row (3), Decoded.Value);
            S.Drops := Row (4); S.Gaps := Row (5);
            S.Used := 1;
            return;
         end if;
         if S.Used = 0 or else Decoded.Value.Part /= S.Used or else
           Row (1) /= S.Rows (0).Pid or else Row (2) /= S.Rows (0).Publisher or else
           Row (3) /= S.Rows (0).Batch or else Row (4) /= S.Drops or else
           Row (5) /= S.Gaps or else Decoded.Value.Trace_ID /= S.Rows (0).Value.Trace_ID
         then
            Discard_Partial (S);
            S.Stats.Rejected_Rows := Increment (S.Stats.Rejected_Rows);
            return;
         end if;
         S.Rows (S.Used) := (Row (0), Row (1), Row (2), Row (3), Decoded.Value);
         if S.Used < 3 then S.Used := S.Used + 1; return; end if;
         Complete := M.Assemble (S.Rows);
         if Complete.Success then
            Result := (True, S.Endpoint, Row (1), Row (2), Row (3),
                       S.Rows (0).Sequence, S.Drops, S.Gaps, Complete.Value);
            S.Stats.Emitted_Events := Increment (S.Stats.Emitted_Events);
            S.Used := 0;
         else
            Discard_Partial (S);
            S.Stats.Rejected_Rows := Increment (S.Stats.Rejected_Rows);
         end if;
   end Feed;
end Compositor_Trace_Stream;
