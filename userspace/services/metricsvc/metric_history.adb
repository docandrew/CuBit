package body Metric_History with SPARK_Mode is
   procedure Append (S : in out State; Pid, Publisher, Batch,
      Producer_Dropped, Batch_Gaps : Unsigned_64; Value : R.Metric_Record;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Exhausted (S) then return; end if;
      S.Values (Index (S.Next)) :=
        (S.Next, Pid, Publisher, Batch, Producer_Dropped, Batch_Gaps, Value);
      S.Next := S.Next + 1;
      Accepted := True;
   end Append;
   procedure Read (S : State; Cursor : Unsigned_64; Value : out Event;
      Next, Gap : out Unsigned_64; Available, Valid_Cursor : out Boolean) is
      Effective : Unsigned_64;
   begin
      Value := (others => <>); Next := Cursor; Gap := 0;
      Available := False; Valid_Cursor := False;
      if Cursor = 0 or Cursor > S.Next then return; end if;
      Effective := Unsigned_64'Max (Cursor, First (S));
      Gap := Effective - Cursor; Next := Effective; Valid_Cursor := True;
      if Effective >= S.Next then return; end if;
      Value := S.Values (Index (Effective));
      if Value.Sequence /= Effective then
         Valid_Cursor := False; Value := (others => <>); return;
      end if;
      Next := Effective + 1; Available := True;
   end Read;
end Metric_History;
