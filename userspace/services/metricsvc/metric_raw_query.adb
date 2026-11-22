with CuBit.Metric_Records;
package body Metric_Raw_Query with SPARK_Mode is
   package R renames CuBit.Metric_Records;
   procedure Fill (Store : Metric_Store.Store; Cursor : Unsigned_64;
      Page : out P.Raw_Page; Written : out P.Raw_Row_Count;
      Next, Gap : out Unsigned_64; Valid : out Boolean) is
      E : Metric_Store.Raw.Event;
      After, Lost : Unsigned_64;
      Present, Good : Boolean;
      Encoded : R.Slot_Words;
   begin
      Page := (others => (others => 0)); Written := 0;
      Next := Cursor; Gap := 0; Valid := False;
      if Cursor = 0 or Cursor > Metric_Store.History_Next (Store) then return; end if;
      for I in P.Raw_Row_Index loop
         pragma Loop_Invariant (Next >= Cursor and Next <= Metric_Store.History_Next (Store));
         pragma Loop_Invariant (Written = I);
         Metric_Store.Read_History (Store, Next, E, After, Lost, Present, Good);
         if not Good then return; end if;
         if I = 0 then Gap := Lost; end if;
         Next := After; Valid := True;
         if not Present then return; end if;
         Page (I) (0) := E.Sequence;
         Page (I) (1) := E.Pid;
         Page (I) (2) := E.Publisher;
         Page (I) (3) := E.Batch;
         Page (I) (4) := E.Producer_Dropped;
         Page (I) (5) := E.Batch_Gaps;
         Encoded := R.Encode (E.Value);
         for W in R.Slot_Word_Index loop
            Page (I) (8 + W) := Encoded (W);
         end loop;
         Written := Written + 1;
      end loop;
   end Fill;
end Metric_Raw_Query;
