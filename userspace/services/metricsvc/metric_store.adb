pragma Ada_2022;
package body Metric_Store with SPARK_Mode is
   use type Records.Record_Kind;
   use type Records.Unit;

   function Saturating_Add (Left, Right : Unsigned_64) return Unsigned_64 is
     (if Right > Unsigned_64'Last - Left then Unsigned_64'Last
      else Left + Right);

   procedure Advance_Time (Item : in out Store; Now_Ms : Unsigned_64) is
   begin
      Item.Now_Ms := Unsigned_64'Max (Item.Now_Ms, Now_Ms);
   end Advance_Time;

   --  Chooses the caller's source, a free slot, or the least recently used
   --  source idle beyond its lease. Found is False when the table is full.
   procedure Select_Source
     (Item : Store; Pid, Tag : Unsigned_64; Chosen : out Source_Index;
      Existing, Found : out Boolean)
     with Post => (if Existing then Found and Owned_By (Item, Chosen, Pid, Tag))
   is
      Free : Source_Count := 0;
      Idle : Source_Count := 0;
   begin
      Chosen := Source_Index'First;
      Existing := False;
      Found := False;
      for S in Source_Index loop
         if Owned_By (Item, S, Pid, Tag) then
            Chosen := S;
            Existing := True;
            Found := True;
            return;
         elsif not Item.Sources (S).Active then
            if Free = 0 then
               Free := S;
            end if;
         elsif Item.Now_Ms >= Item.Sources (S).Last_Use_Ms and then
           Item.Now_Ms - Item.Sources (S).Last_Use_Ms >= Source_Lease_Ms
           and then
           (Idle = 0 or else
            Item.Sources (S).Last_Use_Ms < Item.Sources (Idle).Last_Use_Ms)
         then
            Idle := S;
         end if;
      end loop;
      if Free /= 0 then
         Chosen := Free;
         Found := True;
      elsif Idle /= 0 then
         Chosen := Idle;
         Found := True;
      end if;
   end Select_Source;

   procedure Record_Sample
     (Target : in out Series; Measurement, Stamp : Unsigned_64;
      Replace_Total : Boolean) is
   begin
      Metric_Histograms.Add (Target.Samples, Measurement);
      if Replace_Total then
         Target.Total := Measurement;
      elsif Measurement > Unsigned_64'Last - Target.Total then
         Target.Total := Unsigned_64'Last;
         Target.Total_Saturated := True;
      else
         Target.Total := Target.Total + Measurement;
      end if;
      Target.Last_Time := Stamp;
   end Record_Sample;

   --  Applies one validated record to the caller's source.
   procedure Apply
     (Entry_Item : in out Source; Value : Records.Metric_Record;
      Accepted : out Boolean)
     with Post =>
       Entry_Item.Active = Entry_Item'Old.Active and then
       Entry_Item.Pid = Entry_Item'Old.Pid and then
       Entry_Item.Tag = Entry_Item'Old.Tag
   is
      Key : constant Records.Metric_Key := Value.Key;
      Measurement : Unsigned_64;
      Stamp : Unsigned_64;
   begin
      Accepted := False;
      if Value.Kind = Records.Describe then
         if not Entry_Item.Keys (Key).Declared then
            Entry_Item.Keys (Key) :=
              (Declared => True, Kind => Value.Declared,
               Measure => Value.Measure, Name => Value.Name, others => <>);
            Accepted := True;
         elsif Entry_Item.Keys (Key).Kind = Value.Declared and then
           Entry_Item.Keys (Key).Measure = Value.Measure and then
           Records.Same_Name (Entry_Item.Keys (Key).Name, Value.Name)
         then
            --  Repeated identical declarations are idempotent.
            Accepted := True;
         else
            Entry_Item.Keys (Key).Rejected :=
              Saturating_Add (Entry_Item.Keys (Key).Rejected, 1);
         end if;
         return;
      end if;
      if not Entry_Item.Keys (Key).Declared then
         Entry_Item.Rejected := Saturating_Add (Entry_Item.Rejected, 1);
         return;
      elsif Entry_Item.Keys (Key).Kind /= Value.Kind then
         Entry_Item.Keys (Key).Rejected :=
           Saturating_Add (Entry_Item.Keys (Key).Rejected, 1);
         return;
      end if;
      case Value.Kind is
         when Records.Describe =>
            return;
         when Records.Counter .. Records.Latency =>
            Measurement := Value.Value;
            Stamp := Value.Time_Us;
         when Records.Span =>
            Measurement := Value.End_Us - Value.Start_Us;
            Stamp := Value.End_Us;
      end case;
      Record_Sample
        (Entry_Item.Keys (Key), Measurement, Stamp,
         Replace_Total => Value.Kind = Records.Gauge);
      Accepted := True;
   end Apply;

   --  Adds one validated batch to its publisher's own source.
   procedure Absorb
     (Target : in out Source; Header : Records.Batch_Header;
      Page : Records.Page_Words; Now_Ms : Unsigned_64;
      Outcome : in out Ingest_Outcome)
     with Pre => Target.Active and then
                 Outcome.Accepted = 0 and then Outcome.Rejected = 0 and then
                 (Target.Next_Sequence = 0 or else
                  Header.Sequence >= Target.Next_Sequence),
          Post => Target.Active = Target'Old.Active and then
                  Target.Pid = Target'Old.Pid and then
                  Target.Tag = Target'Old.Tag
   is
      Sequence : constant Records.Batch_Sequence := Header.Sequence;
      Applied : Boolean;
   begin
      if Target.Next_Sequence /= 0 then
         Target.Batch_Gaps := Saturating_Add
           (Target.Batch_Gaps, Sequence - Target.Next_Sequence);
      end if;
      Target.Next_Sequence := Sequence + 1;
      Target.Batches := Saturating_Add (Target.Batches, 1);
      Target.Producer_Dropped := Header.Producer_Dropped;
      Target.Last_Use_Ms := Now_Ms;
      for I in 1 .. Header.Records loop
         pragma Loop_Invariant
           (Target.Active = Target'Loop_Entry.Active and
            Target.Pid = Target'Loop_Entry.Pid and
            Target.Tag = Target'Loop_Entry.Tag);
         pragma Loop_Invariant
           (Outcome.Accepted + Outcome.Rejected = I - 1);
         declare
            Decoded : constant Records.Decoded_Record :=
              Records.Decode (Records.Slot (Page, I));
         begin
            Applied := False;
            if Decoded.Success then
               Apply (Target, Decoded.Value, Applied);
            else
               Target.Rejected := Saturating_Add (Target.Rejected, 1);
            end if;
            if Applied then
               Outcome.Accepted := Outcome.Accepted + 1;
            else
               Outcome.Rejected := Outcome.Rejected + 1;
            end if;
         end;
      end loop;
   end Absorb;

   procedure Ingest
     (Item : in out Store; Pid, Tag : Unsigned_64;
      Page : Records.Page_Words; Bytes : Unsigned_64;
      Outcome : out Ingest_Outcome)
   is
      Header : constant Records.Decoded_Header :=
        Records.Decode_Header (Records.Slot (Page, 0), Bytes);
      Chosen : Source_Index;
      Existing, Found : Boolean;
      Sequence : Records.Batch_Sequence;
   begin
      Outcome := (others => <>);
      if not Header.Success then
         return;
      end if;
      Sequence := Header.Value.Sequence;
      Select_Source (Item, Pid, Tag, Chosen, Existing, Found);
      if not Found then
         Outcome.Result := Protocol.Exhausted;
         return;
      end if;
      if Existing and then Item.Sources (Chosen).Next_Sequence /= 0 and then
        Sequence < Item.Sources (Chosen).Next_Sequence
      then
         --  Replayed or regressed batch: reject without changing state.
         return;
      end if;
      if not Existing then
         Item.Sources (Chosen) :=
           (Active => True, Pid => Pid, Tag => Tag, others => <>);
      end if;
      pragma Assert (Owned_By (Item, Chosen, Pid, Tag));
      Absorb (Item.Sources (Chosen), Header.Value, Page, Item.Now_Ms,
              Outcome);
      Outcome.Result := Protocol.OK;
   end Ingest;

   function Summary
     (From : Source; Key : Records.Metric_Key) return Protocol.Summary_Row
   is
      use Protocol;
      Name_Shift : constant := 8;
      Value : constant Series := From.Keys (Key);
      Row : Summary_Row := [others => 0];
      Packed : Unsigned_64;
   begin
      Row (Row_Source) := From.Pid;
      Row (Row_Publisher_Tag) := From.Tag;
      Row (Row_Key) := Unsigned_64 (Key);
      Row (Row_Kind) := Records.Record_Kind'Enum_Rep (Value.Kind);
      Row (Row_Unit) := Records.Unit'Enum_Rep (Value.Measure);
      Row (Row_Count_Word) := Metric_Histograms.Count (Value.Samples);
      Row (Row_Minimum) := Metric_Histograms.Minimum (Value.Samples);
      Row (Row_Maximum) := Metric_Histograms.Maximum (Value.Samples);
      Row (Row_P50) := Metric_Histograms.Quantile_Upper (Value.Samples, P50);
      Row (Row_P90) := Metric_Histograms.Quantile_Upper (Value.Samples, P90);
      Row (Row_P95) := Metric_Histograms.Quantile_Upper (Value.Samples, P95);
      Row (Row_P99) := Metric_Histograms.Quantile_Upper (Value.Samples, P99);
      Row (Row_P999) :=
        Metric_Histograms.Quantile_Upper (Value.Samples, P999);
      Row (Row_Total) := Value.Total;
      Row (Row_Flags) :=
        (if Value.Total_Saturated then Flag_Total_Saturated else 0)
        or (if Metric_Histograms.Saturated (Value.Samples)
            then Flag_Histogram_Saturated else 0);
      Row (Row_Series_Rejected) := Value.Rejected;
      Row (Row_Last_Time) := Value.Last_Time;
      Row (Row_Source_Batches) := From.Batches;
      Row (Row_Source_Batch_Gaps) := From.Batch_Gaps;
      Row (Row_Source_Producer_Dropped) := From.Producer_Dropped;
      Row (Row_Source_Rejected) := From.Rejected;
      for W in 0 .. Records.Name_Words - 1 loop
         Packed := 0;
         for B in reverse 1 .. Records.Bytes_Per_Word loop
            Packed := Shift_Left (Packed, Name_Shift) or
              Unsigned_64 (Value.Name.Bytes (W * Records.Bytes_Per_Word + B));
         end loop;
         Row (Row_First_Name + W) := Packed;
      end loop;
      return Row;
   end Summary;

   procedure Fill_Summaries
     (Item : Store; Cursor : Series_Cursor; Rows : out Protocol.Summary_Page;
      Written : out Protocol.Row_Count; Next : out Series_Cursor)
   is
      use Protocol;
      S : Source_Index;
      K : Records.Metric_Key;
   begin
      Rows := [others => [others => 0]];
      Written := 0;
      for Ordinal in Cursor .. Series_Slots - 1 loop
         pragma Loop_Invariant (Written <= Rows_Per_Page);
         if Written = Rows_Per_Page then
            Next := Ordinal;
            return;
         end if;
         S := Ordinal / Records.Maximum_Keys + 1;
         K := Ordinal mod Records.Maximum_Keys + 1;
         if Item.Sources (S).Active and then
           Item.Sources (S).Keys (K).Declared
         then
            Rows (Written) := Summary (Item.Sources (S), K);
            Written := Written + 1;
         end if;
      end loop;
      Next := Series_Slots;
   end Fill_Summaries;
end Metric_Store;
