with CCL.VM;
with CuBit.Metric_Records;
with Observatory_Metric_Summaries;
package body Observatory_CCL with SPARK_Mode is
   package H renames CCL.Host_Values;
   package R renames CuBit.Metric_Records;
   package V renames Observatory_Metric_Summaries;
   use type H.Value_Kind;
   use type CCL.Catalog.Catalog_Error;
   function Name (Op : Operation) return String is
     (case Op is
         when Ready => "ready",
         when Row_Count => "count",
         when Metric_Name => "name",
         when Unit_Name => "unit",
         when Source => "source",
         when Publisher => "publisher",
         when Samples => "samples",
         when Minimum => "minimum",
         when Maximum => "maximum",
         when P50 => "p50-upper",
         when P99 => "p99-upper",
         when Total => "total",
         when Dropped => "dropped",
         when Batch_Gaps => "batch-gaps",
         when Lossy => "lossy",
         when Saturated => "saturated");
   procedure Publish (Catalog : in out CCL.Catalog.Interface_Catalog;
      Error : out CCL.Catalog.Catalog_Error) is
      D : CCL.Catalog.Interface_Descriptor;
      O : CCL.Catalog.Operation_Descriptor;
   begin
      CCL.Catalog.Define_Interface ("metrics", 1, 0,
         [16#1B4DEB7660864013#, 16#63E04991EB5DF56B#, 16#DFCA65FD886D0B42#, 16#263C895EFA97025B#], D, Error);
      if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      for Op in Operation loop
         CCL.Catalog.Define_Host_Operation
           (Name (Op), (if Op in Ready | Row_Count then 0 else 1),
            (Argument => H.Integer_Value,
             Result => (if Op in Ready | Lossy | Saturated then H.Boolean_Value
                        elsif Op = Row_Count then H.Integer_Value else H.Text_Value),
             Result_Text_Limit => (if Op = Metric_Name then 32
               elsif Op in Ready | Row_Count | Lossy | Saturated then 0 else 20),
             Authority => CCL.VM.Observe_Authority, others => <>), O, Error);
         if Error /= CCL.Catalog.Catalog_Valid then return; end if;
         CCL.Catalog.Add_Operation (D, O, Error);
         if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      end loop;
      CCL.Catalog.Publish (Catalog, D, Error);
   end Publish;
   procedure Clear (Item : in out Context) is
   begin Item.Available := False; Item.Count := 0; end Clear;
   procedure Replace (Item : in out Context; Rows : P.Summary_Page;
      Count : P.Row_Count; Accepted : out Boolean) is
   begin
      Clear (Item); Accepted := False;
      if not (for all I in P.Row_Index =>
                  (if I < Count then V.Valid_Row (Rows (I)))) then return; end if;
      Item.Page := Rows; Item.Count := Count; Item.Available := True; Accepted := True;
   end Replace;
   procedure Invoke (Item : in out Context; Host_Binding : Unsigned_32;
      Argument : H.Value; Reply : out H.Call_Result) is
      Op : Operation;
      Row : P.Summary_Row;
      Column : P.Row_Word_Index;
      procedure Text (Value : String) is
         Buffer : H.Text;
      begin
         H.Copy_Text (Value, Buffer, Reply.Success);
         Reply.Value := H.Text_Constant (Buffer);
      end Text;
      procedure Number (Value : Unsigned_64) is
         Image : constant String := Value'Image;
      begin
         if Image'Length > 1 and then Image (Image'First) = ' ' then
            Text (Image (Image'First + 1 .. Image'Last));
         else Text (Image); end if;
      end Number;
   begin
      Reply := (Value => H.Integer_Constant (0), Success => False, Why => <>);
      if Host_Binding < Binding (Operation'First) or else Host_Binding > Binding (Operation'Last)
        or else Argument.Kind /= H.Integer_Value then return; end if;
      Op := Operation'Val (Host_Binding - Binding_Base);
      if Op in Ready | Row_Count then
         if Argument.Integer /= 0 then return; end if;
         Reply.Success := True;
         Reply.Value := (if Op = Ready then H.Boolean_Constant (Item.Available)
                         else H.Integer_Constant (Integer_64 (Item.Count)));
         return;
      end if;
      if not Item.Available or else Argument.Integer < 0 or else
        Argument.Integer >= Integer_64 (Item.Count) then return; end if;
      Row := Item.Page (P.Row_Index (Argument.Integer));
      if Op = Metric_Name or else Op = Unit_Name then
         declare
            D : constant R.Decoded_Record := V.Declaration (V.Decode (Row).Value);
            Image : String (1 .. R.Maximum_Name_Bytes) := [others => ' '];
         begin
            if Op = Unit_Name then
               Text ((case D.Value.Measure is when R.Count => "count", when R.Microseconds => "us",
                      when R.Nanoseconds => "ns", when R.Bytes => "bytes"));
            else
               for I in 1 .. D.Value.Name.Length loop
                  Image (I) := Character'Val (D.Value.Name.Bytes (I));
               end loop;
               Text (Image (1 .. D.Value.Name.Length));
            end if;
         end;
         return;
      elsif Op in Lossy | Saturated then
         Reply.Success := True;
         Reply.Value := H.Boolean_Constant
           ((if Op = Saturated then Row (P.Row_Flags) /= 0
             else (Row (P.Row_Series_Rejected) or Row (P.Row_Source_Rejected) or
                   Row (P.Row_Source_Batch_Gaps) or Row (P.Row_Source_Producer_Dropped)) /= 0));
         return;
      end if;
      if Op in Minimum | Maximum | P50 | P99 and then Row (P.Row_Count_Word) = 0 then
         Text ("unavailable"); return;
      end if;
      Column := (case Op is
         when Source => P.Row_Source, when Publisher => P.Row_Publisher_Tag,
         when Samples => P.Row_Count_Word, when Minimum => P.Row_Minimum,
         when Maximum => P.Row_Maximum, when P50 => P.Row_P50, when P99 => P.Row_P99,
         when Total => P.Row_Total, when Dropped => P.Row_Source_Producer_Dropped,
         when Batch_Gaps => P.Row_Source_Batch_Gaps, when others => P.Row_Key);
      Number (Row (Column));
   end Invoke;
end Observatory_CCL;
