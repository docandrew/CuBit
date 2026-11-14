with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;
with CCL.Interface_Sources;

package body CCL.Interfaces.Metrics with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Objects.Catalog.Publication_Result;

   SERIES_FIELDS : constant := 14;
   POINT_FIELDS : constant := 2;
   ROLLUP_FIELDS : constant := 8;
   GAP_FIELDS : constant := 4;
   --  Cells per item: a series' record cell, ten integers, four texts and
   --  two enums (member and unit payload each); a point's member, record
   --  and two integers; a rollup's member, record and eight integers; a
   --  gap's member, record, three integers and its cause.
   SERIES_CELLS : constant := 1 + 10 + 4 + 2 * 2;
   POINT_CELLS : constant := 2 + POINT_FIELDS;
   ROLLUP_CELLS : constant := 2 + ROLLUP_FIELDS;
   GAP_CELLS : constant := 2 + 3 + 2;
   LARGEST_CELLS : constant := SERIES_CELLS;
   LARGEST_TEXT : constant := 4 * 64;

   type Item_Member is (Point, Rollup, Gap);
   function Member_Cell (Member : Item_Member) return CCL.Objects.Cell is
     (CCL.Objects.Variant_Cell (Item_Member'Pos (Member) + 1));

   procedure Define_Types (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean) is
      Specialized : CCL.Types.List_Result;
      Unbound : CCL.Objects.Binding;
      Series_List, Item_List : CCL.Types.Type_Reference;
      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Interface_Sources.Named_Type (Types, Name));
   begin
      Bound := (others => Unbound);
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then
         CCL.Types.Specialize_List (Types, Named ("Metric_Series"), Series_List, Specialized);
         Accepted := Specialized in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      end if;
      if Accepted then
         CCL.Types.Specialize_List (Types, Named ("Metric_Item"), Item_List, Specialized);
         Accepted := Specialized in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      end if;
      if Accepted then CCL.Objects.Bind (Types, Series_List, SERIES_KEY, Bound.Series_List, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Item_List, ITEMS_KEY, Bound.Items, Accepted); end if;
      if Accepted then
         CCL.Objects.Bind (Types, Named ("Metric_Selector"), SELECTOR_KEY, Bound.Selector, Accepted);
      end if;
      if Accepted then
         CCL.Objects.Bind (Types, Named ("Metric_Range"), RANGE_KEY, Bound.Range_Record, Accepted);
      end if;
      if Accepted then
         CCL.Objects.Bind (Types, Named ("Metric_Window"), WINDOW_KEY, Bound.Window_Record, Accepted);
      end if;
      if Accepted then
         CCL.Objects.Bind (Types, Named ("Metric_Stats"), STATS_KEY, Bound.Stats, Accepted);
      end if;
   end Define_Types;

   procedure Publish (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error) is
      Types : CCL.Types.Registry := CCL.Catalog.Visible_Types (Item);
      Bound : Contracts;
      Accepted : Boolean;
      Published : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      --  Reading series observes.
      function Contract_Of (Op : Operation) return CCL.Host_Values.Import_Declaration is
        (case Op is
            when Series =>
              (Argument => CCL.Host_Values.Object_Value, Argument_Schema => SELECTOR_KEY,
               Result => CCL.Host_Values.Object_Value, Result_Schema => SERIES_KEY,
               Authority => CCL.VM.Observe_Authority, others => <>),
            when Range_Of =>
              (Argument => CCL.Host_Values.Object_Value, Argument_Schema => RANGE_KEY,
               Result => CCL.Host_Values.Object_Value, Result_Schema => ITEMS_KEY,
               Authority => CCL.VM.Observe_Authority, others => <>),
            when Window =>
              (Argument => CCL.Host_Values.Object_Value, Argument_Schema => WINDOW_KEY,
               Result => CCL.Host_Values.Object_Value, Result_Schema => STATS_KEY,
               Authority => CCL.VM.Observe_Authority, others => <>));
      procedure Publish_One (Contract : CCL.Objects.Binding; Done : out Boolean) is
      begin
         CCL.Catalog.Publish_Schema (Item, Contract, Published);
         Done := Published in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published;
      end Publish_One;
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Bound, Accepted);
      if Accepted then Publish_One (Bound.Series_List, Accepted); end if;
      if Accepted then Publish_One (Bound.Items, Accepted); end if;
      if Accepted then Publish_One (Bound.Selector, Accepted); end if;
      if Accepted then Publish_One (Bound.Range_Record, Accepted); end if;
      if Accepted then Publish_One (Bound.Window_Record, Accepted); end if;
      if Accepted then Publish_One (Bound.Stats, Accepted); end if;
      if not Accepted then return; end if;
      CCL.Catalog.Define_Interface ("metrics", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation (Name (Op), 1, Contract_Of (Op), Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;

   procedure Start (Contract : CCL.Objects.Binding; Image : out CCL.Objects.Image) is
      Built : CCL.Objects.Build_Result;
   begin
      Image := CCL.Objects.Empty (Contract);
      CCL.Objects.Append (Image, CCL.Objects.Sequence_Cell (0), Built);
   end Start;

   function As_Integer (Value : Unsigned_64) return Integer_64 is
     (if Value > Unsigned_64 (Integer_64'Last) then Integer_64'Last else Integer_64 (Value));

   function Room (Image : CCL.Objects.Image; Cells, Text_Bytes : Natural) return Boolean is
     (Image.Used_Cells >= 1 and then
      Unsigned_32 (CCL.Objects.Maximum_Cells) - Image.Used_Cells >= Unsigned_32 (Cells) and then
      Text_Bytes <= CCL.Objects.Maximum_Text_Bytes and then
      Unsigned_32 (Text_Bytes) <= Unsigned_32 (CCL.Objects.Maximum_Text_Bytes) - Image.Used_Bytes and then
      Image.Cells (1).First < Unsigned_64 (CCL.Objects.Maximum_Cells));

   function Has_Room (Image : CCL.Objects.Image) return Boolean is
     (Room (Image, LARGEST_CELLS + GAP_CELLS, LARGEST_TEXT));

   procedure Count_One (Image : in out CCL.Objects.Image) is
   begin
      Image.Cells (1) := CCL.Objects.Sequence_Cell (Natural (Image.Cells (1).First) + 1);
   end Count_One;

   procedure Number (Image : in out CCL.Objects.Image; Value : Unsigned_64; Built : in out CCL.Objects.Build_Result) is
   begin
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Integer_Cell (As_Integer (Value)), Built);
      end if;
   end Number;
   procedure Member (Image : in out CCL.Objects.Image; Position : Natural; Built : in out CCL.Objects.Build_Result) is
   begin
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Variant_Cell (Position + 1), Built);
      end if;
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Unit_Cell, Built);
      end if;
   end Member;
   procedure Text (Image : in out CCL.Objects.Image; Value : String; Built : in out CCL.Objects.Build_Result) is
   begin
      if Built = CCL.Objects.Added then
         CCL.Objects.Append_Text (Image, Value, Built);
      end if;
   end Text;

   procedure Add_Series
     (Image : in out CCL.Objects.Image; Row : Series_Row; Name, Source, Program, Instance : String;
      Added : out Boolean)
   is
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      Added := Room (Image, SERIES_CELLS, Name'Length + Source'Length + Program'Length + Instance'Length);
      if not Added then return; end if;
      CCL.Objects.Append (Image, CCL.Objects.Product_Cell (SERIES_FIELDS), Built);
      Number (Image, Row.Id, Built);
      Text (Image, Name, Built);
      Text (Image, Source, Built);
      Text (Image, Program, Built);
      Text (Image, Instance, Built);
      Member (Image, Row.Kind, Built);
      Member (Image, Row.Measure, Built);
      Number (Image, Row.Published, Built);
      Number (Image, Row.Retained, Built);
      Number (Image, Row.Shed, Built);
      Number (Image, Row.Evicted, Built);
      Number (Image, Row.First_Us, Built);
      Number (Image, Row.Last_Us, Built);
      Number (Image, Row.Last_Value, Built);
      Added := Built = CCL.Objects.Added;
      if Added then Count_One (Image); end if;
   end Add_Series;

   procedure Add_Point (Image : in out CCL.Objects.Image; Time_Us, Value : Unsigned_64; Added : out Boolean) is
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      Added := Room (Image, POINT_CELLS, 0);
      if not Added then return; end if;
      CCL.Objects.Append (Image, Member_Cell (Point), Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Product_Cell (POINT_FIELDS), Built);
      end if;
      Number (Image, Time_Us, Built);
      Number (Image, Value, Built);
      Added := Built = CCL.Objects.Added;
      if Added then Count_One (Image); end if;
   end Add_Point;

   procedure Add_Rollup (Image : in out CCL.Objects.Image; Row : Rollup_Row; Added : out Boolean) is
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      Added := Room (Image, ROLLUP_CELLS, 0);
      if not Added then return; end if;
      CCL.Objects.Append (Image, Member_Cell (Rollup), Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Product_Cell (ROLLUP_FIELDS), Built);
      end if;
      Number (Image, Row.Start_Us, Built);
      Number (Image, Row.Count, Built);
      Number (Image, Row.Sum, Built);
      Number (Image, Row.Minimum, Built);
      Number (Image, Row.Maximum, Built);
      Number (Image, Row.Last, Built);
      Number (Image, Row.P50, Built);
      Number (Image, Row.P99, Built);
      Added := Built = CCL.Objects.Added;
      if Added then Count_One (Image); end if;
   end Add_Rollup;

   procedure Add_Gap
     (Image : in out CCL.Objects.Image; First_Us, Last_Us, Count : Unsigned_64; Cause : Gap_Cause;
      Added : out Boolean)
   is
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      Added := Room (Image, GAP_CELLS, 0);
      if not Added then return; end if;
      CCL.Objects.Append (Image, Member_Cell (Gap), Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Product_Cell (GAP_FIELDS), Built);
      end if;
      Number (Image, First_Us, Built);
      Number (Image, Last_Us, Built);
      Number (Image, Count, Built);
      Member (Image, Gap_Cause'Pos (Cause), Built);
      Added := Built = CCL.Objects.Added;
      if Added then Count_One (Image); end if;
   end Add_Gap;

   procedure Stats_Value
     (Contract : CCL.Objects.Binding; Values : Stats_Words; Result : out CCL.Objects.Image; Built : out Boolean)
   is
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      Result := CCL.Objects.Empty (Contract);
      CCL.Objects.Append (Result, CCL.Objects.Product_Cell (Stats_Fields), Step);
      for V of Values loop
         Number (Result, V, Step);
      end loop;
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Result, Contract);
   end Stats_Value;
end CCL.Interfaces.Metrics;
