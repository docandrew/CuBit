with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;

with CCL.Interface_Sources;

package body CCL.Interfaces.Logs with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Objects.Catalog.Publication_Result;

   --  Fields of a LogEntry, a LogGap and a LogTally, in order.
   ENTRY_FIELDS : constant := 4;
   GAP_FIELDS : constant := 5;
   TALLY_FIELDS : constant := 7;
   --  LogItem's members, in order.
   type Item_Member is (Logged, Gap, Tally);
   function Member_Cell (Member : Item_Member) return CCL.Objects.Cell is
     (CCL.Objects.Variant_Cell (Item_Member'Pos (Member) + 1));


   procedure Define_Types
     (Types : in out CCL.Types.Registry; Entries : out CCL.Types.Type_Reference;
      Contract, Severity_Contract, Window_Contract : out CCL.Objects.Binding; Accepted : out Boolean)
   is
      Specialized : CCL.Types.List_Result;
      Unbound : CCL.Objects.Binding;
      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Interface_Sources.Named_Type (Types, Name));
   begin
      Entries := CCL.Types.Invalid_Type;
      Contract := Unbound;
      Severity_Contract := Unbound;
      Window_Contract := Unbound;
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then
         CCL.Types.Specialize_List (Types, Named ("LogItem"), Entries, Specialized);
         Accepted := Specialized in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      end if;
      if Accepted then CCL.Objects.Bind (Types, Entries, SCHEMA_KEY, Contract, Accepted); end if;
      if Accepted then
         CCL.Objects.Bind (Types, Named ("Severity"), SEVERITY_KEY, Severity_Contract, Accepted);
      end if;
      if Accepted then
         CCL.Objects.Bind (Types, Named ("LogWindow"), WINDOW_KEY, Window_Contract, Accepted);
      end if;
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Types : CCL.Types.Registry := CCL.Catalog.Visible_Types (Item);
      Entries : CCL.Types.Type_Reference;
      Contract, Severity_Contract, Window_Contract : CCL.Objects.Binding;
      Accepted : Boolean;
      Published : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      --  Reading logs, or what logstore keeps, observes; changing what it
      --  keeps controls.
      function Contract_Of (Op : Operation) return CCL.Host_Values.Import_Declaration is
        (case Op is
            when Recent =>
              (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => MAX_SERVICE_NAME,
               Result => CCL.Host_Values.Object_Value, Result_Schema => SCHEMA_KEY,
               Authority => CCL.VM.Observe_Authority, others => <>),
            when Between =>
              (Argument => CCL.Host_Values.Object_Value, Argument_Schema => WINDOW_KEY,
               Result => CCL.Host_Values.Object_Value, Result_Schema => SCHEMA_KEY,
               Authority => CCL.VM.Observe_Authority, others => <>),
            when Minimum =>
              (Argument => CCL.Host_Values.Integer_Value,
               Result => CCL.Host_Values.Object_Value, Result_Schema => SEVERITY_KEY,
               Authority => CCL.VM.Observe_Authority, others => <>),
            when Set_Minimum =>
              (Argument => CCL.Host_Values.Object_Value, Argument_Schema => SEVERITY_KEY,
               Result => CCL.Host_Values.Object_Value, Result_Schema => SEVERITY_KEY,
               Authority => CCL.VM.Control_Authority, others => <>));
      function Parameters (Op : Operation) return Natural is (if Op = Minimum then 0 else 1);
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Entries, Contract, Severity_Contract, Window_Contract, Accepted);
      if not Accepted then return; end if;
      CCL.Catalog.Publish_Schema (Item, Contract, Published);
      if Published not in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published then
         return;
      end if;
      CCL.Catalog.Publish_Schema (Item, Severity_Contract, Published);
      if Published not in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published then
         return;
      end if;
      CCL.Catalog.Publish_Schema (Item, Window_Contract, Published);
      if Published not in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published then
         return;
      end if;
      CCL.Catalog.Define_Interface ("logs", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation (Name (Op), Parameters (Op), Contract_Of (Op), Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;

   procedure Severity_Value
     (Contract : CCL.Objects.Binding; Level : Severity; Result : out CCL.Objects.Image; Built : out Boolean)
   is
      Step : CCL.Objects.Build_Result;
   begin
      Result := CCL.Objects.Empty (Contract);
      CCL.Objects.Append (Result, CCL.Objects.Variant_Cell (Severity'Pos (Level) + 1), Step);
      --  A member without a payload still carries its Unit payload cell.
      if Step = CCL.Objects.Added then
         CCL.Objects.Append (Result, CCL.Objects.Unit_Cell, Step);
      end if;
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Result, Contract);
   end Severity_Value;

   procedure Severity_Of
     (Contract : CCL.Objects.Binding; Value : CCL.Objects.Image; Level : out Severity; Found : out Boolean)
   is
      Choice : Unsigned_64;
   begin
      Level := Severity'First;
      Found := CCL.Objects.Validate (Value, Contract);
      if not Found then return; end if;
      Choice := Value.Cells (1).First;
      Found := Choice in 1 .. Severity'Pos (Severity'Last) + 1;
      if Found then Level := Severity'Val (Choice - 1); end if;
   end Severity_Of;

   procedure Start (Contract : CCL.Objects.Binding; Image : out CCL.Objects.Image) is
      Built : CCL.Objects.Build_Result;
   begin
      Image := CCL.Objects.Empty (Contract);
      CCL.Objects.Append (Image, CCL.Objects.Sequence_Cell (0), Built);
   end Start;

   --  An unsigned host quantity as a CCL Integer, saturating.
   function As_Integer (Value : Unsigned_64) return Integer_64 is
     (if Value > Unsigned_64 (Integer_64'Last) then Integer_64'Last else Integer_64 (Value));

   --  Room for an item of Cells cells and Text_Bytes of text, and for one
   --  more list element.
   function Room (Image : CCL.Objects.Image; Cells, Text_Bytes : Natural) return Boolean is
     (Image.Used_Cells >= 1 and then
      Unsigned_32 (CCL.Objects.Maximum_Cells) - Image.Used_Cells >= Unsigned_32 (Cells) and then
      Text_Bytes <= CCL.Objects.Maximum_Text_Bytes and then
      Unsigned_32 (Text_Bytes) <= Unsigned_32 (CCL.Objects.Maximum_Text_Bytes) - Image.Used_Bytes and then
      Image.Cells (1).First < Unsigned_64 (MAX_ITEMS));

   --  One more element in the list's count cell.
   procedure Count_One (Image : in out CCL.Objects.Image) is
   begin
      Image.Cells (1) := CCL.Objects.Sequence_Cell (Natural (Image.Cells (1).First) + 1);
   end Count_One;

   procedure Append_Count
     (Image : in out CCL.Objects.Image; Value : Unsigned_64; Built : in out CCL.Objects.Build_Result) is
   begin
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Integer_Cell (As_Integer (Value)), Built);
      end if;
   end Append_Count;

   procedure Add
     (Image : in out CCL.Objects.Image; Time : Unsigned_64; Level : Severity;
      Source : Unsigned_64; Message : String; Added : out Boolean)
   is
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      --  Only a whole entry: room for its cells and its text, or nothing.
      Added := Room (Image, ENTRY_CELLS, Message'Length);
      if not Added then return; end if;
      CCL.Objects.Append (Image, Member_Cell (Logged), Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Product_Cell (ENTRY_FIELDS), Built);
      end if;
      Append_Count (Image, Time, Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Variant_Cell (Severity'Pos (Level) + 1), Built);
      end if;
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Unit_Cell, Built);
      end if;
      Append_Count (Image, Source, Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append_Text (Image, Message, Built);
      end if;
      Added := Built = CCL.Objects.Added;
      if Added then
         Count_One (Image);
      end if;
   end Add;

   function Decimal (Value : Unsigned_64) return String is
      Raw : constant String := Unsigned_64'Image (Value);
   begin
      return Raw (Raw'First + 1 .. Raw'Last);
   end Decimal;

   function Gap_Message (Count : Unsigned_64; Reason : Gap_Reason) return String is
     ("... " & Decimal (Count) & " records " &
      (case Reason is
          when Not_Retained => "not retained",
          when Lost => "lost before delivery",
          when Not_Shown => "not shown (this result holds no more)") & " ...");

   function Tally_Message
     (Published, Retained, Not_Retained, Over_Budget, Publisher_Full : Unsigned_64) return String is
     (Decimal (Published) & " published, " & Decimal (Retained) & " retained, " &
      Decimal (Not_Retained) & " not retained; shed " & Decimal (Over_Budget) & " over budget, " &
      Decimal (Publisher_Full) & " by the publisher");

   procedure Add_Gap
     (Image : in out CCL.Objects.Image; Time, Source, Count : Unsigned_64; Reason : Gap_Reason;
      Added : out Boolean)
   is
      Message : constant String := Gap_Message (Count, Reason);
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      Added := Room (Image, GAP_CELLS, Message'Length);
      if not Added then return; end if;
      CCL.Objects.Append (Image, Member_Cell (Gap), Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Product_Cell (GAP_FIELDS), Built);
      end if;
      Append_Count (Image, Time, Built);
      Append_Count (Image, Source, Built);
      Append_Count (Image, Count, Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Variant_Cell (Gap_Reason'Pos (Reason) + 1), Built);
      end if;
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Unit_Cell, Built);
      end if;
      if Built = CCL.Objects.Added then
         CCL.Objects.Append_Text (Image, Message, Built);
      end if;
      Added := Built = CCL.Objects.Added;
      if Added then
         Count_One (Image);
      end if;
   end Add_Gap;

   procedure Add_Tally
     (Image : in out CCL.Objects.Image;
      Source, Published, Retained, Not_Retained, Over_Budget, Publisher_Full : Unsigned_64;
      Added : out Boolean)
   is
      Message : constant String :=
        Tally_Message (Published, Retained, Not_Retained, Over_Budget, Publisher_Full);
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
   begin
      Added := Room (Image, TALLY_CELLS, Message'Length);
      if not Added then return; end if;
      CCL.Objects.Append (Image, Member_Cell (Tally), Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Product_Cell (TALLY_FIELDS), Built);
      end if;
      Append_Count (Image, Source, Built);
      Append_Count (Image, Published, Built);
      Append_Count (Image, Retained, Built);
      Append_Count (Image, Not_Retained, Built);
      Append_Count (Image, Over_Budget, Built);
      Append_Count (Image, Publisher_Full, Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append_Text (Image, Message, Built);
      end if;
      Added := Built = CCL.Objects.Added;
      if Added then
         Count_One (Image);
      end if;
   end Add_Tally;
end CCL.Interfaces.Logs;
