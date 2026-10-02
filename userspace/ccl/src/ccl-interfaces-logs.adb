with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;

package body CCL.Interfaces.Logs with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Types.Definition_Result;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Build_Result;
   use type CCL.Objects.Catalog.Publication_Result;

   --  Fields of a LogEntry, in order.
   ENTRY_FIELDS : constant := 4;

   --  A member's CCL name, as logs.schema spells it.
   function Member_Name (Level : Severity) return String is
     (case Level is
         when Trace => "Trace", when Debug => "Debug", when Information => "Information",
         when Warning => "Warning", when Error => "Error", when Critical => "Critical");

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Entries : out CCL.Types.Type_Reference;
      Contract : out CCL.Objects.Binding; Accepted : out Boolean)
   is
      Unbound : CCL.Objects.Binding;
      Levels, Log_Entry : CCL.Types.Type_Reference;
      Defined : CCL.Types.Definition_Result;
      Specialized : CCL.Types.List_Result;
      Members : CCL.Types.Component_Array := [others => (others => <>)];
   begin
      Entries := CCL.Types.Invalid_Type;
      Contract := Unbound;
      for Level in Severity loop
         Members (Severity'Pos (Level) + 1) := (CCL.Types.Named (Member_Name (Level)), CCL.Types.Unit_Type);
      end loop;
      CCL.Types.Define
        (Types, (Identifier => CCL.Types.Named ("Severity"), Form => CCL.Types.Sum,
                 Count => Severity'Pos (Severity'Last) + 1, Parts => Members), Levels, Defined);
      Accepted := Defined = CCL.Types.Defined;
      if not Accepted then return; end if;
      CCL.Types.Define
        (Types, (Identifier => CCL.Types.Named ("LogEntry"), Form => CCL.Types.Product,
                 Count => ENTRY_FIELDS,
                 Parts => [1 => (CCL.Types.Named ("time"), CCL.Types.Integer_Type),
                           2 => (CCL.Types.Named ("severity"), Levels),
                           3 => (CCL.Types.Named ("source"), CCL.Types.Integer_Type),
                           4 => (CCL.Types.Named ("message"), CCL.Types.String_Type),
                           others => <>]), Log_Entry, Defined);
      Accepted := Defined = CCL.Types.Defined;
      if not Accepted then return; end if;
      CCL.Types.Specialize_List (Types, Log_Entry, Entries, Specialized);
      Accepted := Specialized in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      if Accepted then
         CCL.Objects.Bind (Types, Entries, SCHEMA_KEY, Contract, Accepted);
      end if;
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Types : CCL.Types.Registry := CCL.Catalog.Visible_Types (Item);
      Entries : CCL.Types.Type_Reference;
      Contract : CCL.Objects.Binding;
      Accepted : Boolean;
      Published : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Operation : CCL.Catalog.Operation_Descriptor;
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Entries, Contract, Accepted);
      if not Accepted then return; end if;
      CCL.Catalog.Publish_Schema (Item, Contract, Published);
      if Published not in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published then
         return;
      end if;
      CCL.Catalog.Define_Interface ("logs", 1, 0, DIGEST, Descriptor, Error);
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Define_Host_Operation
           ("recent", 1,
            (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => MAX_SERVICE_NAME,
             Result => CCL.Host_Values.Object_Value, Result_Schema => SCHEMA_KEY,
             Authority => CCL.VM.Observe_Authority, others => <>),
            Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Add_Operation (Descriptor, Operation, Error);
      end if;
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

   --  An unsigned host quantity as a CCL Integer, saturating.
   function As_Integer (Value : Unsigned_64) return Integer_64 is
     (if Value > Unsigned_64 (Integer_64'Last) then Integer_64'Last else Integer_64 (Value));

   procedure Add
     (Image : in out CCL.Objects.Image; Time : Unsigned_64; Level : Severity;
      Source : Unsigned_64; Message : String; Added : out Boolean)
   is
      Built : CCL.Objects.Build_Result := CCL.Objects.Added;
      Count : constant Unsigned_64 := Image.Cells (1).First;
   begin
      --  Only a whole entry: room for its cells and its text, or nothing.
      Added := Image.Used_Cells >= 1 and then
        Unsigned_32 (CCL.Objects.Maximum_Cells) - Image.Used_Cells >= ENTRY_CELLS and then
        Message'Length <= CCL.Objects.Maximum_Text_Bytes and then
        Unsigned_32 (Message'Length) <= Unsigned_32 (CCL.Objects.Maximum_Text_Bytes) - Image.Used_Bytes and then
        Count < Unsigned_64 (MAX_ENTRIES);
      if not Added then return; end if;
      CCL.Objects.Append (Image, CCL.Objects.Product_Cell (ENTRY_FIELDS), Built);
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Integer_Cell (As_Integer (Time)), Built);
      end if;
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Variant_Cell (Severity'Pos (Level) + 1), Built);
      end if;
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Unit_Cell, Built);
      end if;
      if Built = CCL.Objects.Added then
         CCL.Objects.Append (Image, CCL.Objects.Integer_Cell (As_Integer (Source)), Built);
      end if;
      if Built = CCL.Objects.Added then
         CCL.Objects.Append_Text (Image, Message, Built);
      end if;
      Added := Built = CCL.Objects.Added;
      if Added then
         Image.Cells (1) := CCL.Objects.Sequence_Cell (Natural (Count) + 1);
      end if;
   end Add;
end CCL.Interfaces.Logs;
