with CCL.Host_Values;
with CCL.Language;
with CCL.Objects.Catalog;
with CCL.Types;
with CCL.VM;

package body CCL.Interfaces.Graphics with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Language.Analysis_Status;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Catalog.Publication_Result;
   use type CCL.Objects.Build_Result;

   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean) is
      Checked : CCL.Language.Analysis_Result;
      Types : CCL.Types.Registry;
      Entry_Type, List_Type : CCL.Types.Type_Reference;
      Listed : CCL.Types.List_Result;
   begin
      Bound := (others => <>);
      CCL.Language.Analyze (TYPE_SOURCE & "0", Checked);
      Accepted := CCL.Language.Analysis_Status_Of (Checked) = CCL.Language.Analysis_Succeeded;
      if not Accepted then return; end if;
      Types := CCL.Language.Analysis_Types (Checked);
      Entry_Type := CCL.Types.Find (Types, CCL.Types.Named ("Gfx_Entry"));
      CCL.Types.Specialize_List (Types, Entry_Type, List_Type, Listed);
      Accepted := Listed in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      if Accepted then CCL.Objects.Bind (Types, Entry_Type, ENTRY_KEY, Bound.Single, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, List_Type, ENTRIES_KEY, Bound.Listing, Accepted); end if;
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Bound : Contracts;
      Accepted : Boolean;
      Result : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      --  Reading the report observes the machine; it takes no argument.
      REPORT_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Integer_Value,
         Result => CCL.Host_Values.Object_Value, Result_Schema => ENTRIES_KEY,
         Authority => CCL.VM.Observe_Authority, others => <>);
      procedure Publish_Schema (Contract : CCL.Objects.Binding) is
      begin
         if Accepted then
            CCL.Catalog.Publish_Schema (Item, Contract, Result);
            Accepted := Result in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published;
         end if;
      end Publish_Schema;
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Bound, Accepted);
      Publish_Schema (Bound.Single);
      Publish_Schema (Bound.Listing);
      if not Accepted then return; end if;
      CCL.Catalog.Define_Interface ("graphics", 1, 0, DIGEST, Descriptor, Error);
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Define_Host_Operation (Name (Report), 0, REPORT_CONTRACT, Entry_Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;

   procedure Report_Value
     (Contract : CCL.Objects.Binding; Entries : Entry_Array;
      Result : out CCL.Objects.Image; Built : out Boolean)
   is
      Step : CCL.Objects.Build_Result := CCL.Objects.Added;
      procedure Put (Cell : CCL.Objects.Cell) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append (Result, Cell, Step); end if;
      end Put;
      procedure Text (Value : String) is
      begin
         if Step = CCL.Objects.Added then CCL.Objects.Append_Text (Result, Value, Step); end if;
      end Text;
      --  An enumeration member: its variant cell and its Unit payload.
      procedure Member (Position : Natural) is
      begin
         Put (CCL.Objects.Variant_Cell (Position + 1));
         Put (CCL.Objects.Unit_Cell);
      end Member;
      --  The reason's detail, read as its enumeration's position, kept in
      --  range (a detail out of range is the first member).
      function Position (Detail : GR.Detail_Value; Last : Natural) return Natural is
        (if Detail in 0 .. GR.Detail_Value (Last) then Natural (Detail) else 0);
   begin
      Result := CCL.Objects.Empty (Contract);
      Put (CCL.Objects.Sequence_Cell (Entries'Length));
      for E of Entries loop
         Put (CCL.Objects.Product_Cell (ENTRY_FIELDS));
         Member (GR.Subject_Kind'Pos (E.Kind));
         Put (CCL.Objects.Integer_Cell (Integer_64 (E.Id)));
         Text (GR.Image (E.Subject));
         Member (GR.Path_Kind'Pos (E.Path));
         Put (CCL.Objects.Variant_Cell (GR.Reason'Pos (E.Why) + 1));
         case E.Why is
            when GR.Driver_Failed =>
               Member (Position (E.Detail, GR.Driver_Stage'Pos (GR.Driver_Stage'Last)));
            when GR.Venus_Not_Offered =>
               Member (Position (E.Detail, GR.Venus_Gap'Pos (GR.Venus_Gap'Last)));
            when GR.Policy =>
               Member (Position (E.Detail, GR.Policy_Setting'Pos (GR.Policy_Setting'Last)));
            when GR.Admission_Refused =>
               Member (Position (E.Detail, GR.Admission_Outcome'Pos (GR.Admission_Outcome'Last)));
            when GR.Fell_Back =>
               Member (Position (E.Detail, GR.Fallback_Cause'Pos (GR.Fallback_Cause'Last)));
            when GR.ICD_Init_Failed =>
               Put (CCL.Objects.Integer_Cell (Integer_64 (E.Detail)));
            when GR.None | GR.No_GPU_Device | GR.No_Driver_Bound | GR.Manifest_Lacks_Render
               | GR.Startup_Timed_Out =>
               Put (CCL.Objects.Unit_Cell);
         end case;
         Text (GR.Image (E.Device));
         Put (CCL.Objects.Integer_Cell (Integer_64 (E.Since)));
         Put (CCL.Objects.Boolean_Cell (E.Stale));
      end loop;
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Result, Contract);
   end Report_Value;
end CCL.Interfaces.Graphics;
