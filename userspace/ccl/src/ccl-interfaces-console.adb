with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;

with CCL.Interface_Sources;

package body CCL.Interfaces.Console with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Objects.Build_Result;
   use type CCL.Objects.Catalog.Publication_Result;

   NOTATION_MEMBERS : constant := Notation'Pos (Notation'Last) + 1;
   THEME_MEMBERS : constant := Theme'Pos (Theme'Last) + 1;

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean)
   is
      Specialized : CCL.Types.List_Result;

      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Interface_Sources.Named_Type (Types, Name));
   begin
      Bound := (others => <>);
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);

      if Accepted then CCL.Objects.Bind (Types, Named ("Notation"), NOTATION_KEY, Bound.Notation, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Theme"), THEME_KEY, Bound.Theme, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, Named ("Console_Stats"), STATS_KEY, Bound.Stats, Accepted); end if;
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Types : CCL.Types.Registry := CCL.Catalog.Visible_Types (Item);
      Bound : Contracts;
      Accepted : Boolean;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      procedure Publish_Schema (Contract : CCL.Objects.Binding) is
         Result : CCL.Objects.Catalog.Publication_Result;
      begin
         if Accepted then
            CCL.Catalog.Publish_Schema (Item, Contract, Result);
            Accepted := Result in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published;
         end if;
      end Publish_Schema;
      function Contract_Of (Op : Operation) return CCL.Host_Values.Import_Declaration is
        (case Op is
            when Title =>
              (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => MAX_TITLE,
               Result => CCL.Host_Values.Boolean_Value,
               Authority => CCL.VM.Control_Authority, others => <>),
            when Set_Notation =>
              (Argument => CCL.Host_Values.Object_Value, Argument_Schema => NOTATION_KEY,
               Result => CCL.Host_Values.Object_Value, Result_Schema => NOTATION_KEY,
               Authority => CCL.VM.Control_Authority, others => <>),
            when Set_Theme =>
              (Argument => CCL.Host_Values.Object_Value, Argument_Schema => THEME_KEY,
               Result => CCL.Host_Values.Object_Value, Result_Schema => THEME_KEY,
               Authority => CCL.VM.Control_Authority, others => <>),
            when Stats =>
              (Argument => CCL.Host_Values.Integer_Value,
               Result => CCL.Host_Values.Object_Value, Result_Schema => STATS_KEY,
               Authority => CCL.VM.Observe_Authority, others => <>));
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Bound, Accepted);
      Publish_Schema (Bound.Notation);
      Publish_Schema (Bound.Theme);
      Publish_Schema (Bound.Stats);
      if not Accepted then
         return;
      end if;
      CCL.Catalog.Define_Interface ("console", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation
           (Name (Op), (if Op = Stats then 0 else 1), Contract_Of (Op), Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;

   procedure Member_Value
     (Contract : CCL.Objects.Binding; Choice : CCL.Types.Component_Index;
      Result : out CCL.Objects.Image; Built : out Boolean)
   is
      Step : CCL.Objects.Build_Result;
   begin
      Result := CCL.Objects.Empty (Contract);
      CCL.Objects.Append (Result, CCL.Objects.Variant_Cell (Choice), Step);
      --  A member without a payload still carries its Unit payload cell.
      if Step = CCL.Objects.Added then
         CCL.Objects.Append (Result, CCL.Objects.Unit_Cell, Step);
      end if;
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Result, Contract);
   end Member_Value;

   procedure Notation_Value
     (Contract : CCL.Objects.Binding; Value : Notation;
      Result : out CCL.Objects.Image; Built : out Boolean) is
   begin
      Member_Value (Contract, Notation'Pos (Value) + 1, Result, Built);
   end Notation_Value;

   procedure Theme_Value
     (Contract : CCL.Objects.Binding; Value : Theme;
      Result : out CCL.Objects.Image; Built : out Boolean) is
   begin
      Member_Value (Contract, Theme'Pos (Value) + 1, Result, Built);
   end Theme_Value;

   procedure Stats_Value
     (Contract : CCL.Objects.Binding; Value : Statistics;
      Result : out CCL.Objects.Image; Built : out Boolean)
   is
      Step : CCL.Objects.Build_Result;
      procedure Put (Item : Natural) is
      begin
         if Step = CCL.Objects.Added then
            CCL.Objects.Append (Result, CCL.Objects.Integer_Cell (Integer_64 (Item)), Step);
         end if;
      end Put;
   begin
      Result := CCL.Objects.Empty (Contract);
      CCL.Objects.Append (Result, CCL.Objects.Product_Cell (STATS_FIELDS), Step);
      Put (Value.Cells); Put (Value.Live); Put (Value.Live_Runs);
      Put (Value.Last_Ms); Put (Value.Slowest_Ms); Put (Value.Streams);
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Result, Contract);
   end Stats_Value;

   procedure Member_Of
     (Contract : CCL.Objects.Binding; Value : CCL.Objects.Image; Count : Positive;
      Member : out Positive; Found : out Boolean)
   is
      Choice : Unsigned_64;
   begin
      Member := 1;
      Found := CCL.Objects.Validate (Value, Contract);
      if not Found then return; end if;
      Choice := Value.Cells (1).First;
      Found := Choice in 1 .. Unsigned_64 (Count);
      if Found then Member := Positive (Choice); end if;
   end Member_Of;
end CCL.Interfaces.Console;
