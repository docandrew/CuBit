with CCL.Host_Values;
with CCL.VM;
with CCL.Objects.Catalog;
with CCL.Interface_Sources;

package body CCL.Interfaces.Machine with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Objects.Catalog.Publication_Result;
   use type CCL.Objects.Build_Result;

   procedure Define_Types
     (Types : in out CCL.Types.Registry; Contract : out CCL.Objects.Binding; Accepted : out Boolean)
   is
      Unbound : CCL.Objects.Binding;
   begin
      Contract := Unbound;
      CCL.Interface_Sources.Declare_Types (TYPE_SOURCE, Types, Accepted);
      if Accepted then
         CCL.Objects.Bind (Types, CCL.Interface_Sources.Named_Type (Types, "Power_Answer"), ANSWER_KEY,
                           Contract, Accepted);
      end if;
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Types : CCL.Types.Registry := CCL.Catalog.Visible_Types (Item);
      Contract : CCL.Objects.Binding;
      Accepted : Boolean;
      Published : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      --  Ending the machine controls; a parameterless call takes the
      --  Integer zero.
      Request_Contract : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Integer_Value,
         Result => CCL.Host_Values.Object_Value, Result_Schema => ANSWER_KEY,
         Authority => CCL.VM.Control_Authority, others => <>);
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Types, Contract, Accepted);
      if not Accepted then return; end if;
      CCL.Catalog.Publish_Schema (Item, Contract, Published);
      if Published not in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published then
         return;
      end if;
      CCL.Catalog.Define_Interface ("machine", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation (Name (Op), 0, Request_Contract, Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Mark_Command (Entry_Operation);
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;

   procedure Answer_Value
     (Contract : CCL.Objects.Binding; Item : Answer; Result : out CCL.Objects.Image; Built : out Boolean)
   is
      Step : CCL.Objects.Build_Result;
   begin
      Result := CCL.Objects.Empty (Contract);
      CCL.Objects.Append (Result, CCL.Objects.Variant_Cell (Answer'Pos (Item) + 1), Step);
      --  A member without a payload still carries its Unit payload cell.
      if Step = CCL.Objects.Added then
         CCL.Objects.Append (Result, CCL.Objects.Unit_Cell, Step);
      end if;
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Result, Contract);
   end Answer_Value;
end CCL.Interfaces.Machine;
