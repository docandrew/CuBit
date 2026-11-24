with CCL.Host_Values;
with CCL.Language;
with CCL.Objects.Catalog;
with CCL.Types;
with CCL.VM;

package body CCL.Interfaces.Power with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Language.Analysis_Status;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Catalog.Publication_Result;
   use type CCL.Objects.Build_Result;
   use type PS.Runtime_Kind;

   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean) is
      Checked : CCL.Language.Analysis_Result;
      Types : CCL.Types.Registry;
      Source_Type, List_Type : CCL.Types.Type_Reference;
      Listed : CCL.Types.List_Result;
   begin
      Bound := (others => <>);
      --  The declarations, then a trivial expression: one checked program.
      CCL.Language.Analyze (TYPE_SOURCE & "0", Checked);
      Accepted := CCL.Language.Analysis_Status_Of (Checked) = CCL.Language.Analysis_Succeeded;
      if not Accepted then return; end if;
      Types := CCL.Language.Analysis_Types (Checked);
      Source_Type := CCL.Types.Find (Types, CCL.Types.Named ("Power_Source"));
      CCL.Types.Specialize_List (Types, Source_Type, List_Type, Listed);
      Accepted := Listed in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
      if Accepted then CCL.Objects.Bind (Types, Source_Type, SOURCE_KEY, Bound.Source, Accepted); end if;
      if Accepted then CCL.Objects.Bind (Types, List_Type, SOURCES_KEY, Bound.Sources, Accepted); end if;
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Bound : Contracts;
      Accepted : Boolean;
      Result : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      --  Reading the sources observes the machine; it takes no argument.
      SOURCES_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Integer_Value,
         Result => CCL.Host_Values.Object_Value, Result_Schema => SOURCES_KEY,
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
      Publish_Schema (Bound.Source);
      Publish_Schema (Bound.Sources);
      if not Accepted then return; end if;
      CCL.Catalog.Define_Interface ("power", 1, 0, DIGEST, Descriptor, Error);
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Define_Host_Operation (Name (Sources), 0, SOURCES_CONTRACT, Entry_Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;

   procedure Sources_Value
     (Contract : CCL.Objects.Binding; Sources : PS.Source_Array; Count : PS.Source_Count;
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
      --  Power_Quantity: Unknown, or Known with its value.
      procedure Amount (Known : Boolean; Value : Unsigned_32) is
      begin
         if Known then
            Put (CCL.Objects.Variant_Cell (2));
            Put (CCL.Objects.Integer_Cell (Integer_64 (Value)));
         else
            Member (0);
         end if;
      end Amount;
   begin
      Result := CCL.Objects.Empty (Contract);
      Put (CCL.Objects.Sequence_Cell (Count));
      for I in 1 .. Count loop
         declare
            S : PS.Source renames Sources (I);
         begin
            Put (CCL.Objects.Product_Cell (SOURCE_FIELDS));
            Text (PS.Image (S.Name));
            Member (PS.Source_Kind'Pos (S.Kind));
            Member (PS.Source_State'Pos (S.State));
            Member (PS.Freshness'Pos (S.Fresh));
            Amount (S.Charge.Known, S.Charge.Value);
            --  Power_Runtime: Unknown, To_Empty or To_Full seconds.
            if S.Left.Kind = PS.Unknown then
               Member (0);
            else
               Put (CCL.Objects.Variant_Cell (PS.Runtime_Kind'Pos (S.Left.Kind) + 1));
               Put (CCL.Objects.Integer_Cell (Integer_64 (S.Left.Seconds)));
            end if;
            Amount (S.Energy_Now.Known, S.Energy_Now.Value);
            Amount (S.Energy_Full.Known, S.Energy_Full.Value);
            Amount (S.Energy_Design.Known, S.Energy_Design.Value);
            Amount (S.Rate.Known, S.Rate.Value);
            Amount (S.Voltage.Known, S.Voltage.Value);
            Put (CCL.Objects.Product_Cell (NESTED_FIELDS));
            Member (PS.Flag'Pos (S.Condition.Low));
            Member (PS.Flag'Pos (S.Condition.Replace));
            Member (PS.Flag'Pos (S.Condition.Overload));
            Member (PS.Flag'Pos (S.Condition.Bypass));
            Put (CCL.Objects.Product_Cell (NESTED_FIELDS));
            Amount (S.Load.Known, S.Load.Value);
            Amount (S.Input_Voltage.Known, S.Input_Voltage.Value);
            Amount (S.Output_Voltage.Known, S.Output_Voltage.Value);
            Member (PS.Transfer_Reason'Pos (S.Transfer));
         end;
      end loop;
      Built := Step = CCL.Objects.Added and then CCL.Objects.Validate (Result, Contract);
   end Sources_Value;
end CCL.Interfaces.Power;
