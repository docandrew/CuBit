package body CCL.Host_Values with SPARK_Mode => On is
   use type CCL.Types.Shape;
   use type CCL.VM.Import_Declaration;
   function Kind_Of (Item : CCL.VM.Scalar_Kind) return Value_Kind is
     (if Item = CCL.VM.Integer_Value then Integer_Value else Boolean_Value);
   function From_Scalar (Item : CCL.VM.Value) return Value is
     (if Item.Kind = CCL.VM.Integer_Value then Integer_Constant (Item.Integer)
      else Boolean_Constant (Item.Boolean));
   procedure Copy_Text (Source : String; Item : out Text; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := Source'Length <= Maximum_Text_Length;
      if Success then
         Item.Length := Source'Length;
         Item.Data (1 .. Source'Length) := Source;
      end if;
   end Copy_Text;
   procedure To_Scalar (Item : Value; Scalar : out CCL.VM.Value; Success : out Boolean) is
   begin
      Scalar := CCL.VM.Integer_Constant (0);
      Success := Item.Kind in Integer_Value | Boolean_Value;
      case Item.Kind is
         when Integer_Value => Scalar := CCL.VM.Integer_Constant (Item.Integer);
         when Boolean_Value => Scalar := CCL.VM.Boolean_Constant (Item.Boolean);
         when Text_Value | Handler_Value | Object_Value | Resource_Value => null;
      end case;
   end To_Scalar;
   function From_Bytecode
     (Item : CCL.VM.Import_Declaration;
      Argument_Schema : CCL.Objects.Schema_Key := CCL.Objects.No_Schema;
      Result_Schema : CCL.Objects.Schema_Key := CCL.Objects.No_Schema) return Import_Declaration is
     ((Argument => (if Argument_Schema /= CCL.Objects.No_Schema then Object_Value else Kind_Of (Item.Argument)),
       Result => (if Result_Schema /= CCL.Objects.No_Schema then Object_Value else Kind_Of (Item.Result)),
       Argument_Schema => Argument_Schema, Result_Schema => Result_Schema,
       Authority => Item.Authority, Binding => Item.Binding,
       Ownership_Argument => Item.Ownership_Argument, Local => Item.Local,
       Transfer => Item.Transfer, Cancellation => Item.Cancellation,
       Success_Verb => Item.Success_Verb, Failure_Verb => Item.Failure_Verb,
       Cancel_Verb => Item.Cancel_Verb, others => <>));
   procedure To_Bytecode
     (Item : Import_Declaration; Types : CCL.Types.Registry;
      Argument_Type, Result_Type : CCL.Types.Type_Reference;
      Import : out CCL.VM.Import_Declaration;
      Success : out Boolean;
      Result_Type_Tag : CCL.Ownership.Type_Id := 0;
      Receiver_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type)
   is
      function Supported (Kind : Value_Kind; Ref : CCL.Types.Type_Reference) return Boolean is
        (case Kind is
           when Integer_Value => Ref = CCL.Types.Integer_Type,
           when Boolean_Value => Ref = CCL.Types.Boolean_Type,
           when Object_Value => (not Item.Ownership_Argument or Item.Argument = Resource_Value or Has_Receiver (Item)) and then
             CCL.Objects.Persistable (Types, Ref),
           when Resource_Value => CCL.Types.Known (Types, Ref) and then
             CCL.Types.Describe (Types, Ref).Form = CCL.Types.Resource,
           when others => False);
      function VM_Kind (Ref : CCL.Types.Type_Reference) return CCL.VM.Value_Kind is
        (if Ref = CCL.Types.Integer_Type then CCL.VM.Integer_Value
         elsif Ref = CCL.Types.Boolean_Type then CCL.VM.Boolean_Value
         elsif CCL.Types.Describe (Types, Ref).Form = CCL.Types.Resource then CCL.VM.Resource_Value
         elsif CCL.Types.Is_Scalar_Sum (Types, Ref) then CCL.VM.Variant_Value
         else CCL.VM.Object_Value);
      function Nominal (Ref : CCL.Types.Type_Reference) return CCL.Types.Type_Reference is
        (if Ref in CCL.Types.Integer_Type | CCL.Types.Boolean_Type then CCL.Types.Invalid_Type else Ref);
   begin
      Import := (others => <>);
      Success := Well_Formed (Item) and then Supported (Item.Argument, Argument_Type) and then
        Supported (Item.Result, Result_Type) and then
        ((Item.Result = Resource_Value) = (Result_Type_Tag /= 0)) and then
        (if Has_Receiver (Item) then Supported (Resource_Value, Receiver_Type) and then
           CCL.Types.Describe (Types, Receiver_Type).Identifier = Item.Receiver_Resource
         else Receiver_Type = CCL.Types.Invalid_Type) and then
        (if Item.Argument = Resource_Value then
           CCL.Types.Describe (Types, Argument_Type).Identifier = Item.Argument_Resource) and then
        (if Item.Result = Resource_Value then
           CCL.Types.Describe (Types, Result_Type).Identifier = Item.Result_Resource);
      if Success then
         Import :=
           (Argument => VM_Kind (Argument_Type), Result => VM_Kind (Result_Type),
            Argument_Data_Type => Nominal (Argument_Type), Result_Data_Type => Nominal (Result_Type),
            Authority => Item.Authority, Binding => Item.Binding,
            Ownership_Argument => Item.Ownership_Argument, Local => Item.Local,
            Transfer => Item.Transfer, Cancellation => Item.Cancellation,
            Success_Verb => Item.Success_Verb, Failure_Verb => Item.Failure_Verb,
            Cancel_Verb => Item.Cancel_Verb, Result_Type_Tag => Result_Type_Tag,
            Receiver_Data_Type => Receiver_Type);
      end if;
   end To_Bytecode;
   function Matches_Bytecode
     (Compiled : CCL.VM.Import_Declaration; Declared : Import_Declaration) return Boolean is
     (Well_Formed (Declared) and then
      Portable_Contract (Compiled, Declared.Argument_Schema, Declared.Result_Schema) and then
      From_Bytecode (Compiled, Declared.Argument_Schema, Declared.Result_Schema) = Declared);

   function Matches_Bytecode
     (Compiled : CCL.VM.Import_Declaration; Declared : Import_Declaration;
      Types : CCL.Types.Registry) return Boolean
   is
      Expected : CCL.VM.Import_Declaration;
      Good : Boolean;
      function Ref (Kind : CCL.VM.Value_Kind; Local : CCL.Types.Type_Reference)
        return CCL.Types.Type_Reference is
        (case Kind is
           when CCL.VM.Integer_Value => CCL.Types.Integer_Type,
           when CCL.VM.Boolean_Value => CCL.Types.Boolean_Type,
           when CCL.VM.Variant_Value | CCL.VM.Object_Value | CCL.VM.Resource_Value => Local);
   begin
      if not Has_Resources (Declared) then return Matches_Bytecode (Compiled, Declared); end if;
      To_Bytecode (Declared, Types,
        Ref (Compiled.Argument, Compiled.Argument_Data_Type),
        Ref (Compiled.Result, Compiled.Result_Data_Type), Expected, Good,
        Compiled.Result_Type_Tag, Compiled.Receiver_Data_Type);
      -- A resource operand is read from a compiler-selected owned local.
      -- The catalog checks that local's approved type and the VM checks its
      -- ownership state at every invocation. It is not descriptor identity.
      if Declared.Argument = Resource_Value or Has_Receiver (Declared) then Expected.Local := Compiled.Local; end if;
      return Good and then Expected = Compiled;
   end Matches_Bytecode;
end CCL.Host_Values;
