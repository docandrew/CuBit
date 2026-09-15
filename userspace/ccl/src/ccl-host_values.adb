package body CCL.Host_Values with SPARK_Mode => On is
   use type CCL.VM.Value_Kind;
   function Kind_Of (Item : CCL.VM.Value_Kind) return Value_Kind is
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
      Success := Item.Kind /= Text_Value;
      case Item.Kind is
         when Integer_Value => Scalar := CCL.VM.Integer_Constant (Item.Integer);
         when Boolean_Value => Scalar := CCL.VM.Boolean_Constant (Item.Boolean);
         when Text_Value => null;
      end case;
   end To_Scalar;
   function From_Bytecode (Item : CCL.VM.Import_Declaration) return Import_Declaration is
     ((Argument => Kind_Of (Item.Argument), Result => Kind_Of (Item.Result),
       Authority => Item.Authority, Binding => Item.Binding,
       Ownership_Argument => Item.Ownership_Argument, Local => Item.Local,
       Transfer => Item.Transfer, Cancellation => Item.Cancellation,
       Success_Verb => Item.Success_Verb, Failure_Verb => Item.Failure_Verb,
       Cancel_Verb => Item.Cancel_Verb, others => <>));
   procedure To_Bytecode
     (Item : Import_Declaration; Import : out CCL.VM.Import_Declaration;
      Success : out Boolean) is
   begin
      Import := (others => <>);
      Success := Scalar_Only (Item);
      if Success then
         Import :=
           (Argument => (if Item.Argument = Integer_Value then CCL.VM.Integer_Value else CCL.VM.Boolean_Value),
            Result => (if Item.Result = Integer_Value then CCL.VM.Integer_Value else CCL.VM.Boolean_Value),
            Authority => Item.Authority, Binding => Item.Binding,
            Ownership_Argument => Item.Ownership_Argument, Local => Item.Local,
            Transfer => Item.Transfer, Cancellation => Item.Cancellation,
            Success_Verb => Item.Success_Verb, Failure_Verb => Item.Failure_Verb,
            Cancel_Verb => Item.Cancel_Verb);
      end if;
   end To_Bytecode;
end CCL.Host_Values;
