with Interfaces;
with CCL.VM;
with CCL.Imports;
with CCL.Ownership;

--  Interpreter/host values own their storage. No VM address, temporary-region
--  descriptor, or userspace pointer crosses this boundary.
package CCL.Host_Values with SPARK_Mode => On is
   Maximum_Text_Length : constant := 1_024;
   subtype Text_Length is Natural range 0 .. Maximum_Text_Length;
   type Text is record
      Length : Text_Length := 0;
      Data : String (1 .. Maximum_Text_Length) := [others => ' '];
   end record;
   type Value_Kind is (Integer_Value, Boolean_Value, Text_Value);
   type Value (Kind : Value_Kind := Integer_Value) is record
      case Kind is
         when Integer_Value => Integer : Interfaces.Integer_64 := 0;
         when Boolean_Value => Boolean : Standard.Boolean := False;
         when Text_Value => Content : Text;
      end case;
   end record;
   function Integer_Constant (Item : Interfaces.Integer_64) return Value is
     ((Kind => Integer_Value, Integer => Item));
   function Boolean_Constant (Item : Boolean) return Value is
     ((Kind => Boolean_Value, Boolean => Item));
   function Text_Constant (Item : Text) return Value is
     ((Kind => Text_Value, Content => Item));
   procedure Copy_Text (Source : String; Item : out Text; Success : out Boolean);
   function From_Scalar (Item : CCL.VM.Value) return Value;
   procedure To_Scalar (Item : Value; Scalar : out CCL.VM.Value; Success : out Boolean);
   function Kind_Of (Item : CCL.VM.Value_Kind) return Value_Kind;

   --  Source interface contracts are not bytecode import records. Text limits
   --  participate in exact grant matching, including zero-length-only text.
   type Import_Declaration is record
      Argument : Value_Kind := Integer_Value;
      Result : Value_Kind := Integer_Value;
      Argument_Text_Limit, Result_Text_Limit : Text_Length := 0;
      Authority : CCL.VM.Authority_Class := CCL.VM.No_Authority;
      Binding : Interfaces.Unsigned_32 := 0;
      Ownership_Argument : Boolean := False;
      Local : CCL.Ownership.Binding_Id := 0;
      Transfer : CCL.Imports.Transfer_Mode := CCL.Imports.Copy_Argument;
      Cancellation : CCL.Imports.Cancellation_Mode := CCL.Imports.Not_Cancellable;
      Success_Verb, Failure_Verb, Cancel_Verb : CCL.Ownership.Disposition_Id := 0;
   end record;
   function From_Bytecode (Item : CCL.VM.Import_Declaration) return Import_Declaration;
   function Well_Formed (Item : Import_Declaration) return Boolean is
     ((Item.Argument = Text_Value or Item.Argument_Text_Limit = 0) and
      (Item.Result = Text_Value or Item.Result_Text_Limit = 0));
   function Scalar_Only (Item : Import_Declaration) return Boolean is
     (Well_Formed (Item) and Item.Argument /= Text_Value and Item.Result /= Text_Value);
   procedure To_Bytecode
     (Item : Import_Declaration; Import : out CCL.VM.Import_Declaration;
      Success : out Boolean);
   function Matches (Item : Value; Kind : Value_Kind; Limit : Text_Length) return Boolean is
     (Item.Kind = Kind and then
      (Item.Kind /= Text_Value or else Item.Content.Length <= Limit));
end CCL.Host_Values;
