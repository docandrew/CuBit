with Interfaces;
with CCL.VM;
with CCL.Imports;
with CCL.Ownership;
with CCL.Handler_References;
with CCL.Objects;
with CCL.Types;
with CCL.Resources;

--  Interpreter/host values own their storage. No VM address, temporary-region
--  descriptor, or userspace pointer crosses this boundary.
package CCL.Host_Values with SPARK_Mode => On is
   use type CCL.Objects.Schema_Key;
   use type CCL.Types.Type_Reference;
   use type CCL.VM.Value_Kind;
   use type CCL.Types.Name;
   use type CCL.Imports.Transfer_Mode;
   Maximum_Text_Length : constant := 1_024;
   subtype Text_Length is Natural range 0 .. Maximum_Text_Length;
   type Text is record
      Length : Text_Length := 0;
      Data : String (1 .. Maximum_Text_Length) := [others => ' '];
   end record;
   type Value_Kind is
     (Integer_Value, Boolean_Value, Text_Value, Handler_Value, Object_Value, Resource_Value);
   type Value (Kind : Value_Kind := Integer_Value) is record
      case Kind is
         when Integer_Value => Integer : Interfaces.Integer_64 := 0;
         when Boolean_Value => Boolean : Standard.Boolean := False;
         when Text_Value => Content : Text;
         when Handler_Value => Action : CCL.Handler_References.Reference;
         when Object_Value => Object : CCL.Objects.Image;
         when Resource_Value => Resource : CCL.Resources.Reference;
      end case;
   end record;
   --  A callback owns its whole result, including the returned value's kind.
   --  Unlike an out Value formal, this non-discriminated envelope cannot be
   --  constrained by a caller to one alternative before the callback runs.
   type Call_Result is record
      Value : CCL.Host_Values.Value;
      Success : Boolean := False;
   end record;
   function Integer_Constant (Item : Interfaces.Integer_64) return Value is
     ((Kind => Integer_Value, Integer => Item));
   function Boolean_Constant (Item : Boolean) return Value is
     ((Kind => Boolean_Value, Boolean => Item));
   function Text_Constant (Item : Text) return Value is
     ((Kind => Text_Value, Content => Item));
   function Handler_Constant (Item : CCL.Handler_References.Reference) return Value is
     ((Kind => Handler_Value, Action => Item));
   function Object_Constant (Item : CCL.Objects.Image) return Value is
     ((Kind => Object_Value, Object => Item));
   function Resource_Constant (Item : CCL.Resources.Reference) return Value is
     ((Kind => Resource_Value, Resource => Item));
   -- Host-owned opaque identity, not a persistable value or an authority grant.
   -- Admission requires its owning registry and the declared nominal type.
   procedure Copy_Text (Source : String; Item : out Text; Success : out Boolean);
   function From_Scalar (Item : CCL.VM.Value) return Value
     with Pre => Item.Kind in CCL.VM.Scalar_Kind;
   procedure To_Scalar (Item : Value; Scalar : out CCL.VM.Value; Success : out Boolean);
   function Kind_Of (Item : CCL.VM.Scalar_Kind) return Value_Kind;

   --  Source interface contracts are not bytecode import records. Text limits
   --  participate in exact grant matching, including zero-length-only text.
   type Import_Declaration is record
      Argument : Value_Kind := Integer_Value;
      Result : Value_Kind := Integer_Value;
      Argument_Text_Limit, Result_Text_Limit : Text_Length := 0;
      -- Approved nominal identities, not program-local type numbers or grants.
      Argument_Schema, Result_Schema : CCL.Objects.Schema_Key := CCL.Objects.No_Schema;
      Argument_Resource, Result_Resource : CCL.Types.Name;
      Receiver_Resource : CCL.Types.Name;
      -- Optional owned receiver, separate from the ordinary data argument.
      -- Source spells it first; Parameters counts only data arguments.
      -- Nominal resource names resolve through the approved catalog. Do not
      -- repurpose persistence schema keys or program-local ownership tags.
      Authority : CCL.VM.Authority_Class := CCL.VM.No_Authority;
      Binding : Interfaces.Unsigned_32 := 0;
      Ownership_Argument : Boolean := False;
      Local : CCL.Ownership.Binding_Id := 0;
      Transfer : CCL.Imports.Transfer_Mode := CCL.Imports.Copy_Argument;
      Cancellation : CCL.Imports.Cancellation_Mode := CCL.Imports.Not_Cancellable;
      Success_Verb, Failure_Verb, Cancel_Verb : CCL.Ownership.Disposition_Id := 0;
   end record;
   function Portable_Contract
     (Item : CCL.VM.Import_Declaration;
      Argument_Schema, Result_Schema : CCL.Objects.Schema_Key) return Boolean is
     (Item.Argument /= CCL.VM.Resource_Value and then Item.Result /= CCL.VM.Resource_Value and then
      Item.Result_Type_Tag = 0 and then not CCL.VM.Has_Receiver (Item) and then
      (if Item.Argument = CCL.VM.Object_Value then
          Item.Argument_Data_Type /= CCL.Types.Invalid_Type and Argument_Schema /= CCL.Objects.No_Schema
       elsif Item.Argument = CCL.VM.Variant_Value then
          Item.Argument_Data_Type in CCL.Types.Declared_Type and Argument_Schema /= CCL.Objects.No_Schema
       else Item.Argument_Data_Type = CCL.Types.Invalid_Type) and then
      (if Item.Result = CCL.VM.Object_Value then
          Item.Result_Data_Type /= CCL.Types.Invalid_Type and Result_Schema /= CCL.Objects.No_Schema
       elsif Item.Result = CCL.VM.Variant_Value then
          Item.Result_Data_Type in CCL.Types.Declared_Type and Result_Schema /= CCL.Objects.No_Schema
       else Item.Result_Data_Type = CCL.Types.Invalid_Type));
   function From_Bytecode
     (Item : CCL.VM.Import_Declaration;
      Argument_Schema : CCL.Objects.Schema_Key := CCL.Objects.No_Schema;
      Result_Schema : CCL.Objects.Schema_Key := CCL.Objects.No_Schema) return Import_Declaration
     with Pre => Portable_Contract (Item, Argument_Schema, Result_Schema);
   function Resource_Name_Matches (Kind : Value_Kind; Name : CCL.Types.Name) return Boolean is
     (if Kind = Resource_Value then CCL.Types.Valid_Name (Name) and then
         Name = CCL.Types.Named (CCL.Types.Image (Name))
      else Name = CCL.Types.Named (""));
   function Has_Receiver (Item : Import_Declaration) return Boolean is
     (Item.Receiver_Resource.Length > 0);
   function Has_Resources (Item : Import_Declaration) return Boolean is
     (Item.Argument = Resource_Value or Item.Result = Resource_Value or Has_Receiver (Item));
   function Well_Formed (Item : Import_Declaration) return Boolean is
     ((Item.Argument = Text_Value or Item.Argument_Text_Limit = 0) and
      (Item.Result = Text_Value or Item.Result_Text_Limit = 0) and
      ((Item.Argument = Object_Value) = (Item.Argument_Schema /= CCL.Objects.No_Schema)) and
      ((Item.Result = Object_Value) = (Item.Result_Schema /= CCL.Objects.No_Schema)) and
      Resource_Name_Matches (Item.Argument, Item.Argument_Resource) and
      Resource_Name_Matches (Item.Result, Item.Result_Resource) and
      Resource_Name_Matches
        ((if Has_Receiver (Item) then Resource_Value else Integer_Value), Item.Receiver_Resource) and
      (if Has_Receiver (Item) then Item.Ownership_Argument and
         Item.Transfer /= CCL.Imports.Copy_Argument and
         Item.Argument not in Resource_Value | Handler_Value) and
      (if Item.Argument = Resource_Value then Item.Ownership_Argument and
         Item.Transfer /= CCL.Imports.Copy_Argument) and
      Item.Result /= Handler_Value);
   function Scalar_Only (Item : Import_Declaration) return Boolean is
     (Well_Formed (Item) and not Has_Resources (Item) and Item.Argument in Integer_Value | Boolean_Value and
      Item.Result in Integer_Value | Boolean_Value);
   procedure To_Bytecode
     (Item : Import_Declaration; Types : CCL.Types.Registry;
      Argument_Type, Result_Type : CCL.Types.Type_Reference;
      Import : out CCL.VM.Import_Declaration;
      Success : out Boolean;
      Result_Type_Tag : CCL.Ownership.Type_Id := 0;
      Receiver_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type);
   function Matches_Bytecode
     (Compiled : CCL.VM.Import_Declaration; Declared : Import_Declaration) return Boolean;
   function Matches_Bytecode
     (Compiled : CCL.VM.Import_Declaration; Declared : Import_Declaration;
      Types : CCL.Types.Registry) return Boolean;
   -- Typed in-memory linkage. Resource tag/policy approval is a separate
   -- catalog check; this only compares the complete operation signature.
   function Matches (Item : Value; Kind : Value_Kind; Limit : Text_Length) return Boolean is
     (Kind not in Object_Value | Resource_Value and then Item.Kind = Kind and then
      (Item.Kind /= Text_Value or else Item.Content.Length <= Limit));
   -- An object's self-asserted schema key alone never admits it. The host must
   -- supply the independently approved binding and validate the complete image.
   function Matches (Item : Value; Contract : CCL.Objects.Binding) return Boolean is
     (Item.Kind = Object_Value and then CCL.Objects.Validate (Item.Object, Contract));
end CCL.Host_Values;
