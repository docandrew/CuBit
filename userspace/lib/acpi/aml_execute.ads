pragma Ada_2022;
with AML_Decode;
with AML_Names;
with AML_Coercions;
package AML_Execute with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   type Arguments is array (Natural range 0 .. 6) of AML_Decode.Integer_Value;
   -- Immutable value provenance within one bound namespace snapshot. This is
   -- not an AML RefOf/Index reference and must never be used to mutate a source.
   type Object_Value is record
      ID : Natural := 0;
      Type_Code : Natural range 0 .. 16 := 0;
      Size : Natural := 0;
      Conversion_32, Conversion_64 : AML_Coercions.Result;
   end record;
   type Datum (Is_Object : Boolean := False) is record
      case Is_Object is
         when False => Number : AML_Decode.Integer_Value := 0;
         when True => Object : Object_Value;
      end case;
   end record;
   type Value_Arguments is array (Natural range 0 .. 6) of Datum;
   function As_Values (Args : Arguments) return Value_Arguments with
     Post => (for all I in Args'Range => not As_Values'Result (I).Is_Object
       and then As_Values'Result (I).Number = Args (I));
   type Execution_Status is
     (Returned, Object_Returned, No_Return, Truncated, Unsupported, Uninitialized,
      Missing_Argument, Budget_Exceeded, Invalid_Method, Argument_Mismatch, Mutex_Order, Duplicate_Name, Namespace_Limit, Expression_Limit, Block_Limit, Bad_Package, Invalid_Control,
      Unknown_Name, Unsupported_Value, Bad_Name, Call_Limit, Division_By_Zero, Empty_Buffer, Missing_Result, Value_Limit);
   type Execution_Result (Status : Execution_Status := No_Return) is record
      Charged : Natural;
      case Status is
         when Returned => Value : AML_Decode.Integer_Value;
         when Object_Returned => Object : Object_Value;
         when others => null;
      end case;
   end record;
   type Binding_Status is (Integer_Binding, Method_Binding, Missing_Binding, Non_Integer_Binding, Failed_Binding);
   type Binding_Result (Status : Binding_Status := Missing_Binding) is record
      case Status is
         when Failed_Binding => Failure : Execution_Status;
         when Integer_Binding => Value : AML_Decode.Integer_Value;
         when Method_Binding =>
            Method_ID : Natural;
            Parameters : Natural range 0 .. 7;
         when Non_Integer_Binding =>
            Object : Object_Value;
         when others => null;
      end case;
   end record;
   Max_Method_Bytes : constant := 65536;
   subtype Method_Length is Natural range 0 .. Max_Method_Bytes;
   type Method_Definition
     (Exists : Boolean := False; Length : Method_Length := 0) is record
      case Exists is
         when True =>
            Code : AML_Decode.Bytes (1 .. Length);
            Width : AML_Decode.Integer_Width;
            Flags : AML_Decode.Byte;
            Scope : Natural;
         when False => null;
      end case;
   end record;
   subtype Call_Budget is Natural range 0 .. 32;
   subtype Sync_Level is Natural range 0 .. 15;
   -- This synchronous executor requires exclusive ownership of its context.
   -- No operation currently yields to another AML thread or acquires a Mutex.
   function Method_Level (Flags : AML_Decode.Byte) return Sync_Level;
   type Write_Status is (Written, Write_Missing, Write_Unsupported);
   type Declaration_Status is (Declared, Declaration_Duplicate, Declaration_Missing,
                               Declaration_Full, Declaration_Unsupported);
   type Literal_Kind is (String_Literal, Buffer_Literal);
   type Binding_Purpose is (Inspect_Binding, Evaluate_Binding);
   -- Immutable external data travels explicitly through recursive calls. A
   -- limited formal prevents copying the backing store into executor frames.
   generic
      type Context is private;
      type Read_Context (<>) is limited private;
      with function Context_Valid (Environment : Context) return Boolean;
      with procedure Lookup
        (Environment : in out Context; Input : aliased Read_Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Purpose : Binding_Purpose;
         Binding : out Binding_Result);
      with function Get_Method (Environment : Context; ID : Natural)
         return Method_Definition;
      with procedure Write
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
         Status : out AML_Execute.Write_Status);
      with procedure Begin_Call
        (Environment : in out Context; Scope : Natural; Allowed : out Boolean);
      with procedure End_Call (Environment : in out Context; Scope : Natural);
      with procedure Define_Method
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width;
         Code : AML_Decode.Bytes; Status : out Declaration_Status);
      with procedure Define_Fields
        (Environment : in out Context; Scope : Natural; Region : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes;
         Status : out Execution_Status);
      with procedure Materialize
        (Environment : in out Context; Kind : Literal_Kind; Data : AML_Decode.Bytes;
         Binding : out Binding_Result);
   procedure Execute_With_Input
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Input : aliased Read_Context; Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
     with Global => null, Always_Terminates,
          Pre => Argument_Count <= 7 and then not Result_Out'Constrained
            and then Context_Valid (Environment),
          Post => Result_Out.Charged <= Budget and then Context_Valid (Environment),
          Subprogram_Variant => (Decreases => Calls_Left, Decreases => Natural'(3));
   generic
      type Context is private;
      with function Context_Valid (Environment : Context) return Boolean;
      with function Lookup
        (Environment : Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width) return Binding_Result;
      with function Get_Method (Environment : Context; ID : Natural)
         return Method_Definition;
      with procedure Write
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width; Item : AML_Execute.Datum;
         Status : out AML_Execute.Write_Status);
      with procedure Begin_Call
        (Environment : in out Context; Scope : Natural; Allowed : out Boolean);
      with procedure End_Call (Environment : in out Context; Scope : Natural);
      with procedure Define_Method
        (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
         Flags : AML_Decode.Byte; Width : AML_Decode.Integer_Width;
         Code : AML_Decode.Bytes; Status : out Declaration_Status);
   procedure Execute_Typed
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : in out Context; Scope : Natural; Result_Out : out Execution_Result;
      Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0)
     with Global => null, Always_Terminates,
          Pre => Argument_Count <= 7 and then not Result_Out'Constrained
            and then Context_Valid (Environment),
          Post => Result_Out.Charged <= Budget and then Context_Valid (Environment);
   generic
      type Context is private;
      with function Context_Valid (Environment : Context) return Boolean;
      with function Lookup
        (Environment : Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width) return Binding_Result;
      with function Get_Method (Environment : Context; ID : Natural)
         return Method_Definition;
   function Run_Typed
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Value_Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : Context; Scope : Natural; Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0) return Execution_Result
     with Global => null, Pre => Argument_Count <= 7 and then Context_Valid (Environment),
          Post => Run_Typed'Result.Charged <= Budget;
   generic
      type Context is private;
      with function Context_Valid (Environment : Context) return Boolean;
      with function Lookup
        (Environment : Context; Scope : Natural; Path : AML_Names.Name_Result;
         Width : AML_Decode.Integer_Width)
         return Binding_Result;
      with function Get_Method (Environment : Context; ID : Natural)
         return Method_Definition;
   function Run_Bound
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Arguments; Argument_Count : Natural; Budget : Natural;
      Environment : Context; Scope : Natural; Calls_Left : Call_Budget := 32; Current_Sync : Sync_Level := 0) return Execution_Result
     with Global => null, Pre => Argument_Count <= 7 and then Context_Valid (Environment),
          Post => Run_Bound'Result.Charged <= Budget;
   --  Integer-argument entrypoints into the typed core with bounded control
   --  flow. Run has no namespace; Run_Bound uses a read-only binding. No I/O.
   --  External argument
   --  values survive direct reads/returns; literals, arithmetic results and
   --  predicate conversion observe the table integer width.
   --  Charge one unit before each statement and each source operand.
   function Run
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Arguments; Argument_Count : Natural; Budget : Natural)
      return Execution_Result
     with Pre => Argument_Count <= 7,
          Post => Run'Result.Charged <= Budget;
end AML_Execute;
