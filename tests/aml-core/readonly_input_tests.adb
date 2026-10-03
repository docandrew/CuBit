pragma Ada_2022;
with Ada.Text_IO;
with AML_Execute; use AML_Execute;
with AML_Decode; use AML_Decode;
with AML_Names;
with AML_Integers;
procedure Readonly_Input_Tests is
   use type AML_Names.Parse_Status;
   use type Integer_Value;
   use type Byte;
   type Mutable is limited record
      Active : Natural := 0;
   end record;
   type Backing (Length : Positive) is limited record
      Data : Bytes (1 .. Length) := [others => 0];
   end record;
   function Valid (Environment : Mutable) return Boolean is (Environment.Active <= 33);
   procedure Lookup
     (Environment : in out Mutable; Input : aliased Backing; Scope : Natural;
      Path : AML_Names.Name_Result; Width : Integer_Width;
      Purpose : Binding_Purpose; Binding : out Binding_Result)
   is
      pragma Unreferenced (Environment, Scope);
   begin
      if Path.Kind /= AML_Names.Accepted or else Path.Count /= 1 then
         Binding := (Status => Missing_Binding); return;
      elsif Path.Parts (1) = "FLD0" and then Purpose = Inspect_Binding then
         Binding := (Status => Non_Integer_Binding, Object => (Type_Code => 5, others => <>)); return;
      elsif Path.Parts (1) = "FLD0" then
         Binding := (Status => Integer_Binding,
                 Value => AML_Integers.Normalize
                   (16#1_0000_0000# + Integer_Value (Input.Data (Input.Length)), Width)); return;
      elsif Path.Parts (1) = "MTH0" then
         Binding := (Status => Method_Binding, Method_ID => 1, Parameters => 0); return;
      elsif Path.Parts (1) = "MTH1" then
         Binding := (Status => Method_Binding, Method_ID => 2, Parameters => 0); return;
      else
         Binding := (Status => Missing_Binding); return;
      end if;
   end Lookup;
   function Get_Method (Environment : Mutable; ID : Natural) return Method_Definition is
      pragma Unreferenced (Environment);
   begin
      if ID = 1 then
         return (Exists => True, Length => 5, Code => [16#A4#, 70, 76, 68, 48],
                 Width => Bits_64, Flags => 0, Scope => 1);
      elsif ID = 2 then
         return (Exists => True, Length => 5, Code => [16#A4#, 77, 84, 72, 48],
                 Width => Bits_64, Flags => 0, Scope => 2);
      else
         return (Exists => False, Length => 0);
      end if;
   end Get_Method;
   procedure Write
     (Environment : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Width : Integer_Width; Item : Datum; Status : out Write_Status) is
      pragma Unreferenced (Environment, Scope, Path, Width, Item);
   begin
      Status := Write_Unsupported;
   end Write;
   procedure Begin_Call (Environment : in out Mutable; Scope : Natural; Allowed : out Boolean) is
      pragma Unreferenced (Scope);
   begin
      Allowed := Environment.Active < 33;
      if Allowed then Environment.Active := Environment.Active + 1; end if;
   end Begin_Call;
   procedure End_Call (Environment : in out Mutable; Scope : Natural) is
      pragma Unreferenced (Scope);
   begin
      Environment.Active := Environment.Active - 1;
   end End_Call;
   procedure Define_Method
     (Environment : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Flags : Byte; Width : Integer_Width; Code : Bytes; Status : out Declaration_Status) is
      pragma Unreferenced (Environment, Scope, Path, Flags, Width, Code);
   begin
      Status := Declaration_Unsupported;
   end Define_Method;
   procedure Reject_Fields
     (Environment : in out Mutable; Scope : Natural; Region : AML_Names.Name_Result;
      Flags : AML_Decode.Byte; Entries : AML_Decode.Bytes; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Scope, Region, Flags, Entries);
   begin
      Status := Unsupported;
   end Reject_Fields;
   procedure Reject_Literal
     (Environment : in out Mutable; Kind : Literal_Kind; Data : Bytes; Binding : out Binding_Result)
   is
      pragma Unreferenced (Environment, Kind, Data);
   begin
      Binding := (Status => Failed_Binding, Failure => Unsupported_Value);
   end Reject_Literal;
   procedure Reject_Region
     (Environment : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Natural; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Scope, Path);
   begin
      Token := 0; Status := Unsupported;
   end Reject_Region;
   procedure Reject_Completion
     (Environment : in out Mutable; Input : aliased Backing; Token : Natural;
      Width : Integer_Width; Signature, OEM, Table_ID : Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (Environment, Input, Token, Width, Signature, OEM, Table_ID);
   begin
      Status := Unsupported;
   end Reject_Completion;
   procedure No_Timer (Environment : in out Mutable; Value : out Integer_Value; Available : out Boolean) is
      pragma Unreferenced (Environment);
   begin Value := 0; Available := False; end No_Timer;
   procedure Execute is new Execute_With_Input
     (Mutable, Backing, Valid, Lookup, Get_Method, Write, Begin_Call, End_Call, Define_Method, Reject_Fields, Reject_Literal, Reject_Region, Reject_Completion, No_Timer);
   Input : aliased Backing (1_048_577);
   Environment : Mutable;
   Result : Execution_Result;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for V in Byte loop
      Input.Data (Input.Length) := V;
      for Width in Integer_Width loop
         Execute ([16#A4#, 70, 76, 68, 48], Width, [others => <>], 0, 10,
                  Input, Environment, 0, Result);
         Check (Result.Status = Returned and then Result.Value =
           AML_Integers.Normalize (16#1_0000_0000# + Integer_Value (V), Width));
         Check (Environment.Active = 0);
      end loop;
      Execute ([16#A4#, 16#8E#, 70, 76, 68, 48], Bits_64, [others => <>], 0, 10,
               Input, Environment, 0, Result);
      Check (Result.Status = Returned and then Result.Value = 5);
      -- Both recursive calls must receive this same immutable backing object.
      Execute ([16#A4#, 77, 84, 72, 49], Bits_64, [others => <>], 0, 20,
               Input, Environment, 0, Result);
      Check (Result.Status = Returned and then Result.Value = 16#1_0000_0000# + Integer_Value (V));
      Check (Environment.Active = 0 and then Input.Data (Input.Length) = V);
   end loop;
   for Budget in 0 .. 8 loop
      Execute ([16#A4#, 77, 84, 72, 49], Bits_64, [others => <>], 0, Budget,
               Input, Environment, 0, Result);
      Check (Result.Charged <= Budget and then Environment.Active = 0);
      Check (Result.Status in Returned | Budget_Exceeded);
   end loop;
   Execute ([16#A4#, 77, 84, 72, 49], Bits_64, [others => <>], 0, 20,
            Input, Environment, 0, Result, Calls_Left => 0);
   Check (Result.Status = Call_Limit and then Environment.Active = 0);
   Ada.Text_IO.Put_Line ("AML read-only input checks:" & Checks'Image);
end Readonly_Input_Tests;
