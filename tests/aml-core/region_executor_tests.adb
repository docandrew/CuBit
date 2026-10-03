pragma Ada_2022;
with Ada.Text_IO;
with AML_Execute; use AML_Execute;
with AML_Decode; use AML_Decode;
with AML_Names;
procedure Region_Executor_Tests is
   use type AML_Names.Parse_Status;
   use type Integer_Value;
   use type Byte;
   type Mutable is record
      Active : Natural := 0;
      Reserved : Boolean := False;
      Seen, Completed, Reservations : Natural := 0;
      Reject : Boolean := False;
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
      pragma Unreferenced (Input, Scope, Width, Purpose);
      ID : Natural := 0;
   begin
      if Path.Kind = AML_Names.Accepted and then Path.Count = 1 then
         if Path.Parts (1) = "SIG0" then ID := 1;
         elsif Path.Parts (1) = "OEM0" then ID := 2;
         elsif Path.Parts (1) = "TAB0" then ID := 3;
         elsif Path.Parts (1) = "REG0" and then Environment.Reserved then
            Binding := (Status => Failed_Binding, Failure => Uninitialized); return;
         end if;
      end if;
      if ID = 0 then Binding := (Status => Missing_Binding); return; end if;
      if not Environment.Reserved then raise Program_Error with "lookup before reservation"; end if;
      Environment.Seen := Environment.Seen * 10 + ID;
      Binding := (Status => Method_Binding, Method_ID => ID, Parameters => 0);
   end Lookup;
   function Get_Method (Environment : Mutable; ID : Natural) return Method_Definition is
      pragma Unreferenced (Environment);
   begin
      if ID in 1 .. 3 then
         return (Exists => True, Length => 3, Code => [16#A4#, 16#0A#, Byte (ID)],
                 Width => Bits_64, Flags => 0, Scope => ID);
      end if;
      return (Exists => False, Length => 0);
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
   begin
      Environment.Active := Environment.Active - 1;
      if Scope = 0 then Environment.Reserved := False; end if;
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
   procedure Reserve_Region
     (Environment : in out Mutable; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Natural; Status : out Execution_Status)
   is
      pragma Unreferenced (Scope);
   begin
      Token := 0;
      if Path.Kind /= AML_Names.Accepted or else Path.Count /= 1 or else Path.Parts (1) /= "REG0" then
         Status := Bad_Name;
      elsif Environment.Reserved then Status := Duplicate_Name;
      else
         Environment.Reserved := True;
         Environment.Reservations := Environment.Reservations + 1;
         Token := 1; Status := Returned;
      end if;
   end Reserve_Region;
   procedure Complete_Region
     (Environment : in out Mutable; Input : aliased Backing; Token : Natural;
      Width : Integer_Width; Signature, OEM, Table_ID : Datum; Status : out Execution_Status)
   is
      pragma Unreferenced (Input, Width);
      function Number (D : Datum) return Natural is
        (if D.Is_Object then D.Object.ID else Natural (D.Number));
   begin
      if not Environment.Reserved or else Token /= 1 then raise Program_Error; end if;
      if Number (Signature) /= 1 or else Number (OEM) /= 2 or else Number (Table_ID) /= 3 then
         raise Program_Error with "selector order or values";
      end if;
      if Environment.Reject then Status := Unknown_Name; return; end if;
      Environment.Completed := Environment.Completed + 1;
      Status := Returned;
   end Complete_Region;
   procedure No_Timer (Environment : in out Mutable; Value : out Integer_Value; Available : out Boolean) is
      pragma Unreferenced (Environment);
   begin Value := 0; Available := False; end No_Timer;
   procedure Execute is new Execute_With_Input
     (Mutable, Backing, Valid, Lookup, Get_Method, Write, Begin_Call, End_Call,
      Define_Method, Reject_Fields, Reject_Literal, Reserve_Region, Complete_Region, No_Timer);
   Input : aliased Backing (1);
   Environment : Mutable;
   Result : Execution_Result;
   Checks : Natural := 0;
   Code : constant Bytes := [16#5B#,16#88#,82,69,71,48,
                             83,73,71,48,79,69,77,48,84,65,66,48,16#A4#,1];
   Args_Code : constant Bytes := [16#5B#,16#88#,82,69,71,48,16#68#,16#69#,16#6A#,16#A4#,1];
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Run (Data : Bytes; Budget : Natural := 100) is
   begin
      Execute (Data, Bits_64, [others => <>], 0, Budget, Input, Environment, 0, Result);
      Check (Environment.Active = 0 and not Environment.Reserved and Result.Charged <= Budget);
   end Run;
begin
   Run (Code);
   Check (Result.Status = Returned and then Result.Value = 1);
   Check (Environment.Seen = 123 and Environment.Completed = 1 and Environment.Reservations = 1);
   for N in 0 .. Code'Length loop
      Environment := (others => <>);
      Run (Code (1 .. N));
      Check (Environment.Completed = (if N >= 18 then 1 else 0));
   end loop;
   for Budget in 0 .. 40 loop
      Environment := (others => <>);
      Run (Code, Budget);
      Check (Result.Status in Returned | Budget_Exceeded);
      Check (Environment.Completed <= 1);
   end loop;
   Environment := (others => <>);
   Run (Code (1 .. 18) & Code);
   Check (Result.Status = Duplicate_Name and Environment.Seen = 123 and Environment.Completed = 1);
   Environment := (Reject => True, others => <>);
   Run (Code);
   Check (Result.Status = Unknown_Name and Environment.Seen = 123 and Environment.Completed = 0);
   Environment := (others => <>);
   Run ([16#5B#,16#88#,82,69,71,48,82,69,71,48,0,0]);
   Check (Result.Status = Uninitialized and Environment.Completed = 0);
   Environment := (others => <>);
   Execute (Args_Code, Bits_64,
     [0 => (Is_Object => True, Object => (ID => 1, Type_Code => 2, others => <>)),
      1 => (Is_Object => True, Object => (ID => 2, Type_Code => 2, others => <>)),
      2 => (Is_Object => True, Object => (ID => 3, Type_Code => 2, others => <>)), others => <>],
     3, 100, Input, Environment, 0, Result);
   Check (Result.Status = Returned and Environment.Completed = 1 and Environment.Active = 0 and not Environment.Reserved);
   Ada.Text_IO.Put_Line ("DataTableRegion executor checks:" & Checks'Image);
end Region_Executor_Tests;
