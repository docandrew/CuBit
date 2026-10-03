with AML_Names;
package body Timer_Verification with SPARK_Mode is
   use type AML_Clock.Sample_Status;
   use AML_Execute;
   use AML_Decode;
   type Empty_Input is limited null record;
   function Valid (E : Clock_State) return Boolean
     with Post => Valid'Result
   is
      pragma Unreferenced (E);
   begin return True; end Valid;
   procedure Lookup
     (E : in out Clock_State; Input : aliased Empty_Input; Scope : Natural;
      Path : AML_Names.Name_Result; Width : Integer_Width;
      Purpose : Binding_Purpose; Binding : out Binding_Result)
     with Pre => Valid (E) and not Binding'Constrained, Post => Valid (E)
   is
      pragma Unreferenced (Input, Scope, Path, Width, Purpose);
   begin Binding := (Status => Missing_Binding); end Lookup;
   function Get_Method (E : Clock_State; ID : Natural) return Method_Definition is
      pragma Unreferenced (E, ID);
   begin return (Exists => False, Length => 0); end Get_Method;
   procedure Write
     (E : in out Clock_State; Scope : Natural; Path : AML_Names.Name_Result;
      Width : Integer_Width; Item : Datum; Status : out Write_Status)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope, Path, Width, Item);
   begin Status := Write_Unsupported; end Write;
   procedure Begin_Call (E : in out Clock_State; Scope : Natural; Allowed : out Boolean)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope);
   begin
      Allowed := E.Active < 33;
      if Allowed then E.Active := E.Active + 1; end if;
   end Begin_Call;
   procedure End_Call (E : in out Clock_State; Scope : Natural)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope);
   begin if E.Active > 0 then E.Active := E.Active - 1; end if; end End_Call;
   procedure Define_Method
     (E : in out Clock_State; Scope : Natural; Path : AML_Names.Name_Result;
      Flags : Byte; Width : Integer_Width; Code : Bytes; Status : out Declaration_Status)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope, Path, Flags, Width, Code);
   begin Status := Declaration_Unsupported; end Define_Method;
   procedure Fields
     (E : in out Clock_State; Scope : Natural; Region : AML_Names.Name_Result;
      Flags : Byte; Entries : Bytes; Status : out Execution_Status)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope, Region, Flags, Entries);
   begin Status := Unsupported; end Fields;
   procedure Literal
     (E : in out Clock_State; Kind : Literal_Kind; Data : Bytes; Binding : out Binding_Result)
     with Pre => Valid (E) and not Binding'Constrained, Post => Valid (E)
   is
      pragma Unreferenced (Kind, Data);
   begin Binding := (Status => Failed_Binding, Failure => Unsupported_Value); end Literal;
   procedure Reserve
     (E : in out Clock_State; Scope : Natural; Path : AML_Names.Name_Result;
      Token : out Natural; Status : out Execution_Status)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Scope, Path);
   begin Token := 0; Status := Unsupported; end Reserve;
   procedure Complete
     (E : in out Clock_State; Input : aliased Empty_Input; Token : Natural;
      Width : Integer_Width; Signature, OEM, Table_ID : Datum; Status : out Execution_Status)
     with Pre => Valid (E), Post => Valid (E)
   is
      pragma Unreferenced (Input, Token, Width, Signature, OEM, Table_ID);
   begin Status := Unsupported; end Complete;
   procedure Read_Timer
     (E : in out Clock_State; Value : out Integer_Value; Available : out Boolean)
     with Pre => Valid (E), Post => Valid (E)
   is
      Status : AML_Clock.Sample_Status;
   begin
      AML_Clock.Observe (E.Adapter, E.Microseconds, E.Available, Value, Status);
      Available := Status = AML_Clock.Accepted;
      if E.Reads < Natural'Last then E.Reads := E.Reads + 1; end if;
   end Read_Timer;
   procedure Execute is new Execute_With_Input
     (Clock_State, Empty_Input, Valid, Lookup, Get_Method, Write,
      Begin_Call, End_Call, Define_Method, Fields, Literal, Reserve, Complete, Read_Timer);
   procedure Run
     (Code : Bytes; Width : Integer_Width; Budget : Natural;
      Clock : in out Clock_State; Result : out Execution_Result)
   is
      Input : aliased Empty_Input;
   begin
      Execute (Code, Width, [others => <>], 0, Budget, Input, Clock, 0, Result);
   end Run;
end Timer_Verification;
