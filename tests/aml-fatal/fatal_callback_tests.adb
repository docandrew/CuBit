with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Frame_Handles;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Names;
with Root_Test_Support; use Root_Test_Support;
procedure Fatal_Callback_Tests is
   use type Bytes;
   type Context is record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Calls, Entries, Leaves, Frames : Natural := 0;
   end record;
   function Valid (Environment : Context) return Boolean is
     (Environment.Calls <= 33 and then Environment.Frames <= 33);
   function Lookup (Environment : Context; Scope : Natural; Path : AML_Names.Name_Result;
      Width : Integer_Width) return Binding_Result is
      pragma Unreferenced (Environment, Scope, Path, Width);
   begin return (Status => Method_Binding, Method_ID => 1, Parameters => 0); end Lookup;
   function Get_Method (Environment : Context; ID : Natural) return Method_Definition is
      pragma Unreferenced (Environment, ID);
   begin
      return (Exists => True, Implementation => AML_Implementation, Length => 2,
        Code => (Length => 2, Bytes => [16#A4#,1]), Width => Bits_64, Flags => 8, Scope => 1);
   end Get_Method;
   procedure Write (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
      Width : Integer_Width; Item : Datum; Status : out Write_Status) is
      pragma Unreferenced (Environment, Scope, Path, Width, Item);
   begin Status := Write_Unsupported; end Write;
   procedure Begin_Call (Environment : in out Context; Scope : Natural; Allowed : out Boolean) is
      pragma Unreferenced (Scope);
   begin Allowed := Environment.Calls < 33; if Allowed then Environment.Calls := Environment.Calls + 1; end if; end Begin_Call;
   procedure End_Call (Environment : in out Context; Scope : Natural) is
      pragma Unreferenced (Scope);
   begin Environment.Calls := Environment.Calls - 1; end End_Call;
   procedure Define_Method (Environment : in out Context; Scope : Natural; Path : AML_Names.Name_Result;
      Flags : Byte; Width : Integer_Width; Code : Bytes; Status : out Declaration_Status) is
      pragma Unreferenced (Environment, Scope, Path, Flags, Width, Code);
   begin Status := Declaration_Unsupported; end Define_Method;
   procedure Admit (Environment : in out Context; Domain : out AML_Frame_Handles.Invocation_Domain;
      Status : out Invocation_Status) is
   begin Domain := AML_Frame_Handles.Bind_Domain (Environment.Owner,1); Status := Available; end Admit;
   procedure Open_Frame (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle) is
      pragma Unreferenced (Frame);
   begin Environment.Frames := Environment.Frames + 1; end Open_Frame;
   procedure Close_Frame (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle) is
      pragma Unreferenced (Frame);
   begin Environment.Frames := Environment.Frames - 1; end Close_Frame;
   procedure Enter_Method (Environment : in out Context; Method : Natural;
      Frame : AML_Frame_Handles.Frame_Handle; Status : in out Execution_Status) is
      pragma Unreferenced (Method, Frame);
   begin Environment.Entries := Environment.Entries + 1; Status := Fatal_Requested; end Enter_Method;
   procedure Leave_Method (Environment : in out Context; Frame : AML_Frame_Handles.Frame_Handle;
      Status : in out Execution_Status) is
      pragma Unreferenced (Frame);
   begin Environment.Leaves := Environment.Leaves + 1; Status := Returned; end Leave_Method;
   procedure Execute is new Execute_Session_Typed
     (Context, Valid, Lookup, Get_Method, Write, Begin_Call, End_Call, Define_Method,
      Admit, Open_Frame, Close_Frame, Enter_Method, Leave_Method);
   Environment : Context;
   Success : Boolean;
   Result : Execution_Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Label; end if; end Check;
begin
   AML_Identity.Issuer.Issue (Environment.Owner,Success); Check (Success,"actual test domain owner");
   for Width in Integer_Width loop
      Execute ([16#A4#] & Enc ("SERL"),Width,
        [others => (Integer_Datum,0,Ordinary_Integer)],0,100,Environment,0,Result);
      Check (Result.Status = Unsupported_Value,"entry callback has no fatal payload authority");
      Check (Environment.Calls = 0 and then Environment.Frames = 0,"failed entry unwinds real call frames");
      Check (Environment.Leaves = 0,"failed entry never claims acquired method lock");
   end loop;
   Check (Environment.Entries = 2,"both widths reached forged entry callback");
   Ada.Text_IO.Put_Line ("Fatal callback checks" & Checks'Image);
end Fatal_Callback_Tests;
