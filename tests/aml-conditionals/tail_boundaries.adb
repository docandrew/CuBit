with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Tail_Boundaries is
 use type Integer_Value;
 package N is new AML_Namespace (512);
 use N; use N.Owned;
 A : Arena;
 Input : aliased AML_Table_Backing.State (1, 32);
 Checks : Natural := 0;
 procedure Check(Good : Boolean; Label : String) is
 begin Checks := Checks + 1; if not Good then raise Program_Error with Label & Checks'Image; end if; end Check;
 procedure Test(Label : String; Data : Bytes; Arg : Integer_Value; Expected : Execution_Status; Value : Integer_Value; Budget : Natural; Width : Integer_Width) is
  OK : Boolean; Loaded_Status : Load_Status; Result : Execution_Result;
  Pin : Retained_Root; Retention : Invocation_Retention_Status;
  Args : Value_Arguments := [others => (Integer_Datum, 0, Ordinary_Integer)];
 begin
  Reset(A,OK); Check(OK,Label);
  Load(A,Data,Width,Loaded_Status); Check(Loaded_Status=Loaded,Label);
  Args(0).Number:=Arg;
  Invoke_Retained(A,Input,1,Args,1,Budget,Result,Pin,Retention);
  Check(Result.Status=Expected,Label & Result.Status'Image);
  if Expected=Returned then Check(Result.Value=Value,Label); end if;
  Check(Pin=No_Retained_Root and Retention=No_Root_Required,Label);
  Check(Frame_Root_Count(A)=0 and Retained_Count(A)=0,Label);
  if Label = "exact fuel" or Label = "short fuel" then Check(Result.Charged=Budget,Label); end if;
  Check(Node_Count(A)=1,Label & " nodes");
  Check(Methods_Used(A)=Data'Length-7,Label & " method bytes");
 end Test;
begin
 for Width in Integer_Width loop
  Test("exact fuel", [16#14#,11,16#54#,16#45#,16#53#,16#54#,1,16#A0#,4,1,16#A4#,1],0,Returned,1,4,Width);
  Test("short fuel", [16#14#,11,16#54#,16#45#,16#53#,16#54#,1,16#A0#,4,1,16#A4#,1],0,Budget_Exceeded,0,3,Width);
  Test("tail declaration", [16#14#,20,16#54#,16#45#,16#53#,16#54#,1,16#A0#,13,1,16#08#,16#54#,16#45#,16#4D#,16#50#,1,16#A4#,16#54#,16#45#,16#4D#,16#50#],0,Returned,1,100,Width);
 end loop;
 Ada.Text_IO.Put_Line("TAIL BOUNDARIES PASS" & Checks'Image);
end Tail_Boundaries;
