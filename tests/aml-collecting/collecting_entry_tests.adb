with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Collecting_Entry_Tests is
   package N is new AML_Namespace (64, AML_Delays.Unavailable_Provider, Max_Retained_Roots => 8);
   package C is new N.Owned.Collecting;
   use type N.Load_Status;
   use type C.Access_Status;
   use type C.Collection_Count;
   use type C.Collection_Statistics;
   use type C.Value_Kind;
   use type AML_Execute.Execution_Status;
   use type Integer_Value;
   use type Byte;
   A : C.Arena;
   Input : aliased AML_Table_Backing.State (1,64);
   Args : constant C.Arguments := [others => (C.Immediate_Argument,3)];
   Outcome : C.Result;
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Report : N.Initialization_Report;
   Checks : Natural := 0;
   function Name(Text:String) return Bytes is
      B:Bytes(1..Text'Length);
   begin for I in B'Range loop B(I):=Character'Pos(Text(Text'First+(I-1))); end loop; return B; end Name;
   function Method(Text:String; Count:Byte; Code:Bytes) return Bytes is
     (Bytes'[16#14#,Byte(Code'Length+6)] & Name(Text) & Bytes'[Count] & Code);
   Buffer_Value : constant Bytes := [16#11#,4,16#0A#,1,16#55#];
   Prefix : constant Bytes := Bytes'[16#08#] & Name("MARK") & Bytes'[0,16#08#] & Name("PKG0") & Bytes'[16#12#,2,0,16#08#] & Name("DST0") & Buffer_Value;
   Concat_Code : constant Bytes := Bytes'[16#A4#,16#73#] & Buffer_Value & Buffer_Value & Bytes'[0];
   Copy_Code : constant Bytes := Bytes'[16#9D#] & Buffer_Value & Name("DST0") & Bytes'[16#A4#] & Name("DST0");
   Bad_Store : constant Bytes := Bytes'[16#75#] & Name("MARK") & Bytes'[16#70#] & Buffer_Value & Name("PKG0") & Bytes'[16#A4#,0];
   Dynamic_Code : constant Bytes := Bytes'[16#08#] & Name("TEMP") & Bytes'[16#11#,3,16#68#,16#55#,16#A4#] & Name("TEMP");
   -- Field bits72 is PkgLength encoding 0x48,0x04, not a numeric AML TermArg.
   Region_Code : constant Bytes := Bytes'[16#5B#,16#88#] & Name("REG0") & Bytes'[16#0D#] & Name("DSDT") & Bytes'[0,16#0D#,0,16#0D#,0,16#5B#,16#81#,12] & Name("REG0") & Bytes'[0] & Name("FLD0") & Bytes'[16#48#,4,16#A4#,16#73#] & Buffer_Value & Name("FLD0") & Bytes'[0];
   Fixture : constant Bytes := Prefix & Method("CONC",0,Concat_Code) & Method("COPY",0,Copy_Code) & Method("FAIL",0,Bad_Store) & Method("READ",0,Bytes'[16#A4#]&Name("MARK")) & Method("DYNB",1,Dynamic_Code) & Method("WIDE",0,Region_Code) &
     Bytes'[16#08#] & Name("PKG1") & Bytes'[16#12#,7,1] & Buffer_Value &
     Method("SREF",0,Bytes'[16#70#] & Buffer_Value & Bytes'[16#88#] & Name("PKG1") & Bytes'[0,0,16#A4#,16#83#,16#88#] & Name("PKG1") & Bytes'[0,0]) &
     Method("CFLR",0,Bytes'[16#75#] & Name("MARK") & Bytes'[16#73#,16#11#,2,0,16#11#,2,0] & Name("MARK") & Bytes'[16#A4#,0]) &
     Method("PCKT",0,Bytes'[16#A4#,16#12#,7,1] & Buffer_Value);
   procedure Check(B:Boolean) is
   begin Checks:=Checks+1; if not B then raise Program_Error with Checks'Image & Outcome.Status'Image & Status'Image & Outcome.Charged'Image; end if; end Check;
   procedure Run(Node:N.Node_ID; Count:C.Argument_Count:=0) is
   begin C.Invoke(A,Input,Node,Args,Count,1000,Outcome,Status); Check(Status=C.Available); end Run;
   procedure Buffer_Result(Length:Natural) is
      H:C.Value_Handle:=Outcome.Handle;
      D:C.Value_Description;
      B:Bytes(1..16); Copied:Natural;
   begin
      Check(Outcome.Status=AML_Execute.Object_Returned);
      C.Describe(A,H,D,Status); Check(Status=C.Available and D.Kind=C.Buffer_Description and D.Length=Length);
      C.Read_Bytes(A,H,0,B,Copied,Status); Check(Status=C.Available and Copied=Length and B(1)=16#55#);
      C.Release(A,H,Status); Check(Status=C.Available);
   end Buffer_Result;
begin
   Input.Count:=1; Input.Tables(1):=(0,64); Input.Data:=[others=>0];
   Input.Data(1..4):=[68,83,68,84]; Input.Data(5):=64; Input.Data(9):=2;
   declare Sum:Byte:=0; begin for B of Input.Data loop Sum:=Sum+B; end loop; Input.Data(10):=0-Sum; end;
   for Width in Integer_Width loop
      C.Reset(A,Status); Check(Status=C.Available);
      C.Load(A,Fixture,Width,Loaded,Status); Check(Status=C.Available and Loaded=N.Loaded);
      C.Seal(A,Report,Status); Check(Status=C.Available);
      Run(4); Buffer_Result(2);
      Run(5); Buffer_Result(1);
      Run(6); Check(Outcome.Status=AML_Execute.Unsupported and Outcome.Charged>0);
      Run(7); Check(Outcome.Status=AML_Execute.Returned and Outcome.Number=1);
      Run(8,1); Buffer_Result(3);
      Run(9); Buffer_Result(10);
      Run(11); Buffer_Result(1);
      Run(12); Check(Outcome.Status=AML_Execute.Empty_Buffer and Outcome.Charged>0);
      Run(7); Check(Outcome.Status=AML_Execute.Returned and Outcome.Number=2);
      Run(13); Check(Outcome.Status=AML_Execute.Object_Returned);
      declare H:C.Value_Handle:=Outcome.Handle; begin C.Release(A,H,Status); Check(Status=C.Available); end;
      Run(4); Buffer_Result(2);
      Check(C.Reclamation_Metrics(A).Freed_Elements>0);
      Check(C.Reclamation_Metrics(A).Completed>0 and C.Reclamation_Metrics(A).Rejected=0);
      declare Before : constant C.Collection_Statistics := C.Reclamation_Metrics(A); begin
         C.Reset(A,Status);
         Check(Status=C.Available and C.Reclamation_Metrics(A)=Before);
         Ada.Text_IO.Put_Line("FREED OBJECTS" & Before.Freed_Objects'Image &
           " BYTES" & Before.Freed_Bytes'Image & " ELEMENTS" & Before.Freed_Elements'Image);
      end;
   end loop;
   Ada.Text_IO.Put_Line("COLLECTING ENTRIES"&Checks'Image);
end Collecting_Entry_Tests;
