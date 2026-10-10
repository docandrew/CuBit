with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Names;
with AML_Objects;
with AML_Table_Backing;
procedure Tointeger_Target_Tests is
   use type Integer_Value;
   use type AML_Objects.Allocation_Status;
   procedure Clock (Value : out Integer_Value; Available : out Boolean);
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider, Clock);
   use NS; use NS.Owned;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1,1);
   Result : Execution_Result;
   Loaded_Status : Load_Status;
   OK : Boolean;
   Checks, Calls : Natural := 0;
   Reserve : Boolean := False;
   Token : Name_Reservation;
   Status : Execution_Status;
   Current : constant Node_ID := 4;
   Target : Reference;
   ID : AML_Objects.Object_ID;
   Allocated : AML_Objects.Allocation_Status;
   Prior : State;
   NVAR : constant Bytes := [78,86,65,82];
   STR0 : constant Bytes := [83,84,82,48];
   BUF0 : constant Bytes := [66,85,70,48];
   TEMP : constant Bytes := [84,69,77,80];
   Timer : constant Bytes := [16#5B#,16#33#];
   procedure Check (B : Boolean) is
   begin Checks:=Checks+1; if not B then raise Program_Error with Checks'Image & Result.Status'Image; end if; end Check;
   procedure Clock (Value : out Integer_Value; Available : out Boolean) is
      Located : Lookup_Result;
   begin
      Value:=0; Available:=True;
      if not Reserve then return; end if;
      Calls:=Calls+1;
      if Calls=1 then
         Reserve_Name (A, Current, AML_Names.Read_Name (TEMP), Token, Status); Check (Status=Returned);
      else
         Located:=Resolve(Snapshot(A),Current,AML_Names.Read_Name(TEMP)); Check(Located.Status=Found);
         Check (Has_Integer(Snapshot(A),Located.Node));
         Check (Integer_Data(Snapshot(A),Located.Node)=1);
      end if;
   end Clock;
   procedure Load_Code (Code : Bytes; Width : Integer_Width) is
   begin
      Reset(A,OK);Check(OK);
      Load(A,Bytes'(1=>16#08#)&NVAR&Bytes'(1=>0)
        & Bytes'(1=>16#08#)&STR0&Bytes'(16#0D#,97,0)
        & Bytes'(1=>16#08#)&BUF0&Bytes'(16#11#,3,1,7)
        & Bytes'(16#14#,Byte(6+Code'Length),84,69,83,84,0)&Code,
        Width,Loaded_Status);Check(Loaded_Status=Loaded);
   end Load_Code;
   procedure Run (Code : Bytes; Width : Integer_Width; Expected : Execution_Status; Value : Integer_Value:=0) is
   begin
      Load_Code(Code,Width);
      Invoke(A,Input,Current,[others=><>],0,200,Result);
      Check(Result.Status=Expected);
      if Expected=Returned then Check(Result.Value=Value);end if;
   end Run;
begin
   for Width in Integer_Width loop
      Run(Bytes'(16#99#,16#0D#,49,55,0)&STR0&Bytes'(16#A4#,16#8E#)&STR0,Width,Returned,1);
      Check(Has_Integer(Snapshot(A),2) and then Integer_Data(Snapshot(A),2)=17);
      Run(Bytes'(16#99#,16#0D#,49,55,0)&BUF0&Bytes'(1=>16#A4#)&BUF0,Width,Returned,17);
      Run(Bytes'(16#99#,16#11#,2,0)&STR0&Bytes'(16#A4#,1),Width,Empty_Buffer);
      Check(String_Data(Snapshot(A),2)="a");
      Run(Bytes'(16#99#,16#FF#)&STR0&Bytes'(1=>16#A4#)&STR0,Width,Returned,
          (if Width=Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
      Run(Bytes'(16#99#,1)&NVAR&Bytes'(1=>16#A4#)&NVAR,Width,Returned,1);
      Reserve:=True;Calls:=0;
      Run(Timer&Bytes'(16#99#,1)&TEMP&Timer&Bytes'(1=>16#A4#)&TEMP,Width,Returned,1);
      Check(Calls=2);Reserve:=False;
      -- Source One needs no allocation. Fill object pool after loading code:
      -- explicit replacement must fail atomically without changing STR0.
      Load_Code(Bytes'(16#99#,1)&STR0&Bytes'(16#A4#,1),Width);
      while Values_Used(A).Objects < AML_Objects.Max_Objects loop
         Append(A,Bytes'(1..0=>0),ID,Allocated);Check(Allocated=AML_Objects.Allocated);
      end loop;
      Prior:=Snapshot(A);
      Invoke(A,Input,Current,[others=><>],0,200,Result);
      Check(Result.Status=Value_Limit and then Snapshot(A)=Prior);
      Make_Named_Reference(A,2,Target,OK);Check(OK);
      Store_Reference_Value(A,Target,Width,(Integer_Datum,1,AML_Constant),Status,Explicit_Result_Target);
      Check(Status=Value_Limit and then Snapshot(A)=Prior);
      -- Existing Integer receives a value update even at object exhaustion.
      Make_Named_Reference(A,1,Target,OK);Check(OK);
      ID:=Data_Object(Snapshot(A),1);
      Store_Reference_Value(A,Target,Width,(Integer_Datum,7,Ordinary_Integer),Status,Explicit_Result_Target);
      Check(Status=Returned and then Data_Object(Snapshot(A),1)=ID);
      Check(Integer_Data(Snapshot(A),1)=7);
   end loop;
   Ada.Text_IO.Put_Line("TOINTEGER TARGET PASS"&Checks'Image);
end Tointeger_Target_Tests;
