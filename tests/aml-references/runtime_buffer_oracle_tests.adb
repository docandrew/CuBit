with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Runtime_Buffer_Oracle_Tests is
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   use type Integer_Value;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1,1);
   OK : Boolean;
   Loaded_Status : Load_Status;
   R : Execution_Result;
   Before : NS.State;
   Checks : Natural := 0;
   Mark : constant Bytes := [77,65,82,75];
   Helper_Name : constant Bytes := [67,78,84,48];
   function Method_Data (Name, Code : Bytes; Flags : Byte := 0) return Bytes is
     (Bytes'(16#14#,Byte(6+Code'Length)) & Name & Bytes'(1=>Flags) & Code);
   procedure Check(B : Boolean) is
   begin Checks:=Checks+1; if not B then raise Program_Error with Checks'Image & " " & R.Status'Image; end if; end Check;
   procedure Run(Code : Bytes; W : Integer_Width; Expected : Execution_Status;
                 Value : Integer_Value := 0; Arg : Integer_Value := 4;
                 Mark_Value : Integer_Value := 0; Budget : Natural := 200;
                 Helper : Bytes := [16#A4#,16#0A#,4]) is
   begin
      Reset(A,OK); Check(OK);
      Load(A,Bytes'(1=>16#08#)&Mark&Bytes'(1=>0)&Method_Data(Helper_Name,Helper)&
        Method_Data([84,69,83,84],Code,1),W,Loaded_Status); Check(Loaded_Status=Loaded);
      Before:=Snapshot(A);
      Invoke(A,Input,3,[others=>(Integer_Datum,Arg,Ordinary_Integer)],1,Budget,R);
      Check(R.Status=Expected); if Expected=Returned then Check(R.Value=Value); end if;
      Check(Node_Count(A)=3 and Integer_Data(Snapshot(A),1)=Mark_Value);
      if Mark_Value=0 then pragma Assert(Allocating_Cleanup_Frame(Snapshot(A),Before)); end if;
      Check(R.Charged<=Budget);
   end Run;
begin
   Run([8,84,69,77,80,17,3,104,161,164,135,84,69,77,80], Bits_32, Returned, 2, Arg=>4294967298);
   Run([8,84,69,77,80,17,5,104,161,162,163,164,135,84,69,77,80], Bits_32, Returned, 3, Arg=>4294967296);
   Run([8,84,69,77,80,17,3,104,161,164,135,84,69,77,80], Bits_32, Value_Limit, Arg=>4294968321);
   Run([8,84,69,77,80,17,4,10,2,161,164,135,84,69,77,80], Bits_32, Returned, 2, Arg=>0);
   Run([112,13,49,48,48,48,48,48,48,48,50,0,96,8,84,69,77,80,17,3,96,161,164,135,84,69,77,80], Bits_32, Value_Limit, Arg=>0);
   Run([112,17,11,10,8,2,0,0,0,1,0,0,0,96,8,84,69,77,80,17,3,96,161,164,135,84,69,77,80], Bits_32, Returned, 2, Arg=>0);
   Run([8,84,69,77,80,17,3,104,161,164,135,84,69,77,80], Bits_64, Returned, 2, Arg=>4294967298);
   Run([8,84,69,77,80,17,5,104,161,162,163,164,135,84,69,77,80], Bits_64, Returned, 3, Arg=>4294967296);
   Run([8,84,69,77,80,17,3,104,161,164,135,84,69,77,80], Bits_64, Value_Limit, Arg=>4294968321);
   Run([8,84,69,77,80,17,4,10,2,161,164,135,84,69,77,80], Bits_64, Returned, 2, Arg=>0);
   Run([112,13,49,48,48,48,48,48,48,48,50,0,96,8,84,69,77,80,17,3,96,161,164,135,84,69,77,80], Bits_64, Returned, 2, Arg=>0);
   Run([112,17,11,10,8,2,0,0,0,1,0,0,0,96,8,84,69,77,80,17,3,96,161,164,135,84,69,77,80], Bits_64, Returned, 2, Arg=>0);
   Ada.Text_IO.Put_Line("Runtime Buffer oracle checks"&Checks'Image);
end Runtime_Buffer_Oracle_Tests;
