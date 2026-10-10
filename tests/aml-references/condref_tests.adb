with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with AML_References;
procedure Condref_Tests is
   package NS is new AML_Namespace (32, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   use type Integer_Value;
   use type AML_References.Reference;
   A, Foreign : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   OK : Boolean;
   L : Load_Status;
   R : Execution_Result;
   Prior : NS.State;
   Node : Node_ID;
   Bound_Status : Bind_Status;
   Ref : AML_References.Reference;
   Metadata : Reference_Metadata;
   Status : Execution_Status;
   Copy : Datum;
   Checks : Natural := 0;
   Conditional : constant Bytes := [16#5B#,16#12#];
   Obj : constant Bytes := [79,66,74,48];
   Pkg : constant Bytes := [80,75,71,48];
   Missing : constant Bytes := [77,73,83,83];
   Names : constant array (1 .. 5) of Bytes (1 .. 4) :=
     [[77,84,72,48], [77,84,72,49], [68,69,86,48], [82,69,71,48], [70,76,68,48]];
   Types : constant array (1 .. 5) of Integer_Value := [8,8,6,10,5];
   Nodes : constant array (1 .. 5) of Node_ID := [3,4,5,7,8];
   function Method_Data (Name : Bytes; Code : Bytes; Flags : Byte := 0) return Bytes is
     (Bytes'(16#14#, Byte (Code'Length + 6)) & Name & Bytes'(1 => Flags) & Code);
   procedure Check (B : Boolean) is
   begin Checks := Checks + 1; if not B then raise Program_Error with Checks'Image & " " & R.Status'Image; end if; end Check;
   procedure Prepare (Code : Bytes; W : Integer_Width) is
      Prefix : constant Bytes := Bytes'(1=>16#08#) & Obj & Bytes'(16#0A#,3,16#08#,71,76,79,66,0)
        & Method_Data (Names (1), [16#70#,1,71,76,79,66,16#A4#,1])
        & Method_Data (Names (2), [16#70#,1,71,76,79,66,16#A4#,16#68#], 1)
        & Bytes'(16#5B#,16#82#,5,68,69,86,48);
   begin
      Reset (A, OK); Check (OK);
      Load (A, Prefix & Method_Data ([84,69,83,84], Code), W, L); Check (L = Loaded);
      Bind_Table_Region (A, Root, "REG0", (1,1), Node, Bound_Status); Check (Bound_Status = Bound and Node = 7);
      Bind_Table_Field (A, Root, "FLD0", ((1,1),0,8), Node, Bound_Status); Check (Bound_Status = Bound and Node = 8);
      Load (A, Bytes'(1=>16#08#) & Pkg & Bytes'(16#12#,5,3,0,0,0), W, L); Check(L=Loaded);
   end Prepare;
   procedure Run (Code : Bytes; W : Integer_Width; Expected : Execution_Status;
                  Value : Integer_Value := 0; Budget : Natural := 100) is
   begin
      Prepare (Code,W); Prior := Snapshot (A);
      Invoke (A, Input, 6, [others => (Integer_Datum,0,Ordinary_Integer)],0,Budget,R);
      Check (R.Status = Expected);
      if Expected = Returned then Check (R.Value = Value and R.Origin = Ordinary_Integer); end if;
      Check (Cleanup_Frame (Snapshot (A),Prior));
      Check (R.Charged <= Budget);
   end Run;
begin
   for W in Integer_Width loop
      for I in Names'Range loop
         Run (Bytes'(1=>16#A4#) & Conditional & Names(I) & Bytes'(1 => 0),W,Returned,
           (if W = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
         Run (Conditional & Names(I) & Bytes'(16#60#,16#A4#,16#8E#,16#60#),W,Returned,Types(I));
         Run (Conditional & Names(I) & Bytes'(16#60#,16#A4#,16#87#,16#60#),W,Unsupported_Value);
         Run (Conditional & Names(I) & Bytes'(16#60#,16#A4#,16#83#,16#60#),W,Unsupported_Value);
         Prepare ([16#A4#,0],W); Prior := Snapshot(A);
         Make_Named_Identity(A,Nodes(I),Ref,OK); Check(OK and Named_Identity_Matches(A,Ref));
         Check(not Matches(A,Ref)); Describe_Named_Identity(A,Ref,Metadata);
         Check(Metadata.Kind = Metadata_Only and then Integer_Value (Named_Metadata_Type'Enum_Rep(Metadata.Object_Type))=Types(I));
         Store_Reference_Value(A,Ref,W,(Integer_Datum,7,Ordinary_Integer),Status);
         Check(Status=Unsupported_Value and Snapshot(A)=Prior);
         Copy_And_Attach(A,(Referenced_Destination,Ref),W,(Integer_Datum,7,Ordinary_Integer),Copy,Status);
         Check(Status=Unsupported_Value and Snapshot(A)=Prior);
         Reset(Foreign,OK); Check(OK); Describe_Named_Identity(Foreign,Ref,Metadata);
         Check(Metadata.Kind=Invalid_Reference);
         Reset(A,OK); Check(OK); Describe_Named_Identity(A,Ref,Metadata);
         Check(Metadata.Kind=Invalid_Reference);
      end loop;
      Run(Bytes'(1=>16#A4#)&Conditional&Missing&Bytes'(1=>0),W,Returned,0);
      Run(Bytes'(1=>16#A4#)&Conditional&Bytes'(16#60#,0),W,Returned,
        (if W=Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
      Run(Bytes'(1=>16#A4#)&Conditional&Missing&Missing,W,Unknown_Name);
      Run(Bytes'(16#A4#,16#5B#),W,Truncated);
      Run(Bytes'(1=>16#A4#)&Conditional,W,Truncated);
      Run(Bytes'(1=>16#A4#)&Conditional&Obj,W,Truncated);
      Run(Bytes'(1=>16#A4#)&Conditional&Obj&Bytes'(1=>0),W,Budget_Exceeded,Budget=>1);
   end loop;
   Ada.Text_IO.Put_Line("CondRefOf checks" & Checks'Image);
end Condref_Tests;
