with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Owner_Retention_Tests is
   package N is new AML_Namespace (16, AML_Delays.Unavailable_Provider, Max_Retained_Roots => 2);
   use N; use N.Owned;
   use type AML_Objects.Allocation_Status;
   use type AML_References.Reference;
   use type AML_References.Reference_Kind;
   A, Foreign : Arena;
   Root_Pin, Copy, Second : Retained_Root;
   RS : Retain_Status; Released_Status : Release_Status;
   Status : Execution_Status;
   Value, Readback, Original : Datum;
   ID : AML_Objects.Object_ID;
   Allocation : AML_Objects.Allocation_Status;
   Source : AML_References.Object_Handle;
   Ref : AML_References.Reference;
   OK : Boolean;
   Loaded_Status : Load_Status;
   Input : aliased AML_Table_Backing.State (1,36);
   Result : Execution_Result;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin Checks:=Checks+1; if not B then raise Program_Error with Checks'Image; end if; end Check;
   procedure Drop (Pin : in out Retained_Root) is
   begin Release(A,Pin,Released_Status); Check(Released_Status=Released and then Pin=No_Retained_Root); end Drop;
   procedure Check_Descriptor (R : AML_References.Reference) is
   begin
      Retain(A,(Reference_Datum,R),Root_Pin,RS); Check(RS=Retained);
      Read_Retained(A,Root_Pin,Readback,Status);
      Check(Status=Returned and then Readback=(Datum'(Reference_Datum,R)));
      Drop(Root_Pin);
   end Check_Descriptor;
   procedure Run (Code : Bytes; Fresh : Boolean := True) is
   begin
      if Fresh then
         Reset(A,OK); Check(OK);
         Load(A,Bytes'[16#14#,Byte(6+Code'Length),84,69,83,84,0]&Code,Bits_64,Loaded_Status);
         Check(Loaded_Status=Loaded);
      end if;
      Invoke(A,Input,1,[others=>(Integer_Datum,0,Ordinary_Integer)],0,1000,Result);
   end Run;
begin
   Retain(A,(Integer_Datum,1,Ordinary_Integer),Root_Pin,RS); Check(RS=Invalid_Value and then Retained_Count(A)=0);
   Read_Retained(A,No_Retained_Root,Readback,Status); Check(Status=Unsupported_Value);
   Release(A,Root_Pin,Released_Status); Check(Released_Status=Invalid_Root);
   Reset(A,OK); Check(OK); Reset(Foreign,OK); Check(OK);
   Retain(A,(Reference_Datum,AML_References.No_Reference),Root_Pin,RS); Check(RS=Invalid_Value);
   Append(A,[65,66],ID,Allocation); Check(Allocation=AML_Objects.Allocated);
   Make_Source(A,ID,Source,OK); Check(OK); Read_Source(A,Source,Value,Status); Check(Status=Returned);
   Original:=Value;
   Value.Object.Size:=999; Value.Object.Type_Code:=16;
   Retain(A,Value,Root_Pin,RS); Check(RS=Retained); Copy:=Root_Pin;
   Read_Retained(A,Root_Pin,Readback,Status); Check(Status=Returned and then Readback=Original);
   Read_Retained(Foreign,Root_Pin,Readback,Status); Check(Status=Unsupported_Value);
   Release(Foreign,Copy,Released_Status); Check(Released_Status=Invalid_Root and then Copy=Root_Pin);
   Retain(A,(Integer_Datum,7,AML_Constant),Second,RS); Check(RS=Retained and then Retained_Count(A)=2);
   Retain(A,Value,Copy,RS); Check(RS=Root_Limit and then Copy=No_Retained_Root);
   Copy:=Root_Pin;
   declare
      Before : constant N.State := Snapshot(A);
      Pins_Before : constant Retention_State := Retention_Model(A) with Ghost;
   begin
      Load(A,[16#08#],Bits_64,Loaded_Status);
      Check(Loaded_Status/=Loaded and then Snapshot(A)=Before and then Retained_Count(A)=2);
      pragma Assert(Retention_Model(A)=Pins_Before);
      Read_Retained(A,Copy,Readback,Status); Check(Status=Returned and then Readback=Original);
   end;
   Drop(Root_Pin);
   Release(A,Copy,Released_Status); Check(Released_Status=Invalid_Root);
   Drop(Second);
   Value.Object.ID:=0; Retain(A,Value,Root_Pin,RS); Check(RS=Invalid_Value);
   Value:=Original; Value.Object.Source:=AML_References.No_Object_Handle;
   Retain(A,Value,Root_Pin,RS); Check(RS=Invalid_Value);
   Retain(Foreign,Original,Root_Pin,RS); Check(RS=Invalid_Value);
   Make_Index(A,Source,0,Ref,Status); Check(Status=Returned); Check_Descriptor(Ref);
   -- Actual method-local named descriptor remains data after node cleanup.
   Run([16#08#,84,69,77,80,1,16#A4#,16#71#,84,69,77,80]);
   Check(Result.Status=Reference_Returned); Ref:=Result.Ref;
   Check(not Matches(A,Ref)); Check_Descriptor(Ref);
   -- A direct Return of a current-frame reference is deliberately scrubbed by
   -- Execute.Run. CopyObject into an existing named value stores descriptor data.
   Reset(A,OK); Check(OK);
   declare
      Code : constant Bytes := [16#70#,1,16#60#,16#9D#,16#71#,16#60#,
         83,73,78,75,16#A4#,0];
   begin
      Load(A,Bytes'[16#08#,83,73,78,75,0] &
        Bytes'[16#14#,Byte(6+Code'Length),84,69,83,84,0] & Code,Bits_64,Loaded_Status);
      Check(Loaded_Status=Loaded);
   end;
   Invoke(A,Input,2,[others=>(Integer_Datum,0,Ordinary_Integer)],0,1000,Result);
   Check(Result.Status=Returned);
   Make_Source(A,Data_Object(Snapshot(A),1),Source,OK); Check(OK);
   Read_Source(A,Source,Value,Status);
   Check(Status=Returned and then Value.Value_Kind=Reference_Datum
     and then AML_References.Kind(Value.Ref)=AML_References.Frame_Cell);
   Ref:=Value.Ref; Retain(A,Value,Root_Pin,RS); Check(RS=Retained);
   Invoke(A,Input,2,[others=>(Integer_Datum,0,Ordinary_Integer)],0,1000,Result);
   Check(Result.Status=Returned);
   Retain(A,(Reference_Datum,Ref),Second,RS); Check(RS=Invalid_Value);
   Read_Retained(A,Root_Pin,Readback,Status); Check(Status=Returned and then Readback.Ref=Ref); Drop(Root_Pin);
   -- Self package leaves carry NAME descriptors; construction is an owner path.
   Run([16#08#,84,69,77,80,16#12#,6,1,84,69,77,80,16#A4#,84,69,77,80]);
   Check(Result.Status=Object_Returned);
   Make_Index(A,Result.Object.Source,0,Ref,Status); Check(Status=Returned); Check_Descriptor(Ref);
   Read_Element(A,Ref,ID,OK); Check(OK);
   Make_Source(A,ID,Source,OK); Check(OK); Read_Source(A,Source,Value,Status);
   Check(Status=Returned and then Value.Value_Kind=Reference_Datum and then AML_References.Kind(Value.Ref)=AML_References.Name_Member);
   Check_Descriptor(Value.Ref);
   Retain(A,(Integer_Datum,3,Ordinary_Integer),Root_Pin,RS); Check(RS=Retained);
   Copy:=Root_Pin; Reset(A,OK); Check(OK and then Retained_Count(A)=0);
   Read_Retained(A,Copy,Readback,Status); Check(Status=Unsupported_Value);
   Release(A,Root_Pin,Released_Status); Check(Released_Status=Invalid_Root);
   declare
      package E is new AML_Namespace(1, AML_Delays.Unavailable_Provider,Max_Retained_Roots=>1,Max_Retained_Incarnation=>1);
      EA : E.Owned.Arena; P : E.Owned.Retained_Root;
      ER : E.Owned.Retain_Status; ES : E.Owned.Release_Status;
      use type E.Owned.Retain_Status; use type E.Owned.Release_Status;
   begin
      E.Owned.Reset(EA,OK); Check(OK);
      E.Owned.Retain(EA,(Integer_Datum,1,Ordinary_Integer),P,ER); Check(ER=E.Owned.Retained);
      E.Owned.Release(EA,P,ES); Check(ES=E.Owned.Released);
      E.Owned.Reset(EA,OK); Check(OK);
      E.Owned.Retain(EA,(Integer_Datum,1,Ordinary_Integer),P,ER); Check(ER=E.Owned.Identity_Exhausted);
   end;
   Ada.Text_IO.Put_Line("OWNER RETENTION" & Checks'Image);
end Owner_Retention_Tests;
