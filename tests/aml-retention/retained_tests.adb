with AML_Object_Identifiers;
with Ada.Text_IO;
with AML_Identity.Issuer;
with AML_Retained_Roots;
with AML_Retained_Identities;
with AML_Execute;
with AML_Decode;
with AML_References;
with AML_Frame_Handles;
with AML_Index_Handles;
procedure Retained_Tests is
   use type AML_Retained_Identities.Incarnation;
   use type AML_Execute.Datum;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks+1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   A, B : AML_Identity.Identity;
   Issued : Boolean;
   type Scalar is record
      Number : Integer := 0;
      Flag : Boolean := False;
   end record;
   package R is new AML_Retained_Roots (Scalar, (others => <>), 2, 3);
   use R;
   S, Other : R.State;
   T, Copy, U, Extra : R.Token;
   Status : R.Result_Status;
   V : constant Scalar := (73, True);
   procedure Expect (Expected : R.Result_Status) is
   begin Check (Status = Expected and then Valid (S)); end Expect;
   package D is new AML_Retained_Roots
     (AML_Execute.Datum, (AML_Execute.Integer_Datum, 0, AML_Decode.Ordinary_Integer), 2);
   DS : D.State;
   Pending_Old, Published_Old : D.Token;
   DT : D.Token;
   DR : D.Result_Status;
   use type D.Result_Status;
   procedure Roundtrip (Value : AML_Execute.Datum) is
      Saved : D.Token;
      use type D.Token;
   begin
      D.Reserve (DS, DT, DR); Check (DR = D.Ready);
      Check (D.Read (DS, DT).Status = D.Wrong_Phase and then D.Published_Count (DS) = 0);
      D.Publish (DS, DT, Value, DR); Check (DR = D.Ready);
      Check (D.Read (DS, DT).Value = Value and then D.Published_At (DS, 1).Value = Value);
      Saved := DT;
      D.Release (DS, DT, DR); Check (DR = D.Ready and then DT = D.No_Token);
      D.Release (DS, Saved, DR); Check (DR = D.Invalid_Root);
   end Roundtrip;
begin
   AML_Identity.Issuer.Issue (A, Issued); Check (Issued);
   AML_Identity.Issuer.Issue (B, Issued); Check (Issued);
   Reserve (S, T, Status); Expect (Invalid_Owner); Check (T = No_Token);
   Bind (S, AML_Identity.No_Identity, Status); Expect (Invalid_Owner);
   Reset (S, A, Status); Expect (Invalid_Owner);
   Bind (S, A, Status); Expect (Ready);
   Bind (S, B, Status); Expect (Invalid_Owner);
   Reset (S, A, Status); Expect (Invalid_Owner);
   Bind (Other, B, Status); Check (Status=Ready);
   Reserve (S, T, Status); Expect (Ready); Copy := T;
   Check (Pending_Count (S)=1 and then Published_Count (S)=0 and then Last_Incarnation (S)=1);
   Check (Read (S,T).Status=Wrong_Phase and then Published_At(S,1).Status=Wrong_Phase);
   Release (S,T,Status); Expect (Wrong_Phase); Check (T=Copy);
   Publish (Other,T,V,Status); Check (Status=Invalid_Root);
   Publish (S,T,V,Status); Expect (Ready);
   Check (Read(S,T).Value=V and then Pending_Count(S)=0 and then Published_Count(S)=1);
   Publish (S,T,(9,False),Status); Expect (Wrong_Phase); Check (Read(S,T).Value=V);
   Cancel (S,T,Status); Expect (Wrong_Phase); Check (T=Copy);
   Reserve (S,U,Status); Expect (Ready);
   Reserve (S,Extra,Status); Expect (Root_Limit); Check (Extra=No_Token);
   Cancel (S,U,Status); Expect (Ready); Check (U=No_Token and then Read(S,T).Value=V);
   Release (S,T,Status); Expect (Ready); Check (T=No_Token);
   Reserve (S,T,Status); Expect (Ready); Check (T/=Copy and then Last_Incarnation(S)=3);
   Release (S,Copy,Status); Expect (Invalid_Root);
   Publish (S,T,V,Status); Expect (Ready);
   Reset (S,B,Status); Expect (Ready); Check (Last_Incarnation(S)=3 and then Published_Count(S)=0);
   Check (Read(S,T).Status=Invalid_Root);
   Reset (S,A,Status); Expect (Ready); Check (Read(S,T).Status=Invalid_Root);
   Reserve (S,U,Status); Expect (Identity_Exhausted); Check (U=No_Token);
   D.Bind (DS,A,DR); Check (DR=D.Ready);
   Roundtrip ((AML_Execute.Integer_Datum,16#FEDC_BA98_7654_3210#,AML_Decode.AML_Constant));
   Roundtrip ((Value_Kind=>AML_Execute.Object_Datum,Object=>(others=><>)));
   Roundtrip ((AML_Execute.Reference_Datum,AML_References.No_Reference));
   Roundtrip ((AML_Execute.Reference_Datum,AML_References.Bind_Named(A,1,42)));
   Roundtrip ((AML_Execute.Reference_Datum,AML_References.Bind_Name_Member(A,1,42)));
   Roundtrip ((AML_Execute.Reference_Datum,AML_References.Bind(A,AML_Index_Handles.Bind_Byte(AML_Object_Identifiers.Make_Address(1,AML_Object_Identifiers.First_Incarnation),9))));
   Roundtrip ((AML_Execute.Reference_Datum,AML_References.Bind(A,AML_Index_Handles.Bind_Package(AML_Object_Identifiers.Make_Address(2,AML_Object_Identifiers.First_Incarnation),3))));
   Roundtrip ((AML_Execute.Reference_Datum,AML_References.Bind_Frame
     (AML_Frame_Handles.Bind_Cell(AML_Frame_Handles.Bind_Frame
       (AML_Frame_Handles.Bind_Domain(A,1),1,1),AML_Frame_Handles.Arg_0))));
   D.Reserve (DS, Pending_Old, DR); Check (DR=D.Ready);
   D.Reserve (DS, Published_Old, DR); Check (DR=D.Ready);
   D.Publish (DS, Published_Old,
     (AML_Execute.Integer_Datum, 99, AML_Decode.Ordinary_Integer), DR);
   Check (DR=D.Ready and then D.Pending_Count(DS)=1 and then D.Published_Count(DS)=1);
   D.Reset (DS,B,DR); Check (DR=D.Ready);
   Check (D.Read(DS,Pending_Old).Status=D.Invalid_Root
     and then D.Read(DS,Published_Old).Status=D.Invalid_Root);
   Check (D.Pending_Count(DS)=0 and then D.Published_Count(DS)=0);
   D.Cancel (DS,Pending_Old,DR); Check (DR=D.Invalid_Root);
   D.Release (DS,Published_Old,DR); Check (DR=D.Invalid_Root);
   Ada.Text_IO.Put_Line ("RETAINED" & Checks'Image);
end Retained_Tests;
