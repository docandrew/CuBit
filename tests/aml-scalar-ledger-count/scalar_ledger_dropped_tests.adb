with Ada.Text_IO;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Frame_Handles;
with AML_Scalar_Ledger;
with AML_Scalar_Ledger.Dropped_Equivalence;
procedure Scalar_Ledger_Dropped_Tests is
   package Ledger is new AML_Scalar_Ledger (2, 2, 2);
   package Test is new Ledger.Dropped_Equivalence;
   Owner : AML_Identity.Identity;
   Issued : Boolean;
   Checks : Natural;
begin
   AML_Identity.Issuer.Issue (Owner, Issued);
   if not Issued then raise Program_Error; end if;
   Test.Run (AML_Frame_Handles.Bind_Domain (Owner, 1),
             AML_Frame_Handles.Bind_Domain (Owner, 2), Checks);
   if Checks /= 52_501 then raise Program_Error with "enumeration incomplete"; end if;
   Ada.Text_IO.Put_Line ("PASS" & Checks'Image & " dropped checker/model comparisons");
end Scalar_Ledger_Dropped_Tests;
