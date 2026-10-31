pragma Ada_2022;
with Ada.Text_IO;
with Ada.Exceptions;
with ACPI_Module_Diagnostics;
with ACPI_Native_Module_Notes;
with AML_Identity;
with AML_Identity.Issuer;
with ACPI_Service;
with Owner_Test_Control;
procedure Notes_Tests is
   package D renames ACPI_Module_Diagnostics;
   package T renames Owner_Test_Control;
   Item : D.Observation;
   Token : AML_Identity.Identity;
   Success : Boolean;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with "module diagnostic note disagreement"; end if; end Check;
   procedure Expect (Count : Natural) is
   begin
      Check (T.Log_Count = Count and then T.Recovery_Attempts = Count);
      if Count /= 0 then
         Check (T.Log (Count).Text (1 .. T.Log (Count).Length) = D.Format (Item));
         Check (T.Log (Count).Length <= D.Maximum_Text_Length);
      end if;
   end Expect;
begin
   ACPI_Native_Module_Notes.Observe (Item); Expect (0);
   AML_Identity.Issuer.Issue (Token, Success); Check (Success);
   Item.Definition := ACPI_Service.Namespace.Definition_Identity (Token);
   Item.Count := 1;
   ACPI_Native_Module_Notes.Observe (Item); Expect (1);
   ACPI_Native_Module_Notes.Observe (Item); Expect (1);
   Item.Count := 2;
   ACPI_Native_Module_Notes.Observe (Item); Expect (2);
   Item.Count := D.Count'Last; Item.Resume_Offset := Natural'Last; Item.Charged := Natural'Last;
   ACPI_Native_Module_Notes.Observe (Item); Expect (3);
   ACPI_Native_Module_Notes.Observe (Item); Expect (3);
   Item.Resume_Offset := Item.Resume_Offset - 1;
   ACPI_Native_Module_Notes.Observe (Item); Expect (4);
   declare Empty : D.Observation; begin ACPI_Native_Module_Notes.Observe (Empty); end;
   ACPI_Native_Module_Notes.Observe (Item); Expect (4);
   T.Selected := T.Recovery_Log_Failure;
   Item.Resume_Offset := Item.Resume_Offset - 1;
   declare Raised : Boolean := False; begin
      begin
         ACPI_Native_Module_Notes.Observe (Item);
      exception
         when E : Program_Error =>
            Raised := True;
            Check (Ada.Exceptions.Exception_Message (E) = "modeled recovery logger failure");
      end;
      Check (Raised); -- Hosted boundary only; native runtime terminates here.
   end;
   Expect (5);
   ACPI_Native_Module_Notes.Observe (Item); Expect (5);
   Ada.Text_IO.Put_Line ("Module note checks" & Checks'Image);
end Notes_Tests;
