with Ada.Text_IO;
with AML_Identity.Issuer;
with AML_Retained_Roots;
with AML_Retained_Identities;
procedure Retained_Fill_Tests is
   package R is new AML_Retained_Roots (Integer, 0, 2, 8);
   use type R.Result_Status;
   use type R.Model;
   use type R.Token;
   use type AML_Retained_Identities.Incarnation;
   S, Foreign : R.State;
   Owner, Other : AML_Identity.Identity;
   First, Second, Stale, Foreign_Token : R.Token;
   Status : R.Result_Status;
   OK : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label_Text; end if;
   end Check;
   procedure Reject (Token : R.Token; Expected : R.Result_Status) is
      Before : constant R.Model := R.Snapshot (S) with Ghost;
      Last : constant AML_Retained_Identities.Incarnation := R.Last_Incarnation (S);
   begin
      R.Replace (S, Token, 999, Status);
      Check (Status = Expected and then R.Last_Incarnation (S) = Last, "rejected fill status and issuer");
      pragma Assert (R.Snapshot (S) = Before);
   end Reject;
begin
   AML_Identity.Issuer.Issue (Owner, OK); Check (OK, "owner");
   AML_Identity.Issuer.Issue (Other, OK); Check (OK, "other owner");
   R.Bind (S, Owner, Status); Check (Status = R.Ready, "bind");
   R.Bind (Foreign, Other, Status); Check (Status = R.Ready, "foreign bind");
   Reject (R.No_Token, R.Invalid_Root);
   R.Reserve (S, First, Status); Check (Status = R.Ready, "reserve first");
   Reject (First, R.Wrong_Phase);
   R.Publish (S, First, 11, Status); Check (Status = R.Ready, "publish placeholder");
   R.Reserve (S, Second, Status); Check (Status = R.Ready, "reserve second");
   R.Publish (S, Second, 22, Status); Check (Status = R.Ready, "publish other");
   declare
      Before : constant R.Model := R.Snapshot (S) with Ghost;
      Last : constant AML_Retained_Identities.Incarnation := R.Last_Incarnation (S);
   begin
      R.Replace (S, First, 33, Status);
      Check (Status = R.Ready and then R.Read (S, First).Value = 33
        and then R.Read (S, Second).Value = 22 and then R.Last_Incarnation (S) = Last
        and then R.Published_Count (S) = 2 and then R.Pending_Count (S) = 0,
        "fill preserves token issuer and independent slot");
      pragma Assert (R.Replaced (R.Snapshot (S), Before, First, 33));
   end;
   R.Reserve (Foreign, Foreign_Token, Status); Check (Status = R.Ready, "foreign reserve");
   R.Publish (Foreign, Foreign_Token, 66, Status); Check (Status = R.Ready, "foreign publish");
   Reject (Foreign_Token, R.Invalid_Root);
   Stale := First; R.Release (S, First, Status);
   Check (Status = R.Ready and then First = R.No_Token, "release first");
   Reject (Stale, R.Invalid_Root);
   R.Reserve (S, First, Status); Check (Status = R.Ready, "reuse pin slot");
   R.Publish (S, First, 44, Status); Check (Status = R.Ready, "replacement placeholder");
   Reject (Stale, R.Invalid_Root);
   Check (R.Read (S, First).Value = 44 and then R.Read (S, Second).Value = 22, "replay cannot replace live value");
   R.Release (S, First, Status); Check (Status = R.Ready, "drop replacement");
   declare Before : constant R.Model := R.Snapshot (S) with Ghost; begin
      R.Reserve (S, First, Status); Check (Status = R.Ready, "unused reservation");
      R.Publish (S, First, 0, Status); Check (Status = R.Ready, "unused placeholder");
      R.Release (S, First, Status); Check (Status = R.Ready, "discard placeholder");
      pragma Assert (R.Discarded_Reservation (R.Snapshot (S), Before));
      Check (R.Published_Count (S) = 1 and then R.Read (S, Second).Value = 22,
        "discard preserves existing root");
   end;
   Ada.Text_IO.Put_Line ("RETAINED RESULT FILL" & Checks'Image);
end Retained_Fill_Tests;
