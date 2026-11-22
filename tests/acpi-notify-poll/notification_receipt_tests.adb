pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use type Interfaces.Unsigned_64;
with ACPI_Notification_Receipts;
procedure Notification_Receipt_Tests is
   package P is new ACPI_Notification_Receipts (2);
   package Z is new ACPI_Notification_Receipts (0);
   use type P.State;
   use type P.Ack_Result;
   S : P.State;
   Empty : Z.State;
   Result : P.Ack_Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Unchanged_Ack (ID : P.Number; Expected : P.Ack_Result) is
      Before : constant P.State := S;
   begin
      P.Acknowledge (S, ID, Result);
      Check (Result = Expected and then S = Before and then P.Valid (S));
   end Unchanged_Ack;
begin
   Check (Z.Valid (Empty) and then not Z.Can_Issue (Empty));
   Check (P.Valid (S) and then P.Current (S) = 0 and then P.Can_Issue (S));
   Unchanged_Ack (0, P.Invalid);
   Unchanged_Ack (1, P.Invalid);
   P.Issue (S);
   Check (P.Current (S) = 1 and then P.Issued_Count (S) = 1 and then not P.Can_Issue (S));
   Unchanged_Ack (0, P.Invalid);
   Unchanged_Ack (2, P.Invalid);
   P.Acknowledge (S, 1, Result);
   Check (Result = P.Accepted and then P.Current (S) = 0 and then P.Can_Issue (S));
   Unchanged_Ack (1, P.Repeated);
   P.Issue (S);
   Check (P.Current (S) = 2 and then P.Last_Acknowledged (S) = 1);
   -- A delayed retry of ACK1 must preserve the newer staged receipt2.
   Unchanged_Ack (1, P.Repeated);
   Check (P.Current (S) = 2);
   P.Acknowledge (S, 2, Result);
   Check (Result = P.Accepted and then P.Current (S) = 0 and then not P.Can_Issue (S));
   Check (P.Issued_Count (S) = 2);
   Unchanged_Ack (0, P.Invalid);
   Unchanged_Ack (1, P.Invalid);
   Unchanged_Ack (2, P.Repeated);
   Ada.Text_IO.Put_Line ("Notification receipt checks" & Checks'Image);
end Notification_Receipt_Tests;
