with Ada.Text_IO;
with Interfaces; use Interfaces;
with Process_Memory_Accounts; use Process_Memory_Accounts;
with Process_Memory_Budget;

procedure Process_Memory_Accounts_Test is
   use Process_Memory_Budget;
   Old_Life, New_Life : Account;
   OK : Boolean;
begin
   Open (Old_Life, 0, OK); pragma Assert (not OK);
   Open (Old_Life, 41, OK); pragma Assert (OK);
   Reserve (Old_Life, 41, DMA_Backing, 8, OK); pragma Assert (OK);
   Reserve (Old_Life, 41, Ordinary, 2, OK); pragma Assert (OK);
   Adopt (Old_Life, 41, 9, OK); pragma Assert (not OK and Limit (Old_Life) = 0);
   Adopt (Old_Life, 41, 10, OK); pragma Assert (OK);
   Close (Old_Life, 40, OK); pragma Assert (not OK and Active (Old_Life));
   Close (Old_Life, 41, OK); pragma Assert (OK and Used (Old_Life) = 10);
   Reserve (Old_Life, 41, Metadata, 1, OK); pragma Assert (not OK);
   Adopt (Old_Life, 41, 0, OK); pragma Assert (not OK);
   Open (Old_Life, 42, OK); pragma Assert (not OK);

   -- A new process incarnation can exist while the old account is retained.
   Open (New_Life, 42, OK); pragma Assert (OK);
   Reserve (New_Life, 42, Ordinary, 3, OK); pragma Assert (OK);
   Refund (New_Life, 41, Ordinary, 2, OK); pragma Assert (not OK);
   Refund (Old_Life, 41, Ordinary, 2, OK); pragma Assert (OK);
   pragma Assert (Used (New_Life) = 3 and Used (Old_Life) = 8);
   Refund (Old_Life, 41, Ordinary, 2, OK); pragma Assert (not OK);
   Refund (Old_Life, 41, DMA_Backing, 8, OK); pragma Assert (OK);
   pragma Assert (Reusable (Old_Life));
   Open (Old_Life, 41, OK); pragma Assert (not OK);
   Open (Old_Life, 43, OK); pragma Assert (OK);
   Reserve (Old_Life, 43, Metadata, 1, OK); pragma Assert (OK);
   Refund (Old_Life, 41, Metadata, 1, OK); pragma Assert (not OK);
   pragma Assert (Used (Old_Life) = 1 and Limit (Old_Life) = 0);
   Refund (Old_Life, 43, Metadata, 1, OK); pragma Assert (OK);
   Close (Old_Life, 43, OK); pragma Assert (OK);
   Open (Old_Life, Unsigned_64'Last, OK); pragma Assert (OK);
   Close (Old_Life, Unsigned_64'Last, OK); pragma Assert (OK);
   Open (Old_Life, 1, OK); pragma Assert (not OK); -- Never wrap an identity.
   Ada.Text_IO.Put_Line
     ("PASS memory accounts: closed-life refunds, independent new life, stale identity rejection, no early reuse or wrap");
end Process_Memory_Accounts_Test;
