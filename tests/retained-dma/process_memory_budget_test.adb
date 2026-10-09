with Interfaces; use Interfaces;
with Process_Memory_Budget; use Process_Memory_Budget;
with Ada.Text_IO;
procedure Process_Memory_Budget_Test is
   A, B : Ledger;
   OK : Boolean;
begin
   Reserve (A, Ordinary, 4, OK); pragma Assert (OK);
   Reserve (A, DMA_Backing, 8, OK); pragma Assert (OK);
   Reserve (A, Metadata, 1, OK); pragma Assert (OK and Used (A) = 13);
   Adopt (A, 12, OK); pragma Assert (not OK and Limit (A) = 0 and Used (A) = 13);
   Adopt (A, 13, OK); pragma Assert (OK and Limit (A) = 13);
   for Kind in Charge_Kind loop
      Reserve (A, Kind, 1, OK); pragma Assert (not OK and Used (A) = 13);
   end loop;
   Release (A, Metadata, 2, OK); pragma Assert (not OK and Used (A) = 13);
   Release (A, Ordinary, 4, OK); pragma Assert (OK and Used (A) = 9);
   pragma Assert (Charged (A, DMA_Backing) = 8 and Charged (A, Metadata) = 1);
   Release (A, Ordinary, 1, OK); pragma Assert (not OK and Used (A) = 9);
   Reserve (A, DMA_Backing, 4, OK); pragma Assert (OK and Used (A) = 13);
   Reserve (B, DMA_Backing, Unsigned_64'Last, OK); pragma Assert (OK);
   Reserve (B, Ordinary, 1, OK); pragma Assert (not OK);
   Adopt (B, 1, OK); pragma Assert (not OK and Limit (B) = 0);
   Release (B, DMA_Backing, Unsigned_64'Last, OK); pragma Assert (OK and Used (B) = 0);
   Adopt (B, 1, OK); pragma Assert (OK);
   Reserve (B, Metadata, 1, OK); pragma Assert (OK);
   pragma Assert (Used (A) = 13);
   Reserve (B, Ordinary, 0, OK); pragma Assert (not OK);
   Ada.Text_IO.Put_Line ("PASS common memory budget: mixed charges, pre-resume adoption, owner isolation, exact limits, rollback and overflow");
end Process_Memory_Budget_Test;
