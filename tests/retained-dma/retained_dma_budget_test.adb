with Interfaces; use Interfaces;
with Retained_DMA_Budget; use Retained_DMA_Budget;
with Ada.Text_IO;
procedure Retained_DMA_Budget_Test is
   Charged : Unsigned_64 := 0;
   OK : Boolean;
   Limit : constant Unsigned_64 := Limit_Pages (8 * 1024 ** 3);
begin
   pragma Assert (Limit_Pages (0) = 0);
   pragma Assert (Limit_Pages (Unsigned_64'Last) = Unsigned_64'Last / 2 / 4096);
   pragma Assert (Limit_Pages (1024 ** 4) = 1024 ** 4 / 2 / 4096);
   pragma Assert (Limit_Pages (8191) = 0 and Limit_Pages (8192) = 1);
   pragma Assert (Limit_Pages (8193) = 1);
   pragma Assert (Limit_Pages (8 * 1024 ** 3) * Page_Bytes = 4 * 1024 ** 3);
   for I in 1 .. 128 loop
      Reserve (Limit, Charged, 512, OK);
      pragma Assert (OK and Charged = Unsigned_64 (I) * 512);
   end loop;
   pragma Assert (Charged * 4096 = 256 * 1024 ** 2);
   Cancel_Unpublished (Charged, 512);
   pragma Assert (Charged = 127 * 512);
   Reserve (Limit, Charged, Limit - Charged, OK);
   pragma Assert (OK and Charged = Limit);
   Reserve (Limit, Charged, 1, OK);
   pragma Assert (not OK and Charged = Limit);
   Reserve (Limit, Charged, 0, OK);
   pragma Assert (not OK and Charged = Limit);
   Charged := Unsigned_64'Last - 1;
   Reserve (Unsigned_64'Last, Charged, 2, OK);
   pragma Assert (not OK and Charged = Unsigned_64'Last - 1);
   Reserve (Unsigned_64'Last, Charged, 1, OK);
   pragma Assert (OK and Charged = Unsigned_64'Last);
   Ada.Text_IO.Put_Line ("PASS retained DMA budget: >64MiB, 1TiB policy, reservation rollback, exact exhaustion, zero and overflow rejection");
end Retained_DMA_Budget_Test;
