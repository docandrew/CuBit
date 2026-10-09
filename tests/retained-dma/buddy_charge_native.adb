with BuddyAllocator; use BuddyAllocator;
with Interfaces; use Interfaces;
with System;
with TextIO;
with Virtmem;

package body Buddy_Charge_Native is
   use type System.Address;
   use type Virtmem.PhysAddress;
   Calls, Refunded, Expected : Unsigned_64 := 0;
   -- Private runner may lower the metadata budget and enable this branch.
   Expect_Metadata_Denial : constant Boolean := False;
   -- More than one radix leaf's worth of distinct live frames. Static test
   -- storage avoids consuming the early kernel stack with the address array.
   Batch : array (Positive range 1 .. 600) of Virtmem.PhysAddress;

   procedure Check (OK : Boolean) is
   begin
      if not OK then
         TextIO.println ("FAIL BUDDY-CHARGE");
         raise Program_Error;
      end if;
   end Check;

   procedure Refund (Identity, Pages : Unsigned_64) is
      Probe : System.Address;
   begin
      Check (Identity = Expected and Pages > 0);
      Calls := Calls + 1;
      Refunded := Refunded + Pages;
      -- Deadlocks if callback runs inside buddy lock. This allocation has no
      -- charge; freeing it must not recursively refund the retired identity.
      alloc (0, Probe);
      Check (Probe /= NO_BLOCK_AVAILABLE);
      free (0, Probe);
   end Refund;

   procedure Run is
      Frame : Virtmem.PhysAddress;
      Block : System.Address;
      OK : Boolean;
      Metadata_Pages, Metadata_Limit : Unsigned_64;
   begin
      TextIO.println ("BEGIN BUDDY-CHARGE");
      installChargeRefundHandler (Refund'Access, OK);
      Check (OK);
      installChargeRefundHandler (Refund'Access, OK);
      Check (not OK);
      allocFrame (Frame);
      Check (Frame /= 0);
      bindFrameCharge (Frame, 17, OK);
      Check (not OK); -- Not yet access-owned.
      chargeMetadataUsage (Metadata_Pages, Metadata_Limit);
      Check (Metadata_Pages = 0);
      claimUserFrame (Frame, 42, OK);
      Check (OK);
      if Expect_Metadata_Denial then
         -- A three-page cap cannot complete the four-node initial path.
         for Attempt in 1 .. 3 loop
            bindFrameCharge (Frame, 17, OK);
            Check (not OK);
            chargeMetadataUsage (Metadata_Pages, Metadata_Limit);
            Check (Metadata_Pages = 3 and Metadata_Limit = 3);
         end loop;
         pinOwnedFrame (Frame, 42, OK);
         Check (OK); -- Failed bind never changed access ownership.
         freeFrame (Frame);
         unpinFrame (Frame, OK);
         Check (OK and Calls = 0 and Refunded = 0);
         TextIO.println ("PASS BUDDY-CHARGE: bounded metadata denial, retry, no phantom refund");
         return;
      end if;
      bindFrameCharge (Frame, 0, OK);
      Check (not OK);
      bindFrameCharge (Frame + 1, 17, OK);
      Check (not OK);
      bindFrameCharge (Frame, 17, OK);
      Check (OK);
      bindFrameCharge (Frame, 18, OK);
      Check (not OK); -- Never overwrite an existing charge.
      pinOwnedFrame (Frame, 42, OK);
      Check (OK);
      freeFrame (Frame); -- Clears access owner, retains original charge.
      Check (Calls = 0 and Refunded = 0);
      Expected := 17;
      unpinFrame (Frame, OK);
      Check (OK and Calls = 1 and Refunded = 1);
      unpinFrame (Frame, OK);
      Check (not OK and Calls = 1 and Refunded = 1);

      -- Repeated bind/free exercises slot reuse with distinct incarnations.
      for Iteration in 1 .. 64 loop
         allocFrame (Frame);
         Check (Frame /= 0);
         claimUserFrame (Frame, 42, OK);
         Check (OK);
         Expected := Unsigned_64 (100 + Iteration);
         bindFrameCharge (Frame, Expected, OK);
         Check (OK);
         freeFrame (Frame);
         Check (Calls = Unsigned_64 (1 + Iteration));
      end loop;
      Check (Refunded = 65);

      alloc (3, Block);
      Check (Block /= NO_BLOCK_AVAILABLE);
      Expected := 999;
      for I in 0 .. 7 loop
         Frame := Virtmem.V2P (Block) + Virtmem.PhysAddress (I * 4096);
         claimUserFrame (Frame, 42, OK);
         Check (OK);
         bindFrameCharge (Frame, Expected, OK);
         Check (OK);
         releaseUserFrame (Frame, 42);
      end loop;
      free (3, Block);
      Check (Calls = 66 and Refunded = 73);
      Expected := 1001;
      for I in Batch'Range loop
         allocFrame (Batch (I));
         Check (Batch (I) /= 0);
         claimUserFrame (Batch (I), 42, OK);
         Check (OK);
         bindFrameCharge (Batch (I), Expected, OK);
         Check (OK);
         if I = Batch'First then
            pinOwnedFrame (Batch (I), 42, OK);
            Check (OK);
         end if;
      end loop;
      -- The first charge must survive both metadata growth and deferred free.
      for Address of Batch loop
         freeFrame (Address);
      end loop;
      Check (Calls = 665 and Refunded = 672);
      unpinFrame (Batch (Batch'First), OK);
      Check (OK and Calls = 666 and Refunded = 673);
      chargeMetadataUsage (Metadata_Pages, Metadata_Limit);
      Check (Metadata_Pages >= 5 and Metadata_Pages <= Metadata_Limit);
      TextIO.println ("PASS BUDDY-CHARGE: deferred pin, identity, reuse, block, growth, post-unlock callback");
   end Run;
end Buddy_Charge_Native;
