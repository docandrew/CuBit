with Memory_Accounting; use Memory_Accounting;
with Process_Memory_Budget; use Process_Memory_Budget;
with BuddyAllocator;
with Virtmem;
with Interfaces; use Interfaces;
with TextIO;
with System.Storage_Elements; use System.Storage_Elements;
package body Accounting_Adapter_Native is
   Owners, Charges : array (1 .. 600) of Identity;
   procedure Check (OK : Boolean) is
   begin
      if not OK then
         TextIO.println ("FAIL ACCOUNTING-ADAPTER");
         raise Program_Error;
      end if;
   end Check;
   procedure Run is
      OK, Live, Faulted : Boolean;
      Owner, New_Owner, Charge, New_Charge, Pages, Limit : Identity;
      Stored, Growing, Retiring : Identity;
      Before : Storage_Count;
      Frame : Virtmem.PhysAddress;
      use type Virtmem.PhysAddress;
   begin
      TextIO.println ("BEGIN ACCOUNTING-ADAPTER");
      Initialize (OK); Check (OK);
      Initialize (OK); Check (not OK);
      Before := BuddyAllocator.getFreeBytes;
      for I in Owners'Range loop
         Open (Owners (I), OK); Check (OK);
         Reserve (Owners (I), Ordinary, 1, Charges (I), OK); Check (OK);
         Adopt (Owners (I), 1, OK); Check (OK);
         Reserve (Owners (I), DMA_Backing, 1, Charge, OK); Check (not OK);
         Close (Owners (I), OK); Check (OK);
      end loop;
      for I in Owners'Range loop
         Cancel_Unbound (Charges (I), 1, OK); Check (OK);
         Inspect (Owners (I), Live, Pages, Limit, OK); Check (not OK);
      end loop;
      Metadata_Status (Stored, Growing, Retiring, Limit, Faulted);
      Check (not Faulted and Growing = 0 and Retiring = 0);
      Check (Stored = Identity (Before - BuddyAllocator.getFreeBytes));
      for Kind in Charge_Kind loop
         Open (Owner, OK); Check (OK);
         Reserve (Owner, Kind, 1, Charge, OK); Check (OK);
         BuddyAllocator.allocFrame (Frame); Check (Frame /= 0);
         BuddyAllocator.claimUserFrame (Frame, 42, OK); Check (OK);
         BuddyAllocator.bindFrameCharge (Frame, Charge, OK); Check (OK);
         BuddyAllocator.pinOwnedFrame (Frame, 42, OK); Check (OK);
         Close (Owner, OK); Check (OK);
         Open (New_Owner, OK); Check (OK and Owner /= New_Owner);
         Reserve (New_Owner, Ordinary, 2, New_Charge, OK); Check (OK);
         BuddyAllocator.freeFrame (Frame);
         Inspect (Owner, Live, Pages, Limit, OK);
         Check (OK and not Live and Pages = 1);
         BuddyAllocator.unpinFrame (Frame, OK); Check (OK);
         Inspect (Owner, Live, Pages, Limit, OK); Check (not OK);
         Inspect (New_Owner, Live, Pages, Limit, OK);
         Check (OK and Live and Pages = 2);
         Cancel_Unbound (New_Charge, 2, OK); Check (OK);
         Close (New_Owner, OK); Check (OK);
         Metadata_Status (Stored, Growing, Retiring, Limit, Faulted);
         Check (not Faulted and Growing = 0 and Retiring = 0);
      end loop;
      TextIO.println ("PASS ACCOUNTING-ADAPTER: 600 closed owners, physical metadata balance, all-kind last-pin refund, new-life isolation");
   end Run;
end Accounting_Adapter_Native;
