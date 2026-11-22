with BuddyAllocator;
with Process_Memory_Budget;
with Virtmem;
package body Charged_Metadata_Pages is
   use type Virtmem.PhysAddress;
   use type System.Address;
   procedure Allocate
     (Owner : Memory_Accounting.Identity; Page : out System.Address;
      OK : out Boolean)
   is
      Charge : Memory_Accounting.Identity;
      Frame : Virtmem.PhysAddress;
      Cancelled : Boolean;
   begin
      Page := System.Null_Address;
      Memory_Accounting.Reserve
        (Owner, Process_Memory_Budget.Metadata, 1, Charge, OK);
      if not OK then return; end if;
      BuddyAllocator.allocFrame (Frame);
      if Frame /= 0 then
         BuddyAllocator.bindKernelFrameCharge (Frame, Charge, OK);
         if OK then
            -- Sticky handoff: only actual allocator reclamation may refund
            -- after this point. Never also cancel this reservation.
            Page := Virtmem.P2Va (Frame);
            return;
         end if;
         BuddyAllocator.freeFrame (Frame);
      end if;
      Memory_Accounting.Cancel_Unbound (Charge, 1, Cancelled);
      if not Cancelled then
         raise Program_Error with "Kernel metadata charge rollback failed";
      end if;
      OK := False;
   end Allocate;

   procedure Release (Page : System.Address) is
   begin
      if Page = System.Null_Address then
         raise Program_Error with "Null kernel metadata release";
      end if;
      -- No current PID/account lookup: Buddy keeps the original charge owner.
      BuddyAllocator.free (0, Page);
   end Release;
end Charged_Metadata_Pages;
