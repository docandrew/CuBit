with Memory_Accounting; use Memory_Accounting;
with Process_Memory_Budget; use Process_Memory_Budget;
with BuddyAllocator;
with Retained_DMA_Budget;
with Interfaces; use Interfaces;
with Ada.Text_IO;
with Ada.Exceptions;
with Charged_Metadata_Pages;
with System;
procedure Adapter_Test is
   OK, Live, Faulted : Boolean;
   Owner, Charge, Pages, Limit, Stored, Growing, Retiring : Identity;
   Held : array (1 .. 4096) of Identity;
   Held_Count : Natural := 0;
   protected Results is
      procedure Done;
      procedure Failed;
      function Count return Natural;
      function Errors return Natural;
   private
      Completed, Failures : Natural := 0;
   end Results;
   protected body Results is
      procedure Done is begin Completed := Completed + 1; end Done;
      procedure Failed is begin Failures := Failures + 1; end Failed;
      function Count return Natural is (Completed);
      function Errors return Natural is (Failures);
   end Results;
   procedure Check_Balance is
   begin
      Metadata_Status (Stored, Growing, Retiring, Limit, Faulted);
      pragma Assert (Growing = 0 and Retiring = 0 and not Faulted);
      pragma Assert (Stored = BuddyAllocator.Live_Bytes and Stored <= Limit);
   end Check_Balance;
begin
   Initialize (OK); pragma Assert (OK);
   -- Fail during each possible physical preparation step. A cached record
   -- may let a later attempt succeed with fewer pages; either result must
   -- preserve exact physical metadata accounting after completion.
   for Step in 1 .. 8 loop
      BuddyAllocator.Set_Failure (Step);
      Open (Owner, OK);
      if OK then Close (Owner, OK); pragma Assert (OK);
      else pragma Assert (Owner = 0); end if;
      Check_Balance;
   end loop;
   BuddyAllocator.Set_Failure (0);
   declare
      use type System.Address;
      Page, Denied : System.Address;
      Before : Unsigned_64;
   begin
      Open (Owner, OK); pragma Assert (OK);
      Adopt (Owner, 1, OK); pragma Assert (OK);
      Before := BuddyAllocator.Live_Bytes;
      BuddyAllocator.Set_Failure (1);
      Charged_Metadata_Pages.Allocate (Owner, Page, OK);
      pragma Assert (not OK and Page = System.Null_Address);
      pragma Assert (BuddyAllocator.Live_Bytes = Before);
      Inspect (Owner, Live, Pages, Limit, OK);
      pragma Assert (OK and Live and Pages = 0 and Limit = 1);
      BuddyAllocator.Set_Failure (0);
      BuddyAllocator.Set_Bind_Failure (True);
      Charged_Metadata_Pages.Allocate (Owner, Page, OK);
      pragma Assert (not OK and Page = System.Null_Address);
      pragma Assert (BuddyAllocator.Live_Bytes = Before);
      Inspect (Owner, Live, Pages, Limit, OK);
      pragma Assert (OK and Live and Pages = 0);
      BuddyAllocator.Set_Bind_Failure (False);
      Charged_Metadata_Pages.Allocate (Owner, Page, OK);
      pragma Assert (OK and Page /= System.Null_Address);
      Inspect (Owner, Live, Pages, Limit, OK);
      pragma Assert (OK and Live and Pages = 1);
      Charged_Metadata_Pages.Allocate (Owner, Denied, OK);
      pragma Assert (not OK and Denied = System.Null_Address);
      Close (Owner, OK); pragma Assert (OK);
      Inspect (Owner, Live, Pages, Limit, OK);
      pragma Assert (OK and not Live and Pages = 1);
      Charged_Metadata_Pages.Release (Page);
      Inspect (Owner, Live, Pages, Limit, OK); pragma Assert (not OK);
      Check_Balance;
      Ada.Text_IO.Put_Line
        ("PASS actual charged metadata adapter: allocation/bind failures refund once; closed owner charged until physical release");
   end;
   declare
      A, B, Retained_Charge, Ordinary_Charge, New_Charge : Identity;
      Global_Limit : constant Identity := Retained_DMA_Budget.Limit_Pages
        (Identity (BuddyAllocator.getTotalBytes));
   begin
      Open (A, OK); pragma Assert (OK);
      Open (B, OK); pragma Assert (OK);
      Adopt (B, 1, OK); pragma Assert (OK);
      Reserve (B, Retained_DMA_Backing, 2, New_Charge, OK);
      pragma Assert (not OK and New_Charge = 0);
      -- Failed owner admission must not consume any global capacity.
      Reserve (A, Retained_DMA_Backing, Global_Limit, Retained_Charge, OK);
      pragma Assert (OK);
      Close (A, OK); pragma Assert (OK);
      Reserve (B, Retained_DMA_Backing, 1, New_Charge, OK);
      pragma Assert (not OK); -- owner death is NOT a refund
      Reserve (B, DMA_Backing, 1, Ordinary_Charge, OK); pragma Assert (OK);
      BuddyAllocator.Complete_Physical (Ordinary_Charge, 1);
      Reserve (B, Retained_DMA_Backing, 1, New_Charge, OK);
      pragma Assert (not OK); -- ordinary free is NOT a retained refund
      BuddyAllocator.Complete_Physical (Retained_Charge, 1);
      Reserve (B, Retained_DMA_Backing, 1, New_Charge, OK); pragma Assert (OK);
      Cancel_Unbound (New_Charge, 1, OK); pragma Assert (OK);
      BuddyAllocator.Complete_Physical (Retained_Charge, Global_Limit - 1);
      Inspect (A, Live, Pages, Limit, OK); pragma Assert (not OK);
      Adopt (B, 0, OK); pragma Assert (OK);
      Reserve (B, Retained_DMA_Backing, Global_Limit, New_Charge, OK);
      pragma Assert (OK); -- exact capacity recovered after both refund paths
      Cancel_Unbound (New_Charge, Global_Limit, OK); pragma Assert (OK);
      Close (B, OK); pragma Assert (OK);
      Check_Balance;
   end;
   declare
      task type Worker;
      task body Worker is
         A, B, Tag, Tag_B, Count, Ceiling : Identity;
         Accepted, Active : Boolean;
      begin
         for Round in 1 .. 200 loop
            Open (A, Accepted); pragma Assert (Accepted);
            Reserve (A, DMA_Backing, 3, Tag, Accepted); pragma Assert (Accepted);
            Adopt (A, 2, Accepted); pragma Assert (not Accepted);
            Adopt (A, 3, Accepted); pragma Assert (Accepted);
            Close (A, Accepted); pragma Assert (Accepted);
            Open (B, Accepted); pragma Assert (Accepted and B /= A);
            Reserve (B, Ordinary, 1, Tag_B, Accepted); pragma Assert (Accepted);
            BuddyAllocator.Complete_Physical (Tag, 1);
            Inspect (A, Active, Count, Ceiling, Accepted);
            pragma Assert (Accepted and not Active and Count = 2);
            BuddyAllocator.Complete_Physical (Tag, 2);
            Inspect (A, Active, Count, Ceiling, Accepted); pragma Assert (not Accepted);
            Inspect (B, Active, Count, Ceiling, Accepted);
            pragma Assert (Accepted and Active and Count = 1);
            Cancel_Unbound (Tag_B, 1, Accepted); pragma Assert (Accepted);
            Close (B, Accepted); pragma Assert (Accepted);
         end loop;
         Results.Done;
      exception
         when E : others =>
            Results.Failed;
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      end Worker;
      Workers : array (1 .. 8) of Worker;
   begin
      null;
   end;
   pragma Assert (Results.Count = 8 and Results.Errors = 0);
   Check_Balance;
   -- Exhaust metadata through dynamic growth, not a fixed account ceiling.
   for I in Held'Range loop
      Open (Owner, OK);
      exit when not OK;
      Reserve (Owner, Metadata, 1, Held (I), OK); pragma Assert (OK);
      Close (Owner, OK); pragma Assert (OK);
      Held_Count := I;
   end loop;
   pragma Assert (Held_Count > 600 and Held_Count < Held'Last);
   Check_Balance;
   for I in 1 .. Held_Count loop
      Cancel_Unbound (Held (I), 1, OK); pragma Assert (OK);
   end loop;
   Check_Balance;
   Open (Owner, OK); pragma Assert (OK);
   Close (Owner, OK); pragma Assert (OK);
   Check_Balance;
   -- Physical callback mismatches latch a fault instead of throwing through
   -- allocator code. No new reservation or account is admitted afterwards.
   BuddyAllocator.Complete_Physical (0, 1);
   Metadata_Status (Stored, Growing, Retiring, Limit, Faulted);
   pragma Assert (Faulted and Growing = 0 and Retiring = 0);
   Open (Owner, OK); pragma Assert (not OK);
   Ada.Text_IO.Put_Line ("PASS actual accounting adapter: 8 preparation failures, 8 tasks/1600 lifecycles, quota and metadata exhaustion/recovery, no allocator-under-ledger, exact physical balance, fault latch");
end Adapter_Test;
