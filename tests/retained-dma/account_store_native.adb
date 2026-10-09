with Memory_Account_Store;
with Process_Memory_Budget; use Process_Memory_Budget;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with TextIO;
with BuddyAllocator;
with Virtmem;
package body Account_Store_Native is
   Offset : Storage_Count := 0;
   Fail : Boolean := False;
   function Allocate (Bytes, Alignment : Storage_Count) return Address is
      Result : Address;
   begin
      if Fail then return Null_Address; end if;
      if Alignment > 4096 then return Null_Address; end if;
      BuddyAllocator.alloc (BuddyAllocator.getOrder (Bytes), Result);
      if Result = Null_Address then return Result; end if;
      Offset := Offset + Bytes;
      return Result;
   end Allocate;
   procedure Check (OK : Boolean) is
   begin
      if not OK then
         TextIO.println ("FAIL ACCOUNT-STORE");
         raise Program_Error;
      end if;
   end Check;
   procedure Release_Index_Page (Page : Address) is
   begin
      BuddyAllocator.free (0, Page);
   end Release_Index_Page;
   package S is new Memory_Account_Store (Allocate, Release_Index_Page);
   use type S.Open_Result;
   use type S.Handle;
   use type Virtmem.PhysAddress;
   Owner, Other : S.Store;
   Batch : array (1 .. 600) of S.Handle;
   Saved, Replacement, Foreign, Failed : S.Handle;
   Status : S.Open_Result;
   OK, Live : Boolean;
   Pages, Limit, Bytes : Unsigned_64;
   Refund_Target, New_Life : S.Handle;
   Charged_Frame : Virtmem.PhysAddress;
   Callbacks : Natural := 0;
   Refund_Token : Unsigned_64;
   procedure Physical_Refund (Identity, Count : Unsigned_64) is
      Accepted : Boolean;
   begin
      -- Resolve the stored physical charge identity, never today's PID.
      Check (Identity = Refund_Token and Count = 1);
      S.Refund_Physical (Owner, Identity, Count, Accepted);
      Check (Accepted);
      Refund_Target := S.Resolve (Owner, Identity / 4);
      Callbacks := Callbacks + 1;
   end Physical_Refund;
procedure Run is
begin
   BuddyAllocator.installChargeRefundHandler (Physical_Refund'Access, OK);
   Check (OK);
   S.Open (Owner, S.Block_Bytes - 1, Failed, Status);
   Check (Status = S.Metadata_Limit and Failed = S.No_Account);
   Check (Offset = 0);
   Fail := True;
   S.Open (Owner, 524288, Failed, Status);
   Check (Status = S.No_Memory and not S.Valid (Owner, Failed));
   Fail := False;
   for I in Batch'Range loop
      S.Open (Owner, 524288, Batch (I), Status);
      Check (Status = S.Opened);
      S.Reserve (Owner, Batch (I), DMA_Backing, Unsigned_64 (I), OK);
      Check (OK);
      S.Adopt (Owner, Batch (I), Unsigned_64 (I), OK);
      Check (OK);
      S.Reserve (Owner, Batch (I), Ordinary, 1, OK);
      Check (not OK);
      S.Close (Owner, Batch (I), OK);
      Check (OK and S.Valid (Owner, Batch (I)));
      S.Inspect (Owner, Batch (1), Live, Pages, Limit, OK);
      Check (OK and not Live and Pages = 1 and Limit = 1);
   end loop;
   Bytes := S.Metadata_Bytes (Owner);
   Check (Bytes > 38 * S.Block_Bytes);
   S.Open (Other, 524288, Foreign, Status);
   Check (Status = S.Opened);
   S.Reserve (Owner, Foreign, Metadata, 1, OK);
   Check (not OK and not S.Valid (Other, Batch (1)));
   Saved := Batch (1);
   S.Reserve (Owner, Saved, Ordinary, 1, OK);
   Check (not OK); -- Closed incarnation cannot acquire new charges.
   S.Refund (Owner, Batch (1), DMA_Backing, 2, OK);
   Check (not OK and S.Valid (Owner, Batch (1)));
   S.Refund (Owner, Batch (1), DMA_Backing, 1, OK);
   Check (OK and Batch (1) = S.No_Account);
   Check (not S.Valid (Owner, Saved));
   Fail := True; -- Reuse requires no backing allocation.
   S.Open (Owner, Bytes, Replacement, Status);
   Check (Status = S.Opened and S.Metadata_Bytes (Owner) = Bytes);
   S.Reserve (Owner, Replacement, Ordinary, 3, OK);
   Check (OK);
   S.Refund (Owner, Saved, Ordinary, 1, OK);
   Check (not OK); -- Stale token cannot refund a reused slot.
   S.Inspect (Owner, Replacement, Live, Pages, Limit, OK);
   Check (OK and Live and Pages = 3);
   for I in 2 .. Batch'Last loop
      S.Refund (Owner, Batch (I), DMA_Backing, Unsigned_64 (I), OK);
      Check (OK and Batch (I) = S.No_Account);
   end loop;
   S.Close (Owner, Replacement, OK);
   Check (OK);
   S.Refund (Owner, Replacement, Ordinary, 3, OK);
   Check (OK and Replacement = S.No_Account);
   S.Refund (Owner, Replacement, Ordinary, 3, OK);
   Check (not OK);
   S.Close (Other, Foreign, OK);
   Check (OK and Foreign = S.No_Account);
   -- Actual physical reclamation drives the old closed account's refund.
   -- A new life already exists when the last pin on the old page is dropped.
   for Kind in Charge_Kind loop
   Fail := False;
   S.Open (Owner, Bytes, Refund_Target, Status);
   Check (Status = S.Opened);
   S.Reserve (Owner, Refund_Target, Kind, 1, OK);
   Check (OK);
   BuddyAllocator.allocFrame (Charged_Frame);
   Check (Charged_Frame /= 0);
   BuddyAllocator.claimUserFrame (Charged_Frame, 42, OK);
   Check (OK);
   Refund_Token := S.Charge_Identity (Owner, Refund_Target, Kind);
   BuddyAllocator.bindFrameCharge (Charged_Frame, Refund_Token, OK);
   Check (OK);
   BuddyAllocator.pinOwnedFrame (Charged_Frame, 42, OK);
   Check (OK);
   S.Close (Owner, Refund_Target, OK);
   Check (OK);
   Saved := Refund_Target;
   S.Open (Owner, Bytes, New_Life, Status);
   Check (Status = S.Opened);
   S.Reserve (Owner, New_Life, Ordinary, 2, OK);
   Check (OK);
   BuddyAllocator.freeFrame (Charged_Frame);
   S.Inspect (Owner, Refund_Target, Live, Pages, Limit, OK);
   Check (OK and not Live and Pages = 1 and Callbacks = Charge_Kind'Pos (Kind));
   BuddyAllocator.unpinFrame (Charged_Frame, OK);
   Check (OK and Callbacks = Charge_Kind'Pos (Kind) + 1 and Refund_Target = S.No_Account);
   Check (S.Resolve (Owner, Refund_Token / 4) = S.No_Account);
   S.Inspect (Owner, New_Life, Live, Pages, Limit, OK);
   Check (OK and Live and Pages = 2);
   S.Refund (Owner, Saved, Ordinary, 1, OK);
   Check (not OK);
   S.Refund (Owner, New_Life, Ordinary, 2, OK);
   Check (OK);
   S.Close (Owner, New_Life, OK);
   Check (OK and New_Life = S.No_Account);
   end loop;
   TextIO.println
     ("PASS native account store: 600 retired owners, stale handles, deferred physical refund, new-life isolation");
end Run;
end Account_Store_Native;
