pragma Ada_2022;
with BuddyAllocator;
with Retained_DMA_Budget;
with Memory_Account_Store;
with Spinlocks;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
package body Memory_Accounting is
   use Interfaces;
   Ledger_Lock : Spinlocks.Spinlock;
   Page_Bytes : constant Identity := 4096;
   Growth_Pages : constant := 8; -- One record page + seven index levels.
   Growth_Bytes : constant Identity := Growth_Pages * Page_Bytes;
   -- Bounded per-operation scratch, not account/allocation capacity.
   type Page_List is array (Positive range 1 .. 15) of Address;
   Prepared, Deferred : Page_List := [others => Null_Address];
   Prepared_Count, Deferred_Count : Natural := 0;
   In_Flight, Retiring_Bytes, Metadata_Limit : Identity := 0;
   Ready, Broken : Boolean := False;
   Retained_Pages, Retained_Limit : Identity := 0;
   Retained_Tag : constant Identity := Identity
     (Process_Memory_Budget.Charge_Kind'Pos (Process_Memory_Budget.Retained_DMA_Backing));
   use type Process_Memory_Budget.Charge_Kind;

   function Take_Metadata (Bytes, Alignment : Storage_Count) return Address is
      Result : Address;
   begin
      if Bytes > 4096 or else Alignment > 4096 or else Prepared_Count = 0 then
         return Null_Address;
      end if;
      Result := Prepared (Prepared_Count);
      Prepared (Prepared_Count) := Null_Address;
      Prepared_Count := Prepared_Count - 1;
      return Result;
   end Take_Metadata;

   procedure Defer_Metadata (Page : Address) is
   begin
      Retiring_Bytes := Retiring_Bytes + Page_Bytes;
      if Deferred_Count = Deferred'Last then
         -- Structural invariant violation: retain the unqueued page/charge
         -- forever and stop new admission rather than free under the lock.
         Broken := True;
         return;
      end if;
      Deferred_Count := Deferred_Count + 1;
      Deferred (Deferred_Count) := Page;
   end Defer_Metadata;
   package Accounts is new Memory_Account_Store (Take_Metadata, Defer_Metadata);
   use type Accounts.Open_Result;
   Store : Accounts.Store;

   -- Called under ledger lock. Capture scratch before another CPU may enter.
   procedure Collect
     (Pages : out Page_List; Count, Retired : out Natural) is
   begin
      Pages := [others => Null_Address];
      Count := Deferred_Count;
      Retired := Deferred_Count;
      for I in 1 .. Deferred_Count loop Pages (I) := Deferred (I); end loop;
      for I in 1 .. Prepared_Count loop
         Count := Count + 1;
         Pages (Count) := Prepared (I);
      end loop;
      Deferred_Count := 0;
      Prepared_Count := 0;
   end Collect;

   -- No ledger lock on entry, and all pages are private UNCHARGED metadata.
   procedure Flush
     (Pages : Page_List; Count, Retired : Natural; Credit : Identity := 0) is
   begin
      for I in 1 .. Count loop BuddyAllocator.free (0, Pages (I)); end loop;
      Spinlocks.enterCriticalSection (Ledger_Lock);
      Retiring_Bytes := Retiring_Bytes - Identity (Retired) * Page_Bytes;
      In_Flight := In_Flight - Credit;
      Spinlocks.exitCriticalSection (Ledger_Lock);
   end Flush;

   procedure Apply_Refund
     (Charge, Pages : Identity; Physical : Boolean; OK : out Boolean) is
      Free_Pages : Page_List;
      Count, Retired : Natural;
   begin
      Spinlocks.enterCriticalSection (Ledger_Lock);
      OK := Ready;
      if Charge mod 4 = Retained_Tag and then Pages > Retained_Pages then
         OK := False;
      end if;
      if OK then Accounts.Refund_Physical (Store, Charge, Pages, OK); end if;
      -- Same transaction as the original-owner refund. Ordinary DMA must
      -- never refund this global budget, even if it has the same owner.
      if OK and then Charge mod 4 = Retained_Tag then
         Retained_Pages := Retained_Pages - Pages;
      end if;
      if Physical and not OK then Broken := True; end if;
      Collect (Free_Pages, Count, Retired);
      Spinlocks.exitCriticalSection (Ledger_Lock);
      Flush (Free_Pages, Count, Retired);
   end Apply_Refund;
   procedure Physical_Refund (Charge, Pages : Identity) is
      OK : Boolean;
   begin
      Apply_Refund (Charge, Pages, True, OK);
   end Physical_Refund;

   procedure Initialize (OK : out Boolean) is
   begin
      OK := False;
      if Ready or else Accounts.Block_Bytes > Page_Bytes then return; end if;
      Metadata_Limit := Identity (BuddyAllocator.getTotalBytes) / 1024;
      Retained_Limit := Retained_DMA_Budget.Limit_Pages
        (Identity (BuddyAllocator.getTotalBytes));
      BuddyAllocator.installChargeRefundHandler (Physical_Refund'Access, OK);
      Ready := OK;
   end Initialize;

   function Can_Grow return Boolean is
      Remaining : Identity := Metadata_Limit;
      Used : constant Identity := Accounts.Metadata_Bytes (Store);
   begin
      if Used > Remaining then return False; end if;
      Remaining := Remaining - Used;
      if In_Flight > Remaining then return False; end if;
      Remaining := Remaining - In_Flight;
      if Retiring_Bytes > Remaining then return False; end if;
      return Growth_Bytes <= Remaining - Retiring_Bytes;
   end Can_Grow;

   procedure Open (Owner : out Identity; OK : out Boolean) is
      Fresh, Free_Pages : Page_List := [others => Null_Address];
      Fresh_Count, Count, Retired : Natural := 0;
      Handle : Accounts.Handle;
      Status : Accounts.Open_Result;
      Available : Identity;
   begin
      Owner := 0;
      Spinlocks.enterCriticalSection (Ledger_Lock);
      OK := Ready and then not Broken and then Can_Grow;
      if OK then In_Flight := In_Flight + Growth_Bytes; end if;
      Spinlocks.exitCriticalSection (Ledger_Lock);
      if not OK then return; end if;
      for I in 1 .. Growth_Pages loop
         BuddyAllocator.alloc (0, Fresh (I));
         exit when Fresh (I) = Null_Address;
         Fresh_Count := Fresh_Count + 1;
      end loop;
      Spinlocks.enterCriticalSection (Ledger_Lock);
      Prepared := Fresh;
      Prepared_Count := Fresh_Count;
      Available := Metadata_Limit - (In_Flight - Growth_Bytes);
      if Retiring_Bytes > Available or else Broken then
         OK := False;
      else
         Accounts.Open (Store, Available - Retiring_Bytes, Handle, Status);
         OK := Status = Accounts.Opened;
         if OK then Owner := Accounts.Identity (Store, Handle); end if;
      end if;
      Collect (Free_Pages, Count, Retired);
      Spinlocks.exitCriticalSection (Ledger_Lock);
      -- In-flight credit remains until unused pages actually return to buddy.
      -- Published pages temporarily count twice, conservatively denying growth.
      Flush (Free_Pages, Count, Retired, Growth_Bytes);
   end Open;

   procedure Close (Owner : Identity; OK : out Boolean) is
      Handle : Accounts.Handle;
      Free_Pages : Page_List;
      Count, Retired : Natural;
   begin
      Spinlocks.enterCriticalSection (Ledger_Lock);
      Handle := Accounts.Resolve (Store, Owner);
      Accounts.Close (Store, Handle, OK);
      Collect (Free_Pages, Count, Retired);
      Spinlocks.exitCriticalSection (Ledger_Lock);
      Flush (Free_Pages, Count, Retired);
   end Close;

   procedure Adopt (Owner, Limit_Pages : Identity; OK : out Boolean) is
   begin
      Spinlocks.enterCriticalSection (Ledger_Lock);
      Accounts.Adopt (Store, Accounts.Resolve (Store, Owner), Limit_Pages, OK);
      Spinlocks.exitCriticalSection (Ledger_Lock);
   end Adopt;

   procedure Reserve
     (Owner : Identity; Kind : Process_Memory_Budget.Charge_Kind;
      Pages : Identity; Charge : out Identity; OK : out Boolean) is
      Handle : Accounts.Handle;
   begin
      Charge := 0;
      Spinlocks.enterCriticalSection (Ledger_Lock);
      OK := Ready and then not Broken and then Pages /= 0;
      if Kind = Process_Memory_Budget.Retained_DMA_Backing then
         OK := OK and then Retained_DMA_Budget.Can_Reserve
           (Retained_Limit, Retained_Pages, Pages);
      end if;
      if OK then
         Handle := Accounts.Resolve (Store, Owner);
         Accounts.Reserve (Store, Handle, Kind, Pages, OK);
         if OK then
            Charge := Accounts.Charge_Identity (Store, Handle, Kind);
            if Kind = Process_Memory_Budget.Retained_DMA_Backing then
               Retained_Pages := Retained_Pages + Pages;
            end if;
         end if;
      end if;
      Spinlocks.exitCriticalSection (Ledger_Lock);
   end Reserve;
   procedure Cancel_Unbound (Charge, Pages : Identity; OK : out Boolean) is
   begin
      Apply_Refund (Charge, Pages, False, OK);
   end Cancel_Unbound;
   procedure Inspect
     (Owner : Identity; Live : out Boolean; Pages, Limit_Pages : out Identity;
      OK : out Boolean) is
   begin
      Spinlocks.enterCriticalSection (Ledger_Lock);
      Accounts.Inspect (Store, Accounts.Resolve (Store, Owner), Live, Pages, Limit_Pages, OK);
      Spinlocks.exitCriticalSection (Ledger_Lock);
   end Inspect;
   procedure Metadata_Status
     (Stored, Growing, Retiring, Limit : out Identity; Faulted : out Boolean) is
   begin
      Spinlocks.enterCriticalSection (Ledger_Lock);
      Stored := Accounts.Metadata_Bytes (Store);
      Growing := In_Flight;
      Retiring := Retiring_Bytes;
      Limit := Metadata_Limit;
      Faulted := Broken;
      Spinlocks.exitCriticalSection (Ledger_Lock);
   end Metadata_Status;
end Memory_Accounting;
