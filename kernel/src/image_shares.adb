-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with BuddyAllocator;
with Interrupt_Windows;
with Memory_Accounting;
with Process_Memory_Budget;
with Spinlocks;
with TextIO;

package body Image_Shares is

   use type System.Address;
   use type Policy.User_Count;

   type State is (Free, Filling, Ready);

   type Image is record
      Status   : State := Free;
      Key      : Policy.Key;
      Users    : Policy.User_Count := 0;
      Pages    : Natural := 0;
      -- The frame table: Pages physical addresses, in the kernel's map.
      Table    : System.Address := System.Null_Address;
      Order    : BuddyAllocator.Order := 0;
      -- The image's ledger, and the charge its frames are bound to.
      Ledger   : Memory_Accounting.Identity := 0;
      Charge   : Memory_Accounting.Identity := 0;
   end record;

   Images : array (Live_Share) of Image;
   -- An accounting step that cannot fail did: a broken ledger.
   Ledger_Broken : exception;
   Table_Lock : Spinlocks.Spinlock;

   Address_Bytes : constant := 8;
   type Frame_Table is array (Page_Index range <>) of Virtmem.PhysAddress;

   procedure Acquire (K : Policy.Key; Pages : Policy.Page_Count;
                      S : out Share; Fresh : out Boolean) is
      Joined, OK, Closed : Boolean;
      Block : System.Address;
      Order : BuddyAllocator.Order;
      Ledger, Charge : Memory_Accounting.Identity := 0;
   begin
      S := No_Share;
      Fresh := False;
      if Pages = 0 or else not Policy.Valid (K) then
         return;
      end if;
      Spinlocks.enterCriticalSection (Table_Lock);
      for I in Images'Range loop
         if Images (I).Status = Ready and then Policy.Same (Images (I).Key, K)
           and then Unsigned_64 (Images (I).Pages) = Pages
         then
            Policy.Join (Images (I).Users, Joined);
            if Joined then
               S := I;
               Spinlocks.exitCriticalSection (Table_Lock);
               return;
            end if;
         end if;
      end loop;
      for I in Images'Range loop
         if Images (I).Status = Free then
            Images (I) := (Status => Filling, Key => K, Users => 1, Pages => Natural (Pages),
                           others => <>);
            S := I;
            exit;
         end if;
      end loop;
      Spinlocks.exitCriticalSection (Table_Lock);
      if S = No_Share then
         return;
      end if;

      -- The new image's frame table and ledger, with no lock held.
      Order := BuddyAllocator.getOrder (Storage_Count (Pages) * Address_Bytes);
      BuddyAllocator.alloc (Order, Block);
      if Block /= BuddyAllocator.NO_BLOCK_AVAILABLE then
         declare
            Frames : Frame_Table (0 .. Natural (Pages) - 1) with Import, Address => Block;
         begin
            Frames := [others => 0];
         end;
         Memory_Accounting.Open (Ledger, OK);
         if OK then
            Memory_Accounting.Reserve (Ledger, Process_Memory_Budget.Ordinary,
                                       Memory_Accounting.Identity (Pages), Charge, OK);
            if not OK then
               Memory_Accounting.Close (Ledger, Closed);
               if not Closed then raise Ledger_Broken with "image ledger close"; end if;
               Ledger := 0;
            end if;
         end if;
      else
         OK := False;
      end if;
      Spinlocks.enterCriticalSection (Table_Lock);
      if OK then
         Images (S).Table := Block;
         Images (S).Order := Order;
         Images (S).Ledger := Ledger;
         Images (S).Charge := Charge;
         Fresh := True;
      else
         Images (S) := (others => <>);
      end if;
      Spinlocks.exitCriticalSection (Table_Lock);
      if not OK then
         if Block /= BuddyAllocator.NO_BLOCK_AVAILABLE then
            BuddyAllocator.free (Order, Block);
         end if;
         S := No_Share;
      end if;
   end Acquire;

   function Pages (S : Live_Share) return Natural is (Images (S).Pages);

   function Frame (S : Live_Share; Page : Page_Index) return Virtmem.PhysAddress is
      Frames : Frame_Table (0 .. Images (S).Pages - 1) with Import, Address => Images (S).Table;
   begin
      return Frames (Page);
   end Frame;

   procedure Set_Frame (S : Live_Share; Page : Page_Index; Frame : Virtmem.PhysAddress;
                        OK : out Boolean) is
      Frames : Frame_Table (0 .. Images (S).Pages - 1) with Import, Address => Images (S).Table;
   begin
      OK := False;
      if Images (S).Status /= Filling or else Frames (Page) /= 0 then
         return;
      end if;
      -- No process owns it, so the kernel binding (bindFrameCharge is for
      -- a frame a process owns, and refuses this one).
      BuddyAllocator.bindKernelFrameCharge (Frame, Images (S).Charge, OK);
      if OK then
         Frames (Page) := Frame;
      end if;
   end Set_Frame;

   procedure Publish (S : Live_Share) is
   begin
      Spinlocks.enterCriticalSection (Table_Lock);
      if Images (S).Status = Filling then
         Images (S).Status := Ready;
      end if;
      Spinlocks.exitCriticalSection (Table_Lock);
   end Publish;

   procedure Release (S : Live_Share) is
      Last : Boolean := False;
      Gone : Image;
      Unused : Memory_Accounting.Identity := 0;
      Closed, Cancelled : Boolean;
      Pace : Interrupt_Windows.Pace := Interrupt_Windows.Start;
   begin
      Spinlocks.enterCriticalSection (Table_Lock);
      if Images (S).Status /= Free and then Images (S).Users > 0 then
         Policy.Leave (Images (S).Users, Last);
         if Last then
            Gone := Images (S);
            Images (S) := (others => <>);
         end if;
      end if;
      Spinlocks.exitCriticalSection (Table_Lock);
      if not Last then
         return;
      end if;
      -- Nobody finds the image now: its frames go (a pinned one, under a
      -- debug view, when the view goes), each refunding the ledger.
      declare
         Frames : Frame_Table (0 .. Gone.Pages - 1) with Import, Address => Gone.Table;
      begin
         for F of Frames loop
            if F /= 0 then
               BuddyAllocator.freeFrame (F);
            else
               Unused := Unused + 1;
            end if;
            Interrupt_Windows.Point (Pace);
         end loop;
      end;
      -- Pages a failed fill never bound: their reservation goes unbound.
      if Unused > 0 then
         Memory_Accounting.Cancel_Unbound (Gone.Charge, Unused, Cancelled);
         if not Cancelled then raise Ledger_Broken with "image charge cancel"; end if;
      end if;
      Memory_Accounting.Close (Gone.Ledger, Closed);
      if not Closed then raise Ledger_Broken with "image ledger close"; end if;
      BuddyAllocator.free (Gone.Order, Gone.Table);
   end Release;

   procedure Pin_View (S : Live_Share; Page : Page_Index; Frame : Virtmem.PhysAddress;
                       Pinned : out Boolean) is
   begin
      Pinned := False;
      Spinlocks.enterCriticalSection (Table_Lock);
      if Images (S).Status = Ready and then Images (S).Users > 0
        and then Page < Images (S).Pages
      then
         declare
            Frames : Frame_Table (0 .. Images (S).Pages - 1)
              with Import, Address => Images (S).Table;
         begin
            if Frames (Page) = Frame and then Frame /= 0 then
               BuddyAllocator.pinKernelHeldFrame (Frame, Pinned);
            end if;
         end;
      end if;
      Spinlocks.exitCriticalSection (Table_Lock);
   end Pin_View;

   procedure Page_Of (S : Live_Share; Address : Unsigned_64; Page : out Page_Index;
                      Inside : out Boolean) is
      Base : Natural := 0;
      Span : Unsigned_64;
   begin
      Page := 0;
      Inside := False;
      Spinlocks.enterCriticalSection (Table_Lock);
      if Images (S).Status /= Free then
         for K in 1 .. Images (S).Key.Count loop
            declare
               Seg : Policy.Segment renames Images (S).Key.Segments (K);
            begin
               Span := Policy.Pages_Of (Seg);
               if Address >= Seg.Address and then (Address - Seg.Address) / Policy.Page_Bytes < Span then
                  Page := Base + Natural ((Address - Seg.Address) / Policy.Page_Bytes);
                  Inside := True;
                  exit;
               end if;
               Base := Base + Natural (Span);
            end;
         end loop;
      end if;
      Spinlocks.exitCriticalSection (Table_Lock);
   end Page_Of;

   -- The (key, reason) pairs already said, newest over oldest.
   Said_Capacity : constant := 32;
   type Said_Entry is record
      Used : Boolean := False;
      Key  : Policy.Key;
      Why  : Fallback := No_Room;
   end record;
   Said : array (1 .. Said_Capacity) of Said_Entry;
   Said_Next : Positive range 1 .. Said_Capacity := 1;

   function Reason_Name (Why : Fallback) return String is
     (case Why is
        when Quota_Refused => "quota-refused",
        when No_Room => "no-room",
        when Frame_Refused => "frame-refused",
        when Charge_Refused => "charge-refused",
        when Source_Unreadable => "source-unreadable",
        when Bytes_Differ => "bytes-differ");

   procedure Note_Fallback (K : Policy.Key; Why : Fallback; Name : String) is
      Seen : Boolean := False;
      Last : Natural := Name'First - 1;
   begin
      Spinlocks.enterCriticalSection (Table_Lock);
      for E of Said loop
         if E.Used and then E.Why = Why and then Policy.Same (E.Key, K) then
            Seen := True;
         end if;
      end loop;
      if not Seen then
         Said (Said_Next) := (Used => True, Key => K, Why => Why);
         Said_Next := (if Said_Next = Said_Capacity then 1 else Said_Next + 1);
      end if;
      Spinlocks.exitCriticalSection (Table_Lock);
      if Seen then
         return;
      end if;
      -- The name up to its first NUL (a fixed 16-character field).
      for I in Name'Range loop
         exit when Name (I) = ASCII.NUL;
         Last := I;
      end loop;
      TextIO.println ("Process.Loader: image not shared: " & Name (Name'First .. Last) &
                      " reason=" & Reason_Name (Why));
   end Note_Fallback;

   procedure Statistics (Count, Frames : out Natural) is
   begin
      Count := 0;
      Frames := 0;
      Spinlocks.enterCriticalSection (Table_Lock);
      for I of Images loop
         if I.Status /= Free then
            Count := Count + 1;
            Frames := Frames + I.Pages;
         end if;
      end loop;
      Spinlocks.exitCriticalSection (Table_Lock);
   end Statistics;

end Image_Shares;
