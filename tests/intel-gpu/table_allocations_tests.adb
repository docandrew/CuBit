with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Table_Allocations;
with Intel_GPU_Table_Provenance.Backing;
procedure Table_Allocations_Tests is
   package B renames Intel_GPU_Buffer_Reply;
   Live : Boolean := True;
   Receipt : Unsigned_64 := 0;
   function Admitted (Session, Ticket : Unsigned_64; Slot : Positive) return Boolean is
     (Live and Session = 42 and
      (Ticket = Unsigned_64 (Slot) or Ticket = Unsigned_64 (Slot) + 100));
   function Confirmed (Session, Ticket : Unsigned_64) return Boolean is
     (Session = 42 and Ticket = Receipt);
   package R is new Intel_GPU_Table_Allocations (Admitted, Confirmed);
   Object : R.Registry;
   type Bytes is array (1 .. 32768) of Unsigned_8;
   Metadata : Bytes := [others => 0] with Alignment => 4096;
   Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
   Backing : constant B.Backing := B.From_Linear
     (16#200000#, B.Layout.CPU_Base, 8192, 16#200000#);
   Selected : B.Backing;
   OK : Boolean;
   function Ready return Boolean is (Live);
   function Session_Of (Ticket : Unsigned_64) return Unsigned_64 is
     (if Ticket in 1 .. 100 then 42 else 0);
   procedure Select_Slice
     (Session, Ticket : Unsigned_64; Selected : out B.Backing;
      Allocation_Offset : out Unsigned_64; Accepted : out Boolean) is
   begin
      Allocation_Offset := 0;
      Selected := (Ready => False); Accepted := False;
      if Ticket not in 1 .. 100 then return; end if;
      R.Lookup (Object, Positive (Ticket), Session, Ticket,
                R.Incremental_Tables, Selected, Accepted);
   end Select_Slice;
   package Resolver is new Intel_GPU_Table_Provenance.Backing
     (Ready, Session_Of, Select_Slice);
   procedure Resolve (Ticket : Unsigned_64; Expected : Boolean) is
      CPU, DMA : Unsigned_64;
      Accepted : Boolean;
   begin
      Resolver.Resolve_Owned_Page (42, Ticket, 4096, CPU, DMA, Accepted);
      pragma Assert (Accepted = Expected);
      if Expected then
         pragma Assert (CPU = B.Layout.CPU_Base + 4096 and DMA = 16#201000#);
      else
         pragma Assert (CPU = 0 and DMA = 0);
      end if;
   end Resolve;
   procedure Find (Slot : Positive; Expected : Boolean;
                   Session : Unsigned_64 := 42; Ticket : Unsigned_64 := 0;
                   Role : R.Allocation_Role := R.Incremental_Tables) is
   begin
      R.Lookup (Object, Slot, Session,
                (if Ticket = 0 then Unsigned_64 (Slot) else Ticket), Role, Selected, OK);
      pragma Assert (OK = Expected);
      if Expected then
         pragma Assert (Selected.Ready and then Selected.Bytes = 8192);
         pragma Assert (B.Page_Address (Selected, 4096) = 16#201000#);
      else
         pragma Assert (not Selected.Ready);
      end if;
   end Find;
   procedure Check_Range
     (Slot : Positive; Ticket, First, Bytes : Unsigned_64;
      Expected_Overlap : Boolean; Expected_Accepted : Boolean := True;
      Session : Unsigned_64 := 42; Epoch : Unsigned_64 := 0) is
      Overlaps, Accepted : Boolean;
   begin
      R.Check_Retained_Range (Object, Slot, Session, Ticket,
        (if Epoch = 0 then R.Revision (Object) else Epoch), First, Bytes, Overlaps, Accepted);
      pragma Assert (Accepted = Expected_Accepted and Overlaps = Expected_Overlap);
   end Check_Range;
begin
   R.Install (Object, 1, 42, 1, R.Incremental_Tables, Backing, OK);
   pragma Assert (OK);
   Find (1, True);
   Check_Range (1, 1, 16#200000#, 4096, True);
   Check_Range (1, 1, 16#202000#, 4096, False);
   Check_Range (1, 1, 16#1FFFFF#, 2, True);
   Check_Range (1, 1, 16#201FFF#, 1, True);
   Check_Range (1, 1, 16#1FFFFF#, 1, False);
   Check_Range (1, 1, 16#200000#, 0, True, False);
   Check_Range (1, 1, Unsigned_64'Last, 2, True, False);
   Check_Range (1, 2, 16#300000#, 4096, True, False);
   Check_Range (1, 1, 16#300000#, 4096, True, False, Session => 43);
   Check_Range (1, 1, 16#300000#, 4096, True, False, Epoch => R.Revision (Object) - 1);
   Check_Range (R.Capacity (Object) + 1, 1, 16#300000#, 4096, True, False);
   R.Install (Object, 1, 42, 2, R.Replacement_Image, Backing, OK);
   pragma Assert (not OK); Find (1, True);
   R.Install (Object, 2, 42, 1, R.Incremental_Tables, Backing, OK);
   pragma Assert (not OK);
   R.Install (Object, 2, 42, 2, R.Incremental_Tables, (Ready => False), OK);
   pragma Assert (not OK);
   Live := False;
   R.Install (Object, 2, 42, 2, R.Incremental_Tables, Backing, OK);
   pragma Assert (not OK); Find (1, False); Live := True;
   Find (1, False, Session => 43); Find (1, False, Ticket => 2);
   Find (1, False, Role => R.Replacement_Image);
   R.Extend (Object, Base, 16384, OK); pragma Assert (OK);
   pragma Assert (R.Capacity (Object) >= 100);
   for I in 2 .. 100 loop
      R.Install (Object, I, 42, Unsigned_64 (I), R.Incremental_Tables, Backing, OK);
      pragma Assert (OK);
   end loop;
   R.Extend (Object, Base, 32768, OK); pragma Assert (OK);
   R.Extend (Object, Base + 4096, 32768, OK); pragma Assert (not OK);
   for I in 1 .. 100 loop
      Find (I, True);
      Resolve (Unsigned_64 (I), True);
      R.Retire (Object, I, 42, Unsigned_64 (I), OK); pragma Assert (not OK);
      R.Revoke (Object, I, 43, Unsigned_64 (I), OK); pragma Assert (not OK);
      Find (I, True);
      R.Revoke (Object, I, 42, Unsigned_64 (I), OK); pragma Assert (OK);
      Live := False;
      Check_Range (I, Unsigned_64 (I), 16#200000#, 4096, True);
      Check_Range (I, Unsigned_64 (I), 16#202000#, 4096, False);
      Live := True;
      Find (I, False);
      Resolve (Unsigned_64 (I), False);
      R.Install (Object, I, 42, Unsigned_64 (I) + 100,
                 R.Replacement_Image, Backing, OK); pragma Assert (not OK);
      Receipt := Unsigned_64 (I);
      R.Retire (Object, I, 43, Receipt, OK); pragma Assert (not OK);
      R.Retire (Object, I, 42, Receipt, OK); pragma Assert (OK);
      Check_Range (I, Receipt, 16#202000#, 4096, True, False);
      R.Retire (Object, I, 42, Receipt, OK); pragma Assert (not OK);
      R.Install (Object, I, 42, Receipt + 100, R.Replacement_Image, Backing, OK);
      pragma Assert (OK);
      Find (I, False);
      Resolve (Unsigned_64 (I), False);
      Find (I, True, Ticket => Receipt + 100, Role => R.Replacement_Image);
      Check_Range (I, Receipt, 16#202000#, 4096, True, False);
      R.Retire (Object, I, 42, Receipt, OK); pragma Assert (not OK);
   end loop;
   Find (R.Capacity (Object) + 1, False);
   declare
      Census : R.Registry;
      Storage : Bytes := [others => 0] with Alignment => 4096;
      Epoch : Unsigned_64;
      Found : Boolean;
      Next : Natural;
      Cursor : Positive;
      Turns : Natural := 0;
      Item : R.Retained_Allocation;
      use type R.Allocation_Role;
   begin
      R.Extend (Census, Unsigned_64 (To_Integer (Storage'Address)), 16384, OK);
      pragma Assert (OK and R.Capacity (Census) >= 100);
      R.Install (Census, 100, 42, 100, R.Incremental_Tables, Backing, OK);
      pragma Assert (OK);
      Epoch := R.Revision (Census);
      R.Scan_Session (Census, 42, Epoch, 1, Found, Next, OK);
      pragma Assert (OK and not Found and Next = 65);
      R.Revoke (Census, 100, 42, 100, OK); pragma Assert (OK);
      R.Scan_Session (Census, 42, Epoch, Next, Found, Next, OK);
      pragma Assert (not OK and not Found and Next = 0);
      Epoch := R.Revision (Census);
      Live := False;
      Item := R.Retained_At (Census, 100);
      pragma Assert (Item.Present and then Item.Session = 42 and then
                     Item.Ticket = 100 and then Item.Role = R.Incremental_Tables and then Item.Revoked);
      R.Scan_Session (Census, 42, Epoch, 65, Found, Next, OK);
      pragma Assert (OK and Found); -- revoked/owner-lost records still block parent
      Live := True;
      Receipt := 100;
      R.Retire (Census, 100, 42, 100, OK); pragma Assert (OK);
      pragma Assert (not R.Retained_At (Census, 100).Present);
      Epoch := R.Revision (Census);
      R.Scan_Session (Census, 42, Epoch, 1, Found, Next, OK);
      pragma Assert (OK and not Found and Next = 65);
      -- Mutation behind an already-scanned prefix invalidates continuation.
      R.Install (Census, 1, 42, 1, R.Replacement_Image, Backing, OK); pragma Assert (OK);
      R.Scan_Session (Census, 42, Epoch, 65, Found, Next, OK); pragma Assert (not OK);
      Receipt := 1;
      R.Retire (Census, 1, 42, 1, OK); pragma Assert (OK);
      Epoch := R.Revision (Census);
      Cursor := 1;
      loop
         R.Scan_Session (Census, 42, Epoch, Cursor, Found, Next, OK);
         pragma Assert (OK and not Found);
         Turns := Turns + 1;
         exit when Next = 0;
         pragma Assert (Next = Cursor + 64);
         Cursor := Next;
      end loop;
      pragma Assert (Turns = (R.Capacity (Census) + 63) / 64);
      R.Scan_Session (Census, 0, Epoch, 1, Found, Next, OK); pragma Assert (not OK);
      R.Scan_Session (Census, 42, Epoch, R.Capacity (Census) + 1, Found, Next, OK);
      pragma Assert (not OK);
   end;
   Ada.Text_IO.Put_Line ("table allocation census: PASS bounded scan, revoked retention, mutation-behind-cursor rejection");
   Ada.Text_IO.Put_Line ("table allocation roles: PASS 100 slots, growth, exact retirement, stale-ticket rejection");
   Ada.Text_IO.Put_Line ("retained range observation: PASS revoked/owner-lost cleanup, exact identity/revision, boundaries, overflow and reuse rejection");
end Table_Allocations_Tests;
