with Ada.Text_IO; use Ada.Text_IO;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;
with BuddyAllocator;
with SlabAllocator;
with Spinlocks;
with LinkedLists;
with Page_Admission; use Page_Admission;
with Check_Page_Allocation;
with Check_ELF_Admission;
with Check_Heap_Growth;

procedure Main is
   procedure Ignore (Value : Natural) is null;
   package Lists is new LinkedLists (Natural, Ignore);
   use type Lists.NodePtr;
   Pool : SlabAllocator.Slab;
   A, B, C, D : Address;
   L : Lists.List;
   OK : Boolean;
   Model : array (1 .. 32) of Natural := [others => 0];
   Length : Natural := 0;
   Seed : Unsigned_32 := 42;

   procedure Check_List is
      N : Lists.NodePtr := L.head;
   begin
      pragma Assert (L.length = Length);
      if Length = 0 then
         pragma Assert (L.head = null and then L.tail = null);
         return;
      end if;
      for I in 1 .. Length loop
         pragma Assert (N /= null and then N.element = Model (I));
         pragma Assert (N.next.prev = N and then N.prev.next = N);
         N := N.next;
      end loop;
      pragma Assert (N = L.head);
      N := L.tail;
      for I in reverse 1 .. Length loop
         pragma Assert (N.element = Model (I));
         N := N.prev;
      end loop;
      pragma Assert (N = L.tail);
   end Check_List;
begin
   Check_Page_Allocation;
   Check_ELF_Admission;
   Check_Heap_Growth;
   -- Bootstrap failure is still an exception, but no lock/storage is retained.
   BuddyAllocator.Allow_Allocation := False;
   begin
      SlabAllocator.setup (Pool, 4096 * 8, 1);
      raise Program_Error with "setup unexpectedly succeeded";
   exception
      when SlabAllocator.OutOfMemoryException => null;
   end;
   pragma Assert (not Pool.initialized and then BuddyAllocator.Live_Blocks = 0);
   pragma Assert (Spinlocks.Locks_Held = 0);
   BuddyAllocator.Allow_Allocation := True;
   SlabAllocator.setup (Pool, 4096 * 8, 1);
   SlabAllocator.tryAllocate (Pool, A);
   pragma Assert (A /= Null_Address);
   BuddyAllocator.Allow_Allocation := False;
   for I in 1 .. 100 loop
      SlabAllocator.tryAllocate (Pool, B);
      pragma Assert (B = Null_Address and then Spinlocks.Locks_Held = 0);
      pragma Assert (Pool.numBlocks = 1 and then Pool.numFree = 0);
   end loop;
   begin
      SlabAllocator.Allocate (Pool, B);
      raise Program_Error with "Allocate unexpectedly succeeded";
   exception
      when SlabAllocator.OutOfMemoryException => null;
   end;
   pragma Assert (Spinlocks.Locks_Held = 0);
   SlabAllocator.Deallocate (Pool, A);
   SlabAllocator.tryAllocate (Pool, B);
   pragma Assert (B = A); -- Recovery even while physical allocation is disabled.
   BuddyAllocator.Allow_Allocation := True;
   SlabAllocator.tryAllocate (Pool, C);
   SlabAllocator.tryAllocate (Pool, D);
   pragma Assert (C /= Null_Address and then D /= Null_Address);
   pragma Assert (B /= C and then C /= D and then B /= D);
   declare
      Before : constant Natural := BuddyAllocator.Attempts;
   begin
      SlabAllocator.tryAllocate (Pool, A);
      pragma Assert (A = Null_Address and then Spinlocks.Locks_Held = 0);
      pragma Assert (BuddyAllocator.Attempts = Before); -- Maximum block count.
   end;
   SlabAllocator.teardown (Pool);
   pragma Assert (BuddyAllocator.Live_Blocks = 0);
   Put_Line ("PASS slab: exhaustion, unlocked failure, retry, growth limit, reclamation");

   Lists.setup (1);
   Lists.create (L, 0);
   declare
      Before : constant Natural := Lists.nodeSlab.numFree;
   begin
      Lists.tryInsertFront (L, 1, OK);
      pragma Assert (not OK and then Lists.nodeSlab.numFree = Before);
      Check_List;
   end;
   Lists.create (L, 1000);
   BuddyAllocator.Allow_Allocation := False;
   loop
      Lists.tryInsertFront (L, L.length, OK);
      exit when not OK;
   end loop;
   declare
      Head : constant Lists.NodePtr := L.head;
      Count : constant Natural := L.length;
   begin
      for I in 1 .. 100 loop
         Lists.tryInsertFront (L, 999, OK);
         pragma Assert (not OK and then L.head = Head and then L.length = Count);
         pragma Assert (Spinlocks.Locks_Held = 0);
      end loop;
      Lists.popFront (L);
      Lists.tryInsertFront (L, 999, OK);
      pragma Assert (OK and then L.length = Count);
   end;
   Lists.clear (L);
   Check_List;
   Lists.create (L, Model'Length);
   -- Model-based mixed front/back operations, including empty/singleton/full.
   for Step in 1 .. 20_000 loop
      Seed := Seed * 1_664_525 + 1_013_904_223;
      case Shift_Right (Seed, 16) mod 4 is
         when 0 | 1 =>
            if Length < Model'Length then
               if (Seed and 1) = 0 then
                  Lists.insertFront (L, Step);
                  for I in reverse 1 .. Length loop Model (I + 1) := Model (I); end loop;
                  Model (1) := Step;
               else
                  Lists.insertBack (L, Step);
                  Model (Length + 1) := Step;
               end if;
               Length := Length + 1;
            else
               declare
                  Before : constant Natural := Lists.nodeSlab.numFree;
               begin
                  Lists.tryInsertFront (L, Step, OK);
                  pragma Assert (not OK and then Lists.nodeSlab.numFree = Before);
               end;
            end if;
         when others =>
            if Length > 0 then
               if (Seed and 1) = 0 then
                  Lists.popFront (L);
                  for I in 1 .. Length - 1 loop Model (I) := Model (I + 1); end loop;
               else
                  Lists.popBack (L);
               end if;
               Length := Length - 1;
            end if;
      end case;
      Check_List;
   end loop;
   Lists.clear (L);
   Length := 0;
   Check_List;
   -- Every interval, including whole-list/head/tail/single-node detach.
   -- Caller owns the node handles; outside/mismatched ranges must not mutate.
   for Size in 1 .. 16 loop
      for First_Index in 1 .. Size loop
         for Last_Index in First_Index .. Size loop
            declare
               Detached, Other : Lists.List;
               First_Node, Last_Node, Cursor : Lists.NodePtr;
               Before_Free : constant Natural := Lists.nodeSlab.numFree;
               Count : constant Positive := Last_Index - First_Index + 1;
            begin
               Lists.create (Detached, 0);
               Lists.create (Other, 1);
               Lists.insertBack (Other, 999);
               Lists.create (L, 32);
               for I in 1 .. Size loop Lists.insertBack (L, I); Model (I) := I; end loop;
               Length := Size;
               First_Node := L.head;
               for I in 2 .. First_Index loop First_Node := First_Node.next; end loop;
               Last_Node := First_Node;
               for I in 2 .. Count loop Last_Node := Last_Node.next; end loop;
               Lists.detachRange (L, Other.head, Other.tail, 1, Detached, OK);
               pragma Assert (not OK and Detached.length = 0);
               Check_List;
               Lists.detachRange (L, First_Node, Other.tail, Count, Detached, OK);
               pragma Assert (not OK and Detached.length = 0);
               Check_List;
               Lists.detachRange (L, First_Node, Last_Node, Count, Other, OK);
               pragma Assert (not OK and Other.length = 1);
               Check_List;
               Lists.detachRange (L, L.tail, L.head, Size + 1, Detached, OK);
               pragma Assert (not OK);
               Check_List;
               Lists.detachRange (L, null, Last_Node, Count, Detached, OK);
               pragma Assert (not OK);
               Check_List;
               if Size > 1 then
                  Lists.detachRange (L, L.tail, L.head, 2, Detached, OK);
                  pragma Assert (not OK); -- Never wrap through the list head.
                  Check_List;
               end if;
               declare
                  Free_Nodes : constant Natural := Lists.nodeSlab.numFree;
               begin
                  Lists.detachRange (L, First_Node, Last_Node, Count, Detached, OK);
                  pragma Assert (OK and Lists.nodeSlab.numFree = Free_Nodes);
               end;
               for I in Last_Index + 1 .. Size loop
                  Model (I - Count) := Model (I);
               end loop;
               Length := Size - Count;
               Check_List;
               Cursor := Detached.head;
               for I in First_Index .. Last_Index loop
                  pragma Assert (Cursor.element = I);
                  pragma Assert (Cursor.next.prev = Cursor and Cursor.prev.next = Cursor);
                  Cursor := Cursor.next;
               end loop;
               pragma Assert (Cursor = Detached.head and Detached.length = Count);
               Lists.clear (L); Lists.clear (Detached); Lists.clear (Other);
               Length := 0;
               pragma Assert (Lists.nodeSlab.numFree = Before_Free);
               Check_List;
            end;
         end loop;
      end loop;
   end loop;
   Put_Line ("PASS actual LinkedLists: 816 range detaches, rejection atomicity, node conservation");
   -- Every insertion position; exact node identity, order and free count.
   for Size in 1 .. 16 loop
      for Position in 1 .. Size loop
         declare
            Other : Lists.List;
            Target, Moved : Lists.NodePtr;
            Free_Before : Natural;
         begin
            Lists.create (L, 32);
            Lists.create (Other, 1);
            Lists.insertBack (Other, 999);
            for I in 1 .. Size loop
               Lists.insertBack (L, I); Model (I) := I;
            end loop;
            Length := Size;
            Free_Before := Lists.nodeSlab.numFree;
            Lists.moveFrontBefore (L, null, OK);
            pragma Assert (not OK);
            Check_List;
            Lists.moveFrontBefore (L, Other.head, OK);
            pragma Assert (not OK and Other.head.element = 999);
            Check_List;
            Target := L.head;
            Moved := L.head;
            for I in 2 .. Position loop Target := Target.next; end loop;
            Lists.moveFrontBefore (L, Target, OK);
            pragma Assert (OK and Lists.nodeSlab.numFree = Free_Before);
            if Position > 2 then
               for I in 1 .. Position - 2 loop Model (I) := I + 1; end loop;
               Model (Position - 1) := 1;
               pragma Assert (Target.prev = Moved);
            end if;
            Check_List;
            Lists.clear (L); Lists.clear (Other);
            Length := 0;
            Lists.moveFrontBefore (L, null, OK);
            pragma Assert (not OK);
            Check_List;
         end;
      end loop;
   end loop;
   Put_Line ("PASS actual LinkedLists: 136 head relocations, foreign/null rejection, no allocation");
   Lists.teardown;
   pragma Assert (BuddyAllocator.Live_Blocks = 0 and then Spinlocks.Locks_Held = 0);
   Put_Line ("PASS actual LinkedLists: failure atomicity, bidirectional links, 20000 model operations");

   for Offset in Unsigned_64 range 0 .. 4096 loop
      pragma Assert (Check (8192, 12288, 0, 0, 8192 + Offset, 0, 1, 0) =
                     (if Offset < 4096 then Admitted else Outside_Reservation));
   end loop;
   pragma Assert (Check (8192, 12288, 0, 0, 8191, 0, 1, 0) = Outside_Reservation);
   pragma Assert (Check (8192, 12288, 4096, 5000, 5000, 0, 1, 0) = Outside_Reservation);
   pragma Assert (Check (8192, 12288, 4096, 5000, 4999, 0, 1, 0) = Admitted);
   pragma Assert (Check (8192, 12288, 0, 0, 8192, 1, 1, 0) = Tracking_Full);
   pragma Assert (Check (8192, 12288, 0, 0, 8192, 1, 2, 1) = Quota_Full);
   pragma Assert (Check (8192, 12288, 0, 0, 8192, 1, 2, 0) = Admitted);
   pragma Assert (not Contains (2, 1, 1));
   pragma Assert (not Contains (1, 1, 1));
   pragma Assert (not Contains (User_Limit, Unsigned_64'Last, User_Limit));
   pragma Assert (Contains (User_Limit - 4096, User_Limit, User_Limit - 1));
   pragma Assert (not Contains (User_Limit - 4096, User_Limit, User_Limit));
   Put_Line ("PASS admission: stack/heap exclusive ends, guard boundary, tracking, quota, canonical limit");
end Main;
