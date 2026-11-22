-- Hosted tests for Capability_Tables (CAP-002 step 1). Scenario tests for
-- every result kind, then randomized differential runs against a reference
-- model. Built with -gnata: every contract (and the table invariant Valid)
-- is also checked at run time on the small instance.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Checks; use Checks;
with Fake_Pages;
with Test_Tables; use Test_Tables;
with Differential;

procedure Main is
   use Small;
   use type Fake_Pages.Page_Total;

   function Item (Id : Unsigned_64) return Test_Item is ((Id => Id));
   function Id_Of (E : Test_Item) return Unsigned_64 is (E.Id);

   Owner_A : constant Unsigned_64 := 1;
   Owner_B : constant Unsigned_64 := 2;

   -- Insert and expect Opened; returns the handle.
   procedure Open (T : in out Table; Id : Unsigned_64; H : out Handle;
                   What : String) is
      R : Insert_Result;
   begin
      Insert (T, Item (Id), R);
      Check (R.Status = Opened, What & ": opened");
      if R.Status = Opened then
         H := R.Opened_Handle;
      else
         H := From_Word (0);
      end if;
   end Open;

   procedure Expect_Lookup (T : Table; H : Handle; Want : Handle_Check;
                            What : String; Id : Unsigned_64 := 0) is
      R : constant Lookup_Result := Lookup (T, H);
   begin
      Check (R.Check = Want, What & ": lookup " & R.Check'Image
             & " want " & Want'Image);
      if R.Check = Current and then Want = Current then
         Check (R.Item.Id = Id, What & ": item");
      end if;
   end Expect_Lookup;

   procedure Expect_Close (T : in out Table; H : Handle; Want : Handle_Check;
                           What : String) is
      R : Remove_Result;
   begin
      Close (T, H, R);
      Check (R.Check = Want, What & ": close " & R.Check'Image
             & " want " & Want'Image);
   end Expect_Close;

   function Slot (H : Handle) return Natural is
     (Natural (To_Word (H) and 16#FFFF_FFFF#));
   function Gen (H : Handle) return Unsigned_32 is
     (Unsigned_32 (Shift_Right (To_Word (H), 32)));

   ---------------------------------------------------------------------------
   procedure Test_Empty is
      T : Table;
   begin
      Section ("empty table");
      Configure (T, Owner_A, Default_Limit);
      Check (Is_Empty (T) and then High_Water (T) = 0 and then Pages_Held (T) = 0,
             "fresh table holds nothing");
      Expect_Lookup (T, From_Word (0), Invalid, "zero word");
      Expect_Lookup (T, From_Word (1 * 2 ** 32), Invalid, "slot 0 gen 1 never issued");
      Expect_Close (T, From_Word (16#FFFF_FFFF_FFFF_FFFF#), Invalid, "all-ones word");
      Release_All (T);
      Check (Fake_Pages.Held (Owner_A) = 0, "nothing charged");
   end Test_Empty;

   ---------------------------------------------------------------------------
   procedure Test_Open_Close_Stale is
      T : Table;
      A, B : Handle;
      R : Remove_Result;
   begin
      Section ("open, close, stale, reuse");
      Configure (T, Owner_A, Default_Limit);
      Open (T, 10, A, "first");
      Check (Slot (A) = 0 and then Gen (A) = 1, "first handle is slot 0 gen 1");
      Check (Pages_Held (T) = 2 and then Fake_Pages.Held (Owner_A) = 2,
             "directory + one leaf charged");
      Expect_Lookup (T, A, Current, "first", 10);
      Close (T, A, R);
      Check (R.Check = Current, "close current");
      if R.Check = Current then
         Check (R.Removed.Id = 10, "close returns the removed item");
      end if;
      Check (Entry_Of (T, 0).Item = Empty_Item, "closed entry scrubbed");
      Expect_Lookup (T, A, Stale, "after close");
      Expect_Close (T, A, Stale, "double close");
      Open (T, 11, B, "reuse");
      Check (Slot (B) = 0 and then Gen (B) = 2, "freed slot reused with next generation");
      Expect_Lookup (T, A, Stale, "old handle after reuse");
      Expect_Lookup (T, B, Current, "new handle", 11);
      Expect_Lookup (T, From_Word (3 * 2 ** 32), Invalid, "future generation");
      Expect_Lookup (T, From_Word (2 * 2 ** 32 + 1), Invalid, "slot beyond high water");
      Release_All (T);
   end Test_Open_Close_Stale;

   ---------------------------------------------------------------------------
   procedure Test_Revoke is
      T : Table;
      A, B : Handle;
      R : Remove_Result;
   begin
      Section ("revoke");
      Configure (T, Owner_A, Default_Limit);
      Open (T, 20, A, "victim");
      Revoke (T, A, R);
      Check (R.Check = Current, "revoke current");
      Expect_Lookup (T, A, Revoked, "after revoke");
      Expect_Close (T, A, Revoked, "close after revoke reports Revoked");
      Revoke (T, A, R);
      Check (R.Check = Revoked, "revoke twice reports Revoked");
      Check (Live_Count (T) = 0, "revoked entry no longer counted");
      Open (T, 21, B, "reuse after revoke");
      Check (Slot (B) = Slot (A), "revoked slot reused");
      Expect_Lookup (T, A, Stale, "revoked handle after reuse is Stale");
      Release_All (T);
   end Test_Revoke;

   ---------------------------------------------------------------------------
   procedure Test_LRF_Order is
      T : Table;
      H : array (0 .. 5) of Handle;
      N : Handle;
   begin
      Section ("least-recently-freed reuse order");
      Configure (T, Owner_A, Default_Limit);
      for I in H'Range loop
         Open (T, Unsigned_64 (100 + I), H (I), "fill" & I'Image);
         Check (Slot (H (I)) = I, "fresh slots in order" & I'Image);
      end loop;
      Expect_Close (T, H (3), Current, "free 3");
      Expect_Close (T, H (1), Current, "free 1");
      Expect_Close (T, H (4), Current, "free 4");
      Open (T, 200, N, "reuse a");
      Check (Slot (N) = 3, "first reuse is the least recently freed (3)");
      Expect_Close (T, H (0), Current, "free 0");
      Open (T, 201, N, "reuse b");
      Check (Slot (N) = 1, "then 1");
      Open (T, 202, N, "reuse c");
      Check (Slot (N) = 4, "then 4");
      Open (T, 203, N, "reuse d");
      Check (Slot (N) = 0, "then 0, freed last");
      Open (T, 204, N, "fresh");
      Check (Slot (N) = 6, "free list empty: next never-used slot");
      Release_All (T);
   end Test_LRF_Order;

   ---------------------------------------------------------------------------
   procedure Test_Generation_Retirement is
      T : Table;
      H : array (1 .. Small_Generation_Limit) of Handle;
      N : Handle;
   begin
      Section ("generation limit retires the slot");
      Configure (T, Owner_A, Default_Limit);
      for G in H'Range loop
         Open (T, Unsigned_64 (G), H (G), "cycle" & G'Image);
         Check (Slot (H (G)) = 0 and then Gen (H (G)) = Unsigned_32 (G),
                "slot 0 generation" & G'Image);
         Expect_Close (T, H (G), Current, "cycle close" & G'Image);
      end loop;
      Check (Entry_Of (T, 0).State = Retired, "slot 0 retired at the limit");
      Open (T, 99, N, "after retirement");
      Check (Slot (N) = 1 and then Gen (N) = 1, "a retired slot is never reused");
      for G in H'Range loop
         Expect_Lookup (T, H (G), Stale, "retired generation" & G'Image);
      end loop;
      Expect_Lookup (T, From_Word (4 * 2 ** 32), Invalid, "generation past the limit");
      -- Revoked at the limit: retired, and its last handle still says Revoked.
      for G in 1 .. Small_Generation_Limit loop
         declare
            X : Handle;
            R : Remove_Result;
         begin
            Open (T, 300, X, "slot 1 cycle");
            if G < Small_Generation_Limit then
               Close (T, X, R);
            else
               Revoke (T, X, R);
               Check (Entry_Of (T, Slot_Index (Slot (X))).State = Retired,
                      "revoked at the limit is retired");
               Expect_Lookup (T, X, Revoked, "retired by revocation");
            end if;
         end;
      end loop;
      Release_All (T);
   end Test_Generation_Retirement;

   ---------------------------------------------------------------------------
   procedure Test_Limit is
      T : Table;
      H : array (1 .. 3) of Handle;
      R : Insert_Result;
      N : Handle;
   begin
      Section ("handle limit");
      Configure (T, Owner_A, (Kind => Bounded, Most => 3));
      for I in H'Range loop
         Open (T, Unsigned_64 (I), H (I), "under limit" & I'Image);
      end loop;
      Insert (T, Item (4), R);
      Check (R.Status = Limit_Reached, "fourth open refused: Limit_Reached");
      Check (Live_Count (T) = 3 and then High_Water (T) = 3, "refusal changed nothing");
      Expect_Close (T, H (2), Current, "close one");
      Open (T, 5, N, "room again");
      Check (Slot (N) = 1, "reuses the closed slot");
      Release_All (T);

      Check (Bound (Default_Limit) = Slot_Count'Min (4096, Max_Slots),
             "default limit is 4096 (capped by capacity)");
      Check (Bound (Kernel_Limit_Handles) = Max_Slots, "Kernel_Limit is the capacity");
   end Test_Limit;

   ---------------------------------------------------------------------------
   procedure Test_Capacity is
      T : Table;
      H : Handle;
      R : Insert_Result;
   begin
      Section ("structural capacity (Kernel_Limit)");
      Configure (T, Owner_B, Kernel_Limit_Handles);
      for I in 1 .. Natural (Max_Slots) loop
         Open (T, Unsigned_64 (I), H, "fill");
      end loop;
      Check (Live_Count (T) = Max_Slots, "every slot live");
      Check (Natural (Pages_Held (T)) = 1 + Small_Leaves
             and then Natural (Fake_Pages.Held (Owner_B)) = 1 + Small_Leaves,
             "directory + every leaf charged");
      Insert (T, Item (0), R);
      Check (R.Status = Table_Full, "past capacity: Table_Full");
      Release_All (T);
      Check (Is_Empty (T) and then Fake_Pages.Held (Owner_B) = 0,
             "release returns every page");
   end Test_Capacity;

   ---------------------------------------------------------------------------
   procedure Test_Allocator_Failure is
      T : Table;
      H : Handle;
      R : Insert_Result;
      type Snapshot is array (Slot_Index) of Slot_Entry;
      Before : Snapshot;
      function Same return Boolean is
        (for all S in Slot_Index => Entry_Of (T, S) = Before (S));
      Bad_Before : constant Fake_Pages.Page_Total := Fake_Pages.Bad_Releases;
   begin
      Section ("allocator refusal leaves the table unchanged");
      Configure (T, Owner_A, Default_Limit);

      Fake_Pages.Set_Budget (0);
      Insert (T, Item (1), R);
      Fake_Pages.Clear_Budget;
      Check (R.Status = Out_Of_Memory, "no directory page: Out_Of_Memory");
      Check (Is_Empty (T) and then Fake_Pages.Held (Owner_A) = 0, "still empty, nothing charged");

      Fake_Pages.Set_Budget (1);
      Insert (T, Item (1), R);
      Fake_Pages.Clear_Budget;
      Check (R.Status = Out_Of_Memory, "directory but no leaf: Out_Of_Memory");
      Check (Is_Empty (T) and then Fake_Pages.Held (Owner_A) = 0,
             "the directory obtained for the failed growth was returned");

      for I in 1 .. Natural (Entries_Per_Leaf) loop
         Open (T, Unsigned_64 (I), H, "fill first leaf");
      end loop;
      for S in Slot_Index loop
         Before (S) := Entry_Of (T, S);
      end loop;
      Fake_Pages.Set_Budget (0);
      Insert (T, Item (65), R);
      Fake_Pages.Clear_Budget;
      Check (R.Status = Out_Of_Memory, "second leaf refused: Out_Of_Memory");
      Check (Same, "every entry unchanged");
      Check (High_Water (T) = Entries_Per_Leaf and then Pages_Held (T) = 2
             and then Fake_Pages.Held (Owner_A) = 2, "pages unchanged");
      Open (T, 65, H, "growth after refusal");
      Check (Slot (H) = 64 and then Pages_Held (T) = 3, "grows once memory is available");
      -- A free slot needs no page: refusal does not matter.
      Expect_Close (T, H, Current, "free one");
      Fake_Pages.Set_Budget (0);
      Insert (T, Item (66), R);
      Fake_Pages.Clear_Budget;
      Check (R.Status = Opened, "reuse needs no memory");
      Release_All (T);
      Check (Fake_Pages.Held (Owner_A) = 0, "every page returned");
      Check (Fake_Pages.Bad_Releases = Bad_Before, "no bad release");
   end Test_Allocator_Failure;

   ---------------------------------------------------------------------------
   procedure Test_Release_And_Restart is
      T : Table;
      H, N : Handle;
   begin
      Section ("release on exit, then a new incarnation");
      Configure (T, Owner_A, Default_Limit);
      for I in 1 .. 100 loop
         Open (T, Unsigned_64 (I), H, "fill");
      end loop;
      Check (Pages_Held (T) = 3 and then Fake_Pages.Held (Owner_A) = 3, "two leaves + directory");
      Release_All (T);
      Check (Is_Empty (T) and then Fake_Pages.Held (Owner_A) = 0, "all returned");
      Expect_Lookup (T, H, Invalid, "handle after release");
      Configure (T, Owner_A, Default_Limit);
      Open (T, 1, N, "new incarnation");
      Check (Slot (N) = 0 and then Gen (N) = 1, "starts over");
      Release_All (T);
   end Test_Release_And_Restart;

   ---------------------------------------------------------------------------
   procedure Test_Iteration_And_Words is
      T : Table;
      H : array (0 .. 9) of Handle;
      Live_Seen : Natural := 0;
      type Word_List is array (Positive range <>) of Unsigned_64;
      Sample_Words : constant Word_List :=
        [0, 1, 16#FFFF_FFFF#, 16#1_0000_0000#, 16#DEAD_BEEF_0000_0007#,
         Unsigned_64'Last];
   begin
      Section ("iteration and handle words");
      Configure (T, Owner_A, Default_Limit);
      for I in H'Range loop
         Open (T, Unsigned_64 (I + 1), H (I), "fill");
      end loop;
      for I in H'Range loop
         if I mod 3 = 0 then
            Expect_Close (T, H (I), Current, "thin out");
         end if;
      end loop;
      for S in 0 .. High_Water (T) - 1 loop
         if Is_Live_At (T, S) then
            Live_Seen := Live_Seen + 1;
            Check (To_Word (Handle_At (T, S)) = To_Word (H (Natural (S))),
                   "Handle_At names the issued handle");
            Check (Item_At (T, S).Id = Unsigned_64 (S + 1), "Item_At");
         end if;
      end loop;
      Check (Live_Seen = 6 and then Live_Count (T) = 6, "six live");
      Check (To_Word (H (1)) = 16#0000_0001_0000_0001#, "word = gen << 32 | slot");
      for W of Sample_Words loop
         Check (To_Word (From_Word (W)) = W, "From_Word/To_Word round trip");
      end loop;
      Expect_Lookup (T, From_Word (16#0000_0001_FFFF_FFFF#), Invalid, "slot field 2**32-1");
      Expect_Lookup (T, From_Word (Shift_Left (1, 32) or Unsigned_64 (Max_Slots)),
                     Invalid, "slot field = capacity");
      Release_All (T);
   end Test_Iteration_And_Words;

   ---------------------------------------------------------------------------
   procedure Small_Differential is new Differential (Small, Item, Id_Of);

   Seeds : constant array (1 .. 4) of Unsigned_64 :=
     [16#C0FF_EE00_CAB1_E5ED#, 1, 16#0002_0026_1012#, 16#DEAD_BEEF#];
begin
   Test_Empty;
   Test_Open_Close_Stale;
   Test_Revoke;
   Test_LRF_Order;
   Test_Generation_Retirement;
   Test_Limit;
   Test_Capacity;
   Test_Allocator_Failure;
   Test_Release_And_Restart;
   Test_Iteration_And_Words;
   for I in Seeds'Range loop
      Small_Differential (Seeds (I), 50_000, Owner_B + Unsigned_64 (I),
                          "small seed" & I'Image);
   end loop;
   Check (Fake_Pages.Held_Total = 0, "no page held at exit");
   Check (Fake_Pages.Bad_Releases = 0, "no foreign or double release overall");
   Put_Line ("allocations" & Fake_Pages.Allocations'Image & ", releases"
             & Fake_Pages.Releases'Image & ", refusals" & Fake_Pages.Refusals'Image);
   Finish;
end Main;
