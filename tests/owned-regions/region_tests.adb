--  ABI-002 owned regions: a hosted model of the kernel's run bookkeeping
--  (kernel/src/process-owned_memory.adb: Commit, Decommit, Set_Access,
--  Release, Drop) built on the proved policy (Owned_Reservation_Policy:
--  Within, Commit_Admitted, Overlaps, Adjoins, Split, Decode, Access_Of),
--  checked after every operation against a page-bitmap oracle.
--
--  The model keeps what the kernel keeps: run records (base, count, the
--  frame-list position of the highest page) and one process frame list,
--  head first, into which commits prepend pages upward and other mappings
--  interleave foreign nodes. Checked invariants:
--    * no double mapping: runs are disjoint and a commit over any backed
--      page is refused with nothing changed;
--    * the runs cover exactly the backed pages, each run's frames are a
--      contiguous, descending segment of the frame list (Inventory_Valid);
--    * decommit unmaps exactly the backed pages of the range and frees
--      their frames (the charge to the quota goes with each frame);
--    * access changes only over wholly backed ranges, never beyond the
--      region's grant (read/write; no executable value exists);
--    * release frees every frame and leaves only foreign nodes.
--  Exhaustive over all sequences of three operations on an 8-page region
--  (every range including ones past the end, every operation, another
--  mapping interleaved before the second), then random sequences on a
--  64-page region. Page tables, TLB shootdown, grants and locking are not
--  modelled: the native check (storage-check, OWNED-REGION-CHECK) runs
--  the real kernel.
with Interfaces; use Interfaces;
with Owned_Reservation_Policy; use Owned_Reservation_Policy;
with Ada.Text_IO;
procedure Region_Tests is
   Checks : Natural := 0;
   procedure Check (Good : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Good then
         Ada.Text_IO.Put_Line ("FAIL " & What);
         raise Program_Error;
      end if;
   end Check;

   Region_Base : constant Unsigned_64 := 16#5800_0000_0000#;
   Max_Pages : constant := 64;
   subtype Page_Index is Natural range 0 .. Max_Pages - 1;
   Foreign : constant Integer := -1;
   Max_Nodes : constant := 4 * Max_Pages;
   Max_Runs : constant := Max_Pages;

   type Node_Array is array (1 .. Max_Nodes) of Integer;  --  page, or Foreign
   type Run_Record is record
      Used : Boolean := False;
      Base : Unsigned_64 := 0;
      Count : Natural := 0;
      First : Natural := 0;   --  frame-list position of the highest page
   end record;
   type Run_Array is array (1 .. Max_Runs) of Run_Record;
   type Access_Array is array (Page_Index) of Page_Access;
   type Bits is array (Page_Index) of Boolean;

   type World is record
      Pages : Natural := 8;
      List : Node_Array := (others => 0);
      Length : Natural := 0;
      Runs : Run_Array;
      --  The page tables: present, and each page's access.
      Present : Bits := (others => False);
      Modes : Access_Array := (others => Inaccessible);
      Freed : Natural := 0;
      --  The oracle.
      Backed : Bits := (others => False);
      Oracle_Modes : Access_Array := (others => Inaccessible);
      Oracle_Freed : Natural := 0;
   end record;

   function Region_Bytes (W : World) return Unsigned_64 is (Unsigned_64 (W.Pages) * Page_Bytes);
   function Page_Of (Address : Unsigned_64) return Natural is
     (Natural ((Address - Region_Base) / Page_Bytes));
   function Address_Of (Page : Natural) return Unsigned_64 is
     (Region_Base + Unsigned_64 (Page) * Page_Bytes);

   procedure Prepend (W : in out World; Value : Integer) is
   begin
      W.List (2 .. W.Length + 1) := W.List (1 .. W.Length);
      W.List (1) := Value;
      W.Length := W.Length + 1;
      for R of W.Runs loop
         if R.Used then R.First := R.First + 1; end if;
      end loop;
   end Prepend;

   --  The kernel's Inventory_Valid: the Pages pages below the run's
   --  highest Skip, nodes from First, descending (0, Count: the whole run).
   function Inventory_Valid (W : World; R : Run_Record; Skip, Pages : Natural) return Boolean is
   begin
      if R.Count = 0 or else R.First = 0 or else R.First + R.Count - 1 > W.Length
        or else Pages = 0 or else Skip > R.Count or else Pages > R.Count - Skip
      then
         return False;
      end if;
      for I in Skip .. Skip + Pages - 1 loop
         if W.List (R.First + I) /= Page_Of (R.Base) + R.Count - 1 - I
           or else not W.Present (Page_Of (R.Base) + R.Count - 1 - I)
         then
            return False;
         end if;
      end loop;
      return True;
   end Inventory_Valid;

   --  The kernel's Run_Cut: a policy split in frame-list counts.
   type Cut_Pages is record
      Lower, Dropped, Upper : Natural;
   end record;
   function Cut_Of (R : Run_Record; Address, Bytes : Unsigned_64) return Cut_Pages is
      S : constant Run_Split := Split (R.Base, Page_Count (R.Count), Address, Bytes);
   begin
      return (Natural (S.Lower), Natural (S.Dropped), Natural (S.Upper));
   end Cut_Of;

   --  The kernel's Drop: Cut.Dropped pages from position First + Upper go.
   procedure Drop (W : in out World; Index : Positive; Cut : Cut_Pages) is
      R : Run_Record renames W.Runs (Index);
      Old : constant Run_Record := W.Runs (Index);
      Drop_At : constant Natural := Old.First + Cut.Upper;
      Drop_Page : constant Natural := Page_Of (Old.Base) + Cut.Lower;
   begin
      Check (Cut.Lower + Cut.Dropped + Cut.Upper = Old.Count
             and then Inventory_Valid (W, Old, Cut.Upper, Cut.Dropped),
             "drop of a valid run");
      for P in Drop_Page .. Drop_Page + Cut.Dropped - 1 loop
         W.Present (P) := False;
      end loop;
      W.List (Drop_At .. W.Length - Cut.Dropped) := W.List (Drop_At + Cut.Dropped .. W.Length);
      W.Length := W.Length - Cut.Dropped;
      W.Freed := W.Freed + Cut.Dropped;
      for Other of W.Runs loop
         if Other.Used and then Other.First > Drop_At then
            Other.First := Other.First - Cut.Dropped;
         end if;
      end loop;
      if Cut.Lower = 0 and then Cut.Upper = 0 then
         R := (others => <>);
         return;
      end if;
      if Cut.Lower > 0 then
         R.First := Old.First + Cut.Upper;   --  after the removal
         R.Count := Cut.Lower;
         if Cut.Upper > 0 then
            for S of W.Runs loop
               if not S.Used then
                  S := (Used => True,
                        Base => Old.Base + Unsigned_64 (Cut.Lower + Cut.Dropped) * Page_Bytes,
                        Count => Cut.Upper, First => Old.First);
                  exit;
               end if;
            end loop;
         end if;
      else
         R.Base := Old.Base + Unsigned_64 (Cut.Dropped) * Page_Bytes;
         R.Count := Cut.Upper;
      end if;
   end Drop;

   function Cuts (R : Run_Record; Address, Bytes : Unsigned_64) return Boolean is
     (R.Used and then R.Count > 0 and then Overlaps (R.Base, Page_Count (R.Count), Address, Bytes));

   --  One locked step of the kernel's Change_Range on one region.
   function Change_Step (W : in out World; Address, Bytes : Unsigned_64;
                         Operation : Range_Operation) return Boolean is
   begin
      if not Within (Region_Base, Region_Bytes (W), Address, Bytes) then
         return False;
      end if;
      case Operation is
         when Commit =>
            if not Commit_Admitted (Region_Base, Region_Bytes (W), Address, Bytes) then
               return False;
            end if;
            declare
               Index : Natural := 0;
            begin
               for I in W.Runs'Range loop
                  if Cuts (W.Runs (I), Address, Bytes) then return False; end if;
                  if W.Runs (I).Used and then Adjoins (W.Runs (I).Base, Page_Count (W.Runs (I).Count), Address)
                    and then W.Runs (I).First = 1
                  then
                     Index := I;
                  end if;
               end loop;
               if Index = 0 then
                  for I in W.Runs'Range loop
                     if not W.Runs (I).Used then
                        W.Runs (I) := (Used => True, Base => Address, Count => 0, First => 0);
                        Index := I;
                        exit;
                     end if;
                  end loop;
               end if;
               for P in Page_Of (Address) .. Page_Of (Address) + Natural (Bytes / Page_Bytes) - 1 loop
                  Check (not W.Present (P), "commit maps an unmapped page");
                  Prepend (W, P);
                  W.Present (P) := True;
                  W.Modes (P) := Region_Grant;
                  W.Runs (Index).First := 1;
                  W.Runs (Index).Count := W.Runs (Index).Count + 1;
               end loop;
            end;
            return True;
         when Decommit =>
            for I in W.Runs'Range loop
               if Cuts (W.Runs (I), Address, Bytes) then
                  declare
                     Cut : constant Cut_Pages := Cut_Of (W.Runs (I), Address, Bytes);
                  begin
                     Drop (W, I, Cut);
                     exit when Cut.Lower > 0 and Cut.Upper > 0;
                  end;
               end if;
            end loop;
            return True;
         when Access_Operation =>
            declare
               Covered : Natural := 0;
            begin
               for R of W.Runs loop
                  if Cuts (R, Address, Bytes) then
                     Check (Inventory_Valid (W, R, Cut_Of (R, Address, Bytes).Upper,
                                             Cut_Of (R, Address, Bytes).Dropped),
                            "access over a valid run");
                     Covered := Covered + Cut_Of (R, Address, Bytes).Dropped;
                  end if;
               end loop;
               if Unsigned_64 (Covered) * Page_Bytes /= Bytes then return False; end if;
               for P in Page_Of (Address) .. Page_Of (Address) + Natural (Bytes / Page_Bytes) - 1 loop
                  Check (W.Present (P), "access changes a present page");
                  W.Modes (P) := Access_Of (Operation);
               end loop;
               return True;
            end;
      end case;
   end Change_Step;

   --  The kernel's Change_Range: a decommit or access change of more than
   --  Step_Pages pages goes in steps, highest first, after a check of the
   --  whole range (Range_Acceptable). The kernel's step is 512 pages; the
   --  model's is small so that its regions take several steps.
   Step_Pages : constant := 3;
   function Change (W : in out World; Address, Bytes : Unsigned_64;
                    Operation : Range_Operation) return Boolean is
      Step_Bytes : constant Unsigned_64 := Step_Pages * Page_Bytes;
      Top, Step_Base : Unsigned_64;
      Covered : Natural := 0;
   begin
      if Operation = Commit or else Bytes <= Step_Bytes or else Address > Unsigned_64'Last - Bytes
        or else Address mod Page_Bytes /= 0 or else Bytes mod Page_Bytes /= 0
      then
         return Change_Step (W, Address, Bytes, Operation);
      end if;
      if not Within (Region_Base, Region_Bytes (W), Address, Bytes) then
         return False;
      end if;
      if Operation /= Decommit then
         for R of W.Runs loop
            if Cuts (R, Address, Bytes) then
               Covered := Covered + Cut_Of (R, Address, Bytes).Dropped;
            end if;
         end loop;
         if Unsigned_64 (Covered) * Page_Bytes /= Bytes then return False; end if;
      end if;
      Top := Address + Bytes;
      loop
         Step_Base := (if Top - Address > Step_Bytes then Top - Step_Bytes else Address);
         if not Change_Step (W, Step_Base, Top - Step_Base, Operation) then
            return False;
         end if;
         exit when Step_Base = Address;
         Top := Step_Base;
      end loop;
      return True;
   end Change;

   --  The oracle's answer, and its effect when the call succeeds.
   function Oracle (W : in out World; Address, Bytes : Unsigned_64;
                    Operation : Range_Operation) return Boolean is
      Aligned : constant Boolean :=
        Bytes > 0 and then Address mod Page_Bytes = 0 and then Bytes mod Page_Bytes = 0
        and then Address >= Region_Base and then Address <= Unsigned_64'Last - Bytes
        and then Address + Bytes <= Region_Base + Region_Bytes (W);
      First, Last : Natural;
      Any, All_Backed : Boolean := False;
   begin
      if not Aligned then return False; end if;
      First := Page_Of (Address);
      Last := First + Natural (Bytes / Page_Bytes) - 1;
      All_Backed := True;
      for P in First .. Last loop
         Any := Any or W.Backed (P);
         All_Backed := All_Backed and W.Backed (P);
      end loop;
      case Operation is
         when Commit =>
            if Any or else Bytes > Maximum_Commit_Bytes then return False; end if;
            for P in First .. Last loop
               W.Backed (P) := True;
               W.Oracle_Modes (P) := Read_Write;
            end loop;
         when Decommit =>
            for P in First .. Last loop
               if W.Backed (P) then W.Oracle_Freed := W.Oracle_Freed + 1; end if;
               W.Backed (P) := False;
            end loop;
         when Make_Inaccessible | Make_Read_Only | Make_Read_Write =>
            if not All_Backed then return False; end if;
            for P in First .. Last loop
               W.Oracle_Modes (P) :=
                 (case Operation is
                    when Make_Inaccessible => Inaccessible,
                    when Make_Read_Only => Read_Only,
                    when others => Read_Write);
            end loop;
      end case;
      return True;
   end Oracle;

   procedure Agree (W : World; What : String) is
      Covered : Bits := (others => False);
      Foreign_Nodes, Backed_Count : Natural := 0;
   begin
      for R of W.Runs loop
         if R.Used then
            Check (R.Count > 0, What & ": no empty run");
            Check (Inventory_Valid (W, R, 0, R.Count), What & ": run inventory");
            for P in Page_Of (R.Base) .. Page_Of (R.Base) + R.Count - 1 loop
               Check (not Covered (P), What & ": runs disjoint (no double mapping)");
               Covered (P) := True;
            end loop;
         end if;
      end loop;
      for P in 0 .. W.Pages - 1 loop
         Check (Covered (P) = W.Backed (P), What & ": runs are the backed pages");
         Check (W.Present (P) = W.Backed (P), What & ": mapped exactly when backed");
         if W.Backed (P) then
            Backed_Count := Backed_Count + 1;
            Check (W.Modes (P) = W.Oracle_Modes (P), What & ": access");
            Check (W.Modes (P) <= Region_Grant, What & ": within the grant");
         end if;
      end loop;
      for I in 1 .. W.Length loop
         if W.List (I) = Foreign then Foreign_Nodes := Foreign_Nodes + 1; end if;
      end loop;
      Check (W.Length = Backed_Count + Foreign_Nodes, What & ": frame list is backed pages plus others");
      Check (W.Freed = W.Oracle_Freed,
             What & ": frames freed (and their charge) are the decommitted ones");
   end Agree;

   procedure Step (W : in out World; Address, Bytes : Unsigned_64; Operation : Range_Operation) is
      Oracle_World : World := W;
      Got : constant Boolean := Change (W, Address, Bytes, Operation);
      Want : constant Boolean := Oracle (Oracle_World, Address, Bytes, Operation);
   begin
      Check (Got = Want, "acceptance of " & Operation'Image);
      W.Backed := Oracle_World.Backed;
      W.Oracle_Modes := Oracle_World.Oracle_Modes;
      W.Oracle_Freed := Oracle_World.Oracle_Freed;
      Agree (W, Operation'Image);
   end Step;

   procedure Release_All (W : in out World) is
   begin
      for I in W.Runs'Range loop
         if W.Runs (I).Used then
            Drop (W, I, (Lower => 0, Dropped => W.Runs (I).Count, Upper => 0));
         end if;
      end loop;
      for P in 0 .. W.Pages - 1 loop
         if W.Backed (P) then W.Oracle_Freed := W.Oracle_Freed + 1; end if;
         W.Backed (P) := False;
      end loop;
      Agree (W, "release");
      for R of W.Runs loop
         Check (not R.Used, "release leaves no run");
      end loop;
      for I in 1 .. W.Length loop
         Check (W.List (I) = Foreign, "release leaves only other mappings");
      end loop;
   end Release_All;

   type Op_Choice is record
      First, Count : Natural;
      Operation : Range_Operation;
   end record;
   type Op_List is array (Positive range <>) of Op_Choice;

   Exhaustive_Sequences : Natural := 0;

   procedure Run_Sequence (Ops : Op_List) is
      W : World;
   begin
      for I in Ops'Range loop
         --  Another mapping's frame between the first and second steps.
         if I = 2 then Prepend (W, Foreign); end if;
         Step (W, Address_Of (Ops (I).First), Unsigned_64 (Ops (I).Count) * Page_Bytes,
               Ops (I).Operation);
      end loop;
      Release_All (W);
      Exhaustive_Sequences := Exhaustive_Sequences + 1;
   end Run_Sequence;

   --  Every (first, count) range of an 8-page region, ones past it too.
   procedure Exhaustive (Depth : Natural; Prefix : Op_List) is
   begin
      if Depth = 0 then
         Run_Sequence (Prefix);
         return;
      end if;
      for First in 0 .. 8 loop
         for Count in 1 .. 9 - First loop
            for Operation in Range_Operation loop
               Exhaustive (Depth - 1, Prefix & Op_Choice'(First, Count, Operation));
            end loop;
         end loop;
      end loop;
   end Exhaustive;

   --  A small deterministic generator for the long sequences.
   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next (Limit : Positive) return Natural is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Natural (Seed mod Unsigned_64 (Limit));
   end Next;
begin
   --  Decode: exactly the five positions, never anything else.
   for Raw in Unsigned_64'(0) .. 16 loop
      declare
         Operation : Range_Operation;
         Valid : Boolean;
      begin
         Decode (Raw, Operation, Valid);
         Check (Valid = (Raw <= 4), "decode accepts 0 .. 4 only");
         if Valid then Check (Range_Operation'Pos (Operation) = Raw, "decode position"); end if;
      end;
   end loop;
   declare
      Operation : Range_Operation;
      Valid : Boolean;
   begin
      Decode (Unsigned_64'Last, Operation, Valid);
      Check (not Valid, "decode refuses Last");
   end;
   for Operation in Access_Operation loop
      Check (Access_Of (Operation) <= Region_Grant, "access within the grant");
   end loop;
   Check (Access_Of (Make_Inaccessible) = Inaccessible and then Access_Of (Make_Read_Only) = Read_Only
          and then Access_Of (Make_Read_Write) = Read_Write, "access values");

   --  Admission at the edges, and malformed ranges.
   declare
      Bytes : constant Unsigned_64 := 64 * Page_Bytes;
   begin
      Check (Within (Region_Base, Bytes, Region_Base, Bytes), "whole region");
      Check (not Within (Region_Base, Bytes, Region_Base, Bytes + Page_Bytes), "past the end");
      Check (not Within (Region_Base, Bytes, Region_Base - Page_Bytes, Page_Bytes), "below");
      Check (not Within (Region_Base, Bytes, Region_Base + Bytes, Page_Bytes), "at the end");
      Check (not Within (Region_Base, Bytes, Region_Base + 1, Page_Bytes), "unaligned address");
      Check (not Within (Region_Base, Bytes, Region_Base, 1), "unaligned bytes");
      Check (not Within (Region_Base, Bytes, Region_Base, 0), "empty");
      Check (not Within (Region_Base, Bytes, Unsigned_64'Last - Page_Bytes + 1, Page_Bytes), "wrap");
      Check (not Within (Region_Base, 0, Region_Base, Page_Bytes), "empty region");
      Check (Commit_Admitted (Region_Base, Maximum_Reserved_Bytes, Region_Base, Maximum_Commit_Bytes)
             and then not Commit_Admitted (Region_Base, Maximum_Reserved_Bytes, Region_Base,
                                           Maximum_Commit_Bytes + Page_Bytes), "commit bound");
   end;

   Exhaustive (3, (1 .. 0 => <>));
   Ada.Text_IO.Put_Line ("exhaustive sequences:" & Exhaustive_Sequences'Image);

   --  Long random sequences on a 64-page region.
   for Trial in 1 .. 2_000 loop
      declare
         W : World;
      begin
         W.Pages := Max_Pages;
         for S in 1 .. 60 loop
            declare
               First : constant Natural := Next (Max_Pages);
               Count : constant Natural := 1 + Next (Max_Pages - First);
               Operation : constant Range_Operation := Range_Operation'Val (Next (5));
            begin
               if Next (4) = 0 and then W.Length < Max_Nodes - Max_Pages then
                  Prepend (W, Foreign);
               end if;
               Step (W, Address_Of (First), Unsigned_64 (Count) * Page_Bytes, Operation);
            end;
         end loop;
         Release_All (W);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("owned regions: PASS checks=" & Checks'Image &
                         "; a hosted model of the kernel adapter, not native page tables");
end Region_Tests;
