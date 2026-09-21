with Interfaces;
package body Heap_Slabs with SPARK_Mode is
   use type Interfaces.Unsigned_32;

   function Page_Of (Position : Offset) return Slab_Id is
   begin
      return Slab_Id (Interfaces.Unsigned_32 (Position) / Slab_Bytes + 1);
   end Page_Of;

   function Local_Of (Position : Offset) return Local_Offset is
   begin
      return Local_Offset (Interfaces.Unsigned_32 (Position) mod Slab_Bytes);
   end Local_Of;

   procedure Initialize (Heap : out State) is
      Empty : Heap_Bitmap.Pool;
   begin
      Heap_Bitmap.Initialize (Empty, Slots_For (Size_Class'First));
      Heap := (Pages => [others => Empty], Kinds => [others => Size_Class'First],
               Current => [others => Slab_Id'First]);
   end Initialize;

   function Scan_Pages (Heap : State; C : Size_Class) return Slab_Reference
     with No_Inline, Pre => Valid (Heap),
       Post => (Scan_Pages'Result /= No_Slab) =
         (for some P in Slab_Id => Used (Heap, P) = 0 or else
            (Class_Of (Heap, P) = C and then Used (Heap, P) < Slots_For (C))) and then
         (if Scan_Pages'Result /= No_Slab then Used (Heap, Scan_Pages'Result) = 0 or else
            (Class_Of (Heap, Scan_Pages'Result) = C and then
             Used (Heap, Scan_Pages'Result) < Slots_For (C)))
   is
      Capacity : constant Slot := Slots_For (C);
      Start : constant Slab_Id := Heap.Current (C);
      Lookahead_Slabs : constant := 8;
      Nearby_End : constant Slab_Id := Start + Natural'Min (Lookahead_Slabs, Slab_Id'Last - Start);
   begin
      -- Pack existing partial slabs before assigning empty backing.
      -- Receive the already selected class and calculate capacity once. Return
      -- one bounded scalar instead of a padded Page/Found result aggregate.
      -- A short lookahead avoids revisiting a full prefix in common mixed-size
      -- churn. It is only a selection heuristic: the complete scans below still
      -- establish success iff suitable capacity exists. No index maintenance is
      -- added to allocation/release; the worst case adds eight slab probes.
      for P in Start + 1 .. Nearby_End loop
         if Heap.Kinds (P) = C and then Used (Heap, P) < Capacity then
            return P;
         end if;
      end loop;
      for P in Slab_Id loop
         if Heap.Kinds (P) = C and then Used (Heap, P) < Capacity then
            return P;
         end if;
         pragma Loop_Invariant
           (for all Q in Slab_Id'First .. P =>
              Heap.Kinds (Q) /= C or else Used (Heap, Q) = Slots_For (C));
      end loop;
      for P in Slab_Id loop
         if Used (Heap, P) = 0 then return P; end if;
         pragma Loop_Invariant (for all Q in Slab_Id'First .. P => Used (Heap, Q) > 0);
      end loop;
      return No_Slab;
   end Scan_Pages;

   procedure Find_Page (Heap : State; Size : Request_Size; Page : out Slab_Id; Found : out Boolean)
     with Inline_Always, Pre => Valid (Heap),
       Post => Found = Can_Allocate (Heap, Size) and then
         (if Found then Used (Heap, Page) = 0 or else
            (Class_Of (Heap, Page) = Class_For (Size) and then
             Used (Heap, Page) < Slots_For (Class_For (Size))))
   is
      C : constant Size_Class := Class_For (Size);
      Selected : Slab_Reference;
   begin
      Page := Heap.Current (C);
      Found := Heap.Kinds (Page) = C and then Used (Heap, Page) < Slots_For (C);
      if not Found then
         Selected := Scan_Pages (Heap, C);
         Found := Selected /= No_Slab;
         if Found then Page := Selected; end if;
      end if;
   end Find_Page;

   procedure Allocate (Heap : in out State; Size : Request_Size; Value : out Allocation) is
      C : constant Size_Class := Class_For (Size);
      Page : Slab_Id;
      Found, OK : Boolean;
      Item : Slot;
   begin
      Value := (Value => (Success => False));
      Find_Page (Heap, Size, Page, Found);
      if not Found then return; end if;
      if Heap.Kinds (Page) /= C then
         Heap_Bitmap.Reconfigure (Heap.Pages (Page), Slots_For (C), OK);
         pragma Assert (OK);
         Heap.Kinds (Page) := C;
      end if;
      Heap_Bitmap.Allocate (Heap.Pages (Page), Item, OK);
      pragma Assert (OK);
      Heap.Current (C) := Page;
      Value := (Value => (True, First_Byte (Page, C, Item)));
   end Allocate;

   procedure Release (Heap : in out State; Position : Offset; Status : out Release_Status) is
      Page : constant Slab_Id := Page_Of (Position);
      C : constant Size_Class := Heap.Kinds (Page);
      Relative : constant Local_Offset := Local_Of (Position);
      OK : Boolean;
   begin
      if not Aligned (Relative, C) then Status := Invalid_Offset; return; end if;
      Heap_Bitmap.Release (Heap.Pages (Page), Quotient (Relative, C) + 1, OK);
      -- A successful release makes this slab a known source of free capacity.
      -- Prefer it on the next request for this class, rather than scanning
      -- from an unrelated full slab. Find_Page still validates the hint.
      if OK then Heap.Current (C) := Page; end if;
      Status := (if OK then Released else Not_Allocated);
   end Release;

   procedure Prove_Disjoint (A, B : Slab_Id; AC, BC : Size_Class; AI, BI : Slot) is
   begin
      null;
   end Prove_Disjoint;
end Heap_Slabs;
