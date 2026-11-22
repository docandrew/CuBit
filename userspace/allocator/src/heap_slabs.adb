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
      Heap_Bitmap.Initialize (Empty, Size_Class'First);
      Heap := [others => Empty];
   end Initialize;

   function Scan_Pages (Heap : State; C : Size_Class; Start : Slab_Id) return Slab_Reference
     with No_Inline, Pre => Valid (Heap),
       Post => (Scan_Pages'Result /= No_Slab) =
         (for some P in Slab_Id => Used (Heap, P) = 0 or else
            (Class_Of (Heap, P) = C and then Used (Heap, P) < Slots_For (C))) and then
         (if Scan_Pages'Result /= No_Slab then Used (Heap, Scan_Pages'Result) = 0 or else
            (Class_Of (Heap, Scan_Pages'Result) = C and then
             Used (Heap, Scan_Pages'Result) < Slots_For (C)))
   is
      Capacity : constant Slot := Slots_For (C);
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
         if Class_Of (Heap, P) = C and then Used (Heap, P) < Capacity then
            return P;
         end if;
      end loop;
      for P in Slab_Id loop
         if Class_Of (Heap, P) = C and then Used (Heap, P) < Capacity then
            return P;
         end if;
         pragma Loop_Invariant
           (for all Q in Slab_Id'First .. P =>
              Class_Of (Heap, Q) /= C or else Used (Heap, Q) = Slots_For (C));
      end loop;
      for P in Slab_Id loop
         if Used (Heap, P) = 0 then return P; end if;
         pragma Loop_Invariant (for all Q in Slab_Id'First .. P => Used (Heap, Q) > 0);
      end loop;
      return No_Slab;
   end Scan_Pages;

   procedure Find_Page (Heap : State; Size : Request_Size; Hint, Empty : Slab_Id; Page : out Slab_Id; Found : out Boolean)
     with Inline_Always, Pre => Valid (Heap),
       Post => Found = Can_Allocate (Heap, Size) and then
         (if Found then Used (Heap, Page) = 0 or else
            (Class_Of (Heap, Page) = Class_For (Size) and then
             Used (Heap, Page) < Slots_For (Class_For (Size))))
   is
      C : constant Size_Class := Class_For (Size);
      Selected : Slab_Reference;
   begin
      Page := Hint;
      Found := Class_Of (Heap, Page) = C and then Used (Heap, Page) < Slots_For (C);
      if not Found and then Used (Heap, Empty) = 0 then
         Page := Empty;
         Found := True;
      end if;
      if not Found then
         Selected := Scan_Pages (Heap, C, Hint);
         Found := Selected /= No_Slab;
         if Found then Page := Selected; end if;
      end if;
   end Find_Page;

   procedure Allocate (Heap : in out State; Size : Request_Size; Hint, Empty : Slab_Id; Value : out Allocation) is
      C : constant Size_Class := Class_For (Size);
      Page : Slab_Id;
      Found, OK : Boolean;
      Relative : Heap_Bitmap.Local;
   begin
      Value := (Value => (Success => False));
      Find_Page (Heap, Size, Hint, Empty, Page, Found);
      if not Found then return; end if;
      if Class_Of (Heap, Page) /= C then
         Heap_Bitmap.Reconfigure (Heap (Page), C, OK);
         pragma Assert (OK);
      end if;
      Heap_Bitmap.Allocate (Heap (Page), Relative, OK);
      pragma Assert (OK);
      Value := (Value => (True, (Page - 1) * Slab_Bytes + Relative));
      pragma Assert (Page_Of (Value.Value.Position) = Page and then Local_Of (Value.Value.Position) = Relative);
   end Allocate;

   procedure Allocate_At (Heap : in out State; Page : Slab_Id; Class : Size_Class;
                          Position : out Offset; Success : out Boolean) is
      Relative : Heap_Bitmap.Local;
      Untouched : Boolean;
      Before : constant State := Heap with Ghost;
   begin
      Position := 0;
      Success := False;
      if Heap_Bitmap.Class (Heap (Page)) /= Class then return; end if;
      Heap_Bitmap.Take (Heap (Page), Relative, Success, Untouched);
      if Success then
         Position := (Page - 1) * Slab_Bytes + Relative;
         pragma Assert (Page_Of (Position) = Page and then Local_Of (Position) = Relative);
         pragma Assert (Class_Of (Before, Page) = Class and then Class_Of (Heap, Page) = Class);
         pragma Assert (not Heap_Bitmap.Live (Before (Page), Heap_Bitmap.Item_Of (Heap (Page), Relative)));
         pragma Assert (Heap_Bitmap.Item_Of (Heap (Page), Relative) = Relative / Stride (Class) + 1);
         pragma Assert (not Member (Before, Page, Local_Of (Position) / Stride (Class) + 1));
         pragma Assert (not Live (Before, Position));
      end if;
   end Allocate_At;

   function Is_Live (Heap : State; Position : Offset) return Boolean is
     (Heap_Bitmap.Is_Live (Heap (Page_Of (Position)), Local_Of (Position)));

   procedure Release_Reporting (Heap : in out State; Position : Offset; Status : out Release_Status;
                                Class : out Size_Class; Was_Full, Emptied, Single : out Boolean) is
      Page : constant Slab_Id := Page_Of (Position);
      Full, OK : Boolean;
   begin
      Class := Heap_Bitmap.Class (Heap (Page));
      -- The pool's capacity is its class's block count: one load beside
      -- the count, no computation.
      Full := Used (Heap, Page) = Heap_Bitmap.Capacity (Heap (Page));
      Heap_Bitmap.Release (Heap (Page), Local_Of (Position), OK);
      if OK then
         pragma Assert (Class_Of (Heap, Page) = Class);
         pragma Assert (Heap_Bitmap.Item_Of (Heap (Page), Local_Of (Position)) = Local_Of (Position) / Stride (Class) + 1);
         pragma Assert (not Member (Heap, Page, Local_Of (Position) / Stride (Class) + 1));
         pragma Assert (not Live (Heap, Position));
      end if;
      Was_Full := OK and then Full;
      Emptied := OK and then Used (Heap, Page) = 0;
      Single := OK and then Used (Heap, Page) = 1;
      Status := (if OK then Released else Not_Allocated);
   end Release_Reporting;

   procedure Release (Heap : in out State; Position : Offset; Status : out Release_Status) is
      Page : constant Slab_Id := Page_Of (Position);
      Class : constant Size_Class := Heap_Bitmap.Class (Heap (Page));
      OK : Boolean;
   begin
      Heap_Bitmap.Release (Heap (Page), Local_Of (Position), OK);
      if OK then
         --  The class is kept, so Live reads the bit the release cleared.
         pragma Assert (Class_Of (Heap, Page) = Class);
         pragma Assert (Heap_Bitmap.Item_Of (Heap (Page), Local_Of (Position)) = Local_Of (Position) / Stride (Class) + 1);
         pragma Assert (not Member (Heap, Page, Local_Of (Position) / Stride (Class) + 1));
         pragma Assert (not Live (Heap, Position));
      end if;
      Status := (if OK then Released else Not_Allocated);
   end Release;

   procedure Prove_Disjoint (A, B : Slab_Id; AC, BC : Size_Class; AI, BI : Slot) is
   begin
      null;
   end Prove_Disjoint;
end Heap_Slabs;
