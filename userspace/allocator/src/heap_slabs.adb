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

   -- A multiple of a class's stride is aligned to it (each stride is a
   -- constant: linear in each case).
   procedure Prove_Multiple (K : Natural; Class : Size_Class) with Ghost,
     Pre => K < Heap_Bitmap.Max_Slots, Post => (K * Stride (Class)) mod Stride (Class) = 0
   is
   begin
      case Class is
         when Bytes_16 => pragma Assert ((K * 16) mod 16 = 0);
         when Bytes_32 => pragma Assert ((K * 32) mod 32 = 0);
         when Bytes_48 => pragma Assert ((K * 48) mod 48 = 0);
         when Bytes_64 => pragma Assert ((K * 64) mod 64 = 0);
         when Bytes_80 => pragma Assert ((K * 80) mod 80 = 0);
         when Bytes_96 => pragma Assert ((K * 96) mod 96 = 0);
         when Bytes_112 => pragma Assert ((K * 112) mod 112 = 0);
         when Bytes_128 => pragma Assert ((K * 128) mod 128 = 0);
         when Bytes_160 => pragma Assert ((K * 160) mod 160 = 0);
         when Bytes_192 => pragma Assert ((K * 192) mod 192 = 0);
         when Bytes_224 => pragma Assert ((K * 224) mod 224 = 0);
         when Bytes_256 => pragma Assert ((K * 256) mod 256 = 0);
         when Bytes_320 => pragma Assert ((K * 320) mod 320 = 0);
         when Bytes_384 => pragma Assert ((K * 384) mod 384 = 0);
         when Bytes_448 => pragma Assert ((K * 448) mod 448 = 0);
         when Bytes_512 => pragma Assert ((K * 512) mod 512 = 0);
         when Bytes_640 => pragma Assert ((K * 640) mod 640 = 0);
         when Bytes_768 => pragma Assert ((K * 768) mod 768 = 0);
         when Bytes_896 => pragma Assert ((K * 896) mod 896 = 0);
         when Bytes_1024 => pragma Assert ((K * 1_024) mod 1_024 = 0);
         when Bytes_1280 => pragma Assert ((K * 1_280) mod 1_280 = 0);
         when Bytes_1536 => pragma Assert ((K * 1_536) mod 1_536 = 0);
         when Bytes_1792 => pragma Assert ((K * 1_792) mod 1_792 = 0);
         when Bytes_2048 => pragma Assert ((K * 2_048) mod 2_048 = 0);
         when Bytes_2560 => pragma Assert ((K * 2_560) mod 2_560 = 0);
         when Bytes_3072 => pragma Assert ((K * 3_072) mod 3_072 = 0);
         when Bytes_3584 => pragma Assert ((K * 3_584) mod 3_584 = 0);
         when Bytes_4096 => pragma Assert ((K * 4_096) mod 4_096 = 0);
      end case;
   end Prove_Multiple;

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
      Prove_Multiple (Item - 1, C);
      pragma Assert (Page_Of (Value.Value.Position) = Page and then Heap.Kinds (Page) = C);
      pragma Assert (Local_Of (Value.Value.Position) = (Item - 1) * Stride (C));
      pragma Assert (Local_Of (Value.Value.Position) mod Stride (Heap.Kinds (Page_Of (Value.Value.Position))) = 0);
      pragma Assert (Local_Of (Value.Value.Position) / Stride (C) + 1 = Item);
   end Allocate;

   procedure Allocate_At (Heap : in out State; Page : Slab_Id; Class : Size_Class;
                          Position : out Offset; Success : out Boolean) is
      Item : Slot;
   begin
      Position := 0;
      Success := False;
      if Heap.Kinds (Page) /= Class then return; end if;
      Heap_Bitmap.Take (Heap.Pages (Page), Item, Success);
      if Success then
         Position := First_Byte (Page, Class, Item);
         Prove_Multiple (Item - 1, Class);
         pragma Assert (Page_Of (Position) = Page and then Heap.Kinds (Page) = Class);
         pragma Assert (Local_Of (Position) = (Item - 1) * Stride (Class));
         pragma Assert (Local_Of (Position) mod Stride (Heap.Kinds (Page_Of (Position))) = 0);
         pragma Assert (Local_Of (Position) / Stride (Class) + 1 = Item);
      end if;
   end Allocate_At;

   function Is_Live (Heap : State; Position : Offset) return Boolean is
      Item : Natural;
      Exact : Boolean;
   begin
      Divide (Local_Of (Position), Heap.Kinds (Page_Of (Position)), Item, Exact);
      return Exact and then Heap_Bitmap.Live (Heap.Pages (Page_Of (Position)), Item + 1);
   end Is_Live;

   procedure Release_Reporting (Heap : in out State; Position : Offset; Status : out Release_Status;
                                Class : out Size_Class; Was_Full, Emptied, Single : out Boolean) is
      Page : constant Slab_Id := Page_Of (Position);
      Relative : constant Local_Offset := Local_Of (Position);
      Item : Natural;
      Full, Exact, OK : Boolean;
   begin
      Class := Heap.Kinds (Page);
      Was_Full := False;
      Emptied := False;
      Single := False;
      -- One multiplication by the class's reciprocal: the slot, and
      -- whether Relative starts it.
      Divide (Relative, Class, Item, Exact);
      if not Exact then Status := Invalid_Offset; return; end if;
      -- The pool's capacity is its class's slot count (Valid): one load
      -- beside the count, no computation.
      Full := Used (Heap, Page) = Heap_Bitmap.Capacity (Heap.Pages (Page));
      Heap_Bitmap.Release (Heap.Pages (Page), Item + 1, OK);
      if OK then
         Was_Full := Full;
         Emptied := Used (Heap, Page) = 0;
         Single := Used (Heap, Page) = 1;
         -- A slab that was full is a new source of free capacity: prefer it
         -- on the next search for this class, rather than scanning from an
         -- unrelated full slab (Find_Page still validates the hint). Only
         -- on that transition, so the common release stores nothing here.
         if Full then Heap.Current (Class) := Page; end if;
      end if;
      Status := (if OK then Released else Not_Allocated);
   end Release_Reporting;

   procedure Release (Heap : in out State; Position : Offset; Status : out Release_Status) is
      Class : Size_Class;
      Was_Full, Emptied, Single : Boolean;
   begin
      Release_Reporting (Heap, Position, Status, Class, Was_Full, Emptied, Single);
   end Release;

   procedure Prove_Disjoint (A, B : Slab_Id; AC, BC : Size_Class; AI, BI : Slot) is
   begin
      null;
   end Prove_Disjoint;
end Heap_Slabs;
