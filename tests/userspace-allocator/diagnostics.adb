with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Bounded_Vectors;
with Heap_Slab_Instance;
with Heap_Classes; use Heap_Classes;
with Interfaces; use Interfaces;
procedure Diagnostics is
   package H renames Heap_Slab_Instance;
   use type H.Release_Status;
   use type Size_Class;
   type Workload is (Fixed64, Fixed256, Small, Mixed, Boundary, Bimodal);
   subtype Live_Index is Natural range 0 .. 1_023;
   type Live_Entry is record
      Active : Boolean := False;
      Position : H.Offset := 0;
   end record;
   Live : array (Live_Index) of Live_Entry;
   Heap : H.State;
   Value : H.Allocation;
   Status : H.Release_Status;
   Seed : Unsigned_64;
   Iterations : constant := 1_000_000;
   Page_Misses, Cache_Hits, Refills, Page_Probes : Long_Long_Integer;
   Max_Page : Natural;
   -- Separate executable policy model: ordinary membership and a standard
   -- bounded vector, with no inspection of private allocator representation.
   -- This is regression evidence, not another formal proof.
   type Membership is array (H.Slot) of Boolean;
   package Slots is new Ada.Containers.Bounded_Vectors (Positive, H.Slot);
   use type Ada.Containers.Count_Type;
   Cache_Limit : constant Ada.Containers.Count_Type := 64;
   type Model_Page is record
      Class : Size_Class := Size_Class'First;
      Occupied : Natural range 0 .. H.Slot'Last := 0;
      Active : Membership := [others => False];
      Available : Slots.Vector (Cache_Limit);
   end record;
   Model : array (H.Slab_Id) of Model_Page;
   Hints : array (Size_Class) of H.Slab_Id;
   function Next return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Next;
   procedure Check (OK : Boolean) is
   begin
      if not OK then raise Program_Error with "diagnostic oracle mismatch"; end if;
   end Check;
   procedure Model_Take (Size : Request_Size; Position : out H.Offset;
                         Hint_Hit, Reused : out Boolean; Probes : out Natural) is
      C : constant Size_Class := Class_For (Size);
      P : H.Slab_Id := Hints (C);
      Limit : constant Positive := H.Slab_Bytes / Stride (C);
      Found : Boolean := Model (P).Class = C and then Model (P).Occupied < Limit;
      S : H.Slot;
   begin
      Hint_Hit := Found;
      Probes := 1;
      if not Found then
         for Q in P + 1 .. Natural'Min (P + 8, H.Slab_Id'Last) loop
            Probes := Probes + 1;
            if Model (Q).Class = C and then Model (Q).Occupied < Limit then
               P := Q; Found := True; exit;
            end if;
         end loop;
      end if;
      if not Found then
         for Q in H.Slab_Id loop
            Probes := Probes + 1;
            if Model (Q).Class = C and then Model (Q).Occupied < Limit then
               P := Q; Found := True; exit;
            end if;
         end loop;
      end if;
      if not Found then
         for Q in H.Slab_Id loop
            Probes := Probes + 1;
            if Model (Q).Occupied = 0 then P := Q; Found := True; exit; end if;
         end loop;
      end if;
      Check (Found);
      if Model (P).Class /= C then
         Check (Model (P).Occupied = 0);
         Model (P) := (Class => C, others => <>);
      end if;
      Reused := not Model (P).Available.Is_Empty;
      if not Reused then
         for I in 1 .. Limit loop
            if not Model (P).Active (I) then Model (P).Available.Append (I); end if;
            exit when Model (P).Available.Length = Cache_Limit;
         end loop;
      end if;
      Check (not Model (P).Available.Is_Empty);
      S := Model (P).Available.Last_Element;
      Model (P).Available.Delete_Last;
      Check (S <= Limit and then not Model (P).Active (S));
      Model (P).Active (S) := True;
      Model (P).Occupied := Model (P).Occupied + 1;
      Hints (C) := P;
      Position := (P - 1) * H.Slab_Bytes + (S - 1) * Stride (C);
   end Model_Take;

   procedure Model_Drop (Position : H.Offset) is
      P : constant H.Slab_Id := Position / H.Slab_Bytes + 1;
      Local : constant Natural := Position mod H.Slab_Bytes;
      C : constant Size_Class := Model (P).Class;
      S : constant H.Slot := Local / Stride (C) + 1;
   begin
      Check (Local mod Stride (C) = 0 and then Model (P).Active (S));
      Model (P).Active (S) := False;
      Model (P).Occupied := Model (P).Occupied - 1;
      Check (not Model (P).Available.Contains (S));
      if Model (P).Available.Length = Cache_Limit then
         Model (P).Available.Delete_Last;
      end if;
      Model (P).Available.Append (S);
      Hints (C) := P;
   end Model_Drop;

   procedure Drop (I : Live_Index) is
   begin
      if Live (I).Active then
         Model_Drop (Live (I).Position);
         H.Release (Heap, Live (I).Position, Status);
         Check (Status = H.Released);
         Live (I).Active := False;
      end if;
   end Drop;
   procedure Trace (Work : Workload; Count : Positive; Measure : Boolean) is
      Sizes : constant array (0 .. 24) of Request_Size :=
        [1, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 256, 257,
         511, 512, 513, 1023, 1024, 1025, 2047, 2049, 4096];
      I : Live_Index;
      N : Unsigned_64;
      Size : Request_Size;
      Expected : H.Offset;
      Hint_Hit, Reused : Boolean;
      Probes : Natural;
   begin
      Seed := 16#123456789ABCDEF#;
      for Step in 1 .. Count loop
         I := Live_Index (Next mod 1_024);
         case Work is
            when Fixed64 => Size := 64;
            when Fixed256 => Size := 256;
            when Small => Size := Request_Size (1 + Next mod 128);
            when Mixed => Size := Request_Size (1 + Next mod 4_096);
            when Boundary => Size := Sizes (Integer (Next mod Sizes'Length));
            when Bimodal =>
               N := Next;
               Size := (if N mod 8 = 0 then Request_Size (2_048 + Shift_Right (N, 3) mod 2_049)
                        else Request_Size (1 + Shift_Right (N, 3) mod 64));
         end case;
         Drop (I);
         Model_Take (Size, Expected, Hint_Hit, Reused, Probes);
         H.Allocate (Heap, Size, Value);
         Check (Value.Value.Success and then Expected = Value.Value.Position);
         Live (I) := (True, Value.Value.Position);
         if Measure then
            if not Hint_Hit then Page_Misses := Page_Misses + 1; end if;
            if Reused then Cache_Hits := Cache_Hits + 1;
            else Refills := Refills + 1; end if;
            Page_Probes := Page_Probes + Long_Long_Integer (Probes);
            Max_Page := Natural'Max (Max_Page, Probes);
         end if;
      end loop;
   end Trace;
begin
   Put_Line ("workload,allocations,page_misses,cache_hits,refills,page_probes,max_page_probes");
   for Work in Workload loop
      H.Initialize (Heap);
      Model := [others => <>];
      Hints := [others => H.Slab_Id'First];
      Live := [others => (False, 0)];
      Trace (Work, 10_000, False);
      for I in Live_Index loop Drop (I); end loop;
      for I in Live_Index loop
         declare
            Expected : H.Offset;
            Hint_Hit, Reused : Boolean;
            Probes : Natural;
         begin
            Model_Take (64, Expected, Hint_Hit, Reused, Probes);
            H.Allocate (Heap, 64, Value);
            Check (Value.Value.Success and then Value.Value.Position = Expected);
         end;
         Live (I) := (True, Value.Value.Position);
      end loop;
      Page_Misses := 0; Cache_Hits := 0; Refills := 0; Page_Probes := 0;
      Max_Page := 0;
      Trace (Work, Iterations, True);
      Put_Line (Work'Image & "," & Iterations'Image & "," & Page_Misses'Image & "," &
                Cache_Hits'Image & "," & Refills'Image & "," & Page_Probes'Image & "," & Max_Page'Image);
      for I in Live_Index loop Drop (I); end loop;
   end loop;
end Diagnostics;
