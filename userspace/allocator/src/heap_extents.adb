package body Heap_Extents with SPARK_Mode is
   procedure Initialize (Heap : out State) is
   begin
      Heap := (Owners => [others => No_Page], Lengths => [others => 0]);
   end Initialize;

   function Find (Heap : State; Count, Alignment : Run_Length) return Page_Reference with
     Post => (Find'Result /= No_Page) = Can_Allocate (Heap, Count, Alignment) and then
       (if Find'Result /= No_Page then (Find'Result - 1) mod Alignment = 0 and then
          Free_Run (Heap, Find'Result, Count))
   is
   begin
      for P in Page_Id loop
         if (P - 1) mod Alignment = 0 and then Free_Run (Heap, P, Count) then return P; end if;
         pragma Loop_Invariant
           (for all Q in 1 .. P => (Q - 1) mod Alignment /= 0 or else not Free_Run (Heap, Q, Count));
      end loop;
      return No_Page;
   end Find;

   procedure Allocate (Heap : in out State; Count, Alignment : Run_Length;
                       First : out Page_Reference) is
      Before : constant State := Heap with Ghost;
   begin
      First := Find (Heap, Count, Alignment);
      if First = No_Page then return; end if;
      for P in First .. First + Count - 1 loop
         Heap.Owners (P) := First;
         pragma Loop_Invariant
           (for all Q in Page_Id => Heap.Owners (Q) =
              (if Q in First .. P then First else Before.Owners (Q)));
      end loop;
      Heap.Lengths (First) := Count;
   end Allocate;

   procedure Release (Heap : in out State; First : Page_Id; Success : out Boolean) is
      Before : constant State := Heap with Ghost;
      Count : constant Page_Reference := Heap.Lengths (First);
   begin
      Success := Count > 0;
      if not Success then return; end if;
      for P in First .. First + Count - 1 loop
         Heap.Owners (P) := No_Page;
         pragma Loop_Invariant
           (for all Q in Page_Id => Heap.Owners (Q) =
              (if Q in First .. P then No_Page else Before.Owners (Q)));
      end loop;
      Heap.Lengths (First) := 0;
   end Release;
end Heap_Extents;
