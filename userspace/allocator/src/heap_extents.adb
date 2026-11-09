package body Heap_Extents with SPARK_Mode is
   procedure Initialize (Heap : out State) is
   begin
      Heap := (Owners => [others => No_Page], Lengths => [others => 0]);
   end Initialize;

   --  The first aligned start whose run is free. A window that holds an
   --  owned page is skipped past the whole run that page belongs to: no
   --  start inside the window or that run can be free, so this visits each
   --  allocated run at most once instead of testing every start.
   --  The first aligned start whose run is free. A window that holds an
   --  owned page is skipped past the whole run that page belongs to: no
   --  start inside the window or that run can be free, so this visits each
   --  allocated run about once instead of testing every start.
   function Find (Heap : State; Count, Alignment : Run_Length) return Page_Reference with
     Pre => Valid (Heap),
     Post => (Find'Result /= No_Page) = Can_Allocate (Heap, Count, Alignment) and then
       (if Find'Result /= No_Page then (Find'Result - 1) mod Alignment = 0 and then
          Free_Run (Heap, Find'Result, Count))
   is
      Last_Start : constant Page_Id := Page_Count - Count + 1;
      P : Positive := 1;
      Blocker, First_Of_Run : Page_Id;
      Found_Blocker : Boolean;
      Next : Positive;
   begin
      while P <= Last_Start loop
         pragma Loop_Invariant
           (for all Q in 1 .. P - 1 => (Q - 1) mod Alignment /= 0 or else not Free_Run (Heap, Q, Count));
         pragma Loop_Variant (Increases => P);
         if (P - 1) mod Alignment /= 0 then
            P := P + 1;
         else
            --  The last owned page in P .. P + Count - 1, if any.
            Found_Blocker := False;
            Blocker := P;
            for Q in reverse P .. P + Count - 1 loop
               pragma Loop_Invariant (for all R in Q + 1 .. P + Count - 1 => Heap.Owners (R) = No_Page);
               if Heap.Owners (Q) /= No_Page then
                  Blocker := Q;
                  Found_Blocker := True;
                  exit;
               end if;
            end loop;
            if not Found_Blocker then
               pragma Assert (Free_Run (Heap, P, Count));
               return P;
            end if;
            --  Blocker's run ends at Next - 1; no start from P up to there
            --  is free (each holds Blocker, or is itself owned).
            First_Of_Run := Heap.Owners (Blocker);
            Next := First_Of_Run + Heap.Lengths (First_Of_Run);
            pragma Assert (Next > Blocker);
            pragma Assert (for all Q in P .. Blocker => not Free_Run (Heap, Q, Count));
            pragma Assert (for all Q in Blocker .. Next - 1 => Heap.Owners (Q) = First_Of_Run);
            pragma Assert (for all Q in P .. Next - 1 => not Free_Run (Heap, Q, Count));
            P := Next;
         end if;
      end loop;
      pragma Assert (for all Q in P .. Page_Count => not Fits (Q, Count));
      pragma Assert (for all Q in Page_Id => (Q - 1) mod Alignment /= 0 or else not Free_Run (Heap, Q, Count));
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
