package body Heap_Extents with SPARK_Mode is
   procedure Initialize (Heap : out State) is
   begin
      Heap := (Owners => [others => No_Page], Lengths => [others => 0], Spans => [others => 0], Parks => [others => False], Rover => 1,
               Depths => [others => 0], Present => [others => 0], Holes => [others => [others => (1, 1)]]);
   end Initialize;

   --  No run starting at an owned page is free.
   procedure Prove_Owned (Heap : State; A, B : Page_Id; Count : Run_Length) with Ghost,
     Pre => (for all R in A .. B => Heap.Owners (R) /= No_Page),
     Post => (for all R in A .. B => not Free_Run (Heap, R, Count))
   is
   begin
      for R in A .. B loop
         pragma Assert (Heap.Owners (R) /= No_Page);
         pragma Assert (not Free_Run (Heap, R, Count));
         pragma Loop_Invariant (for all S in A .. R => not Free_Run (Heap, S, Count));
      end loop;
   end Prove_Owned;

   --  Room depends on the owners alone: A's free aligned run is B's.
   procedure Prove_Room (A, B : State; Count, Alignment : Run_Length) with Ghost,
     Pre => A.Owners = B.Owners and then Can_Allocate (A, Count, Alignment),
     Post => Can_Allocate (B, Count, Alignment)
   is
   begin
      for P in Page_Id loop
         pragma Loop_Invariant
           (for all Q in 1 .. P - 1 => (Q - 1) mod Alignment /= 0 or else not Free_Run (A, Q, Count));
         if (P - 1) mod Alignment = 0 and then Free_Run (A, P, Count) then
            pragma Assert (Free_Run (B, P, Count));
            return;
         end if;
      end loop;
      pragma Assert (not Can_Allocate (A, Count, Alignment));
   end Prove_Room;

   procedure Prove_Same_Owners (A, B : State; Count, Alignment : Run_Length) with Ghost,
     Pre => A.Owners = B.Owners,
     Post => Can_Allocate (A, Count, Alignment) = Can_Allocate (B, Count, Alignment)
   is
   begin
      if Can_Allocate (A, Count, Alignment) then
         Prove_Room (A, B, Count, Alignment);
      end if;
      if Can_Allocate (B, Count, Alignment) then
         Prove_Room (B, A, Count, Alignment);
      end if;
   end Prove_Same_Owners;

   --  The first aligned start from Start up whose run is free. A window
   --  that holds an owned page is skipped past the whole run that page
   --  belongs to: no start inside the window or that run can be free, so
   --  this visits each allocated run about once instead of testing every
   --  start.
   function Find_From (Heap : State; Count, Alignment : Run_Length; Start : Page_Id) return Page_Reference with
     Pre => Valid (Heap),
     Post => (if Find_From'Result /= No_Page then Find_From'Result >= Start
                and then (Find_From'Result - 1) mod Alignment = 0
                and then Free_Run (Heap, Find_From'Result, Count)
              else (for all Q in Start .. Page_Count =>
                      (Q - 1) mod Alignment /= 0 or else not Free_Run (Heap, Q, Count)))
   is
      Last_Start : constant Page_Id := Page_Count - Count + 1;
      P : Positive := Start;
      Blocker, First_Of_Run : Page_Id;
      Found_Blocker : Boolean;
      Next : Positive;
   begin
      while P <= Last_Start loop
         pragma Loop_Invariant (P >= Start);
         pragma Loop_Invariant
           (for all Q in Start .. P - 1 => (Q - 1) mod Alignment /= 0 or else not Free_Run (Heap, Q, Count));
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
            pragma Assert (for all Q in Blocker .. Next - 1 => Heap.Owners (Q) /= No_Page);
            Prove_Owned (Heap, Blocker, Next - 1, Count);
            pragma Assert (for all Q in P .. Next - 1 => not Free_Run (Heap, Q, Count));
            P := Next;
         end if;
      end loop;
      pragma Assert (for all Q in P .. Page_Count => not Fits (Q, Count));
      return No_Page;
   end Find_From;

   --  From the rover, then (when that finds nothing) from the start.
   function Find (Heap : State; Count, Alignment : Run_Length) return Page_Reference with
     Pre => Valid (Heap),
     Post => (Find'Result /= No_Page) = Can_Allocate (Heap, Count, Alignment) and then
       (if Find'Result /= No_Page then (Find'Result - 1) mod Alignment = 0 and then
          Free_Run (Heap, Find'Result, Count))
   is
      First : Page_Reference;
   begin
      if Heap.Rover > 1 then
         First := Find_From (Heap, Count, Alignment, Heap.Rover);
         if First /= No_Page then
            pragma Assert ((First - 1) mod Alignment = 0 and then Free_Run (Heap, First, Count));
            return First;
         end if;
      end if;
      First := Find_From (Heap, Count, Alignment, 1);
      if First /= No_Page then
         pragma Assert ((First - 1) mod Alignment = 0 and then Free_Run (Heap, First, Count));
      end if;
      return First;
   end Find;

   --  The hole class of a run of Pages.
   function Class_Of (Pages : Run_Length) return Hole_Class is
     (if Pages <= Exact_Holes then Pages - 1
      elsif Pages < 128 then Exact_Holes elsif Pages < 256 then Exact_Holes + 1
      elsif Pages < 512 then Exact_Holes + 2 elsif Pages < 1_024 then Exact_Holes + 3
      elsif Pages < 2_048 then Exact_Holes + 4 elsif Pages < 4_096 then Exact_Holes + 5
      else Exact_Holes + 6);

   --  Remember a free run (a hint; the newest replaces the top of a full
   --  stack).
   procedure Note_Hole (Heap : in out State; Start : Page_Id; Pages : Run_Length) with
     Inline, Post => Same_Map (Heap, Heap'Old) and then Heap.Rover = Heap'Old.Rover
   is
      K : constant Hole_Class := Class_Of (Pages);
   begin
      if Heap.Depths (K) < Hole_Depth then
         Heap.Depths (K) := Heap.Depths (K) + 1;
      end if;
      Heap.Present (K / 64) := Heap.Present (K / 64) or Hole_Word (2) ** (K mod 64);
      Heap.Holes (K) (Heap.Depths (K)) := (Start, Pages);
      if Fits (Start, Pages) then
         Heap.Spans (Start) := Pages;
         Heap.Spans (Start + Pages - 1) := Pages;
      end if;
   end Note_Hole;

   --  The pages First .. First + Count - 1 are free (read from the map).
   function Run_Is_Free (Heap : State; First : Page_Id; Count : Run_Length) return Boolean with
     Pre => Fits (First, Count),
     Post => Run_Is_Free'Result = Free_Run (Heap, First, Count)
   is
   begin
      for P in First .. First + Count - 1 loop
         if Heap.Owners (P) /= No_Page then return False; end if;
         pragma Loop_Invariant (for all Q in First .. P => Heap.Owners (Q) = No_Page);
      end loop;
      return True;
   end Run_Is_Free;

   --  Drop class K's top hole (if any); its summary bit goes with its last.
   procedure Pop_Hole (Heap : in out State; K : Hole_Class) with
     Inline, Post => Same_Map (Heap, Heap'Old) and then Heap.Rover = Heap'Old.Rover
   is
   begin
      if Heap.Depths (K) > 0 then
         Heap.Depths (K) := Heap.Depths (K) - 1;
      end if;
      if Heap.Depths (K) = 0 then
         Heap.Present (K / 64) := Heap.Present (K / 64) and not (Hole_Word (2) ** (K mod 64));
      end if;
   end Pop_Hole;

   --  The lowest set bit's number (Value /= 0), by halving.
   function Lowest_Bit (Value : Hole_Word) return Natural with
     Post => Lowest_Bit'Result < 64
   is
      V : Hole_Word := Value;
      N : Natural := 0;
   begin
      if (V and 16#FFFF_FFFF#) = 0 then N := N + 32; V := V / 2 ** 32; end if;
      if (V and 16#FFFF#) = 0 then N := N + 16; V := V / 2 ** 16; end if;
      if (V and 16#FF#) = 0 then N := N + 8; V := V / 2 ** 8; end if;
      if (V and 16#F#) = 0 then N := N + 4; V := V / 2 ** 4; end if;
      if (V and 16#3#) = 0 then N := N + 2; V := V / 2 ** 2; end if;
      if (V and 16#1#) = 0 then N := N + 1; end if;
      return N;
   end Lowest_Bit;

   --  The lowest class from From up that has a hole (No_Class: none).
   No_Class : constant := Hole_Class'Last + 1;
   subtype Class_Search is Natural range 0 .. No_Class;
   function Next_Class (Heap : State; From : Hole_Class) return Class_Search is
      Low : constant Hole_Word := (if From < 64 then Heap.Present (0) and not (Hole_Word (2) ** From - 1) else 0);
      High : constant Hole_Word :=
        (if From < 64 then Heap.Present (1) else Heap.Present (1) and not (Hole_Word (2) ** (From - 64) - 1));
      K : Natural;
   begin
      if Low /= 0 then
         return Lowest_Bit (Low);
      elsif High /= 0 then
         K := 64 + Lowest_Bit (High);
         return (if K <= Hole_Class'Last then K else No_Class);
      end if;
      return No_Class;
   end Next_Class;

   --  A remembered hole of at least Count pages that the map confirms is
   --  free, from the smallest class that has one up (No_Page: none).
   --  Holes the map no longer confirms are dropped; a confirmed one that is
   --  too short (in a class of many lengths) is kept, and the next class
   --  is tried. Remainder: what the hole has beyond Count.
   procedure Take_Hole (Heap : in out State; Count : Run_Length; First : out Page_Reference;
                        Remainder : out Page_Reference) with
     Pre => Valid (Heap),
     Post => Valid (Heap) and then Same_Map (Heap, Heap'Old) and then Heap.Rover = Heap'Old.Rover
       and then (if First /= No_Page then Free_Run (Heap, First, Count)
                   and then Remainder <= Page_Count - (First + Count - 1))
   is
      H : Hole;
      K : Class_Search := Next_Class (Heap, Class_Of (Count));
      Advance : Boolean;
   begin
      First := No_Page;
      Remainder := 0;
      --  Each step drops a hole or moves to a higher class: bounded.
      for Attempt in 1 .. 2 * Hole_Depth loop
         pragma Loop_Invariant (Same_Map (Heap, Heap'Loop_Entry) and then Heap.Rover = Heap'Loop_Entry.Rover);
         pragma Loop_Invariant (Valid (Heap));
         exit when K = No_Class;
         Advance := True;
         if Heap.Depths (K) > 0 then
            H := Heap.Holes (K) (Heap.Depths (K));
            if H.Pages >= Count and then Fits (H.Start, H.Pages) and then Run_Is_Free (Heap, H.Start, Count) then
               Pop_Hole (Heap, K);
               First := H.Start;
               Remainder := H.Pages - Count;
               return;
            elsif H.Pages >= Count or else not Fits (H.Start, H.Pages) or else not Run_Is_Free (Heap, H.Start, 1)
            then
               --  Stale: dropped; this class again while it has more.
               Pop_Hole (Heap, K);
               Advance := Heap.Depths (K) = 0;
            end if;
            --  Otherwise too short for Count: the next class.
         else
            Pop_Hole (Heap, K);
         end if;
         if Advance then
            exit when K = Hole_Class'Last;
            K := Next_Class (Heap, K + 1);
         end if;
      end loop;
   end Take_Hole;

   --  Take pages First .. First + Count - 1, free in the map; the rest of
   --  a hole that was taken (Remainder pages) is remembered as a hole.
   procedure Take (Heap : in out State; First : Page_Id; Count : Run_Length; Remainder : Page_Reference) with
     Pre => Valid (Heap) and then Fits (First, Count) and then Free_Run (Heap, First, Count),
     Post => Valid (Heap) and then Length (Heap, First) = Count and then
       (for all P in Page_Id =>
          (if P in First .. First + Count - 1 then Owner (Heap, P) = First
           else Owner (Heap, P) = Owner (Heap'Old, P))) and then
       (for all P in Page_Id => (if P /= First then Length (Heap, P) = Length (Heap'Old, P)))
   is
      Before : constant State := Heap with Ghost;
   begin
      for P in First .. First + Count - 1 loop
         Heap.Owners (P) := First;
         pragma Loop_Invariant
           (for all Q in Page_Id => Heap.Owners (Q) =
              (if Q in First .. P then First else Before.Owners (Q)));
      end loop;
      Heap.Lengths (First) := Count;
      Heap.Parks (First) := False;
      if First = Heap.Rover and then First + Count <= Page_Count then
         Heap.Rover := First + Count;
      end if;
      --  A hint: the map is checked when it is taken.
      if Remainder > 0 and then First + Count <= Page_Count then
         Note_Hole (Heap, First + Count, Remainder);
      end if;
   end Take;

   procedure Allocate_Hinted (Heap : in out State; Count : Run_Length; First : out Page_Reference) is
      Remainder : Page_Reference;
   begin
      Take_Hole (Heap, Count, First, Remainder);
      if First /= No_Page then
         Take (Heap, First, Count, Remainder);
      end if;
   end Allocate_Hinted;

   procedure Allocate (Heap : in out State; Count, Alignment : Run_Length;
                       First : out Page_Reference) is
      Remainder : Page_Reference := 0;
      Before : constant State := Heap with Ghost;
   begin
      First := No_Page;
      --  A remembered hole first (single pages need no alignment), then
      --  the search.
      if Alignment = 1 then
         Take_Hole (Heap, Count, First, Remainder);
         pragma Assert (Heap.Owners = Before.Owners and then Heap.Lengths = Before.Lengths);
         Prove_Same_Owners (Heap, Before, Count, Alignment);
         if First /= No_Page then
            pragma Assert (Free_Run (Before, First, Count) and then (First - 1) mod Alignment = 0);
            pragma Assert (Can_Allocate (Before, Count, Alignment));
         end if;
      end if;
      if First = No_Page then
         First := Find (Heap, Count, Alignment);
         Remainder := 0;
      end if;
      if First /= No_Page then
         Take (Heap, First, Count, Remainder);
      end if;
   end Allocate;

   procedure Release (Heap : in out State; First : Page_Id; Success : out Boolean) is
      Before : constant State := Heap with Ghost;
      Count : constant Page_Reference := Heap.Lengths (First);
   begin
      Success := Count > 0 and then not Heap.Parks (First);
      if not Success then return; end if;
      for P in First .. First + Count - 1 loop
         Heap.Owners (P) := No_Page;
         pragma Loop_Invariant
           (for all Q in Page_Id => Heap.Owners (Q) =
              (if Q in First .. P then No_Page else Before.Owners (Q)));
      end loop;
      Heap.Lengths (First) := 0;
      if First < Heap.Rover then
         Heap.Rover := First;
      end if;
      --  Merge with the free neighbours the spans name (hints, checked
      --  only where they end: a stale span makes a bad hint, which a later
      --  allocation drops after checking the map).
      declare
         Start : Page_Id := First;
         Pages : Run_Length := Count;
         Left, Right : Page_Reference;
      begin
         if First > 1 and then Heap.Owners (First - 1) = No_Page then
            Left := Heap.Spans (First - 1);
            if Left > 0 and then Left < First and then Pages <= Page_Count - Left then
               Start := First - Left;
               Pages := Pages + Left;
            end if;
         end if;
         if First + Count <= Page_Count and then Heap.Owners (First + Count) = No_Page then
            Right := Heap.Spans (First + Count);
            if Right > 0 and then Pages <= Page_Count - Right and then Fits (Start, Pages + Right) then
               Pages := Pages + Right;
            end if;
         end if;
         Note_Hole (Heap, Start, Pages);
      end;
   end Release;

   procedure Grow (Heap : in out State; First : Page_Id; Count : Run_Length; Success : out Boolean) is
      Before : constant State := Heap with Ghost;
      Held : constant Page_Reference := Heap.Lengths (First);
   begin
      Success := Held > 0 and then not Heap.Parks (First) and then Count > Held and then Fits (First, Count)
        and then Run_Is_Free (Heap, First + Held, Count - Held);
      if not Success then
         return;
      end if;
      --  The run's own pages are First's already (Valid).
      pragma Assert (for all Q in First .. First + Held - 1 => Heap.Owners (Q) = First);
      for P in First + Held .. First + Count - 1 loop
         Heap.Owners (P) := First;
         pragma Loop_Invariant
           (for all Q in Page_Id => Heap.Owners (Q) =
              (if Q in First + Held .. P then First else Before.Owners (Q)));
      end loop;
      Heap.Lengths (First) := Count;
      pragma Assert (for all Q in First .. First + Count - 1 => Heap.Owners (Q) = First);
   end Grow;

   procedure Park (Heap : in out State; First : Page_Id; Pages : out Page_Reference; Success : out Boolean) is
   begin
      Pages := Heap.Lengths (First);
      Success := Pages > 0 and then not Heap.Parks (First);
      if Success then
         Heap.Parks (First) := True;
      end if;
   end Park;

   procedure Unpark (Heap : in out State; First : Page_Id; Success : out Boolean) is
   begin
      Success := Heap.Lengths (First) > 0 and then Heap.Parks (First);
      if Success then
         Heap.Parks (First) := False;
      end if;
   end Unpark;
end Heap_Extents;
