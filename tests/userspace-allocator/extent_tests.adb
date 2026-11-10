with Ada.Text_IO;
with Heap_Extents;
with Interfaces;
use type Interfaces.Unsigned_32;
procedure Extent_Tests is
   use Heap_Extents;
   Heap : State;
   First, Other : Page_Reference;
   OK : Boolean;
   procedure Check (Condition : Boolean) is
   begin
      if not Condition then raise Program_Error with "extent test failed"; end if;
   end Check;
begin
   Initialize (Heap);
   Allocate (Heap, Page_Count, 1, First);
   Check (First = 1);
   Allocate (Heap, 1, 1, Other);
   Check (Other = No_Page);
   Release (Heap, 2, OK);
   Check (not OK and then Length (Heap, 1) = Page_Count);
   Release (Heap, 1, OK);
   Check (OK);
   Release (Heap, 1, OK);
   Check (not OK);
   -- Every alignment supported by the core, not just powers of two. With
   -- page 1 occupied the first aligned candidate is alignment + 1.
   Allocate (Heap, 1, 1, First);
   Check (First = 1);
   for Alignment in 1 .. Page_Count - 1 loop
      Allocate (Heap, 1, Alignment, Other);
      Check (Other = Alignment + 1);
      Check (Owner (Heap, Other) = Other and then Length (Heap, Other) = 1);
      Release (Heap, Other, OK);
      Check (OK);
   end loop;
   Allocate (Heap, 1, Page_Count, Other);
   Check (Other = No_Page);
   Release (Heap, 1, OK);
   Check (OK);
   Allocate (Heap, Page_Count, Page_Count, First);
   Check (First = 1);
   for P in Page_Id loop Check (Owner (Heap, P) = 1); end loop;
   Release (Heap, 1, OK);
   Check (OK);
   for P in Page_Id loop Check (Owner (Heap, P) = 0 and then Length (Heap, P) = 0); end loop;
   Ada.Text_IO.Put_Line ("PASS extent alignment, exhaustion, interior/double release and reuse");
   -- Remembered holes, the rover, parking (ALLOC-001 step 12): 200,000
   -- random operations against a page model. Every run handed out was free
   -- in the model; Allocate fails only when the model has no fitting run
   -- (hints never hide room); Allocate_Hinted may fail but never hands out
   -- an owned page; a parked run is neither released nor parked again;
   -- Owner/Length agree with the model after every step.
   declare
      Max_Blocks : constant := 64;
      type Block is record
         First : Page_Reference := No_Page;
         Count : Natural := 0;
         Parked : Boolean := False;
      end record;
      Blocks : array (1 .. Max_Blocks) of Block;
      Model : array (Page_Id) of Natural := [others => 0];
      Seed : Interfaces.Unsigned_32 := 16#5EED_0012#;
      function Next (Bound : Positive) return Positive is
      begin
         Seed := Seed * 1_664_525 + 1_013_904_223;
         return Natural (Interfaces.Shift_Right (Seed, 8) mod Interfaces.Unsigned_32 (Bound)) + 1;
      end Next;
      function Model_Fits (Count, Alignment : Positive) return Boolean is
         Run : Natural := 0;
      begin
         for P in Page_Id loop
            if Model (P) = 0 then
               Run := Run + 1;
            else
               Run := 0;
            end if;
            if Run >= Count and then (P - Count) mod Alignment = 0 then
               return True;
            end if;
         end loop;
         return False;
      end Model_Fits;
      K, Count, Alignment : Positive;
      Got : Page_Reference;
      Pages : Page_Reference;
      Done : Boolean;
      Hinted, Searched, Refused, Parks : Natural := 0;
   begin
      Initialize (Heap);
      for Step in 1 .. 200_000 loop
         K := Next (Max_Blocks);
         if Blocks (K).First = No_Page then
            Count := (if Next (8) = 1 then Next (512) else Next (64));
            Alignment := (if Next (16) = 1 then Next (8) else 1);
            if Next (2) = 1 then
               Allocate_Hinted (Heap, Count, Got);
               Hinted := Hinted + (if Got /= No_Page then 1 else 0);
            else
               Allocate (Heap, Count, Alignment, Got);
               Check ((Got /= No_Page) = Model_Fits (Count, Alignment));
               Searched := Searched + (if Got /= No_Page then 1 else 0);
               Refused := Refused + (if Got = No_Page then 1 else 0);
            end if;
            if Got /= No_Page then
               for P in Got .. Got + Count - 1 loop
                  Check (Model (P) = 0);
                  Model (P) := K;
               end loop;
               Check (Length (Heap, Got) = Count and then not Parked (Heap, Got));
               Blocks (K) := (Got, Count, False);
            end if;
         else
            case Next (4) is
               when 1 =>
                  -- Park, then a second park and a release must fail.
                  Park (Heap, Blocks (K).First, Pages, Done);
                  Check (Done = not Blocks (K).Parked);
                  if Done then
                     Parks := Parks + 1;
                     Check (Pages = Blocks (K).Count);
                     Blocks (K).Parked := True;
                  end if;
                  Release (Heap, Blocks (K).First, Done);
                  Check (not Done);
               when 2 =>
                  Unpark (Heap, Blocks (K).First, Done);
                  Check (Done = Blocks (K).Parked);
                  Blocks (K).Parked := False;
               when others =>
                  Release (Heap, Blocks (K).First, Done);
                  Check (Done = not Blocks (K).Parked);
                  if Done then
                     for P in Blocks (K).First .. Blocks (K).First + Blocks (K).Count - 1 loop
                        Model (P) := 0;
                     end loop;
                     Release (Heap, Blocks (K).First, Done);
                     Check (not Done);
                     Blocks (K) := (others => <>);
                  end if;
            end case;
         end if;
         if Step mod 1_000 = 0 then
            for P in Page_Id loop
               Check ((Owner (Heap, P) = No_Page) = (Model (P) = 0));
               if Model (P) /= 0 then
                  Check (Owner (Heap, P) = Blocks (Model (P)).First);
               end if;
            end loop;
         end if;
      end loop;
      Check (Hinted > 1_000 and then Searched > 1_000 and then Refused > 100 and then Parks > 1_000);
      Ada.Text_IO.Put_Line ("PASS extent holes, rover and parking: 200000 random operations against a page model"
                            & " (hinted" & Hinted'Image & ", searched" & Searched'Image & ", refused"
                            & Refused'Image & ", parked" & Parks'Image & ")");
   end;
end Extent_Tests;
