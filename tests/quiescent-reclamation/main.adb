--  Hosted simulation for Quiescent_Reclamation. Readers on each CPU take
--  pointers to live pages and drop them only at that CPU's next quiescent
--  point. Pages are unlinked, retired and freed when their grace period has
--  elapsed. Assert: a page is never freed while any reader still holds it.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Quiescent_Reclamation; use Quiescent_Reclamation;

procedure Main is
   Pages : constant := 32;
   type Page_Id is range 1 .. Pages;
   type Page_State is (Linked, Retired, Freed);

   S : Counters := [others => 0];   --  the adapter's per-CPU counters
   Online : constant CPU_Set := [others => True];
   Page : array (Page_Id) of Page_State := [others => Linked];
   Retired_At : array (Page_Id) of Counters := [others => [others => 0]];
   --  Holds (C, P): CPU C's reader holds a pointer into page P, taken since
   --  C's last quiescent point.
   Holds : array (CPU_Index, Page_Id) of Boolean := [others => [others => False]];

   Seed : Unsigned_64 := 16#DEAD_BEEF_1234_5678#;
   function Next (N : Positive) return Natural is
   begin
      Seed := Seed * 6364136223846793005 + 1442695040888963407;
      return Natural (Shift_Right (Seed, 33) mod Unsigned_64 (N));
   end Next;

   Frees, Retirements, Reads : Natural := 0;
begin
   for Step in 1 .. 500_000 loop
      declare
         C : constant CPU_Index := Next (CPU_Index'Last + 1);
         P : constant Page_Id := Page_Id (Next (Pages) + 1);
      begin
         case Next (4) is
            when 0 =>  --  A reader looks up a page: only linked pages are reachable.
               if Page (P) = Linked then
                  Holds (C, P) := True;
                  Reads := Reads + 1;
               end if;
            when 1 =>  --  Quiescent point: this CPU's reader drops everything.
               for Q in Page_Id loop
                  Holds (C, Q) := False;
               end loop;
               S (C) := Next_Count (S (C));
            when 2 =>  --  Unlink and retire a page (snapshot after unlinking).
               if Page (P) = Linked then
                  Page (P) := Retired;
                  Retired_At (P) := S;
                  Retirements := Retirements + 1;
               end if;
            when others =>  --  Reclaim every page whose grace period elapsed.
               for Q in Page_Id loop
                  if Page (Q) = Retired and then
                     Grace_Elapsed (S, Retired_At (Q), Online)
                  then
                     for R in CPU_Index loop
                        pragma Assert (not Holds (R, Q),
                                       "page freed while a reader holds it");
                     end loop;
                     Page (Q) := Freed;
                     Frees := Frees + 1;
                  end if;
               end loop;
               --  Recycle freed pages into the directory again.
               if Page (P) = Freed then
                  Page (P) := Linked;
               end if;
         end case;
      end;
   end loop;

   --  An offline CPU does not block; an online, idle one does.
   declare
      T : Counters := [others => 0];
      Snap : constant Counters := T;
      Only_Zero : CPU_Set := [others => False];
   begin
      Only_Zero (0) := True;
      pragma Assert (not Grace_Elapsed (T, Snap, Only_Zero));
      T (1) := Next_Count (T (1));
      pragma Assert (not Grace_Elapsed (T, Snap, Only_Zero));
      T (0) := Next_Count (T (0));
      pragma Assert (Grace_Elapsed (T, Snap, Only_Zero));
      --  Saturation: a snapshot at the maximum never elapses.
      T (0) := Count'Last;
      pragma Assert (Next_Count (T (0)) = Count'Last);
      pragma Assert (not Grace_Elapsed (T, T, Only_Zero));
   end;

   Ada.Text_IO.Put_Line
     ("PASS: quiescent reclamation, 500000 steps:" & Reads'Image & " reads," &
      Retirements'Image & " retirements," & Frees'Image &
      " frees, none while held");
end Main;
