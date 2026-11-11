pragma Ada_2022;

package body Platform_Pool_Sets with SPARK_Mode is

   -- Sorted and disjoint members ascend: every member ends before every
   -- later one begins.
   procedure Lemma_Ascending (S : State)
   with Ghost, Global => null,
        Pre  => Sorted_Disjoint (S),
        Post => (for all I in 1 .. S.Member_Total =>
                   (for all J in I + 1 .. S.Member_Total =>
                      S.Members (I).Pages.Last < S.Members (J).Pages.First))
   is
   begin
      for J in 2 .. S.Member_Total loop
         pragma Loop_Invariant
           (for all I in 1 .. J - 1 =>
              (for all K in I + 1 .. J - 1 =>
                 S.Members (I).Pages.Last < S.Members (K).Pages.First));
         pragma Assert (S.Members (J - 1).Pages.Last < S.Members (J).Pages.First);
         pragma Assert
           (for all I in 1 .. J - 1 => S.Members (I).Pages.Last < S.Members (J).Pages.First);
      end loop;
   end Lemma_Ascending;

   function Containing (S : State; Request : Interval) return Member_Count is
   begin
      for I in 1 .. S.Member_Total loop
         if Inside (Request, S.Members (I).Pages) then
            return I;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => not Inside (Request, S.Members (J).Pages));
      end loop;
      return 0;
   end Containing;

   function First_Overlap (S : State; Request : Interval) return Member_Count is
   begin
      for I in 1 .. S.Member_Total loop
         if Overlaps (Request, S.Members (I).Pages) then
            return I;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => not Overlaps (Request, S.Members (J).Pages));
      end loop;
      return 0;
   end First_Overlap;

   procedure Insert (S : in out State; Item : Member; Status : out Insert_Status) is
      --  In place: the kernel's frames are small (no copies of the table).
      Old   : constant Member_Array := S.Members with Ghost;
      Total : constant Member_Count := S.Member_Total;
      Place : Positive;
   begin
      if not Valid (Item.Pages) then
         Status := Invalid_Interval;
         return;
      elsif First_Overlap (S, Item.Pages) /= 0 then
         Status := Overlaps_Member;
         return;
      elsif Total = S.Members_Room then
         Status := Members_Full;
         return;
      end if;

      Lemma_Ascending (S);
      -- Place: the first member beginning after Item ends (Total + 1 when
      -- none does). Members before it end before Item begins.
      Place := Total + 1;
      for I in 1 .. Total loop
         if S.Members (I).Pages.First > Item.Pages.Last then
            Place := I;
            exit;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => Old (J).Pages.First <= Item.Pages.Last);
      end loop;
      pragma Assert (Place <= Total + 1);
      pragma Assert (for all J in 1 .. Place - 1 => Old (J).Pages.First <= Item.Pages.Last);
      pragma Assert (for all J in 1 .. Place - 1 => Old (J).Pages.Last < Item.Pages.First);
      pragma Assert (for all J in Place .. Total => Old (J).Pages.First > Item.Pages.Last);

      -- Shift Place .. Total up by one, from the top.
      for I in reverse Place .. Total loop
         S.Members (I + 1) := S.Members (I);
         pragma Loop_Invariant (for all J in 1 .. I => S.Members (J) = Old (J));
         pragma Loop_Invariant (for all J in I + 1 .. Total + 1 => S.Members (J) = Old (J - 1));
         pragma Loop_Invariant
           (for all J in Total + 2 .. S.Members_Room => S.Members (J) = Old (J));
      end loop;
      S.Members (Place) := Item;
      pragma Assert
        (for all J in 1 .. Total + 1 =>
           S.Members (J) = (if J < Place then Old (J)
                            elsif J = Place then Item
                            else Old (J - 1)));
      S.Member_Total := Total + 1;
      Status := Inserted;

      pragma Assert (for all I in 1 .. S.Member_Total => Valid (S.Members (I).Pages));
      pragma Assert
        (for all I in 1 .. S.Member_Total - 1 =>
           S.Members (I).Pages.Last < S.Members (I + 1).Pages.First);
      pragma Assert (S.Members (Place) = Item);
      pragma Assert
        (for all J in 1 .. Total =>
           S.Members (if J < Place then J else J + 1) = Old (J));
      pragma Assert
        (for all I in 1 .. S.Member_Total =>
           S.Members (I) = Item
           or else S.Members (I) = Old (if I < Place then I else I - 1));
   end Insert;

   function Decide (S : State; Holder : Identity; Request : Interval) return Decision is
      Member : Member_Count;
   begin
      if not Valid (Request) then
         return (Kind => Invalid_Request, Member => 0, Claim => 0);
      end if;
      Member := Containing (S, Request);
      if Member = 0 then
         return (Kind => Outside_Pool, Member => 0, Claim => 0);
      end if;
      for K in 1 .. S.Claim_Total loop
         if S.Claims (K).Claimant /= Holder
           and then Overlaps (S.Claims (K).Pages, Request)
         then
            return (Kind   => (if S.Members (Member).Kind = Chipset_Shared then Admitted
                               else Claimed),
                    Member => Member,
                    Claim  => K);
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. K =>
              S.Claims (J).Claimant = Holder
              or else not Overlaps (S.Claims (J).Pages, Request));
      end loop;
      return (Kind => Admitted, Member => Member, Claim => 0);
   end Decide;

   procedure Add_Mapping
     (S : State; T : in out Mapping_Table; M : Mapping; Status : out Add_Status) is
   begin
      for K in 1 .. T.Total loop
         if T.Items (K) = M then
            Status := Already_Recorded;
            return;
         end if;
      end loop;
      if T.Total = T.Room then
         Status := Table_Full;
         return;
      end if;
      pragma Assert (Inside (M.Pages, S.Members (Containing (S, M.Pages)).Pages));
      pragma Assert (not Conflicts (S, M));
      T.Total := T.Total + 1;
      T.Items (T.Total) := M;
      Status := Added;
   end Add_Mapping;

   procedure Copy (From : Mapping_Table; To : in out Mapping_Table) is
   begin
      for K in 1 .. From.Total loop
         To.Items (K) := From.Items (K);
         pragma Loop_Invariant (for all J in 1 .. K => To.Items (J) = From.Items (J));
      end loop;
      To.Total := From.Total;
   end Copy;

   procedure Take_Conflicting
     (S : State; T : in out Mapping_Table; Found : out Boolean; Taken : out Mapping)
   is
      Last : Mapping_Count;
   begin
      Taken := ((First => 0, Last => 0), No_Identity, 0, Device_BAR);
      for K in 1 .. T.Total loop
         if Conflicts (S, T.Items (K)) then
            Taken := T.Items (K);
            Last := T.Total;
            T.Items (K) := T.Items (Last);
            T.Total := Last - 1;
            Found := True;
            return;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. K => not Conflicts (S, T.Items (J)));
      end loop;
      Found := False;
   end Take_Conflicting;

   procedure Add_Claim
     (S : in out State; Pages : Interval; Claimant : Identity; Status : out Claim_Status) is
   begin
      for K in 1 .. S.Claim_Total loop
         if S.Claims (K) = (Pages, Claimant) then
            Status := Duplicate;
            return;
         end if;
      end loop;
      if First_Overlap (S, Pages) = 0 then
         Status := Outside_Members;
      elsif S.Claim_Total = S.Claims_Room then
         Status := Claims_Full;
      else
         S.Claim_Total := S.Claim_Total + 1;
         S.Claims (S.Claim_Total) := (Pages, Claimant);
         Status := Recorded;
      end if;
   end Add_Claim;

   procedure Release_Claimant (S : in out State; Claimant : Identity) is
      Before : constant Claim_Array := S.Claims with Ghost;
      Total  : constant Claim_Count := S.Claim_Total;
      Kept   : Claim_Count := 0;
   begin
      for R in 1 .. Total loop
         if S.Claims (R).Claimant /= Claimant then
            Kept := Kept + 1;
            S.Claims (Kept) := S.Claims (R);
         end if;
         pragma Loop_Invariant (Kept <= R);
         pragma Loop_Invariant
           (for all K in 1 .. Kept =>
              S.Claims (K).Claimant /= Claimant
              and then (for some J in 1 .. R => S.Claims (K) = Before (J)));
         pragma Loop_Invariant
           (for all J in 1 .. R =>
              (if Before (J).Claimant /= Claimant then
                 (for some K in 1 .. Kept => S.Claims (K) = Before (J))));
         pragma Loop_Invariant
           (for all K in R + 1 .. S.Claims_Room => S.Claims (K) = Before (K));
      end loop;
      pragma Assert (for all K in 1 .. Kept => Valid (S.Claims (K).Pages));
      S.Claim_Total := Kept;
   end Release_Claimant;

   ---------------------------------------------------------------------------
   -- Lemmas
   ---------------------------------------------------------------------------

   procedure Lemma_Outside_Claim
     (S : State; Pages : Interval; Request : Interval)
   is
      I : constant Member_Count := Containing (S, Request);
   begin
      pragma Assert (not Overlaps (Pages, S.Members (I).Pages));
      pragma Assert (Inside (Request, S.Members (I).Pages));
   end Lemma_Outside_Claim;

   procedure Lemma_Fewer_Claims (Before, After : State; T : Mapping_Table) is
   begin
      if not Consistent (Before, T) then
         return;
      end if;
      for M in 1 .. T.Total loop
         pragma Assert (not Conflicts (Before, T.Items (M)));
         pragma Assert
           (T.Items (M).Kind = Chipset_Shared
            or else (for all K in 1 .. After.Claim_Total =>
                       After.Claims (K).Claimant = T.Items (M).Holder
                       or else not Overlaps (After.Claims (K).Pages, T.Items (M).Pages)));
         pragma Assert (not Conflicts (After, T.Items (M)));
         pragma Loop_Invariant
           (for all N in 1 .. M => not Conflicts (After, T.Items (N)));
      end loop;
   end Lemma_Fewer_Claims;

   procedure Lemma_Shared_Claim
     (Before, After : State; T : Mapping_Table; Pages : Interval; Claimant : Identity)
   is
      pragma Unreferenced (Claimant);   -- named in the precondition only
   begin
      for M in 1 .. T.Total loop
         pragma Assert (not Conflicts (Before, T.Items (M)));
         if T.Items (M).Kind = Device_BAR then
            -- It lies inside a Device_BAR member, which the new claim misses.
            pragma Assert
              (for some I in 1 .. Before.Member_Total =>
                 Inside (T.Items (M).Pages, Before.Members (I).Pages)
                 and then Before.Members (I).Kind = Device_BAR);
            pragma Assert (not Overlaps (Pages, T.Items (M).Pages));
            pragma Assert
              (for all K in 1 .. Before.Claim_Total =>
                 After.Claims (K).Claimant = T.Items (M).Holder
                 or else not Overlaps (After.Claims (K).Pages, T.Items (M).Pages));
         end if;
         pragma Assert (not Conflicts (After, T.Items (M)));
         pragma Loop_Invariant
           (for all N in 1 .. M => not Conflicts (After, T.Items (N)));
      end loop;
   end Lemma_Shared_Claim;

   procedure Lemma_Same_Items (S : State; From, To : Mapping_Table) is
   begin
      null;
   end Lemma_Same_Items;

end Platform_Pool_Sets;
