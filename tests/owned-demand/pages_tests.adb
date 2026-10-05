with Ada.Text_IO; use Ada.Text_IO;
with Owned_Demand_Policy; use Owned_Demand_Policy;
with Owned_Demand_Pages; use Owned_Demand_Pages;
procedure Pages_Tests is
   Pages : Map;
   Seen : array (Page_Index) of Boolean := (others => False);
   Expected_Mode : array (Page_Index) of Owned_Demand_Policy.Access_Mode;
   type Sizes is array (Positive range <>) of Page_Count;
   Counts : constant Sizes := (0, 1, 2, 17, 4096);
   Index : Page_Index;
   Rank : Page_Count;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   Check (Map'Size <= 4096 * 8);
   for Count of Counts loop
      Initialize (Pages, Count, Guard);
      Check (Length (Pages) = Count and Resident_Count (Pages) = 0);
      for I in Page_Index loop
         Check (Valid_Index (Pages, I) = (I < Count));
         if I < Count then
            Check (Mode (Pages, I) = Guard and not Resident (Pages, I));
         end if;
         for Span of Counts loop
            Check (Valid_Range (Pages, I, Span) =
              (Span > 0 and I < Count and I + Span <= Count));
         end loop;
      end loop;
   end loop;
   for Order in 0 .. 2 loop
      Initialize (Pages, Maximum_Pages, Read_Write);
      Seen := (others => False);
      Expected_Mode := (others => Read_Write);
      for Step in Page_Index loop
         Index := (case Order is
           when 0 => Step,
           when 1 => Maximum_Pages - 1 - Step,
           when others => (Step * 4051 + 123) mod Maximum_Pages);
         Rank := 0;
         for I in Index + 1 .. Page_Index'Last loop
            if Seen (I) then Rank := Rank + 1; end if;
         end loop;
         Check (Preceding (Pages, Index) = Rank);
         Check (not Resident (Pages, Index));
         Commit (Pages, Index);
         Seen (Index) := True;
         Check (Resident_Count (Pages) = Step + 1);
         -- Changing permission must neither discard backing nor alter rank.
         Set_Mode (Pages, Index, 1, Guard);
         Expected_Mode (Index) := Guard;
         Check (Resident (Pages, Index) and Mode (Pages, Index) = Guard);
         Check (Preceding (Pages, Index) = Rank);
      end loop;
      Set_Mode (Pages, 1, Maximum_Pages - 2, Read_Only);
      for I in 1 .. Maximum_Pages - 2 loop Expected_Mode (I) := Read_Only; end loop;
      for I in Page_Index loop
         Check (Resident (Pages, I));
         Check (Mode (Pages, I) = Expected_Mode (I));
         Check (Preceding (Pages, I) = Maximum_Pages - 1 - I);
      end loop;
      Check (Resident_Count (Pages) = Maximum_Pages);
      -- Reuse erases every bit, including the unused tail of a smaller map.
      Initialize (Pages, 1, Read_Write);
      Check (not Resident (Pages, 0) and Preceding (Pages, 0) = 0);
   end loop;
   Initialize (Pages, Maximum_Pages, Read_Write);
   for I in Page_Index loop
      if I mod 2 = 0 then Commit (Pages, I); end if;
   end loop;
   Set_Mode (Pages, 17, 2053, Guard);
   for I in Page_Index loop
      Check (Resident (Pages, I) = (I mod 2 = 0));
      Check (Mode (Pages, I) =
        (if I >= 17 and I < 2070 then Guard else Read_Write));
   end loop;
   -- Model repeated release/recommit, including guard/read-only permissions.
   -- Use independent residency/rank accounting after every removal.
   for Order in 0 .. 2 loop
      Initialize (Pages, Maximum_Pages, Read_Write);
      Seen := (others => True);
      Expected_Mode := (others => Read_Write);
      for I in Page_Index loop Commit (Pages, I); end loop;
      Set_Mode (Pages, 0, 17, Guard);
      Set_Mode (Pages, 17, 2053, Read_Only);
      for I in Page_Index loop
         Expected_Mode (I) :=
           (if I < 17 then Guard elsif I < 2070 then Read_Only else Read_Write);
      end loop;
      for Step in Page_Index loop
         Index := (case Order is
           when 0 => Step,
           when 1 => Maximum_Pages - 1 - Step,
           when others => (Step * 4051 + 123) mod Maximum_Pages);
         Discard (Pages, Index);
         Seen (Index) := False;
         Check (Resident_Count (Pages) = Maximum_Pages - Step - 1);
         Rank := 0;
         for I in reverse Page_Index loop
            Check (Resident (Pages, I) = Seen (I));
            Check (Mode (Pages, I) = Expected_Mode (I));
            -- Check one independently counted insertion rank each step.
            if I > Index and Seen (I) then Rank := Rank + 1; end if;
         end loop;
         Check (Preceding (Pages, Index) = Rank);
         -- Recommit then discard again: backing can be reused, permission stays.
         Commit (Pages, Index);
         Check (Resident_Count (Pages) = Maximum_Pages - Step);
         Check (Mode (Pages, Index) = Expected_Mode (Index));
         Discard (Pages, Index);
         Check (Resident_Count (Pages) = Maximum_Pages - Step - 1);
      end loop;
      Check (Resident_Count (Pages) = 0);
   end loop;
   Put_Line ("PASS demand metadata" & Checks'Image & " checks; bytes" &
               Integer'Image (Map'Size / 8));
end Pages_Tests;
