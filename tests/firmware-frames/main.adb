with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Frames; use Firmware_Frames;

procedure Main is
   Rounding_Cases, Maps_Checked, Blocks_Checked : Natural := 0;
   Seed : Unsigned_32 := 16#F17ECAFE#;
   function Random (Bound : Positive) return Natural is
   begin
      Seed := Seed * 1_664_525 + 1_013_904_223;
      return Natural (Shift_Right (Seed, 8) mod Unsigned_32 (Bound));
   end Random;

   procedure Check_Rounding (Low, High : Byte_Address) is
      Whole : constant Span := Whole_Pages (Low, High);
      Touched : constant Span := Touched_Pages (Low, High);
      Start : constant Frame := Low / Page_Bytes;
      Finish : constant Frame := High / Page_Bytes;
   begin
      for F in Frame'Max (0, Start - Count'Min (Start, 1)) ..
        Frame'Min (Frame'Last, Finish + 1)
      loop
         pragma Assert
           (Contains (Whole, F, F) =
             (F * Page_Bytes >= Low and then (F + 1) * Page_Bytes - 1 <= High));
         pragma Assert
           (Contains (Touched, F, F) =
             (F * Page_Bytes <= High and then (F + 1) * Page_Bytes - 1 >= Low));
      end loop;
      Rounding_Cases := Rounding_Cases + 1;
   end Check_Rounding;

   type Raw_Region is record
      Present : Boolean := False;
      Kind : Region_Kind := Reserved;
      Low, High : Byte_Address := 0;
   end record;
   -- Nonzero lower bound exercises owner/index handling independent of PFNs.
   subtype Region_Index is Natural range 5 .. 9;
   type Raw_Map is array (Region_Index) of Raw_Region;
   Raw : Raw_Map;
   Map : Region_Array (Region_Index);
   subtype Test_Frame is Frame range 0 .. 31;
   type Owner_Map is array (Test_Frame) of Natural;
   Expected : Owner_Map;
   Actual : Owner_Map;

   procedure Check_Map is
      Owner, Candidate_Owner : Natural;
      Blocked : Boolean;
   begin
      Map := [others => <>];
      for I in Region_Index loop
         if Raw (I).Present then
            Map (I).Kind := Raw (I).Kind;
            Map (I).Pages := (if Raw (I).Kind = Usable then
              Whole_Pages (Raw (I).Low, Raw (I).High)
              else Touched_Pages (Raw (I).Low, Raw (I).High));
         end if;
      end loop;
      -- Oracle works directly in bytes, not the normalized span predicates.
      for F in Test_Frame loop
         Owner := 0;
         Blocked := False;
         for I in Region_Index loop
            if Raw (I).Present then
               if Raw (I).Kind = Reserved then
                  if Raw (I).Low <= (F + 1) * Page_Bytes - 1 and then
                     Raw (I).High >= F * Page_Bytes
                  then
                     Blocked := True;
                  end if;
               elsif Owner = 0 and then Raw (I).Low <= F * Page_Bytes and then
                 Raw (I).High >= (F + 1) * Page_Bytes - 1
               then
                  Owner := I;
               end if;
            end if;
         end loop;
         Expected (F) := (if Blocked then 0 else Owner);
      end loop;
      -- Check every candidate interval and decision, not just tiled choices.
      for I in Region_Index loop
         if Map (I).Kind = Usable then
            for Low in Test_Frame loop
               for High in Low .. Test_Frame'Last loop
                  if Contains (Map (I).Pages, Low, High) then
                     declare
                        D : constant Decision := Classify (Map, I, Low, High);
                        All_Owned : Boolean := True;
                     begin
                        for F in Low .. High loop
                           All_Owned := All_Owned and Expected (F) = I;
                        end loop;
                        pragma Assert ((D = Admit) = All_Owned);
                        if D = Reject then
                           for F in Low .. High loop
                              pragma Assert (Expected (F) /= I);
                           end loop;
                        end if;
                        Blocks_Checked := Blocks_Checked + 1;
                     end;
                  end if;
               end loop;
            end loop;
         end if;
      end loop;

      -- Exercise the production setup traversal against the independent oracle.
      -- Handoff filtering is separately covered by boot-frame-allocator tests.
      Actual := [others => 0];
      for I in Region_Index loop
         if Map (I).Kind = Usable then
            declare
               Cursor : Boundary := First (Map (I).Pages);
               Stop : constant Boundary := Limit (Map (I).Pages);
            begin
               while Cursor < Stop loop
                  declare
                     Order : Block_Order := Largest_Block (Cursor, Stop - 1, 5);
                     Pages : Count := Block_Pages (Order);
                     D : Decision;
                  begin
                     loop
                        D := Classify (Map, I, Cursor, Cursor + Pages - 1);
                        exit when D /= Split;
                        pragma Assert (Order > 0);
                        Order := Order - 1;
                        Pages := Pages / 2;
                     end loop;
                     pragma Assert (Pages > 0 and Cursor mod Pages = 0);
                     if D = Admit then
                        for F in Cursor .. Cursor + Pages - 1 loop
                           pragma Assert (Actual (F) = 0);
                           Actual (F) := I;
                        end loop;
                     end if;
                     Cursor := Cursor + Pages;
                  end;
               end loop;
            end;
         end if;
      end loop;
      pragma Assert (Actual = Expected);
      -- Independently check the per-frame boot-admission path, too.
      for F in Test_Frame loop
         Candidate_Owner := 0;
         for I in Region_Index loop
            if Map (I).Kind = Usable and then Contains (Map (I).Pages, F, F)
              and then Classify (Map, I, F, F) = Admit
            then
               pragma Assert (Candidate_Owner = 0);
               Candidate_Owner := I;
            end if;
         end loop;
         pragma Assert (Candidate_Owner = Expected (F));
      end loop;
      Maps_Checked := Maps_Checked + 1;
   end Check_Map;

   type Length_Array is array (Positive range <>) of Count;
   Lengths : constant Length_Array := [1, 2, 4095, 4096, 4097, 8192, 8193];
begin
   for Offset in Count range 0 .. Page_Bytes - 1 loop
      for Length of Lengths loop
         Check_Rounding (Offset, Offset + Length - 1);
         Check_Rounding (Byte_Address'Last - 4 * Page_Bytes + Offset,
           Byte_Address'Last - 4 * Page_Bytes + Offset + Length - 1);
      end loop;
   end loop;
   Check_Rounding (Byte_Address'Last, Byte_Address'Last);
   Check_Rounding (0, 0);

   -- Exact aligned arena, duplicate coverage, one-byte reservations on either
   -- side of a page boundary, and exclusions appearing before/after usable RAM.
   Raw := [others => <>];
   Raw (5) := (True, Usable, 0, 32 * Page_Bytes - 1);
   Check_Map;
   Raw (7) := Raw (5);
   Check_Map;
   for Offset in Count range 0 .. 2 loop
      Raw (6) := (True, Reserved, Page_Bytes - 1 + Offset, Page_Bytes - 1 + Offset);
      Check_Map;
      Raw (9) := Raw (6);
      Raw (6) := (others => <>);
      Check_Map;
   end loop;
   for Trial in 1 .. 2_000 loop
      Raw := [others => <>];
      for I in Region_Index loop
         declare
            Low : constant Count := Count (Random (32 * 4096));
            High : constant Count := Low + Count (Random (Positive (32 * Page_Bytes - Low)));
         begin
            Raw (I) := (True, (if Random (4) = 0 then Reserved else Usable), Low, High);
         end;
      end loop;
      Check_Map;
   end loop;

   -- Full-width block arithmetic without allocating/dereferencing physical RAM.
   for Maximum in Block_Order loop
      declare
         Pages : constant Count := Block_Pages (Maximum);
         O : constant Block_Order := Largest_Block
           (Boundary'Last - Pages, Frame'Last, Maximum);
      begin
         pragma Assert (O = Maximum);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS firmware admission:" & Rounding_Cases'Image & " byte ranges," &
     Maps_Checked'Image & " maps," & Blocks_Checked'Image & " candidate blocks");
end Main;
