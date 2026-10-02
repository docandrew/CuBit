with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VA_Placement;
procedure VA_Placement_Tests is
   package VA renames Intel_GPU_VA_Placement;
   Page : constant Unsigned_64 := 4096;
   Used : VA.Extents (1 .. 8);
   Count : Natural;
   First, Expected : Unsigned_64;
   Found, Fits, Expected_Found : Boolean;
begin
   -- Independent page occupancy oracle, descending (unsorted) claims.
   for Mask in Unsigned_32 range 0 .. 255 loop
      Count := 0;
      for I in reverse 0 .. 7 loop
         if (Mask and Shift_Left (1, I)) /= 0 then
            Count := Count + 1;
            Used (Count) := (Unsigned_64 (I) * Page, Unsigned_64 (I + 1) * Page);
         end if;
      end loop;
      for Pages in 1 .. 8 loop
         for Power in 0 .. 3 loop
            Expected := 0; Expected_Found := False;
            for Start in 0 .. 8 - Pages loop
               Fits := Start mod (2 ** Power) = 0;
               for I in Start .. Start + Pages - 1 loop
                  Fits := Fits and (Mask and Shift_Left (1, I)) = 0;
               end loop;
               if Fits then
                  Expected := Unsigned_64 (Start) * Page;
                  Expected_Found := True; exit;
               end if;
            end loop;
            VA.Find ((0, 8 * Page), Used (1 .. Count), Unsigned_64 (Pages) * Page,
                     Page * 2 ** Power, First, Found);
            pragma Assert (Found = Expected_Found and First = Expected);
         end loop;
      end loop;
   end loop;
   -- Reservations need not be disjoint: imported exclusions can overlap or
   -- nest. Check both orders, nonzero windows and non-one array bounds against
   -- a separate page bitmap rather than against Available itself.
   declare
      Claims : VA.Extents (7 .. 8);
      Base : constant Unsigned_64 := 16 * Page;
      Occupied : array (0 .. 7) of Boolean;
   begin
      for A in 0 .. 7 loop
         for B in A + 1 .. 8 loop
            for C in 0 .. 7 loop
               for D in C + 1 .. 8 loop
                  Claims := (7 => (Base + Unsigned_64 (A) * Page,
                                    Base + Unsigned_64 (B) * Page),
                             8 => (Base + Unsigned_64 (C) * Page,
                                    Base + Unsigned_64 (D) * Page));
                  for I in Occupied'Range loop
                     Occupied (I) := (I >= A and I < B) or (I >= C and I < D);
                  end loop;
                  for Pages in 1 .. 8 loop
                     for Power in 0 .. 3 loop
                        Expected := 0; Expected_Found := False;
                        for Start in 0 .. 8 - Pages loop
                           Fits := (16 + Start) mod (2 ** Power) = 0;
                           for I in Start .. Start + Pages - 1 loop
                              Fits := Fits and not Occupied (I);
                           end loop;
                           if Fits then
                              Expected := Base + Unsigned_64 (Start) * Page;
                              Expected_Found := True; exit;
                           end if;
                        end loop;
                        VA.Find ((Base, Base + 8 * Page), Claims,
                                 Unsigned_64 (Pages) * Page, Page * 2 ** Power,
                                 First, Found);
                        pragma Assert (Found = Expected_Found and First = Expected);
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end;
   -- Exercise the maximum accepted collision chain and reject one more claim.
   declare
      Claims : VA.Extents (1 .. 4097);
   begin
      for I in Claims'Range loop
         Claims (I) := (Unsigned_64 (I - 1) * Page, Unsigned_64 (I) * Page);
      end loop;
      VA.Find ((0, 4098 * Page), Claims (1 .. 4096), Page, Page, First, Found);
      pragma Assert (Found and First = 4096 * Page);
      VA.Find ((0, 4098 * Page), Claims, Page, Page, First, Found);
      pragma Assert (not Found and First = 0);
   end;
   -- Huge address reservations do not require huge physical allocations.
   VA.Find ((2 ** 40, 2 ** 48), Used (1 .. 0), 2 ** 39, 2 ** 40, First, Found);
   pragma Assert (Found and First = 2 ** 40);
   VA.Find ((2 ** 48 - Page, 2 ** 48), Used (1 .. 0), Page, Page, First, Found);
   pragma Assert (Found and First = 2 ** 48 - Page);
   for Fault in 1 .. 9 loop
      declare
         Window : VA.Extent := (Page, 8 * Page);
         Bytes : Unsigned_64 := Page;
         Alignment : Unsigned_64 := Page;
      begin
         Count := 0;
         case Fault is
            when 1 => Window.Limit := 2 ** 48 + Page;
            when 2 => Window.First := 1;
            when 3 => Window.Limit := Window.First;
            when 4 => Bytes := 0;
            when 5 => Bytes := Unsigned_64'Last;
            when 6 => Alignment := 3 * Page;
            when 7 => Alignment := 0;
            when 8 => Count := 1; Used (1) := (0, Page);
            when 9 => Count := 1; Used (1) := (Page, Page);
         end case;
         VA.Find (Window, Used (1 .. Count), Bytes, Alignment, First, Found);
         pragma Assert (not Found and First = 0);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("GPU VA placement PASS: 8192 occupancy + 41472 overlapping-range cases, maximum chain, sparse high VA and malformed input");
end VA_Placement_Tests;
