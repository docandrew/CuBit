with Ada.Text_IO;
with Interfaces; use Interfaces;
with Block_Paths; use Block_Paths;
with Sector_Accounting;

procedure Path_Decoding is
   Cases : Natural := 0;
begin
   for Geometry in 0 .. 2 loop
      declare
         Sectors : constant Sector_Accounting.Block_Sectors := 2 ** (Geometry + 1);
         Pointers : constant Unsigned_64 := 256 * 2 ** Geometry;
         Double_Start : constant Unsigned_64 := 12 + Pointers;
         Triple_Start : constant Unsigned_64 := Double_Start + Pointers * Pointers;
         Limit : constant Unsigned_64 := Triple_Start + Pointers * Pointers * Pointers;

         --  Oracle: triple slots by subtracting whole subtrees, rather than the
         --  implementation's nested quotient/modulus expressions.
         procedure Check (Logical : Unsigned_64) is
            Path : constant Block_Path := Decode (Logical, Sectors);
         begin
            if Logical < 12 then
               pragma Assert (Path.Kind = Direct and then
                 Unsigned_64 (Path.Direct_Slot) = Logical);
            elsif Logical < Double_Start then
               pragma Assert (Path.Kind = Single_Indirect and then
                 Unsigned_64 (Path.Single_Slot) = Logical - 12);
            elsif Logical < Triple_Start then
               pragma Assert (Path.Kind = Double_Indirect and then
                 Unsigned_64 (Path.Root_Slot) = (Logical - Double_Start) / Pointers and then
                 Unsigned_64 (Path.Leaf_Slot) = (Logical - Double_Start) mod Pointers);
            elsif Logical < Limit then
               pragma Assert (Path.Kind = Triple_Indirect);
               declare
                  Rest : constant Unsigned_64 := Logical - Triple_Start;
                  Top : constant Unsigned_64 := Rest / (Pointers * Pointers);
                  Middle : constant Unsigned_64 :=
                    (Rest - Top * Pointers * Pointers) / Pointers;
                  Bottom : constant Unsigned_64 :=
                    Rest - Top * Pointers * Pointers - Middle * Pointers;
               begin
                  pragma Assert (Unsigned_64 (Path.Top_Slot) = Top and then
                    Unsigned_64 (Path.Middle_Slot) = Middle and then
                    Unsigned_64 (Path.Bottom_Slot) = Bottom and then Top < Pointers);
               end;
            else
               pragma Assert (Path.Kind = Unsupported);
            end if;
            Cases := Cases + 1;
         end Check;
      begin
         pragma Assert (First_Double (Sectors) = Double_Start);
         pragma Assert (First_Triple (Sectors) = Triple_Start);
         pragma Assert (Block_Limit (Sectors) = Limit);
         pragma Assert (Limit <= Maximum_Logical_Blocks);
         --  Every direct/single/double path and the first triple leaf.
         for Logical in Unsigned_64 range 0 .. Triple_Start + Pointers loop
            Check (Logical);
         end loop;
         if Geometry = 0 then
            --  1 KiB: the whole triple extent and the first rejected index.
            for Logical in Triple_Start + Pointers + 1 .. Limit loop
               Check (Logical);
            end loop;
         else
            --  Larger geometries: both sides of every triple leaf boundary
            --  (hence every middle and top boundary), plus the final leaf.
            for Leaf in 1 .. Pointers * Pointers - 1 loop
               Check (Triple_Start + Leaf * Pointers - 1);
               Check (Triple_Start + Leaf * Pointers);
            end loop;
            for Logical in Limit - Pointers .. Limit + 1 loop
               Check (Logical);
            end loop;
         end if;
         Check (Unsigned_64'Last);
         Check (2 ** 32);
         Check (2 ** 63);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("BLOCK-PATH-CHECK: PASS" & Cases'Image & " paths");
end Path_Decoding;
