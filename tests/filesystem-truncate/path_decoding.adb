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
         Limit : constant Unsigned_64 := 12 + Pointers + Pointers * Pointers;
      begin
         for Logical in Unsigned_64 range 0 .. Limit loop
            declare
               Path : constant Block_Path := Decode (Logical, Sectors);
            begin
               if Logical < 12 then
                  pragma Assert (Path.Kind = Direct and then
                    Unsigned_64 (Path.Direct_Slot) = Logical);
               elsif Logical < 12 + Pointers then
                  pragma Assert (Path.Kind = Single_Indirect and then
                    Unsigned_64 (Path.Single_Slot) = Logical - 12);
               elsif Logical < Limit then
                  pragma Assert (Path.Kind = Double_Indirect and then
                    Unsigned_64 (Path.Root_Slot) = (Logical - 12 - Pointers) / Pointers and then
                    Unsigned_64 (Path.Leaf_Slot) = (Logical - 12 - Pointers) mod Pointers);
               else
                  pragma Assert (Path.Kind = Unsupported);
               end if;
               Cases := Cases + 1;
            end;
         end loop;
         pragma Assert (Decode (Unsigned_64'Last, Sectors).Kind = Unsupported);
         pragma Assert (Decode (2 ** 32, Sectors).Kind = Unsupported);
         pragma Assert (Decode (2 ** 63, Sectors).Kind = Unsupported);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("BLOCK-PATH-CHECK: PASS" & Cases'Image & " exhaustive paths");
end Path_Decoding;
