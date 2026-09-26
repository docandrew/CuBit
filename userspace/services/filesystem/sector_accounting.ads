with Interfaces; use Interfaces;

--  Ext2 counts allocated storage in 512-byte units, including pointer blocks.
--  Plan before allocation; publish only with the corresponding inode mapping.
package Sector_Accounting with Pure, SPARK_Mode is
   subtype Block_Sectors is Unsigned_32 range 2 .. 8
     with Static_Predicate => Block_Sectors in 2 | 4 | 8;
   subtype Attached_Blocks is Positive range 1 .. 3;
   subtype Retired_Blocks is Natural range 0 .. 12 + 1024 + 1;

   procedure Plan_Removal
     (Current : Unsigned_32; Sectors : Block_Sectors; Blocks : Retired_Blocks;
      Updated : out Unsigned_32; Fits : out Boolean)
     with Post =>
       Fits = (Unsigned_64 (Sectors) * Unsigned_64 (Blocks) <=
                 Unsigned_64 (Current)) and then
       (if Fits then
          Unsigned_64 (Updated) + Unsigned_64 (Sectors) * Unsigned_64 (Blocks) =
            Unsigned_64 (Current)
        else Updated = Current);

   procedure Plan_Addition
     (Current : Unsigned_32; Sectors : Block_Sectors; Blocks : Attached_Blocks;
      Updated : out Unsigned_32; Fits : out Boolean)
     with Post =>
       Fits = (Unsigned_64 (Current) + Unsigned_64 (Sectors) * Unsigned_64 (Blocks)
                 <= Unsigned_64 (Unsigned_32'Last)) and then
       (if Fits then
          Unsigned_64 (Updated) = Unsigned_64 (Current) +
            Unsigned_64 (Sectors) * Unsigned_64 (Blocks)
        else Updated = Current);
end Sector_Accounting;
