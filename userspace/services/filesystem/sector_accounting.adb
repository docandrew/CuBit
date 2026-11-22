package body Sector_Accounting with SPARK_Mode is
   procedure Plan_Removal
     (Current : Unsigned_32; Sectors : Block_Sectors; Blocks : Retired_Blocks;
      Updated : out Unsigned_32; Fits : out Boolean)
   is
      Removed : constant Unsigned_32 := Sectors * Unsigned_32 (Blocks);
   begin
      Fits := Removed <= Current;
      Updated := (if Fits then Current - Removed else Current);
   end Plan_Removal;

   procedure Plan_Addition
     (Current : Unsigned_32; Sectors : Block_Sectors; Blocks : Attached_Blocks;
      Updated : out Unsigned_32; Fits : out Boolean)
   is
      Total : constant Unsigned_64 := Unsigned_64 (Current) +
        Unsigned_64 (Sectors) * Unsigned_64 (Blocks);
   begin
      Fits := Total <= Unsigned_64 (Unsigned_32'Last);
      Updated := (if Fits then Unsigned_32 (Total) else Current);
   end Plan_Addition;
end Sector_Accounting;
