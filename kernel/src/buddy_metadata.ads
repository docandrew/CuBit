pragma Ada_2022;
with Buddy_Geometry;
with Buddy_Blocks;
with Interfaces;
with System;

-- Byte layout only. Reserving/mapping these bytes and excluding them from
-- allocatable memory remain the physical adapter's responsibility.
package Buddy_Metadata with SPARK_Mode, Pure is
   subtype Frame is Buddy_Geometry.Frame;
   use type Buddy_Geometry.Count;
   use type Interfaces.Unsigned_64;
   type Byte_Count is range 0 .. 2 ** 44;
   subtype Table_Size is Byte_Count range 1 .. 2 ** 41;
   subtype Page_Size is Byte_Count range 1 .. 2 ** 30;
   subtype Entry_Size is Byte_Count range 1 .. 2;
   type Table_Kind is (Pin_State, Frame_Owners, Block_State);
   Descriptor_Bytes : constant := Buddy_Blocks.Descriptor_Bits / System.Storage_Unit;
   pragma Compile_Time_Error
     (Buddy_Blocks.Descriptor'Size mod System.Storage_Unit /= 0
      or else Buddy_Blocks.Descriptor'Object_Size /= Buddy_Blocks.Descriptor'Size,
      "Buddy descriptor size/stride must agree with byte-indexed metadata");

   function Entry_Bytes (Kind : Table_Kind) return Entry_Size is
     (if Kind = Block_State then Descriptor_Bytes else 1) with Inline_Always;
   function Bytes (Highest : Frame; Kind : Table_Kind) return Table_Size is
     ((Byte_Count (Highest) + 1) * Entry_Bytes (Kind)) with Inline_Always;
   function Offset (Item : Frame; Kind : Table_Kind) return Byte_Count is
     (Byte_Count (Item) * Entry_Bytes (Kind)) with Inline_Always;
   function Pages (Highest : Frame; Kind : Table_Kind; Granule : Page_Size)
     return Table_Size is ((Bytes (Highest, Kind) - 1) / Granule + 1)
     with Inline_Always;

   function Fits_At (Base : Interfaces.Unsigned_64; Highest : Frame;
                     Kind : Table_Kind) return Boolean is
     (Base <= Interfaces.Unsigned_64'Last - Interfaces.Unsigned_64 (Bytes (Highest, Kind) - 1))
     with Ghost;
   function Address_Of (Base : Interfaces.Unsigned_64; Item : Frame;
                        Kind : Table_Kind) return Interfaces.Unsigned_64 is
     (Base + Interfaces.Unsigned_64 (Offset (Item, Kind))) with Inline_Always,
     Pre => Fits_At (Base, Item, Kind),
     Post => Address_Of'Result >= Base and then
       Address_Of'Result <= Interfaces.Unsigned_64'Last -
         Interfaces.Unsigned_64 (Entry_Bytes (Kind) - 1);

   procedure Prove_Slot (Item, Highest : Frame; Kind : Table_Kind) with Ghost,
     Pre => Item <= Highest,
     Post => Offset (Item, Kind) + Entry_Bytes (Kind) <= Bytes (Highest, Kind);
   procedure Prove_Separate (Left, Right : Frame; Kind : Table_Kind) with Ghost,
     Pre => Left < Right,
     Post => Offset (Left, Kind) + Entry_Bytes (Kind) <= Offset (Right, Kind);
   procedure Prove_Block
     (First : Frame; Length : Buddy_Geometry.Frame_Count; Highest : Frame;
      Kind : Table_Kind) with Ghost,
     Pre => Buddy_Geometry.Fits (First, Length, Highest),
     Post => Offset (First, Kind) + Byte_Count (Length) * Entry_Bytes (Kind)
       <= Bytes (Highest, Kind);
   procedure Prove_Page_Coverage
     (Highest : Frame; Kind : Table_Kind; Granule : Page_Size) with Ghost,
     Post => Pages (Highest, Kind, Granule) * Granule >= Bytes (Highest, Kind)
       and then (Pages (Highest, Kind, Granule) - 1) * Granule < Bytes (Highest, Kind);
   procedure Prove_Address_Separation
     (Base : Interfaces.Unsigned_64; Left, Right, Highest : Frame;
      Kind : Table_Kind) with Ghost,
     Pre => Left < Right and then Right <= Highest and then Fits_At (Base, Highest, Kind),
     Post => Fits_At (Base, Left, Kind) and then Fits_At (Base, Right, Kind)
       and then Address_Of (Base, Left, Kind) + Interfaces.Unsigned_64 (Entry_Bytes (Kind) - 1)
         < Address_Of (Base, Right, Kind);
end Buddy_Metadata;
