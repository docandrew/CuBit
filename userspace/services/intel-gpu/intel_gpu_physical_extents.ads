with Interfaces; use Interfaces;
package Intel_GPU_Physical_Extents with SPARK_Mode, Pure is
   -- Device-visible backing addresses, not CPU or GPU virtual addresses.
   -- Admission establishes geometry only: the caller must authenticate each
   -- allocation and retain it until every CPU/GPU user has finished. A future
   -- IOMMU adapter supplies DMA addresses here instead of assuming DMA=physical.
   Allocation_Order : constant := 9;
   Block_Bytes : constant Unsigned_64 := 4096 * 2 ** Allocation_Order;
   subtype Block_Index is Natural range 0 .. 15;
   type Addresses is array (Block_Index) of Unsigned_64;
   Capacity : constant Unsigned_64 := Unsigned_64 (Addresses'Length) * Block_Bytes;
   type Map is private;
   function Ready (Object : Map) return Boolean;
   function Committed_Bytes (Object : Map) return Unsigned_64;
   -- Snapshots may grow, but all previously committed addresses must agree.
   -- Identity/authority is checked separately by the owning allocation service.
   function Compatible (Left, Right : Map) return Boolean;
   procedure Admit (Bases : Addresses; Object : out Map; Success : out Boolean;
                    Committed_Blocks : Natural := Addresses'Length)
     with Post => Ready (Object) = Success;
   type Span is record
      Valid : Boolean := False;
      Address, Bytes : Unsigned_64 := 0;
   end record;
   -- Resolve only the physically contiguous prefix, NEVER the whole requested
   -- range across a block boundary. Callers iterate to populate 4KiB PTEs or
   -- supported/aligned larger mappings. This operation publishes no mapping.
   function Resolve (Object : Map; Offset, Bytes : Unsigned_64) return Span
     with Post =>
       Resolve'Result.Valid =
         (Ready (Object) and then Bytes /= 0 and then Offset < Committed_Bytes (Object)
          and then Bytes <= Committed_Bytes (Object) - Offset)
       and then
         (if Resolve'Result.Valid then
            Resolve'Result.Bytes > 0 and then
            Resolve'Result.Bytes = Unsigned_64'Min
              (Bytes, Block_Bytes - Offset mod Block_Bytes)
          else Resolve'Result.Bytes = 0 and Resolve'Result.Address = 0);
private
   type Map is record
      Accepted : Boolean := False;
      Count : Natural range 0 .. Addresses'Length := 0;
      Bases : Addresses := [others => 0];
   end record;
end Intel_GPU_Physical_Extents;
