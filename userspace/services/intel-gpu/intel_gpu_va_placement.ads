with Interfaces; use Interfaces;
package Intel_GPU_VA_Placement with SPARK_Mode is
   -- Numeric GPU byte addresses only. No physical/DMA/CPU addresses, backing
   -- allocation, binding, ownership, retirement or hardware reads here.
   -- Caller supplies its admitted window, including any required guard holes.
   type Extent is record
      First, Limit : Unsigned_64 := 0;
   end record;
   type Extents is array (Positive range <>) of Extent;
   function Available
     (Window : Extent; Used : Extents; First, Bytes : Unsigned_64)
      return Boolean is
     (First >= Window.First and then First < Window.Limit and then Bytes > 0
      and then Bytes <= Window.Limit - First and then
      (for all Claim of Used => Claim.Limit <= First or else
         First + Bytes <= Claim.First));
   procedure Find
     (Window : Extent; Used : Extents; Bytes, Alignment : Unsigned_64;
      First : out Unsigned_64; Found : out Boolean)
   with Global => null,
     Post => (if Found then Available (Window, Used, First, Bytes) and then
                 Alignment >= 4096 and then First mod Alignment = 0
              else First = 0);
   -- At most4096 retained ranges. Reject malformed/out-of-window ranges,
   -- non-page quantities and non-power-of-two alignment. Unsorted ranges
   -- are accepted. Search does not mutate or reserve: serialize with commit.
end Intel_GPU_VA_Placement;
