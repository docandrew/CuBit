with Interfaces; use Interfaces;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Extent_Directory;
package Intel_GPU_Buffer_Reply with SPARK_Mode is
   package Layout renames Intel_GPU_Buffer_Backing;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   type Outcome is (Invalid, Retry, Denied, Granted);
   -- Extent-backed view deliberately has no scalar physical-base accessor.
   -- Admission is geometric; only authenticated retained supervisor replies
   -- may supply Map and Arena_ID at the live transport boundary.
   type Extent_View is private;
   -- Internal borrowed geometry: the limited directory owner and its committed
   -- metadata MUST outlive this view and all slices/BOs made from it. Never
   -- serialize a view or reconstruct one from a client address. Quarantine
   -- invalidates lookups but does not release physical backing.
   function From_Extents
     (Map : Intel_GPU_Extent_Directory.Borrowed_View; Arena_ID, Offset, Bytes : Unsigned_64)
      return Extent_View;
   function Valid (Object : Extent_View) return Boolean;
   function Slice (Parent : Extent_View; Offset, Bytes : Unsigned_64) return Extent_View;
   function Page_Address (Parent : Extent_View; Offset : Unsigned_64) return Unsigned_64;
   function CPU_Address (Object : Extent_View) return Unsigned_64;
   function Byte_Count (Object : Extent_View) return Unsigned_64;
   function Same_Arena (Left, Right : Extent_View) return Boolean;
   function Overlaps_DMA (Object : Extent_View; First, Bytes : Unsigned_64)
     return Boolean;
   type Backing (Ready : Boolean := False) is record
      case Ready is
         when True =>
            CPU_Address, Bytes : Unsigned_64;
            View : Extent_View;
         when False => null;
      end case;
   end record;
   function From_View (View : Extent_View) return Backing;
   -- Logical size follows the committed extent view, not the per-request
   -- Page_Count limit. This does not allocate memory or widen wire requests.
   -- Constructor for genuinely contiguous backing (including the existing
   -- supervisor reply during transport migration). Consumers see only View.
   function From_Linear (DMA, CPU, Bytes, Arena_DMA : Unsigned_64) return Backing;
   function Valid (Object : Backing) return Boolean;
   function Same_Arena (Left, Right : Backing) return Boolean;
   -- Invalid backing/ranges conservatively conflict. Device-visible addresses,
   -- not CPU virtual addresses. This does not establish allocation authority.
   function Overlaps_DMA (Object : Backing; First, Bytes : Unsigned_64)
     return Boolean;
   -- Driver-private subdivision of an already retained allocation. Does not
   -- allocate, grant authority, or track overlapping slices; the serialized
   -- owner must assign disjoint slices and retain the complete parent.
   function Slice (Parent : Backing; Offset, Bytes : Unsigned_64) return Backing;
   -- Resolve one complete aligned 4 KiB page; zero means invalid. Mappers
   -- must use this boundary instead of assuming physical base + offset.
   function Page_Address (Parent : Backing; Offset : Unsigned_64)
     return Unsigned_64;
   function Valid
     (Pages : Layout.Page_Count; DMA, CPU, Bytes : Unsigned_64) return Boolean is
     (Bytes = Unsigned_64 (Pages) * 4096 and then
      Bytes <= Layout.Capacity and then
      CPU >= Layout.CPU_Base and then CPU mod 4096 = 0 and then
      CPU - Layout.CPU_Base <= Layout.Capacity - Bytes and then
      DMA >= CPU - Layout.CPU_Base and then
      Layout.Valid_Physical (DMA - (CPU - Layout.CPU_Base)))
     with Global => null,
       Post => (if Valid'Result then
         Bytes > 0 and then Bytes <= Layout.Capacity and then
         DMA /= 0 and then DMA mod 4096 = 0 and then DMA <= 2 ** 32 - Bytes and then
         CPU >= Layout.CPU_Base and then CPU mod 4096 = 0 and then
         CPU <= Layout.CPU_Base + Layout.Capacity - Bytes);
   -- Transport owner verifies token/status/endpoint lifetime first. These
   -- checks admit numbers only, not actual mappings or allocation authority.
   -- Caller must also require Same_Arena across its pool and
   -- reject overlap with earlier slots before writing or exporting backing.
   function Classify
     (Index : Layout.Slot; Pages : Layout.Page_Count;
      Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Data : Words) return Outcome
     with Post => (if Classify'Result = Granted then
       Valid (Pages, Data (0), Data (1), Data (2)) and
       Data (3) = Unsigned_64 (Index));
   function Decode
     (Index : Layout.Slot; Pages : Layout.Page_Count;
      Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Data : Words) return Backing
     with Post => (if Decode'Result.Ready then Valid (Decode'Result));
private
   type Extent_View is record
      Accepted : Boolean := False;
      Identity, First, Length : Unsigned_64 := 0;
      Map : Intel_GPU_Extent_Directory.Borrowed_View;
      Linear_Base : Unsigned_64 := 0;
   end record;
end Intel_GPU_Buffer_Reply;
