with Heap_Bitmap;
with Heap_Classes; use Heap_Classes;
generic
   Slab_Count : Positive;
package Heap_Slabs with SPARK_Mode, Pure is
   Slab_Bytes : constant := 65_536;
   Arena_Bytes : constant Positive := Slab_Count * Slab_Bytes;
   subtype Slab_Id is Positive range 1 .. Slab_Count;
   subtype Offset is Natural range 0 .. Arena_Bytes - 1;
   subtype Local_Offset is Natural range 0 .. Slab_Bytes - 1;
   subtype Slot is Heap_Bitmap.Slot;
   function Slots_For (Class : Size_Class) return Slot is (Quotient (Slab_Bytes, Class));
   function Page_Of (Position : Offset) return Slab_Id with Inline_Always,
     Post => Page_Of'Result = Position / Slab_Bytes + 1;
   function Local_Of (Position : Offset) return Local_Offset with Inline_Always,
     Post => Local_Of'Result = Position mod Slab_Bytes;
   function First_Byte (Page : Slab_Id; Class : Size_Class; Item : Slot) return Offset is
     ((Page - 1) * Slab_Bytes + (Item - 1) * Stride (Class)) with
     Pre => Item <= Slots_For (Class),
     Post => Page_Of (First_Byte'Result) = Page and then
       Local_Of (First_Byte'Result) = (Item - 1) * Stride (Class) and then
       First_Byte'Result mod 16 = 0 and then First_Byte'Result <= Arena_Bytes - Stride (Class);

   type State is private;
   function Valid (Heap : State) return Boolean with Ghost;
   function Class_Of (Heap : State; Page : Slab_Id) return Size_Class;
   function Used (Heap : State; Page : Slab_Id) return Heap_Bitmap.Count;
   function Member (Heap : State; Page : Slab_Id; Item : Slot) return Boolean;
   function Live (Heap : State; Position : Offset) return Boolean;
   function Can_Allocate (Heap : State; Size : Request_Size) return Boolean with Ghost;
   function Preserved (Before, After : State; Page : Slab_Id; Item : Slot) return Boolean with Ghost;

   type Result (Success : Boolean := False) is record
      case Success is
         when True => Position : Offset;
         when False => null;
      end case;
   end record;
   for Result use record
      Success at 0 range 0 .. 7;
      Position at 4 range 0 .. 31;
   end record;
   for Result'Size use 64;
   type Allocation is record Value : Result; end record;

   procedure Initialize (Heap : out State) with
     Post => Valid (Heap) and then (for all P in Slab_Id => Used (Heap, P) = 0);
   procedure Allocate (Heap : in out State; Size : Request_Size; Value : out Allocation) with
     Inline_Always, Pre => Valid (Heap),
     Post => Valid (Heap) and then Value.Value.Success = Can_Allocate (Heap'Old, Size) and then
       (if Value.Value.Success then
          Live (Heap, Value.Value.Position) and then not Live (Heap'Old, Value.Value.Position) and then
          Class_Of (Heap, Page_Of (Value.Value.Position)) = Class_For (Size) and then
          Value.Value.Position mod 16 = 0 and then
          Preserved (Heap'Old, Heap, Page_Of (Value.Value.Position),
                     Local_Of (Value.Value.Position) / Stride (Class_For (Size)) + 1)
        else Heap = Heap'Old);
   type Release_Status is (Released, Not_Allocated, Invalid_Offset);
   procedure Release (Heap : in out State; Position : Offset; Status : out Release_Status) with
     Inline_Always, Pre => Valid (Heap),
     Post => Valid (Heap) and then (Status = Released) = Live (Heap'Old, Position) and then
       (for all P in Slab_Id => Class_Of (Heap, P) = Class_Of (Heap'Old, P)) and then
       (if Status = Released then not Live (Heap, Position) and then
          Preserved (Heap'Old, Heap, Page_Of (Position),
                     Local_Of (Position) / Stride (Class_Of (Heap, Page_Of (Position))) + 1)
        else Heap = Heap'Old);

   procedure Prove_Disjoint (A, B : Slab_Id; AC, BC : Size_Class; AI, BI : Slot) with Ghost,
     Pre => AI <= Slots_For (AC) and then BI <= Slots_For (BC) and then
       (A /= B or else (AC = BC and then AI /= BI)),
     Post => First_Byte (A, AC, AI) + Stride (AC) <= First_Byte (B, BC, BI) or else
             First_Byte (B, BC, BI) + Stride (BC) <= First_Byte (A, AC, AI);
private
   subtype Slab_Reference is Natural range 0 .. Slab_Count;
   No_Slab : constant Slab_Reference := 0;
   type Pools is array (Slab_Id) of Heap_Bitmap.Pool;
   type Classes is array (Slab_Id) of Size_Class;
   type Hints is array (Size_Class) of Slab_Id;
   type State is record
      Pages : Pools;
      Kinds : Classes;
      Current : Hints;
   end record;
   function Valid (Heap : State) return Boolean is
     (for all P in Slab_Id => Heap_Bitmap.Valid (Heap.Pages (P)) and then
        Heap_Bitmap.Capacity (Heap.Pages (P)) = Slots_For (Heap.Kinds (P)));
   function Class_Of (Heap : State; Page : Slab_Id) return Size_Class is (Heap.Kinds (Page));
   function Used (Heap : State; Page : Slab_Id) return Heap_Bitmap.Count is
     (Heap_Bitmap.Used (Heap.Pages (Page)));
   function Member (Heap : State; Page : Slab_Id; Item : Slot) return Boolean is
     (Heap_Bitmap.Live (Heap.Pages (Page), Item));
   function Live (Heap : State; Position : Offset) return Boolean is
     (Local_Of (Position) mod Stride (Heap.Kinds (Page_Of (Position))) = 0 and then
        Member (Heap, Page_Of (Position),
          Local_Of (Position) / Stride (Heap.Kinds (Page_Of (Position))) + 1));
   function Can_Allocate (Heap : State; Size : Request_Size) return Boolean is
     (for some P in Slab_Id => Used (Heap, P) = 0 or else
        (Heap.Kinds (P) = Class_For (Size) and then Used (Heap, P) < Slots_For (Heap.Kinds (P))));
   function Preserved (Before, After : State; Page : Slab_Id; Item : Slot) return Boolean is
     ((for all P in Slab_Id =>
         (if Before.Kinds (P) /= After.Kinds (P) then Used (Before, P) = 0)) and then
      (for all P in Slab_Id =>
         (for all I in Slot => (if P /= Page or else I /= Item then
            Member (Before, P, I) = Member (After, P, I)))));
end Heap_Slabs;
