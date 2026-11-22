pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.CuAlloc_Pool;
with Check_Results; use Check_Results;
with Events; use Events;
with Pooled;
with Recording_Pools; use Recording_Pools;
with Shapes; use Shapes;

package body Pool_Checks is

   use type System.Address;

   function Usable_Size (Item : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_usable_size";

   function Usable (A : System.Address) return Unsigned_64 is
     (Usable_Size (Unsigned_64 (To_Integer (A))));

   Recorder : Recording_Pool;

   type Record_Access is access Shapes.Square with Storage_Pool => Recorder;
   type Text_Access is access String with Storage_Pool => Recorder;
   type Class_Access is access Shapes.Shape'Class with Storage_Pool => Recorder;
   type Named_Access is access Shapes.Named'Class with Storage_Pool => Recorder;
   type Tracked_Access is access Tracked with Storage_Pool => Recorder;

   procedure Free is new Ada.Unchecked_Deallocation (Shapes.Square, Record_Access);
   procedure Free is new Ada.Unchecked_Deallocation (String, Text_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Shapes.Shape'Class, Class_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Shapes.Named'Class, Named_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Tracked, Tracked_Access);

   --  Two Integer bounds before a String's characters; the finalization
   --  collection's node (four pointers) before a controlled object.
   Bounds_Bytes : constant := 2 * Integer'Size / System.Storage_Unit;
   Text_Length : constant := 17;
   Finalization_Header_Bytes : constant := 4 * System.Word_Size / System.Storage_Unit;
   subtype Five_Points is Polygon (5);

   procedure Exact_Sizes is
      R : Record_Access := new Square'(Id => 1, Side => 2);
      Record_Size : constant Storage_Count := Recorder.Last_Size;
      T : Text_Access := new String'("seventeen letters");
      Text_Size : constant Storage_Count := Recorder.Last_Size;
      C : Class_Access := new Polygon'(Points => 5, Id => 2, Lengths => [others => 1], Scale => 1);
      Class_Size : constant Storage_Count := Recorder.Last_Size;
      N : Named_Access := new Polygon'(Points => 7, Id => 3, Lengths => [others => 1], Scale => 1);
      K : Tracked_Access := new Tracked;
      Controlled_Size : constant Storage_Count := Recorder.Last_Size;
   begin
      Note ("pool sizes: record" & Record_Size'Image & " (Max_Size_In_Storage_Elements"
            & Storage_Count'(Square'Max_Size_In_Storage_Elements)'Image & ", Size" & Natural (Square'Size)'Image & " bits) string"
            & Text_Size'Image & " class-wide" & Class_Size'Image & " controlled" & Controlled_Size'Image);
      Check (Record_Size = Square'Max_Size_In_Storage_Elements, "Allocate size of a tagged record");
      Check (Text_Size >= Storage_Count (Bounds_Bytes + Text_Length),
             "Allocate size of a String includes its bounds");
      Check (Class_Size = Five_Points'Max_Size_In_Storage_Elements, "Allocate size of a class-wide discriminated object");
      Check (Controlled_Size = Tracked'Size / System.Storage_Unit + Finalization_Header_Bytes
               and then Controlled_Size = Tracked'Max_Size_In_Storage_Elements,
             "Allocate size of a controlled object includes the collection header");
      Check (C.Area = 5 and then N.Name_Length = 7 and then T.all = "seventeen letters",
             "objects in a user-defined pool");
      Free (R);
      Free (T);
      Free (C);
      Free (N);
      Clear;
      Free (K);
      Check (Trace = "F0", "controlled object in a user pool finalized before Deallocate");
      Check (Recorder.Allocations = 5 and then Recorder.Frees = 5 and then Recorder.Exact_Frees = 5
               and then (for all B of Recorder.Live => B.Address = System.Null_Address),
             "Deallocate receives exact address, size and alignment (" & Recorder.Exact_Frees'Image
             & " of" & Recorder.Frees'Image & ")");
   end Exact_Sizes;

   procedure Default_Pool is
      use type Pooled.Integer_Access, Pooled.String_Access, Pooled.Shape_Access;
      procedure Free is new Ada.Unchecked_Deallocation (Integer, Pooled.Integer_Access);
      procedure Free is new Ada.Unchecked_Deallocation (String, Pooled.String_Access);
      procedure Free is new Ada.Unchecked_Deallocation (Shapes.Shape'Class, Pooled.Shape_Access);
      I : Pooled.Integer_Access := new Integer'(42);
      S : Pooled.String_Access := new String'(1 .. 300 => 'x');
      C : Pooled.Shape_Access := new Cube'(Id => 1, Side => 2, Depth => 3);
   begin
      Check (Pooled.Integer_Access'Storage_Pool'Address = CuBit.CuAlloc_Pool.Pool'Address
               and then Pooled.Shape_Access'Storage_Pool'Address = CuBit.CuAlloc_Pool.Pool'Address,
             "pragma Default_Storage_Pool (CuBit.CuAlloc_Pool.Pool) in a package spec");
      Check (I.all = 42 and then S'Length = 300 and then C.Area = 12
               and then Usable (I.all'Address) >= 4 and then Usable (C.all'Address) > 0,
             "CuAlloc blocks behind the default pool");
      Free (I);
      Free (S);
      Free (C);
      Check (I = null and then S = null and then C = null, "sized frees through the default pool");
   end Default_Pool;

   procedure Run is
   begin
      Exact_Sizes;
      Default_Pool;
   end Run;

end Pool_Checks;
