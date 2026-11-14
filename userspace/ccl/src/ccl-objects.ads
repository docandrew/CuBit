with Interfaces;
with CCL.Types;

--  Native, pointer-free CCL data objects. This is NOT the disk/network codec.
--  A trusted schema binding supplies meaning; local Type_References never
--  travel in the object. Products/sums use declaration order, depth first.
--
--  A value is as large as its content: a Sized_Image holds Cell_Room cells
--  and Text_Room bytes of text, chosen by what it holds (CCL.Objects.Holders
--  grows one on the heap), up to the named ceilings VALUE_CELL_CEILING and
--  VALUE_TEXT_CEILING. Image is one fixed encoding of a value, the 16 KiB
--  page (Maximum_Cells, Maximum_Text_Bytes) that config's shared-memory
--  channel maps and small interface results are built in; To_Sized copies a
--  page's value out.
package CCL.Objects with SPARK_Mode is
   use Interfaces;
   --  The most one value holds: cells, and bytes of text (CAPACITY).
   VALUE_CELL_CEILING : constant := 2 ** 21;
   VALUE_TEXT_CEILING : constant := 2 ** 25;
   subtype Cell_Room_Count is Natural range 0 .. VALUE_CELL_CEILING;
   subtype Text_Room_Count is Natural range 0 .. VALUE_TEXT_CEILING;
   --  The page encoding's rooms.
   Maximum_Cells : constant := CCL.Types.Maximum_Value_Cells;
   Maximum_Text_Bytes : constant := 8192;
   Native_Image_Bytes : constant := 16_384;
   Header_Bytes : constant := 48;
   Cell_Bytes : constant := 16;
   Text_Offset : constant := Header_Bytes + Maximum_Cells * Cell_Bytes;
   Padding_Offset : constant := Text_Offset + Maximum_Text_Bytes;
   Padding_Length : constant := Native_Image_Bytes - Padding_Offset;
   Format_Version : constant Unsigned_32 := 1;
   type Schema_Key is array (Natural range 0 .. 3) of Unsigned_64
     with Component_Size => 64, Size => 256;
   No_Schema : constant Schema_Key := [others => 0];
   type Cell is record
      First, Second : Unsigned_64 := 0;
   end record with Size => 128;
   for Cell use record
      First at 0 range 0 .. 63;
      Second at 8 range 0 .. 63;
   end record;
   type Cell_Sequence is array (Positive range <>) of Cell with Component_Size => 128;
   subtype Cell_Index is Positive range 1 .. Maximum_Cells;
   subtype Cell_Array is Cell_Sequence (Cell_Index);
   type Padding_Bytes is array (1 .. Padding_Length) of Unsigned_8 with Component_Size => 8;

   --  All fields have valid representations for every bit pattern. Never map
   --  untrusted bytes directly onto Ada enums, Booleans or constrained counts.
   --  Schema words use native byte order, like the rest of this local ABI.
   type Image is record
      Schema : Schema_Key := No_Schema;
      Version : Unsigned_32 := Format_Version;
      Used_Cells, Used_Bytes, Reserved : Unsigned_32 := 0;
      Cells : Cell_Array := [others => <>];
      Text : String (1 .. Maximum_Text_Bytes) := [others => Character'Val (0)];
      Padding : Padding_Bytes := [others => 0];
   end record with Size => Native_Image_Bytes * 8, Alignment => 4096;
   for Image use record
      Schema at 0 range 0 .. 255;
      Version at 32 range 0 .. 31;
      Used_Cells at 36 range 0 .. 31;
      Used_Bytes at 40 range 0 .. 31;
      Reserved at 44 range 0 .. 31;
      Cells at Header_Bytes range 0 .. Maximum_Cells * Cell_Bytes * 8 - 1;
      Text at Text_Offset range 0 .. Maximum_Text_Bytes * 8 - 1;
      Padding at Padding_Offset range 0 .. Padding_Length * 8 - 1;
   end record;

   --  A value of Cell_Room cells and Text_Room bytes of text: the page's
   --  header, cells and packed text without its padding. Its unused cells
   --  and text are zero, as a page's are (Validate).
   type Sized_Image (Cell_Room : Cell_Room_Count; Text_Room : Text_Room_Count) is record
      Schema : Schema_Key := No_Schema;
      Version : Unsigned_32 := Format_Version;
      Used_Cells, Used_Bytes, Reserved : Unsigned_32 := 0;
      Cells : Cell_Sequence (1 .. Cell_Room) := [others => <>];
      Text : String (1 .. Text_Room) := [others => Character'Val (0)];
   end record;
   --  The cells and text a value holds, as rooms (a counter beyond a ceiling
   --  saturates there: no such image validates).
   function Cells_Held (Object : Sized_Image) return Cell_Room_Count is
     (if Object.Used_Cells > VALUE_CELL_CEILING then VALUE_CELL_CEILING else Natural (Object.Used_Cells));
   function Bytes_Held (Object : Sized_Image) return Text_Room_Count is
     (if Object.Used_Bytes > VALUE_TEXT_CEILING then VALUE_TEXT_CEILING else Natural (Object.Used_Bytes));
   function Cells_Held (Object : Image) return Cell_Room_Count is
     (if Object.Used_Cells > Maximum_Cells then Maximum_Cells else Natural (Object.Used_Cells));
   function Bytes_Held (Object : Image) return Text_Room_Count is
     (if Object.Used_Bytes > Maximum_Text_Bytes then Maximum_Text_Bytes else Natural (Object.Used_Bytes));
   --  A page's value in a sized image of exactly its content.
   function To_Sized (Object : Image) return Sized_Image
     with Post => To_Sized'Result.Cell_Room = Cells_Held (Object) and then
                  To_Sized'Result.Text_Room = Bytes_Held (Object);
   --  A sized value in a page, when it fits one (Fits_Page).
   function Fits_Page (Object : Sized_Image) return Boolean is
     (Object.Used_Cells <= Maximum_Cells and then Object.Used_Bytes <= Maximum_Text_Bytes and then
      Natural (Object.Used_Cells) <= Object.Cell_Room and then Natural (Object.Used_Bytes) <= Object.Text_Room);
   function To_Page (Object : Sized_Image) return Image
     with Pre => Fits_Page (Object);

   type Binding is private;
   --  The caller obtains Key + Types + Root from an approved schema binding,
   --  NOT from an untrusted object's claim. A digest is not authority. This
   --  routine neither computes a schema digest nor authenticates its source.
   procedure Bind
     (Types : CCL.Types.Registry; Root : CCL.Types.Type_Reference; Key : Schema_Key;
      Contract : out Binding; Accepted : out Boolean);
   function Is_Bound (Contract : Binding) return Boolean;
   function Root_Type (Contract : Binding) return CCL.Types.Type_Reference;
   function Identity (Contract : Binding) return Schema_Key;
   --  The same approved identity and complete nominal root definition.
   --  Local numbering/unreachable declarations are not part of that identity.
   --  This compares metadata only; it never grants access or authenticates it.
   function Same_Schema (Left, Right : Binding) return Boolean with Global => null;
   function Matches_Type
     (Contract : Binding; Local_Types : CCL.Types.Registry;
      Local_Root : CCL.Types.Type_Reference) return Boolean with Global => null;
   -- Full nominal correspondence to an in-process type, independent of local
   -- numbering. The caller supplies an approved Contract; this grants nothing.
   function Persistable
     (Types : CCL.Types.Registry; Root : CCL.Types.Type_Reference) return Boolean;
   --  Values one evaluation may hold (the value arena): what Persistable
   --  allows, plus range types and recursive types (a list of the owner
   --  itself). Only Persistable values cross a host boundary as images.
   function Storable
     (Types : CCL.Types.Registry; Root : CCL.Types.Type_Reference) return Boolean;

   function Empty (Contract : Binding) return Image;
   --  An empty value of Cell_Room cells and Text_Room bytes.
   function Empty
     (Contract : Binding; Cell_Room : Cell_Room_Count; Text_Room : Text_Room_Count) return Sized_Image
     with Post => Empty'Result.Cell_Room = Cell_Room and then Empty'Result.Text_Room = Text_Room;
   type Build_Result is (Added, Full, Invalid_Image);
   --  Append builds an incomplete object; Validate is required before use.
   --  Full: the object's room is used up (a page's, or a sized image's:
   --  CCL.Objects.Holders then grows it).
   procedure Append (Object : in out Image; Value : Cell; Result : out Build_Result)
     with Post => (if Result /= Added then Object = Object'Old);
   procedure Append_Text
     (Object : in out Image; Value : String; Result : out Build_Result)
     with Post => (if Result /= Added then Object = Object'Old);
   --  (A sized image may be large: the contracts name its counters, not a
   --  copy of the whole object.)
   procedure Append (Object : in out Sized_Image; Value : Cell; Result : out Build_Result)
     with Post => (if Result /= Added then
                     Object.Used_Cells = Object.Used_Cells'Old and then Object.Used_Bytes = Object.Used_Bytes'Old);
   procedure Append_Text
     (Object : in out Sized_Image; Value : String; Result : out Build_Result)
     with Post => (if Result /= Added then
                     Object.Used_Cells = Object.Used_Cells'Old and then Object.Used_Bytes = Object.Used_Bytes'Old);

   function Integer_Cell (Value : Integer_64) return Cell;
   function Boolean_Cell (Value : Boolean) return Cell is
     ((First => (if Value then 1 else 0), Second => 0));
   function Character_Cell (Value : Character) return Cell is
     ((First => Character'Pos (Value), Second => 0));
   function Product_Cell (Fields : CCL.Types.Component_Count) return Cell is
     ((First => Unsigned_64 (Fields), Second => 0));
   function Variant_Cell (Choice : CCL.Types.Component_Index) return Cell is
     ((First => Unsigned_64 (Choice), Second => 0));
   --  A list: its element count, then the elements depth first.
   function Sequence_Cell (Count : Natural) return Cell is
     ((First => Unsigned_64 (Count), Second => 0));
   Unit_Cell : constant Cell := (0, 0);
   function Integer_Of (Value : Cell) return Integer_64;

   --  Requires a stable owned snapshot or kernel-enforced immutable mapping.
   --  Read-only access for the receiver alone does NOT satisfy that condition.
   --  Checks nested types, counts, packed text ranges, exact consumption and
   --  zero unused storage. No recursion, pointer fixups, or object rebuilding.
   function Validate (Object : Image; Contract : Binding) return Boolean;
   --  The same for a sized image: its unused room is zero too.
   function Validate (Object : Sized_Image; Contract : Binding) return Boolean;
private
   type Binding is record
      Types : CCL.Types.Registry;
      Root : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Key : Schema_Key := No_Schema;
      Bound : Boolean := False;
   end record;
   function Is_Bound (Contract : Binding) return Boolean is (Contract.Bound);
   function Root_Type (Contract : Binding) return CCL.Types.Type_Reference is (Contract.Root);
   function Identity (Contract : Binding) return Schema_Key is (Contract.Key);
end CCL.Objects;
