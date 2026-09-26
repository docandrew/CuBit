with Interfaces;
with CCL.Types;

--  Native, pointer-free CCL data objects. This is NOT the disk/network codec.
--  A trusted schema binding supplies meaning; local Type_References never
--  travel in the object. Products/sums use declaration order, depth first.
package CCL.Objects with SPARK_Mode is
   use Interfaces;
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
   subtype Cell_Index is Positive range 1 .. Maximum_Cells;
   type Cell_Array is array (Cell_Index) of Cell with Component_Size => 128;
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

   function Empty (Contract : Binding) return Image;
   type Build_Result is (Added, Full, Invalid_Image);
   --  Append builds an incomplete object; Validate is required before use.
   procedure Append (Object : in out Image; Value : Cell; Result : out Build_Result)
     with Post => (if Result /= Added then Object = Object'Old);
   procedure Append_Text
     (Object : in out Image; Value : String; Result : out Build_Result)
     with Post => (if Result /= Added then Object = Object'Old);

   function Integer_Cell (Value : Integer_64) return Cell;
   function Boolean_Cell (Value : Boolean) return Cell is
     ((First => (if Value then 1 else 0), Second => 0));
   function Character_Cell (Value : Character) return Cell is
     ((First => Character'Pos (Value), Second => 0));
   function Product_Cell (Fields : CCL.Types.Component_Count) return Cell is
     ((First => Unsigned_64 (Fields), Second => 0));
   function Variant_Cell (Choice : CCL.Types.Component_Index) return Cell is
     ((First => Unsigned_64 (Choice), Second => 0));
   Unit_Cell : constant Cell := (0, 0);
   function Integer_Of (Value : Cell) return Integer_64;

   --  Requires a stable owned snapshot or kernel-enforced immutable mapping.
   --  Read-only access for the receiver alone does NOT satisfy that condition.
   --  Checks nested types, counts, packed text ranges, exact consumption and
   --  zero unused storage. No recursion, pointer fixups, or object rebuilding.
   function Validate (Object : Image; Contract : Binding) return Boolean;
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
