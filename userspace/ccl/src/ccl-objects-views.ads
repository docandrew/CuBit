with CCL.Objects.Holders;

-- Owned, indexed views of ordinary native CCL values. A projection is a small
-- position, not another object and not a pointer into a caller's grant.
-- Cursors are relative to their owning Snapshot (like local type references
-- are relative to a registry), never IPC handles or transferable authority.
--  A Sized_Snapshot holds a value of up to Cell_Room cells and Text_Room
--  bytes; Snapshot is the page's room (an Image). Any snapshot captures a
--  page or a sized image that fits it.
package CCL.Objects.Views with SPARK_Mode is
   type Sized_Snapshot (Cell_Room : Cell_Room_Count; Text_Room : Text_Room_Count) is limited private;
   subtype Snapshot is Sized_Snapshot (Maximum_Cells, Maximum_Text_Bytes);
   type Cursor is private;
   No_Value : constant Cursor;
   procedure Capture
     (Object : in out Sized_Snapshot; Contract : Binding; Value : Image; Accepted : out Boolean);
   --  Accepted is False too when Value is larger than Object's room.
   procedure Capture
     (Object : in out Sized_Snapshot; Contract : Binding; Value : Sized_Image; Accepted : out Boolean);
   procedure Capture
     (Object : in out Sized_Snapshot; Contract : Binding; Value : Holders.Holder; Accepted : out Boolean);
   -- Input must be stable owned memory. Captures one copy, validates it and
   -- indexes subtree boundaries once. Failed replacement also invalidates old
   -- cursors. Keep each cursor with its owning snapshot, not another instance.
   procedure Clear (Object : in out Sized_Snapshot);
   procedure Capture_Local
     (Object : in out Sized_Snapshot; Local_Types : Types.Registry;
      Kind : Types.Type_Reference; Value : Image; Accepted : out Boolean);
   -- In-process construction only: Value must carry No_Schema. The snapshot
   -- retains a local nominal type, not an advertised schema identity. It
   -- cannot be sent until Copy_Value matches an independently approved binding.
   procedure Append_Value
     (Object : Sized_Snapshot; Position : Cursor; Value : in out Image;
      Result : out Build_Result);
   procedure Append_Value
     (Object : Sized_Snapshot; Position : Cursor; Value : in out Sized_Image;
      Result : out Build_Result);
   -- Append native subtree cells/text to an incomplete builder image, rebasing
   -- text. Caller validates the completed layout before publishing it. Failure
   -- invalidates the builder; do not publish a partially assembled image.
   function Root (Object : Sized_Snapshot) return Cursor;
   --  The cells the captured value takes (0 when none is), and the part of
   --  it that starts at cell Position: every part, in order, for a walk
   --  that measures the value.
   function Cells_Used (Object : Sized_Snapshot) return Cell_Room_Count;
   function At_Cell (Object : Sized_Snapshot; Position : Positive) return Cursor;
   function Is_Valid (Object : Sized_Snapshot; Position : Cursor) return Boolean;
   function Type_Of (Object : Sized_Snapshot; Position : Cursor) return Types.Type_Reference;
   function Local_Type
     (Object : Sized_Snapshot; Position : Cursor; Local_Types : Types.Registry)
      return Types.Type_Reference;
   function Describe (Object : Sized_Snapshot; Position : Cursor) return Types.Description;
   function Field
     (Object : Sized_Snapshot; Position : Cursor; Index : Types.Component_Index) return Cursor;
   function Payload (Object : Sized_Snapshot; Position : Cursor) return Cursor;
   --  A list's element count, and its element Index (1-based): Element
   --  steps over the earlier elements; First_Element and Next_Element walk
   --  a list in order at a constant cost per element.
   subtype Element_Count is Natural range 0 .. VALUE_CELL_CEILING;
   function Length (Object : Sized_Snapshot; Position : Cursor) return Element_Count;
   function Element
     (Object : Sized_Snapshot; Position : Cursor; Index : Positive) return Cursor;
   function First_Element (Object : Sized_Snapshot; Position : Cursor) return Cursor;
   --  The element after Item in the list at Position (No_Value after the last).
   function Next_Element (Object : Sized_Snapshot; Position, Item : Cursor) return Cursor;
   function Alternative (Object : Sized_Snapshot; Position : Cursor) return Types.Component_Count;
   function Scalar (Object : Sized_Snapshot; Position : Cursor) return Cell;
   function Text (Object : Sized_Snapshot; Position : Cursor) return String;
   subtype Text_Size is Natural range 0 .. VALUE_TEXT_CEILING;
   function Text_Length (Object : Sized_Snapshot; Position : Cursor) return Text_Size;
   procedure Copy_Text
     (Object : Sized_Snapshot; Position : Cursor; Target : out String; Accepted : out Boolean);
   procedure Read_Text
     (Object : Sized_Snapshot; Position : Cursor; Index : Positive;
      Value : out Character; Accepted : out Boolean);
   -- String views are 1-based. Length/indexing need not copy their bytes;
   -- Copy_Text requires exact length, slides bounds, and never truncates.
   procedure Copy_Value
     (Object : Sized_Snapshot; Position : Cursor; Target : Binding;
      Value : out Image; Accepted : out Boolean)
     with Post => (if Accepted then Validate (Value, Target));
   -- Materialize just this subtree under an independently approved target
   -- binding. Full nominal correspondence is required; a matching layout is
   -- not enough. Text offsets are rebased and unrelated data is not exported.
   -- This copies native cells/text, not a serialized representation, and
   -- neither discovers a schema nor grants authority to send the result.
   -- Invalid positions/types yield Invalid_Type, No_Value, zero/empty data;
   -- callers use Type_Of before interpreting scalar data. Field visits only
   -- preceding siblings, never their contents. Payload is constant time.
private
   subtype Position_Count is Natural range 0 .. VALUE_CELL_CEILING;
   type Cursor is record
      Epoch : Unsigned_64 := 0;
      Position : Position_Count := 0;
   end record;
   No_Value : constant Cursor := (others => <>);
   type Entry_Info is record
      Kind : Types.Type_Reference := Types.Invalid_Type;
      Last : Position_Count := 0;
      Choice : Types.Component_Count := 0;
      Text_First : Positive range 1 .. VALUE_TEXT_CEILING + 1 := 1;
      Text_Last : Natural range 0 .. VALUE_TEXT_CEILING := 0;
   end record;
   type Index_Array is array (Positive range <>) of Entry_Info;
   type Sized_Snapshot (Cell_Room : Cell_Room_Count; Text_Room : Text_Room_Count) is limited record
      Contract : Binding;
      Value : Sized_Image (Cell_Room, Text_Room);
      Entries : Index_Array (1 .. Cell_Room);
      Used : Position_Count := 0;
      Epoch : Unsigned_64 := 0;
      Ready : Boolean := False;
   end record;
end CCL.Objects.Views;
