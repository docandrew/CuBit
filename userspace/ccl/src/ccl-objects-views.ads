-- Owned, indexed views of ordinary native CCL values. A projection is a small
-- position, not another 16 KiB object and not a pointer into a caller's grant.
-- Cursors are relative to their owning Snapshot (like local type references
-- are relative to a registry), never IPC handles or transferable authority.
package CCL.Objects.Views with SPARK_Mode is
   type Snapshot is limited private;
   type Cursor is private;
   No_Value : constant Cursor;
   procedure Capture
     (Object : in out Snapshot; Contract : Binding; Value : Image; Accepted : out Boolean);
   -- Input must be stable owned memory. Captures one copy, validates it and
   -- indexes subtree boundaries once. Failed replacement also invalidates old
   -- cursors. Keep each cursor with its owning snapshot, not another instance.
   procedure Clear (Object : in out Snapshot);
   procedure Capture_Local
     (Object : in out Snapshot; Local_Types : Types.Registry;
      Kind : Types.Type_Reference; Value : Image; Accepted : out Boolean);
   -- In-process construction only: Value must carry No_Schema. The snapshot
   -- retains a local nominal type, not an advertised schema identity. It
   -- cannot be sent until Copy_Value matches an independently approved binding.
   procedure Append_Value
     (Object : Snapshot; Position : Cursor; Value : in out Image;
      Result : out Build_Result);
   -- Append native subtree cells/text to an incomplete builder image, rebasing
   -- text. Caller validates the completed layout before publishing it. Failure
   -- invalidates the builder; do not publish a partially assembled image.
   function Root (Object : Snapshot) return Cursor;
   function Is_Valid (Object : Snapshot; Position : Cursor) return Boolean;
   function Type_Of (Object : Snapshot; Position : Cursor) return Types.Type_Reference;
   function Local_Type
     (Object : Snapshot; Position : Cursor; Local_Types : Types.Registry)
      return Types.Type_Reference;
   function Describe (Object : Snapshot; Position : Cursor) return Types.Description;
   function Field
     (Object : Snapshot; Position : Cursor; Index : Types.Component_Index) return Cursor;
   function Payload (Object : Snapshot; Position : Cursor) return Cursor;
   function Alternative (Object : Snapshot; Position : Cursor) return Types.Component_Count;
   function Scalar (Object : Snapshot; Position : Cursor) return Cell;
   function Text (Object : Snapshot; Position : Cursor) return String;
   subtype Text_Size is Natural range 0 .. Maximum_Text_Bytes;
   function Text_Length (Object : Snapshot; Position : Cursor) return Text_Size;
   procedure Copy_Text
     (Object : Snapshot; Position : Cursor; Target : out String; Accepted : out Boolean);
   procedure Read_Text
     (Object : Snapshot; Position : Cursor; Index : Positive;
      Value : out Character; Accepted : out Boolean);
   -- String views are 1-based. Length/indexing need not copy their bytes;
   -- Copy_Text requires exact length, slides bounds, and never truncates.
   procedure Copy_Value
     (Object : Snapshot; Position : Cursor; Target : Binding;
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
   subtype Position_Count is Natural range 0 .. Maximum_Cells;
   type Cursor is record
      Epoch : Unsigned_64 := 0;
      Position : Position_Count := 0;
   end record;
   No_Value : constant Cursor := (others => <>);
   type Entry_Info is record
      Kind : Types.Type_Reference := Types.Invalid_Type;
      Last : Position_Count := 0;
      Choice : Types.Component_Count := 0;
      Text_First : Positive range 1 .. Maximum_Text_Bytes + 1 := 1;
      Text_Last : Natural range 0 .. Maximum_Text_Bytes := 0;
   end record;
   type Index_Array is array (Cell_Index) of Entry_Info;
   type Snapshot is limited record
      Contract : Binding;
      Value : Image;
      Entries : Index_Array;
      Used : Position_Count := 0;
      Epoch : Unsigned_64 := 0;
      Ready : Boolean := False;
   end record;
end CCL.Objects.Views;
