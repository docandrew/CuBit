with Interfaces;
with Text_Limits; use Text_Limits;
with Text_Pieces;
with Text_Cursors;

--  Undo and redo (docs/text-editor.md, "Undo and redo"). The stores are
--  append-only, so an edit is recorded by reference: an insert or a
--  removal is its offset, its length and the pieces it put in or took out
--  (copied into the piece log, whatever their length). Undo applies the
--  inverse, redo the edit again; neither copies text. The document applies
--  them (it owns the piece table); this unit keeps the log.
--
--  Records form groups, one per undo step, with the selections before and
--  after. Typing (and erasing) coalesces into the open group while it
--  continues where the last key left off, within COALESCE_GAP_US, up to a
--  word boundary; anything else seals the group. A new group drops the redo
--  groups. At capacity, Forget_Oldest drops the oldest half of the groups
--  in one step, so editing never stops.
--
--  Proved: no runtime errors; every index the log hands out is in range.
package Text_History with SPARK_Mode is
   use type Interfaces.Unsigned_64;

   COALESCE_GAP_US : constant := 1_000_000;

   MAXIMUM_ENTRIES : constant := 2 ** 26;
   subtype Entry_Count is Natural range 0 .. MAXIMUM_ENTRIES;
   subtype Entry_Index is Entry_Count range 1 .. MAXIMUM_ENTRIES;

   type Record_Kind is (Inserted, Removed);
   type Edit_Record is record
      Kind : Record_Kind := Inserted;
      Offset : Byte_Offset := 0;
      Length : Byte_Count := 0;
      Pieces_First : Entry_Index := 1;
      Pieces_Count : Entry_Count := 0;
   end record;

   type Group_Kind is (Typing, Erasing, Other, Host_Edit);

   --  The history's capacities, chosen by the host.
   type Limits is record
      Groups : Entry_Index := 65_536;
      Records : Entry_Index := 262_144;
      Pieces : Entry_Index := 1_048_576;
      Selections : Entry_Index := 262_144;
   end record;

   type History (Groups, Records, Pieces, Selections : Entry_Index) is private;

   --  The log's counts within its capacities, an open group the last.
   function Valid (H : History) return Boolean;

   procedure Initialize (H : in out History)
     with Post => Valid (H) and then not Can_Undo (H) and then not Can_Redo (H) and then Is_Saved (H);

   function Can_Undo (H : History) return Boolean;
   function Can_Redo (H : History) return Boolean;
   --  Groups done (undoable) and in total (done plus redoable).
   function Done_Groups (H : History) return Entry_Count;
   function Total_Groups (H : History) return Entry_Count;

   --  Start a group, or continue the open one (Continued True) when Kind is
   --  Typing or Erasing like it, At_Offset is where it left off, and less
   --  than COALESCE_GAP_US passed. A new group logs Before. Ok False: no
   --  room for Before (Forget_Oldest, or Clear, and try again).
   procedure Begin_Group
     (H : in out History; Kind : Group_Kind; Before : Text_Cursors.Set; At_Offset : Byte_Offset;
      Now_Us : Interfaces.Unsigned_64; Word_Boundary : Boolean; Continued, Ok : out Boolean)
     with Pre => Valid (H), Post => Valid (H);
   --  Record an insert of P at Offset into the open group (a piece
   --  contiguous with the group's last insert extends it).
   procedure Record_Insert (H : in out History; Offset : Byte_Offset; P : Text_Pieces.Piece; Ok : out Boolean)
     with Pre => Valid (H) and then Group_Open (H), Post => Valid (H) and then Group_Open (H);
   --  Record a removal at Offset of Length bytes; then its pieces, in order,
   --  with Record_Removed_Piece.
   procedure Record_Remove (H : in out History; Offset : Byte_Offset; Length : Byte_Count; Ok : out Boolean)
     with Pre => Valid (H) and then Group_Open (H), Post => Valid (H) and then Group_Open (H);
   procedure Record_Removed_Piece (H : in out History; P : Text_Pieces.Piece; Ok : out Boolean)
     with Pre => Valid (H) and then Group_Open (H), Post => Valid (H) and then Group_Open (H);
   --  Close the open group: log After (replacing the coalesced group's
   --  last After when it has the same count), and where typing continues.
   procedure End_Group
     (H : in out History; After : Text_Cursors.Set; Continue_At : Byte_Offset; Ok : out Boolean)
     with Pre => Valid (H), Post => Valid (H);
   function Group_Open (H : History) return Boolean;
   --  No further coalescing into the last group.
   procedure Seal (H : in out History) with Pre => Valid (H), Post => Valid (H);

   --  The group the next Undo undoes, and the next Redo redoes.
   function Undo_Group (H : History) return Entry_Index
     with Pre => Valid (H) and then Can_Undo (H), Post => Undo_Group'Result <= Total_Groups (H);
   function Redo_Group (H : History) return Entry_Index
     with Pre => Valid (H) and then Can_Redo (H), Post => Redo_Group'Result <= Total_Groups (H);
   --  A group's records (applied in order; undone in reverse).
   procedure Group_Records (H : History; Group : Entry_Index; First : out Entry_Index; Count : out Entry_Count)
     with Pre => Valid (H) and then Group <= Total_Groups (H);
   function Record_At (H : History; Index : Entry_Index) return Edit_Record
     with Pre => Valid (H) and then Index <= Record_Count (H);
   function Piece_At (H : History; Index : Entry_Index) return Text_Pieces.Piece
     with Pre => Valid (H) and then Index <= Piece_Log_Count (H);
   --  The group's selections before (Which False) or after (True).
   procedure Group_Selections
     (H : History; Group : Entry_Index; After : Boolean; First : out Entry_Index; Count : out Entry_Count)
     with Pre => Valid (H) and then Group <= Total_Groups (H);
   function Selection_At (H : History; Index : Entry_Index) return Text_Cursors.Selection
     with Pre => Valid (H) and then Index <= Selection_Log_Count (H);
   function Record_Count (H : History) return Entry_Count;
   function Piece_Log_Count (H : History) return Entry_Count;
   function Selection_Log_Count (H : History) return Entry_Count;

   --  After the document undid Undo_Group, or redid Redo_Group.
   procedure Mark_Undone (H : in out History) with Pre => Valid (H) and then Can_Undo (H), Post => Valid (H);
   procedure Mark_Redone (H : in out History) with Pre => Valid (H) and then Can_Redo (H), Post => Valid (H);

   --  The document now matches its file.
   procedure Mark_Saved (H : in out History) with Pre => Valid (H), Post => Valid (H) and then Is_Saved (H);
   --  The file now holds the state the history had at Done_Groups = Done
   --  in Era (a save of a snapshot that finished later). Ignored when the
   --  groups were renumbered since (Forget_Oldest, Clear, dropped redo).
   function Era (H : History) return Natural;
   procedure Mark_Saved_At (H : in out History; Done : Entry_Count; In_Era : Natural)
     with Pre => Valid (H), Post => Valid (H);
   function Is_Saved (H : History) return Boolean;

   --  Drop the oldest half of the done groups (at least one), keeping the
   --  rest and their indices consistent.
   procedure Forget_Oldest (H : in out History) with Pre => Valid (H), Post => Valid (H);
   --  Drop everything (an edit too large for the history's capacity).
   procedure Clear (H : in out History)
     with Pre => Valid (H), Post => Valid (H) and then not Can_Undo (H) and then not Can_Redo (H);
private
   type Group is record
      Kind : Group_Kind := Other;
      Records_First : Entry_Index := 1;
      Records_Count : Entry_Count := 0;
      Before_First : Entry_Index := 1;
      Before_Count : Entry_Count := 0;
      After_First : Entry_Index := 1;
      After_Count : Entry_Count := 0;
      --  The logs' lengths once this group is complete: dropping the redo
      --  groups after it truncates the logs to these.
      Record_End, Piece_End, Selection_End : Entry_Count := 0;
      Continue_At : Byte_Offset := 0;
      Time_Us : Interfaces.Unsigned_64 := 0;
      Sealed : Boolean := True;
   end record;
   type Group_Array is array (Entry_Index range <>) of Group;
   type Record_Array is array (Entry_Index range <>) of Edit_Record;
   type Piece_Array is array (Entry_Index range <>) of Text_Pieces.Piece;
   type Selection_Array is array (Entry_Index range <>) of Text_Cursors.Selection;

   type History (Groups, Records, Pieces, Selections : Entry_Index) is record
      Group_Log : Group_Array (1 .. Groups);
      Record_Log : Record_Array (1 .. Records);
      Piece_Log : Piece_Array (1 .. Pieces);
      Selection_Log : Selection_Array (1 .. Selections);
      Done, Total : Entry_Count := 0;
      Record_Used, Piece_Used, Selection_Used : Entry_Count := 0;
      Open : Boolean := False;
      --  Done when the document last matched its file; Saved_Valid False
      --  when that state was forgotten or dropped.
      Saved_At : Entry_Count := 0;
      Saved_Valid : Boolean := True;
      --  Moves on whenever group numbers stop meaning what they meant.
      Epoch : Natural := 0;
   end record;

   function Valid (H : History) return Boolean is
     (H.Done <= H.Total and then H.Total <= H.Groups
      and then H.Record_Used <= H.Records and then H.Piece_Used <= H.Pieces
      and then H.Selection_Used <= H.Selections
      and then (if H.Open then H.Total >= 1 and then H.Done = H.Total));

   function Can_Undo (H : History) return Boolean is (H.Done > 0 and then not H.Open);
   function Can_Redo (H : History) return Boolean is (H.Done < H.Total and then not H.Open);
   function Done_Groups (H : History) return Entry_Count is (H.Done);
   function Total_Groups (H : History) return Entry_Count is (H.Total);
   function Group_Open (H : History) return Boolean is (H.Open);
   function Record_Count (H : History) return Entry_Count is (H.Record_Used);
   function Piece_Log_Count (H : History) return Entry_Count is (H.Piece_Used);
   function Selection_Log_Count (H : History) return Entry_Count is (H.Selection_Used);
   function Is_Saved (H : History) return Boolean is (H.Saved_Valid and then H.Saved_At = H.Done);
   function Era (H : History) return Natural is (H.Epoch);
end Text_History;
