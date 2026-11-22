with Text_Limits; use Text_Limits;
with Text_Stores; use Text_Stores;

--  The piece table (docs/text-editor.md, "The piece table"). A document is
--  a sequence of pieces, each a run of bytes in its store. Pieces sit in
--  blocks of at most BLOCK_PIECES, and each block keeps its byte and
--  line-feed totals, so finding an offset or a line scans the block totals
--  and then one block: O(blocks + BLOCK_PIECES), never the document.
--
--  An edit touches at most a block's pieces and, when a block splits or
--  empties, the block array. Every block in use holds at least one piece,
--  and its totals are recounted from its pieces after every change
--  (Valid). Proved: no runtime errors, and Valid is kept by every
--  operation. Tested (tests/text-editor, against a reference string):
--  that the sequence of bytes is the one the edits describe.
package Text_Pieces with SPARK_Mode is
   type Piece is record
      First : Byte_Offset := 0;   --  in the store
      Length : Piece_Length := 0;
      Feeds : Piece_Length := 0;   --  line feeds in it
   end record;

   BLOCK_PIECES : constant := 64;
   subtype Slot_Count is Natural range 0 .. BLOCK_PIECES;
   subtype Slot_Index is Slot_Count range 1 .. BLOCK_PIECES;
   --  2 ** 20 blocks of 2 ** 20 bytes: MAXIMUM_BYTES.
   MAXIMUM_BLOCKS : constant := 2 ** 20;
   subtype Block_Count is Natural range 0 .. MAXIMUM_BLOCKS;
   subtype Block_Index is Block_Count range 1 .. MAXIMUM_BLOCKS;
   MAXIMUM_BLOCK_BYTES : constant := BLOCK_PIECES * MAXIMUM_PIECE_BYTES;
   subtype Block_Bytes is Byte_Count range 0 .. MAXIMUM_BLOCK_BYTES;

   --  Where an offset lies: a piece (by block and slot), how far into it,
   --  where it starts in the document and how many line feeds come before
   --  it. Block 0: the document is empty.
   type Position is record
      Block : Block_Count := 0;
      Slot : Slot_Count := 0;
      Within : Piece_Length := 0;
      Start : Byte_Offset := 0;
      Feeds_Before : Byte_Count := 0;
   end record;

   type Table (Capacity : Block_Index) is private;

   --  Cheap: what reading needs (checked on every call in tests).
   function Valid (T : Table) return Boolean;
   --  Expensive: every block in use holds pieces and its totals are their
   --  sums, and the table's totals are the blocks' sums. Kept by every
   --  change (proved), checked around changes in tests.
   function Consistent (T : Table) return Boolean;
   function Length (T : Table) return Byte_Count;
   function Line_Feeds (T : Table) return Byte_Count;
   function Blocks_Used (T : Table) return Block_Count;
   --  Blocks still free: an insert needs at most one.
   function Room (T : Table) return Boolean;
   function Piece_Count (T : Table) return Natural
     with Pre => Valid (T);

   procedure Initialize (T : in out Table)
     with Post => Valid (T) and then Consistent (T) and then Length (T) = 0 and then Line_Feeds (T) = 0;

   function Count_In (T : Table; Block : Block_Index) return Slot_Count
     with Pre => Valid (T) and then Block <= Blocks_Used (T);
   function Has_Piece (T : Table; At_Position : Position) return Boolean is
     (At_Position.Block in 1 .. Blocks_Used (T) and then At_Position.Slot in 1 .. Count_In (T, At_Position.Block))
     with Pre => Valid (T);
   function Element (T : Table; At_Position : Position) return Piece
     with Pre => Valid (T) and then Has_Piece (T, At_Position);

   --  The piece holding the byte at Offset; for Offset = Length, the last
   --  piece with Within = its length.
   function Locate (T : Table; Offset : Byte_Offset) return Position
     with Pre => Valid (T) and then Offset <= Length (T),
          Post => (Locate'Result.Block = 0) = (Blocks_Used (T) = 0)
                  and then Locate'Result.Block <= Blocks_Used (T)
                  and then (if Locate'Result.Block > 0 then Has_Piece (T, Locate'Result));
   --  The piece holding the Nth line feed (1 .. Line_Feeds); Within = 0.
   function Locate_Feed (T : Table; Nth : Byte_Count) return Position
     with Pre => Valid (T) and then Nth >= 1 and then Nth <= Line_Feeds (T),
          Post => Has_Piece (T, Locate_Feed'Result);


   --  The next piece in document order (Start and Feeds_Before follow;
   --  Within becomes 0). Found False after the last piece.
   procedure Next (T : Table; At_Position : in out Position; Found : out Boolean)
     with Pre => Valid (T) and then Has_Piece (T, At_Position),
          Post => (if Found then Has_Piece (T, At_Position));
   --  The previous piece (Within becomes its length).
   procedure Previous (T : Table; At_Position : in out Position; Found : out Boolean)
     with Pre => Valid (T) and then Has_Piece (T, At_Position),
          Post => (if Found then Has_Piece (T, At_Position));

   --  Count bytes of P from From, with its line feeds counted in S (none
   --  when the piece is not in the store: not reached for a table's own).
   function Part (S : Store; P : Piece; From, Count : Piece_Length) return Piece
     with Pre => From <= P.Length and then Count <= P.Length - From,
          Post => Part'Result.Length = Count and then Part'Result.Feeds <= Count;

   --  Insert New_Piece (bytes already in S) at Offset. A piece that ends
   --  where New_Piece starts in the store, at Offset, is extended instead
   --  when the two fit one piece (typing makes one piece, not one per key).
   --  Done False: no free block (Room), nothing changed.
   procedure Insert
     (T : in out Table; S : Store; Offset : Byte_Offset; New_Piece : Piece; Done : out Boolean)
     with Pre => Valid (T) and then Consistent (T) and then Offset <= Length (T)
                 and then New_Piece.Feeds <= New_Piece.Length
                 and then Length (T) <= MAXIMUM_BYTES - MAXIMUM_PIECE_BYTES,
          Post => Valid (T) and then Consistent (T);

   --  Remove Count bytes from Offset. Done False: no free block for the
   --  split at either end; nothing changed.
   procedure Delete (T : in out Table; S : Store; Offset : Byte_Offset; Count : Byte_Count; Done : out Boolean)
     with Pre => Valid (T) and then Consistent (T) and then Offset <= Length (T) and then Count <= Length (T) - Offset,
          Post => Valid (T) and then Consistent (T);
private
   type Piece_Array is array (Slot_Index) of Piece;
   type Block is record
      Items : Piece_Array := [others => (others => <>)];
      Count : Slot_Count := 0;
      Bytes : Block_Bytes := 0;
      Feeds : Block_Bytes := 0;
   end record;
   type Block_Array is array (Block_Index range <>) of Block;
   type Table (Capacity : Block_Index) is record
      Blocks : Block_Array (1 .. Capacity);
      Used : Block_Count := 0;
      Bytes : Byte_Count := 0;
      Feeds : Byte_Count := 0;
   end record;

   --  The totals of a block's first Count pieces.
   function Sum_Bytes (Items : Piece_Array; Count : Slot_Count) return Block_Bytes is
     (if Count = 0 then 0 else Sum_Bytes (Items, Count - 1) + Byte_Count (Items (Count).Length))
     with Subprogram_Variant => (Decreases => Count),
          Post => Sum_Bytes'Result <= Byte_Count (Count) * MAXIMUM_PIECE_BYTES;
   function Sum_Feeds (Items : Piece_Array; Count : Slot_Count) return Block_Bytes is
     (if Count = 0 then 0 else Sum_Feeds (Items, Count - 1) + Byte_Count (Items (Count).Feeds))
     with Subprogram_Variant => (Decreases => Count),
          Post => Sum_Feeds'Result <= Byte_Count (Count) * MAXIMUM_PIECE_BYTES;

   function Valid_Block (B : Block) return Boolean is
     (B.Count >= 1 and then B.Bytes = Sum_Bytes (B.Items, B.Count) and then B.Feeds = Sum_Feeds (B.Items, B.Count)
      and then (for all K in 1 .. B.Count => B.Items (K).Feeds <= B.Items (K).Length));

   --  The totals of the first Count blocks.
   function Sum_Blocks (Blocks : Block_Array; Count : Block_Count) return Byte_Count is
     (if Count = 0 then 0 else Sum_Blocks (Blocks, Count - 1) + Blocks (Count).Bytes)
     with Pre => Count <= Blocks'Last and then Blocks'First = 1,
          Subprogram_Variant => (Decreases => Count),
          Post => Sum_Blocks'Result <= Byte_Count (Count) * MAXIMUM_BLOCK_BYTES;
   function Sum_Block_Feeds (Blocks : Block_Array; Count : Block_Count) return Byte_Count is
     (if Count = 0 then 0 else Sum_Block_Feeds (Blocks, Count - 1) + Blocks (Count).Feeds)
     with Pre => Count <= Blocks'Last and then Blocks'First = 1,
          Subprogram_Variant => (Decreases => Count),
          Post => Sum_Block_Feeds'Result <= Byte_Count (Count) * MAXIMUM_BLOCK_BYTES;

   function Valid (T : Table) return Boolean is
     (T.Used <= T.Capacity and then (for all B in 1 .. T.Used => T.Blocks (B).Count >= 1)
      and then (if T.Used = 0 then T.Bytes = 0 and then T.Feeds = 0));
   function Consistent (T : Table) return Boolean is
     (T.Used <= T.Capacity
      and then (for all B in 1 .. T.Used => Valid_Block (T.Blocks (B)))
      and then T.Bytes = Sum_Blocks (T.Blocks, T.Used)
      and then T.Feeds = Sum_Block_Feeds (T.Blocks, T.Used));

   function Length (T : Table) return Byte_Count is (T.Bytes);
   function Line_Feeds (T : Table) return Byte_Count is (T.Feeds);
   function Blocks_Used (T : Table) return Block_Count is (T.Used);
   function Room (T : Table) return Boolean is (T.Used < T.Capacity);
   function Count_In (T : Table; Block : Block_Index) return Slot_Count is (T.Blocks (Block).Count);
   function Element (T : Table; At_Position : Position) return Piece is
     (T.Blocks (At_Position.Block).Items (At_Position.Slot));
end Text_Pieces;
