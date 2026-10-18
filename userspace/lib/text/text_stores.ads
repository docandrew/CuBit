with Text_Limits; use Text_Limits;

--  A document's bytes (docs/text-editor.md, "The piece table"): one
--  append-only store. A load appends the file's bytes; typing and pasting
--  append the new text; nothing already stored is ever moved or changed, so
--  pieces and undo records can refer to stored bytes forever.
--
--  The store grows in chunks of CHUNK_BYTES, allocated as they are needed
--  (CuAlloc natively), up to the capacity its host chose. A piece never
--  spans two chunks: Room says how much fits before the current chunk ends.
--  Bytes are bytes: nothing is decoded or changed on the way in or out.
package Text_Stores with SPARK_Mode is
   CHUNK_BYTES : constant := 4_194_304;

   type Store is limited private;

   --  An empty store that will hold at most Capacity bytes.
   procedure Create (S : out Store; Capacity : Byte_Count)
     with Post => Length (S) = 0 and then Maximum (S) = Capacity;
   --  Free every chunk.
   procedure Destroy (S : in out Store)
     with Post => Length (S) = 0;

   function Length (S : Store) return Byte_Count;
   function Maximum (S : Store) return Byte_Count;

   --  What can be appended in one piece now: up to the end of the current
   --  chunk (a whole new chunk when it is full), and up to the capacity.
   function Room (S : Store) return Byte_Count
     with Post => Room'Result <= Maximum (S) - Byte_Count'Min (Length (S), Maximum (S))
                  and then Room'Result <= CHUNK_BYTES;

   procedure Append (S : in out Store; Text : String)
     with Pre => Text'Length <= Room (S),
          Post => Length (S) = Length (S)'Old + Text'Length and Maximum (S) = Maximum (S)'Old;

   function Element (S : Store; Position : Byte_Offset) return Character
     with Pre => Position < Length (S);

   --  Into'Length bytes from First (they may span chunks).
   procedure Read (S : Store; First : Byte_Offset; Into : out String)
     with Pre => First <= Length (S) and then Into'Length <= Length (S) - First;

   --  Line feeds in Count bytes from First.
   function Line_Feeds (S : Store; First : Byte_Offset; Count : Piece_Length) return Piece_Length
     with Pre => First <= Length (S) and then Byte_Count (Count) <= Length (S) - First,
          Post => Line_Feeds'Result <= Count;

   --  The offset (from First) just after the Nth line feed in Count bytes
   --  from First; Count when there are fewer.
   function After_Feed
     (S : Store; First : Byte_Offset; Count : Piece_Length; Nth : Piece_Length) return Piece_Length
     with Pre => First <= Length (S) and then Byte_Count (Count) <= Length (S) - First and then Nth >= 1,
          Post => After_Feed'Result <= Count;
private
   pragma SPARK_Mode (Off);
   subtype Chunk_Offset is Byte_Count range 0 .. CHUNK_BYTES - 1;
   type Chunk is array (Chunk_Offset) of Character;
   type Chunk_Access is access Chunk;
   type Chunk_Table is array (Natural range <>) of Chunk_Access;
   type Table_Access is access Chunk_Table;
   type Store is limited record
      Chunks : Table_Access := null;
      Used : Byte_Count := 0;
      Capacity : Byte_Count := 0;
   end record;
end Text_Stores;
