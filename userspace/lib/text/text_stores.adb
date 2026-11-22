with Ada.Unchecked_Deallocation;

package body Text_Stores with SPARK_Mode => Off is
   procedure Free is new Ada.Unchecked_Deallocation (Chunk, Chunk_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Chunk_Table, Table_Access);

   function Chunks_For (Capacity : Byte_Count) return Natural is
     (Natural ((Capacity + CHUNK_BYTES - 1) / CHUNK_BYTES));

   procedure Create (S : out Store; Capacity : Byte_Count) is
   begin
      S.Used := 0;
      S.Capacity := Capacity;
      S.Chunks := new Chunk_Table'(0 .. Natural'Max (Chunks_For (Capacity), 1) - 1 => null);
   end Create;

   procedure Destroy (S : in out Store) is
   begin
      if S.Chunks /= null then
         for C of S.Chunks.all loop
            Free (C);
         end loop;
         Free (S.Chunks);
      end if;
      S.Used := 0;
   end Destroy;

   function Length (S : Store) return Byte_Count is (S.Used);
   function Maximum (S : Store) return Byte_Count is (S.Capacity);

   function Room (S : Store) return Byte_Count is
      In_Chunk : constant Byte_Count := CHUNK_BYTES - S.Used mod CHUNK_BYTES;
   begin
      return Byte_Count'Min (In_Chunk, S.Capacity - Byte_Count'Min (S.Used, S.Capacity));
   end Room;

   procedure Append (S : in out Store; Text : String) is
      Index : constant Natural := Natural (S.Used / CHUNK_BYTES);
      Within : constant Chunk_Offset := S.Used mod CHUNK_BYTES;
   begin
      if Text'Length = 0 then
         return;
      end if;
      if S.Chunks (Index) = null then
         --  No initialization: pages are touched only as text arrives.
         S.Chunks (Index) := new Chunk;
      end if;
      declare
         Target : String (1 .. Text'Length) with Import, Address => S.Chunks (Index) (Within)'Address;
      begin
         Target := Text;
      end;
      S.Used := S.Used + Text'Length;
   end Append;

   function Element (S : Store; Position : Byte_Offset) return Character is
     (S.Chunks (Natural (Position / CHUNK_BYTES)) (Position mod CHUNK_BYTES));

   procedure Read (S : Store; First : Byte_Offset; Into : out String) is
      Done : Natural := 0;
      At_Offset : Byte_Offset := First;
   begin
      while Done < Into'Length loop
         declare
            Within : constant Chunk_Offset := At_Offset mod CHUNK_BYTES;
            Take : constant Natural :=
              Natural'Min (Into'Length - Done, Natural (CHUNK_BYTES - Within));
            Source : String (1 .. Take)
              with Import, Address => S.Chunks (Natural (At_Offset / CHUNK_BYTES)) (Within)'Address;
         begin
            Into (Into'First + Done .. Into'First + Done + Take - 1) := Source;
            Done := Done + Take;
            At_Offset := At_Offset + Byte_Count (Take);
         end;
      end loop;
   end Read;

   --  Count bytes from First lie in at most two chunks (Count <= a piece).
   function Line_Feeds (S : Store; First : Byte_Offset; Count : Piece_Length) return Piece_Length is
      Result : Natural := 0;
      Done : Natural := 0;
      At_Offset : Byte_Offset := First;
      Total : constant Natural := Natural (Count);
   begin
      while Done < Total loop
         declare
            Within : constant Chunk_Offset := At_Offset mod CHUNK_BYTES;
            Take : constant Natural := Natural'Min (Total - Done, Natural (CHUNK_BYTES - Within));
            Source : String (1 .. Take)
              with Import, Address => S.Chunks (Natural (At_Offset / CHUNK_BYTES)) (Within)'Address;
         begin
            for C of Source loop
               Result := Result + Boolean'Pos (C = ASCII.LF);
            end loop;
            Done := Done + Take;
            At_Offset := At_Offset + Byte_Count (Take);
         end;
      end loop;
      return Piece_Length (Result);
   end Line_Feeds;

   function After_Feed
     (S : Store; First : Byte_Offset; Count : Piece_Length; Nth : Piece_Length) return Piece_Length
   is
      Seen : Natural := 0;
      Done : Natural := 0;
      At_Offset : Byte_Offset := First;
      Total : constant Natural := Natural (Count);
   begin
      --  Over the chunk memory directly (a piece lies in at most two chunks).
      while Done < Total loop
         declare
            Within : constant Chunk_Offset := At_Offset mod CHUNK_BYTES;
            Take : constant Natural := Natural'Min (Total - Done, Natural (CHUNK_BYTES - Within));
            Source : String (1 .. Take)
              with Import, Address => S.Chunks (Natural (At_Offset / CHUNK_BYTES)) (Within)'Address;
         begin
            for I in Source'Range loop
               if Source (I) = ASCII.LF then
                  Seen := Seen + 1;
                  if Seen = Natural (Nth) then
                     return Piece_Length (Done + I);
                  end if;
               end if;
            end loop;
            Done := Done + Take;
            At_Offset := At_Offset + Byte_Count (Take);
         end;
      end loop;
      return Count;
   end After_Feed;
end Text_Stores;
