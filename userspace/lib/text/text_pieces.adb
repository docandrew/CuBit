package body Text_Pieces with SPARK_Mode is
   HALF_BLOCK : constant := BLOCK_PIECES / 2;

   --  Recount a block's totals from its pieces.
   procedure Recount (B : in out Block)
     with Pre => B.Count >= 1 and then (for all K in 1 .. B.Count => B.Items (K).Feeds <= B.Items (K).Length),
          Post => Valid_Block (B) and then B.Items = B.Items'Old and then B.Count = B.Count'Old
   is
      Bytes, Feeds : Block_Bytes := 0;
   begin
      for K in 1 .. B.Count loop
         pragma Loop_Invariant (Bytes = Sum_Bytes (B.Items, K - 1) and then Feeds = Sum_Feeds (B.Items, K - 1));
         Bytes := Bytes + Byte_Count (B.Items (K).Length);
         Feeds := Feeds + Byte_Count (B.Items (K).Feeds);
      end loop;
      B.Bytes := Bytes;
      B.Feeds := Feeds;
   end Recount;

   --  Recount the table's totals from its blocks.
   procedure Recount (T : in out Table)
     with Pre => T.Used <= T.Capacity and then (for all B in 1 .. T.Used => Valid_Block (T.Blocks (B))),
          Post => Valid (T) and then Consistent (T) and then T.Blocks = T.Blocks'Old and then T.Used = T.Used'Old
   is
      Bytes, Feeds : Byte_Count := 0;
   begin
      for B in 1 .. T.Used loop
         pragma Loop_Invariant (Bytes = Sum_Blocks (T.Blocks, B - 1)
                                and then Feeds = Sum_Block_Feeds (T.Blocks, B - 1));
         Bytes := Bytes + T.Blocks (B).Bytes;
         Feeds := Feeds + T.Blocks (B).Feeds;
      end loop;
      T.Bytes := Bytes;
      T.Feeds := Feeds;
   end Recount;

   procedure Initialize (T : in out Table) is
   begin
      --  Only blocks 1 .. Used are ever read.
      T.Used := 0;
      T.Bytes := 0;
      T.Feeds := 0;
   end Initialize;

   function Piece_Count (T : Table) return Natural is
      Result : Natural := 0;
   begin
      for B in 1 .. T.Used loop
         pragma Loop_Invariant (Result <= (B - 1) * BLOCK_PIECES);
         Result := Result + T.Blocks (B).Count;
      end loop;
      return Result;
   end Piece_Count;

   function Locate (T : Table; Offset : Byte_Offset) return Position is
      Accumulated, Feeds : Byte_Count := 0;
   begin
      if T.Used = 0 then
         return (others => <>);
      end if;
      for B in 1 .. T.Used loop
         pragma Loop_Invariant (Accumulated = Sum_Blocks (T.Blocks, B - 1)
                                and then Feeds = Sum_Block_Feeds (T.Blocks, B - 1));
         if Offset < Accumulated + T.Blocks (B).Bytes or else B = T.Used then
            declare
               Current : Block renames T.Blocks (B);
               Start : Byte_Count := Accumulated;
               Before : Byte_Count := Feeds;
            begin
               for K in 1 .. Current.Count loop
                  pragma Loop_Invariant (Start = Accumulated + Sum_Bytes (Current.Items, K - 1)
                                         and then Before = Feeds + Sum_Feeds (Current.Items, K - 1));
                  if Offset < Start + Byte_Count (Current.Items (K).Length) or else K = Current.Count then
                     return (Block => B, Slot => K,
                             Within => (if Offset <= Start then 0
                                        else Piece_Length (Byte_Count'Min (Offset - Start,
                                                                           Byte_Count (Current.Items (K).Length)))),
                             Start => Start, Feeds_Before => Before);
                  end if;
                  Start := Start + Byte_Count (Current.Items (K).Length);
                  Before := Before + Byte_Count (Current.Items (K).Feeds);
               end loop;
            end;
         end if;
         Accumulated := Accumulated + T.Blocks (B).Bytes;
         Feeds := Feeds + T.Blocks (B).Feeds;
      end loop;
      --  Not reached: the last block answers.
      return (Block => T.Used, Slot => T.Blocks (T.Used).Count, others => <>);
   end Locate;

   function Locate_Feed (T : Table; Nth : Byte_Count) return Position is
      Accumulated, Feeds : Byte_Count := 0;
   begin
      for B in 1 .. T.Used loop
         pragma Loop_Invariant (Accumulated = Sum_Blocks (T.Blocks, B - 1)
                                and then Feeds = Sum_Block_Feeds (T.Blocks, B - 1));
         if Nth <= Feeds + T.Blocks (B).Feeds or else B = T.Used then
            declare
               Current : Block renames T.Blocks (B);
               Start : Byte_Count := Accumulated;
               Before : Byte_Count := Feeds;
            begin
               for K in 1 .. Current.Count loop
                  pragma Loop_Invariant (Start = Accumulated + Sum_Bytes (Current.Items, K - 1)
                                         and then Before = Feeds + Sum_Feeds (Current.Items, K - 1));
                  if Nth <= Before + Byte_Count (Current.Items (K).Feeds) or else K = Current.Count then
                     return (Block => B, Slot => K, Within => 0, Start => Start, Feeds_Before => Before);
                  end if;
                  Start := Start + Byte_Count (Current.Items (K).Length);
                  Before := Before + Byte_Count (Current.Items (K).Feeds);
               end loop;
            end;
         end if;
         Accumulated := Accumulated + T.Blocks (B).Bytes;
         Feeds := Feeds + T.Blocks (B).Feeds;
      end loop;
      return (Block => T.Used, Slot => T.Blocks (T.Used).Count, others => <>);
   end Locate_Feed;

   procedure Next (T : Table; At_Position : in out Position; Found : out Boolean) is
      Current : constant Piece := Element (T, At_Position);
   begin
      Found := True;
      if At_Position.Slot < T.Blocks (At_Position.Block).Count then
         At_Position.Slot := At_Position.Slot + 1;
      elsif At_Position.Block < T.Used then
         At_Position.Block := At_Position.Block + 1;
         At_Position.Slot := 1;
      else
         Found := False;
         return;
      end if;
      if At_Position.Start <= MAXIMUM_BYTES - Byte_Count (Current.Length) then
         At_Position.Start := At_Position.Start + Byte_Count (Current.Length);
      end if;
      if At_Position.Feeds_Before <= MAXIMUM_BYTES - Byte_Count (Current.Feeds) then
         At_Position.Feeds_Before := At_Position.Feeds_Before + Byte_Count (Current.Feeds);
      end if;
      At_Position.Within := 0;
   end Next;

   procedure Previous (T : Table; At_Position : in out Position; Found : out Boolean) is
   begin
      Found := True;
      if At_Position.Slot > 1 then
         At_Position.Slot := At_Position.Slot - 1;
      elsif At_Position.Block > 1 then
         At_Position.Block := At_Position.Block - 1;
         At_Position.Slot := T.Blocks (At_Position.Block).Count;
      else
         Found := False;
         return;
      end if;
      declare
         Current : constant Piece := Element (T, At_Position);
      begin
         At_Position.Start := At_Position.Start - Byte_Count'Min (At_Position.Start, Byte_Count (Current.Length));
         At_Position.Feeds_Before :=
           At_Position.Feeds_Before - Byte_Count'Min (At_Position.Feeds_Before, Byte_Count (Current.Feeds));
         At_Position.Within := Current.Length;
      end;
   end Previous;

   function Part (S : Store; P : Piece; From, Count : Piece_Length) return Piece is
     (First => (if P.First <= MAXIMUM_BYTES - Byte_Count (From) then P.First + Byte_Count (From) else P.First),
      Length => Count,
      Feeds => (if P.First <= Length (S) and then Byte_Count (From) + Byte_Count (Count) <= Length (S) - P.First
                then Line_Feeds (S, P.First + Byte_Count (From), Count) else 0));

   --  Move the upper half of block Index into a new block after it.
   procedure Split_Block (T : in out Table; Index : Block_Index)
     with Pre => T.Used < T.Capacity and then Index <= T.Used
                 and then (for all B in 1 .. T.Used => Valid_Block (T.Blocks (B)))
                 and then T.Blocks (Index).Count > HALF_BLOCK,
          Post => T.Used = T.Used'Old + 1 and then (for all B in 1 .. T.Used => Valid_Block (T.Blocks (B)))
                  and then T.Blocks (Index).Count = HALF_BLOCK
                  and then T.Blocks (Index + 1).Count = T.Blocks'Old (Index).Count - HALF_BLOCK
   is
      Upper : Block;
      Moved : constant Slot_Count := T.Blocks (Index).Count - HALF_BLOCK;
   begin
      for K in 1 .. Moved loop
         pragma Assert (T.Blocks (Index).Items (HALF_BLOCK + K).Feeds <= T.Blocks (Index).Items (HALF_BLOCK + K).Length);
         Upper.Items (K) := T.Blocks (Index).Items (HALF_BLOCK + K);
         pragma Loop_Invariant (for all J in 1 .. K => Upper.Items (J).Feeds <= Upper.Items (J).Length);
      end loop;
      Upper.Count := Moved;
      Recount (Upper);
      T.Blocks (Index).Count := HALF_BLOCK;
      Recount (T.Blocks (Index));
      if Index < T.Used then
         T.Blocks (Index + 2 .. T.Used + 1) := T.Blocks (Index + 1 .. T.Used);
      end if;
      T.Blocks (Index + 1) := Upper;
      T.Used := T.Used + 1;
   end Split_Block;

   --  Put Items (1 .. Count) at slot At_Slot of block Index, moving the
   --  pieces from At_Slot up.
   type Small_Array is array (1 .. 2) of Piece;
   procedure Put_Pieces
     (B : in out Block; At_Slot : Slot_Index; Items : Small_Array; Count : Positive)
     with Pre => Count <= 2 and then B.Count + Count <= BLOCK_PIECES and then At_Slot <= B.Count + 1
                 and then (for all K in 1 .. B.Count => B.Items (K).Feeds <= B.Items (K).Length)
                 and then (for all K in 1 .. Count => Items (K).Feeds <= Items (K).Length),
          Post => Valid_Block (B) and then B.Count = B.Count'Old + Count
   is
      Old : constant Slot_Count := B.Count;
   begin
      --  Make room: pieces from At_Slot move up by Count.
      for J in reverse At_Slot .. Old loop
         B.Items (J + Count) := B.Items (J);
         pragma Loop_Invariant (for all K in 1 .. J - 1 => B.Items (K).Feeds <= B.Items (K).Length);
         pragma Loop_Invariant (for all K in J + Count .. Old + Count => B.Items (K).Feeds <= B.Items (K).Length);
      end loop;
      pragma Assert (for all K in 1 .. At_Slot - 1 => B.Items (K).Feeds <= B.Items (K).Length);
      pragma Assert (for all K in At_Slot + Count .. Old + Count => B.Items (K).Feeds <= B.Items (K).Length);
      for K in 1 .. Count loop
         B.Items (At_Slot + K - 1) := Items (K);
         pragma Loop_Invariant (for all M in 1 .. At_Slot - 1 => B.Items (M).Feeds <= B.Items (M).Length);
         pragma Loop_Invariant (for all M in At_Slot .. At_Slot + K - 1 => B.Items (M).Feeds <= B.Items (M).Length);
         pragma Loop_Invariant
           (for all M in At_Slot + Count .. Old + Count => B.Items (M).Feeds <= B.Items (M).Length);
      end loop;
      B.Count := Old + Count;
      Recount (B);
   end Put_Pieces;

   procedure Insert
     (T : in out Table; S : Store; Offset : Byte_Offset; New_Piece : Piece; Done : out Boolean)
   is
      At_Position : Position;
      Before : Position;
      Found : Boolean;
   begin
      Done := True;
      if New_Piece.Length = 0 then
         return;
      end if;
      if T.Used = 0 then
         T.Blocks (1) := (Items => [1 => New_Piece, others => (others => <>)], Count => 1, others => 0);
         Recount (T.Blocks (1));
         T.Used := 1;
         Recount (T);
         return;
      end if;
      At_Position := Locate (T, Offset);
      --  The piece that ends at Offset, if any.
      Before := At_Position;
      Found := At_Position.Within > 0 and then At_Position.Within = Element (T, At_Position).Length;
      if not Found and then At_Position.Within = 0 then
         Previous (T, Before, Found);
      end if;
      if Found then
         declare
            B : constant Block_Index := Before.Block;
            K : constant Slot_Index := Before.Slot;
            Prior : constant Piece := T.Blocks (B).Items (K);
         begin
            if Prior.First <= MAXIMUM_BYTES - Byte_Count (Prior.Length)
              and then Prior.First + Byte_Count (Prior.Length) = New_Piece.First
              and then Prior.Length <= MAXIMUM_PIECE_BYTES - New_Piece.Length
            then
               T.Blocks (B).Items (K) :=
                 (First => Prior.First, Length => Prior.Length + New_Piece.Length,
                  Feeds => Prior.Feeds + New_Piece.Feeds);
               Recount (T.Blocks (B));
               Recount (T);
               return;
            end if;
         end;
      end if;
      declare
         Current : constant Piece := Element (T, At_Position);
         Split : constant Boolean := At_Position.Within > 0 and then At_Position.Within < Current.Length;
         Needed : constant Positive := (if Split then 2 else 1);
         Block_Number : Block_Index := At_Position.Block;
         Slot : Slot_Index := At_Position.Slot;
      begin
         if T.Blocks (Block_Number).Count + Needed > BLOCK_PIECES then
            if T.Used >= T.Capacity then
               Done := False;
               return;
            end if;
            Split_Block (T, Block_Number);
            if Slot > HALF_BLOCK then
               Block_Number := Block_Number + 1;
               Slot := Slot - HALF_BLOCK;
            end if;
         end if;
         if Split then
            --  Left part stays in the slot; the new piece and the right
            --  part follow it.
            T.Blocks (Block_Number).Items (Slot) := Part (S, Current, 0, At_Position.Within);
            Put_Pieces (T.Blocks (Block_Number), Slot + 1,
                        [New_Piece, Part (S, Current, At_Position.Within, Current.Length - At_Position.Within)], 2);
         elsif At_Position.Within = 0 then
            Put_Pieces (T.Blocks (Block_Number), Slot, [New_Piece, (others => <>)], 1);
         else
            Put_Pieces (T.Blocks (Block_Number), Slot + 1, [New_Piece, (others => <>)], 1);
         end if;
      end;
      Recount (T);
   end Insert;

   --  Make a piece boundary at Offset (split the piece across it).
   procedure Split_At (T : in out Table; S : Store; Offset : Byte_Offset; Done : out Boolean)
     with Pre => Valid (T) and then Consistent (T) and then Offset <= Length (T),
          Post => Valid (T) and then Consistent (T)
   is
      At_Position : Position;
   begin
      Done := True;
      if T.Used = 0 then
         return;
      end if;
      At_Position := Locate (T, Offset);
      declare
         Current : constant Piece := Element (T, At_Position);
         Block_Number : Block_Index := At_Position.Block;
         Slot : Slot_Index := At_Position.Slot;
      begin
         if At_Position.Within = 0 or else At_Position.Within >= Current.Length then
            return;
         end if;
         if T.Blocks (Block_Number).Count + 1 > BLOCK_PIECES then
            if T.Used >= T.Capacity then
               Done := False;
               return;
            end if;
            Split_Block (T, Block_Number);
            if Slot > HALF_BLOCK then
               Block_Number := Block_Number + 1;
               Slot := Slot - HALF_BLOCK;
            end if;
         end if;
         T.Blocks (Block_Number).Items (Slot) := Part (S, Current, 0, At_Position.Within);
         Put_Pieces (T.Blocks (Block_Number), Slot + 1,
                     [Part (S, Current, At_Position.Within, Current.Length - At_Position.Within), (others => <>)], 1);
      end;
      Recount (T);
   end Split_At;

   procedure Delete (T : in out Table; S : Store; Offset : Byte_Offset; Count : Byte_Count; Done : out Boolean) is
      First_Position, Last_Position : Position;
   begin
      Done := True;
      if Count = 0 or else T.Used = 0 then
         return;
      end if;
      Split_At (T, S, Offset, Done);
      if not Done then
         return;
      end if;
      if Offset + Count > Length (T) then
         return;   --  Not reached: splitting keeps the length.
      end if;
      Split_At (T, S, Offset + Count, Done);
      if not Done or else Offset + Count > Length (T) or else T.Used = 0 then
         return;
      end if;
      First_Position := Locate (T, Offset);
      Last_Position := Locate (T, Offset + Count - 1);
      if First_Position.Block > Last_Position.Block
        or else (First_Position.Block = Last_Position.Block and then First_Position.Slot > Last_Position.Slot)
      then
         return;   --  Not reached for a consistent table.
      end if;
      declare
         B1 : constant Block_Index := First_Position.Block;
         S1 : constant Slot_Index := First_Position.Slot;
         B2 : constant Block_Index := Last_Position.Block;
         S2 : constant Slot_Index := Last_Position.Slot;
         Low, High : Block_Count;
      begin
         if B1 = B2 then
            declare
               Current : Block renames T.Blocks (B1);
               Removed : constant Slot_Count := S2 - S1 + 1;
            begin
               if S2 < Current.Count then
                  Current.Items (S1 .. Current.Count - Removed) := Current.Items (S2 + 1 .. Current.Count);
               end if;
               Current.Count := Current.Count - Removed;
            end;
            Low := B1;
            High := (if T.Blocks (B1).Count = 0 then B1 else B1 - 1);
         else
            T.Blocks (B1).Count := S1 - 1;
            declare
               Current : Block renames T.Blocks (B2);
            begin
               if S2 < Current.Count then
                  Current.Items (1 .. Current.Count - S2) := Current.Items (S2 + 1 .. Current.Count);
               end if;
               Current.Count := Current.Count - S2;
            end;
            Low := (if T.Blocks (B1).Count = 0 then B1 else B1 + 1);
            High := (if T.Blocks (B2).Count = 0 then B2 else B2 - 1);
         end if;
         if T.Blocks (B1).Count > 0 then
            Recount (T.Blocks (B1));
         end if;
         if B2 /= B1 and then T.Blocks (B2).Count > 0 then
            Recount (T.Blocks (B2));
         end if;
         if Low <= High then
            declare
               Gone : constant Block_Count := High - Low + 1;
            begin
               if High < T.Used then
                  T.Blocks (Low .. T.Used - Gone) := T.Blocks (High + 1 .. T.Used);
               end if;
               T.Used := T.Used - Gone;
            end;
         end if;
      end;
      Recount (T);
   end Delete;
end Text_Pieces;
