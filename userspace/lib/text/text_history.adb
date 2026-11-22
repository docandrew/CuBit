package body Text_History with SPARK_Mode is
   use type Text_Pieces.Piece;

   procedure Initialize (H : in out History) is
   begin
      --  Only entries up to the used counts are ever read.
      H.Done := 0;
      H.Total := 0;
      H.Record_Used := 0;
      H.Piece_Used := 0;
      H.Selection_Used := 0;
      H.Open := False;
      H.Saved_At := 0;
      H.Saved_Valid := True;
   end Initialize;

   procedure Next_Epoch (H : in out History)
     with Pre => Valid (H), Post => Valid (H) and then H.Done = H.Done'Old and then H.Total = H.Total'Old
                                    and then H.Open = H.Open'Old
                                    and then H.Record_Used = H.Record_Used'Old and then H.Piece_Used = H.Piece_Used'Old
                                    and then H.Selection_Used = H.Selection_Used'Old
   is
   begin
      H.Epoch := (if H.Epoch < Natural'Last then H.Epoch + 1 else 0);
   end Next_Epoch;

   procedure Log_Selections
     (H : in out History; Value : Text_Cursors.Set; First : out Entry_Index; Count : out Entry_Count; Ok : out Boolean)
     with Pre => Valid (H),
          Post => Valid (H) and then H.Done = H.Done'Old and then H.Total = H.Total'Old and then H.Open = H.Open'Old
                  and then H.Record_Used = H.Record_Used'Old and then H.Piece_Used = H.Piece_Used'Old
   is
      N : constant Natural := Text_Cursors.Count (Value);
   begin
      First := (if H.Selection_Used < H.Selections then H.Selection_Used + 1 else 1);
      Count := 0;
      Ok := N <= H.Selections - H.Selection_Used;
      if not Ok then
         return;
      end if;
      for K in 1 .. N loop
         pragma Loop_Invariant (H.Selection_Used = H.Selection_Used'Loop_Entry + K - 1
                                and then H.Selection_Used'Loop_Entry + N <= H.Selections);
         H.Selection_Used := H.Selection_Used + 1;
         H.Selection_Log (H.Selection_Used) := Text_Cursors.Element (Value, K);
      end loop;
      Count := N;
   end Log_Selections;

   procedure Begin_Group
     (H : in out History; Kind : Group_Kind; Before : Text_Cursors.Set; At_Offset : Byte_Offset;
      Now_Us : Interfaces.Unsigned_64; Word_Boundary : Boolean; Continued, Ok : out Boolean)
   is
   begin
      Continued := False;
      Ok := True;
      H.Open := False;
      if H.Total > 0 and then H.Done = H.Total and then Kind in Typing | Erasing then
         declare
            Last_Group : constant Entry_Index := H.Total;
            G : constant Group := H.Group_Log (Last_Group);
         begin
            if not G.Sealed and then G.Kind = Kind and then G.Continue_At = At_Offset
              and then Now_Us >= G.Time_Us and then Now_Us - G.Time_Us < COALESCE_GAP_US
              and then not Word_Boundary
            then
               H.Group_Log (Last_Group).Time_Us := Now_Us;
               Continued := True;
               H.Open := True;
               return;
            end if;
         end;
      end if;
      --  A new group: the redo groups go.
      if H.Done < H.Total then
         Next_Epoch (H);
         if H.Done = 0 then
            H.Record_Used := 0;
            H.Piece_Used := 0;
            H.Selection_Used := 0;
         else
            H.Record_Used := Natural'Min (H.Group_Log (H.Done).Record_End, H.Record_Used);
            H.Piece_Used := Natural'Min (H.Group_Log (H.Done).Piece_End, H.Piece_Used);
            H.Selection_Used := Natural'Min (H.Group_Log (H.Done).Selection_End, H.Selection_Used);
         end if;
         H.Total := H.Done;
      end if;
      if H.Saved_At > H.Done then
         H.Saved_Valid := False;
      end if;
      if H.Total > 0 then
         H.Group_Log (H.Total).Sealed := True;
      end if;
      if H.Total >= H.Groups then
         Ok := False;
         return;
      end if;
      declare
         First : Entry_Index;
         Count : Entry_Count;
      begin
         Log_Selections (H, Before, First, Count, Ok);
         if not Ok then
            return;
         end if;
         H.Total := H.Total + 1;
         H.Done := H.Total;
         H.Group_Log (H.Total) :=
           (Kind => Kind,
            Records_First => (if H.Record_Used < H.Records then H.Record_Used + 1 else H.Records),
            Records_Count => 0,
            Before_First => First, Before_Count => Count,
            After_First => First, After_Count => 0,
            Record_End => H.Record_Used, Piece_End => H.Piece_Used, Selection_End => H.Selection_Used,
            Continue_At => At_Offset, Time_Us => Now_Us, Sealed => Kind not in Typing | Erasing);
         H.Open := True;
      end;
   end Begin_Group;

   --  The open group's last record, if it is the log's last record.
   function Last_Record_Is_Open (H : History) return Boolean is
     (Valid (H) and then H.Open and then H.Total >= 1 and then H.Group_Log (H.Total).Records_Count > 0
      and then H.Record_Used >= 1
      and then H.Group_Log (H.Total).Records_First <= H.Records - H.Group_Log (H.Total).Records_Count + 1
      and then H.Group_Log (H.Total).Records_First + H.Group_Log (H.Total).Records_Count - 1 = H.Record_Used);

   procedure Append_Piece (H : in out History; P : Text_Pieces.Piece; Ok : out Boolean)
     with Pre => Valid (H) and then Last_Record_Is_Open (H),
          Post => Valid (H) and then H.Open and then H.Record_Used = H.Record_Used'Old
   is
      Last_Record : constant Entry_Index := H.Record_Used;
      R : constant Edit_Record := H.Record_Log (Last_Record);
   begin
      Ok := True;
      --  A piece contiguous in the store with the record's last one extends it.
      if R.Pieces_Count > 0 and then R.Pieces_First + R.Pieces_Count - 1 = H.Piece_Used and then H.Piece_Used >= 1
      then
         declare
            Last_Piece : constant Entry_Index := H.Piece_Used;
            Last : constant Text_Pieces.Piece := H.Piece_Log (Last_Piece);
         begin
            if Last.First <= MAXIMUM_BYTES - Byte_Count (Last.Length)
              and then Last.First + Byte_Count (Last.Length) = P.First
              and then Last.Length <= MAXIMUM_PIECE_BYTES - P.Length
              and then Last.Feeds <= MAXIMUM_PIECE_BYTES - P.Feeds
            then
               H.Piece_Log (Last_Piece) :=
                 (First => Last.First, Length => Last.Length + P.Length, Feeds => Last.Feeds + P.Feeds);
               return;
            end if;
         end;
      end if;
      if H.Piece_Used >= H.Pieces or else R.Pieces_Count >= MAXIMUM_ENTRIES then
         Ok := False;
         return;
      end if;
      H.Piece_Used := H.Piece_Used + 1;
      H.Piece_Log (H.Piece_Used) := P;
      if R.Pieces_Count = 0 then
         H.Record_Log (Last_Record).Pieces_First := H.Piece_Used;
      end if;
      H.Record_Log (Last_Record).Pieces_Count := R.Pieces_Count + 1;
   end Append_Piece;

   procedure New_Record (H : in out History; Item : Edit_Record; Ok : out Boolean)
     with Pre => Valid (H) and then H.Open,
          Post => Valid (H) and then H.Open and then (if Ok then H.Record_Used >= 1)
   is
      Last_Group : constant Entry_Index := H.Total;
      G : constant Group := H.Group_Log (Last_Group);
   begin
      Ok := H.Record_Used < H.Records and then G.Records_Count < MAXIMUM_ENTRIES;
      if not Ok then
         return;
      end if;
      H.Record_Used := H.Record_Used + 1;
      H.Record_Log (H.Record_Used) := Item;
      if G.Records_Count = 0 then
         H.Group_Log (Last_Group).Records_First := H.Record_Used;
      end if;
      H.Group_Log (Last_Group).Records_Count := G.Records_Count + 1;
   end New_Record;

   procedure Record_Insert (H : in out History; Offset : Byte_Offset; P : Text_Pieces.Piece; Ok : out Boolean) is
   begin
      if Last_Record_Is_Open (H) then
         declare
            R : constant Edit_Record := H.Record_Log (H.Record_Used);
         begin
            if R.Kind = Inserted and then R.Offset <= MAXIMUM_BYTES - R.Length
              and then R.Offset + R.Length = Offset and then R.Length <= MAXIMUM_BYTES - Byte_Count (P.Length)
            then
               Append_Piece (H, P, Ok);
               if Ok then
                  H.Record_Log (H.Record_Used).Length := R.Length + Byte_Count (P.Length);
               end if;
               return;
            end if;
         end;
      end if;
      New_Record (H, (Kind => Inserted, Offset => Offset, Length => 0, others => <>), Ok);
      if Ok and then Last_Record_Is_Open (H) then
         Append_Piece (H, P, Ok);
         if Ok then
            H.Record_Log (H.Record_Used).Length := Byte_Count (P.Length);
         end if;
      end if;
   end Record_Insert;

   procedure Record_Remove (H : in out History; Offset : Byte_Offset; Length : Byte_Count; Ok : out Boolean) is
   begin
      New_Record (H, (Kind => Removed, Offset => Offset, Length => Length, others => <>), Ok);
   end Record_Remove;

   procedure Record_Removed_Piece (H : in out History; P : Text_Pieces.Piece; Ok : out Boolean) is
   begin
      if Last_Record_Is_Open (H) then
         Append_Piece (H, P, Ok);
      else
         Ok := False;
      end if;
   end Record_Removed_Piece;

   procedure End_Group
     (H : in out History; After : Text_Cursors.Set; Continue_At : Byte_Offset; Ok : out Boolean)
   is
      N : constant Natural := Text_Cursors.Count (After);
   begin
      Ok := True;
      if not H.Open then
         return;
      end if;
      declare
         G : constant Group := H.Group_Log (H.Total);
      begin
         if G.After_Count = N and then N > 0 and then G.After_First <= H.Selections - N + 1
           and then G.After_First + N - 1 = H.Selection_Used
         then
            for K in 1 .. N loop
               H.Selection_Log (G.After_First + K - 1) := Text_Cursors.Element (After, K);
            end loop;
         else
            declare
               First : Entry_Index;
               Count : Entry_Count;
            begin
               Log_Selections (H, After, First, Count, Ok);
               if Ok then
                  H.Group_Log (H.Total).After_First := First;
                  H.Group_Log (H.Total).After_Count := Count;
               end if;
            end;
         end if;
      end;
      H.Group_Log (H.Total).Continue_At := Continue_At;
      H.Group_Log (H.Total).Record_End := H.Record_Used;
      H.Group_Log (H.Total).Piece_End := H.Piece_Used;
      H.Group_Log (H.Total).Selection_End := H.Selection_Used;
      H.Open := False;
   end End_Group;

   procedure Seal (H : in out History) is
   begin
      if H.Total > 0 and then not H.Open then
         H.Group_Log (H.Total).Sealed := True;
      end if;
   end Seal;

   function Undo_Group (H : History) return Entry_Index is (H.Done);
   function Redo_Group (H : History) return Entry_Index is (H.Done + 1);

   procedure Group_Records (H : History; Group : Entry_Index; First : out Entry_Index; Count : out Entry_Count) is
   begin
      First := H.Group_Log (Group).Records_First;
      Count := H.Group_Log (Group).Records_Count;
      if Count > 0 and then (First > H.Record_Used or else Count > H.Record_Used - First + 1) then
         Count := 0;   --  Not reached: a group's records are in the log.
      end if;
   end Group_Records;

   function Record_At (H : History; Index : Entry_Index) return Edit_Record is (H.Record_Log (Index));
   function Piece_At (H : History; Index : Entry_Index) return Text_Pieces.Piece is (H.Piece_Log (Index));

   procedure Group_Selections
     (H : History; Group : Entry_Index; After : Boolean; First : out Entry_Index; Count : out Entry_Count)
   is
   begin
      if After then
         First := H.Group_Log (Group).After_First;
         Count := H.Group_Log (Group).After_Count;
      else
         First := H.Group_Log (Group).Before_First;
         Count := H.Group_Log (Group).Before_Count;
      end if;
      if Count > 0 and then (First > H.Selection_Used or else Count > H.Selection_Used - First + 1) then
         Count := 0;
      end if;
   end Group_Selections;

   function Selection_At (H : History; Index : Entry_Index) return Text_Cursors.Selection is
     (H.Selection_Log (Index));

   procedure Mark_Undone (H : in out History) is
   begin
      H.Done := H.Done - 1;
   end Mark_Undone;

   procedure Mark_Redone (H : in out History) is
   begin
      H.Done := H.Done + 1;
   end Mark_Redone;

   procedure Mark_Saved (H : in out History) is
   begin
      H.Saved_At := H.Done;
      H.Saved_Valid := True;
   end Mark_Saved;

   procedure Mark_Saved_At (H : in out History; Done : Entry_Count; In_Era : Natural) is
   begin
      if In_Era = H.Epoch and then Done <= H.Total then
         H.Saved_At := Done;
         H.Saved_Valid := True;
      end if;
   end Mark_Saved_At;


   procedure Clear (H : in out History) is
   begin
      Next_Epoch (H);
      H.Done := 0;
      H.Total := 0;
      H.Record_Used := 0;
      H.Piece_Used := 0;
      H.Selection_Used := 0;
      H.Open := False;
      H.Saved_Valid := False;
   end Clear;

   procedure Forget_Oldest (H : in out History) is
      Gone : constant Entry_Count := Natural'Max (1, H.Done / 2);
   begin
      if H.Done <= 1 or else Gone >= H.Done then
         Clear (H);
         return;
      end if;
      Next_Epoch (H);
      declare
         Records_Gone : constant Entry_Count := Natural'Min (H.Group_Log (Gone).Record_End, H.Record_Used);
         Pieces_Gone : constant Entry_Count := Natural'Min (H.Group_Log (Gone).Piece_End, H.Piece_Used);
         Selections_Gone : constant Entry_Count :=
           Natural'Min (H.Group_Log (Gone).Selection_End, H.Selection_Used);
         function Less (Value, Amount : Entry_Count) return Entry_Count is
           (if Value > Amount then Value - Amount else 0);
         function Less_Index (Value : Entry_Index; Amount : Entry_Count) return Entry_Index is
           (if Value > Amount then Value - Amount else 1);
      begin
         if Records_Gone < H.Record_Used then
            H.Record_Log (1 .. H.Record_Used - Records_Gone) := H.Record_Log (Records_Gone + 1 .. H.Record_Used);
         end if;
         H.Record_Used := H.Record_Used - Records_Gone;
         for K in 1 .. H.Record_Used loop
            H.Record_Log (K).Pieces_First := Less_Index (H.Record_Log (K).Pieces_First, Pieces_Gone);
         end loop;
         if Pieces_Gone < H.Piece_Used then
            H.Piece_Log (1 .. H.Piece_Used - Pieces_Gone) := H.Piece_Log (Pieces_Gone + 1 .. H.Piece_Used);
         end if;
         H.Piece_Used := H.Piece_Used - Pieces_Gone;
         if Selections_Gone < H.Selection_Used then
            H.Selection_Log (1 .. H.Selection_Used - Selections_Gone) :=
              H.Selection_Log (Selections_Gone + 1 .. H.Selection_Used);
         end if;
         H.Selection_Used := H.Selection_Used - Selections_Gone;
         H.Group_Log (1 .. H.Total - Gone) := H.Group_Log (Gone + 1 .. H.Total);
         for K in 1 .. H.Total - Gone loop
            declare
               G : Group renames H.Group_Log (K);
            begin
               G.Records_First := Less_Index (G.Records_First, Records_Gone);
               G.Before_First := Less_Index (G.Before_First, Selections_Gone);
               G.After_First := Less_Index (G.After_First, Selections_Gone);
               G.Record_End := Less (G.Record_End, Records_Gone);
               G.Piece_End := Less (G.Piece_End, Pieces_Gone);
               G.Selection_End := Less (G.Selection_End, Selections_Gone);
            end;
         end loop;
         if H.Saved_At < Gone then
            H.Saved_Valid := False;
         else
            H.Saved_At := H.Saved_At - Gone;
         end if;
         H.Done := H.Done - Gone;
         H.Total := H.Total - Gone;
      end;
   end Forget_Oldest;
end Text_History;
