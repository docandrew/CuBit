with Ada.Unchecked_Deallocation;
with Text_Layout;
with Text_UTF8;

package body Text_Documents is
   use type Text_Syntax.Language;
   use type Text_Syntax.Line_State;

   SEARCH_WINDOW : constant := 65_536;
   SPACE : constant Character := ' ';

   procedure Free is new Ada.Unchecked_Deallocation (Document, Document_Access);
   procedure Free is new Ada.Unchecked_Deallocation (String, Name_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Checkpoint_Array, Checkpoint_Access);
   procedure Free_Cache is new Ada.Unchecked_Deallocation (Line_Cache, Line_Cache_Access);

   function Limits_For (File_Bytes, Edit_Bytes : Byte_Count) return Limits is
      Stored : constant Byte_Count := File_Bytes + Edit_Bytes + Text_Stores.CHUNK_BYTES;
      --  Pieces: the file cut into pieces, then a piece or two per edit,
      --  blocks half full after splits.
      Pieces : constant Byte_Count := Stored / (MAXIMUM_PIECE_BYTES / 2) + 65_536;
      Blocks : constant Byte_Count :=
        Byte_Count'Min (Pieces / (Text_Pieces.BLOCK_PIECES / 2) + 16, Text_Pieces.MAXIMUM_BLOCKS);
   begin
      return (Store_Bytes => Byte_Count'Min (Stored, MAXIMUM_BYTES),
              Piece_Blocks => Text_Pieces.Block_Index (Blocks),
              Selections => 65_536, Marks => 4_096,
              History => (Groups => 65_536, Records => 262_144,
                          Pieces => Text_History.Entry_Index (Byte_Count'Min (Pieces * 2, 2 ** 24)),
                          Selections => 262_144));
   end Limits_For;

   function Create (Settings : Limits; Name : String) return Document_Access is
      D : constant Document_Access :=
        new Document (Settings.Piece_Blocks, Settings.Selections, Settings.Marks, Settings.History.Groups,
                      Settings.History.Records, Settings.History.Pieces, Settings.History.Selections);
   begin
      Text_Stores.Create (D.Store, Settings.Store_Bytes);
      Text_Pieces.Initialize (D.Pieces);
      Text_Cursors.Initialize (D.Cursors, 0);
      Text_History.Initialize (D.History);
      Text_Marks.Initialize (D.Marks);
      D.Title := new String'(Name);
      D.Lines_Cache := new Line_Cache;
      D.Kind := Text_Syntax.Language_Of (Name);
      D.Checkpoints := new Checkpoint_Array'(0 .. 63 => Text_Syntax.INITIAL_STATE);
      D.Valid_Checkpoints := 1;
      return D;
   end Create;

   procedure Destroy (D : in out Document_Access) is
   begin
      if D /= null then
         Text_Stores.Destroy (D.Store);
         Free (D.Title);
         Free (D.Checkpoints);
         Free_Cache (D.Lines_Cache);
         Free (D);
      end if;
   end Destroy;

   function Name (D : Document) return String is (D.Title.all);
   procedure Set_Name (D : in out Document; Value : String) is
   begin
      Free (D.Title);
      D.Title := new String'(Value);
   end Set_Name;
   function Language (D : Document) return Text_Syntax.Language is (D.Kind);
   procedure Set_Language (D : in out Document; Value : Text_Syntax.Language) is
   begin
      D.Kind := Value;
      D.Valid_Checkpoints := 1;
      D.Revision_Count := D.Revision_Count + 1;
   end Set_Language;

   procedure Changed (D : in out Document) is
   begin
      D.Revision_Count := D.Revision_Count + 1;
   end Changed;

   ---------------------------------------------------------------------------
   --  Reading.

   function Length (D : Document) return Byte_Count is (Text_Pieces.Length (D.Pieces));
   function Line_Total (D : Document) return Line_Number is
     (Line_Number (Text_Pieces.Line_Feeds (D.Pieces) + 1));
   function Revision (D : Document) return Unsigned_64 is (D.Revision_Count);
   function Modified (D : Document) return Boolean is (not Text_History.Is_Saved (D.History));
   function Uses_CRLF (D : Document) return Boolean is (D.CRLF);
   function Loading (D : Document) return Boolean is (D.Is_Loading);
   function Expected_Bytes (D : Document) return Byte_Count is (D.Expected);
   function Last_Refusal (D : Document) return Refusal is (D.Refused);
   function Pieces_Used (D : Document) return Natural is (Text_Pieces.Piece_Count (D.Pieces));

   procedure Mark_Saved (D : in out Document) is
   begin
      Text_History.Mark_Saved (D.History);
      Changed (D);
   end Mark_Saved;

   function Byte_At (D : Document; Offset : Byte_Offset) return Character is
      P : constant Text_Pieces.Position := Text_Pieces.Locate (D.Pieces, Offset);
      Item : constant Text_Pieces.Piece := Text_Pieces.Element (D.Pieces, P);
   begin
      return Text_Stores.Element (D.Store, Item.First + Byte_Count (P.Within));
   end Byte_At;

   procedure Read (D : Document; Offset : Byte_Offset; Into : out String) is
      P : Text_Pieces.Position;
      Done : Natural := 0;
      Found : Boolean := True;
   begin
      if Into'Length = 0 then
         return;
      end if;
      P := Text_Pieces.Locate (D.Pieces, Offset);
      while Done < Into'Length and then Found loop
         declare
            Item : constant Text_Pieces.Piece := Text_Pieces.Element (D.Pieces, P);
            Take : constant Natural :=
              Natural'Min (Into'Length - Done, Natural (Item.Length) - Natural (P.Within));
         begin
            if Take > 0 then
               Text_Stores.Read (D.Store, Item.First + Byte_Count (P.Within),
                                 Into (Into'First + Done .. Into'First + Done + Take - 1));
               Done := Done + Take;
            end if;
            exit when Done >= Into'Length;
            Text_Pieces.Next (D.Pieces, P, Found);
         end;
      end loop;
   end Read;

   function Slice (D : Document; First, Last : Byte_Offset) return String is
      Result : String (1 .. Natural (Last - First));
   begin
      Read (D, First, Result);
      return Result;
   end Slice;

   function Content (D : Document) return String is (Slice (D, 0, Length (D)));

   --  How many pieces Line_Start scans on from the line before before it
   --  finds the line by its feed count instead.
   FORWARD_PIECES : constant := 4;

   function Line_Start (D : Document; L : Line_Number) return Byte_Offset is
      Cache : Line_Cache renames D.Lines_Cache.all;
      Slot : constant Natural := Natural (L mod LINE_CACHE_SIZE);
   begin
      if L = 1 then
         return 0;
      elsif L > Line_Total (D) then
         return Length (D);
      end if;
      if Cache.Revision /= D.Revision_Count then
         Cache.Revision := D.Revision_Count;
         Cache.Entries := [others => (others => <>)];
      elsif Cache.Entries (Slot).Line = L then
         return Cache.Entries (Slot).Start;
      end if;
      --  The line before is known (rows are laid out in order): scan on
      --  from its start to its line feed, a line's bytes instead of a
      --  piece's prefix.
      declare
         Before : constant Natural := Natural ((L - 1) mod LINE_CACHE_SIZE);
      begin
         if Cache.Entries (Before).Line = L - 1 and then Cache.Entries (Before).Start < Length (D) then
            declare
               P : Text_Pieces.Position := Text_Pieces.Locate (D.Pieces, Cache.Entries (Before).Start);
               Found : Boolean := True;
            begin
               for Step in 1 .. FORWARD_PIECES loop
                  declare
                     Item : constant Text_Pieces.Piece := Text_Pieces.Element (D.Pieces, P);
                     Rest : constant Piece_Length := Item.Length - P.Within;
                     From : constant Byte_Offset := Item.First + Byte_Count (P.Within);
                     After : constant Piece_Length :=
                       (if Rest = 0 then 0 else Text_Stores.After_Feed (D.Store, From, Rest, 1));
                  begin
                     if Rest > 0 and then (After < Rest
                       or else Text_Stores.Element (D.Store, From + Byte_Count (After) - 1) = ASCII.LF)
                     then
                        declare
                           Start : constant Byte_Offset := P.Start + Byte_Count (P.Within) + Byte_Count (After);
                        begin
                           Cache.Entries (Slot) := (Line => L, Start => Start, others => <>);
                           return Start;
                        end;
                     end if;
                  end;
                  Text_Pieces.Next (D.Pieces, P, Found);
                  exit when not Found;
               end loop;
            end;
         end if;
      end;
      declare
         P : constant Text_Pieces.Position := Text_Pieces.Locate_Feed (D.Pieces, Byte_Count (L - 1));
         Item : constant Text_Pieces.Piece := Text_Pieces.Element (D.Pieces, P);
         K : constant Byte_Count := Byte_Count (L - 1) - P.Feeds_Before;
         Start : constant Byte_Offset :=
           P.Start + Byte_Count (Text_Stores.After_Feed (D.Store, Item.First, Item.Length, Piece_Length (K)));
      begin
         Cache.Entries (Slot) := (Line => L, Start => Start, others => <>);
         return Start;
      end;
   end Line_Start;

   function Line_End (D : Document; L : Line_Number) return Byte_Offset is
      Cache : Line_Cache renames D.Lines_Cache.all;
      Slot : constant Natural := Natural (L mod LINE_CACHE_SIZE);
   begin
      if L >= Line_Total (D) then
         return Length (D);
      end if;
      if Cache.Revision = D.Revision_Count and then Cache.Entries (Slot).Line = L
        and then Cache.Entries (Slot).Has_Stop
      then
         return Cache.Entries (Slot).Stop;
      end if;
      declare
         Start : constant Byte_Offset := Line_Start (D, L);
         Feed : constant Byte_Offset := Line_Start (D, L + 1) - 1;
         Stop : constant Byte_Offset :=
           (if Feed > Start and then Byte_At (D, Feed - 1) = ASCII.CR then Feed - 1 else Feed);
      begin
         if Cache.Revision = D.Revision_Count then
            Cache.Entries (Slot) := (Line => L, Start => Start, Has_Stop => True, Stop => Stop);
         end if;
         return Stop;
      end;
   end Line_End;

   function Line_Of (D : Document; Offset : Byte_Offset) return Line_Number is
   begin
      if Length (D) = 0 then
         return 1;
      end if;
      declare
         P : constant Text_Pieces.Position := Text_Pieces.Locate (D.Pieces, Byte_Offset'Min (Offset, Length (D)));
         Item : constant Text_Pieces.Piece := Text_Pieces.Element (D.Pieces, P);
      begin
         return Line_Number (P.Feeds_Before + Byte_Count (Text_Stores.Line_Feeds (D.Store, Item.First, P.Within)) + 1);
      end;
   end Line_Of;

   function Take_Snapshot (D : Document) return Snapshot_Access is
      Count : constant Natural := Text_Pieces.Piece_Count (D.Pieces);
      Result : constant Snapshot_Access := new Snapshot;
      P : Text_Pieces.Position;
      Found : Boolean := Count > 0;
   begin
      Result.Pieces := new Piece_List (1 .. Count);
      Result.Length := Length (D);
      Result.Done := Text_History.Done_Groups (D.History);
      Result.Era := Text_History.Era (D.History);
      if Found then
         P := Text_Pieces.Locate (D.Pieces, 0);
         for K in 1 .. Count loop
            Result.Pieces (K) := Text_Pieces.Element (D.Pieces, P);
            Text_Pieces.Next (D.Pieces, P, Found);
            exit when not Found;
         end loop;
      end if;
      return Result;
   end Take_Snapshot;

   function Snapshot_Length (S : Snapshot) return Byte_Count is (S.Length);

   procedure Read_Snapshot (D : Document; S : Snapshot; Offset : Byte_Offset; Into : out String) is
      Start : Byte_Offset := 0;
      Done : Natural := 0;
   begin
      --  Pieces are read in order: find the first, then copy on.
      for K in S.Pieces'Range loop
         exit when Done >= Into'Length;
         declare
            Item : constant Text_Pieces.Piece := S.Pieces (K);
            Stop : constant Byte_Offset := Start + Byte_Count (Item.Length);
         begin
            if Stop > Offset + Byte_Count (Done) then
               declare
                  From : constant Byte_Offset := Offset + Byte_Count (Done) - Start;
                  Take : constant Natural := Natural'Min (Into'Length - Done, Natural (Stop - Start - From));
               begin
                  Text_Stores.Read (D.Store, Item.First + From, Into (Into'First + Done .. Into'First + Done + Take - 1));
                  Done := Done + Take;
               end;
            end if;
            Start := Stop;
         end;
      end loop;
   end Read_Snapshot;

   procedure Mark_Snapshot_Saved (D : in out Document; S : Snapshot) is
   begin
      Text_History.Mark_Saved_At (D.History, S.Done, S.Era);
      Changed (D);
   end Mark_Snapshot_Saved;

   procedure Free_Snapshot (S : in out Snapshot_Access) is
      procedure Free_List is new Ada.Unchecked_Deallocation (Piece_List, Piece_List_Access);
      procedure Free_Snapshot_Record is new Ada.Unchecked_Deallocation (Snapshot, Snapshot_Access);
   begin
      if S /= null then
         Free_List (S.Pieces);
         Free_Snapshot_Record (S);
      end if;
   end Free_Snapshot;

   ---------------------------------------------------------------------------
   --  Segments.

   function Line_Bytes (D : Document; L : Line_Number) return Byte_Count is (Line_End (D, L) - Line_Start (D, L));

   --  Segments start every SEGMENT_STRIDE bytes, moved back to a code point
   --  start (at most three bytes): none is longer than SEGMENT_BYTES.
   SEGMENT_STRIDE : constant := SEGMENT_BYTES - Text_UTF8.MAXIMUM_SEQUENCE;

   function Segment_Count (D : Document; L : Line_Number) return Positive is
     (Positive'Max (1, Natural ((Line_Bytes (D, L) + SEGMENT_STRIDE - 1) / SEGMENT_STRIDE)));

   function Segment (D : Document; L : Line_Number; Index : Natural) return Segment_Place is
      Start : constant Byte_Offset := Line_Start (D, L);
      Stop : constant Byte_Offset := Line_End (D, L);
      --  Back to a code point start (at most three continuation bytes).
      function Adjust (Offset : Byte_Offset) return Byte_Offset is
         Result : Byte_Offset := Offset;
      begin
         for Step in 1 .. Text_UTF8.MAXIMUM_SEQUENCE - 1 loop
            exit when Result <= Start or else Result >= Stop
              or else not Text_UTF8.Is_Continuation (Byte_At (D, Result));
            Result := Result - 1;
         end loop;
         return Result;
      end Adjust;
      First : constant Byte_Offset :=
        (if Index = 0 then Start else Adjust (Byte_Offset'Min (Start + Byte_Count (Index) * SEGMENT_STRIDE, Stop)));
      Next : constant Byte_Offset := Start + Byte_Count (Index + 1) * SEGMENT_STRIDE;
   begin
      return (Line => L, Index => Index, First => First,
              Last => (if Next >= Stop then Stop else Adjust (Next)));
   end Segment;

   function Segment_Of (D : Document; Offset : Byte_Offset) return Segment_Place is
      L : constant Line_Number := Line_Of (D, Offset);
      Start : constant Byte_Offset := Line_Start (D, L);
      Count : constant Positive := Segment_Count (D, L);
      Index : Natural := Natural'Min (Natural ((Offset - Byte_Offset'Min (Offset, Start)) / SEGMENT_STRIDE), Count - 1);
      Place : Segment_Place := Segment (D, L, Index);
   begin
      if Offset < Place.First and then Index > 0 then
         Index := Index - 1;
         Place := Segment (D, L, Index);
      end if;
      return Place;
   end Segment_Of;

   function Effective_Wrap (D : Document; Settings : Layout_Settings; L : Line_Number) return Column_Count is
     (if Settings.Wrap > 0 then Settings.Wrap
      elsif Line_Bytes (D, L) > SEGMENT_STRIDE then Natural'Max (Settings.Window_Columns, 1)
      else 0);

   function Segment_Text (D : Document; Place : Segment_Place) return String is (Slice (D, Place.First, Place.Last));

   ---------------------------------------------------------------------------
   --  Selections.

   function Selections (D : Document) return Text_Cursors.Set is (D.Cursors);
   function Selection_Count (D : Document) return Natural is (Text_Cursors.Count (D.Cursors));
   function Selection (D : Document; Index : Positive) return Text_Cursors.Selection is
     (Text_Cursors.Element (D.Cursors, Index));
   function Primary_Index (D : Document) return Positive is (Text_Cursors.Primary (D.Cursors));
   function Primary (D : Document) return Text_Cursors.Selection is
     (Text_Cursors.Element (D.Cursors, Text_Cursors.Primary (D.Cursors)));

   --  An offset inside a CR LF pair or a UTF-8 sequence moves back to its
   --  start: carets and selection edges are always at code point starts.
   function Snap (D : Document; Offset : Byte_Offset) return Byte_Offset is
      Result : Byte_Offset := Byte_Offset'Min (Offset, Length (D));
   begin
      if Result = 0 or else Result >= Length (D) then
         return Result;
      end if;
      if Byte_At (D, Result) = ASCII.LF and then Byte_At (D, Result - 1) = ASCII.CR then
         return Result - 1;
      end if;
      for Step in 1 .. Text_UTF8.MAXIMUM_SEQUENCE - 1 loop
         exit when Result = 0 or else not Text_UTF8.Is_Continuation (Byte_At (D, Result));
         Result := Result - 1;
      end loop;
      return Result;
   end Snap;

   procedure Set_Caret (D : in out Document; Offset : Byte_Offset; Extend : Boolean := False) is
      P : constant Text_Cursors.Selection := Primary (D);
      At_Offset : constant Byte_Offset := Snap (D, Offset);
   begin
      Text_Cursors.Keep_Primary (D.Cursors);
      Text_Cursors.Replace
        (D.Cursors, 1, (Anchor => (if Extend then P.Anchor else At_Offset), Head => At_Offset, others => <>));
      Text_History.Seal (D.History);
   end Set_Caret;

   procedure Set_Selection (D : in out Document; Anchor, Head : Byte_Offset) is
   begin
      Text_Cursors.Keep_Primary (D.Cursors);
      Text_Cursors.Replace (D.Cursors, 1, (Anchor => Snap (D, Anchor), Head => Snap (D, Head), others => <>));
      Text_History.Seal (D.History);
   end Set_Selection;

   procedure Add_Selection (D : in out Document; Anchor, Head : Byte_Offset) is
      Added : Boolean;
   begin
      Text_Cursors.Add (D.Cursors, (Anchor => Snap (D, Anchor), Head => Snap (D, Head), others => <>), True, Added);
      Text_Cursors.Normalize (D.Cursors);
      Text_History.Seal (D.History);
   end Add_Selection;

   procedure Keep_Primary (D : in out Document) is
   begin
      Text_Cursors.Keep_Primary (D.Cursors);
   end Keep_Primary;

   procedure Collapse (D : in out Document) is
   begin
      for K in 1 .. Selection_Count (D) loop
         Text_Cursors.Replace (D.Cursors, K, Text_Cursors.Caret (Text_Cursors.First_Of (Selection (D, K))));
      end loop;
      Text_Cursors.Normalize (D.Cursors);
   end Collapse;

   procedure Select_All (D : in out Document) is
   begin
      Set_Selection (D, 0, Length (D));
   end Select_All;

   function Selected_Text (D : Document; Separator : String := [1 => ASCII.LF]) return String is
      Total : Natural := 0;
   begin
      for K in 1 .. Selection_Count (D) loop
         declare
            S : constant Text_Cursors.Selection := Selection (D, K);
         begin
            Total := Total + Natural (Text_Cursors.Last_Of (S) - Text_Cursors.First_Of (S))
              + (if K > 1 then Separator'Length else 0);
         end;
      end loop;
      declare
         Result : String (1 .. Total);
         At_Index : Natural := 0;
      begin
         for K in 1 .. Selection_Count (D) loop
            declare
               S : constant Text_Cursors.Selection := Selection (D, K);
               Part : constant String := Slice (D, Text_Cursors.First_Of (S), Text_Cursors.Last_Of (S));
            begin
               if K > 1 then
                  Result (At_Index + 1 .. At_Index + Separator'Length) := Separator;
                  At_Index := At_Index + Separator'Length;
               end if;
               Result (At_Index + 1 .. At_Index + Part'Length) := Part;
               At_Index := At_Index + Part'Length;
            end;
         end loop;
         return Result;
      end;
   end Selected_Text;

   ---------------------------------------------------------------------------
   --  Movement.

   function Next_Position (D : Document; H : Byte_Offset) return Byte_Offset is
      L : Line_Number;
   begin
      if H >= Length (D) then
         return Length (D);
      end if;
      L := Line_Of (D, H);
      if H >= Line_End (D, L) then
         return Line_Start (D, L + 1);
      end if;
      declare
         Place : constant Segment_Place := Segment_Of (D, H);
         Text : constant String := Segment_Text (D, Place);
      begin
         if Text'Length = 0 then
            return H + 1;
         end if;
         return Place.First + Byte_Count (Text_Layout.Next_Boundary (Text, Natural (H - Place.First) + 1) - 1);
      end;
   end Next_Position;

   function Previous_Position (D : Document; H : Byte_Offset) return Byte_Offset is
      L : Line_Number;
   begin
      if H = 0 then
         return 0;
      end if;
      L := Line_Of (D, H);
      if H <= Line_Start (D, L) then
         return Line_End (D, L - 1);
      elsif H > Line_End (D, L) then
         return Line_End (D, L);
      end if;
      declare
         Here : constant Segment_Place := Segment_Of (D, H);
         Place : constant Segment_Place :=
           (if H = Here.First and then Here.Index > 0 then Segment (D, L, Here.Index - 1) else Here);
         Text : constant String := Segment_Text (D, Place);
      begin
         if Text'Length = 0 then
            return H - 1;
         end if;
         return Place.First
           + Byte_Count (Text_Layout.Previous_Boundary (Text, Natural (H - Place.First) + 1) - 1);
      end;
   end Previous_Position;

   function Word_Byte (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' or else Character'Pos (C) >= 16#80#);

   function Word_Right (D : Document; H : Byte_Offset) return Byte_Offset is
      L : constant Line_Number := Line_Of (D, H);
      Stop : constant Byte_Offset := Line_End (D, L);
   begin
      if H >= Stop then
         return Next_Position (D, H);
      end if;
      declare
         Place : constant Segment_Place := Segment_Of (D, H);
         Text : constant String := Segment_Text (D, Place);
         I : Natural := Natural (H - Place.First) + 1;
      begin
         while I <= Text'Last and then not Word_Byte (Text (I)) loop
            I := I + 1;
         end loop;
         while I <= Text'Last and then Word_Byte (Text (I)) loop
            I := I + 1;
         end loop;
         return Place.First + Byte_Count (I - 1);
      end;
   end Word_Right;

   function Word_Left (D : Document; H : Byte_Offset) return Byte_Offset is
      L : constant Line_Number := Line_Of (D, H);
   begin
      if H <= Line_Start (D, L) then
         return Previous_Position (D, H);
      end if;
      declare
         Here : constant Segment_Place := Segment_Of (D, H);
         Place : constant Segment_Place :=
           (if H = Here.First and then Here.Index > 0 then Segment (D, L, Here.Index - 1) else Here);
         Text : constant String := Segment_Text (D, Place);
         I : Natural := Natural (H - Place.First);
      begin
         while I >= 1 and then not Word_Byte (Text (I)) loop
            I := I - 1;
         end loop;
         while I >= 1 and then Word_Byte (Text (I)) loop
            I := I - 1;
         end loop;
         return Place.First + Byte_Count (I);
      end;
   end Word_Left;

   --  Smart home: the first non-blank of the line, or its start when
   --  already there.
   function Home_Of (D : Document; H : Byte_Offset) return Byte_Offset is
      L : constant Line_Number := Line_Of (D, H);
      Start : constant Byte_Offset := Line_Start (D, L);
      Place : constant Segment_Place := Segment (D, L, 0);
      Text : constant String := Segment_Text (D, Place);
      I : Natural := 1;
   begin
      while I <= Text'Last and then Text (I) in ' ' | ASCII.HT loop
         I := I + 1;
      end loop;
      return (if H = Start + Byte_Count (I - 1) then Start else Start + Byte_Count (I - 1));
   end Home_Of;

   --  Rows of a segment.
   type Row_Place is record
      Segment : Segment_Place;
      Row : Positive := 1;
      Rows : Positive := 1;
      First, Last : Byte_Offset := 0;   --  the row's bytes; Last exclusive
   end record;

   function Row_In (D : Document; Settings : Layout_Settings; Place : Segment_Place; Row : Integer) return Row_Place
   is
      Text : constant String := Segment_Text (D, Place);
      Starts : Text_Layout.Row_Starts;
      Count : Text_Layout.Row_Index;
      R : Positive;
   begin
      Text_Layout.Wrap (Text, Effective_Wrap (D, Settings, Place.Line), Settings.Tab, Starts, Count);
      R := (if Row < 1 or else Row > Count then Count else Row);
      return (Segment => Place, Row => R, Rows => Count,
              First => Place.First + Byte_Count (Starts (R) - 1),
              Last => (if R < Count then Place.First + Byte_Count (Starts (R + 1) - 1) else Place.Last));
   end Row_In;

   --  The row holding H (a caret at a soft row break shows on the later row).
   function Row_Of (D : Document; Settings : Layout_Settings; H : Byte_Offset) return Row_Place is
      Place : constant Segment_Place := Segment_Of (D, H);
      Text : constant String := Segment_Text (D, Place);
      Starts : Text_Layout.Row_Starts;
      Count : Text_Layout.Row_Index;
      R : Positive := 1;
      Within : constant Positive := Natural (H - Byte_Offset'Min (H, Place.First)) + 1;
   begin
      Text_Layout.Wrap (Text, Effective_Wrap (D, Settings, Place.Line), Settings.Tab, Starts, Count);
      while R < Count and then Starts (R + 1) <= Within loop
         R := R + 1;
      end loop;
      return (Segment => Place, Row => R, Rows => Count,
              First => Place.First + Byte_Count (Starts (R) - 1),
              Last => (if R < Count then Place.First + Byte_Count (Starts (R + 1) - 1) else Place.Last));
   end Row_Of;

   function Column_In_Row (D : Document; Settings : Layout_Settings; Row : Row_Place; H : Byte_Offset)
                           return Column_Count
   is
      Text : constant String := Slice (D, Row.First, Row.Last);
   begin
      return Text_Layout.Column_Of (Text, Natural (Byte_Offset'Min (H, Row.Last) - Row.First) + 1, Settings.Tab);
   end Column_In_Row;

   function Offset_In_Row (D : Document; Settings : Layout_Settings; Row : Row_Place; Column : Column_Count)
                           return Byte_Offset
   is
      Text : constant String := Slice (D, Row.First, Row.Last);
      Index : Positive := Text_Layout.Index_At_Column (Text, Column, Settings.Tab);
   begin
      --  The end of a row that wraps is the next row's start: stay on this one.
      if Index > Text'Last and then Row.Row < Row.Rows and then Text'Length > 0 then
         Index := Text_Layout.Previous_Boundary (Text, Index);
      end if;
      return Row.First + Byte_Count (Index - 1);
   end Offset_In_Row;

   --  The row after or before Row (None at the document's ends).
   procedure Neighbour_Row
     (D : Document; Settings : Layout_Settings; Row : Row_Place; Down : Boolean; Result : out Row_Place;
      Found : out Boolean)
   is
      L : constant Line_Number := Row.Segment.Line;
   begin
      Found := True;
      if Down then
         if Row.Row < Row.Rows then
            Result := Row_In (D, Settings, Row.Segment, Row.Row + 1);
         elsif Row.Segment.Index + 1 < Segment_Count (D, L) then
            Result := Row_In (D, Settings, Segment (D, L, Row.Segment.Index + 1), 1);
         elsif L < Line_Total (D) then
            Result := Row_In (D, Settings, Segment (D, L + 1, 0), 1);
         else
            Found := False;
            Result := Row;
         end if;
      else
         if Row.Row > 1 then
            Result := Row_In (D, Settings, Row.Segment, Row.Row - 1);
         elsif Row.Segment.Index > 0 then
            Result := Row_In (D, Settings, Segment (D, L, Row.Segment.Index - 1), Integer'Last);
         elsif L > 1 then
            Result := Row_In (D, Settings, Segment (D, L - 1, Segment_Count (D, L - 1) - 1), Integer'Last);
         else
            Found := False;
            Result := Row;
         end if;
      end if;
   end Neighbour_Row;

   procedure Vertical
     (D : Document; Settings : Layout_Settings; S : in out Text_Cursors.Selection; Down : Boolean; Rows : Positive)
   is
      Row : Row_Place := Row_Of (D, Settings, S.Head);
      Next : Row_Place;
      Found : Boolean;
      Goal : constant Column_Count := (if S.Has_Goal then S.Goal else Column_In_Row (D, Settings, Row, S.Head));
   begin
      for Step in 1 .. Rows loop
         Neighbour_Row (D, Settings, Row, Down, Next, Found);
         if not Found then
            S.Head := (if Down then Length (D) else 0);
            S.Goal := Goal;
            S.Has_Goal := True;
            return;
         end if;
         Row := Next;
      end loop;
      S.Head := Offset_In_Row (D, Settings, Row, Goal);
      S.Goal := Goal;
      S.Has_Goal := True;
   end Vertical;

   procedure Move
     (D : in out Document; Where : Motion; Extend : Boolean; Settings : Layout_Settings; Page_Rows : Positive := 1)
   is
   begin
      for K in 1 .. Selection_Count (D) loop
         declare
            S : Text_Cursors.Selection := Selection (D, K);
            Collapsing : constant Boolean := not Extend and then not Text_Cursors.Is_Empty (S);
         begin
            case Where is
               when Left =>
                  S.Head := (if Collapsing then Text_Cursors.First_Of (S) else Previous_Position (D, S.Head));
               when Right =>
                  S.Head := (if Collapsing then Text_Cursors.Last_Of (S) else Next_Position (D, S.Head));
               when Word_Left => S.Head := Word_Left (D, S.Head);
               when Word_Right => S.Head := Word_Right (D, S.Head);
               when Line_Home => S.Head := Home_Of (D, S.Head);
               when Line_End_Motion => S.Head := Line_End (D, Line_Of (D, S.Head));
               when Up | Down => Vertical (D, Settings, S, Where = Down, 1);
               when Page_Up | Page_Down => Vertical (D, Settings, S, Where = Page_Down, Page_Rows);
               when Document_Start => S.Head := 0;
               when Document_End => S.Head := Length (D);
            end case;
            if Where not in Up | Down | Page_Up | Page_Down then
               S.Has_Goal := False;
            end if;
            if not Extend then
               S.Anchor := S.Head;
            end if;
            Text_Cursors.Replace (D.Cursors, K, S);
         end;
      end loop;
      Text_Cursors.Normalize (D.Cursors);
      Text_History.Seal (D.History);
   end Move;

   procedure Add_Cursor_Vertically (D : in out Document; Down : Boolean; Settings : Layout_Settings) is
      S : Text_Cursors.Selection := Primary (D);
      Added : Boolean;
   begin
      S.Anchor := S.Head;
      Vertical (D, Settings, S, Down, 1);
      S.Anchor := S.Head;
      Text_Cursors.Add (D.Cursors, S, True, Added);
      Text_Cursors.Normalize (D.Cursors);
      Text_History.Seal (D.History);
   end Add_Cursor_Vertically;

   procedure Select_Column_Box
     (D : in out Document; Anchor_Offset, Head_Offset : Byte_Offset; Settings : Layout_Settings)
   is
      A_Line : constant Line_Number := Line_Of (D, Anchor_Offset);
      H_Line : constant Line_Number := Line_Of (D, Head_Offset);
      A_Row : constant Row_Place := Row_Of (D, Settings, Anchor_Offset);
      H_Row : constant Row_Place := Row_Of (D, Settings, Head_Offset);
      A_Column : constant Column_Count := Column_In_Row (D, Settings, A_Row, Anchor_Offset);
      H_Column : constant Column_Count := Column_In_Row (D, Settings, H_Row, Head_Offset);
      Low : constant Line_Number := Line_Number'Min (A_Line, H_Line);
      High : constant Line_Number := Line_Number'Max (A_Line, H_Line);
      First_Done : Boolean := False;
      Added : Boolean;
   begin
      for L in Low .. High loop
         declare
            Row : constant Row_Place := Row_In (D, Settings, Segment (D, L, 0), 1);
            S : constant Text_Cursors.Selection :=
              (Anchor => Offset_In_Row (D, Settings, Row, A_Column),
               Head => Offset_In_Row (D, Settings, Row, H_Column), Goal => H_Column, Has_Goal => True);
         begin
            if not First_Done then
               Text_Cursors.Initialize (D.Cursors, 0);
               Text_Cursors.Replace (D.Cursors, 1, S);
               First_Done := True;
            else
               Text_Cursors.Add (D.Cursors, S, L = H_Line, Added);
               exit when not Added;
            end if;
         end;
      end loop;
      Text_Cursors.Normalize (D.Cursors);
      Text_History.Seal (D.History);
   end Select_Column_Box;

   function Word_Around (D : Document; H : Byte_Offset) return Text_Cursors.Selection is
      Place : constant Segment_Place := Segment_Of (D, H);
      Text : constant String := Segment_Text (D, Place);
      I : Natural := Natural (H - Place.First);   --  bytes before H in the segment
      J : Natural := I;
   begin
      while I >= 1 and then Word_Byte (Text (I)) loop
         I := I - 1;
      end loop;
      while J < Text'Last and then Word_Byte (Text (J + 1)) loop
         J := J + 1;
      end loop;
      return (Anchor => Place.First + Byte_Count (I), Head => Place.First + Byte_Count (J), others => <>);
   end Word_Around;

   procedure Select_Word_Or_Next (D : in out Document) is
      P : constant Text_Cursors.Selection := Primary (D);
   begin
      if Text_Cursors.Is_Empty (P) then
         for K in 1 .. Selection_Count (D) loop
            Text_Cursors.Replace (D.Cursors, K, Word_Around (D, Selection (D, K).Head));
         end loop;
         Text_Cursors.Normalize (D.Cursors);
         return;
      end if;
      --  The next occurrence of the primary selection's text after the last selection.
      declare
         Needle : constant String := Slice (D, Text_Cursors.First_Of (P), Text_Cursors.Last_Of (P));
         Settings : constant Text_Search.Options := (Case_Sensitive => True, others => False);
         From : constant Byte_Offset := Text_Cursors.Last_Of (Selection (D, Selection_Count (D)));
         Position : Byte_Offset := From;
         Wrapped : Boolean := False;
      begin
         if Needle'Length = 0 or else Needle'Length > Text_Search.MAXIMUM_PATTERN then
            return;
         end if;
         loop
            declare
               Count : constant Byte_Count := Byte_Count'Min (SEARCH_WINDOW, Length (D) - Position);
               Window : String (1 .. Natural (Count));
               Found : Boolean;
               At_Index : Positive;
            begin
               Read (D, Position, Window);
               Text_Search.Find_Literal (Window, Needle, 1, Settings, SPACE, SPACE, Found, At_Index);
               if Found then
                  declare
                     First : constant Byte_Offset := Position + Byte_Count (At_Index - 1);
                  begin
                     if Wrapped and then First >= Text_Cursors.First_Of (P) then
                        return;
                     end if;
                     Add_Selection (D, First, First + Needle'Length);
                     return;
                  end;
               end if;
               if Position + Count >= Length (D) then
                  exit when Wrapped;
                  Wrapped := True;
                  Position := 0;
               else
                  Position := Position + Count - Byte_Count (Needle'Length - 1);
               end if;
            end;
         end loop;
      end;
   end Select_Word_Or_Next;

   procedure Select_Line (D : in out Document) is
   begin
      for K in 1 .. Selection_Count (D) loop
         declare
            S : constant Text_Cursors.Selection := Selection (D, K);
            Low : constant Line_Number := Line_Of (D, Text_Cursors.First_Of (S));
            High : constant Line_Number := Line_Of (D, Text_Cursors.Last_Of (S));
         begin
            Text_Cursors.Replace (D.Cursors, K, (Anchor => Line_Start (D, Low), Head => Line_Start (D, High + 1),
                                                 others => <>));
         end;
      end loop;
      Text_Cursors.Normalize (D.Cursors);
   end Select_Line;

   procedure Split_Into_Lines (D : in out Document) is
      Old : constant Text_Cursors.Set := D.Cursors;
      First : Boolean := True;
      Added : Boolean := True;
   begin
      for K in 1 .. Text_Cursors.Count (Old) loop
         declare
            S : constant Text_Cursors.Selection := Text_Cursors.Element (Old, K);
            Low : constant Line_Number := Line_Of (D, Text_Cursors.First_Of (S));
            High : constant Line_Number := Line_Of (D, Text_Cursors.Last_Of (S));
         begin
            for L in Low .. High loop
               declare
                  F : constant Byte_Offset := Byte_Offset'Max (Line_Start (D, L), Text_Cursors.First_Of (S));
                  E : constant Byte_Offset := Byte_Offset'Min (Line_End (D, L), Text_Cursors.Last_Of (S));
                  Item : constant Text_Cursors.Selection := (Anchor => F, Head => Byte_Offset'Max (F, E), others => <>);
               begin
                  if First then
                     Text_Cursors.Initialize (D.Cursors, 0);
                     Text_Cursors.Replace (D.Cursors, 1, Item);
                     First := False;
                  else
                     Text_Cursors.Add (D.Cursors, Item, False, Added);
                  end if;
               end;
               exit when not Added;
            end loop;
         end;
      end loop;
      Text_Cursors.Normalize (D.Cursors);
   end Split_Into_Lines;

   ---------------------------------------------------------------------------
   --  Highlighting invalidation.

   procedure Invalidate_From (D : in out Document; Offset : Byte_Offset) is
      L : constant Line_Number := Line_Of (D, Byte_Offset'Min (Offset, Length (D)));
      First_Invalid : constant Natural := Natural ((L - 1) / CHECKPOINT_LINES) + 1;
   begin
      if Line_Total (D) = D.Known_Lines then
         if First_Invalid < D.Valid_Checkpoints then
            D.Reusable_Checkpoints := Natural'Max (D.Reusable_Checkpoints, D.Valid_Checkpoints);
         end if;
      else
         D.Reusable_Checkpoints := 0;
         D.Known_Lines := Line_Total (D);
      end if;
      D.Valid_Checkpoints := Natural'Min (D.Valid_Checkpoints, First_Invalid);
   end Invalidate_From;

   ---------------------------------------------------------------------------
   --  Primitive edits: the piece table, the history, the marks.

   procedure Begin_Group
     (D : in out Document; Kind : Text_History.Group_Kind; At_Offset : Byte_Offset; Now_Us : Unsigned_64;
      Boundary : Boolean)
   is
      Continued, Ok : Boolean;
   begin
      Text_History.Begin_Group (D.History, Kind, D.Cursors, At_Offset, Now_Us, Boundary, Continued, Ok);
      if not Ok then
         Text_History.Forget_Oldest (D.History);
         Text_History.Begin_Group (D.History, Kind, D.Cursors, At_Offset, Now_Us, Boundary, Continued, Ok);
         if not Ok then
            Text_History.Clear (D.History);
            D.Refused := History_Dropped;
         end if;
      end if;
   end Begin_Group;

   procedure End_Group (D : in out Document; Continue_At : Byte_Offset) is
      Ok : Boolean;
   begin
      if Text_History.Group_Open (D.History) then
         Text_History.End_Group (D.History, D.Cursors, Continue_At, Ok);
         if not Ok then
            Text_History.Clear (D.History);
            D.Refused := History_Dropped;
         end if;
      end if;
   end End_Group;

   procedure History_Failed (D : in out Document) is
   begin
      --  An edit too large for the history: it applies, unrecorded.
      Text_History.Clear (D.History);
      D.Refused := History_Dropped;
   end History_Failed;

   --  Store Text and put it at Offset; Record it in the open group.
   procedure Raw_Insert (D : in out Document; Offset : Byte_Offset; Text : String; Ok : out Boolean) is
      Done : Natural := 0;
   begin
      Ok := True;
      while Done < Text'Length loop
         declare
            Take : constant Natural :=
              Natural (Byte_Count'Min (Byte_Count'Min (Byte_Count (Text'Length - Done), Text_Stores.Room (D.Store)),
                                       MAXIMUM_PIECE_BYTES));
            Item : Text_Pieces.Piece;
            Inserted : Boolean;
            Recorded : Boolean;
         begin
            if Take = 0 then
               Ok := False;
               D.Refused := Document_Full;
               exit;
            end if;
            Item.First := Text_Stores.Length (D.Store);
            Text_Stores.Append (D.Store, Text (Text'First + Done .. Text'First + Done + Take - 1));
            Item.Length := Piece_Length (Take);
            Item.Feeds := Text_Stores.Line_Feeds (D.Store, Item.First, Item.Length);
            Text_Pieces.Insert (D.Pieces, D.Store, Offset + Byte_Count (Done), Item, Inserted);
            if not Inserted then
               Ok := False;
               D.Refused := Document_Full;
               exit;
            end if;
            if Text_History.Group_Open (D.History) then
               Text_History.Record_Insert (D.History, Offset + Byte_Count (Done), Item, Recorded);
               if not Recorded then
                  History_Failed (D);
               end if;
            end if;
            Done := Done + Take;
         end;
      end loop;
      if Done > 0 then
         Text_Marks.Adjust_Insert (D.Marks, Offset, Byte_Count (Done));
         Invalidate_From (D, Offset);
         Changed (D);
      end if;
   end Raw_Insert;

   --  Remove Count bytes at Offset, logging the pieces in the open group.
   procedure Raw_Delete (D : in out Document; Offset : Byte_Offset; Count : Byte_Count; Ok : out Boolean) is
      Deleted : Boolean;
   begin
      Ok := True;
      if Count = 0 then
         return;
      end if;
      if Text_History.Group_Open (D.History) then
         declare
            Recorded : Boolean;
            P : Text_Pieces.Position := Text_Pieces.Locate (D.Pieces, Offset);
            Left : Byte_Count := Count;
            Found : Boolean := True;
         begin
            Text_History.Record_Remove (D.History, Offset, Count, Recorded);
            while Recorded and then Left > 0 and then Found loop
               declare
                  Item : constant Text_Pieces.Piece := Text_Pieces.Element (D.Pieces, P);
                  Take : constant Piece_Length :=
                    Piece_Length (Byte_Count'Min (Left, Byte_Count (Item.Length - P.Within)));
               begin
                  if Take > 0 then
                     Text_History.Record_Removed_Piece
                       (D.History, Text_Pieces.Part (D.Store, Item, P.Within, Take), Recorded);
                     Left := Left - Byte_Count (Take);
                  end if;
                  exit when Left = 0;
                  Text_Pieces.Next (D.Pieces, P, Found);
               end;
            end loop;
            if not Recorded then
               History_Failed (D);
            end if;
         end;
      end if;
      Text_Pieces.Delete (D.Pieces, D.Store, Offset, Count, Deleted);
      if not Deleted then
         Ok := False;
         D.Refused := Document_Full;
         return;
      end if;
      Text_Marks.Adjust_Delete (D.Marks, Offset, Count);
      Invalidate_From (D, Offset);
      Changed (D);
   end Raw_Delete;

   --  Put logged pieces back at Offset (undo of a removal, redo of an insert).
   procedure Insert_Logged (D : in out Document; Offset : Byte_Offset; First : Text_History.Entry_Index;
                            Count : Text_History.Entry_Count) is
      At_Offset : Byte_Offset := Offset;
      Ok : Boolean;
      Total : Byte_Count := 0;
   begin
      for K in First .. First + Count - 1 loop
         declare
            Item : constant Text_Pieces.Piece := Text_History.Piece_At (D.History, K);
         begin
            Text_Pieces.Insert (D.Pieces, D.Store, At_Offset, Item, Ok);
            exit when not Ok;
            At_Offset := At_Offset + Byte_Count (Item.Length);
            Total := Total + Byte_Count (Item.Length);
         end;
      end loop;
      Text_Marks.Adjust_Insert (D.Marks, Offset, Total);
      Invalidate_From (D, Offset);
      Changed (D);
   end Insert_Logged;

   procedure Delete_Unlogged (D : in out Document; Offset : Byte_Offset; Count : Byte_Count) is
      Ok : Boolean;
   begin
      Text_Pieces.Delete (D.Pieces, D.Store, Offset, Count, Ok);
      Text_Marks.Adjust_Delete (D.Marks, Offset, Count);
      Invalidate_From (D, Offset);
      Changed (D);
   end Delete_Unlogged;

   ---------------------------------------------------------------------------
   --  The editing engine: per selection, replace [First, Last) with a text.

   type Edit is record
      First, Last : Byte_Offset := 0;
      Text_First : Positive := 1;
      Text_Last : Natural := 0;
   end record;
   type Edit_Array is array (Positive range <>) of Edit;
   type Edit_Array_Access is access Edit_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Edit_Array, Edit_Array_Access);

   --  Apply Edits (sorted by First, disjoint), their texts slices of
   --  Texts. With Carets, each selection becomes a caret after its text;
   --  otherwise the selections move with the edits.
   procedure Apply
     (D : in out Document; Edits : Edit_Array; Texts : String; Kind : Text_History.Group_Kind;
      Now_Us : Unsigned_64; Boundary : Boolean; Carets : Boolean)
   is
      Ok : Boolean := True;
      Delta_Before : Long_Long_Integer := 0;
   begin
      D.Refused := None;
      if D.Is_Loading then
         D.Refused := Read_Only_Document;
         return;
      end if;
      for E of Edits loop
         if Text_Marks.Blocks_Edit (D.Marks, E.First, E.Last) then
            D.Refused := Protected_Region;
            return;
         end if;
      end loop;
      if Text_Stores.Room (D.Store) = 0
        or else Text_Stores.Maximum (D.Store) - Text_Stores.Length (D.Store)
                < Byte_Count (Texts'Length) * Byte_Count (Natural'Max (1, (if Carets then 1 else 1)))
      then
         D.Refused := Document_Full;
         return;
      end if;
      Begin_Group (D, Kind, Primary (D).Head, Now_Us, Boundary);
      for K in reverse Edits'Range loop
         declare
            E : constant Edit := Edits (K);
         begin
            if E.Last > E.First then
               Raw_Delete (D, E.First, E.Last - E.First, Ok);
               exit when not Ok;
            end if;
            if E.Text_Last >= E.Text_First then
               Raw_Insert (D, E.First, Texts (E.Text_First .. E.Text_Last), Ok);
               exit when not Ok;
            end if;
            if not Carets then
               if E.Last > E.First then
                  Text_Cursors.Adjust_Delete (D.Cursors, E.First, E.Last - E.First);
               end if;
               if E.Text_Last >= E.Text_First then
                  Text_Cursors.Adjust_Insert (D.Cursors, E.First, Byte_Count (E.Text_Last - E.Text_First + 1));
               end if;
            end if;
         end;
      end loop;
      if Carets and then Ok then
         Text_Cursors.Initialize (D.Cursors, 0);
         for K in Edits'Range loop
            declare
               E : constant Edit := Edits (K);
               Inserted : constant Long_Long_Integer := Long_Long_Integer (Natural'Max (0, E.Text_Last - E.Text_First + 1));
               Caret_At : constant Byte_Offset :=
                 Byte_Offset (Long_Long_Integer (E.First) + Delta_Before + Inserted);
               Added : Boolean;
            begin
               if K = Edits'First then
                  Text_Cursors.Replace (D.Cursors, 1, Text_Cursors.Caret (Caret_At));
               else
                  Text_Cursors.Add (D.Cursors, Text_Cursors.Caret (Caret_At), False, Added);
               end if;
               Delta_Before := Delta_Before + Inserted - Long_Long_Integer (E.Last - E.First);
            end;
         end loop;
      end if;
      Text_Cursors.Clamp (D.Cursors, Length (D));
      Text_Cursors.Normalize (D.Cursors);
      End_Group (D, Primary (D).Head);
   end Apply;

   --  One edit per selection, all with the same text.
   procedure Replace_Selections
     (D : in out Document; Text : String; Kind : Text_History.Group_Kind; Now_Us : Unsigned_64; Boundary : Boolean)
   is
      N : constant Natural := Selection_Count (D);
      Edits : Edit_Array_Access := new Edit_Array (1 .. N);
   begin
      for K in 1 .. N loop
         declare
            S : constant Text_Cursors.Selection := Selection (D, K);
         begin
            Edits (K) := (First => Text_Cursors.First_Of (S), Last => Text_Cursors.Last_Of (S),
                          Text_First => Text'First, Text_Last => Text'Last);
         end;
      end loop;
      Apply (D, Edits.all, Text, Kind, Now_Us, Boundary, Carets => True);
      Free (Edits);
   end Replace_Selections;

   procedure Insert_Text (D : in out Document; Text : String; Now_Us : Unsigned_64 := 0; Typed : Boolean := False) is
      Boundary : constant Boolean :=
        Text'Length /= 1 or else Text (Text'First) in ' ' | ASCII.HT | ASCII.LF | '.' | ',' | ';' | '(' | ')';
   begin
      Replace_Selections (D, Text, (if Typed then Text_History.Typing else Text_History.Other), Now_Us, Boundary);
   end Insert_Text;

   procedure Paste (D : in out Document; Text : String; Now_Us : Unsigned_64 := 0) is
      N : constant Natural := Selection_Count (D);
      Lines : Natural := 1;
   begin
      for C of Text loop
         if C = ASCII.LF then
            Lines := Lines + 1;
         end if;
      end loop;
      if N > 1 and then Lines = N then
         --  One line per selection.
         declare
            Edits : Edit_Array_Access := new Edit_Array (1 .. N);
            Start : Positive := Text'First;
         begin
            for K in 1 .. N loop
               declare
                  S : constant Text_Cursors.Selection := Selection (D, K);
                  Stop : Natural := Start;
               begin
                  while Stop <= Text'Last and then Text (Stop) /= ASCII.LF loop
                     Stop := Stop + 1;
                  end loop;
                  Edits (K) := (First => Text_Cursors.First_Of (S), Last => Text_Cursors.Last_Of (S),
                                Text_First => Start, Text_Last => Stop - 1);
                  Start := Stop + 1;
               end;
            end loop;
            Apply (D, Edits.all, Text, Text_History.Other, Now_Us, True, Carets => True);
            Free (Edits);
         end;
      else
         Insert_Text (D, Text, Now_Us);
      end if;
   end Paste;

   procedure New_Line (D : in out Document; Now_Us : Unsigned_64 := 0) is
      --  Auto-indent: the primary line's leading blanks.
      L : constant Line_Number := Line_Of (D, Primary (D).Head);
      Head_Text : constant String := Segment_Text (D, Segment (D, L, 0));
      Indent_Length : Natural := 0;
      Break : constant String := (if D.CRLF then ASCII.CR & ASCII.LF else [1 => ASCII.LF]);
   begin
      while Indent_Length < Head_Text'Length and then Head_Text (Indent_Length + 1) in ' ' | ASCII.HT loop
         Indent_Length := Indent_Length + 1;
      end loop;
      Indent_Length := Natural'Min (Indent_Length, Natural (Primary (D).Head - Line_Start (D, L)));
      Replace_Selections (D, Break & Head_Text (1 .. Indent_Length), Text_History.Other, Now_Us, True);
   end New_Line;

   procedure Delete_Range_Per_Selection (D : in out Document; Forward, Whole_Word : Boolean; Now_Us : Unsigned_64) is
      N : constant Natural := Selection_Count (D);
      Edits : Edit_Array_Access := new Edit_Array (1 .. N);
      Previous_Last : Byte_Offset := 0;
   begin
      for K in 1 .. N loop
         declare
            S : constant Text_Cursors.Selection := Selection (D, K);
            F, L : Byte_Offset;
         begin
            if not Text_Cursors.Is_Empty (S) then
               F := Text_Cursors.First_Of (S);
               L := Text_Cursors.Last_Of (S);
            elsif Forward then
               F := S.Head;
               L := (if Whole_Word then Word_Right (D, S.Head) else Next_Position (D, S.Head));
            else
               L := S.Head;
               F := (if Whole_Word then Word_Left (D, S.Head) else Previous_Position (D, S.Head));
            end if;
            F := Byte_Offset'Max (F, Previous_Last);
            L := Byte_Offset'Max (L, F);
            Edits (K) := (First => F, Last => L, Text_First => 1, Text_Last => 0);
            Previous_Last := L;
         end;
      end loop;
      Apply (D, Edits.all, "", (if Whole_Word then Text_History.Other else Text_History.Erasing), Now_Us,
             Boundary => Forward, Carets => True);
      Free (Edits);
   end Delete_Range_Per_Selection;

   procedure Delete_Backward (D : in out Document; Now_Us : Unsigned_64 := 0; Whole_Word : Boolean := False) is
   begin
      Delete_Range_Per_Selection (D, False, Whole_Word, Now_Us);
   end Delete_Backward;

   procedure Delete_Forward (D : in out Document; Now_Us : Unsigned_64 := 0; Whole_Word : Boolean := False) is
   begin
      Delete_Range_Per_Selection (D, True, Whole_Word, Now_Us);
   end Delete_Forward;

   --  The distinct lines the selections touch, ascending. A selection that
   --  ends at a line's start does not include that line.
   type Line_Array is array (Positive range <>) of Line_Number;
   type Line_Array_Access is access Line_Array;
   procedure Free is new Ada.Unchecked_Deallocation (Line_Array, Line_Array_Access);

   function Touched_Lines (D : Document) return Line_Array_Access is
      Total : Natural := 0;
      Last_Seen : Text_Limits.Line_Count := 0;
   begin
      for Pass in 1 .. 2 loop
         declare
            Result : Line_Array_Access := (if Pass = 2 then new Line_Array (1 .. Total) else null);
            Count : Natural := 0;
         begin
            Last_Seen := 0;
            for K in 1 .. Selection_Count (D) loop
               declare
                  S : constant Text_Cursors.Selection := Selection (D, K);
                  Low : constant Line_Number := Line_Of (D, Text_Cursors.First_Of (S));
                  High : Line_Number := Line_Of (D, Text_Cursors.Last_Of (S));
               begin
                  if High > Low and then Text_Cursors.Last_Of (S) = Line_Start (D, High) then
                     High := High - 1;
                  end if;
                  for L in Line_Number'Max (Low, Last_Seen + 1) .. High loop
                     Count := Count + 1;
                     if Pass = 2 then
                        Result (Count) := L;
                     end if;
                     Last_Seen := L;
                  end loop;
               end;
            end loop;
            if Pass = 2 then
               return Result;
            end if;
            Total := Count;
            Free (Result);
         end;
      end loop;
      return null;
   end Touched_Lines;

   procedure Indent (D : in out Document; Settings : Layout_Settings) is
      Lines : Line_Array_Access := Touched_Lines (D);
      Unit : constant String (1 .. Settings.Tab) := [others => ' '];
      Edits : Edit_Array_Access := new Edit_Array (1 .. Lines'Length);
   begin
      for K in Lines'Range loop
         declare
            Start : constant Byte_Offset := Line_Start (D, Lines (K));
         begin
            Edits (K) := (First => Start, Last => Start, Text_First => 1, Text_Last => Unit'Length);
         end;
      end loop;
      --  Carets at a line's start move after the indent.
      for K in 1 .. Selection_Count (D) loop
         null;
      end loop;
      Apply (D, Edits.all, Unit, Text_History.Other, 0, True, Carets => False);
      Free (Edits);
      Free (Lines);
   end Indent;

   procedure Outdent (D : in out Document; Settings : Layout_Settings) is
      Lines : Line_Array_Access := Touched_Lines (D);
      Edits : Edit_Array_Access := new Edit_Array (1 .. Lines'Length);
      Used : Natural := 0;
   begin
      for K in Lines'Range loop
         declare
            Start : constant Byte_Offset := Line_Start (D, Lines (K));
            Text : constant String := Segment_Text (D, Segment (D, Lines (K), 0));
            Remove : Natural := 0;
         begin
            if Text'Length > 0 and then Text (1) = ASCII.HT then
               Remove := 1;
            else
               while Remove < Settings.Tab and then Remove < Text'Length and then Text (Remove + 1) = ' ' loop
                  Remove := Remove + 1;
               end loop;
            end if;
            if Remove > 0 then
               Used := Used + 1;
               Edits (Used) := (First => Start, Last => Start + Byte_Count (Remove), Text_First => 1, Text_Last => 0);
            end if;
         end;
      end loop;
      if Used > 0 then
         Apply (D, Edits (1 .. Used), "", Text_History.Other, 0, True, Carets => False);
      end if;
      Free (Edits);
      Free (Lines);
   end Outdent;

   procedure Toggle_Comment (D : in out Document) is
      Prefix : constant String := Text_Syntax.Comment_Prefix (D.Kind);
      Lines : Line_Array_Access;
      Edits : Edit_Array_Access;
      All_Commented : Boolean := True;
      Least_Indent : Natural := Natural'Last;
      Used : Natural := 0;
      With_Space : constant String := Prefix & " ";

      function Indent_Of (Text : String) return Natural is
         I : Natural := 0;
      begin
         while I < Text'Length and then Text (I + 1) in ' ' | ASCII.HT loop
            I := I + 1;
         end loop;
         return I;
      end Indent_Of;
   begin
      if Prefix'Length = 0 then
         return;
      end if;
      Lines := Touched_Lines (D);
      Edits := new Edit_Array (1 .. Lines'Length);
      for L of Lines.all loop
         declare
            Text : constant String := Segment_Text (D, Segment (D, L, 0));
            I : constant Natural := Indent_Of (Text);
         begin
            if I < Text'Length then
               Least_Indent := Natural'Min (Least_Indent, I);
               if Text'Length - I < Prefix'Length or else Text (I + 1 .. I + Prefix'Length) /= Prefix then
                  All_Commented := False;
               end if;
            end if;
         end;
      end loop;
      if Least_Indent = Natural'Last then
         Free (Edits);
         Free (Lines);
         return;
      end if;
      for L of Lines.all loop
         declare
            Start : constant Byte_Offset := Line_Start (D, L);
            Text : constant String := Segment_Text (D, Segment (D, L, 0));
            I : constant Natural := Indent_Of (Text);
         begin
            if I < Text'Length then
               Used := Used + 1;
               if All_Commented then
                  declare
                     Gone : Natural := Prefix'Length;
                  begin
                     if I + Gone < Text'Length and then Text (I + Gone + 1) = ' ' then
                        Gone := Gone + 1;
                     end if;
                     Edits (Used) := (First => Start + Byte_Count (I), Last => Start + Byte_Count (I + Gone),
                                      Text_First => 1, Text_Last => 0);
                  end;
               else
                  Edits (Used) := (First => Start + Byte_Count (Least_Indent), Last => Start + Byte_Count (Least_Indent),
                                   Text_First => 1, Text_Last => With_Space'Length);
               end if;
            end if;
         end;
      end loop;
      Apply (D, Edits (1 .. Used), With_Space, Text_History.Other, 0, True, Carets => False);
      Free (Edits);
      Free (Lines);
   end Toggle_Comment;

   procedure Delete_Lines (D : in out Document) is
      Lines : Line_Array_Access := Touched_Lines (D);
      Edits : Edit_Array_Access := new Edit_Array (1 .. Lines'Length);
      Used : Natural := 0;
   begin
      --  Runs of consecutive lines become one removal.
      for K in Lines'Range loop
         declare
            Start : constant Byte_Offset := Line_Start (D, Lines (K));
            Stop : Byte_Offset := Line_Start (D, Lines (K) + 1);
         begin
            if Lines (K) = Line_Total (D) and then Lines (K) > 1 then
               --  The last line takes the line break before it.
               Stop := Length (D);
               if Used > 0 and then Edits (Used).Last >= Line_End (D, Lines (K) - 1) then
                  Edits (Used).Last := Stop;
               else
                  Used := Used + 1;
                  Edits (Used) := (First => Line_End (D, Lines (K) - 1), Last => Stop, Text_First => 1, Text_Last => 0);
               end if;
            elsif Used > 0 and then Edits (Used).Last = Start then
               Edits (Used).Last := Stop;
            else
               Used := Used + 1;
               Edits (Used) := (First => Start, Last => Stop, Text_First => 1, Text_Last => 0);
            end if;
         end;
      end loop;
      Apply (D, Edits (1 .. Used), "", Text_History.Other, 0, True, Carets => True);
      Free (Edits);
      Free (Lines);
   end Delete_Lines;

   procedure Duplicate_Lines (D : in out Document) is
      Lines : Line_Array_Access := Touched_Lines (D);
   begin
      if Lines'Length = 0 then
         Free (Lines);
         return;
      end if;
      declare
         First : constant Byte_Offset := Line_Start (D, Lines (Lines'First));
         Stop : constant Byte_Offset := Line_End (D, Lines (Lines'Last));
         Copy : constant String := Slice (D, First, Stop);
         Break : constant String := (if D.CRLF then ASCII.CR & ASCII.LF else [1 => ASCII.LF]);
         Texts : constant String := Break & Copy;
         Edits : constant Edit_Array := [1 => (First => Stop, Last => Stop, Text_First => 1, Text_Last => Texts'Length)];
      begin
         Apply (D, Edits, Texts, Text_History.Other, 0, True, Carets => False);
      end;
      Free (Lines);
   end Duplicate_Lines;

   procedure Seal_Typing (D : in out Document) is
   begin
      Text_History.Seal (D.History);
   end Seal_Typing;

   ---------------------------------------------------------------------------
   --  Undo and redo.

   function Can_Undo (D : Document) return Boolean is (Text_History.Can_Undo (D.History));
   function Can_Redo (D : Document) return Boolean is (Text_History.Can_Redo (D.History));

   procedure Restore_Selections (D : in out Document; Group : Text_History.Entry_Index; After : Boolean) is
      First : Text_History.Entry_Index;
      Count : Text_History.Entry_Count;
      Added : Boolean;
   begin
      Text_History.Group_Selections (D.History, Group, After, First, Count);
      if Count = 0 then
         return;
      end if;
      Text_Cursors.Initialize (D.Cursors, 0);
      for K in First .. First + Count - 1 loop
         if K = First then
            Text_Cursors.Replace (D.Cursors, 1, Text_History.Selection_At (D.History, K));
         else
            Text_Cursors.Add (D.Cursors, Text_History.Selection_At (D.History, K), False, Added);
         end if;
      end loop;
      Text_Cursors.Clamp (D.Cursors, Length (D));
      Text_Cursors.Normalize (D.Cursors);
   end Restore_Selections;

   procedure Undo (D : in out Document) is
      Group : Text_History.Entry_Index;
      First : Text_History.Entry_Index;
      Count : Text_History.Entry_Count;
   begin
      Text_History.Seal (D.History);
      if not Text_History.Can_Undo (D.History) then
         return;
      end if;
      Group := Text_History.Undo_Group (D.History);
      Text_History.Group_Records (D.History, Group, First, Count);
      for K in reverse First .. First + Count - 1 loop
         declare
            R : constant Text_History.Edit_Record := Text_History.Record_At (D.History, K);
         begin
            case R.Kind is
               when Text_History.Inserted => Delete_Unlogged (D, R.Offset, R.Length);
               when Text_History.Removed => Insert_Logged (D, R.Offset, R.Pieces_First, R.Pieces_Count);
            end case;
         end;
      end loop;
      Restore_Selections (D, Group, After => False);
      Text_History.Mark_Undone (D.History);
      Changed (D);
   end Undo;

   procedure Redo (D : in out Document) is
      Group : Text_History.Entry_Index;
      First : Text_History.Entry_Index;
      Count : Text_History.Entry_Count;
   begin
      if not Text_History.Can_Redo (D.History) then
         return;
      end if;
      Group := Text_History.Redo_Group (D.History);
      Text_History.Group_Records (D.History, Group, First, Count);
      for K in First .. First + Count - 1 loop
         declare
            R : constant Text_History.Edit_Record := Text_History.Record_At (D.History, K);
         begin
            case R.Kind is
               when Text_History.Inserted => Insert_Logged (D, R.Offset, R.Pieces_First, R.Pieces_Count);
               when Text_History.Removed => Delete_Unlogged (D, R.Offset, R.Length);
            end case;
         end;
      end loop;
      Restore_Selections (D, Group, After => True);
      Text_History.Mark_Redone (D.History);
      Changed (D);
   end Redo;

   ---------------------------------------------------------------------------
   --  Loading.

   procedure Begin_Load (D : in out Document; Expected : Byte_Count) is
   begin
      D.Is_Loading := True;
      D.Expected := Expected;
   end Begin_Load;

   procedure Load_Chunk (D : in out Document; Bytes : String; Ok : out Boolean) is
      Done : Natural := 0;
   begin
      Ok := True;
      while Done < Bytes'Length loop
         declare
            Take : constant Natural :=
              Natural (Byte_Count'Min (Byte_Count'Min (Byte_Count (Bytes'Length - Done), Text_Stores.Room (D.Store)),
                                       MAXIMUM_PIECE_BYTES));
            Item : Text_Pieces.Piece;
            Inserted : Boolean;
         begin
            if Take = 0 then
               Ok := False;
               return;
            end if;
            Item.First := Text_Stores.Length (D.Store);
            Text_Stores.Append (D.Store, Bytes (Bytes'First + Done .. Bytes'First + Done + Take - 1));
            Item.Length := Piece_Length (Take);
            Item.Feeds := Text_Stores.Line_Feeds (D.Store, Item.First, Item.Length);
            Text_Pieces.Insert (D.Pieces, D.Store, Length (D), Item, Inserted);
            if not Inserted then
               Ok := False;
               return;
            end if;
            Done := Done + Take;
         end;
      end loop;
      Changed (D);
   end Load_Chunk;

   procedure End_Load (D : in out Document) is
   begin
      D.Is_Loading := False;
      if Line_Total (D) > 1 then
         declare
            Feed : constant Byte_Offset := Line_Start (D, 2) - 1;
         begin
            D.CRLF := Feed > 0 and then Byte_At (D, Feed - 1) = ASCII.CR;
         end;
      end if;
      Text_History.Initialize (D.History);
      Text_Cursors.Initialize (D.Cursors, 0);
      D.Valid_Checkpoints := 1;
      D.Reusable_Checkpoints := 0;
      D.Known_Lines := Line_Total (D);
      Changed (D);
   end End_Load;

   ---------------------------------------------------------------------------
   --  Marks.

   procedure Add_Mark (D : in out Document; Item : Text_Marks.Mark; Added : out Boolean) is
   begin
      Text_Marks.Add (D.Marks, Item, Added);
      Changed (D);
   end Add_Mark;

   procedure Remove_Marks (D : in out Document; Id : Text_Marks.Mark_Id) is
   begin
      Text_Marks.Remove (D.Marks, Id);
      Changed (D);
   end Remove_Marks;

   function Mark_Count (D : Document) return Natural is (Text_Marks.Count (D.Marks));
   function Mark (D : Document; Index : Positive) return Text_Marks.Mark is (Text_Marks.Element (D.Marks, Index));

   procedure Replace_Region (D : in out Document; Id : Text_Marks.Mark_Id; Text : String; Done : out Boolean) is
      Index : constant Natural := Text_Marks.Find (D.Marks, Id);
      Ok : Boolean;
   begin
      Done := False;
      if Index = 0 or else D.Is_Loading then
         return;
      end if;
      declare
         M : constant Text_Marks.Mark := Text_Marks.Element (D.Marks, Index);
      begin
         Begin_Group (D, Text_History.Host_Edit, M.First, 0, True);
         Raw_Delete (D, M.First, M.Last - M.First, Ok);
         Text_Cursors.Adjust_Delete (D.Cursors, M.First, M.Last - M.First);
         if Ok then
            Raw_Insert (D, M.First, Text, Ok);
            Text_Cursors.Adjust_Insert (D.Cursors, M.First, Text'Length);
         end if;
         --  The region covers its new text.
         declare
            Again : constant Natural := Text_Marks.Find (D.Marks, Id);
         begin
            if Again > 0 then
               Text_Marks.Set_Extent (D.Marks, Again, M.First, M.First + Text'Length);
            end if;
         end;
         Text_Cursors.Normalize (D.Cursors);
         End_Group (D, Primary (D).Head);
         Done := Ok;
      end;
   end Replace_Region;

   ---------------------------------------------------------------------------
   --  Search.

   procedure Set_Search (D : in out Document; Pattern : String; Settings : Text_Search.Options;
                         Status : out Search_Status) is
      Result : Text_Search.Compile_Result;
      use type Text_Search.Compile_Result;
   begin
      D.Finding.Active := False;
      D.Counting := (others => <>);
      D.Pattern_Length := Natural'Min (Pattern'Length, Text_Search.MAXIMUM_PATTERN);
      D.Pattern (1 .. D.Pattern_Length) := Pattern (Pattern'First .. Pattern'First + D.Pattern_Length - 1);
      D.Search_Options := Settings;
      D.Program_Ready := False;
      if D.Pattern_Length = 0 then
         Status := No_Pattern;
         return;
      end if;
      if Settings.Regular then
         Text_Search.Compile (D.Pattern (1 .. D.Pattern_Length), Settings.Case_Sensitive, D.Program, Result);
         if Result /= Text_Search.Compiled then
            Status := Bad_Pattern;
            D.Pattern_Length := 0;
            return;
         end if;
         D.Program_Ready := True;
         --  The leading literal, unless its last character is repeated or
         --  optional (then it is not required).
         D.Prefix_Length := 0;
         while D.Prefix_Length < D.Pattern_Length
           and then D.Pattern (D.Prefix_Length + 1) in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | ' '
         loop
            D.Prefix_Length := D.Prefix_Length + 1;
         end loop;
         if D.Prefix_Length < D.Pattern_Length and then D.Prefix_Length > 0
           and then D.Pattern (D.Prefix_Length + 1) in '*' | '?' | '{'
         then
            D.Prefix_Length := D.Prefix_Length - 1;
         end if;
      end if;
      Status := Ready;
   end Set_Search;

   function Search_Pattern (D : Document) return String is (D.Pattern (1 .. D.Pattern_Length));
   function Search_Settings (D : Document) return Text_Search.Options is (D.Search_Options);

   --  The first match at or after From within one window (literal) or one
   --  segment (regex): its offsets, or Found False and the offset the next
   --  window starts at.
   procedure Next_Match
     (D : Document; From : Byte_Offset; Found : out Boolean; First, Last : out Byte_Offset;
      Resume : out Byte_Offset)
   is
      Needle : constant String := D.Pattern (1 .. D.Pattern_Length);
   begin
      Found := False;
      First := From;
      Last := From;
      if From >= Length (D) and then not D.Search_Options.Regular then
         Resume := Length (D);
         return;
      end if;
      if D.Search_Options.Regular and then D.Prefix_Length > 0 and then From < Length (D) then
         --  Skip to the next occurrence of the literal prefix: only its
         --  segment can hold a match.
         declare
            Count : constant Byte_Count := Byte_Count'Min (SEARCH_WINDOW, Length (D) - From);
            Window : String (1 .. Natural (Count));
            Found_Prefix : Boolean;
            At_Index : Positive;
            Literal : constant Text_Search.Options :=
              (Case_Sensitive => D.Search_Options.Case_Sensitive, Whole_Word => False, Regular => False);
         begin
            Read (D, From, Window);
            Text_Search.Find_Literal (Window, D.Pattern (1 .. D.Prefix_Length), 1, Literal, SPACE, SPACE,
                                      Found_Prefix, At_Index);
            if not Found_Prefix then
               Resume := (if From + Count >= Length (D) then Length (D)
                          else From + Count - Byte_Count'Min (Count - 1, Byte_Count (D.Prefix_Length - 1)));
               return;
            end if;
            declare
               Candidate : constant Byte_Offset := From + Byte_Count (At_Index - 1);
               Place : constant Segment_Place := Segment_Of (D, Candidate);
               Text : constant String := Segment_Text (D, Place);
               Match_First : Positive;
               Match_Length : Natural;
            begin
               Text_Search.Match (D.Program, Text, Natural (From - Byte_Offset'Min (From, Place.First)) + 1, Found,
                                  Match_First, Match_Length);
               if Found then
                  First := Place.First + Byte_Count (Match_First - 1);
                  Last := First + Byte_Count (Match_Length);
               end if;
               Resume := (if Place.Index + 1 < Segment_Count (D, Place.Line) then Segment (D, Place.Line, Place.Index + 1).First
                          elsif Place.Line < Line_Total (D) then Line_Start (D, Place.Line + 1)
                          else Length (D));
               return;
            end;
         end;
      elsif D.Search_Options.Regular then
         declare
            Place : constant Segment_Place := Segment_Of (D, From);
            Text : constant String := Segment_Text (D, Place);
            Match_First : Positive;
            Match_Length : Natural;
         begin
            Text_Search.Match (D.Program, Text, Natural (From - Place.First) + 1, Found, Match_First, Match_Length);
            if Found then
               First := Place.First + Byte_Count (Match_First - 1);
               Last := First + Byte_Count (Match_Length);
            end if;
            --  The next segment, or the next line.
            if Place.Index + 1 < Segment_Count (D, Place.Line) then
               Resume := Segment (D, Place.Line, Place.Index + 1).First;
            elsif Place.Line < Line_Total (D) then
               Resume := Line_Start (D, Place.Line + 1);
            else
               Resume := Length (D);
            end if;
         end;
      else
         declare
            Count : constant Byte_Count := Byte_Count'Min (SEARCH_WINDOW, Length (D) - From);
            Window : String (1 .. Natural (Count));
            At_Index : Positive;
         begin
            Read (D, From, Window);
            Text_Search.Find_Literal
              (Window, Needle, 1, D.Search_Options, (if From > 0 then Byte_At (D, From - 1) else SPACE),
               (if From + Count < Length (D) then Byte_At (D, From + Count) else SPACE), Found, At_Index);
            if Found then
               First := From + Byte_Count (At_Index - 1);
               Last := First + Needle'Length;
            end if;
            Resume := (if From + Count >= Length (D) then Length (D)
                       else From + Count - Byte_Count'Min (Count - 1, Byte_Count (Needle'Length - 1)));
         end;
      end if;
   end Next_Match;

   --  The last match that starts before Before, in the window or segment
   --  ending there; Resume is where the previous window ends.
   procedure Previous_Match
     (D : Document; Before : Byte_Offset; Found : out Boolean; First, Last : out Byte_Offset;
      Resume : out Byte_Offset)
   is
      Needle : constant String := D.Pattern (1 .. D.Pattern_Length);
   begin
      Found := False;
      First := 0;
      Last := 0;
      Resume := 0;
      if Before = 0 then
         return;
      end if;
      if D.Search_Options.Regular then
         declare
            Place : constant Segment_Place := Segment_Of (D, Before - 1);
            Text : constant String := Segment_Text (D, Place);
            From : Positive := 1;
            Ok : Boolean;
            Match_First : Positive;
            Match_Length : Natural;
         begin
            loop
               exit when From > Text'Length + 1;
               Text_Search.Match (D.Program, Text, From, Ok, Match_First, Match_Length);
               exit when not Ok or else Place.First + Byte_Count (Match_First - 1) >= Before;
               Found := True;
               First := Place.First + Byte_Count (Match_First - 1);
               Last := First + Byte_Count (Match_Length);
               From := Match_First + Natural'Max (Match_Length, 1);
            end loop;
            Resume := (if Place.First > 0 then Place.First - (if Place.Index = 0 then 1 else 0) else 0);
            if Place.Index = 0 and then Place.First > 0 then
               --  Before the line break: the previous line's end.
               Resume := Line_End (D, Place.Line - 1) + 1;
            end if;
         end;
      else
         declare
            Start : constant Byte_Offset := (if Before > SEARCH_WINDOW then Before - SEARCH_WINDOW else 0);
            Stop : constant Byte_Offset := Byte_Offset'Min (Length (D), Before + Byte_Count (Needle'Length - 1));
            Window : String (1 .. Natural (Stop - Start));
            At_Index : Positive;
         begin
            Read (D, Start, Window);
            --  A match must start before Before.
            Text_Search.Find_Literal_Backward
              (Window, Needle, Natural (Before - Start) + Needle'Length - 1, D.Search_Options,
               (if Start > 0 then Byte_At (D, Start - 1) else SPACE),
               (if Stop < Length (D) then Byte_At (D, Stop) else SPACE), Found, At_Index);
            if Found then
               First := Start + Byte_Count (At_Index - 1);
               Last := First + Needle'Length;
            end if;
            Resume := (if Start = 0 then 0 else Start + Byte_Count'Min (Byte_Count (Needle'Length - 1), Before - Start));
            if Resume >= Before then
               Resume := Start;
            end if;
         end;
      end if;
   end Previous_Match;

   procedure Find
     (D : in out Document; Forward : Boolean; Budget : Work_Budget; Status : out Search_Status)
   is
      Spent : Natural := 0;
      Hit : Boolean;
      First, Last, Resume : Byte_Offset;
      P : constant Text_Cursors.Selection := Primary (D);
   begin
      if D.Pattern_Length = 0 then
         Status := No_Pattern;
         return;
      end if;
      if not D.Finding.Active or else D.Finding.Forward /= Forward then
         D.Finding := (Active => True, Forward => Forward,
                       Position => (if Forward then Text_Cursors.Last_Of (P) else Text_Cursors.First_Of (P)),
                       Origin => (if Forward then Text_Cursors.Last_Of (P) else Text_Cursors.First_Of (P)),
                       Wrapped => False);
         --  An empty match at the caret itself does not count: start one on.
      end if;
      loop
         declare
            J : Find_Job renames D.Finding;
         begin
            if Forward then
               Next_Match (D, J.Position, Hit, First, Last, Resume);
               if Hit and then Last = First and then First = Text_Cursors.Last_Of (P)
                 and then Text_Cursors.Is_Empty (P) and then not J.Wrapped
               then
                  --  An empty match where we stand: look one further.
                  Hit := False;
                  Resume := Next_Position (D, First);
                  if Resume = First then
                     Resume := Length (D);
                  end if;
               end if;
               if Hit and then J.Wrapped and then First >= J.Origin then
                  Hit := False;
                  Resume := Length (D);
                  J.Active := False;
                  Status := Not_Found;
                  return;
               end if;
            else
               Previous_Match (D, J.Position, Hit, First, Last, Resume);
               if Hit and then J.Wrapped and then First < J.Origin then
                  J.Active := False;
                  Status := Not_Found;
                  return;
               end if;
            end if;
            if Hit then
               J.Active := False;
               Set_Selection (D, First, Last);
               Status := Found;
               return;
            end if;
            Spent := Spent + Natural (Byte_Count'Min ((if Resume > J.Position then Resume - J.Position
                                                       else J.Position - Resume) + 1, 2 ** 20));
            if Forward then
               if Resume >= Length (D) or else Resume <= J.Position then
                  if J.Wrapped then
                     J.Active := False;
                     Status := Not_Found;
                     return;
                  end if;
                  J.Wrapped := True;
                  J.Position := 0;
               else
                  J.Position := Resume;
               end if;
               if J.Wrapped and then J.Position > J.Origin then
                  J.Active := False;
                  Status := Not_Found;
                  return;
               end if;
            else
               if J.Position = 0 or else Resume >= J.Position then
                  if J.Wrapped then
                     J.Active := False;
                     Status := Not_Found;
                     return;
                  end if;
                  J.Wrapped := True;
                  J.Position := Length (D);
               else
                  J.Position := Resume;
               end if;
               if J.Wrapped and then J.Position < J.Origin then
                  J.Active := False;
                  Status := Not_Found;
                  return;
               end if;
            end if;
            if Spent >= Budget then
               Status := Searching;
               return;
            end if;
         end;
      end loop;
   end Find;

   procedure Select_All_Matches (D : in out Document; Count : out Natural) is
      Position : Byte_Offset := 0;
      Found : Boolean;
      First, Last, Resume : Byte_Offset;
      Added : Boolean := True;
   begin
      Count := 0;
      if D.Pattern_Length = 0 then
         return;
      end if;
      loop
         Next_Match (D, Position, Found, First, Last, Resume);
         if Found then
            if Count = 0 then
               Text_Cursors.Initialize (D.Cursors, 0);
               Text_Cursors.Replace (D.Cursors, 1, (Anchor => First, Head => Last, others => <>));
            else
               Text_Cursors.Add (D.Cursors, (Anchor => First, Head => Last, others => <>), False, Added);
               exit when not Added;
            end if;
            Count := Count + 1;
            Position := (if Last > First then Last else Next_Position (D, Last));
            exit when Position >= Length (D) and then (Last >= Length (D) or else not D.Search_Options.Regular);
            exit when Last = First and then Position = First;
         else
            exit when Resume <= Position or else Resume >= Length (D);
            Position := Resume;
         end if;
      end loop;
      Text_Cursors.Normalize (D.Cursors);
      Text_History.Seal (D.History);
   end Select_All_Matches;

   procedure Replace_One (D : in out Document; Replacement : String; Status : out Search_Status) is
      P : constant Text_Cursors.Selection := Primary (D);
      Found : Boolean;
      First, Last, Resume : Byte_Offset;
   begin
      if D.Pattern_Length = 0 then
         Status := No_Pattern;
         return;
      end if;
      if not Text_Cursors.Is_Empty (P) then
         Next_Match (D, Text_Cursors.First_Of (P), Found, First, Last, Resume);
         if Found and then First = Text_Cursors.First_Of (P) and then Last = Text_Cursors.Last_Of (P) then
            Keep_Primary (D);
            Insert_Text (D, Replacement);
         end if;
      end if;
      Find (D, True, Work_Budget'Last, Status);
   end Replace_One;

   procedure Replace_All (D : in out Document; Replacement : String; Count : out Natural) is
   begin
      Select_All_Matches (D, Count);
      if Count > 0 then
         Insert_Text (D, Replacement);
      end if;
   end Replace_All;

   procedure Matches_In (D : Document; Text : String) is
      Needle : constant String := D.Pattern (1 .. D.Pattern_Length);
      From : Positive := Text'First;
      Found : Boolean;
      At_Index : Positive;
      Length : Natural;
   begin
      if D.Pattern_Length = 0 or else Text'Length = 0 then
         return;
      end if;
      loop
         exit when From > Text'Last;
         if D.Search_Options.Regular then
            Text_Search.Match (D.Program, Text, From, Found, At_Index, Length);
         else
            Text_Search.Find_Literal (Text, Needle, From, D.Search_Options, SPACE, SPACE, Found, At_Index);
            Length := Needle'Length;
         end if;
         exit when not Found;
         if Length > 0 then
            Visit (At_Index, Length);
         end if;
         From := At_Index + Natural'Max (Length, 1);
      end loop;
   end Matches_In;

   procedure Count_Matches (D : in out Document; Budget : Work_Budget; Total : out Natural; Done : out Boolean) is
      C : Count_Job renames D.Counting;
      Spent : Natural := 0;
      Found : Boolean;
      First, Last, Resume : Byte_Offset;
   begin
      if D.Pattern_Length = 0 then
         Total := 0;
         Done := True;
         return;
      end if;
      if not C.Active or else C.Revision /= D.Revision_Count then
         C := (Active => True, Position => 0, Total => 0, Revision => D.Revision_Count, Complete => False);
      end if;
      while not C.Complete and then Spent < Budget loop
         Next_Match (D, C.Position, Found, First, Last, Resume);
         if Found then
            C.Total := C.Total + 1;
            Spent := Spent + Natural (Byte_Count'Min (Last - C.Position + 1, 2 ** 20));
            C.Position := (if Last > First then Last else Next_Position (D, Last));
            if C.Position >= Length (D) and then Last = First then
               C.Complete := True;
            end if;
         else
            Spent := Spent + Natural (Byte_Count'Min (Resume - Byte_Offset'Min (Resume, C.Position) + 1, 2 ** 20));
            if Resume <= C.Position or else Resume >= Length (D) then
               C.Complete := True;
            else
               C.Position := Resume;
            end if;
         end if;
         if C.Position >= Length (D) and then not D.Search_Options.Regular then
            C.Complete := True;
         end if;
      end loop;
      Total := C.Total;
      Done := C.Complete;
   end Count_Matches;

   ---------------------------------------------------------------------------
   --  Highlighting.

   function Highlight_Watermark (D : Document) return Line_Number is
     (Text_Limits.Line_Count (D.Valid_Checkpoints - 1) * CHECKPOINT_LINES + 1);

   procedure Ensure_Checkpoints (D : in out Document; Needed : Natural) is
   begin
      if Needed > D.Checkpoints'Length then
         declare
            Bigger : constant Checkpoint_Access :=
              new Checkpoint_Array'(0 .. Natural'Max (Needed, 2 * D.Checkpoints'Length) - 1 => Text_Syntax.INITIAL_STATE);
         begin
            Bigger (0 .. D.Checkpoints'Length - 1) := D.Checkpoints.all;
            Free (D.Checkpoints);
            D.Checkpoints := Bigger;
         end;
      end if;
   end Ensure_Checkpoints;

   function First_Segment_Text (D : Document; L : Line_Number) return String is
     (Segment_Text (D, Segment (D, L, 0)));

   procedure Line_Spans
     (D : in out Document; L : Line_Number; Text : String; Spans : out Text_Syntax.Span_Array;
      Count : out Text_Syntax.Span_Count; Exact : out Boolean)
   is
      State : Text_Syntax.Line_State;
      Finish : Text_Syntax.Line_State;
      K : constant Natural := Natural ((L - 1) / CHECKPOINT_LINES);
   begin
      if D.Kind = Text_Syntax.Plain_Text then
         Count := 0;
         Exact := True;
         Spans := [others => (others => <>)];
         return;
      end if;
      if D.Cache_Revision = D.Revision_Count and then D.Cache_Line = L - 1 and then L > 1 then
         State := D.Cache_State;
         Exact := D.Cache_Exact;
      else
         --  From the nearest checkpoint above.
         Exact := K < D.Valid_Checkpoints;
         State := (if K < D.Checkpoints'Length then D.Checkpoints (K) else Text_Syntax.INITIAL_STATE);
         for Before in Text_Limits.Line_Count (K) * CHECKPOINT_LINES + 1 .. L - 1 loop
            declare
               Line_Text : constant String := First_Segment_Text (D, Before);
               Ignored : Text_Syntax.Span_Array;
               Ignored_Count : Text_Syntax.Span_Count;
               Normal : constant String (1 .. Line_Text'Length) := Line_Text;
               After : Text_Syntax.Line_State;
            begin
               Text_Syntax.Lex_Line (D.Kind, Normal, State, Ignored, Ignored_Count, After);
               State := After;
            end;
         end loop;
      end if;
      declare
         Normal : constant String (1 .. Text'Length) := Text;
      begin
         Text_Syntax.Lex_Line (D.Kind, Normal, State, Spans, Count, Finish);
      end;
      D.Demand := Line_Count'Max (D.Demand, L);
      D.Cache_Line := L;
      D.Cache_State := Finish;
      D.Cache_Exact := Exact;
      D.Cache_Revision := D.Revision_Count;
   end Line_Spans;

   procedure Highlight_Pump (D : in out Document; Budget : Work_Budget; Busy, Changed : out Boolean) is
      Last_Checkpoint : constant Natural := Natural ((Line_Total (D) - 1) / CHECKPOINT_LINES);
      Lines_Done : Natural := 0;
   begin
      Changed := False;
      Busy := False;
      if D.Kind = Text_Syntax.Plain_Text or else D.Is_Loading then
         return;
      end if;
      Ensure_Checkpoints (D, Last_Checkpoint + 1);
      --  Only as far as a view has asked (one interval beyond).
      while D.Valid_Checkpoints <= Natural'Min (Last_Checkpoint, Natural (D.Demand / CHECKPOINT_LINES) + 1) loop
         if Lines_Done >= Budget then
            Busy := True;
            return;
         end if;
         --  Lex the CHECKPOINT_LINES lines from the last valid checkpoint.
         declare
            K : constant Natural := D.Valid_Checkpoints - 1;
            State : Text_Syntax.Line_State := D.Checkpoints (K);
            First : constant Line_Number := Text_Limits.Line_Count (K) * CHECKPOINT_LINES + 1;
            Old : constant Text_Syntax.Line_State := D.Checkpoints (K + 1);
            Position : Byte_Offset := Line_Start (D, First);
         begin
            for L in First .. Line_Number'Min (First + CHECKPOINT_LINES - 1, Line_Total (D)) loop
               declare
                  --  Read on from the line's start to its feed (one segment at most).
                  Count : constant Byte_Count :=
                    Byte_Count'Min (SEGMENT_BYTES, Length (D) - Byte_Offset'Min (Position, Length (D)));
                  Window : String (1 .. Natural (Count));
                  Stop : Natural := 0;
                  Ignored : Text_Syntax.Span_Array;
                  Ignored_Count : Text_Syntax.Span_Count;
               begin
                  Read (D, Position, Window);
                  while Stop < Window'Length and then Window (Stop + 1) /= ASCII.LF loop
                     Stop := Stop + 1;
                  end loop;
                  declare
                     Line_Stop : constant Natural :=
                       (if Stop > 0 and then Window (Stop) = ASCII.CR then Stop - 1 else Stop);
                     After : Text_Syntax.Line_State;
                  begin
                     Text_Syntax.Lex_Line (D.Kind, Window (1 .. Line_Stop), State, Ignored, Ignored_Count, After);
                     State := After;
                  end;
                  if Stop < Window'Length then
                     Position := Position + Byte_Count (Stop) + 1;
                  else
                     --  A line longer than a segment: its next line by the index.
                     Position := Line_Start (D, L + 1);
                  end if;
               end;
            end loop;
            D.Checkpoints (K + 1) := State;
            D.Valid_Checkpoints := D.Valid_Checkpoints + 1;
            Lines_Done := Lines_Done + CHECKPOINT_LINES;
            if State /= Old then
               Changed := True;
               D.Reusable_Checkpoints := 0;
            elsif D.Reusable_Checkpoints > D.Valid_Checkpoints and then Line_Total (D) = D.Known_Lines then
               --  Converged: the checkpoints after this one are still right.
               D.Valid_Checkpoints := D.Reusable_Checkpoints;
               D.Reusable_Checkpoints := 0;
            end if;
         end;
      end loop;
      if Changed then
         D.Cache_Revision := 0;
      end if;
   end Highlight_Pump;
end Text_Documents;
