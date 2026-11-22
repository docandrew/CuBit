with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Text_Commands;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with Text_Limits; use Text_Limits;
with Text_Stores;
with Text_Pieces;
with Text_Cursors;
with Text_History;
with Text_Marks;
with Text_UTF8;
with Text_Layout;
with Text_Search;
with Text_Syntax;
with Text_Documents; use Text_Documents;
with Text_Test_Support; use Text_Test_Support;

--  The editor core against reference models (docs/text-editor.md,
--  tests/text-editor/README.md): randomized differential tests and
--  directed cases. Run with a seed to reproduce: text_core_tests [seed].
procedure Text_Core_Tests is
   use type Text_Cursors.Selection;
   use type Text_History.Record_Kind;
   use type Text_Syntax.Token_Kind;
   use type Text_Syntax.Lexer_Mode;

   Write_Interface : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 1 and then Ada.Command_Line.Argument (1) = "--write-interface";
   Seed : constant Unsigned_64 :=
     (if Ada.Command_Line.Argument_Count >= 1 and then not Write_Interface
      then Unsigned_64'Value (Ada.Command_Line.Argument (1)) else 20261014);
   INTERFACE_PATH : constant String := "../../userspace/apps/text/text.ccl-interface";

   ---------------------------------------------------------------------------
   --  Random text: mostly ASCII with line feeds, some UTF-8 and CR LF.
   function Random_Text (G : in out Generator; Length : Natural) return String is
      Result : Unbounded_String;
   begin
      while Natural (Ada.Strings.Unbounded.Length (Result)) < Length loop
         case Random (G, 20) is
            when 0 => Append (Result, ASCII.LF);
            when 1 => Append (Result, "é");
            when 2 => Append (Result, ASCII.CR & ASCII.LF);
            when 3 => Append (Result, ' ');
            when 4 => Append (Result, "日本");
            when 5 => Append (Result, ASCII.HT);
            when others => Append (Result, Character'Val (Character'Pos ('a') + Random (G, 26)));
         end case;
      end loop;
      return To_String (Result);
   end Random_Text;

   function Count_Feeds (S : String) return Natural is
      N : Natural := 0;
   begin
      for C of S loop
         if C = ASCII.LF then
            N := N + 1;
         end if;
      end loop;
      return N;
   end Count_Feeds;

   ---------------------------------------------------------------------------
   procedure Test_Pieces is
      G : Generator := Make (Seed);
      S : Text_Stores.Store;
      type Table_Access is access Text_Pieces.Table;
      T : constant Table_Access := new Text_Pieces.Table (8_192);
      Reference : Unbounded_String;
      Ok : Boolean;
      Mismatch : Natural := 0;
      Refused : Natural := 0;

      function Table_Text return String is
         Result : String (1 .. Natural (Text_Pieces.Length (T.all)));
         P : Text_Pieces.Position;
         Done : Natural := 0;
         Found : Boolean := True;
      begin
         if Result'Length = 0 then
            return Result;
         end if;
         P := Text_Pieces.Locate (T.all, 0);
         while Found loop
            declare
               Item : constant Text_Pieces.Piece := Text_Pieces.Element (T.all, P);
            begin
               Text_Stores.Read (S, Item.First, Result (Done + 1 .. Done + Natural (Item.Length)));
               Done := Done + Natural (Item.Length);
            end;
            Text_Pieces.Next (T.all, P, Found);
         end loop;
         return Result;
      end Table_Text;

      procedure Insert_At (Offset : Byte_Offset; Text : String) is
         Done : Natural := 0;
      begin
         while Done < Text'Length loop
            declare
               Take : constant Natural :=
                 Natural'Min (Natural'Min (Text'Length - Done, Natural (Text_Stores.Room (S))), MAXIMUM_PIECE_BYTES);
               Item : Text_Pieces.Piece;
            begin
               Item.First := Text_Stores.Length (S);
               Text_Stores.Append (S, Text (Text'First + Done .. Text'First + Done + Take - 1));
               Item.Length := Piece_Length (Take);
               Item.Feeds := Text_Stores.Line_Feeds (S, Item.First, Item.Length);
               Text_Pieces.Insert (T.all, S, Offset + Byte_Count (Done), Item, Ok);
               if not Ok then
                  Refused := Refused + 1;
               end if;
               Done := Done + Take;
            end;
         end loop;
      end Insert_At;
   begin
      Section ("piece table vs reference string");
      Text_Stores.Create (S, 512 * 1_048_576);
      Text_Pieces.Initialize (T.all);
      for Step in 1 .. 6_000 loop
         declare
            Length : constant Natural := Natural (Text_Pieces.Length (T.all));
            Kind : constant Natural := Random (G, 10);
         begin
            if Kind < 6 or else Length = 0 then
               declare
                  Size : constant Natural :=
                    (case Random (G, 20) is when 0 => Random (G, 40_000), when 1 .. 3 => Random (G, 200),
                                             when others => 1 + Random (G, 4));
                  Text : constant String := Random_Text (G, Size);
                  Offset : constant Natural := Random (G, Length + 1);
               begin
                  Insert_At (Byte_Offset (Offset), Text);
                  Insert (Reference, Offset + 1, Text);
               end;
            else
               declare
                  First : constant Natural := Random (G, Length);
                  Count : constant Natural :=
                    Natural'Min (Length - First, (if Random (G, 10) = 0 then Random (G, 50_000) else 1 + Random (G, 20)));
               begin
                  Text_Pieces.Delete (T.all, S, Byte_Offset (First), Byte_Count (Count), Ok);
                  if not Ok then
                     Refused := Refused + 1;
                  end if;
                  Delete (Reference, First + 1, First + Count);
               end;
            end if;
            if Natural (Text_Pieces.Length (T.all)) /= Ada.Strings.Unbounded.Length (Reference)
              or else not Text_Pieces.Consistent (T.all)
            then
               Mismatch := Mismatch + 1;
            end if;
            if Step mod 250 = 0 then
               declare
                  Got : constant String := Table_Text;
                  Want : constant String := To_String (Reference);
               begin
                  Check (Got = Want, "pieces: content after" & Step'Image & " edits");
                  Check (Natural (Text_Pieces.Line_Feeds (T.all)) = Count_Feeds (Want),
                         "pieces: line feeds after" & Step'Image & " edits");
               end;
            end if;
         end;
      end loop;
      Check (Mismatch = 0, "pieces: length and Valid after every edit (" & Mismatch'Image & " mismatches)");
      Check (Refused = 0, "pieces: every insert and delete accepted (" & Refused'Image & " refused)");
      Note ("pieces in use:" & Text_Pieces.Piece_Count (T.all)'Image & ", blocks:"
            & Text_Pieces.Blocks_Used (T.all)'Image);
      Text_Stores.Destroy (S);
   end Test_Pieces;

   ---------------------------------------------------------------------------
   --  Lines through the document against the reference.
   procedure Test_Lines is
      G : Generator := Make (Seed + 1);
      D : Document_Access := Create (Limits_For (4 * 1_048_576, 16 * 1_048_576), "lines.txt");
      Text : constant String := Random_Text (G, 300_000);
      Ok : Boolean;
      Starts : array (1 .. Count_Feeds (Text) + 1) of Natural := [others => 0];
      Bad_Start, Bad_Line : Natural := 0;
   begin
      Section ("line index vs reference");
      Begin_Load (D.all, Text'Length);
      for K in 0 .. (Text'Length - 1) / 7_919 loop
         Load_Chunk (D.all, Text (Text'First + K * 7_919 .. Natural'Min (Text'Last, Text'First + (K + 1) * 7_919 - 1)), Ok);
      end loop;
      End_Load (D.all);
      Check (Content (D.all) = Text, "lines: loaded content");
      declare
         L : Natural := 1;
      begin
         Starts (1) := 0;
         for I in Text'Range loop
            if Text (I) = ASCII.LF then
               L := L + 1;
               Starts (L) := I - Text'First + 1;
            end if;
         end loop;
      end;
      Check (Natural (Text_Documents.Line_Total (D.all)) = Starts'Length, "lines: line count");
      for Trial in 1 .. 5_000 loop
         declare
            L : constant Positive := 1 + Random (G, Starts'Length);
            Offset : constant Natural := Random (G, Text'Length + 1);
            Want_Line : Natural := 1;
         begin
            if Natural (Line_Start (D.all, Line_Number (L))) /= Starts (L) then
               Bad_Start := Bad_Start + 1;
            end if;
            for K in Starts'Range loop
               exit when Starts (K) > Offset;
               Want_Line := K;
            end loop;
            if Natural (Line_Of (D.all, Byte_Offset (Offset))) /= Want_Line then
               Bad_Line := Bad_Line + 1;
            end if;
         end;
      end loop;
      Check (Bad_Start = 0, "lines: Line_Start for 5,000 random lines (" & Bad_Start'Image & " wrong)");
      Check (Bad_Line = 0, "lines: Line_Of for 5,000 random offsets (" & Bad_Line'Image & " wrong)");
      --  Consecutive lines, as rows are laid out (Line_Start scans on from
      --  the line before; Line_End is cached), across random edits.
      declare
         Bad_Run : Natural := 0;
      begin
         for Round in 1 .. 200 loop
            Set_Selection (D.all, Byte_Offset (Random (G, Natural (Length (D.all)) + 1)),
                           Byte_Offset (Random (G, Natural (Length (D.all)) + 1)));
            if Round mod 3 = 0 then
               Delete_Backward (D.all);
            else
               Insert_Text (D.all, Random_Text (G, 1 + Random (G, 40)));
            end if;
            declare
               Now : constant String := Content (D.all);
               Total : constant Natural := Natural (Line_Total (D.all));
               First : constant Positive := 1 + Random (G, Total);
               L : Positive := 1;
               Start : Natural := 0;
               I : Positive := Now'First;
            begin
               --  The reference: walk to line First, then check 120 lines.
               while L < First loop
                  if Now (I) = ASCII.LF then
                     L := L + 1;
                     Start := I - Now'First + 1;
                  end if;
                  I := I + 1;
               end loop;
               for Line in First .. Natural'Min (Total, First + 119) loop
                  declare
                     Stop : Natural := Start;
                  begin
                     while Stop < Now'Length and then Now (Now'First + Stop) /= ASCII.LF loop
                        Stop := Stop + 1;
                     end loop;
                     declare
                        Want_End : constant Natural :=
                          (if Stop < Now'Length and then Stop > Start and then Now (Now'First + Stop - 1) = ASCII.CR
                           then Stop - 1 else Stop);
                     begin
                        if Natural (Line_Start (D.all, Line_Number (Line))) /= Start
                          or else Natural (Line_End (D.all, Line_Number (Line))) /= Want_End
                        then
                           Bad_Run := Bad_Run + 1;
                        end if;
                     end;
                     Start := Stop + 1;
                  end;
               end loop;
            end;
         end loop;
         Check (Bad_Run = 0, "lines: consecutive Line_Start and Line_End across 200 edits (" & Bad_Run'Image
                & " wrong)");
      end;
      Destroy (D);
   end Test_Lines;

   ---------------------------------------------------------------------------
   --  Random commands with several cursors; every group's result is
   --  remembered, then undone and redone in order.
   procedure Test_Undo is
      G : Generator := Make (Seed + 2);
      D : Document_Access := Create (Limits_For (1_048_576, 64 * 1_048_576), "undo.adb");
      type Snapshot_Array is array (0 .. 1_500) of Unbounded_String;
      Snapshots : access Snapshot_Array := new Snapshot_Array;
      Count : Natural := 0;
      Now : Unsigned_64 := 0;
      Settings : constant Layout_Settings := (others => <>);
      Ok : Boolean;
      Undo_Bad, Redo_Bad : Natural := 0;
   begin
      Section ("undo and redo vs snapshots");
      Begin_Load (D.all, 0);
      Load_Chunk (D.all, Random_Text (G, 4_000), Ok);
      End_Load (D.all);
      Snapshots (0) := To_Unbounded_String (Content (D.all));
      for Step in 1 .. Snapshots'Last loop
         declare
            Before : constant String := Content (D.all);
            Length : constant Natural := Before'Length;
         begin
            Now := Now + 2 * Text_History.COALESCE_GAP_US;
            --  Place one to four cursors.
            Set_Caret (D.all, Byte_Offset (Random (G, Length + 1)));
            for Extra in 1 .. Random (G, 4) loop
               declare
                  A : constant Natural := Random (G, Length + 1);
               begin
                  Add_Selection (D.all, Byte_Offset (A), Byte_Offset (Natural'Min (Length, A + Random (G, 3) * Random (G, 6))));
               end;
            end loop;
            case Random (G, 11) is
               when 0 | 1 | 2 => Insert_Text (D.all, Random_Text (G, 1 + Random (G, 30)), Now);
               when 3 => Delete_Backward (D.all, Now);
               when 4 => Delete_Forward (D.all, Now, Whole_Word => Random (G, 2) = 0);
               when 5 => New_Line (D.all, Now);
               when 6 => Paste (D.all, Random_Text (G, Random (G, 3_000)), Now);
               when 7 => Delete_Lines (D.all);
               when 8 => Indent (D.all, Settings);
               when 9 => Toggle_Comment (D.all);
               when others => Outdent (D.all, Settings);
            end case;
            if Content (D.all) /= Before then
               Count := Count + 1;
               Snapshots (Count) := To_Unbounded_String (Content (D.all));
            end if;
         end;
         exit when Count = Snapshots'Last;
      end loop;
      Note ("groups recorded:" & Count'Image);
      for K in reverse 0 .. Count - 1 loop
         Undo (D.all);
         if Content (D.all) /= To_String (Snapshots (K)) then
            Undo_Bad := Undo_Bad + 1;
         end if;
      end loop;
      Check (Undo_Bad = 0, "undo: every step restores the text before it (" & Undo_Bad'Image & " wrong)");
      Check (not Can_Undo (D.all), "undo: nothing left to undo");
      for K in 1 .. Count loop
         Redo (D.all);
         if Content (D.all) /= To_String (Snapshots (K)) then
            Redo_Bad := Redo_Bad + 1;
         end if;
      end loop;
      Check (Redo_Bad = 0, "redo: every step reapplies (" & Redo_Bad'Image & " wrong)");
      Destroy (D);
   end Test_Undo;

   procedure Test_Coalescing is
      D : Document_Access := Create (Limits_For (1_024, 1_048_576), "c.txt");
      Ok : Boolean;
      Now : Unsigned_64 := 1_000;
   begin
      Section ("typing coalesces into undo steps");
      Begin_Load (D.all, 0);
      Load_Chunk (D.all, "", Ok);
      End_Load (D.all);
      for C of String'("hello world") loop
         Insert_Text (D.all, [C], Now, Typed => True);
         Now := Now + 50_000;
      end loop;
      Check (Content (D.all) = "hello world", "coalesce: typed text");
      Undo (D.all);
      Check (Content (D.all) = "hello", "coalesce: one undo takes back the last word and its space ("
             & Content (D.all) & ")");
      Undo (D.all);
      Check (Content (D.all) = "", "coalesce: the next undo takes the first word");
      Redo (D.all);
      Redo (D.all);
      Check (Content (D.all) = "hello world", "coalesce: redo both");
      --  A pause longer than the gap starts a new step.
      Insert_Text (D.all, "x", Now, Typed => True);
      Insert_Text (D.all, "y", Now + 2 * Text_History.COALESCE_GAP_US, Typed => True);
      Undo (D.all);
      Check (Content (D.all) = "hello worldx", "coalesce: a pause splits steps");
      --  Modified tracks the saved state across undo.
      Mark_Saved (D.all);
      Check (not Modified (D.all), "saved: not modified after save");
      Insert_Text (D.all, "z", Now + 10 * Text_History.COALESCE_GAP_US);
      Check (Modified (D.all), "saved: modified after an edit");
      Undo (D.all);
      Check (not Modified (D.all), "saved: unmodified after undoing back to the save");
      Destroy (D);
   end Test_Coalescing;

   --  Direct checks on the log (holes the mutation checks found).
   procedure Test_History_Records is
      H : access Text_History.History := new Text_History.History (8, 16, 16, 16);
      S : Text_Cursors.Set (4);
      Continued, Ok : Boolean;
   begin
      Section ("history records stay apart by kind");
      Text_History.Initialize (H.all);
      Text_Cursors.Initialize (S, 0);
      Text_History.Begin_Group (H.all, Text_History.Other, S, 5, 0, True, Continued, Ok);
      Text_History.Record_Remove (H.all, 5, 3, Ok);
      Text_History.Record_Removed_Piece (H.all, (First => 100, Length => 3, Feeds => 0), Ok);
      Text_History.Record_Insert (H.all, 8, (First => 200, Length => 2, Feeds => 0), Ok);
      Check (Text_History.Record_Count (H.all) = 2, "an insert right after a removal is a record of its own");
      Check (Text_History.Record_At (H.all, 2).Kind = Text_History.Inserted
             and then Text_History.Record_At (H.all, 1).Length = 3,
             "the removal keeps its length; the insert is an insert");
      Text_History.Record_Insert (H.all, 10, (First => 202, Length => 1, Feeds => 0), Ok);
      Check (Text_History.Record_Count (H.all) = 2 and then Text_History.Record_At (H.all, 2).Length = 3,
             "a contiguous insert extends the insert record");
   end Test_History_Records;

   procedure Test_History_Capacity is
      Settings : Text_Documents.Limits := Limits_For (1_024, 1_048_576);
      D : Document_Access;
      Ok : Boolean;
      Undone : Natural := 0;
   begin
      Section ("history at capacity forgets the oldest steps");
      Settings.History := (Groups => 8, Records => 64, Pieces => 64, Selections => 64);
      D := Create (Settings, "h.txt");
      Begin_Load (D.all, 0);
      Load_Chunk (D.all, "", Ok);
      End_Load (D.all);
      for K in 1 .. 100 loop
         Insert_Text (D.all, Character'Val (Character'Pos ('a') + K mod 26) & "");
      end loop;
      Check (Content (D.all)'Length = 100, "capacity: every edit applied");
      while Can_Undo (D.all) loop
         Undo (D.all);
         Undone := Undone + 1;
      end loop;
      Check (Undone in 1 .. 8, "capacity: some recent steps remain undoable (" & Undone'Image & ")");
      Check (Content (D.all)'Length = 100 - Undone, "capacity: undo of the kept steps is exact");
      Destroy (D);
   end Test_History_Capacity;

   ---------------------------------------------------------------------------
   procedure Test_Cursors is
      G : Generator := Make (Seed + 3);
      Bad : Natural := 0;
   begin
      Section ("selections normalize vs reference");
      for Trial in 1 .. 2_000 loop
         declare
            S : Text_Cursors.Set (64);
            N : constant Positive := 1 + Random (G, 40);
            Added : Boolean;
            type Range_Array is array (1 .. 64) of Text_Cursors.Selection;
            Items : Range_Array;
            type Coverage is array (0 .. 200) of Boolean;
            Covered : Coverage := [others => False];
            Carets : array (0 .. 200) of Boolean := [others => False];
         begin
            for K in 1 .. N loop
               declare
                  A : constant Natural := Random (G, 150);
                  B : constant Natural := (if Random (G, 3) = 0 then A else Natural'Min (199, A + Random (G, 12)));
               begin
                  Items (K) := (Anchor => Byte_Offset (A), Head => Byte_Offset (B), others => <>);
                  if K = 1 then
                     Text_Cursors.Initialize (S);
                     Text_Cursors.Replace (S, 1, Items (K));
                  else
                     Text_Cursors.Add (S, Items (K), False, Added);
                  end if;
                  for P in Natural'Min (A, B) .. Natural'Max (A, B) - 1 loop
                     Covered (P) := True;
                  end loop;
                  if A = B then
                     Carets (A) := True;
                  end if;
               end;
            end loop;
            Text_Cursors.Normalize (S);
            if not Text_Cursors.Is_Normal (S) then
               Bad := Bad + 1;
            end if;
            --  Every covered byte is still covered, nothing else is.
            declare
               Now_Covered : Coverage := [others => False];
            begin
               for K in 1 .. Text_Cursors.Count (S) loop
                  declare
                     E : constant Text_Cursors.Selection := Text_Cursors.Element (S, K);
                  begin
                     for P in Text_Cursors.First_Of (E) .. Text_Cursors.Last_Of (E) - 1 loop
                        Now_Covered (Natural (P)) := True;
                     end loop;
                  end;
               end loop;
               if Now_Covered /= Covered then
                  Bad := Bad + 1;
               end if;
            end;
            --  Every caret is inside or at the edge of some selection.
            for P in Carets'Range loop
               if Carets (P) then
                  declare
                     Kept : Boolean := False;
                  begin
                     for K in 1 .. Text_Cursors.Count (S) loop
                        declare
                           E : constant Text_Cursors.Selection := Text_Cursors.Element (S, K);
                        begin
                           Kept := Kept or else (Natural (Text_Cursors.First_Of (E)) <= P
                                                 and then P <= Natural (Text_Cursors.Last_Of (E)));
                        end;
                     end loop;
                     if not Kept then
                        Bad := Bad + 1;
                     end if;
                  end;
               end if;
            end loop;
         end;
      end loop;
      Check (Bad = 0, "selections: 2,000 random sets sorted, merged, nothing lost (" & Bad'Image & " wrong)");
      declare
         S : Text_Cursors.Set (8);
         Added : Boolean;
      begin
         Text_Cursors.Initialize (S, 0);
         Text_Cursors.Replace (S, 1, (Anchor => 0, Head => 5, others => <>));
         Text_Cursors.Add (S, (Anchor => 5, Head => 9, others => <>), False, Added);
         Text_Cursors.Normalize (S);
         Check (Text_Cursors.Count (S) = 2, "touching selections stay apart");
         Text_Cursors.Add (S, Text_Cursors.Caret (9), False, Added);
         Text_Cursors.Normalize (S);
         Check (Text_Cursors.Count (S) = 2, "a cursor at a selection's edge merges into it");
      end;
   end Test_Cursors;

   ---------------------------------------------------------------------------
   --  A plain decoder to compare with.
   procedure Reference_Decode (S : String; I : Positive; Code : out Natural; Size : out Positive; Valid : out Boolean) is
      B0 : constant Natural := Character'Pos (S (I));
      function Cont (K : Positive) return Boolean is
        (I + K <= S'Last and then Character'Pos (S (I + K)) / 64 = 2);
      function V (K : Positive) return Natural is (Character'Pos (S (I + K)) mod 64);
   begin
      Code := B0;
      Size := 1;
      Valid := B0 < 128;
      if B0 >= 16#C2# and then B0 <= 16#DF# and then Cont (1) then
         Code := (B0 mod 32) * 64 + V (1);
         Size := 2;
         Valid := True;
      elsif B0 >= 16#E0# and then B0 <= 16#EF# and then Cont (1) and then Cont (2) then
         Code := (B0 mod 16) * 4096 + V (1) * 64 + V (2);
         if Code >= 16#800# and then (Code < 16#D800# or else Code > 16#DFFF#) then
            Size := 3;
            Valid := True;
         else
            Code := B0;
         end if;
      elsif B0 >= 16#F0# and then B0 <= 16#F4# and then Cont (1) and then Cont (2) and then Cont (3) then
         Code := (B0 mod 8) * 262_144 + V (1) * 4096 + V (2) * 64 + V (3);
         if Code >= 16#1_0000# and then Code <= 16#10_FFFF# then
            Size := 4;
            Valid := True;
         else
            Code := B0;
         end if;
      end if;
   end Reference_Decode;

   procedure Test_UTF8 is
      G : Generator := Make (Seed + 4);
      Bad : Natural := 0;
      function Size_Of (S : String) return Natural is (Text_UTF8.Cluster_Size (S, S'First));
   begin
      Section ("UTF-8 and clusters");
      for Trial in 1 .. 200_000 loop
         declare
            S : String (1 .. 4);
            Code : Text_UTF8.Code_Point;
            Size : Text_UTF8.Sequence_Length;
            Valid : Boolean;
            R_Code : Natural;
            R_Size : Positive;
            R_Valid : Boolean;
         begin
            for K in S'Range loop
               S (K) := Character'Val ((if Random (G, 2) = 0 then 16#80# + Random (G, 64) else Random (G, 256)));
            end loop;
            S (1) := Character'Val ((case Random (G, 4) is when 0 => 16#C0# + Random (G, 32),
                                                          when 1 => 16#E0# + Random (G, 16),
                                                          when 2 => 16#F0# + Random (G, 16),
                                                          when others => Random (G, 256)));
            Text_UTF8.Decode (S, 1, Code, Size, Valid);
            Reference_Decode (S, 1, R_Code, R_Size, R_Valid);
            if Valid /= R_Valid or else (Valid and then (Natural (Code) /= R_Code or else Size /= R_Size)) then
               Bad := Bad + 1;
            end if;
         end;
      end loop;
      Check (Bad = 0, "utf-8: 200,000 random sequences decode as the reference (" & Bad'Image & " wrong)");
      Check (Size_Of ("e" & Character'Val (16#CC#) & Character'Val (16#81#) & "x") = 3, "cluster: e + combining acute");
      Check (Size_Of (ASCII.CR & ASCII.LF & "x") = 2, "cluster: CR LF");
      Check (Size_Of ("🇺🇸🇫🇷") = 8, "cluster: one regional-indicator pair (flag)");
      Check (Size_Of ("👩‍💻x") = 11, "cluster: ZWJ sequence (woman technologist)");
      Check (Size_Of ("👍🏽x") = 8, "cluster: emoji with skin-tone modifier");
      Check (Size_Of (Character'Val (16#FF#) & "a") = 1, "cluster: an invalid byte alone");
      Check (Size_Of ("日本") = 3, "cluster: CJK character");
      Check (Text_UTF8.Width (16#65E5#) = 2, "width: CJK is wide");
      Check (Text_UTF8.Width (Character'Pos ('a')) = 1, "width: ASCII is narrow");
   end Test_UTF8;

   ---------------------------------------------------------------------------
   procedure Test_Layout is
      G : Generator := Make (Seed + 5);
      Bad_Rows, Bad_Width, Bad_Round : Natural := 0;
   begin
      Section ("layout: wrap rows and columns");
      for Trial in 1 .. 3_000 loop
         declare
            Text : constant String := Random_Text (G, Random (G, 600));
            Clean : String := Text;
            Width : constant Natural := 1 + Random (G, 80);
            Tab : constant Tab_Width := 1 + Random (G, 8);
            Starts : Text_Layout.Row_Starts;
            Count : Text_Layout.Row_Index;
         begin
            for C of Clean loop
               if C in ASCII.LF | ASCII.CR then
                  C := ' ';
               end if;
            end loop;
            Text_Layout.Wrap (Clean, Width, Tab, Starts, Count);
            for K in 2 .. Count loop
               if Starts (K) <= Starts (K - 1) then
                  Bad_Rows := Bad_Rows + 1;
               end if;
            end loop;
            --  Each row fits unless it is one cluster.
            for K in 1 .. Count loop
               declare
                  First : constant Positive := Starts (K);
                  Last : constant Natural := (if K < Count then Starts (K + 1) - 1 else Clean'Length);
                  Row : constant String (1 .. Last - First + 1) := Clean (First .. Last);
               begin
                  if Row'Length > 0 and then Text_Layout.Column_Of (Row, Row'Last + 1, Tab) > Width
                    and then Text_Layout.Next_Boundary (Row, 1) <= Row'Last
                    and then (for some I in Row'Range => Row (I) in ' ' | ASCII.HT) = False
                  then
                     null;   --  a word longer than the row: broken at a cluster
                  elsif Row'Length > 0 and then Text_Layout.Column_Of (Row, Row'Last + 1, Tab) > Width
                    and then Text_Layout.Next_Boundary (Row, 1) <= Row'Last
                    and then not (Row (Row'Last) in ' ' | ASCII.HT)
                  then
                     Bad_Width := Bad_Width + 1;
                  end if;
               end;
            end loop;
            --  Column of a boundary, back to the same boundary.
            if Clean'Length > 0 then
               declare
                  I : Positive := 1;
               begin
                  while I <= Clean'Last loop
                     if Text_Layout.Index_At_Column (Clean, Text_Layout.Column_Of (Clean, I, Tab), Tab) /= I
                       and then Text_Layout.Advance (Clean, I, Text_Layout.Column_Of (Clean, I, Tab), Tab) > 0
                     then
                        Bad_Round := Bad_Round + 1;
                     end if;
                     I := Text_Layout.Next_Boundary (Clean, I);
                  end loop;
               end;
            end if;
         end;
      end loop;
      Check (Bad_Rows = 0, "layout: row starts increase (" & Bad_Rows'Image & " wrong)");
      declare
         Starts : Text_Layout.Row_Starts;
         Count : Text_Layout.Row_Index;
      begin
         Text_Layout.Wrap ("hello world foo", 8, 4, Starts, Count);
         Check (Count = 3 and then Starts (2) = 7 and then Starts (3) = 13,
                "wrap breaks after the last space that fits (hello / world / foo)");
         Text_Layout.Wrap ("abcdefghijkl", 5, 4, Starts, Count);
         Check (Count = 3 and then Starts (2) = 6 and then Starts (3) = 11, "a word longer than the row breaks at the width");
      end;
      Check (Bad_Width = 0, "layout: rows fit the width except unbreakable words (" & Bad_Width'Image & " wrong)");
      Check (Bad_Round = 0, "layout: column <-> index round trip at boundaries (" & Bad_Round'Image & " wrong)");
   end Test_Layout;

   ---------------------------------------------------------------------------
   procedure Test_Search is
      G : Generator := Make (Seed + 6);
      Bad_Literal, Bad_Regex, Compared, Loose_Count : Natural := 0;
      Alphabet : constant String := "abAB c";
   begin
      Section ("search: literal vs naive, regex vs Python re");
      for Trial in 1 .. 20_000 loop
         declare
            Text : String (1 .. Random (G, 60));
            Pattern : String (1 .. 1 + Random (G, 3));
            Settings : constant Text_Search.Options :=
              (Case_Sensitive => Random (G, 2) = 0, Whole_Word => Random (G, 3) = 0, Regular => False);
            Found : Boolean;
            At_Index : Positive;
            Want : Natural := 0;
            function Fold (C : Character) return Character is
              (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);
            function Is_Word (C : Character) return Boolean is (C in 'a' .. 'z' | 'A' .. 'Z');
         begin
            for C of Text loop
               C := Alphabet (1 + Random (G, Alphabet'Length));
            end loop;
            for C of Pattern loop
               C := Alphabet (1 + Random (G, 4));
            end loop;
            for K in 1 .. Text'Length - Pattern'Length + 1 loop
               if (for all J in Pattern'Range =>
                     (if Settings.Case_Sensitive then Text (K + J - 1) = Pattern (J)
                      else Fold (Text (K + J - 1)) = Fold (Pattern (J))))
                 and then (not Settings.Whole_Word
                           or else ((K = 1 or else not Is_Word (Text (K - 1)))
                                    and then (K + Pattern'Length > Text'Last or else not Is_Word (Text (K + Pattern'Length)))))
               then
                  Want := K;
                  exit;
               end if;
            end loop;
            Text_Search.Find_Literal (Text, Pattern, 1, Settings, ' ', ' ', Found, At_Index);
            if Found /= (Want > 0) or else (Found and then At_Index /= Want) then
               Bad_Literal := Bad_Literal + 1;
            end if;
         end;
      end loop;
      Check (Bad_Literal = 0, "literal: 20,000 random searches as the naive reference (" & Bad_Literal'Image & " wrong)");
      --  The regex corpus: run.sh writes it with Python re's answers
      --  (regex_reference.py).
      declare
         Corpus : File_Type;
         --  Field N, 1-based (the search functions take 1-based text).
         function Field (Line : String; N : Positive) return String is
            Start : Positive := Line'First;
            Count : Positive := 1;
            function Rebased (Part : String) return String is
               Result : constant String (1 .. Part'Length) := Part;
            begin
               return Result;
            end Rebased;
         begin
            for I in Line'Range loop
               if Line (I) = ASCII.HT then
                  if Count = N then
                     return Rebased (Line (Start .. I - 1));
                  end if;
                  Count := Count + 1;
                  Start := I + 1;
               end if;
            end loop;
            return Rebased (Line (Start .. Line'Last));
         end Field;
      begin
         Open (Corpus, In_File, "build/regex-cases.txt");
         while not End_Of_File (Corpus) loop
            declare
               Line : constant String := Get_Line (Corpus);
               Pattern : constant String := Field (Line, 1);
               Raw : constant String := Field (Line, 2);
               Text : constant String (1 .. Raw'Length) := Raw;
               Case_Sensitive : constant Boolean := Field (Line, 3) = "1";
               Want_Found : constant Boolean := Field (Line, 4) = "1";
               Want_First : constant Natural := Natural'Value (Field (Line, 5));
               Want_Length : constant Natural := Natural'Value (Field (Line, 6));
               Loose : constant Boolean := Field (Line, 7) = "1";
               Program : Text_Search.Program;
               Status : Text_Search.Compile_Result;
               Found : Boolean;
               First : Positive;
               Length : Natural;
               use type Text_Search.Compile_Result;
            begin
               Text_Search.Compile (Pattern, Case_Sensitive, Program, Status);
               Compared := Compared + 1;
               if Status /= Text_Search.Compiled then
                  Bad_Regex := Bad_Regex + 1;
                  if Bad_Regex <= 5 then
                     Put_Line ("  regex did not compile: /" & Pattern & "/ " & Status'Image);
                  end if;
               else
                  Text_Search.Match (Program, Text, 1, Found, First, Length);
                  if Loose then
                     Loose_Count := Loose_Count + 1;
                  end if;
                  if Found /= Want_Found
                    or else (Found and then (First /= Want_First or else (not Loose and then Length /= Want_Length)))
                  then
                     Bad_Regex := Bad_Regex + 1;
                     if Bad_Regex <= 5 then
                        Put_Line ("  regex differs: /" & Pattern & "/ on """ & Text & """ case " & Case_Sensitive'Image
                                  & ": ours " & Found'Image & First'Image & Length'Image & ", re "
                                  & Want_Found'Image & Want_First'Image & Want_Length'Image);
                     end if;
                  end if;
               end if;
            end;
         end loop;
         Close (Corpus);
      exception
         when Name_Error =>
            Check (False, "regex: corpus build/regex-cases.txt missing (run.sh writes it)");
      end;
      Check (Compared > 0 and then Bad_Regex = 0,
             "regex: random patterns match as Python re (" & Bad_Regex'Image & " wrong of" & Compared'Image & ";"
             & Loose_Count'Image & " repeat an empty-matching group: start compared, not length)");
      declare
         Program : Text_Search.Program;
         Status : Text_Search.Compile_Result;
         Found : Boolean;
         First : Positive;
         Length : Natural;
         Long : constant String (1 .. SEGMENT_BYTES) := [others => 'a'];
         use type Text_Search.Compile_Result;
      begin
         Text_Search.Compile ("(a*)*b", True, Program, Status);
         Text_Search.Match (Program, Long, 1, Found, First, Length);
         Check (Status = Text_Search.Compiled and then not Found,
                "regex: (a*)*b on 16 KiB of 'a' ends (no exponential backtracking)");
         Text_Search.Compile ("a(b", True, Program, Status);
         Check (Status = Text_Search.Unbalanced, "regex: unbalanced group refused");
         Text_Search.Compile ("*a", True, Program, Status);
         Check (Status = Text_Search.Bad_Repeat, "regex: leading repeat refused");
         Text_Search.Compile ("[z-a]", True, Program, Status);
         Check (Status = Text_Search.Bad_Class, "regex: reversed class range refused");
      end;
   end Test_Search;

   ---------------------------------------------------------------------------
   procedure Test_Syntax is
      Spans : Text_Syntax.Span_Array;
      Count : Text_Syntax.Span_Count;
      Finish : Text_Syntax.Line_State;
      Line : Unbounded_String;
      --  The kind at the first occurrence of Needle in the last line lexed.
      function Kind_At (Needle : String; Position : Natural := 0) return Text_Syntax.Token_Kind is
         P : constant Natural := (if Position > 0 then Position else Index (Line, Needle));
      begin
         for K in 1 .. Count loop
            if P >= Spans (K).First and then P < Spans (K).First + Spans (K).Length then
               return Spans (K).Kind;
            end if;
         end loop;
         return Text_Syntax.Plain;
      end Kind_At;
      procedure Lex (Item : Text_Syntax.Language; Text : String;
                     Start : Text_Syntax.Line_State := Text_Syntax.INITIAL_STATE) is
         Normal : constant String (1 .. Text'Length) := Text;
         From : constant Text_Syntax.Line_State := Start;
      begin
         Line := To_Unbounded_String (Text);
         Text_Syntax.Lex_Line (Item, Normal, From, Spans, Count, Finish);
      end Lex;
      G : Generator := Make (Seed + 7);
      Bad : Natural := 0;
   begin
      Section ("syntax: lexers");
      Lex (Text_Syntax.Ada_Source, "procedure P is X : Integer := 16#FF#; -- note");
      Check (Kind_At ("procedure") = Text_Syntax.Keyword, "ada: procedure is a keyword");
      Check (Kind_At ("Integer") = Text_Syntax.Type_Name, "ada: Integer is a type");
      Check (Kind_At ("16#FF#") = Text_Syntax.Number and then Kind_At ("FF#") = Text_Syntax.Number, "ada: based number");
      Check (Kind_At ("note") = Text_Syntax.Comment, "ada: comment");
      Lex (Text_Syntax.Ada_Source, "C := 'x'; N := A'Length; S := ""a""""b"";");
      Check (Kind_At ("'x'") = Text_Syntax.Character_Text, "ada: character literal");
      Check (Kind_At ("Length") = Text_Syntax.Attribute, "ada: attribute tick");
      Check (Kind_At ("b""") = Text_Syntax.String_Text, "ada: string with doubled quote");
      Lex (Text_Syntax.C_Source, "int x = 1; /* open");
      Check (Kind_At ("int") = Text_Syntax.Type_Name and then Finish.Mode = Text_Syntax.Block_Comment,
             "c: type, then a comment that continues");
      Lex (Text_Syntax.C_Source, "still */ return 0;", Finish);
      Check (Kind_At ("still") = Text_Syntax.Comment and then Kind_At ("return") = Text_Syntax.Keyword
             and then Finish.Mode = Text_Syntax.Normal, "c: comment closes, keyword after");
      Lex (Text_Syntax.C_Source, "#define X \");
      Check (Finish.Mode = Text_Syntax.Continued_Directive, "c: directive continues");
      Lex (Text_Syntax.Rust_Source, "/* a /* nested */ still */ fn main() { let s = r#""x""#; }");
      Check (Kind_At ("still") = Text_Syntax.Comment and then Kind_At ("fn") = Text_Syntax.Keyword,
             "rust: nested comment closes at the right place");
      Check (Kind_At ("#""x") = Text_Syntax.String_Text, "rust: raw string");
      Lex (Text_Syntax.Rust_Source, "fn f<'a>(x: &'a str) -> char { 'c' }");
      Check (Kind_At ("'a>") = Text_Syntax.Label and then Kind_At ("'c'") = Text_Syntax.Character_Text,
             "rust: lifetime vs character");
      Lex (Text_Syntax.CCL_Source, "(setting ""key"" (Launch_Entry label => ""x"")) # comment");
      Check (Kind_At ("""key") = Text_Syntax.String_Text and then Kind_At ("Launch_Entry") = Text_Syntax.Type_Name
             and then Kind_At ("label") = Text_Syntax.Label and then Kind_At ("comment") = Text_Syntax.Comment,
             "ccl: string, type, label, comment");
      Lex (Text_Syntax.Markdown, "# Title");
      Check (Kind_At ("Title") = Text_Syntax.Heading, "markdown: heading");
      Lex (Text_Syntax.Markdown, "Some `code` and **strong** and [link](url)");
      Check (Kind_At ("code") = Text_Syntax.Code and then Kind_At ("strong") = Text_Syntax.Strong
             and then Kind_At ("link") = Text_Syntax.Link, "markdown: inline code, strong, link");
      Lex (Text_Syntax.Markdown, "```ada");
      Check (Finish.Mode = Text_Syntax.Fenced_Code, "markdown: fence opens");
      --  Random bytes never break the span contract (checked by -gnata).
      for Trial in 1 .. 20_000 loop
         declare
            Text : constant String := Random_Text (G, Random (G, 120)) & "/*""'#`*[";
            Item : constant Text_Syntax.Language := Text_Syntax.Language'Val (Random (G, 6));
            Start : constant Text_Syntax.Line_State :=
              (Mode => Text_Syntax.Lexer_Mode'Val (Random (G, 6)), Depth => Random (G, 3), Hashes => Random (G, 3));
         begin
            Lex (Item, Text, Start);
            for K in 2 .. Count loop
               if Spans (K).First < Spans (K - 1).First + Spans (K - 1).Length then
                  Bad := Bad + 1;
               end if;
            end loop;
         end;
      end loop;
      Check (Bad = 0, "syntax: 20,000 random lines in every language and state keep spans ordered");
   end Test_Syntax;

   ---------------------------------------------------------------------------
   procedure Test_Marks is
      D : Document_Access := Create (Limits_For (1_024, 1_048_576), "m.adb");
      Ok, Added, Done : Boolean;
   begin
      Section ("marks: anchored ranges, protected regions, host edits");
      Begin_Load (D.all, 0);
      Load_Chunk (D.all, "begin" & ASCII.LF & "   --  generated" & ASCII.LF & "   null;" & ASCII.LF & "end;", Ok);
      End_Load (D.all);
      --  "   --  generated\n" is generated (6 .. 23); "null;" region is free.
      Add_Mark (D.all, (Kind => Text_Marks.Generated, First => 6, Last => 23, Id => 7, others => <>), Added);
      Add_Mark (D.all, (Kind => Text_Marks.Diagnostic, First => 26, Last => 31, Level => Text_Marks.Error, Id => 9,
                        others => <>), Added);
      Set_Caret (D.all, 10);
      Insert_Text (D.all, "x");
      Check (Last_Refusal (D.all) = Protected_Region and then Content (D.all)'Length = 36,
             "marks: typing inside a generated region is refused");
      Set_Caret (D.all, 23);
      Insert_Text (D.all, "x");
      Check (Last_Refusal (D.all) = None and then Content (D.all)'Length = 37,
             "marks: typing at a region's end is allowed (outside it)");
      Undo (D.all);
      Set_Caret (D.all, 6);
      Insert_Text (D.all, "x");
      Check (Last_Refusal (D.all) = None, "marks: typing at a region's start is allowed (outside it)");
      Undo (D.all);
      Set_Caret (D.all, 0);
      Insert_Text (D.all, "-- top" & ASCII.LF);
      Check (Mark (D.all, 1).First = 13 and then Mark (D.all, 2).First = 33, "marks: an insert before moves them");
      Replace_Region (D.all, 7, "   Handler_Stub;" & ASCII.LF, Done);
      Check (Done and then Content (D.all) = "-- top" & ASCII.LF & "begin" & ASCII.LF & "   Handler_Stub;" & ASCII.LF
             & "   null;" & ASCII.LF & "end;", "marks: the host replaces a generated region");
      Check (Mark (D.all, 1).Last - Mark (D.all, 1).First = 17, "marks: the region covers its new text");
      Undo (D.all);
      Check (Content (D.all) = "-- top" & ASCII.LF & "begin" & ASCII.LF & "   --  generated" & ASCII.LF
             & "   null;" & ASCII.LF & "end;", "marks: a host edit undoes like any other");
      Destroy (D);
   end Test_Marks;
   --  The text interface's descriptor (docs/ccl-interface-descriptors.md),
   --  generated from the command table: every command an operation.
   function Descriptor return String is
      use Text_Commands;
      LF : constant Character := ASCII.LF;
      function Image (N : Natural) return String is
        (Natural'Image (N) (2 .. Natural'Image (N)'Last));
      function Counted (Text : String) return String is (Image (Text'Length) & ":" & Text);
      Result : Unbounded_String :=
        To_Unbounded_String ("CCL-INTERFACE-DESCRIPTOR 1" & LF & "name " & Counted ("text") & LF & "version 1 0" & LF
                             & "operations" & Natural'Image (Real_Command'Pos (Real_Command'Last)
                                                             - Real_Command'Pos (Real_Command'First) + 1) & LF);
   begin
      for C in Real_Command loop
         Append (Result, "operation " & Image (Real_Command'Pos (C) - Real_Command'Pos (Real_Command'First)) & LF
                 & "name " & Counted (Name (C)) & LF
                 & "parameters " & (if Argument_Of (C) = No_Argument then "0" else "1") & LF
                 & "argument " & (case Argument_Of (C) is
                                     when String_Argument => "string 1024",
                                     when others => "integer") & LF
                 & "result boolean" & LF & "authority control" & LF & "ownership-argument false" & LF
                 & "transfer copy" & LF & "cancellation not-cancellable" & LF & "success-verb 0" & LF
                 & "failure-verb 0" & LF & "cancel-verb 0" & LF & "end-operation" & LF);
      end loop;
      Append (Result, "end-interface" & LF);
      return To_String (Result);
   end Descriptor;

   procedure Test_Interface is
      use Ada.Streams.Stream_IO;
      F : Ada.Streams.Stream_IO.File_Type;
      Want : constant String := Descriptor;
   begin
      Section ("the text CCL interface matches the command table");
      if Write_Interface then
         Create (F, Out_File, INTERFACE_PATH);
         String'Write (Stream (F), Want);
         Close (F);
         Note ("wrote " & INTERFACE_PATH);
      end if;
      Open (F, In_File, INTERFACE_PATH);
      declare
         Got : String (1 .. Natural (Size (F)));
      begin
         String'Read (Stream (F), Got);
         Close (F);
         Check (Got = Want, "text.ccl-interface is the table's descriptor (run.sh --interface rewrites it)");
      end;
   exception
      when Ada.Streams.Stream_IO.Name_Error =>
         Check (False, "text.ccl-interface missing (run.sh --interface writes it)");
   end Test_Interface;
begin
   Put_Line ("text core tests, seed" & Seed'Image);
   Test_Interface;
   Test_Pieces;
   Test_Lines;
   Test_Undo;
   Test_Coalescing;
   Test_History_Records;
   Test_History_Capacity;
   Test_Cursors;
   Test_UTF8;
   Test_Layout;
   Test_Search;
   Test_Syntax;
   Test_Marks;
   Summary;
end Text_Core_Tests;
