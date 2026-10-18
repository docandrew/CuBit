with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Appearance;
with CuBit.UI;
with Text_Commands; use Text_Commands;
with Text_Cursors;
with Text_Documents; use Text_Documents;
with Text_Editors;
with Text_Limits; use Text_Limits;
with Text_Shell; use Text_Shell;
with Text_Host_Files; use Text_Host_Files;
with Text_Host_Support; use Text_Host_Support;

--  Benchmarks (docs/text-editor.md, "Measurements"): open time for big
--  synthetic files, keypress-to-frame, cursor and page moves, full frames,
--  search, huge undo, a 100 MB single line. Linux-hosted, built -O3 -gnatp
--  as native apps are, rendering into memory; not CuBit measurements.
--    text_bench [megabytes]   (default 100)
procedure Text_Bench is
   FILES : constant String := "/home/doc/cubit-build-tmp/text-editor/files";
   MIB : constant := 1_048_576;
   Size_MiB : constant Positive :=
     (if Ada.Command_Line.Argument_Count >= 1 then Positive'Value (Ada.Command_Line.Argument (1)) else 100);
   KEYS : constant := 400;
   --  "frames" ("frames4k"): only the open and key-to-frame part at 1080p
   --  (at 4K), for profiling.
   Four_K_Frames : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 2 and then Ada.Command_Line.Argument (2) = "frames4k";
   Frames_Only : constant Boolean :=
     Four_K_Frames or else (Ada.Command_Line.Argument_Count >= 2 and then Ada.Command_Line.Argument (2) = "frames");
   --  "line": only the single long line at 4K (for profiling).
   Line_Only : constant Boolean :=
     Ada.Command_Line.Argument_Count >= 2 and then Ada.Command_Line.Argument (2) = "line";

   Host : aliased Host_Files;

   procedure Write_File (Path : String; Total : Natural; Line_Of : access function (K : Positive) return String) is
      use Ada.Streams.Stream_IO;
      F : Ada.Streams.Stream_IO.File_Type;
      Written : Natural := 0;
      K : Positive := 1;
      Buffer : String (1 .. MIB);
      Used : Natural := 0;
   begin
      Create (F, Out_File, Path);
      while Written + Used < Total loop
         declare
            L : constant String := Line_Of (K);
         begin
            if Used + L'Length > Buffer'Length then
               declare
                  Bytes : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Used))
                    with Import, Address => Buffer'Address;
               begin
                  Write (F, Bytes);
               end;
               Written := Written + Used;
               Used := 0;
            end if;
            Buffer (Used + 1 .. Used + L'Length) := L;
            Used := Used + L'Length;
            K := K + 1;
         end;
      end loop;
      declare
         Bytes : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Used))
           with Import, Address => Buffer'Address;
      begin
         Write (F, Bytes);
      end;
      Close (F);
   end Write_File;

   Code_Lines : constant array (0 .. 7) of access constant String :=
     [new String'("   procedure Insert (T : in out Table; Offset : Byte_Offset; Item : Piece) is" & ASCII.LF),
      new String'("      --  Every block in use holds at least one piece; totals recounted." & ASCII.LF),
      new String'("      Count : constant Natural := Natural (Line_Count (D) - 1) / CHECKPOINT_LINES;" & ASCII.LF),
      new String'("   begin" & ASCII.LF),
      new String'("      if Offset > Length (T) then raise Program_Error with ""offset""; end if;" & ASCII.LF),
      new String'("      Put_Line (""value"" & Integer'Image (Count) & 'x');" & ASCII.LF),
      new String'("   end Insert;" & ASCII.LF),
      new String'(ASCII.LF & "")];
   function Code_Line (K : Positive) return String is
     (if K = Size_MiB * 20_000 then "      Needle_Near_The_End := True;" & ASCII.LF else Code_Lines (K mod 8).all);
   function Long_Piece (K : Positive) return String is
     ((if K mod 9 = 0 then " " else "") & "token" & Natural'Image (K mod 1000));

   type Sample is record
      Total, Worst : Unsigned_64 := 0;
      Count : Natural := 0;
   end record;
   procedure Add (S : in out Sample; Us : Unsigned_64) is
   begin
      S.Total := S.Total + Us;
      S.Worst := Unsigned_64'Max (S.Worst, Us);
      S.Count := S.Count + 1;
   end Add;
   function Show (S : Sample) return String is
     ("mean" & Unsigned_64'Image (S.Total / Unsigned_64'Max (1, Unsigned_64 (S.Count))) & " us, worst"
      & Unsigned_64'Image (S.Worst) & " us (" & Natural'Image (S.Count) & " runs)");

   procedure Report (What, Result : String) is
   begin
      Put_Line ("| " & What & " | " & Result & " |");
   end Report;

   --  Where key-to-frame time goes (microseconds, summed since a reset):
   --  the command, building rows and damage, painting.
   Handle_Us, Damage_Us, Render_Us : Unsigned_64 := 0;

   --  One key through the shell, then the frame it causes.
   function Key_To_Frame (App : in out Shell; Target : Surface; Item : Event) return Unsigned_64 is
      Start : constant Unsigned_64 := Now_Us;
      Handled, Built : Unsigned_64;
      Damage : CuBit.UI.Rect;
   begin
      Set_Clock (App, Start);
      Handle (App, Item);
      Handled := Now_Us;
      Take_Damage (App, Damage);
      Built := Now_Us;
      if not CuBit.UI.Is_Empty (Damage) then
         Render (App, Canvas (Target, Damage), Bounds (Target));
      end if;
      Handle_Us := Handle_Us + (Handled - Start);
      Damage_Us := Damage_Us + (Built - Handled);
      Render_Us := Render_Us + (Now_Us - Built);
      return Now_Us - Start;
   end Key_To_Frame;

   procedure Reset_Split is
   begin
      Handle_Us := 0;
      Damage_Us := 0;
      Render_Us := 0;
   end Reset_Split;

   function Split (Runs : Positive) return String is
     ("command" & Unsigned_64'Image (Handle_Us / Unsigned_64 (Runs)) & " us, rows and damage"
      & Unsigned_64'Image (Damage_Us / Unsigned_64 (Runs)) & " us, painting"
      & Unsigned_64'Image (Render_Us / Unsigned_64 (Runs)) & " us");

   procedure Open_Timed (App : in out Shell; Target : Surface; Name : String; Label : String) is
      Start : constant Unsigned_64 := Now_Us;
      First_Frame : Unsigned_64 := 0;
      Busy, Changed : Boolean;
   begin
      Open (App, Name);
      loop
         Set_Clock (App, Now_Us);
         Pump (App, 100_000, Busy, Changed);
         if First_Frame = 0 and then Active_Document (App) /= null and then Length (Active_Document (App).all) > 0
         then
            Frame (App, Target);
            First_Frame := Now_Us - Start;
         end if;
         exit when not Loading (App);
      end loop;
      Report ("Open " & Label & ": first frame", Unsigned_64'Image (First_Frame / 1_000) & " ms ("
              & Unsigned_64'Image (First_Frame) & " us)");
      Report ("Open " & Label & ": fully loaded and indexed",
              Unsigned_64'Image ((Now_Us - Start) / 1_000) & " ms," & Line_Number'Image (Line_Total (Active_Document (App).all))
              & " lines," & Natural'Image (Pieces_Used (Active_Document (App).all)) & " pieces");
      Frame (App, Target);
   end Open_Timed;

   procedure Frames (App : in out Shell; Target : Surface; Label : String) is
      Typing, Moving, Paging, Full, Scrolling : Sample;
      Ok : Boolean;
      D : constant Document_Access := Active_Document (App);
      Lines : constant Line_Number := Line_Total (D.all);
   begin
      Execute (App, Go_To_Line, Integer (Lines / 2), "", Ok);
      Frame (App, Target);
      Reset_Split;
      for K in 1 .. KEYS loop
         Add (Typing, Key_To_Frame (App, Target, Typed ([1 => Character'Val (Character'Pos ('a') + K mod 26)])));
      end loop;
      Report ("Typing split, " & Label, Split (KEYS));
      for K in 1 .. KEYS loop
         Add (Moving, Key_To_Frame (App, Target, Text_Shell.Key ((if K mod 2 = 0 then Down else Up))));
      end loop;
      Reset_Split;
      for K in 1 .. KEYS / 4 loop
         Add (Paging, Key_To_Frame (App, Target, Text_Shell.Key (Page_Down)));
      end loop;
      Report ("Page down split, " & Label, Split (KEYS / 4));
      for K in 1 .. KEYS / 4 loop
         Add (Scrolling, Key_To_Frame (App, Target, (Kind => Wheel_Event, Steps => -1, X => 400, Y => 400,
                                                     others => <>)));
      end loop;
      for K in 1 .. 50 loop
         declare
            Start : constant Unsigned_64 := Now_Us;
         begin
            Text_Editors.Invalidate (Active_Editor (App).all);
            Frame (App, Target);
            Add (Full, Now_Us - Start);
         end;
      end loop;
      Report ("Type a character, key to frame complete, " & Label, Show (Typing));
      Report ("Cursor up/down, key to frame, " & Label, Show (Moving));
      Report ("Page down, key to frame, " & Label, Show (Paging));
      Report ("Wheel scroll 3 rows, to frame, " & Label, Show (Scrolling));
      Report ("Full editor repaint, " & Label, Show (Full));
      for K in 1 .. KEYS + 4 loop
         Execute (App, Undo, 0, "", Ok);
         exit when not Ok;
      end loop;
   end Frames;

   procedure Timed_Find (App : in out Shell; Target : Surface; Pattern : String; Regular : Boolean; Label : String) is
      D : constant Document_Access := Active_Document (App);
      Status : Search_Status;
      Start : Unsigned_64;
      Total : Natural;
      Done : Boolean;
      Ok : Boolean;
   begin
      Execute (App, Move_Document_Start, 0, "", Ok);
      Set_Search (D.all, Pattern, (Case_Sensitive => True, Whole_Word => False, Regular => Regular), Status);
      Start := Now_Us;
      loop
         Find (D.all, True, 8 * MIB, Status);
         exit when Status /= Searching;
      end loop;
      Report ("Find " & Label & " (to the match)", Unsigned_64'Image ((Now_Us - Start) / 1_000) & " ms, "
              & Search_Status'Image (Status) & ", line" & Line_Number'Image (Line_Of (D.all, Primary (D.all).Head)));
      Start := Now_Us;
      loop
         Count_Matches (D.all, 8 * MIB, Total, Done);
         exit when Done;
      end loop;
      Report ("Count all matches of " & Label, Unsigned_64'Image ((Now_Us - Start) / 1_000) & " ms,"
              & Natural'Image (Total) & " matches");
      Set_Search (D.all, "", (others => <>), Status);
      Frame (App, Target);
   end Timed_Find;

   procedure Run (Width, Height : Positive; Label : String; Big : String; Long : String) is
      Target : constant Surface := New_Surface (Width, Height);
      App : Shell;
      Ok : Boolean;
   begin
      Initialize (App, Host'Unchecked_Access, Dark => False, Edit_Bytes => 512 * MIB);
      Frame (App, Target);
      if Line_Only then
         goto Long_Line;
      end if;
      Open_Timed (App, Target, Big, Natural'Image (Size_MiB) & " MiB code file, " & Label);
      Frames (App, Target, Natural'Image (Size_MiB) & " MiB file, " & Label);
      if Frames_Only then
         return;
      end if;
      if Width = 1920 then
         Timed_Find (App, Target, "Needle_Near_The_End", False, "a rare literal near the end");
         Timed_Find (App, Target, "Put_Line", False, "a literal on 1 line in 8");
         Timed_Find (App, Target, "Needle_[A-Z][a-z]+_The_End", True, "a regex near the end");
         --  Select all, delete, undo: one step each way, whatever the size.
         declare
            D : constant Document_Access := Active_Document (App);
            Before : constant Byte_Count := Length (D.all);
            Start : Unsigned_64 := Now_Us;
         begin
            Execute (App, Select_All, 0, "", Ok);
            Execute (App, Delete_Backward, 0, "", Ok);
            Report ("Select all + delete (" & Natural'Image (Size_MiB) & " MiB)",
                    Unsigned_64'Image (Now_Us - Start) & " us");
            Start := Now_Us;
            Execute (App, Undo, 0, "", Ok);
            Report ("Undo of that delete", Unsigned_64'Image (Now_Us - Start) & " us, length restored: "
                    & Boolean'Image (Length (D.all) = Before));
            --  Paste 16 MiB at 1,000 cursors? A 16 MiB paste at one cursor.
            declare
               Big_Paste : constant access String := new String'(1 .. 16 * MIB => 'p');
            begin
               Set_Clipboard (Host, Big_Paste.all);
            end;
            Execute (App, Go_To_Line, 1_000, "", Ok);
            Start := Now_Us;
            Execute (App, Paste, 0, "", Ok);
            Report ("Paste 16 MiB", Unsigned_64'Image ((Now_Us - Start) / 1_000) & " ms");
            Start := Now_Us;
            Execute (App, Undo, 0, "", Ok);
            Report ("Undo the paste", Unsigned_64'Image (Now_Us - Start) & " us");
            --  10,000 cursors typing.
            Execute (App, Go_To_Line, 2_000, "", Ok);
            for K in 1 .. 1 loop
               declare
                  S : Text_Cursors.Selection;
                  pragma Unreferenced (S);
               begin
                  null;
               end;
            end loop;
            for K in 2_001 .. 12_000 loop
               Add_Selection (D.all, Line_Start (D.all, Line_Number (K)), Line_Start (D.all, Line_Number (K)));
            end loop;
            Start := Now_Us;
            declare
               Elapsed : constant Unsigned_64 := Key_To_Frame (App, Target, Typed ("x"));
            begin
               Report ("Type at 10,000 cursors, key to frame", Unsigned_64'Image (Elapsed) & " us ("
                       & Natural'Image (Selection_Count (D.all)) & " cursors)");
            end;
            Execute (App, Undo, 0, "", Ok);
            Execute (App, Single_Selection, 0, "", Ok);
         end;
      end if;
      Execute (App, Close_Document, 0, "", Ok);
      Execute (App, Close_Document, 0, "", Ok);
      <<Long_Line>>
      Open_Timed (App, Target, Long, Natural'Image (Size_MiB) & " MiB single line, " & Label);
      declare
         Moves, Typing : Sample;
      begin
         for K in 1 .. 100 loop
            Add (Moves, Key_To_Frame (App, Target, Text_Shell.Key (End_Key, Control => True)));
            Add (Moves, Key_To_Frame (App, Target, Text_Shell.Key (Home, Control => True)));
         end loop;
         Add (Moves, Key_To_Frame (App, Target, Text_Shell.Key (End_Key, Control => True)));
         for K in 1 .. 200 loop
            Add (Typing, Key_To_Frame (App, Target, Typed ("y")));
         end loop;
         Report ("Single line: jump start <-> end, key to frame, " & Label, Show (Moves));
         Report ("Single line: type at its end, key to frame, " & Label, Show (Typing));
      end;
   end Run;
begin
   Put_Line ("text bench:" & Positive'Image (Size_MiB) & " MiB files (Linux-hosted, -O3 -gnatp, memory surface)");
   Ada.Directories.Create_Path (FILES);
   declare
      Big : constant String := FILES & "/bench-" & Positive'Image (Size_MiB) (2 .. Positive'Image (Size_MiB)'Last)
        & "mib.adb";
      Long : constant String := FILES & "/bench-line-" & Positive'Image (Size_MiB) (2 .. Positive'Image (Size_MiB)'Last)
        & "mib.txt";
   begin
      if not Ada.Directories.Exists (Big) then
         Write_File (Big, Size_MiB * MIB, Code_Line'Access);
      end if;
      if not Ada.Directories.Exists (Long) then
         Write_File (Long, Size_MiB * MIB, Long_Piece'Access);
      end if;
      Install_Themes (CuBit.Appearance.Alloy_Light);
      Grant (Host, FILES, Writable => True, Label => "scratch");
      Put_Line ("| Measurement | Result |");
      Put_Line ("| --- | --- |");
      if not Line_Only and then not Four_K_Frames then
         Run (1920, 1080, "1080p", Big, Long);
      end if;
      if not Frames_Only or else Four_K_Frames then
         Run (3840, 2160, "4K", Big, Long);
      end if;
   end;
end Text_Bench;
