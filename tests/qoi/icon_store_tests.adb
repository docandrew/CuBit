--  Hosted tests of Desktop_Icon_Store (tests/qoi/README.md). The store is
--  loaded once per process, so each run checks one load:
--    icon_store_tests <atlas.qoi> <atlas.rgba> good <chunk bytes>
--    icon_store_tests <file> <atlas.rgba> truncated|corrupt|wrong-size|read-failed
--  "good": every icon equals the source PNGs (the .rgba oracle) pixel for
--  pixel. Every other load must leave every icon drawn as the fallback glyph.
with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.QOI;
with Desktop_Icons;
with Desktop_Icon_Store;

procedure Icon_Store_Tests is
   package Store renames Desktop_Icon_Store;
   package Q renames CuBit.QOI;
   use type Store.Status, Q.Failure;
   type Bytes_Access is access Q.Byte_Array;

   File  : constant String := Ada.Command_Line.Argument (1);
   Oracle : constant String := Ada.Command_Line.Argument (2);
   Mode  : constant String := Ada.Command_Line.Argument (3);
   Checks, Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Load (Path : String) return Bytes_Access is
      use Ada.Streams.Stream_IO;
      F : Ada.Streams.Stream_IO.File_Type;
      Size : constant Natural := Natural (Ada.Directories.Size (Path));
      Data : constant Bytes_Access := new Q.Byte_Array (1 .. Size);
      Stream_Data : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Size));
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Open (F, In_File, Path);
      Read (F, Stream_Data, Last);
      Close (F);
      for I in Data'Range loop
         Data (I) := Unsigned_8 (Stream_Data (Ada.Streams.Stream_Element_Offset (I)));
      end loop;
      return Data;
   end Load;

   --  The oracle's pixel of Icon at (X, Y) as 16#AARRGGBB#.
   function Expected (RGBA : Q.Byte_Array; Icon : Desktop_Icons.Icon_ID;
                      X, Y : Store.Coordinate) return Unsigned_32 is
      At_Byte : constant Positive := RGBA'First + 4 *
        ((Desktop_Icons.Icon_ID'Pos (Icon) * Store.Icon_Size + Y) * Store.Atlas_Width + X);
   begin
      return Shift_Left (Unsigned_32 (RGBA (At_Byte + 3)), 24) or
        Shift_Left (Unsigned_32 (RGBA (At_Byte)), 16) or
        Shift_Left (Unsigned_32 (RGBA (At_Byte + 1)), 8) or Unsigned_32 (RGBA (At_Byte + 2));
   end Expected;

   function All_Fallback return Boolean is
     (for all Icon in Desktop_Icons.Icon_ID =>
        (for all Y in Store.Coordinate =>
           (for all X in Store.Coordinate => Store.Pixel (Icon, X, Y) = Store.Fallback (X, Y))));

   procedure Feed_All (Data : Q.Byte_Array; Chunk : Natural) is
      At_Byte : Positive := Data'First;
   begin
      if Chunk = 0 then
         Store.Feed (Data);
         return;
      end if;
      while At_Byte <= Data'Last and then Store.Wants_More loop
         declare
            Last : constant Natural := Natural'Min (Data'Last, At_Byte + Chunk - 1);
         begin
            Store.Feed (Data (At_Byte .. Last));
            At_Byte := Last + 1;
         end;
      end loop;
   end Feed_All;

   Data : constant Bytes_Access := Load (File);
   RGBA : constant Bytes_Access := Load (Oracle);
   Result : Store.Load_Result;
begin
   --  The glyph is visible: transparent margin, frame, cross and fill.
   Check (Store.Fallback (0, 0) = Store.Glyph_Clear and then
          Store.Fallback (Store.Glyph_Margin, 10) = Store.Glyph_Frame and then
          Store.Fallback (5, 5) = Store.Glyph_Cross and then
          Store.Fallback (5, 18) = Store.Glyph_Cross and then
          Store.Fallback (10, 8) = Store.Glyph_Fill, "fallback glyph shape");
   Check (RGBA'Length = 4 * Store.Atlas_Pixels, "oracle is one atlas");
   Check (Store.Current = Store.Not_Loaded and then All_Fallback, "fallback before loading");
   Store.Begin_Load;
   if Mode = "good" then
      Feed_All (Data.all, Natural'Value (Ada.Command_Line.Argument (4)));
      Store.End_Load (False, Result);
      Check (Result.Loaded and then Store.Current = Store.Loaded, "atlas loads");
      Check ((for all Icon in Desktop_Icons.Icon_ID =>
                (for all Y in Store.Coordinate =>
                   (for all X in Store.Coordinate =>
                      Store.Pixel (Icon, X, Y) = Expected (RGBA.all, Icon, X, Y)))),
             "every icon equals its source PNG");
   elsif Mode = "truncated" then
      Feed_All (Data (Data'First .. Data'Last - 1), 4096);
      Store.End_Load (False, Result);
      Check (not Result.Loaded and then Result.Error = Q.Truncated, "truncated atlas rejected");
   elsif Mode = "corrupt" then
      declare
         Bad : Q.Byte_Array := Data.all;
      begin
         Bad (Bad'First) := Bad (Bad'First) xor 16#FF#;
         Feed_All (Bad, 7);
      end;
      Store.End_Load (False, Result);
      Check (not Result.Loaded and then Result.Error = Q.Bad_Magic, "corrupt atlas rejected");
   elsif Mode = "wrong-size" then
      Feed_All (Data.all, 64);
      Store.End_Load (False, Result);
      Check (not Result.Loaded and then
             (Result.Wrong_Size or else Result.Error = Q.Over_Limit), "wrong-size image rejected");
   elsif Mode = "read-failed" then
      Feed_All (Data.all, 4096);
      Store.End_Load (True, Result);
      Check (not Result.Loaded, "a failed read never loads");
   else
      Check (False, "unknown mode " & Mode);
   end if;
   if Mode /= "good" then
      Check (Store.Current = Store.Unavailable and then All_Fallback,
             Mode & ": every icon is the fallback glyph");
   end if;
   Put_Line ("icon store " & Mode & ":" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Icon_Store_Tests;
