------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Hosted tests of Tar_Archives (SELF-003 step 1): archives built here,
--  block by block, well-formed and adversarial. Each archive is made from
--  ustar headers this file writes itself (Put_Header), so every fixture's
--  provenance is the code below; nothing is a stored blob.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Tar_Archives; use Tar_Archives;

procedure Main is
   Failures : Natural := 0;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   type Block_List is array (Positive range <>) of Block;
   Archive : Block_List (1 .. 64);
   Count : Natural := 0;

   procedure Add (B : Block) is
   begin
      Count := Count + 1;
      Archive (Count) := B;
   end Add;

   procedure Put_Text (B : in out Block; At_Offset : Natural; Text : String) is
   begin
      for K in Text'Range loop
         B (At_Offset + K - Text'First) := Character'Pos (Text (K));
      end loop;
   end Put_Text;

   --  An octal field of Width bytes: Width - 1 digits and a NUL.
   procedure Put_Octal (B : in out Block; At_Offset, Width : Natural; Value : Unsigned_64) is
      V : Unsigned_64 := Value;
   begin
      for K in reverse 0 .. Width - 2 loop
         B (At_Offset + K) := Character'Pos ('0') + Unsigned_8 (V mod 8);
         V := V / 8;
      end loop;
      B (At_Offset + Width - 1) := 0;
   end Put_Octal;

   procedure Seal (B : in out Block) is
      Sum : Unsigned_64 := 0;
   begin
      for K in 148 .. 155 loop
         B (K) := Character'Pos (' ');
      end loop;
      for X of B loop
         Sum := Sum + Unsigned_64 (X);
      end loop;
      Put_Octal (B, 148, 7, Sum);
      B (155) := Character'Pos (' ');
   end Seal;

   type Magic is (Posix_Magic, Gnu_Magic, No_Magic);

   function Header
     (Name : String; Size : Byte_Count := 0; Flag : Character := '0'; Prefix : String := "";
      Form : Magic := Posix_Magic) return Block
   is
      B : Block := [others => 0];
   begin
      Put_Text (B, 0, Name);
      Put_Octal (B, 100, 8, 8#644#);
      Put_Octal (B, 108, 8, 0);
      Put_Octal (B, 116, 8, 0);
      Put_Octal (B, 124, 12, Unsigned_64 (Size));
      Put_Octal (B, 136, 12, 0);
      B (156) := Character'Pos (Flag);
      case Form is
         when Posix_Magic =>
            Put_Text (B, 257, "ustar");
            Put_Text (B, 263, "00");
         when Gnu_Magic =>
            Put_Text (B, 257, "ustar  ");
         when No_Magic =>
            null;
      end case;
      Put_Text (B, 345, Prefix);
      Seal (B);
      return B;
   end Header;

   function Data (Text : String) return Block is
      B : Block := [others => 0];
   begin
      Put_Text (B, 0, Text);
      return B;
   end Data;

   Zero : constant Block := [others => 0];

   procedure Add_Body (Text : String) is
      First : Positive := Text'First;
   begin
      while First <= Text'Last loop
         declare
            Last : constant Natural := Natural'Min (Text'Last, First + BLOCK_BYTES - 1);
         begin
            Add (Data (Text (First .. Last)));
            First := Last + 1;
         end;
      end loop;
   end Add_Body;

   procedure Add_File (Name : String; Text : String) is
   begin
      Add (Header (Name, Text'Length));
      Add_Body (Text);
   end Add_File;

   procedure Add_End is
   begin
      Add (Zero);
      Add (Zero);
   end Add_End;

   --  A pax record "LENGTH KEY=VALUE\n", its length counting itself.
   function Pax_Record (Key, Value : String) return String is
      Body_Length : constant Natural := 1 + Key'Length + 1 + Value'Length + 1;
      function Image (N : Natural) return String is
         S : constant String := Natural'Image (N);
      begin
         return S (S'First + 1 .. S'Last);
      end Image;
      Total : Natural := Body_Length + 1;
   begin
      while Image (Total)'Length + Body_Length /= Total loop
         Total := Total + 1;
      end loop;
      return Image (Total) & " " & Key & "=" & Value & ASCII.LF;
   end Pax_Record;

   procedure Add_Meta (Flag : Character; Text : String) is
   begin
      Add (Header ("././@PaxHeader", Text'Length, Flag));
      Add_Body (Text);
   end Add_Meta;

   type Seen is record
      Kind : Event_Kind := Nothing;
      Why : Refusal := No_Refusal;
      Path : String (1 .. 600) := [others => ' '];
      Path_Length : Natural := 0;
      Size : Byte_Count := 0;
   end record;
   type Seen_List is array (1 .. 64) of Seen;

   --  Walk Archive (1 .. Count): the entry events, the final one last, and
   --  the data bytes received (checked against each file's size).
   procedure Walk
     (Events : out Seen_List; Last : out Natural; Bytes : out String; Bytes_Last : out Natural;
      Ceiling : Limits := (Maximum_Entries => 100, Maximum_Bytes => 1_000_000);
      Specials : Specials_Policy := Refuse_Specials; Stop_At : Natural := 0)
   is
      W : Walker;
      E : Event;
      Expected, Got : Byte_Count := 0;
      Blocks : constant Natural := (if Stop_At = 0 then Count else Stop_At);
   begin
      Events := [others => <>];
      Last := 0;
      Bytes := [others => ' '];
      Bytes_Last := 0;
      Start (W, Ceiling, Specials);
      for I in 1 .. Blocks loop
         Feed (W, Archive (I), E);
         case E.Kind is
            when Directory_Entry | File_Start | Skipped_Special =>
               Last := Last + 1;
               Events (Last).Kind := E.Kind;
               Events (Last).Path_Length := Natural'Min (Path (W)'Length, 600);
               Events (Last).Path (1 .. Events (Last).Path_Length) :=
                 Path (W) (Path (W)'First .. Path (W)'First + Events (Last).Path_Length - 1);
               Events (Last).Size := E.Size;
               if E.Kind = File_Start then
                  Check (Got = Expected, "the previous file got all its bytes");
                  Expected := E.Size;
                  Got := 0;
               end if;
            when File_Data =>
               Got := Got + Byte_Count (E.Data_Length);
               Check (E.Last_Piece = (Got = Expected), "the last piece is the last");
               for K in 0 .. E.Data_Length - 1 loop
                  Bytes_Last := Bytes_Last + 1;
                  Bytes (Bytes_Last) := Character'Val (Archive (I) (K));
               end loop;
            when Finished | Refused =>
               Last := Last + 1;
               Events (Last).Kind := E.Kind;
               Events (Last).Why := E.Why;
               return;
            when Nothing =>
               null;
         end case;
      end loop;
      Finish (W, E);
      Last := Last + 1;
      Events (Last).Kind := E.Kind;
      Events (Last).Why := E.Why;
   end Walk;

   Events : Seen_List;
   Last : Natural;
   Bytes : String (1 .. 4096);
   Bytes_Last : Natural;

   function Path_Of (N : Positive) return String is (Events (N).Path (1 .. Events (N).Path_Length));

   procedure Expect_Refusal (Why : Refusal; What : String; Specials : Specials_Policy := Refuse_Specials;
                             Ceiling : Limits := (Maximum_Entries => 100, Maximum_Bytes => 1_000_000)) is
   begin
      Walk (Events, Last, Bytes, Bytes_Last, Ceiling => Ceiling, Specials => Specials);
      Check (Last >= 1 and then Events (Last).Kind = Refused and then Events (Last).Why = Why,
             What & " is refused as " & Why'Image &
             (if Last >= 1 then " (got " & Events (Last).Kind'Image & " " & Events (Last).Why'Image & ")"
              else ""));
   end Expect_Refusal;

   procedure Reset is
   begin
      Count := 0;
   end Reset;

   Long_Path : constant String := "deep/" & [1 .. 290 => 'x'] & "/file.txt";
begin
   --  1. A small tree: a directory, two files (one empty), the end.
   Reset;
   Add (Header ("pkg/", Flag => '5'));
   Add_File ("pkg/a.txt", "hello");
   Add_File ("pkg/empty", "");
   Add_File ("pkg/two-blocks", [1 .. 700 => 'z']);
   Add_End;
   Walk (Events, Last, Bytes, Bytes_Last);
   Check (Last = 5, "four entries and the end");
   Check (Events (1).Kind = Directory_Entry and then Path_Of (1) = "pkg", "the directory, its '/' dropped");
   Check (Events (2).Kind = File_Start and then Path_Of (2) = "pkg/a.txt" and then Events (2).Size = 5, "a.txt");
   Check (Events (3).Kind = File_Start and then Events (3).Size = 0, "the empty file");
   Check (Events (4).Size = 700, "a file over two blocks");
   Check (Events (5).Kind = Finished, "the two zero blocks end it");
   Check (Bytes_Last = 705 and then Bytes (1 .. 5) = "hello", "the data, exactly the declared sizes");

   --  2. "./" prefixes, as `tar -C dir .` writes them; "./" itself is the target.
   Reset;
   Add (Header ("./", Flag => '5'));
   Add_File ("./a", "x");
   Add_File ("././b/c", "y");
   Add_End;
   Walk (Events, Last, Bytes, Bytes_Last);
   Check (Last = 3 and then Path_Of (1) = "a" and then Path_Of (2) = "b/c" and then Events (3).Kind = Finished,
          "leading ./ dropped; ./ itself is no entry");

   --  3. Paths that would leave the target, each refused.
   declare
      procedure Unsafe (Bad : String) is
      begin
         Reset;
         Add_File (Bad, "x");
         Add_End;
         Expect_Refusal (Unsafe_Path, "the path " & Bad);
      end Unsafe;
   begin
      Unsafe ("../evil");
      Unsafe ("a/../../b");
      Unsafe ("/etc/passwd");
      Unsafe ("a//b");
      Unsafe ("a/./b");
      Unsafe ("a/.");
      Unsafe ("a/..");
      Unsafe ("..");
      Unsafe ("a\b");
      Unsafe ("a" & ASCII.SOH & "b");
      Unsafe ("a" & Character'Val (16#C3#) & "b");
      Unsafe ("a" & ASCII.DEL);
   end;
   Reset;
   Add_File ("dir/", "x");
   Add_End;
   Expect_Refusal (Unsafe_Path, "a file whose name ends in '/'");
   Reset;
   Add (Header ("a/../b", Flag => '5'));
   Add_End;
   Expect_Refusal (Unsafe_Path, "a directory a/../b");

   --  4. Links, devices, FIFOs: refused, or skipped (never created).
   for Flag of String'("12346") loop
      Reset;
      Add (Header ("link", Flag => Flag));
      Add_File ("after", "ok");
      Add_End;
      Expect_Refusal (Special_Entry, "a special entry " & Flag);
      Walk (Events, Last, Bytes, Bytes_Last, Specials => Skip_Specials);
      Check (Last = 3 and then Events (1).Kind = Skipped_Special and then Path_Of (1) = "link"
             and then Events (2).Kind = File_Start and then Events (3).Kind = Finished,
             "a special entry " & Flag & " skipped, the rest read");
   end loop;
   Reset;
   Add (Header ("../escape", Flag => '2'));
   Add_End;
   Expect_Refusal (Unsafe_Path, "a skipped link with an unsafe name", Specials => Skip_Specials);

   --  5. Damaged headers.
   Reset;
   Add (Header ("a", 1));
   Archive (1) (0) := Character'Pos ('b');
   Add (Data ("x"));
   Add_End;
   Expect_Refusal (Bad_Checksum, "a header changed after its checksum");
   Reset;
   Add (Header ("v7", 0, Form => No_Magic));
   Add_End;
   Expect_Refusal (Not_Ustar, "a header without the ustar magic");
   Reset;
   declare
      B : Block := Header ("n", 0);
   begin
      Put_Text (B, 124, "0000000009");
      Seal (B);
      Add (B);
   end;
   Add_End;
   Expect_Refusal (Bad_Number, "a size with a non-octal digit");
   Reset;
   declare
      B : Block := Header ("n", 0);
   begin
      B (124 .. 135) := [16#80#, others => 16#FF#];
      Seal (B);
      Add (B);
   end;
   Add_End;
   Expect_Refusal (Base_256_Number, "a base-256 size");
   Reset;
   declare
      B : Block := Header ("n", 0);
   begin
      B (124 .. 135) := [others => Character'Pos (' ')];
      Seal (B);
      Add (B);
   end;
   Add_End;
   Expect_Refusal (Bad_Number, "a size of spaces only");
   Reset;
   Add (Header ("", 0));
   Add_End;
   Expect_Refusal (Empty_Name, "a header with no name");
   Reset;
   Add (Header ("sparse", 0, Flag => 'S'));
   Add_End;
   Expect_Refusal (Unsupported_Entry, "a GNU sparse entry");
   Reset;
   Add (Header ("d", 10, Flag => '5'));
   Add (Data ("0123456789"));
   Add_End;
   Expect_Refusal (Directory_Data, "a directory with data");

   --  6. Sizes and ceilings.
   Reset;
   Add (Header ("huge", 8#77777777777#));
   Add_End;
   Expect_Refusal (Too_Many_Bytes, "a file larger than the byte ceiling");
   Reset;
   Add (Header ("huge", 8#77777777777#));
   Add_End;
   Walk (Events, Last, Bytes, Bytes_Last,
         Ceiling => (Maximum_Entries => 10, Maximum_Bytes => Byte_Count'Last));
   Check (Events (Last).Kind = Refused and then Events (Last).Why = Truncated,
          "an 8 GiB file whose data is missing is truncated");
   Reset;
   Add_File ("a", "1");
   Add_File ("b", "2");
   Add_File ("c", "3");
   Add_End;
   Expect_Refusal (Too_Many_Entries, "a third entry past a ceiling of two",
                   Ceiling => (Maximum_Entries => 2, Maximum_Bytes => 100));
   Expect_Refusal (Too_Many_Bytes, "a third byte past a ceiling of two",
                   Ceiling => (Maximum_Entries => 10, Maximum_Bytes => 2));

   --  7. Truncation and the end marker.
   Reset;
   Add (Header ("cut", 600));
   Add (Data ("x"));
   Expect_Refusal (Truncated, "an archive cut inside a file's data");
   Reset;
   Add_File ("no-end", "x");
   Expect_Refusal (Truncated, "an archive without its zero blocks");
   Reset;
   Add_File ("one-zero", "x");
   Add (Zero);
   Expect_Refusal (Truncated, "an archive with one zero block");
   Reset;
   Add_File ("a", "x");
   Add (Zero);
   Add_File ("b", "y");
   Add_End;
   Expect_Refusal (Lone_Zero_Block, "a lone zero block between entries");
   Reset;
   Add_File ("a", "x");
   Add_End;
   Add (Data ("trailing garbage after the end"));
   Walk (Events, Last, Bytes, Bytes_Last);
   Check (Events (Last).Kind = Finished, "blocks after the end are padding");

   --  8. A refusal is final: what follows is refused alike.
   declare
      W : Walker;
      E : Event;
   begin
      Start (W, (Maximum_Entries => 10, Maximum_Bytes => 100), Refuse_Specials);
      Feed (W, Header ("../x", 0), E);
      Check (E.Kind = Refused and then E.Why = Unsafe_Path, "the first refusal");
      Feed (W, Header ("fine", 0), E);
      Check (E.Kind = Refused and then E.Why = Unsafe_Path, "a later good header is refused too");
      Finish (W, E);
      Check (E.Kind = Refused and then E.Why = Unsafe_Path, "and so is the end");
   end;

   --  9. POSIX prefix + name; GNU magic ignores the prefix field.
   Reset;
   Add (Header ("name.txt", 1, Prefix => "long/prefix"));
   Add (Data ("p"));
   Add (Header ("gnu.txt", 1, Prefix => "IGNORED", Form => Gnu_Magic));
   Add (Data ("g"));
   Add_End;
   Walk (Events, Last, Bytes, Bytes_Last);
   Check (Path_Of (1) = "long/prefix/name.txt", "the POSIX prefix joins the name");
   Check (Path_Of (2) = "gnu.txt", "a GNU header's prefix bytes are not a prefix");
   Reset;
   Add (Header ("x", 0, Prefix => "../up"));
   Add_End;
   Expect_Refusal (Unsafe_Path, "a prefix of ../up");

   --  10. pax extended headers.
   Reset;
   Add_Meta ('x', Pax_Record ("mtime", "1700000000.5") & Pax_Record ("path", Long_Path));
   Add_File ("truncated-ustar-name", "L");
   Add_File ("plain", "P");
   Add_End;
   Walk (Events, Last, Bytes, Bytes_Last);
   Check (Last = 3 and then Path_Of (1) = Long_Path and then Path_Of (2) = "plain",
          "a pax path names only the next entry");
   Reset;
   Add_Meta ('g', Pax_Record ("comment", "global attributes, as git archive writes"));
   Add_File ("after-global", "G");
   Add_End;
   Walk (Events, Last, Bytes, Bytes_Last);
   Check (Last = 2 and then Path_Of (1) = "after-global", "a global header changes nothing");
   Reset;
   Add_Meta ('x', Pax_Record ("path", "../../etc/shadow"));
   Add_File ("innocent", "x");
   Add_End;
   Expect_Refusal (Unsafe_Path, "a pax path of ../../etc/shadow");
   Reset;
   Add_Meta ('x', Pax_Record ("size", "99"));
   Add_File ("sized", "x");
   Add_End;
   Expect_Refusal (Unsupported_Pax, "a pax size override");
   Reset;
   Add_Meta ('x', Pax_Record ("GNU.sparse.map", "0,1"));
   Add_File ("sparse", "x");
   Add_End;
   Expect_Refusal (Unsupported_Pax, "GNU sparse pax records");
   declare
      procedure Bad_Record (Bad : String) is
      begin
         Reset;
         Add_Meta ('x', Bad);
         Add_File ("f", "x");
         Add_End;
         Expect_Refusal (Bad_Pax_Record, "the pax record """ & Bad & """");
      end Bad_Record;
   begin
      Bad_Record ("12 path=ab" & ASCII.LF & "zz");
      Bad_Record ("5 a=b" & ASCII.LF);
      Bad_Record ("99 path=a" & ASCII.LF);
      Bad_Record ("x path=a" & ASCII.LF);
      Bad_Record ("7 path" & ASCII.LF);
      Bad_Record ("8 path=ab");
      Bad_Record ("7 =abc" & ASCII.LF);
      Bad_Record ("99999999999999999999 path=a" & ASCII.LF);
   end;
   Reset;
   Add (Header ("././@PaxHeader", MAXIMUM_META_BYTES + 1, Flag => 'x'));
   Add_End;
   Expect_Refusal (Meta_Too_Large, "a pax header beyond the ceiling");

   --  11. GNU long names.
   Reset;
   Add_Meta ('L', Long_Path & ASCII.NUL);
   Add_File ("short", "S");
   Add_End;
   Walk (Events, Last, Bytes, Bytes_Last);
   Check (Last = 2 and then Path_Of (1) = Long_Path, "a GNU long name names the next entry");
   Reset;
   Add_Meta ('L', "../outside" & ASCII.NUL);
   Add_File ("short", "S");
   Add_End;
   Expect_Refusal (Unsafe_Path, "a GNU long name of ../outside");
   Reset;
   Add (Header ("././@LongLink", 0, Flag => 'L'));
   Add_End;
   Expect_Refusal (Empty_Name, "an empty GNU long name");

   --  12. The header helpers on their own.
   declare
      B : Block := Header ("c", 0);
      V : Field_Value;
      Why : Refusal;
   begin
      Check (Checksum_Matches (B), "a sealed header's checksum matches");
      B (200) := 1;
      Check (not Checksum_Matches (B), "one changed byte breaks it");
      Put_Text (B, 124, "  1234567 " & ASCII.NUL & " ");
      Read_Octal (B, 124, 12, V, Why);
      Check (Why = No_Refusal and then V = 8#1234567#, "leading spaces, digits, NUL and space");
      Put_Text (B, 124, "777777777777");
      Read_Octal (B, 124, 12, V, Why);
      Check (Why = No_Refusal and then V = MAXIMUM_FIELD_VALUE, "twelve digits, no terminator");
      Put_Text (B, 124, "12 34       ");
      Read_Octal (B, 124, 12, V, Why);
      Check (Why = Bad_Number, "digits after the terminator");
   end;

   if Failures = 0 then
      Put_Line ("tar-reader: PASS (" & Natural'Image (Checks) & " checks)");
   else
      Put_Line ("tar-reader: FAIL," & Natural'Image (Failures) & " of" & Natural'Image (Checks));
      raise Program_Error;
   end if;
end Main;
