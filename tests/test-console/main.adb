------------------------------------------------------------------------------
--  Hosted checks of the WRITE retirement's pieces (ABI-002):
--  the test console object's request decision (Test_Console_ABI.Decide:
--  every right, label 0 .. 3, word count 0 .. 4 and byte count around its
--  bounds) and the debug-text framing (CuBit.Console_Lines: line ends,
--  splits, controls, non-UTF-8 text, the console mark).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Test_Console_ABI; use Test_Console_ABI;
with CuBit.Console_Lines;
with CuBit.Log_Records;
with CuBit.Log_Records.Texts;

procedure Main is
   package Lines renames CuBit.Console_Lines;
   package Logs renames CuBit.Log_Records;
   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & What);
      end if;
   end Check;
   function Refused (D : Decision; Why : Outcome) return Boolean is
     (not D.Valid and then D.Why = Why);

   --  The framer's line so far.
   function Line_Of (F : Lines.Framer) return String is
      Buffer : Lines.Line_Buffer;
      Used : Lines.Line_Length;
   begin
      Lines.Line (F, Buffer, Used);
      return Buffer (1 .. Used);
   end Line_Of;

   --  Text as Plain makes it.
   function Plain (Text : String) return String is
      Copy : String (1 .. Text'Length) := Text;
   begin
      Lines.Plain (Copy);
      return Copy;
   end Plain;

   --  Every line Text frames into, joined by '|'.
   function Framed (Text : String) return String is
      F : Lines.Framer;
      Complete : Boolean;
      Result : String (1 .. 4 * Text'Length + 8) := [others => ' '];
      Used : Natural := 0;
      procedure Add (S : String) is
      begin
         Result (Used + 1 .. Used + S'Length) := S;
         Used := Used + S'Length;
      end Add;
   begin
      for C of Text loop
         Lines.Feed (F, C, Complete);
         if Complete then
            Add (Line_Of (F) & "|");
            Lines.Clear (F);
         end if;
      end loop;
      Add ("<" & Line_Of (F) & ">");
      return Result (1 .. Used);
   end Framed;

   Long : constant String (1 .. Lines.Maximum_Line_Bytes + 3) := [others => 'x'];
   type Counts is array (Positive range <>) of Unsigned_64;
   Byte_Counts : constant Counts :=
     [0, 1, 2, Maximum_Bytes - 1, Maximum_Bytes, Maximum_Bytes + 1, Unsigned_64'Last];
begin
   --  The decision.
   for Writable in Boolean loop
      for Label in Unsigned_64 range 0 .. 3 loop
         for Length in Unsigned_64 range 0 .. 4 loop
            for Bytes of Byte_Counts loop
               declare
                  D : constant Decision := Decide (Writable, Label, Length, Bytes);
               begin
                  if not Writable then
                     Check (Refused (D, Not_Permitted), "no Write refused first");
                  elsif Label /= 1 then
                     Check (Refused (D, Unknown_Operation), "unknown label" & Label'Image);
                  elsif Length /= Request_Words then
                     Check (Refused (D, Malformed), "words" & Length'Image);
                  elsif Bytes = 0 or else Bytes > Maximum_Bytes then
                     Check (Refused (D, Too_Long), "bytes" & Bytes'Image);
                  else
                     Check (D.Valid and then D.Count = Bytes, "accepted" & Bytes'Image);
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   --  The wire codes are fixed (the runtime and the kernel share them).
   Check (Operation'Enum_Rep (Write) = 1 and then Request_Words = 2
          and then Address_Word = 0 and then Count_Word = 1, "request layout");
   Check (Outcome_Word_Of (Done) = 1 and then Outcome_Word_Of (Unreadable) = 6
          and then not Is_Outcome_Word (0) and then not Is_Outcome_Word (7), "outcome codes");

   --  The framing.
   Check (Framed ("hello" & ASCII.LF) = "hello|<>", "one line");
   Check (Framed ("a" & ASCII.CR & ASCII.LF & "b") = "a|<b>", "CRLF ends a line; the rest waits");
   Check (Framed (ASCII.LF & "" & ASCII.LF) = "||<>", "empty lines are lines (the route skips them)");
   Check (Framed ("t" & ASCII.HT & "x" & ASCII.ESC & ASCII.DEL & ASCII.LF) = "t" & ASCII.HT & "x??|<>",
          "TAB kept, other controls and DEL shown as ?");
   Check (Framed (Long) = Long (1 .. Lines.Maximum_Line_Bytes) & "|<xxx>", "a long line continues");
   Check (Plain ("caf" & Character'Val (16#C3#) & Character'Val (16#A9#)) =
            "caf" & Character'Val (16#C3#) & Character'Val (16#A9#), "UTF-8 kept");
   Check (Plain ("bad" & Character'Val (16#FF#) & "x") = "bad?x", "not UTF-8: high bytes as ?");
   Check (Logs.Valid_Text (Plain ("z" & Character'Val (16#C3#))), "a cut scalar becomes valid text");

   --  The console mark.
   Check (Logs.Valid_Field_Name (Lines.Console_Field), "the mark is a field name");
   declare
      Made : constant Logs.Decoded := Logs.Make ("line", Logs.Debug);
      Marked, Other, False_Mark : Logs.Decoded;
   begin
      Check (Made.Success and then not Lines.Is_Console (Made.Value), "unmarked record");
      if Made.Success then
         Marked := Logs.With_Field (Made.Value, Lines.Console_Field, Logs.Truth, 1);
         Other := Logs.With_Field (Made.Value, "consolex", Logs.Truth, 1);
         False_Mark := Logs.With_Field (Made.Value, Lines.Console_Field, Logs.Truth, 0);
         Check (Marked.Success and then Lines.Is_Console (Marked.Value), "marked record");
         Check (Other.Success and then not Lines.Is_Console (Other.Value), "another field is no mark");
         Check (False_Mark.Success and then not Lines.Is_Console (False_Mark.Value), "a false mark is none");
         --  The mark survives the wire (logstore decodes what it reads).
         if Marked.Success then
            declare
               Bytes : Logs.Wire_Buffer;
               Used : Logs.Wire_Count;
               Back : Logs.Decoded;
            begin
               Logs.Encode (Marked.Value, Bytes, Used);
               Back := Logs.Decode (Bytes, Used);
               Check (Back.Success and then Lines.Is_Console (Back.Value)
                      and then Logs.Texts.Text (Back.Value) = "line", "mark round trip");
            end;
         end if;
      end if;
   end;

   Ada.Text_IO.Put_Line ("test-console: " & (if Failures = 0 then "PASS" else "FAIL") &
                         Checks'Image & " checks");
end Main;
