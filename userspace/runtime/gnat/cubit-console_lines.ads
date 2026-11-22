------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Debug text as log lines (ABI-002, the WRITE retirement): a program's
--  debugPrint text, cut into lines that each make a log record, and the
--  mark a record carries when it is console text.
--
--  @description
--  Pure framing, no I/O (CuBit.Console_Route feeds it and publishes), and
--  no secondary stack (text comes out into caller buffers). A
--  line ends at a line feed, or at Maximum_Line_Bytes (the rest continues
--  on the next line), so nothing is dropped for length. Carriage returns
--  are left out; other control characters (but TAB) and DEL become '?';
--  a line that is not UTF-8 has its bytes above 127 shown as '?' (Plain):
--  every line makes a valid record (CuBit.Log_Records.Valid_Text).
--
--  Console text: a record with the Truth field Console_Field set. A
--  program's debug text is such a record (Debug severity), and so is a
--  record a program would also have echoed to its console (CuBit.Log's
--  echo, Desktop_Logs). Test builds' logstore mirrors exactly these onto
--  the serial console (Test_Console_ABI), so each line appears there once.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Log_Records;

package CuBit.Console_Lines with Pure, SPARK_Mode is
   package Logs renames CuBit.Log_Records;

   Maximum_Line_Bytes : constant := Logs.Maximum_Text_Bytes;
   subtype Line_Length is Natural range 0 .. Maximum_Line_Bytes;

   --  The field marking console text, and its value.
   Console_Field : constant String := "console";

   subtype Line_Buffer is String (1 .. Maximum_Line_Bytes);

   type Framer is private;

   function Length (Item : Framer) return Line_Length;
   --  The line so far (a completed one until Clear), copied out:
   --  Into (1 .. Length).
   procedure Line (Item : Framer; Into : out Line_Buffer; Length : out Line_Length)
   with Post => Length = Console_Lines.Length (Item);

   --  Take one character. Complete: a line ended (by a line feed, or by
   --  reaching Maximum_Line_Bytes); Line holds it until Clear.
   procedure Feed (Item : in out Framer; C : Character; Complete : out Boolean)
   with Pre  => Length (Item) < Maximum_Line_Bytes,
        Post => (if not Complete then Length (Item) < Maximum_Line_Bytes);

   procedure Clear (Item : in out Framer)
   with Post => Length (Item) = 0;

   --  Text made as it can stand in a record, in place: unchanged when it
   --  is UTF-8 without controls; otherwise each byte above 127, each
   --  control but TAB and DEL becomes '?'.
   procedure Plain (Text : in out String)
   with Post => Logs.Valid_Text (Text);

   --  Whether Value is console text (Console_Field set).
   function Is_Console (Value : Logs.Log_Record) return Boolean;

private
   type Framer is record
      Data : String (1 .. Maximum_Line_Bytes) := [others => ' '];
      Used : Line_Length := 0;
   end record;
   function Length (Item : Framer) return Line_Length is (Item.Used);
end CuBit.Console_Lines;
