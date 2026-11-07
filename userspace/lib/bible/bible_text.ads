------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The images' UTF-8 text as the reading face's characters (APP-005).
--  CuBit.Fonts.Serif draws printable ASCII and, at 16#80# .. 16#FF#, the
--  Windows-1252 set: curly quotes, dashes, the ellipsis, the pilcrow the
--  King James Version marks paragraphs with, and the Latin-1 letters. To_Face
--  maps each UTF-8 character to that byte; the two the face lacks (e and o
--  with a macron, in a few Berean Standard Bible names) fold to the plain
--  letter, and anything else becomes "?" (tools/build_bible.py refuses
--  verse text with anything else, so a "?" means a damaged image).
--
--  Proved (SPARK, level 2): no run-time errors on any input, and the output
--  is never longer than the input.
------------------------------------------------------------------------------
package Bible_Text with SPARK_Mode, Pure is

   --  Windows-1252 positions of the typographic characters.
   ELLIPSIS     : constant Character := Character'Val (16#85#);
   LEFT_SINGLE  : constant Character := Character'Val (16#91#);
   RIGHT_SINGLE : constant Character := Character'Val (16#92#);
   LEFT_DOUBLE  : constant Character := Character'Val (16#93#);
   RIGHT_DOUBLE : constant Character := Character'Val (16#94#);
   EN_DASH      : constant Character := Character'Val (16#96#);
   EM_DASH      : constant Character := Character'Val (16#97#);
   NO_BREAK     : constant Character := Character'Val (16#A0#);
   PILCROW      : constant Character := Character'Val (16#B6#);
   UNKNOWN      : constant Character := '?';

   procedure To_Face (Source : String; Target : out String; Used : out Natural)
     with Pre  => Target'Length >= Source'Length,
          Post => Used <= Source'Length;

   --  To_Face of a whole string.
   function Face_Text (Source : String) return String
     with Post => Face_Text'Result'Length <= Source'Length;

   --  The text in printable ASCII (CCL's strings are ASCII today): curly
   --  quotes become straight ones, the dashes "-" and "--", the ellipsis
   --  "...", the Latin letters their plain forms ("ae" for the ligature),
   --  the pilcrow is left out; anything else is "?". At most three bytes
   --  per byte of Source.
   function To_ASCII (Source : String) return String
     with Pre  => Source'Length <= Natural'Last / 3,
          Post => To_ASCII'Result'Length <= 3 * Source'Length;

   --  A word-breaking space in face text (the no-break space is not one).
   function Is_Break (C : Character) return Boolean is (C = ' ');

end Bible_Text;
