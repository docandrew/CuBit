--  UTF-8 for the editor (docs/text-editor.md, "Layout"): decoding, reduced
--  grapheme clusters and display widths. Bytes that are not valid UTF-8 are
--  never changed; each such byte is a cluster of its own, shown boxed.
--
--  A cluster is a base code point and what attaches to it: combining marks,
--  variation selectors, emoji modifiers, a zero-width joiner and the code
--  point after it, the second of a regional-indicator pair, and LF after CR.
--  This is a reduced, tested subset of UAX #29. Clusters are found forward
--  from a known boundary (a line or segment start) and are at most
--  MAXIMUM_CLUSTER_BYTES long, so every step is bounded. Proved: no runtime
--  errors, every step advances within the text.
package Text_UTF8 with SPARK_Mode, Pure is
   MAXIMUM_CODE_POINT : constant := 16#10FFFF#;
   type Code_Point is range 0 .. MAXIMUM_CODE_POINT;
   REPLACEMENT : constant Code_Point := 16#FFFD#;
   ZERO_WIDTH_JOINER : constant Code_Point := 16#200D#;

   MAXIMUM_SEQUENCE : constant := 4;
   subtype Sequence_Length is Positive range 1 .. MAXIMUM_SEQUENCE;
   MAXIMUM_CLUSTER_BYTES : constant := 32;
   subtype Cluster_Length is Positive range 1 .. MAXIMUM_CLUSTER_BYTES;

   --  Display columns a cluster takes (a tab is the layout's business).
   subtype Cell_Width is Natural range 0 .. 2;

   --  The code point at Text (Index): Valid False for a byte that does not
   --  start a well-formed sequence (overlong, surrogate, beyond U+10FFFF,
   --  truncated); then Size is 1 and Code is the byte's value.
   procedure Decode
     (Text : String; Index : Positive; Code : out Code_Point; Size : out Sequence_Length; Valid : out Boolean)
     with Pre => Index in Text'Range,
          Post => Size <= Text'Last - Index + 1;

   function Is_Continuation (Byte : Character) return Boolean is
     (Character'Pos (Byte) in 16#80# .. 16#BF#);

   --  Attaches to the code point before it.
   function Extends (Code : Code_Point) return Boolean;
   function Is_Regional_Indicator (Code : Code_Point) return Boolean is
     (Code in 16#1F1E6# .. 16#1F1FF#);
   --  East Asian wide and fullwidth forms, and emoji presentation: 2.
   function Width (Code : Code_Point) return Cell_Width;

   --  The cluster starting at Text (Index): its length in bytes.
   function Cluster_Size (Text : String; Index : Positive) return Cluster_Length
     with Pre => Index in Text'Range,
          Post => Cluster_Size'Result <= Text'Last - Index + 1;

   --  The cluster's first code point (the one drawn) and its width; an
   --  invalid byte is drawn boxed, two cells wide.
   procedure Cluster_Face
     (Text : String; Index : Positive; Code : out Code_Point; Columns : out Cell_Width; Valid : out Boolean)
     with Pre => Index in Text'Range;

   --  Word characters for word movement and whole-word search: letters,
   --  digits, '_' and any non-ASCII code point.
   function Is_Word (Code : Code_Point) return Boolean is
     (Code in Character'Pos ('a') .. Character'Pos ('z') | Character'Pos ('A') .. Character'Pos ('Z')
             | Character'Pos ('0') .. Character'Pos ('9') | Character'Pos ('_')
      or else Code >= 16#80#);
end Text_UTF8;
