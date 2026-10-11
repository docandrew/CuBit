------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Text and CCL forms of what an app copies (docs/clipboard.md)
--
--  A bounded builder for the two forms a copy offers: the plain text form
--  and the CCL literal that reads back as the same typed value (a String,
--  a list of file names, a LogEntry record). Quoting follows the CCL
--  reader's string escapes (\" \\ \n \r \t); a byte the reader cannot take
--  back makes the literal unusable (Fits = False), never a different value.
--  Single_Line prepares pasted text for a one-line field.
--
--  Proved (level 2, tests/clipboard): no overflow; a builder never writes
--  past its capacity; Single_Line keeps only printable bytes.
------------------------------------------------------------------------------
package Clipboard_Text with SPARK_Mode, Pure is

   --  The clipboard's own bound on one form (P.Maximum_Payload).
   Maximum_Form : constant := 65_536;
   subtype Form_Capacity is Positive range 1 .. Maximum_Form;
   subtype Form_Length is Natural range 0 .. Maximum_Form;

   function Is_Printable (Item : Character) return Boolean is
     (Item in ' ' .. '~');

   --  Keep the printable bytes of Text (Text'First .. Text'First + Kept - 1),
   --  each line break or tab becoming one space; drop other control bytes.
   procedure Single_Line (Text : in out String; Kept : out Natural)
   with Pre  => Text'Last < Positive'Last,
        Post => Kept <= Text'Length and then
          (for all I in Text'First .. Text'First + Kept - 1 =>
             Is_Printable (Text (I)));

   type Builder (Capacity : Form_Capacity) is record
      Data   : String (1 .. Capacity) := [others => ' '];
      Length : Form_Length := 0;
      --  False once something did not fit or could not be quoted: the
      --  form is then not offered.
      Fits   : Boolean := True;
   end record
   with Dynamic_Predicate => Builder.Length <= Builder.Capacity;

   procedure Clear (B : in out Builder)
   with Post => B.Length = 0 and then B.Fits;

   --  Append Text as it is.
   procedure Append (B : in out Builder; Text : String)
   with Post => B.Length <= B.Capacity and then
     (if not B'Old.Fits then not B.Fits);

   --  Append Text as a CCL string literal ("..." with escapes).
   procedure Append_Quoted (B : in out Builder; Text : String)
   with Post => B.Length <= B.Capacity and then
     (if not B'Old.Fits then not B.Fits);

   function Image (B : Builder) return String is
     (B.Data (1 .. B.Length));

   --  The CCL list forms: "(list " ... ")"; the empty list of strings
   --  is written with its element type, as the reader requires.
   List_Open    : constant String := "(list ";
   List_Close   : constant String := ")";
   Empty_String_List : constant String := "(list-of String)";
   String_List_Type  : constant String := "List-String";
   String_Type       : constant String := "String";
end Clipboard_Text;
