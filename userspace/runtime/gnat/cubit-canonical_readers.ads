--  Reading a typed setting's canonical source without the CCL analyser
--  (docs/ccl-boot-configuration.md, "Typed settings").
--
--  A typed setting is stored as its value's canonical source: the one
--  spelling CCL.Typed_Settings renders for it (every record field named, in
--  declaration order; sums as Type.Member or (Type.Member payload); strings
--  quoted with only \", \\, \n and \r escaped; integers in decimal with no sign but
--  a leading '-' and no leading zero). A reader that knows the type at build
--  time therefore needs only these few token readers: ccl-manifest
--  generates one decoder per catalog type (CCL.Setting_Codecs.*) as a fixed
--  sequence of calls to them, so reading a setting costs a setting-sized
--  record and no CCL type check, evaluation or object image.
--
--  Every reader takes the source, the next unread position and a running
--  verdict OK. Once OK is False a reader changes nothing but its out
--  parameter (which gets a fixed value), so a decoder can call them in
--  sequence and look at OK once. Position never moves past Source'Last + 1.
pragma Ada_2022;
with Interfaces;
with CuBit.Launch_Arguments;

package CuBit.Canonical_Readers with SPARK_Mode, Pure is
   use type Interfaces.Integer_64;

   --  The longest canonical source a typed value may have: the launch
   --  block's budget for a program's parameter value
   --  (CuBit.Launch_Arguments.Parameter_Value_Budget), which typed
   --  settings share.
   Maximum_Canonical : constant := CuBit.Launch_Arguments.Parameter_Value_Budget;
   subtype Canonical_Length is Natural range 0 .. Maximum_Canonical;

   --  A string field's value, unescaped: never longer than its source.
   type Text is record
      Length : Canonical_Length := 0;
      Data : String (1 .. Maximum_Canonical) := [others => ' '];
   end record;
   function Image (Item : Text) return String is (Item.Data (1 .. Item.Length));
   Empty_Text : constant Text := (Length => 0, Data => [others => ' ']);

   --  A source a reader can walk: its last position leaves room for the
   --  position just past it.
   function Readable (Source : String) return Boolean is
     (Source'Last < Positive'Last);
   function In_Source (Source : String; Position : Positive) return Boolean is
     (Readable (Source) and then Position in Source'First .. Source'Last + 1);

   --  Whether the source continues with Literal at Position.
   function Next_Is (Source : String; Position : Positive; Literal : String) return Boolean is
     (Literal'Length <= Source'Last - Position + 1
      and then Source (Position .. Position + Literal'Length - 1) = Literal)
   with Pre => In_Source (Source, Position) and then Literal'Length > 0;

   --  Whether the source continues with Literal followed by a separator
   --  (' ', ')' or the end): a whole member name, not the start of another.
   function Next_Is_Word (Source : String; Position : Positive; Literal : String) return Boolean is
     (Next_Is (Source, Position, Literal)
      and then (Position + Literal'Length > Source'Last
                or else Source (Position + Literal'Length) in ' ' | ')'))
   with Pre => In_Source (Source, Position) and then Literal'Length > 0;

   --  Consume exactly Literal.
   procedure Expect (Source : String; Position : in out Positive; OK : in out Boolean; Literal : String)
   with Always_Terminates,
     Pre => In_Source (Source, Position) and then Literal'Length > 0,
     Post => In_Source (Source, Position) and then Position >= Position'Old
       and then (if not OK'Old then not OK and then Position = Position'Old)
       and then (if OK then Position = Position'Old + Literal'Length);

   --  Consume exactly Literal as a whole word (Next_Is_Word).
   procedure Expect_Word (Source : String; Position : in out Positive; OK : in out Boolean; Literal : String)
   with Always_Terminates,
     Pre => In_Source (Source, Position) and then Literal'Length > 0,
     Post => In_Source (Source, Position) and then Position >= Position'Old
       and then (if not OK'Old then not OK and then Position = Position'Old)
       and then (if OK then Position = Position'Old + Literal'Length);

   --  A decimal integer in Low .. High, spelled as Integer_64'Image without
   --  its leading space: "0", or an optional '-' and digits not starting
   --  with '0'. Value is Low unless OK.
   procedure Read_Integer
     (Source : String; Position : in out Positive; OK : in out Boolean;
      Low, High : Interfaces.Integer_64; Value : out Interfaces.Integer_64)
   with Always_Terminates,
     Pre => In_Source (Source, Position) and then Low <= High,
     Post => In_Source (Source, Position) and then Position >= Position'Old
       and then Value in Low .. High
       and then (if not OK'Old then not OK and then Position = Position'Old)
       and then (if not OK then Value = Low);

   --  true or false. Value is False unless OK.
   procedure Read_Boolean (Source : String; Position : in out Positive; OK : in out Boolean; Value : out Boolean)
   with Always_Terminates,
     Pre => In_Source (Source, Position),
     Post => In_Source (Source, Position) and then Position >= Position'Old
       and then (if not OK'Old then not OK and then Position = Position'Old)
       and then (if not OK then not Value);

   --  A quoted string: '"', then characters, then '"'. A line feed or
   --  carriage return is never raw: it is written \n or \r. '\' is followed
   --  by '"' or '\' (the character itself), 'n' or 'r'; nothing else.
   --  Item is Empty_Text unless OK.
   procedure Read_Text (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Text)
   with Always_Terminates,
     Pre => In_Source (Source, Position),
     Post => In_Source (Source, Position) and then Position >= Position'Old
       and then (if not OK'Old then not OK and then Position = Position'Old)
       and then (if not OK then Item = Empty_Text);

   --  The whole source was read.
   procedure Finish (Source : String; Position : Positive; OK : in out Boolean)
   with Always_Terminates,
     Pre => In_Source (Source, Position),
     Post => OK = (OK'Old and then Position = Source'Last + 1);
end CuBit.Canonical_Readers;
