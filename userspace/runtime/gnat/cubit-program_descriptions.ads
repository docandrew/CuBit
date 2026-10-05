------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A program's description: its typed parameters, how they render into its
--  argv, and its connectors (docs/ccl-launch-parameters.md, "Legacy tools: typed
--  parameters over argv" and "Connectors, not stdio"). The manifest tool writes
--  it as the program's .cubit.description section; launchers validate it
--  here, check a call's values against it, and render them: the launch
--  block's argv (CuBit.Launch_Arguments) and one delegated place per file
--  value (CuBit.Launch_Grants). Arguments stay data; the places are the
--  authority, and procmgr checks those against what the launcher holds.
--
--  CuBit has no stdin, stdout or stderr. A program declares any number of
--  connectors, each with a fully qualified name (unix.stderr, com.cubit.stdlog),
--  a direction, an element type and a signal mode. A ported Unix program's
--  file descriptors map onto connectors (porting glue, read by its libc only).
--
--  Descriptor, little-endian:
--    "PDSC", u16 version (1), u8 parameter count (0 .. 16),
--    u8 piece count (0 .. 24), u8 connector count (0 .. 16),
--    u8 descriptor count (0 .. 8), u16 reserved (0), then
--    per parameter: u8 kind, u8 flags (1: many, 2: optional),
--      u8 name length (1 .. 32), the name;
--    per piece: u8 piece (1 Literal, 2 Value, 3 When_Set), u8 parameter
--      index (Value, When_Set; 0 for Literal), u8 text length (Literal and
--      When_Set: 1 .. 48; Value: 0), the text;
--    per connector: u8 direction, u8 element, u8 mode, u8 ring pages
--      (1 .. 255), u8 name length (3 .. 48), the qualified name;
--    per descriptor: u8 descriptor number, u8 connector index (an outlet
--      for descriptors other than 0, an inlet for 0).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;

package CuBit.Program_Descriptions with Pure, SPARK_Mode is

   Magic_0 : constant := Character'Pos ('P');
   Magic_1 : constant := Character'Pos ('D');
   Magic_2 : constant := Character'Pos ('S');
   Magic_3 : constant := Character'Pos ('C');
   Version : constant := 1;
   Header_Bytes : constant := 12;

   Maximum_Parameters  : constant := 16;
   Maximum_Pieces      : constant := 24;
   Maximum_Name_Bytes  : constant := 32;
   Maximum_Text_Bytes  : constant := 48;
   Maximum_Connectors       : constant := 16;
   Maximum_Connector_Name_Bytes : constant := 48;
   Minimum_Connector_Name_Bytes : constant := 3;
   Maximum_Descriptors : constant := 8;
   Maximum_Descriptor_Bytes : constant :=
     Header_Bytes + Maximum_Parameters * (3 + Maximum_Name_Bytes) +
     Maximum_Pieces * (3 + Maximum_Text_Bytes) +
     Maximum_Connectors * (5 + Maximum_Connector_Name_Bytes) + Maximum_Descriptors * 2;
   --  It fits a manifest section (CCL.Manifests.MAX_SECTION_BYTES, 4096).
   pragma Compile_Time_Error
     (Maximum_Descriptor_Bytes > 4_096, "descriptor exceeds a manifest section");

   type Kind is
     (Input_File, Output_File, Input_Directory, Output_Directory, Flag, Text);
   for Kind use
     (Input_File => 1, Output_File => 2, Input_Directory => 3,
      Output_Directory => 4, Flag => 5, Text => 6);
   Many_Flag     : constant Unsigned_8 := 1;
   Optional_Flag : constant Unsigned_8 := 2;

   type Piece_Kind is (Literal, Value, When_Set);
   for Piece_Kind use (Literal => 1, Value => 2, When_Set => 3);

   --  Connectors.
   type Connector_Direction is (Inlet, Outlet);
   for Connector_Direction use (Inlet => 1, Outlet => 2);
   --  An element's type. Text: one element per line; Bytes: raw chunks;
   --  Log_Record: CuBit's structured log record (docs/typed-logging.md).
   type Element_Kind is (Text_Lines, Raw_Bytes, Integers, Log_Records);
   for Element_Kind use
     (Text_Lines => 1, Raw_Bytes => 2, Integers => 3, Log_Records => 4);
   --  How values arrive: a sequence of elements; exactly one value, ever;
   --  a current state (intermediate values may be coalesced); discrete
   --  transitions, never coalesced.
   type Signal_Kind is (Stream, One_Shot, Level, Edge);
   for Signal_Kind use (Stream => 1, One_Shot => 2, Level => 3, Edge => 4);

   subtype Parameter_Count is Natural range 0 .. Maximum_Parameters;
   subtype Parameter_Index is Natural range 0 .. Maximum_Parameters - 1;
   subtype Piece_Count is Natural range 0 .. Maximum_Pieces;
   subtype Connector_Count is Natural range 0 .. Maximum_Connectors;
   subtype Connector_Index is Natural range 0 .. Maximum_Connectors - 1;
   subtype Descriptor_Count is Natural range 0 .. Maximum_Descriptors;
   subtype Descriptor_Length is Natural range 0 .. Maximum_Descriptor_Bytes;
   subtype Descriptor_Number is Natural range 0 .. 255;
   --  A connector's ring, in pages, as the producer creates it.
   subtype Ring_Pages is Positive range 1 .. 255;

   type Bytes is array (Positive range <>) of Unsigned_8;

   --  A qualified connector name: at least two dot-separated components of
   --  [a-z0-9-], each non-empty.
   function Valid_Connector_Name (Name : String) return Boolean is
     (Name'Length in Minimum_Connector_Name_Bytes .. Maximum_Connector_Name_Bytes
      and then (for all C of Name => C in 'a' .. 'z' | '0' .. '9' | '-' | '.')
      and then Name (Name'First) /= '.' and then Name (Name'Last) /= '.'
      and then (for some K in Name'Range => Name (K) = '.')
      and then (for all K in Name'First .. Name'Last - 1 =>
                  not (Name (K) = '.' and then Name (K + 1) = '.')));

   --  The ring a connector's producer creates (CuBit.Streams' stream id): its
   --  position plus one, since 0 means "no stream".
   function Ring_Id (P : Connector_Index) return Unsigned_16 is (Unsigned_16 (P) + 1);

   --  The decoded description.
   type Parameter is record
      Of_Kind  : Kind := Text;
      Many     : Boolean := False;
      Optional : Boolean := False;
      Name     : String (1 .. Maximum_Name_Bytes) := [others => ' '];
      Name_Length : Natural range 0 .. Maximum_Name_Bytes := 0;
   end record;
   type Piece is record
      Of_Kind   : Piece_Kind := Literal;
      Parameter : Parameter_Index := 0;
      Text      : String (1 .. Maximum_Text_Bytes) := [others => ' '];
      Text_Length : Natural range 0 .. Maximum_Text_Bytes := 0;
   end record;
   --  An inlet or an outlet.
   type Connector is record
      Direction : Connector_Direction := Outlet;
      Element   : Element_Kind := Text_Lines;
      Signal    : Signal_Kind := Stream;
      Pages     : Ring_Pages := 1;
      Name      : String (1 .. Maximum_Connector_Name_Bytes) := [others => ' '];
      Name_Length : Natural range 0 .. Maximum_Connector_Name_Bytes := 0;
   end record;
   type Descriptor_Map is record
      Number : Descriptor_Number := 0;
      Target : Connector_Index := 0;
   end record;
   type Parameter_Array is array (Parameter_Index) of Parameter;
   type Piece_Array is array (1 .. Maximum_Pieces) of Piece;
   type Connector_Array is array (Connector_Index) of Connector;
   type Descriptor_Array is array (1 .. Maximum_Descriptors) of Descriptor_Map;
   type Signature is record
      Parameters : Parameter_Array;
      Parameter_Total : Parameter_Count := 0;
      Pieces : Piece_Array;
      Piece_Total : Piece_Count := 0;
      Connectors : Connector_Array;
      Connector_Total : Connector_Count := 0;
      Descriptors : Descriptor_Array;
      Descriptor_Total : Descriptor_Count := 0;
   end record;

   function Well_Formed (S : Signature) return Boolean is
     ((for all P in 1 .. S.Piece_Total =>
         (if S.Pieces (P).Of_Kind /= Literal then
            S.Pieces (P).Parameter < S.Parameter_Total))
      and then
      (for all P in 1 .. S.Piece_Total =>
         (if S.Pieces (P).Of_Kind /= Value then S.Pieces (P).Text_Length >= 1))
      and then
      (for all D in 1 .. S.Descriptor_Total =>
         S.Descriptors (D).Target < S.Connector_Total));

   --  Decode a .cubit.description section. Malformed: Accepted False and S
   --  describes nothing.
   procedure Decode (Item : Bytes; S : out Signature; Accepted : out Boolean)
   with Pre  => Item'First = 1 and then Item'Length <= Maximum_Descriptor_Bytes,
        Post => (if Accepted then Well_Formed (S)
                 else S.Parameter_Total = 0 and then S.Piece_Total = 0
                      and then S.Connector_Total = 0 and then S.Descriptor_Total = 0);

   --  The parameter called Name, if S has one.
   procedure Find (S : Signature; Name : String; Index : out Parameter_Index;
                   Found : out Boolean)
   with Post => (if Found then Index < S.Parameter_Total);

   --  The connector called Name, if S has one.
   procedure Find_Connector (S : Signature; Name : String; Index : out Connector_Index;
                        Found : out Boolean)
   with Post => (if Found then Index < S.Connector_Total);

   --  A call's values: entries (parameter index, text), in order, the text
   --  in one buffer. A Flag's entry means true; its text is ignored.
   Maximum_Values : constant := 256;
   Maximum_Value_Text : constant := 32 * 1024;
   subtype Value_Count is Natural range 0 .. Maximum_Values;
   subtype Text_Position is Natural range 0 .. Maximum_Value_Text;
   type Value_Entry is record
      Parameter : Parameter_Index := 0;
      First : Positive := 1;
      Last  : Text_Position := 0;
   end record;
   type Value_Array is array (1 .. Maximum_Values) of Value_Entry;
   type Values is record
      Entries : Value_Array;
      Total : Value_Count := 0;
      Text : String (1 .. Maximum_Value_Text) := [others => ' '];
      Used : Text_Position := 0;
   end record;

   procedure Clear (V : out Values)
   with Post => V.Total = 0 and then V.Used = 0;

   --  Add one value for parameter P; False when the buffers are full.
   procedure Add (V : in out Values; P : Parameter_Index; Item : String;
                  Added : out Boolean);

   type Check_Result is
     (Matches,
      Unknown_Parameter,   --  a value for a parameter the program lacks
      Missing_Parameter,   --  a required parameter has no value
      Too_Many_Values,     --  more than one value for a single parameter
      Bad_File_Name,       --  a file value is not a grantable CuBit name
      Bad_Text,            --  a text value holds a NUL
      Too_Many_Places,     --  more file values than places a launch carries
      Too_Large);          --  the argv would not fit a launch block

   function Rights_For (K : Kind) return Unsigned_8 is
     (case K is
         when Input_File | Input_Directory => CuBit.Launch_Grants.Read_Right,
         when Output_File | Output_Directory =>
           CuBit.Launch_Grants.Read_Right or CuBit.Launch_Grants.Write_Right
           or CuBit.Launch_Grants.Create_Right,
         when Flag | Text => 0);

   --  Check V against S and render: Block gets argv (Program, then the
   --  pieces in order), Grants one place per file value with the rights its
   --  kind carries. Nothing is to be launched unless Result = Matches.
   procedure Render
     (S : Signature; V : Values; Program : String;
      Block : out CuBit.Launch_Arguments.Builder;
      Grants : out CuBit.Launch_Grants.Builder;
      Result : out Check_Result)
   with Pre => Well_Formed (S) and then V.Total <= Maximum_Values
               and then V.Used <= Maximum_Value_Text
               and then (for all E in 1 .. V.Total =>
                           V.Entries (E).Last <= V.Used
                           and then V.Entries (E).First <= V.Entries (E).Last + 1);

   ---------------------------------------------------------------------------
   --  OP_PROGRAM_DESCRIPTION, procmgr's answer to "what does this program
   --  take and offer?", for a program the requester may launch (its
   --  .cubit.launch names it), so a launcher needs no read access to
   --  program files. The requester lends procmgr a writable grant: the
   --  program name (Name_Bytes), then room for Maximum_Descriptor_Bytes.
   --    words (0) grant reference (CuBit.Grant_References wire form)
   --    words (1) name bytes
   --  Reply OK: words (0) = the description's length, written after the
   --  name; 0 when the program has none.
   --  Reply error: words (0) = CuBit.Launch_Arguments.Launch_Failure
   --  (Not_Granted: not in the requester's launch table; Spawn_Failed: the
   --  program could not be read; Arguments_Rejected: its description is
   --  malformed).
   ---------------------------------------------------------------------------
   Description_Operation     : constant := 16#0109#;
   Description_Request_Words : constant := 2;

end CuBit.Program_Descriptions;
