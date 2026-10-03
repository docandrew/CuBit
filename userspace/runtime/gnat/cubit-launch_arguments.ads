------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Launch arguments: the argument vector and environment a launcher gives
--  a new process (docs/process-arguments.md).
--
--  @description
--  A block is a header followed by NUL-terminated strings, little-endian:
--
--     0  u16  format version (Format_Version)
--     2  u16  reserved, zero
--     4  u32  argument count (argv[0], the program name, included)
--     8  u32  environment count
--    12  u32  string bytes (block length - Header_Bytes)
--    16  the argument strings, then the environment strings, each ended
--        by one NUL byte
--
--  Arguments are data, never authority: nothing in a block grants access
--  to anything. procmgr validates a block (Validate) before the kernel maps
--  it read-only into the child at Block_Address; the child validates it
--  again before use. Validate is proved to accept exactly the Well_Formed
--  blocks, and Next_String to stay within a well-formed block.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Launch_Arguments with Pure, SPARK_Mode is

   Format_Version      : constant := 1;
   Header_Bytes        : constant := 16;
   Maximum_Block_Bytes : constant := 64 * 1024;
   Maximum_Strings     : constant := 4096;

   --  Where the kernel maps a process's block, read-only, and the page
   --  size it maps it in. Must match kernel/src/process_launch.ads.
   Block_Address : constant := 16#0000_5A00_0000_0000#;

   subtype Block_Length is Natural range 0 .. Maximum_Block_Bytes;
   subtype Present_Length is
     Block_Length range Header_Bytes .. Maximum_Block_Bytes;
   subtype String_Count is Natural range 0 .. Maximum_Strings;
   subtype Block_Index is Positive range 1 .. Maximum_Block_Bytes;
   --  A position one past the last byte is a valid cursor value.
   subtype Cursor is Positive range 1 .. Maximum_Block_Bytes + 1;
   type Block is array (Block_Index range <>) of Unsigned_8;

   --  Header field offsets (bytes from the start of the block).
   Version_Offset           : constant := 0;
   Reserved_Offset          : constant := 2;
   Argument_Count_Offset    : constant := 4;
   Environment_Count_Offset : constant := 8;
   String_Bytes_Offset      : constant := 12;
   Field_16_Bytes           : constant := 2;
   Field_32_Bytes           : constant := 4;

   Terminator : constant Unsigned_8 := 0;

   function Field_16 (Item : Block; Offset : Natural) return Unsigned_16 is
     (Unsigned_16 (Item (Offset + 1)) or
      Shift_Left (Unsigned_16 (Item (Offset + 2)), 8))
   with Pre => Item'First = 1 and then Item'Length >= Field_16_Bytes
               and then Offset <= Item'Length - Field_16_Bytes;

   function Field_32 (Item : Block; Offset : Natural) return Unsigned_32 is
     (Unsigned_32 (Item (Offset + 1)) or
      Shift_Left (Unsigned_32 (Item (Offset + 2)), 8) or
      Shift_Left (Unsigned_32 (Item (Offset + 3)), 16) or
      Shift_Left (Unsigned_32 (Item (Offset + 4)), 24))
   with Pre => Item'First = 1 and then Item'Length >= Field_32_Bytes
               and then Offset <= Item'Length - Field_32_Bytes;

   function Header_Valid (Item : Block) return Boolean is
     (Item'Length in Present_Length
      and then Field_16 (Item, Version_Offset) = Format_Version
      and then Field_16 (Item, Reserved_Offset) = 0
      and then Field_32 (Item, Argument_Count_Offset) <= Maximum_Strings
      and then Field_32 (Item, Environment_Count_Offset) <=
               Maximum_Strings - Field_32 (Item, Argument_Count_Offset)
      and then Field_32 (Item, String_Bytes_Offset) =
               Unsigned_32 (Item'Length - Header_Bytes))
   with Pre => Item'First = 1;

   function Arguments_Declared (Item : Block) return String_Count is
     (Natural (Field_32 (Item, Argument_Count_Offset)))
   with Pre => Item'First = 1 and then Header_Valid (Item);

   function Environment_Declared (Item : Block) return String_Count is
     (Natural (Field_32 (Item, Environment_Count_Offset)))
   with Pre => Item'First = 1 and then Header_Valid (Item);

   function Strings_Declared (Item : Block) return String_Count is
     (String_Count (Field_32 (Item, Argument_Count_Offset) +
                    Field_32 (Item, Environment_Count_Offset)))
   with Pre => Item'First = 1 and then Header_Valid (Item);

   --  The number of terminators among the string bytes up to Last.
   function Terminators (Item : Block; Last : Natural) return Natural is
     (if Last <= Header_Bytes then 0
      else Terminators (Item, Last - 1) +
           (if Item (Last) = Terminator then 1 else 0))
   with Ghost,
        Pre => Item'First = 1 and then Last <= Item'Last,
        Post => (if Last <= Header_Bytes then Terminators'Result = 0
                 else Terminators'Result <= Last - Header_Bytes),
        Subprogram_Variant => (Decreases => Last);

   --  A valid header; the string bytes end with a terminator; and they hold
   --  exactly as many terminators as declared strings. Together: exactly
   --  Strings_Declared strings, each terminated, and nothing after them.
   function Well_Formed (Item : Block) return Boolean is
     (Header_Valid (Item)
      and then (Item'Length = Header_Bytes
                or else Item (Item'Last) = Terminator)
      and then Terminators (Item, Item'Last) = Strings_Declared (Item))
   with Ghost, Pre => Item'First = 1;

   type Validation is
     (Valid, Wrong_Length, Unknown_Format, Too_Many_Strings,
      Length_Mismatch, Unterminated, Count_Mismatch);

   function Validate (Item : Block) return Validation
   with Pre => Item'First = 1,
        Post => (Validate'Result = Valid) = Well_Formed (Item);

   --  The string starting at Position: its bytes are Position .. Last (an
   --  empty string has Last = Position - 1); Next starts the one after it.
   procedure Next_String
     (Item : Block; Position : Positive; Last : out Natural;
      Next : out Cursor)
   with Pre => Item'First = 1 and then Well_Formed (Item)
               and then Position in Header_Bytes + 1 .. Item'Last,
        Post => Last in Position - 1 .. Item'Last - 1
                and then Item (Last + 1) = Terminator
                and then Next = Last + 2
                and then (for all K in Position .. Last =>
                            Item (K) /= Terminator);

   --  The bounds of string Index (1-based, arguments then environment).
   --  Found is False only if the block runs out first, which a well-formed
   --  block never does for Index <= Strings_Declared (tested, not proved).
   procedure Locate
     (Item : Block; Index : Positive; First : out Positive;
      Last : out Natural; Found : out Boolean)
   with Pre => Item'First = 1 and then Well_Formed (Item),
        Post => (if Found then First in Header_Bytes + 1 .. Item'Last
                   and then Last in First - 1 .. Item'Last - 1
                   and then Item (Last + 1) = Terminator);

   ---------------------------------------------------------------------------
   --  Encoder, for launchers. Arguments first, then environment entries.
   --  Finish validates the result with Validate, so a block it accepts is
   --  well formed whatever the encoder's own bookkeeping did.
   ---------------------------------------------------------------------------
   type Builder is record
      Data           : Block (1 .. Maximum_Block_Bytes) := [others => 0];
      Used           : Present_Length := Header_Bytes;
      Arguments      : String_Count := 0;
      Environment    : String_Count := 0;
      In_Environment : Boolean := False;
   end record;

   function Builder_Valid (B : Builder) return Boolean is
     (B.Arguments + B.Environment <= Maximum_Strings);

   procedure Start (B : out Builder)
   with Post => Builder_Valid (B) and then B.Used = Header_Bytes;

   --  Rejected (B unchanged) if Value holds a NUL, does not fit, the string
   --  count is exhausted, or (for an argument) environment entries began.
   procedure Add_Argument
     (B : in out Builder; Value : String; Accepted : out Boolean)
   with Pre => Builder_Valid (B), Post => Builder_Valid (B);

   procedure Add_Environment
     (B : in out Builder; Value : String; Accepted : out Boolean)
   with Pre => Builder_Valid (B), Post => Builder_Valid (B);

   --  Writes the header. The block is B.Data (1 .. Length).
   procedure Finish
     (B : in out Builder; Length : out Present_Length; Accepted : out Boolean)
   with Pre => Builder_Valid (B),
        Post => Builder_Valid (B) and then Length = B.Used
                and then (if Accepted
                          then Well_Formed (B.Data (1 .. Length)));

   ---------------------------------------------------------------------------
   --  OP_LAUNCH, procmgr's spawn with launch arguments. The requester lends
   --  procmgr a grant holding the program name (Name_Bytes) followed by a
   --  launch block (Argument_Bytes; zero: none, the child gets no block):
   --    words (0) grant reference (CuBit.Grant_References wire form)
   --    words (1) name bytes, (2) argument bytes, (3) priority (0: default)
   --  Reply OK: words (0) = child PID, (1) = its generation (as in its
   --  EVENT_CHILD_EXIT); the requester becomes the child's parent and
   --  receives that event (CuBit.Child_Exits).
   --  Reply error: words (0) = Launch_Failure.
   ---------------------------------------------------------------------------
   Launch_Operation     : constant := 16#0106#;
   Launch_Request_Words : constant := 4;

   --  Not_Granted: the requester's launch table (CuBit.Launch_Authority)
   --  does not name the program, or the program asks for authority the
   --  requester does not hold.
   type Launch_Failure is
     (Malformed_Request, Grant_Unavailable, Arguments_Rejected, Spawn_Failed,
      Not_Granted);
   for Launch_Failure use
     (Malformed_Request => 1, Grant_Unavailable => 2,
      Arguments_Rejected => 3, Spawn_Failed => 4, Not_Granted => 5);

   Maximum_Name_Bytes : constant := 255;
   subtype Name_Length is Natural range 1 .. Maximum_Name_Bytes;

   function Request_Valid (Name_Bytes, Argument_Bytes : Unsigned_64)
     return Boolean is
     (Name_Bytes in 1 .. Maximum_Name_Bytes
      and then (Argument_Bytes = 0
                or else Argument_Bytes in
                          Header_Bytes .. Maximum_Block_Bytes));

end CuBit.Launch_Arguments;
