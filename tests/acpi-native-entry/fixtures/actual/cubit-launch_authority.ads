------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What a program may start, and what its children may hold
--  (docs/process-arguments.md, "Launch authority").
--
--  @description
--  An executable's launch table (.cubit.launch) names the programs it may
--  start with OP_LAUNCH; procmgr refuses any other name before anything
--  runs. There is no PATH search: names are compared exactly. Layout,
--  little-endian:
--
--     0  u32  Magic ("LNCH")
--     4  u16  Table_Version
--     6  u16  name count (0 .. Maximum_Names)
--     8  per name: u8 length (1 .. 255), then that many non-NUL bytes
--
--  and nothing after the last name. A child started with OP_LAUNCH holds
--  less authority than its launcher, never more: every scope it requests
--  must be covered by a scope the launcher holds (Scope_Covered).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Launch_Authority with SPARK_Mode is

   Magic         : constant Unsigned_32 := 16#48434E4C#;  --  "LNCH"
   Table_Version : constant := 1;
   Header_Bytes  : constant := 8;
   Maximum_Names : constant := 32;
   Maximum_Table_Bytes : constant := 512;
   Maximum_Name_Bytes  : constant := 255;

   Version_Offset : constant := 4;
   Count_Offset   : constant := 6;

   subtype Table_Length is Natural range 0 .. Maximum_Table_Bytes;
   subtype Table_Index is Positive range 1 .. Maximum_Table_Bytes;
   subtype Name_Count is Natural range 0 .. Maximum_Names;
   type Table_Bytes is array (Table_Index range <>) of Unsigned_8;

   function U16_At (Item : Table_Bytes; Offset : Natural) return Unsigned_16
   is (Unsigned_16 (Item (Offset + 1)) or
       Shift_Left (Unsigned_16 (Item (Offset + 2)), 8))
   with Pre => Item'First = 1 and then Item'Length >= 2
               and then Offset <= Item'Length - 2;

   function U32_At (Item : Table_Bytes; Offset : Natural) return Unsigned_32
   is (Unsigned_32 (Item (Offset + 1)) or
       Shift_Left (Unsigned_32 (Item (Offset + 2)), 8) or
       Shift_Left (Unsigned_32 (Item (Offset + 3)), 16) or
       Shift_Left (Unsigned_32 (Item (Offset + 4)), 24))
   with Pre => Item'First = 1 and then Item'Length >= 4
               and then Offset <= Item'Length - 4;

   --  Remaining names, starting at Position, are well formed and end the
   --  table exactly.
   function Names_Valid
     (Item : Table_Bytes; Position : Positive; Remaining : Name_Count)
      return Boolean
   is (if Remaining = 0 then Position = Item'Last + 1
       else Position <= Item'Last
            and then Item (Position) > 0
            and then Natural (Item (Position)) <= Item'Last - Position
            and then (for all K in Position + 1 ..
                                   Position + Natural (Item (Position)) =>
                        Item (K) /= 0)
            and then Names_Valid
              (Item, Position + 1 + Natural (Item (Position)), Remaining - 1))
   with Ghost,
        Pre => Item'First = 1 and then Item'Length <= Maximum_Table_Bytes
               and then Position <= Item'Last + 1,
        Subprogram_Variant => (Decreases => Remaining);

   function Header_Valid (Item : Table_Bytes) return Boolean is
     (Item'Length in Header_Bytes .. Maximum_Table_Bytes
      and then U32_At (Item, 0) = Magic
      and then U16_At (Item, Version_Offset) = Table_Version
      and then U16_At (Item, Count_Offset) <= Maximum_Names)
   with Pre => Item'First = 1;

   function Well_Formed (Item : Table_Bytes) return Boolean is
     (Header_Valid (Item)
      and then Names_Valid
        (Item, Header_Bytes + 1, Natural (U16_At (Item, Count_Offset))))
   with Ghost, Pre => Item'First = 1;

   function Valid (Item : Table_Bytes) return Boolean
   with Pre => Item'First = 1,
        Post => Valid'Result = Well_Formed (Item);

   ---------------------------------------------------------------------------
   --  OP_LAUNCH_TABLE: the requester's own launch table (what it may start;
   --  its manifest's may_launch), so a launcher such as the CCL console can
   --  offer exactly those programs. Seeing one's own grants needs no
   --  authority. The requester lends procmgr a writable grant of
   --  Maximum_Table_Bytes:
   --    words (0) grant reference (CuBit.Grant_References wire form)
   --  Reply OK: words (0) = the table's length written there (0: none).
   ---------------------------------------------------------------------------
   Table_Operation     : constant := 16#010A#;
   Table_Request_Words : constant := 1;

   --  Name Index (1-based) of a valid table: its bytes are Item (First ..
   --  Last); Found False past the last name.
   procedure Name_At
     (Item : Table_Bytes; Index : Positive; First : out Positive; Last : out Natural;
      Found : out Boolean)
   with Pre => Item'First = 1 and then Valid (Item),
        Post => (if Found then First <= Last + 1 and then Last <= Item'Last);

   --  Whether Name is one of the table's names (exact, byte for byte).
   function Contains (Item : Table_Bytes; Name : String) return Boolean
   with Pre => Item'First = 1 and then Valid (Item);

   ---------------------------------------------------------------------------
   --  Attenuation. Scopes are procmgr's access entries (service, rights
   --  bits, prefix). A child's scope is covered when the launcher holds one
   --  for the same service with at least the same rights and, for the
   --  filesystem, a prefix that matches the child's prefix as a path
   --  (CuBit.File_Access.Scope_Matches); other services need the same
   --  prefix exactly.
   ---------------------------------------------------------------------------
   Filesystem_Service : constant Unsigned_8 := 0;
   Maximum_Prefix_Bytes : constant := 256;   --  CuBit.File_Access

   function Rights_Within (Held, Requested : Unsigned_8) return Boolean is
     ((Requested and not Held) = 0);

   function Scope_Covered
     (Held_Service, Held_Rights : Unsigned_8; Held_Prefix : String;
      Service, Rights : Unsigned_8; Prefix : String) return Boolean
   with Pre => Held_Prefix'Length <= Maximum_Prefix_Bytes
               and then Prefix'Length <= Maximum_Prefix_Bytes;

end CuBit.Launch_Authority;
