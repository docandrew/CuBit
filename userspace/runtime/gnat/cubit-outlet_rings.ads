------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Launcher-owned outlet rings (docs/ccl-launch-parameters.md, "Launcher-owned
--  outlet rings"): which of a child's outlets write into a ring its
--  launcher owns and lends it.
--
--  One table format serves twice. In the OP_LAUNCH request (after the
--  delegated places) it lists the launcher's forwardable grants to procmgr;
--  in the child's launch block (between the strings and the description)
--  it lists the grants procmgr derived for the child, and Owner is the
--  grants' owner the child names when it acquires them: procmgr, in whose
--  grant namespace derived grants live.
--
--  Table, little-endian:
--    "RING", u16 version (1), u16 count (0 .. 16), u64 owner PID (0 in a
--    request), then per entry: u8 connector index, 7 reserved bytes (0),
--    u64 grant reference (CuBit.Grant_References wire form).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Outlet_Rings with Pure, SPARK_Mode is

   Magic_0 : constant := Character'Pos ('R');
   Magic_1 : constant := Character'Pos ('I');
   Magic_2 : constant := Character'Pos ('N');
   Magic_3 : constant := Character'Pos ('G');
   Version : constant := 1;
   Header_Bytes : constant := 16;
   Entry_Bytes  : constant := 16;
   Maximum_Entries : constant := 16;
   Maximum_Bytes : constant := Header_Bytes + Maximum_Entries * Entry_Bytes;

   subtype Entry_Count is Natural range 0 .. Maximum_Entries;
   subtype Table_Length is Natural range 0 .. Maximum_Bytes;
   subtype Outlet_Number is Natural range 0 .. 255;

   type Bytes is array (Positive range <>) of Unsigned_8;

   type Ring_Entry is record
      Outlet : Outlet_Number := 0;
      Grant : Unsigned_64 := 0;
   end record;
   type Entry_Array is array (1 .. Maximum_Entries) of Ring_Entry;

   type Table is record
      Owner   : Unsigned_64 := 0;
      Entries : Entry_Array;
      Count   : Entry_Count := 0;
   end record;

   --  No connector is listed twice.
   function Distinct (T : Table) return Boolean is
     (for all I in 1 .. T.Count =>
        (for all J in 1 .. T.Count => (if I /= J then T.Entries (I).Outlet /= T.Entries (J).Outlet)));

   --  The table's length when encoded.
   function Length_Of (T : Table) return Table_Length is
     (if T.Count = 0 then 0 else Header_Bytes + T.Count * Entry_Bytes);

   --  Encode T into Item (1 .. Length): nothing when it has no entries.
   procedure Encode (T : Table; Item : out Bytes; Length : out Table_Length)
   with Pre  => Item'First = 1 and then Item'Length = Maximum_Bytes,
        Post => Length = Length_Of (T);

   --  Whether Item starts with a ring table, and its length if so.
   procedure Measure (Item : Bytes; Present : out Boolean; Length : out Table_Length)
   with Pre  => Item'First = 1,
        Post => (if Present then Length in Header_Bytes .. Item'Length
                 else Length = 0);

   --  Decode the table at the start of Item (Length bytes, from Measure).
   --  Malformed (reserved bytes set, a connector twice): Accepted False, no
   --  entries.
   procedure Decode (Item : Bytes; T : out Table; Accepted : out Boolean)
   with Pre  => Item'First = 1 and then Item'Length <= Maximum_Bytes,
        Post => (if Accepted then Distinct (T) and then Length_Of (T) = Item'Length
                 else T.Count = 0);

end CuBit.Outlet_Rings;
