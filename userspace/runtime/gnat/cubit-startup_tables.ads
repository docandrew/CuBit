------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Startup tables: name -> capability handle, for what a process holds at
--  launch (CAP-002, docs/capability-handles.md "Startup capabilities").
--
--  @description
--  One format serves three readers:
--  * the manifest compiler writes a program's declaration as an ELF
--    section (.cubit.startup): every entry Unassigned, handle 0;
--  * the launcher (procmgr, devmgr) decodes it, obtains each capability,
--    and encodes the filled table (Granted or Absent) for the child;
--  * the child's runtime decodes the filled table and looks names up.
--  A name is the startup binding name from the manifest; the kernel never
--  sees names. A handle is the kernel's opaque word (slot and generation);
--  nothing here interprets it beyond "zero is no handle".
--
--  Layout, little-endian:
--
--     header (Header_Bytes)
--      0  u32  Magic
--      4  u16  Format_Version
--      6  u16  entry count (0 .. Maximum_Entries)
--      8  u8   handle limit kind (Limit_Kind)
--      9  3 x u8 reserved, zero
--     12  u32  handle limit count: 1 .. Maximum_Handle_Limit for At_Most,
--              zero otherwise
--     entries (Entry_Bytes each, in declaration order)
--      0  u8   kind (Capability_Kind)
--      1  u8   need (Need)
--      2  u8   state (Entry_State)
--      3  u8   name length (1 .. Maximum_Name_Bytes)
--      4  4 x u8 reserved, zero
--      8  u64  handle word: zero unless Granted, nonzero when Granted
--     16  name bytes, Maximum_Name_Bytes, zero after the name
--
--  The table is exactly Header_Bytes + count * Entry_Bytes long. Names are
--  lowercase kebab names (a leading letter, a-z, 0-9 and single hyphens,
--  no trailing hyphen) and pairwise distinct. An Absent entry is optional.
--
--  Decode is proved free of run-time errors and to accept only tables
--  satisfying Valid_Table, with a typed reason for every refusal. It reads
--  each byte of its input at most once: a launcher or child holding the
--  table in shared memory copies it once into a Bytes value and decodes
--  that copy (single fetch). Encode/Decode round trips are tested.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Startup_Tables with Pure, SPARK_Mode is

   --  "CBST" read as a little-endian word.
   Magic          : constant Unsigned_32 := 16#5453_4243#;
   Format_Version : constant := 1;

   --  A manifest declares at most 32 requests (CCL.Manifests.MAX_BINDINGS);
   --  the launcher adds the kernel's two self entries; the rest is room.
   Maximum_Entries    : constant := 40;
   Maximum_Name_Bytes : constant := 64;

   --  The handle limit a manifest gets when it does not name one
   --  (docs/capability-handles.md: 4096 for every program class for now),
   --  and the structural capacity of a process's handle table, the bound a
   --  Kernel_Limit table has (kernel Capability_Tables: 512 leaves x 64 slots).
   Standard_Handle_Limit : constant := 4_096;
   Maximum_Handle_Limit  : constant := 32_768;

   Header_Bytes        : constant := 16;
   Entry_Fixed_Bytes   : constant := 16;
   Entry_Bytes         : constant := Entry_Fixed_Bytes + Maximum_Name_Bytes;
   Maximum_Table_Bytes : constant := Header_Bytes + Maximum_Entries * Entry_Bytes;

   --  Header field offsets (bytes from the start of the table).
   Magic_Offset          : constant := 0;
   Version_Offset        : constant := 4;
   Count_Offset          : constant := 6;
   Limit_Kind_Offset     : constant := 8;
   Header_Reserved_First : constant := 9;
   Header_Reserved_Bytes : constant := 3;
   Limit_Count_Offset    : constant := 12;

   --  Entry field offsets (bytes from the start of the entry).
   Kind_Offset          : constant := 0;
   Need_Offset          : constant := 1;
   State_Offset         : constant := 2;
   Name_Length_Offset   : constant := 3;
   Entry_Reserved_First : constant := 4;
   Entry_Reserved_Bytes : constant := 4;
   Handle_Offset        : constant := 8;
   Name_Offset          : constant := Entry_Fixed_Bytes;

   --  What a startup capability is. Wire codes are fixed: a new kind takes
   --  a new code, never an old one.
   type Capability_Kind is
     (Endpoint, Notification, Framebuffer, Render, Network, Device_Memory,
      IO_Ports, Interrupt, DMA, Scheduling, Process, Resource);
   for Capability_Kind use
     (Endpoint => 1, Notification => 2, Framebuffer => 3, Render => 4,
      Network => 5, Device_Memory => 6, IO_Ports => 7, Interrupt => 8,
      DMA => 9, Scheduling => 10, Process => 11, Resource => 12);

   --  A required capability the launcher cannot obtain fails the launch; an
   --  optional one is recorded Absent and the program runs without it.
   type Need is (Required, Optional);
   for Need use (Required => 1, Optional => 2);

   --  Unassigned: declared, not yet obtained (manifest section). Granted:
   --  the handle is the capability. Absent: an optional capability the
   --  launcher did not obtain.
   type Entry_State is (Unassigned, Granted, Absent);
   for Entry_State use (Unassigned => 1, Granted => 2, Absent => 3);

   --  Standard_Limit is Standard_Handle_Limit, named so that a manifest
   --  which says nothing gets a value written down in one place.
   --  Kernel_Limit: no limit of the program's own, only the kernel's
   --  (Maximum_Handle_Limit) and the process's memory quota.
   type Limit_Kind is (Standard_Limit, At_Most, Kernel_Limit);
   for Limit_Kind use (Standard_Limit => 1, At_Most => 2, Kernel_Limit => 3);

   subtype Handle_Count is Unsigned_32 range 1 .. Maximum_Handle_Limit;

   type Handle_Limit (Kind : Limit_Kind := Standard_Limit) is record
      case Kind is
         when At_Most =>
            Count : Handle_Count := Standard_Handle_Limit;
         when Standard_Limit | Kernel_Limit =>
            null;
      end case;
   end record;

   --  The number of handles the limit allows.
   function Effective_Limit (Limit : Handle_Limit) return Handle_Count is
     (case Limit.Kind is
         when Standard_Limit => Standard_Handle_Limit,
         when At_Most        => Limit.Count,
         when Kernel_Limit      => Maximum_Handle_Limit);

   --  The kernel's handle word. Zero is never a handle (generation 0 is
   --  never issued).
   No_Handle : constant Unsigned_64 := 0;

   ---------------------------------------------------------------------------
   --  Names
   ---------------------------------------------------------------------------
   Pad : constant Character := Character'Val (0);
   subtype Name_Length is Natural range 0 .. Maximum_Name_Bytes;
   subtype Name_Index is Positive range 1 .. Maximum_Name_Bytes;
   type Name_Bytes is array (Name_Index) of Character;

   --  Canonical: the bytes after Length are Pad, so "=" compares names.
   type Name is record
      Length : Name_Length := 0;
      Text   : Name_Bytes := [others => Pad];
   end record;

   function Name_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | '0' .. '9' | '-');

   function Valid_Name (Item : Name) return Boolean is
     (Item.Length >= 1
      and then Item.Text (1) in 'a' .. 'z'
      and then Item.Text (Item.Length) /= '-'
      and then (for all I in 1 .. Item.Length => Name_Character (Item.Text (I)))
      and then (for all I in 2 .. Item.Length =>
                  not (Item.Text (I) = '-' and then Item.Text (I - 1) = '-'))
      and then (for all I in Item.Length + 1 .. Maximum_Name_Bytes =>
                  Item.Text (I) = Pad));

   --  Whether Text is a valid name (same rule as Valid_Name).
   function Valid_Name_Text (Text : String) return Boolean is
     (Text'Length in 1 .. Maximum_Name_Bytes
      and then Text (Text'First) in 'a' .. 'z'
      and then Text (Text'Last) /= '-'
      and then (for all I in Text'Range => Name_Character (Text (I)))
      and then (for all I in Text'Range =>
                  (if I > Text'First then not (Text (I) = '-' and then Text (I - 1) = '-'))));

   function To_Name (Text : String) return Name
   with Pre  => Text'Length <= Maximum_Name_Bytes,
        Post => To_Name'Result.Length = Text'Length
                and then (for all I in 1 .. Text'Length =>
                            To_Name'Result.Text (I) = Text (Text'First + (I - 1)))
                and then (for all I in Text'Length + 1 .. Maximum_Name_Bytes =>
                            To_Name'Result.Text (I) = Pad)
                and then (if Valid_Name_Text (Text) then Valid_Name (To_Name'Result));

   --  The names the launcher gives the kernel's self capabilities; a
   --  manifest may not declare them.
   Self_Text         : constant String := "self";
   Self_Process_Text : constant String := "self-process";

   function Reserved_Name (Item : Name) return Boolean is
     (Item = To_Name (Self_Text) or else Item = To_Name (Self_Process_Text));

   ---------------------------------------------------------------------------
   --  Tables
   ---------------------------------------------------------------------------
   type Startup_Entry is record
      Label       : Name;
      Kind        : Capability_Kind := Endpoint;
      Requirement : Need := Required;
      State       : Entry_State := Unassigned;
      Handle      : Unsigned_64 := No_Handle;
   end record;

   function Valid_Entry (Item : Startup_Entry) return Boolean is
     (Valid_Name (Item.Label)
      and then (case Item.State is
                  when Unassigned => Item.Handle = No_Handle,
                  when Granted    => Item.Handle /= No_Handle,
                  when Absent     => Item.Handle = No_Handle
                                     and then Item.Requirement = Optional));

   subtype Entry_Count is Natural range 0 .. Maximum_Entries;
   subtype Entry_Index is Positive range 1 .. Maximum_Entries;
   type Entry_Array is array (Entry_Index) of Startup_Entry;

   type Table is record
      Limit   : Handle_Limit;
      Count   : Entry_Count := 0;
      Entries : Entry_Array;
   end record;

   --  A function, not a constant: a constant of this size would need
   --  elaboration code, which the libc (No_Elaboration_Code) cannot have.
   function Empty_Table return Table is
     ((Limit => (Kind => Standard_Limit), Count => 0, Entries => [others => <>]));

   function Distinct_Names (Item : Table) return Boolean is
     (for all I in 1 .. Item.Count =>
        (for all J in I + 1 .. Item.Count =>
           Item.Entries (I).Label /= Item.Entries (J).Label));

   function Valid_Table (Item : Table) return Boolean is
     ((for all I in 1 .. Item.Count => Valid_Entry (Item.Entries (I)))
      and then Distinct_Names (Item));

   --  A manifest's declaration: nothing obtained yet.
   function Declaration (Item : Table) return Boolean is
     (for all I in 1 .. Item.Count => Item.Entries (I).State = Unassigned);

   --  A launcher's result: every entry obtained or, if optional, absent.
   function Complete (Item : Table) return Boolean is
     (for all I in 1 .. Item.Count => Item.Entries (I).State /= Unassigned);

   --  The entry named Label, or 0.
   function Find (Item : Table; Label : Name) return Entry_Count
   with Post => (if Find'Result /= 0
                 then Find'Result <= Item.Count
                      and then Item.Entries (Find'Result).Label = Label
                 else (for all I in 1 .. Item.Count =>
                         Item.Entries (I).Label /= Label));

   ---------------------------------------------------------------------------
   --  Wire form
   ---------------------------------------------------------------------------
   subtype Table_Length is Natural range 0 .. Maximum_Table_Bytes;
   subtype Byte_Index is Positive range 1 .. Maximum_Table_Bytes;
   type Bytes is array (Byte_Index range <>) of Unsigned_8;

   function Encoded_Length (Count : Entry_Count) return Table_Length is
     (Header_Bytes + Count * Entry_Bytes);

   procedure Encode (Item : Table; Output : out Bytes; Length : out Table_Length)
   with Pre  => Valid_Table (Item)
                and then Output'First = 1
                and then Output'Length >= Encoded_Length (Item.Count),
        Post => Length = Encoded_Length (Item.Count);

   type Decode_Result is
     (Decoded, Wrong_Length, Bad_Magic, Unknown_Version, Too_Many_Entries,
      Bad_Limit, Bad_Reserved, Bad_Kind, Bad_Need, Bad_State, Bad_Name,
      Bad_Handle, Duplicate_Name);

   procedure Decode (Raw : Bytes; Result : out Table; Outcome : out Decode_Result)
   with Pre  => Raw'First = 1,
        Post => (if Outcome = Decoded
                 then Valid_Table (Result)
                      and then Raw'Length = Encoded_Length (Result.Count)
                 else Result = Empty_Table);

end CuBit.Startup_Tables;
