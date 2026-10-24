with Interfaces; use Interfaces;
with CCL.Resource_Sections;

--  The .cubit.devices ELF section: the devices a driver drives, from its
--  manifest's `drives` list (HW-002, docs/device-matching.md). CCL.Manifests
--  writes it with Encode; devmgr and the image tools read it with Decode,
--  which treats the bytes as untrusted and accepts exactly what Encode
--  can produce. Matching and precedence are defined here once, for devmgr,
--  the image check and the tests.
package CCL.Device_Sections with Pure, SPARK_Mode => On is
   MAGIC : constant := 16#5644_4243#;   --  "CBDV" little-endian
   VERSION : constant := 1;
   HEADER_BYTES : constant := 8;
   ENTRY_BYTES : constant := 16;
   --  Kind, payload length and two reserved bytes precede each payload.
   ENTRY_PREFIX_BYTES : constant := 4;
   MAX_ENTRIES : constant := 16;
   MAX_SECTION_BYTES : constant := HEADER_BYTES + MAX_ENTRIES * ENTRY_BYTES;

   type Match_Kind is (PCI_IDs, PCI_Class, ACPI_ID, USB_Class, Platform);
   for Match_Kind use
     (PCI_IDs => 1, PCI_Class => 2, ACPI_ID => 3, USB_Class => 4, Platform => 5);

   --  0xFFFF reads back from configuration space where no function is.
   NO_VENDOR : constant := 16#FFFF#;
   --  An ACPI _HID or _CID: "PNP" and four hex digits (7), or a four-character
   --  vendor prefix and four hex digits (8).
   ACPI_ID_MIN : constant := 7;
   ACPI_ID_MAX : constant := 8;
   subtype ACPI_Length is Natural range 0 .. ACPI_ID_MAX;
   subtype ACPI_Index is Positive range 1 .. ACPI_ID_MAX;
   type ACPI_Text is array (ACPI_Index) of Character;

   subtype Platform_Device is CCL.Resource_Sections.Platform_Device;
   use type CCL.Resource_Sections.Platform_Device;

   --  One `drives` entry. Fields a kind does not use are zero. PCI_IDs:
   --  Vendor, First .. Last. PCI_Class and USB_Class: Class, Subclass and
   --  Program (the PCI programming interface, or the USB protocol).
   --  ACPI_ID: the ID. Platform: the device.
   type Device_Match is record
      Kind        : Match_Kind := PCI_IDs;
      Vendor      : Unsigned_16 := 0;
      First       : Unsigned_16 := 0;
      Last        : Unsigned_16 := 0;
      Class       : Unsigned_8 := 0;
      Subclass    : Unsigned_8 := 0;
      Program   : Unsigned_8 := 0;
      ID          : ACPI_Text := [others => ASCII.NUL];
      ID_Length   : ACPI_Length := 0;
      Device      : Platform_Device := Platform_Device'First;
   end record;

   function Valid_ID (ID : ACPI_Text; Length : ACPI_Length) return Boolean is
     (Length in ACPI_ID_MIN .. ACPI_ID_MAX
      and then (for all I in ACPI_Index =>
                  (if I <= Length then ID (I) in 'A' .. 'Z' | '0' .. '9'
                   else ID (I) = ASCII.NUL)));

   --  Whether M is an entry Encode can write: the unused fields zero.
   function Valid_Match (M : Device_Match) return Boolean is
     (case M.Kind is
         when PCI_IDs =>
            M.Vendor /= NO_VENDOR and then M.First <= M.Last
            and then M.Class = 0 and then M.Subclass = 0 and then M.Program = 0
            and then M.ID_Length = 0 and then (for all C of M.ID => C = ASCII.NUL)
            and then M.Device = Platform_Device'First,
         when PCI_Class | USB_Class =>
            M.Vendor = 0 and then M.First = 0 and then M.Last = 0
            and then M.ID_Length = 0 and then (for all C of M.ID => C = ASCII.NUL)
            and then M.Device = Platform_Device'First,
         when ACPI_ID =>
            Valid_ID (M.ID, M.ID_Length)
            and then M.Vendor = 0 and then M.First = 0 and then M.Last = 0
            and then M.Class = 0 and then M.Subclass = 0 and then M.Program = 0
            and then M.Device = Platform_Device'First,
         when Platform =>
            M.Vendor = 0 and then M.First = 0 and then M.Last = 0
            and then M.Class = 0 and then M.Subclass = 0 and then M.Program = 0
            and then M.ID_Length = 0 and then (for all C of M.ID => C = ASCII.NUL));

   subtype Entry_Count is Natural range 0 .. MAX_ENTRIES;
   subtype Entry_Index is Positive range 1 .. MAX_ENTRIES;
   type Match_Array is array (Entry_Index) of Device_Match;
   type Match_Table is record
      Count   : Entry_Count := 0;
      Entries : Match_Array := [others => (others => <>)];
   end record;

   function Valid_Table (T : Match_Table) return Boolean is
     (T.Count >= 1
      and then (for all I in 1 .. T.Count => Valid_Match (T.Entries (I))));

   type Byte_Array is array (Positive range <>) of Unsigned_8;
   subtype Section_Length is Natural range 0 .. MAX_SECTION_BYTES;
   subtype Section_Bytes is Byte_Array (1 .. MAX_SECTION_BYTES);

   --  The section for T: Bytes (1 .. Length).
   procedure Encode (T : Match_Table; Bytes : out Section_Bytes; Length : out Section_Length)
   with Pre  => Valid_Table (T),
        Post => Length = HEADER_BYTES + T.Count * ENTRY_BYTES;

   type Decode_Status is
     (Decoded, Invalid_Length, Invalid_Header, Invalid_Entry);

   procedure Decode (Data : Byte_Array; T : out Match_Table; Status : out Decode_Status)
   with Pre  => Data'First = 1 and then Data'Length <= MAX_SECTION_BYTES,
        Post => (if Status = Decoded then Valid_Table (T));

   --  What devmgr knows of one PCI function (its configuration header).
   type PCI_Function is record
      Vendor    : Unsigned_16 := NO_VENDOR;
      Device    : Unsigned_16 := 0;
      Class     : Unsigned_8 := 0;
      Subclass  : Unsigned_8 := 0;
      Program : Unsigned_8 := 0;
   end record;

   --  How specifically an entry matches a function: 0 not at all; a class
   --  match 1; an ID-range match above every class match, the narrower the
   --  range the higher (docs/device-matching.md "Precedence").
   ID_RANK_BASE : constant := 2;
   subtype Rank is Natural range 0 .. ID_RANK_BASE + 16#FFFF#;
   No_Rank    : constant Rank := 0;
   Class_Rank : constant Rank := 1;

   function Rank_Of (M : Device_Match; F : PCI_Function) return Rank is
     (case M.Kind is
         when PCI_IDs =>
            (if F.Vendor = M.Vendor and then F.Device in M.First .. M.Last
             then ID_RANK_BASE + (16#FFFF# - Natural (M.Last - M.First))
             else No_Rank),
         when PCI_Class =>
            (if F.Vendor /= NO_VENDOR and then F.Class = M.Class
               and then F.Subclass = M.Subclass and then F.Program = M.Program
             then Class_Rank else No_Rank),
         when ACPI_ID | USB_Class | Platform => No_Rank)
   with Pre => Valid_Match (M);

   --  The best rank of any entry of T for F, and the first entry with it
   --  (0 when none matches).
   procedure Best (T : Match_Table; F : PCI_Function; Best_Rank : out Rank; At_Entry : out Entry_Count)
   with Pre  => Valid_Table (T),
        Post => (if At_Entry = 0 then Best_Rank = No_Rank
                 else At_Entry <= T.Count
                      and then Best_Rank = Rank_Of (T.Entries (At_Entry), F)
                      and then Best_Rank /= No_Rank)
                and then (for all I in 1 .. T.Count => Rank_Of (T.Entries (I), F) <= Best_Rank);

   --  Whether T declares the platform device P.
   function Declares_Platform (T : Match_Table; P : Platform_Device) return Boolean is
     (for some I in 1 .. T.Count =>
        T.Entries (I).Kind = Platform and then T.Entries (I).Device = P)
   with Pre => T.Count <= MAX_ENTRIES;

   --  Whether some device could match A and B (entries of two different
   --  drivers) equally specifically: then neither may bind it, and an
   --  image that holds both is refused.
   function Ambiguous (A, B : Device_Match) return Boolean is
     (A.Kind = B.Kind
      and then
        (case A.Kind is
            when PCI_IDs =>
               A.Vendor = B.Vendor
               and then A.First <= B.Last and then B.First <= A.Last
               and then A.Last - A.First = B.Last - B.First,
            when PCI_Class | USB_Class =>
               A.Class = B.Class and then A.Subclass = B.Subclass
               and then A.Program = B.Program,
            when ACPI_ID => A.ID_Length = B.ID_Length
                              and then (for all C in ACPI_Index => A.ID (C) = B.ID (C)),
            when Platform => A.Device = B.Device))
   with Pre => Valid_Match (A) and then Valid_Match (B);
end CCL.Device_Sections;
