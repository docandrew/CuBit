------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A tar reader for untrusted archives (SELF-003 step 1, archive.unpack):
--  the POSIX ustar interchange format, with pax extended headers and GNU
--  long names, read one 512-byte block at a time.
--
--  @description
--  The format is POSIX.1-2017's (IEEE Std 1003.1-2017, the pax utility's
--  "ustar Interchange Format" and "pax Interchange Format"), plus the two
--  GNU tar header kinds every source tarball uses: the old GNU magic and
--  the 'L' long-name entry (GNU tar manual, "Basic Tar Format").
--
--  The walker turns blocks into events: a directory, the start of a file
--  and its data a block at a time, a skipped special entry, the end of the
--  archive, or a refusal. It does no input or output; the caller reads the
--  archive and writes what the events say, under a place it was granted.
--
--  What it guarantees (proved at level 2, tests/tar-reader):
--  * every path it hands out is Confined: relative, made of non-empty
--    components of printable ASCII without '\', none of them "." or "..",
--    so joined below a directory it never names anything outside it;
--  * a file's data events carry exactly the size its header declared, and
--    a refusal is final (every later event is the same refusal);
--  * the caller's ceilings on entries and bytes hold;
--  * links, devices and FIFOs are refused, or skipped when the caller says
--    so; never followed or created.
--  Everything else it refuses, with a reason: a bad checksum, a non-ustar
--  header, a number it cannot read, sparse or base-256 entries, a pax size
--  override, a truncated archive, or a lone zero block.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;

package Tar_Archives with SPARK_Mode, Pure is
   subtype Byte is Interfaces.Unsigned_8;

   --  Byte counts: sizes, ceilings and totals (a signed 64-bit range, so
   --  the arithmetic is plain integers to the prover).
   type Byte_Count is range 0 .. 2 ** 63 - 1;

   --  The ustar logical record: every header and data block.
   BLOCK_BYTES : constant := 512;
   subtype Block_Offset is Natural range 0 .. BLOCK_BYTES - 1;
   type Block is array (Block_Offset) of Byte;
   subtype Piece_Length is Natural range 0 .. BLOCK_BYTES;

   --  Named ceilings (LIM-001): CuBit's path limit (PATH_MAX parity), and
   --  the records of one pax extended header or GNU long name.
   MAXIMUM_PATH_BYTES : constant := 4_096;
   MAXIMUM_META_BYTES : constant := 16_384;
   subtype Path_Length is Natural range 0 .. MAXIMUM_PATH_BYTES;
   subtype Meta_Length is Natural range 0 .. MAXIMUM_META_BYTES;

   --  A size or checksum field holds at most twelve octal digits.
   MAXIMUM_FIELD_VALUE : constant := 8 ** 12 - 1;
   subtype Field_Value is Byte_Count range 0 .. MAXIMUM_FIELD_VALUE;
   subtype Entry_Bytes is Field_Value;

   --  A path character: printable ASCII, never '\' (a separator elsewhere).
   function Allowed (C : Character) return Boolean is
     (C in ' ' .. '~' and then C /= '\');

   --  Whether the component starting at I is "." or "..".
   function Dot_Component (S : String; I : Positive) return Boolean is
     (S (I) = '.' and then
        (I = S'Last or else S (I + 1) = '/' or else
           (S (I + 1) = '.' and then (I + 1 = S'Last or else S (I + 2) = '/'))))
   with Pre => I in S'Range;

   --  The one rule for every path handed out: relative, non-empty
   --  components of Allowed characters separated by single '/', none "."
   --  or "..". Below a directory it names something inside it.
   function Confined (S : String) return Boolean is
     (S'Length in 1 .. MAXIMUM_PATH_BYTES
      and then S (S'First) /= '/' and then S (S'Last) /= '/'
      and then (for all I in S'Range => Allowed (S (I)))
      and then (for all I in S'First .. S'Last - 1 => not (S (I) = '/' and then S (I + 1) = '/'))
      and then (for all I in S'Range =>
                  (if I = S'First or else S (I - 1) = '/' then not Dot_Component (S, I))));

   --  Entry kinds, by ustar typeflag ('0' or NUL, '7' contiguous, '5',
   --  '1', '2', '3', '4', '6', 'x', 'g') and GNU tar's 'L' and 'K'.
   type Entry_Kind is
     (Regular_File, Directory, Hard_Link, Symbolic_Link, Character_Device,
      Block_Device, FIFO, Pax_Extended, Pax_Global, GNU_Long_Name, GNU_Long_Link,
      Unsupported_Kind);
   subtype Special_Kind is Entry_Kind range Hard_Link .. FIFO;

   --  Why an archive was refused.
   type Refusal is
     (No_Refusal,
      Bad_Checksum,          --  the header's sum does not match
      Not_Ustar,             --  neither the POSIX nor the GNU magic
      Bad_Number,            --  a size or checksum is not octal digits
      Base_256_Number,       --  GNU's binary number form (files of 64 GiB and up)
      Empty_Name,            --  a header (or long name) with no name
      Path_Too_Long,         --  beyond MAXIMUM_PATH_BYTES
      Unsafe_Path,           --  not Confined: absolute, "..", ".", "//", '\' or control bytes
      Special_Entry,         --  a link, device or FIFO, and the caller refuses them
      Unsupported_Entry,     --  a typeflag this reader does not know (sparse, volume, ...)
      Unsupported_Pax,       --  a pax size override or GNU sparse records
      Directory_Data,        --  a directory header that declares data
      Bad_Pax_Record,        --  a pax record that is not "LENGTH KEY=VALUE\n"
      Meta_Too_Large,        --  an extended header or long name beyond MAXIMUM_META_BYTES
      Lone_Zero_Block,       --  one zero block, then more headers
      Truncated,             --  the input ended before the two zero blocks
      Too_Many_Entries,      --  the caller's ceiling on entries
      Too_Many_Bytes);       --  the caller's ceiling on file bytes

   --  The reason's name (native builds discard enumeration images).
   function Name (Why : Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Bad_Checksum => "Bad_Checksum",
         when Not_Ustar => "Not_Ustar",
         when Bad_Number => "Bad_Number",
         when Base_256_Number => "Base_256_Number",
         when Empty_Name => "Empty_Name",
         when Path_Too_Long => "Path_Too_Long",
         when Unsafe_Path => "Unsafe_Path",
         when Special_Entry => "Special_Entry",
         when Unsupported_Entry => "Unsupported_Entry",
         when Unsupported_Pax => "Unsupported_Pax",
         when Directory_Data => "Directory_Data",
         when Bad_Pax_Record => "Bad_Pax_Record",
         when Meta_Too_Large => "Meta_Too_Large",
         when Lone_Zero_Block => "Lone_Zero_Block",
         when Truncated => "Truncated",
         when Too_Many_Entries => "Too_Many_Entries",
         when Too_Many_Bytes => "Too_Many_Bytes");

   type Specials_Policy is (Refuse_Specials, Skip_Specials);

   subtype Entry_Count is Natural;
   type Limits is record
      Maximum_Entries : Entry_Count := 0;
      Maximum_Bytes : Byte_Count := 0;
   end record;

   type Event_Kind is
     (Nothing,            --  a block consumed (metadata, padding after the end)
      Directory_Entry,    --  Path names a directory
      File_Start,         --  Path names a file of Size bytes (its data follows)
      File_Data,          --  Data_Length bytes of the current file: the block's first
      Skipped_Special,    --  Path names a link, device or FIFO the caller skips
      Finished,           --  the two zero blocks: the archive ended well
      Refused);           --  Why says why; final

   type Event is record
      Kind : Event_Kind := Nothing;
      Why : Refusal := No_Refusal;
      Size : Entry_Bytes := 0;
      Data_Length : Piece_Length := 0;
      --  File_Data: the file's last piece (File_Start of an empty file
      --  needs none).
      Last_Piece : Boolean := False;
   end record;

   type Walker is private;

   --  The walker's own consistency (a refusal is recorded with its reason,
   --  the ceilings hold, data is pending exactly while a body is read).
   function Valid (W : Walker) return Boolean;
   function Remaining (W : Walker) return Entry_Bytes;
   function Has_Path (W : Walker) return Boolean;
   function Path (W : Walker) return String
   with Pre => Has_Path (W), Post => Confined (Path'Result);
   --  The raw name of an entry refused as Unsafe_Path, for reports only
   --  (it is not Confined; it may hold any byte).
   function Rejected_Name (W : Walker) return String;
   function Failed (W : Walker) return Boolean;
   function Ended (W : Walker) return Boolean;
   function Failure (W : Walker) return Refusal;
   function Entries (W : Walker) return Entry_Count;
   function File_Bytes (W : Walker) return Byte_Count;
   function Ceilings (W : Walker) return Limits;

   procedure Start (W : out Walker; Ceiling : Limits; Specials : Specials_Policy)
   with Post => Valid (W) and then not Failed (W) and then not Ended (W) and then Entries (W) = 0
     and then File_Bytes (W) = 0 and then Ceilings (W) = Ceiling;

   --  The next block of the archive.
   procedure Feed (W : in out Walker; B : Block; E : out Event)
   with
     Pre => Valid (W),
     Post =>
       Valid (W)
       and then Ceilings (W) = Ceilings (W'Old)
       and then Entries (W) <= Ceilings (W).Maximum_Entries
       and then File_Bytes (W) <= Ceilings (W).Maximum_Bytes
       and then (if Failed (W'Old) then E.Kind = Refused and then Failed (W))
       and then (if Ended (W'Old) and then not Failed (W'Old) then E.Kind = Nothing)
       and then (E.Kind = Refused) = Failed (W)
       and then (if E.Kind = Refused then E.Why = Failure (W) and then E.Why /= No_Refusal)
       and then (if E.Kind = Finished then Ended (W))
       and then (if E.Kind in Directory_Entry | File_Start | Skipped_Special then Has_Path (W))
       and then (if E.Kind = File_Start then Remaining (W) = E.Size)
       and then (if E.Kind = File_Data then
                   E.Data_Length in 1 .. BLOCK_BYTES
                   and then Remaining (W'Old) >= Byte_Count (E.Data_Length)
                   and then Remaining (W) = Remaining (W'Old) - Byte_Count (E.Data_Length)
                   and then E.Last_Piece = (Remaining (W) = 0));

   --  The input ended: Finished if the archive did, else a refusal
   --  (Truncated) unless one was given already.
   procedure Finish (W : in out Walker; E : out Event)
   with Pre => Valid (W),
        Post => Valid (W) and then (if Ended (W'Old) and then not Failed (W'Old) then E.Kind = Finished
                 else E.Kind = Refused and then Failed (W) and then E.Why = Failure (W)
                      and then E.Why /= No_Refusal);

   --  Header pieces, exposed for the hosted tests.
   function Checksum_Matches (B : Block) return Boolean;
   procedure Read_Octal
     (B : Block; First : Block_Offset; Width : Positive; Value : out Field_Value; Why : out Refusal)
   with Always_Terminates, Pre => Width <= 12 and then First <= BLOCK_BYTES - Width;
private
   type Phase_Kind is (Expect_Header, In_Data, In_Skip, In_Meta, Ended_Phase, Failed_Phase);
   type Meta_Kind is (Pax_Meta, Long_Name_Meta);

   subtype Path_Buffer is String (1 .. MAXIMUM_PATH_BYTES);
   subtype Meta_Buffer is String (1 .. MAXIMUM_META_BYTES);

   type Walker is record
      Phase : Phase_Kind := Expect_Header;
      Why : Refusal := No_Refusal;
      Specials : Specials_Policy := Refuse_Specials;
      Ceiling : Limits;
      Entries : Entry_Count := 0;
      Bytes : Byte_Count := 0;
      Zero_Blocks : Natural range 0 .. 1 := 0;
      Remaining : Entry_Bytes := 0;
      --  The path handed out with the last entry event.
      Current : Path_Buffer := [others => ' '];
      Current_Length : Path_Length := 0;
      Rejected : Path_Buffer := [others => ' '];
      Rejected_Length : Path_Length := 0;
      --  A path from a pax record or long name, for the next entry.
      Pending : Path_Buffer := [others => ' '];
      Pending_Length : Path_Length := 0;
      --  The extended header or long name being collected.
      Meta : Meta_Buffer := [others => ' '];
      Meta_Length : Tar_Archives.Meta_Length := 0;
      Meta_Kind : Tar_Archives.Meta_Kind := Pax_Meta;
   end record;

   function Valid (W : Walker) return Boolean is
     ((W.Phase = Failed_Phase) = (W.Why /= No_Refusal)
      and then W.Entries <= W.Ceiling.Maximum_Entries
      and then W.Bytes <= W.Ceiling.Maximum_Bytes
      and then (W.Phase in In_Data | In_Skip | In_Meta) = (W.Remaining > 0)
      and then (if W.Phase = In_Meta then
                  Byte_Count (W.Meta_Length) + W.Remaining <= MAXIMUM_META_BYTES));
   function Remaining (W : Walker) return Entry_Bytes is (W.Remaining);
   function Has_Path (W : Walker) return Boolean is
     (W.Current_Length >= 1 and then Confined (W.Current (1 .. W.Current_Length)));
   function Path (W : Walker) return String is (W.Current (1 .. W.Current_Length));
   function Rejected_Name (W : Walker) return String is (W.Rejected (1 .. W.Rejected_Length));
   function Failed (W : Walker) return Boolean is (W.Phase = Failed_Phase);
   function Ended (W : Walker) return Boolean is (W.Phase = Ended_Phase);
   function Failure (W : Walker) return Refusal is (W.Why);
   function Entries (W : Walker) return Entry_Count is (W.Entries);
   function File_Bytes (W : Walker) return Byte_Count is (W.Bytes);
   function Ceilings (W : Walker) return Limits is (W.Ceiling);
end Tar_Archives;
