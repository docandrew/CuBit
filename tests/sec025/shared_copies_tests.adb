--  SEC-025: decoders read peer-writable memory only through one private
--  copy (CuBit.Shared_Copies). Each case below gives the copy a hostile
--  read that rewrites a word right after its first read, as a peer racing
--  the decoder would, and checks that every element is read exactly once
--  and that the decode is of the first-read values (deterministic: the
--  change is made by the read itself).
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Shared_Copies;
with ISO_Records;
with Partition_Tables;
procedure Shared_Copies_Tests is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Ada.Text_IO.Put_Line ("SEC-025-COPIES: FAIL " & What);
         raise Program_Error;
      end if;
   end Check;

   procedure Put32 (Into : in out ISO_Records.Bytes; At_Offset : Natural; Value : Unsigned_32) is
   begin
      for I in 0 .. 3 loop
         Into (At_Offset + I) := Unsigned_8 (Shift_Right (Value, 8 * I) and 16#FF#);
         Into (At_Offset + 7 - I) := Unsigned_8 (Shift_Right (Value, 8 * I) and 16#FF#);
      end loop;
   end Put32;
begin
   --  ISO9660 directory record (filesystem iso9660): the extent word turns
   --  to 16#7FFF_FFFF# after its first read; a record whose extent lies far
   --  past the volume must never be decoded from the copy.
   declare
      package Copies is new CuBit.Shared_Copies (Natural, Unsigned_8, ISO_Records.Bytes);
      Shared : ISO_Records.Sector := [others => 0];
      Reads : array (ISO_Records.Sector'Range) of Natural := [others => 0];
      --  The byte after which the peer rewrites the extent.
      Flip_After : Natural := 9;
      function Hostile (I : Natural) return Unsigned_8 is
         Value : constant Unsigned_8 := Shared (I);
      begin
         Reads (I) := Reads (I) + 1;
         if I = Flip_After then Put32 (Shared, 2, 16#7FFF_FFFF#); end if;
         return Value;
      end Hostile;
      procedure Copy is new Copies.Copy_With (Hostile);
      Data : ISO_Records.Sector;
      Item : ISO_Records.File_Record;
      Consumed : Natural;
      Valid : Boolean;
   begin
      Shared (0) := 34;                 --  record length
      Put32 (Shared, 2, 40);            --  extent
      Put32 (Shared, 10, 2048);         --  bytes
      Shared (25) := 0;                 --  a file
      Shared (28 .. 31) := [1, 0, 0, 1];
      Shared (32) := 1;                 --  name length
      Shared (33) := Character'Pos ('A');
      Copy (Data);
      Check ((for all N of Reads => N = 1), "iso: each sector byte read once");
      ISO_Records.Decode_Record (Data, 0, 100, Item, Consumed, Valid);
      Check (Valid and then Item.Extent = 40 and then Item.Bytes = 2048,
             "iso: the record decoded is the one first read, inside the volume");
      Copy (Data);
      ISO_Records.Decode_Record (Data, 0, 100, Item, Consumed, Valid);
      Check (not Valid, "iso: the rewritten record (extent past the volume) is refused");
      --  Torn: rewritten in the middle of the copy. The copy mixes old and
      --  new bytes, but it is one private value: its checks refuse it.
      Put32 (Shared, 2, 40); Flip_After := 3; Reads := [others => 0];
      Copy (Data);
      ISO_Records.Decode_Record (Data, 0, 100, Item, Consumed, Valid);
      Check ((for all N of Reads => N = 1) and then not Valid,
             "iso: a copy torn by the peer is refused, never half-trusted");
   end;

   --  GPT entries (filesystem fs_partitions): an entry's first LBA changes
   --  after its first read; the checksum and the decode see the same copy.
   declare
      use Partition_Tables;
      package Copies is new CuBit.Shared_Copies (Positive, Unsigned_8, Bytes);
      Shared : Bytes (1 .. 256) := [others => 0];
      Reads : array (Shared'Range) of Natural := [others => 0];
      First_At : constant := 33;  --  entry 0's first LBA (offset 32)
      function Hostile (I : Positive) return Unsigned_8 is
         Value : constant Unsigned_8 := Shared (I);
      begin
         Reads (I) := Reads (I) + 1;
         if I = First_At then Shared (First_At) := 16#FF#; end if;
         return Value;
      end Hostile;
      procedure Copy is new Copies.Copy_With (Hostile);
      Entries : Bytes (1 .. 256);
      Expected : Unsigned_32;
   begin
      Shared (First_At) := 34;
      Expected := CRC32 (Shared);
      Copy (Entries);
      Check ((for all N of Reads => N = 1), "gpt: each entry byte read once");
      Check (CRC32 (Entries) = Expected and then Decode_Entry (Entries, 0).First = 34,
             "gpt: the checksummed copy is the decoded copy");
   end;

   --  A client's network target (netstack parseNetScheme): a character
   --  changes after its first read; the parser sees one consistent text.
   declare
      package Copies is new CuBit.Shared_Copies (Positive, Character, String);
      Shared : String := "tcp:example.org:80";
      Reads : array (Shared'Range) of Natural := [others => 0];
      function Hostile (I : Positive) return Character is
         Value : constant Character := Shared (I);
      begin
         Reads (I) := Reads (I) + 1;
         if I = 5 then Shared (5) := '/'; end if;
         return Value;
      end Hostile;
      procedure Copy is new Copies.Copy_With (Hostile);
      Text : String (Shared'Range);
   begin
      Copy (Text);
      Check ((for all N of Reads => N = 1) and then Text = "tcp:example.org:80",
             "net: each target character read once, the copy is the first text");
   end;

   --  Copy_At: the production path, a volatile view at an address.
   declare
      package Copies is new CuBit.Shared_Copies (Positive, Unsigned_8, Partition_Tables.Bytes);
      Source : Partition_Tables.Bytes (1 .. 64) := [for I in 1 .. 64 => Unsigned_8 (I)];
      Target : Partition_Tables.Bytes (1 .. 64);
   begin
      Copies.Copy_At (Source'Address, Target);
      Check (Partition_Tables."=" (Target, Source), "copy_at copies the bytes at an address");
   end;
   Ada.Text_IO.Put_Line ("SEC-025-COPIES: PASS checks=" & Natural'Image (Checks));
end Shared_Copies_Tests;
