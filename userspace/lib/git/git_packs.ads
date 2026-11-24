------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  A packfile reader for untrusted packs (SELF-003 step 2): the pack a
--  fetch answers with, read in pieces of any size, each entry's data
--  inflated into the caller's buffer.
--
--  @description
--  git's documentation, gitformat-pack ("pack-*.pack files have the
--  following format"): "PACK", a version (2 or 3) and an object count,
--  each a 4-byte network-order number; then the entries; then the hash of
--  everything before it (SHA-1 or SHA-256: the repository's object format). An entry starts with its type (commit, tree,
--  blob, tag, OFS_DELTA, REF_DELTA) and inflated size as a little-endian
--  base-128 number; an OFS_DELTA names its base by a negative offset
--  (base-128, most significant group first, each continuation adding
--  one), a REF_DELTA by the base's object name (20 or 32 bytes); then a zlib
--  stream (Inflate_Streams) of exactly the declared size.
--
--  The reader checks the framing, the sizes and the trailer; it does not
--  resolve deltas or name objects (Git_Deltas, Git_Objects do, from
--  bytes the caller keeps). For each entry it gives the offset, the
--  CRC-32 of the entry's bytes (what a pack index records) and, at
--  Pack_End, the pack's checksum (its name).
--
--  Proved at level 2 (tests/git-codecs): no run-time error; the caller's
--  ceilings on objects, object size and pack size hold; an entry ends
--  only with exactly its declared size inflated; an OFS_DELTA's base
--  lies before it, inside the pack; output goes only to Output
--  (Out_Next'Old .. Out_Next - 1); Pack_End only after every entry and a
--  matching trailer; a refusal is final.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with Inflate_Streams; use Inflate_Streams;
with Git_Protocol;
with Git_Hashes;

package Git_Packs with SPARK_Mode is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_32;
   use type Git_Protocol.Object_Format;

   type Object_Kind is (Commit_Object, Tree_Object, Blob_Object, Tag_Object, Ofs_Delta, Ref_Delta);
   subtype Whole_Kind is Object_Kind range Commit_Object .. Tag_Object;
   subtype Delta_Kind is Object_Kind range Ofs_Delta .. Ref_Delta;

   --  Pack sizes and offsets. A pack's ceiling is at most 2 ** 61 bytes.
   MAXIMUM_PACK_CEILING : constant := 2 ** 61;
   subtype Pack_Offset is Byte_Count range 0 .. MAXIMUM_PACK_CEILING;
   --  The header: signature, version, count.
   HEADER_BYTES : constant := 12;
   subtype Object_Count is Natural;

   type Limits is record
      Maximum_Objects : Object_Count := 0;
      Maximum_Object_Bytes : Byte_Count := 0;   --  an entry's inflated size
      Maximum_Pack_Bytes : Pack_Offset := 0;
   end record;

   type Refusal is
     (No_Refusal,
      Bad_Signature,      --  not "PACK"
      Bad_Version,        --  not version 2 or 3
      Too_Many_Objects,   --  past the caller's ceiling
      Bad_Object_Type,    --  type 0 or 5
      Bad_Size,           --  a size too large to represent
      Object_Too_Large,   --  past the caller's ceiling
      Bad_Delta_Offset,   --  an OFS_DELTA base not before the entry
      Bad_Compression,    --  the zlib stream was refused (Compression_Failure says why)
      Size_Mismatch,      --  the stream does not inflate to the declared size
      Bad_Checksum,       --  the trailer is not the hash of the pack
      Pack_Too_Large,     --  past the caller's ceiling
      Truncated);         --  the input ended inside the pack

   function Name (Why : Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Bad_Signature => "Bad_Signature",
         when Bad_Version => "Bad_Version",
         when Too_Many_Objects => "Too_Many_Objects",
         when Bad_Object_Type => "Bad_Object_Type",
         when Bad_Size => "Bad_Size",
         when Object_Too_Large => "Object_Too_Large",
         when Bad_Delta_Offset => "Bad_Delta_Offset",
         when Bad_Compression => "Bad_Compression",
         when Size_Mismatch => "Size_Mismatch",
         when Bad_Checksum => "Bad_Checksum",
         when Pack_Too_Large => "Pack_Too_Large",
         when Truncated => "Truncated");

   type Event_Kind is
     (Need_Input,    --  every input byte was taken (output may have been written)
      Output_Full,   --  the output buffer is full (the entry's data continues)
      Entry_Start,   --  an entry's header: Kind, Size, Entry_Offset, the base
      Entry_End,     --  the entry's data is complete: Entry_CRC
      Pack_End,      --  the trailer matched: Checksum
      Refused);      --  Failure says why; final

   type Pack_Reader is private;

   function Valid (P : Pack_Reader) return Boolean;
   function Failed (P : Pack_Reader) return Boolean;
   function Ended (P : Pack_Reader) return Boolean;
   function Failure (P : Pack_Reader) return Refusal;
   function Compression_Failure (P : Pack_Reader) return Inflate_Streams.Refusal;
   function Ceilings (P : Pack_Reader) return Limits;
   function Format (P : Pack_Reader) return Git_Protocol.Object_Format;
   --  Bytes of pack read so far.
   function Offset (P : Pack_Reader) return Pack_Offset;
   --  The header's count (once read), and the entries finished.
   function Count (P : Pack_Reader) return Object_Count;
   function Entries_Done (P : Pack_Reader) return Object_Count;
   --  The current (or last) entry.
   function In_Entry_Data (P : Pack_Reader) return Boolean;
   function Kind (P : Pack_Reader) return Object_Kind;
   function Size (P : Pack_Reader) return Byte_Count;
   function Entry_Offset (P : Pack_Reader) return Pack_Offset;
   function Base_Offset (P : Pack_Reader) return Pack_Offset;
   function Base_Id (P : Pack_Reader) return Git_Protocol.Object_Id;
   --  Bytes of the entry's data delivered so far.
   function Delivered (P : Pack_Reader) return Byte_Count;
   function Entry_CRC (P : Pack_Reader) return Interfaces.Unsigned_32;
   function Checksum (P : Pack_Reader) return Git_Protocol.Object_Id;

   procedure Start (P : out Pack_Reader; Ceiling : Limits; Format : Git_Protocol.Object_Format)
   with Post => Valid (P) and then not Failed (P) and then not Ended (P) and then Offset (P) = 0
                  and then Entries_Done (P) = 0 and then Ceilings (P) = Ceiling
                  and then Git_Packs.Format (P) = Format;

   procedure Step
     (P : in out Pack_Reader;
      Input : Byte_Array;
      In_Next : in out Positive;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      E : out Event_Kind)
   with
     Pre =>
       Valid (P)
       and then Input'Last < Positive'Last - 128 and then Output'Last < Positive'Last
       and then In_Next in Input'First .. Input'Last + 1
       and then Out_Next in Output'First .. Output'Last + 1,
     Post =>
       Valid (P)
       and then Ceilings (P) = Ceilings (P'Old) and then Format (P) = Format (P'Old)
       and then In_Next in In_Next'Old .. Input'Last + 1
       and then Out_Next in Out_Next'Old .. Output'Last + 1
       and then Offset (P) = Offset (P'Old) + Byte_Count (In_Next - In_Next'Old)
       and then Offset (P) <= Ceilings (P).Maximum_Pack_Bytes
       and then Entries_Done (P) <= Count (P) and then Count (P) <= Ceilings (P).Maximum_Objects
       and then (for all I in Output'Range =>
                   (if I < Out_Next'Old or else I >= Out_Next then Output (I) = Output'Old (I)))
       and then (if Out_Next /= Out_Next'Old then In_Entry_Data (P'Old)
                   and then Delivered (P) = Delivered (P'Old) + Byte_Count (Out_Next - Out_Next'Old))
       and then (if Failed (P'Old) then E = Refused and then In_Next = In_Next'Old)
       and then (if Ended (P'Old) then E = Pack_End and then In_Next = In_Next'Old)
       and then (E = Refused) = Failed (P)
       and then (if E = Refused then Failure (P) /= No_Refusal)
       and then (E = Pack_End) = Ended (P)
       and then (if E = Need_Input then In_Next = Input'Last + 1)
       and then (if E = Output_Full then Out_Next = Output'Last + 1 and then In_Entry_Data (P))
       and then (if E = Entry_Start then
                   Size (P) <= Ceilings (P).Maximum_Object_Bytes and then Delivered (P) = 0
                   and then (if Kind (P) = Ofs_Delta then
                               Base_Offset (P) >= HEADER_BYTES and then Base_Offset (P) < Entry_Offset (P)))
       and then (if E = Entry_End then Delivered (P) = Size (P) and then Entries_Done (P) >= 1)
       and then (if E = Pack_End then Entries_Done (P) = Count (P));

   --  The input ended: Truncated unless the pack did.
   procedure Input_Ended (P : in out Pack_Reader; E : out Event_Kind)
   with Pre => Valid (P),
        Post => Valid (P) and then E in Pack_End | Refused and then (E = Pack_End) = Ended (P'Old)
          and then (E = Refused) = Failed (P);
private
   type Phase_Kind is
     (Pack_Header, Entry_Header, Ofs_Offset, Ref_Base, Entry_Data, Trailer, Done_Phase, Failed_Phase);

   --  A size's base-128 groups: the last group starts at bit 53 at most.
   MAXIMUM_SHIFT : constant := 53;

   type Pack_Reader is record
      Phase : Phase_Kind := Pack_Header;
      Why : Refusal := No_Refusal;
      Inner : Inflate_Streams.Refusal := Inflate_Streams.No_Refusal;
      Ceiling : Limits;
      Format : Git_Protocol.Object_Format := Git_Protocol.Sha1_Format;
      Offset : Pack_Offset := 0;
      Header : Byte_Array (1 .. HEADER_BYTES) := [others => 0];
      Header_Read : Natural range 0 .. HEADER_BYTES := 0;
      Count : Object_Count := 0;
      Done_Entries : Object_Count := 0;
      Kind : Object_Kind := Blob_Object;
      Size : Byte_Count := 0;
      --  An entry's size while its groups are read, and the next group's
      --  position.
      Pending : Interfaces.Unsigned_64 := 0;
      Shift : Natural range 0 .. MAXIMUM_SHIFT + 7 := 0;
      Header_Started : Boolean := False;
      Entry_At : Pack_Offset := 0;
      --  An OFS_DELTA's distance while it is read.
      Distance : Byte_Count := 0;
      Base_At : Pack_Offset := 0;
      Base : Git_Protocol.Object_Id := [others => 0];
      Base_Read : Natural range 0 .. Git_Protocol.MAXIMUM_ID_BYTES := 0;
      Z : Inflate_Streams.Inflater;
      Delivered : Byte_Count := 0;
      --  The running CRC-32 register (the CRC is its complement).
      CRC : Interfaces.Unsigned_32 := 0;
      Hash : Git_Hashes.Context;
      Sum : Git_Protocol.Object_Id := [others => 0];
      Trailer : Git_Protocol.Object_Id := [others => 0];
      Trailer_Read : Natural range 0 .. Git_Protocol.MAXIMUM_ID_BYTES := 0;
   end record;

   function Valid (P : Pack_Reader) return Boolean is
     ((P.Phase = Failed_Phase) = (P.Why /= No_Refusal)
      and then P.Offset <= P.Ceiling.Maximum_Pack_Bytes
      and then P.Done_Entries <= P.Count and then P.Count <= P.Ceiling.Maximum_Objects
      and then P.Size <= P.Ceiling.Maximum_Object_Bytes
      and then P.Entry_At <= P.Offset
      and then Inflate_Streams.Valid (P.Z)
      and then Inflate_Streams.Total_In (P.Z) <= P.Offset
      and then (if P.Phase in Entry_Header | Ofs_Offset | Ref_Base | Entry_Data then P.Done_Entries < P.Count)
      and then (if P.Phase = Pack_Header then P.Header_Read < HEADER_BYTES and then P.Done_Entries = 0)
      and then Git_Hashes.Format (P.Hash) = P.Format
      and then (if P.Phase = Ref_Base then P.Base_Read < Git_Protocol.Id_Bytes (P.Format))
      and then (if P.Phase = Trailer then P.Trailer_Read < Git_Protocol.Id_Bytes (P.Format))
      and then (if P.Phase in Trailer | Done_Phase then P.Done_Entries = P.Count)
      and then (if P.Phase = Entry_Data then
                  Inflate_Streams.Format (P.Z) = Zlib_Stream and then Inflate_Streams.Ceiling (P.Z) = P.Size
                  and then P.Delivered = Inflate_Streams.Total_Out (P.Z))
      and then P.Delivered <= P.Size
      and then (if P.Kind = Ofs_Delta and then P.Phase in Ref_Base | Entry_Data then
                  P.Base_At >= HEADER_BYTES and then P.Base_At < P.Entry_At));

   function Failed (P : Pack_Reader) return Boolean is (P.Phase = Failed_Phase);
   function Ended (P : Pack_Reader) return Boolean is (P.Phase = Done_Phase);
   function Failure (P : Pack_Reader) return Refusal is (P.Why);
   function Compression_Failure (P : Pack_Reader) return Inflate_Streams.Refusal is (P.Inner);
   function Ceilings (P : Pack_Reader) return Limits is (P.Ceiling);
   function Format (P : Pack_Reader) return Git_Protocol.Object_Format is (P.Format);
   function Offset (P : Pack_Reader) return Pack_Offset is (P.Offset);
   function Count (P : Pack_Reader) return Object_Count is (P.Count);
   function Entries_Done (P : Pack_Reader) return Object_Count is (P.Done_Entries);
   function In_Entry_Data (P : Pack_Reader) return Boolean is (P.Phase = Entry_Data);
   function Kind (P : Pack_Reader) return Object_Kind is (P.Kind);
   function Size (P : Pack_Reader) return Byte_Count is (P.Size);
   function Entry_Offset (P : Pack_Reader) return Pack_Offset is (P.Entry_At);
   function Base_Offset (P : Pack_Reader) return Pack_Offset is (P.Base_At);
   function Base_Id (P : Pack_Reader) return Git_Protocol.Object_Id is (P.Base);
   function Delivered (P : Pack_Reader) return Byte_Count is (P.Delivered);
   function Entry_CRC (P : Pack_Reader) return Interfaces.Unsigned_32 is (not P.CRC);
   function Checksum (P : Pack_Reader) return Git_Protocol.Object_Id is (P.Sum);
end Git_Packs;
