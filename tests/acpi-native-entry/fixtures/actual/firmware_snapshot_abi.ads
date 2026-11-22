------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The FIRMWARE_SNAPSHOT system call: read-only export of the kernel's
--  sealed boot snapshot of the firmware (ACPI) tables. Shared by the kernel
--  (kernel/src/firmware_snapshot_export.ad?) and the runtime
--  (CuBit.Firmware_Snapshot, the only caller).
--
--  @description
--  The kernel copies every admitted firmware table once at boot into an
--  immutable snapshot (Firmware_Tables.Snapshots) and seals it. This call
--  reads that snapshot and nothing else: no physical address, no mapping,
--  no live firmware memory. FACS, NVS and the ACPI registers are not in it.
--
--  Authority: a CAP_FIRMWARE_SNAPSHOT capability with RIGHT_READ, named by
--  handle. The kernel mints the only root at boot (devmgr's launch roots);
--  devmgr passes it to procmgr, which installs a read-only copy only in a
--  program whose manifest declares (Request.Firmware_Snapshot "<binding>")
--  and whose startup entry approves it, (firmware-snapshot approve-declared).
--
--  Arguments (one word each):
--     arg0  the authority handle
--     arg1  Operation
--     arg2  Table: a logical table number, 1 .. table count (Describe, Copy)
--     arg3  Offset: a byte offset into that table (Copy)
--     arg4  Destination: the caller's buffer (Copy)
--     arg5  Capacity: the buffer's length, 0 .. Chunk_Max (Copy)
--     arg6  the caller's Reply record (Reply_Words words)
--  Unused arguments must be zero. The result word is an Outcome code
--  (never zero: an unknown call returns zero, which is no Outcome).
--
--  Order of checks: the authority against the calling process first, then
--  the request words at full width (Well_Formed), then the snapshot (ready,
--  table known, offset within the table), and only then any copy.
--
--  Copy_Table_Part copies Capacity bytes into Destination: the table's
--  bytes from Offset, then zeros. Copied (in the reply) counts table bytes.
--  At the end of the table (Offset = extent) it is Ok with Copied = 0;
--  past the end is Beyond_End; Capacity 0 before the end is
--  Empty_Before_End. Copy_Failed means the caller's memory refused a write,
--  possibly after a prefix: discard the whole destination. The reply is
--  written only for Ok; every reply word a layout leaves unused is zero.
--  There is no per-table limit here: a table's extent is its descriptor's.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Firmware_Snapshot_ABI with Pure, SPARK_Mode is

   System_Call_Number : constant := 144;
   Interface_Version  : constant := 1;

   --  Bytes one Copy_Table_Part copies at most: a bound on the work of one
   --  call, not on a table's size. The caller loops.
   Chunk_Max : constant := 4_096;

   ---------------------------------------------------------------------------
   --  Operations and outcomes (wire codes are fixed; a new one takes a new
   --  code)
   ---------------------------------------------------------------------------
   type Operation is (Catalog_Info, Describe_Table, Copy_Table_Part);
   for Operation use (Catalog_Info => 1, Describe_Table => 2, Copy_Table_Part => 3);

   type Outcome is
     (Ok, Not_Ready, Unknown_Table, Beyond_End, Empty_Before_End, Copy_Failed,
      Not_Authorized, Malformed);
   for Outcome use
     (Ok => 1, Not_Ready => 2, Unknown_Table => 3, Beyond_End => 4,
      Empty_Before_End => 5, Copy_Failed => 6, Not_Authorized => 7,
      Malformed => 8);

   function Outcome_Word (Item : Outcome) return Unsigned_64 is
     (Unsigned_64 (Outcome'Enum_Rep (Item)));

   function Is_Outcome_Word (Word : Unsigned_64) return Boolean is
     (Word in Outcome_Word (Outcome'First) .. Outcome_Word (Outcome'Last));

   --  The outcome a result word reports; Malformed for a word that is none
   --  (zero is what a kernel without this call returns).
   function To_Outcome (Word : Unsigned_64) return Outcome
   with Post => (if Is_Outcome_Word (Word) then Outcome_Word (To_Outcome'Result) = Word
                 else To_Outcome'Result = Malformed);

   ---------------------------------------------------------------------------
   --  Requests
   ---------------------------------------------------------------------------
   subtype Table_Number is Unsigned_32;          --  0 is never a table
   subtype Capacity_Bytes is Unsigned_64 range 0 .. Chunk_Max;

   --  arg1 .. arg5 as the caller passed them.
   type Request_Words is record
      Op, Table, Offset, Destination, Capacity : Unsigned_64 := 0;
   end record;

   type Request is record
      Op          : Operation := Catalog_Info;
      Table       : Table_Number := 0;
      Offset      : Unsigned_64 := 0;
      Destination : Unsigned_64 := 0;
      Capacity    : Capacity_Bytes := 0;
   end record;

   --  Every field an operation does not use is zero, and a destination
   --  range does not wrap.
   function Valid_Request (Item : Request) return Boolean is
     (case Item.Op is
         when Catalog_Info =>
            Item.Table = 0 and then Item.Offset = 0
            and then Item.Destination = 0 and then Item.Capacity = 0,
         when Describe_Table =>
            Item.Offset = 0 and then Item.Destination = 0 and then Item.Capacity = 0,
         when Copy_Table_Part =>
            Item.Destination <= Unsigned_64'Last - Unsigned_64 (Item.Capacity));

   function Known_Operation (Word : Unsigned_64) return Boolean is
     (Word = Operation'Enum_Rep (Catalog_Info)
      or else Word = Operation'Enum_Rep (Describe_Table)
      or else Word = Operation'Enum_Rep (Copy_Table_Part));

   function Operation_Of (Word : Unsigned_64) return Operation is
     (if Word = Operation'Enum_Rep (Catalog_Info) then Catalog_Info
      elsif Word = Operation'Enum_Rep (Describe_Table) then Describe_Table
      else Copy_Table_Part)
   with Pre  => Known_Operation (Word),
        Post => Operation'Enum_Rep (Operation_Of'Result) = Word;

   --  The words name a known operation, every word is in its range at full
   --  width, every word the operation does not use is zero, and the
   --  destination range does not wrap.
   function Well_Formed (Words : Request_Words) return Boolean is
     (Known_Operation (Words.Op)
      and then Words.Table <= Unsigned_64 (Table_Number'Last)
      and then Words.Capacity <= Chunk_Max
      and then
        (case Operation_Of (Words.Op) is
            when Catalog_Info =>
               Words.Table = 0 and then Words.Offset = 0
               and then Words.Destination = 0 and then Words.Capacity = 0,
            when Describe_Table =>
               Words.Offset = 0 and then Words.Destination = 0 and then Words.Capacity = 0,
            when Copy_Table_Part =>
               Words.Destination <= Unsigned_64'Last - Words.Capacity));

   function Encode (Item : Request) return Request_Words is
     ((Op          => Operation'Enum_Rep (Item.Op),
       Table       => Unsigned_64 (Item.Table),
       Offset      => Item.Offset,
       Destination => Item.Destination,
       Capacity    => Unsigned_64 (Item.Capacity)));

   --  The request well-formed words name (narrowed only after Well_Formed).
   function Decoded (Words : Request_Words) return Request is
     ((Op          => Operation_Of (Words.Op),
       Table       => Table_Number (Words.Table),
       Offset      => Words.Offset,
       Destination => Words.Destination,
       Capacity    => Capacity_Bytes (Words.Capacity)))
   with Pre => Well_Formed (Words);

   --  Total decoding: Valid exactly for well-formed words, and then the
   --  request is valid and encodes back to the same words.
   procedure Decode (Words : Request_Words; Item : out Request; Valid : out Boolean)
   with Post => Valid = Well_Formed (Words)
                and then (if Valid then Item = Decoded (Words)
                            and then Valid_Request (Item)
                            and then Encode (Item) = Words
                          else Item = Request'(others => <>));

   --  Encoding a valid request gives well-formed words that decode to it.
   procedure Lemma_Encode_Decodes (Item : Request)
   with Ghost, Global => null,
        Pre  => Valid_Request (Item),
        Post => Well_Formed (Encode (Item)) and then Decoded (Encode (Item)) = Item;

   ---------------------------------------------------------------------------
   --  Copy plan: the outcome and table byte count of Copy_Table_Part from a
   --  table's extent, before anything is copied.
   ---------------------------------------------------------------------------
   type Copy_Plan is record
      Status : Outcome := Malformed;
      Copied : Capacity_Bytes := 0;
   end record;

   function Plan_Copy
     (Extent, Offset : Unsigned_64; Capacity : Capacity_Bytes) return Copy_Plan
   with Post =>
     Plan_Copy'Result.Status in Ok | Beyond_End | Empty_Before_End
     and then
       (if Offset > Extent then Plan_Copy'Result = (Beyond_End, 0)
        elsif Capacity = 0 and then Offset < Extent then
           Plan_Copy'Result = (Empty_Before_End, 0)
        else Plan_Copy'Result.Status = Ok
           and then Unsigned_64 (Plan_Copy'Result.Copied) =
                    Unsigned_64'Min (Unsigned_64 (Capacity), Extent - Offset)
           and then (Plan_Copy'Result.Copied > 0 or else Offset = Extent));

   ---------------------------------------------------------------------------
   --  Replies: Reply_Words little-endian words, written only for Ok
   ---------------------------------------------------------------------------
   Reply_Words : constant := 6;
   Reply_Bytes : constant := Reply_Words * 8;
   type Reply_Index is range 1 .. Reply_Words;
   type Reply is array (Reply_Index) of Unsigned_64;
   Empty_Reply : constant Reply := [others => 0];

   --  Catalog_Info: words 1 .. 6.
   Info_Version  : constant Reply_Index := 1;
   Info_Ready    : constant Reply_Index := 2;   --  0 or 1
   Info_Count    : constant Reply_Index := 3;   --  tables, 0 unless ready
   Info_Largest  : constant Reply_Index := 4;   --  largest extent, bytes
   Info_Total    : constant Reply_Index := 5;   --  sum of extents, bytes
   Info_Chunk    : constant Reply_Index := 6;   --  Chunk_Max

   --  Describe_Table: words 1 .. 4, then zeros.
   Describe_Table_Word : constant Reply_Index := 1;  --  the table asked for
   Describe_Signature  : constant Reply_Index := 2;  --  4 bytes, first lowest
   Describe_Revision   : constant Reply_Index := 3;
   Describe_Extent     : constant Reply_Index := 4;  --  bytes
   Describe_Used       : constant Reply_Index := Describe_Extent;

   --  Copy_Table_Part: word 1, then zeros.
   Copy_Copied : constant Reply_Index := 1;
   Copy_Used   : constant Reply_Index := Copy_Copied;

   Signature_Length : constant := 4;
   subtype Signature_Index is Positive range 1 .. Signature_Length;
   type Signature_Bytes is array (Signature_Index) of Unsigned_8;

   type Catalog is record
      Ready       : Boolean := False;
      Table_Count : Unsigned_32 := 0;
      Largest     : Unsigned_64 := 0;
      Total       : Unsigned_64 := 0;
   end record;

   --  What a kernel reports: an unready snapshot has no inventory, and no
   --  table is larger than all of them together.
   function Valid_Catalog (Item : Catalog) return Boolean is
     ((if not Item.Ready then Item.Table_Count = 0 and then Item.Largest = 0
         and then Item.Total = 0)
      and then Item.Largest <= Item.Total);

   type Description is record
      Table     : Table_Number := 0;
      Signature : Signature_Bytes := [others => 0];
      Revision  : Unsigned_8 := 0;
      Extent    : Unsigned_64 := 0;
   end record;

   --  Byte I of a signature is bits 8 * (I - 1) .. 8 * I - 1 of the word.
   Byte_Bits : constant := 8;
   Byte_Mask : constant Unsigned_64 := 2 ** Byte_Bits - 1;
   function Signature_Word (Item : Signature_Bytes) return Unsigned_64 is
     (Unsigned_64 (Item (1)) or Shift_Left (Unsigned_64 (Item (2)), Byte_Bits)
      or Shift_Left (Unsigned_64 (Item (3)), 2 * Byte_Bits)
      or Shift_Left (Unsigned_64 (Item (4)), 3 * Byte_Bits));

   function Signature_Of (Word : Unsigned_64) return Signature_Bytes is
     ([Unsigned_8 (Word and Byte_Mask),
       Unsigned_8 (Shift_Right (Word, Byte_Bits) and Byte_Mask),
       Unsigned_8 (Shift_Right (Word, 2 * Byte_Bits) and Byte_Mask),
       Unsigned_8 (Shift_Right (Word, 3 * Byte_Bits) and Byte_Mask)]);

   function Encode_Info (Item : Catalog) return Reply is
     ([Info_Version => Interface_Version,
       Info_Ready   => (if Item.Ready then 1 else 0),
       Info_Count   => Unsigned_64 (Item.Table_Count),
       Info_Largest => Item.Largest,
       Info_Total   => Item.Total,
       Info_Chunk   => Chunk_Max]);

   function Encode_Description (Item : Description) return Reply is
     ([Describe_Table_Word => Unsigned_64 (Item.Table),
       Describe_Signature  => Signature_Word (Item.Signature),
       Describe_Revision   => Unsigned_64 (Item.Revision),
       Describe_Extent     => Item.Extent,
       others              => 0]);

   function Encode_Copy (Copied : Capacity_Bytes) return Reply is
     ([Copy_Copied => Unsigned_64 (Copied), others => 0]);

   function Info_Well_Formed (Words : Reply) return Boolean is
     (Words (Info_Version) = Interface_Version
      and then Words (Info_Ready) <= 1
      and then Words (Info_Count) <= Unsigned_64 (Unsigned_32'Last)
      and then Words (Info_Chunk) = Chunk_Max
      and then Valid_Catalog
        ((Ready => Words (Info_Ready) = 1,
          Table_Count => Unsigned_32 (Words (Info_Count)),
          Largest => Words (Info_Largest), Total => Words (Info_Total))));

   function Description_Well_Formed (Words : Reply; Table : Table_Number) return Boolean is
     (Words (Describe_Table_Word) = Unsigned_64 (Table)
      and then Table /= 0
      and then Words (Describe_Signature) <= Unsigned_64 (Unsigned_32'Last)
      and then Words (Describe_Revision) <= Unsigned_64 (Unsigned_8'Last)
      and then (for all I in Describe_Used + 1 .. Reply_Index'Last => Words (I) = 0));

   function Copy_Well_Formed (Words : Reply; Capacity : Capacity_Bytes) return Boolean is
     (Words (Copy_Copied) <= Unsigned_64 (Capacity)
      and then (for all I in Copy_Used + 1 .. Reply_Index'Last => Words (I) = 0));

   --  Decoders (the runtime's): Valid exactly for a well-formed reply;
   --  otherwise the result is the default value.
   procedure Decode_Info (Words : Reply; Item : out Catalog; Valid : out Boolean)
   with Post => Valid = Info_Well_Formed (Words)
                and then (if Valid then Valid_Catalog (Item) and then Encode_Info (Item) = Words
                          else Item = Catalog'(others => <>));

   procedure Decode_Description
     (Words : Reply; Table : Table_Number; Item : out Description; Valid : out Boolean)
   with Post => Valid = Description_Well_Formed (Words, Table)
                and then (if Valid then Item.Table = Table
                            and then Encode_Description (Item) = Words
                          else Item = Description'(others => <>));

   procedure Decode_Copy
     (Words : Reply; Capacity : Capacity_Bytes; Copied : out Capacity_Bytes; Valid : out Boolean)
   with Post => Valid = Copy_Well_Formed (Words, Capacity)
                and then (if Valid then Copied <= Capacity and then Encode_Copy (Copied) = Words
                          else Copied = 0);

   --  The encoders' replies are well formed and decode to what was encoded.
   procedure Lemma_Info_Round_Trip (Item : Catalog)
   with Ghost, Global => null,
        Pre  => Valid_Catalog (Item),
        Post => Info_Well_Formed (Encode_Info (Item));

   procedure Lemma_Description_Round_Trip (Item : Description)
   with Ghost, Global => null,
        Pre  => Item.Table /= 0,
        Post => Description_Well_Formed (Encode_Description (Item), Item.Table)
                and then Signature_Of (Signature_Word (Item.Signature)) = Item.Signature;

   procedure Lemma_Copy_Round_Trip (Copied, Capacity : Capacity_Bytes)
   with Ghost, Global => null,
        Pre  => Copied <= Capacity,
        Post => Copy_Well_Formed (Encode_Copy (Copied), Capacity);

end Firmware_Snapshot_ABI;
