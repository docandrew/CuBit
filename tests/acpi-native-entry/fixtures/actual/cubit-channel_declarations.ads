------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A server's declared channel offers as bytes: the ELF section
--  .cubit.channels (CAP-002 step 3, docs/capability-handles.md section 4),
--  written by the manifest compiler from the manifest's `channels` entry and
--  read by launchers and inspection (handles(process), Observatory).
--
--  @description
--  Layout, little-endian:
--     header (Header_Bytes)
--      0  u32  Magic ("CBCH")
--      4  u16  Format_Version
--      6  u16  offer count (0 .. Maximum_Offers)
--     offers (Offer_Bytes each)
--      0  u8   contract length (1 .. Maximum_Contract_Bytes)
--      1  u8   class (CuBit.Channel_Limits.Channel_Class)
--      2  u8   per-client kind (Limit_Kind)
--      3  u8   total kind
--      4  u32  per-client count: 1 .. Maximum_Total for At_Most, else 0
--      8  u32  total count, the same
--     12  4 x u8 reserved, zero
--     16  contract bytes, zero after the contract
--
--  Valid: contracts are 1 to 64 letters, digits, '.', '-' or '_', zero
--  padded, pairwise distinct; kinds and classes are known codes; when both
--  limits are At_Most the per-client count is at most the total.
--
--  Proved (level 2): Decode is free of run-time errors, reads each input
--  byte at most once, and accepts only valid tables, with a typed reason
--  for every refusal. Encode/Decode round trips are tested.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Limits; use CuBit.Channel_Limits;

package CuBit.Channel_Declarations with Pure, SPARK_Mode is

   --  "CBCH" read as a little-endian word.
   Magic          : constant Unsigned_32 := 16#4843_4243#;
   Format_Version : constant := 1;

   --  The manifest compiler's limit (CCL.Manifests.Model.MAX_CHANNEL_OFFERS).
   Maximum_Offers         : constant := 8;
   Maximum_Contract_Bytes : constant := 64;

   Header_Bytes : constant := 8;
   Offer_Fixed_Bytes : constant := 16;
   Offer_Bytes  : constant := Offer_Fixed_Bytes + Maximum_Contract_Bytes;
   Maximum_Table_Bytes : constant := Header_Bytes + Maximum_Offers * Offer_Bytes;

   --  Field offsets.
   Magic_Offset      : constant := 0;
   Version_Offset    : constant := 4;
   Count_Offset      : constant := 6;
   Length_Offset     : constant := 0;
   Class_Offset      : constant := 1;
   Per_Kind_Offset   : constant := 2;
   Total_Kind_Offset : constant := 3;
   Per_Count_Offset  : constant := 4;
   Total_Count_Offset : constant := 8;
   Reserved_First    : constant := 12;
   Reserved_Bytes    : constant := 4;
   Contract_Offset   : constant := Offer_Fixed_Bytes;

   subtype Contract_Length is Natural range 0 .. Maximum_Contract_Bytes;
   subtype Contract_Index is Positive range 1 .. Maximum_Contract_Bytes;
   type Contract_Bytes is array (Contract_Index) of Character;

   type Contract_Name is record
      Length : Contract_Length := 0;
      Text   : Contract_Bytes := [others => Character'Val (0)];
   end record;

   function Contract_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '-' | '_');

   function Valid_Contract (Item : Contract_Name) return Boolean is
     (Item.Length >= 1
      and then (for all I in 1 .. Item.Length => Contract_Character (Item.Text (I)))
      and then (for all I in Item.Length + 1 .. Maximum_Contract_Bytes =>
                  Item.Text (I) = Character'Val (0)));

   type Offer is record
      Contract   : Contract_Name;
      Per_Client : Channel_Limit;
      Total      : Channel_Limit;
      Class      : Channel_Class := Normal;
   end record;

   function Consistent (Item : Offer) return Boolean is
     (if Item.Per_Client.Kind = At_Most and then Item.Total.Kind = At_Most
      then Item.Per_Client.Count <= Item.Total.Count);

   subtype Offer_Count is Natural range 0 .. Maximum_Offers;
   subtype Offer_Index is Positive range 1 .. Maximum_Offers;
   type Offer_Array is array (Offer_Index) of Offer;

   type Table is record
      Count  : Offer_Count := 0;
      Offers : Offer_Array;
   end record;

   function Valid_Table (T : Table) return Boolean is
     ((for all I in 1 .. T.Count =>
         Valid_Contract (T.Offers (I).Contract) and then Consistent (T.Offers (I)))
      and then (for all I in 1 .. T.Count =>
                  (for all J in 1 .. T.Count =>
                     (if I /= J then T.Offers (I).Contract /= T.Offers (J).Contract))));

   subtype Table_Length is Natural range 0 .. Maximum_Table_Bytes;
   subtype Byte_Index is Positive range 1 .. Maximum_Table_Bytes;
   type Bytes is array (Byte_Index range <>) of Unsigned_8;

   function Encoded_Length (Count : Offer_Count) return Table_Length is
     (Header_Bytes + Count * Offer_Bytes);

   procedure Encode (Item : Table; Output : out Bytes; Length : out Table_Length)
   with Pre  => Valid_Table (Item) and then Output'First = 1
                and then Output'Length >= Encoded_Length (Item.Count),
        Post => Length = Encoded_Length (Item.Count);

   type Decode_Result is
     (Decoded, Too_Short, Bad_Magic, Bad_Version, Bad_Count, Bad_Length,
      Bad_Contract, Duplicate_Contract, Bad_Kind, Bad_Count_Value, Inconsistent,
      Bad_Class, Reserved_Not_Zero);

   procedure Decode (Raw : Bytes; Result : out Table; Outcome : out Decode_Result)
   with Pre  => Raw'First = 1,
        Post => (if Outcome = Decoded then Valid_Table (Result));

end CuBit.Channel_Declarations;
