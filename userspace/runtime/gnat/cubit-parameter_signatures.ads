------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A program's typed parameter signature: its .cubit.parameters descriptor
--  (docs/ccl-launch-parameters.md).
--
--  @description
--  A program that declares typed parameters in its manifest carries their
--  canonical signature: the CCL type declarations of its parameter record
--  and every type the record reaches, in dependency order, the record last,
--  each spelled one way (ccl-manifest renders them), so equal signatures
--  are equal text. Its identity is the SHA-256 of that text. A launcher
--  checks a value against the signature with the CCL analyser and hands the
--  child the value's canonical source; the child reads it with the decoder
--  ccl-manifest generated from the same signature, whose identity it
--  carries.
--
--  The descriptor, little-endian:
--
--     0  4   magic "PRMS"
--     4  u16 format version (Format_Version)
--     6  u16 signature bytes S, 1 .. Maximum_Signature_Bytes
--     8  32  identity: the SHA-256 of the signature text
--    40  u8  root name bytes R, 1 .. Maximum_Root_Bytes
--    41  R   the parameter record's type name: a letter, then letters,
--            digits and single underscores, never a trailing one (an Ada
--            identifier, as the binding names it)
--    41+R S  the signature text, printable ASCII
--
--  and nothing after. Validate is proved to accept exactly the Well_Formed
--  descriptors, and Encode to produce only well-formed ones. Neither
--  computes the identity: the manifest tool hashes the text with the
--  verified SHA-256 (SPARKTLSCrypto), and the launcher compares it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Parameter_Signatures with Pure, SPARK_Mode is

   Format_Version          : constant := 1;
   Identity_Bytes          : constant := 32;
   Maximum_Root_Bytes      : constant := 32;
   Maximum_Signature_Bytes : constant := 2048;

   --  "PRMS".
   Magic_Bytes : constant := 4;
   Magic_0 : constant Unsigned_8 := Character'Pos ('P');
   Magic_1 : constant Unsigned_8 := Character'Pos ('R');
   Magic_2 : constant Unsigned_8 := Character'Pos ('M');
   Magic_3 : constant Unsigned_8 := Character'Pos ('S');

   --  Field offsets, in bytes from the start.
   Version_Offset          : constant := 4;
   Signature_Length_Offset : constant := 6;
   Identity_Offset         : constant := 8;
   Root_Length_Offset      : constant := Identity_Offset + Identity_Bytes;
   Header_Bytes            : constant := Root_Length_Offset + 1;
   Field_16_Bytes          : constant := 2;
   Byte_Bits               : constant := 8;

   Maximum_Descriptor_Bytes : constant :=
     Header_Bytes + Maximum_Root_Bytes + Maximum_Signature_Bytes;

   subtype Descriptor_Length is Natural range 0 .. Maximum_Descriptor_Bytes;
   subtype Descriptor_Index is Positive range 1 .. Maximum_Descriptor_Bytes;
   type Bytes is array (Descriptor_Index range <>) of Unsigned_8;

   subtype Identity_Index is Positive range 1 .. Identity_Bytes;
   type Identity is array (Identity_Index) of Unsigned_8;

   subtype Root_Length is Positive range 1 .. Maximum_Root_Bytes;
   subtype Signature_Length is Positive range 1 .. Maximum_Signature_Bytes;

   function Letter (Item : Unsigned_8) return Boolean is
     (Item in Character'Pos ('A') .. Character'Pos ('Z')
           | Character'Pos ('a') .. Character'Pos ('z'));
   function Identifier_Character (Item : Unsigned_8) return Boolean is
     (Letter (Item) or else Item in Character'Pos ('0') .. Character'Pos ('9')
      or else Item = Character'Pos ('_'));
   function Printable (Item : Unsigned_8) return Boolean is
     (Item in Character'Pos (' ') .. Character'Pos ('~'));

   function Field_16 (Item : Bytes; Offset : Natural) return Unsigned_16 is
     (Unsigned_16 (Item (Offset + 1)) or
      Shift_Left (Unsigned_16 (Item (Offset + 2)), Byte_Bits))
   with Pre => Item'First = 1 and then Item'Length >= Field_16_Bytes
               and then Offset <= Item'Length - Field_16_Bytes;

   --  The header as far as its fixed fields go.
   function Header_Valid (Item : Bytes) return Boolean is
     (Item'Length >= Header_Bytes
      and then Item (1) = Magic_0 and then Item (2) = Magic_1
      and then Item (3) = Magic_2 and then Item (4) = Magic_3
      and then Field_16 (Item, Version_Offset) = Format_Version)
   with Pre => Item'First = 1;

   function Declared_Root (Item : Bytes) return Natural is
     (Natural (Item (Root_Length_Offset + 1)))
   with Pre => Item'First = 1 and then Item'Length >= Header_Bytes;

   function Declared_Signature (Item : Bytes) return Natural is
     (Natural (Field_16 (Item, Signature_Length_Offset)))
   with Pre => Item'First = 1 and then Item'Length >= Header_Bytes;

   --  The declared lengths are in range and add up to the descriptor's.
   function Lengths_Valid (Item : Bytes) return Boolean is
     (Declared_Root (Item) in Root_Length
      and then Declared_Signature (Item) in Signature_Length
      and then Item'Length = Header_Bytes + Declared_Root (Item) + Declared_Signature (Item))
   with Pre => Item'First = 1 and then Item'Length >= Header_Bytes;

   --  An identifier: a letter, then letters, digits and single
   --  underscores, not ending with one.
   function Identifier_Valid (Item : Bytes; First, Last : Positive) return Boolean is
     (Letter (Item (First))
      and then (for all K in First .. Last => Identifier_Character (Item (K)))
      and then Item (Last) /= Character'Pos ('_')
      and then (for all K in First .. Last - 1 =>
                  not (Item (K) = Character'Pos ('_') and then Item (K + 1) = Character'Pos ('_'))))
   with Pre => First <= Last and then First in Item'Range and then Last in Item'Range;

   function Root_First (Item : Bytes) return Positive is (Header_Bytes + 1)
   with Pre => Item'First = 1;
   function Root_Last (Item : Bytes) return Positive is (Header_Bytes + Declared_Root (Item))
   with Pre => Item'First = 1 and then Item'Length >= Header_Bytes and then Lengths_Valid (Item);
   function Signature_First (Item : Bytes) return Positive is (Root_Last (Item) + 1)
   with Pre => Item'First = 1 and then Item'Length >= Header_Bytes and then Lengths_Valid (Item);

   function Well_Formed (Item : Bytes) return Boolean is
     (Item'First = 1
      and then Item'Length <= Maximum_Descriptor_Bytes
      and then Header_Valid (Item)
      and then Lengths_Valid (Item)
      and then Identifier_Valid (Item, Root_First (Item), Root_Last (Item))
      and then (for all K in Signature_First (Item) .. Item'Last => Printable (Item (K))));

   type Validation is
     (Valid, Wrong_Length, Wrong_Magic, Unknown_Format, Length_Mismatch,
      Invalid_Root, Invalid_Signature);

   function Validate (Item : Bytes) return Validation
   with Pre  => Item'First = 1,
        Post => (Validate'Result = Valid) = Well_Formed (Item);

   --  The parts of a well-formed descriptor.
   function Identity_Of (Item : Bytes) return Identity is
     ([for I in Identity_Index => Item (Identity_Offset + I)])
   with Pre => Well_Formed (Item);

   function Root_Of (Item : Bytes) return String
   with Pre  => Well_Formed (Item),
        Post => Root_Of'Result'First = 1 and then Root_Of'Result'Length = Declared_Root (Item);

   function Signature_Of (Item : Bytes) return String
   with Pre  => Well_Formed (Item),
        Post => Signature_Of'Result'First = 1
                and then Signature_Of'Result'Length = Declared_Signature (Item);

   --  The descriptor of Root's signature with that identity, in
   --  Item (1 .. Length). Rejected (Length 0) if Root is not an identifier
   --  of 1 .. Maximum_Root_Bytes, or Signature is not 1 ..
   --  Maximum_Signature_Bytes of printable ASCII.
   procedure Encode
     (Root      : String;
      Signature : String;
      Digest    : Identity;
      Item      : out Bytes;
      Length    : out Descriptor_Length;
      Accepted  : out Boolean)
   with Pre  => Item'First = 1 and then Item'Length = Maximum_Descriptor_Bytes,
        Post => (if Accepted then Length >= Header_Bytes and then Well_Formed (Item (1 .. Length))
                 else Length = 0);

end CuBit.Parameter_Signatures;
