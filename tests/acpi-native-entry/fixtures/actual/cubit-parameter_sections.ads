------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The typed parameters section of a launch block
--  (docs/ccl-launch-parameters.md, docs/process-arguments.md): the one value
--  of a program's parameter record its launcher supplied.
--
--  @description
--  procmgr checks the launcher's value against the program's signature
--  (CuBit.Parameter_Signatures), renders its canonical source and places it
--  in the child's launch block, with the signature's identity, in the
--  trailer after the ring table and before the description. The child's
--  generated binding (package Parameters) finds it, compares the identity
--  with its own and decodes the source. A program that declares no
--  parameters gets no section.
--
--  Section, little-endian:
--
--     0  4   magic "PVAL"
--     4  u16 value bytes V, 1 .. Maximum_Value_Bytes
--     6  32  the signature's identity (SHA-256 of its text)
--    38  V   the value's canonical CCL source, printable ASCII
--
--  Measure finds a section at the start of a trailer; Validate is proved to
--  accept exactly the Well_Formed sections and Encode to produce only
--  well-formed ones. The source itself is checked by the binding's Decode.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Launch_Arguments;
with CuBit.Parameter_Signatures;

package CuBit.Parameter_Sections with Pure, SPARK_Mode is

   package Signatures renames CuBit.Parameter_Signatures;

   --  The longest canonical value: the launch block's budget for it
   --  (CuBit.Launch_Arguments), and what the generated decoders read
   --  (CuBit.Canonical_Readers.Maximum_Canonical is this).
   Maximum_Value_Bytes : constant := CuBit.Launch_Arguments.Parameter_Value_Budget;

   Magic_Bytes : constant := 4;
   Magic_0 : constant Unsigned_8 := Character'Pos ('P');
   Magic_1 : constant Unsigned_8 := Character'Pos ('V');
   Magic_2 : constant Unsigned_8 := Character'Pos ('A');
   Magic_3 : constant Unsigned_8 := Character'Pos ('L');

   Value_Length_Offset : constant := 4;
   Identity_Offset     : constant := 6;
   Header_Bytes        : constant := Identity_Offset + Signatures.Identity_Bytes;
   Byte_Bits           : constant := 8;
   Maximum_Section_Bytes : constant := Header_Bytes + Maximum_Value_Bytes;
   pragma Compile_Time_Error
     (Header_Bytes /= CuBit.Launch_Arguments.Parameter_Header_Budget
      or else Maximum_Section_Bytes /= CuBit.Launch_Arguments.Parameter_Section_Budget,
      "a parameters section must fit the launch block's trailer budget for it");

   subtype Section_Length is Natural range 0 .. Maximum_Section_Bytes;
   subtype Value_Length is Positive range 1 .. Maximum_Value_Bytes;
   type Bytes is array (Positive range <>) of Unsigned_8;

   function Declared_Value (Item : Bytes) return Natural is
     (Natural (Item (Value_Length_Offset + 1))
      + Natural (Item (Value_Length_Offset + 2)) * 2 ** Byte_Bits)
   with Pre => Item'First = 1 and then Item'Length >= Header_Bytes;

   --  The header: magic, and a value length in range that fits Item.
   function Header_Valid (Item : Bytes) return Boolean is
     (Item'Length >= Header_Bytes
      and then Item (1) = Magic_0 and then Item (2) = Magic_1
      and then Item (3) = Magic_2 and then Item (4) = Magic_3
      and then Declared_Value (Item) in Value_Length
      and then Declared_Value (Item) <= Item'Length - Header_Bytes)
   with Pre => Item'First = 1;

   function Well_Formed (Item : Bytes) return Boolean is
     (Item'First = 1
      and then Header_Valid (Item)
      and then Item'Length = Header_Bytes + Declared_Value (Item)
      and then (for all K in Header_Bytes + 1 .. Item'Last => Signatures.Printable (Item (K))));

   --  Whether Item starts with a section header, and the section's length
   --  if so (the bytes after it are the description).
   procedure Measure (Item : Bytes; Present : out Boolean; Length : out Section_Length)
   with Pre  => Item'First = 1,
        Post => (if Present then Length in Header_Bytes + 1 .. Item'Length
                   and then Header_Valid (Item)
                   and then Length = Header_Bytes + Declared_Value (Item)
                 else Length = 0);

   type Validation is (Valid, Wrong_Length, Wrong_Magic, Invalid_Value);

   function Validate (Item : Bytes) return Validation
   with Pre  => Item'First = 1,
        Post => (Validate'Result = Valid) = Well_Formed (Item);

   function Identity_Of (Item : Bytes) return Signatures.Identity is
     ([for I in Signatures.Identity_Index => Item (Identity_Offset + I)])
   with Pre => Well_Formed (Item);

   --  The value's canonical source is Item (Header_Bytes + 1 .. Item'Last):
   --  readers copy it out themselves (no function result, so the libc and
   --  services without a secondary stack can use this unit).

   --  The section for Value under Digest, in Item (1 .. Length). Rejected
   --  (Length 0) unless Value is 1 .. Maximum_Value_Bytes of printable
   --  ASCII.
   procedure Encode
     (Digest   : Signatures.Identity;
      Value    : String;
      Item     : out Bytes;
      Length   : out Section_Length;
      Accepted : out Boolean)
   with Pre  => Item'First = 1 and then Item'Length = Maximum_Section_Bytes,
        Post => (if Accepted then Length >= Header_Bytes and then Well_Formed (Item (1 .. Length))
                 else Length = 0);

end CuBit.Parameter_Sections;
