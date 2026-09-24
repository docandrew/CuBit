with Interfaces; use Interfaces;

--  Portable identity only: a reference is not authority or an address.
package CuBit.Grant_References with Pure, SPARK_Mode is
   Maximum_Slot : constant Unsigned_64 := 4095;
   Maximum_Generation : constant Unsigned_64 :=
     Unsigned_64 (Unsigned_32'Last);
   subtype Global_Slot is Unsigned_64 range 0 .. Maximum_Slot;
   subtype Generation is Unsigned_64 range 1 .. Maximum_Generation;
   type Reference is record
      slot : Global_Slot := 0;
      generation : Grant_References.Generation := 1;
   end record;
   --  Canonical IPC identity: generation in the high 32 bits, slot in the
   --  low 32 bits. Unused slot bits must be zero; no truncation or aliases.
   --  Encoding conveys no authority: Acquire still authenticates the owner,
   --  grantee, generation, access and range in the kernel.
   Wire_Field_Base : constant Unsigned_64 := 2 ** 32;
   function Valid_Wire (Word : Unsigned_64) return Boolean is
     (Word / Wire_Field_Base in Generation and then
      Word mod Wire_Field_Base in Global_Slot);
   function Encode (Item : Reference) return Unsigned_64 is
     (Item.generation * Wire_Field_Base + Item.slot)
     with Post => Valid_Wire (Encode'Result) and then
       Encode'Result / Wire_Field_Base = Item.generation and then
       Encode'Result mod Wire_Field_Base = Item.slot;
   function Decode (Word : Unsigned_64) return Reference is
     (slot => Word mod Wire_Field_Base,
      generation => Word / Wire_Field_Base)
     with Pre => Valid_Wire (Word),
          Post => Encode (Decode'Result) = Word;
   --  Kernel query: zero = owned inactive slot, live generation otherwise;
   --  errors/foreign slots return U64'Last. Generations never wrap.
   function Retirement_Confirmed
     (Item : Reference; Observed : Unsigned_64) return Boolean is
     (Observed = 0 or else
      (Observed in Generation and then Observed > Item.generation));
end CuBit.Grant_References;
