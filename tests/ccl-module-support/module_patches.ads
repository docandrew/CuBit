with Interfaces; use Interfaces;
with CCL.Format;
with CCL.Catalog;
with CCL.Objects;

--  Test support for CCLB v8 corruption tests. The profile gives every value
--  exactly one encoding, so a test finds a field by its encoded bytes and
--  replaces them; lengths may differ (a longer head, an inserted item).
package Module_Patches is
   type Bytes is array (Positive range <>) of Unsigned_8;

   --  Replace an occurrence of From in Data (0 .. Length - 1) by To: the only
   --  one when Occurrence is 0, else the Occurrence'th. Found is False, and
   --  nothing changes, when there is no such occurrence (or, for 0, more
   --  than one) or the result does not fit.
   procedure Replace
     (Data : in out CCL.Format.Byte_Array; Length : in out CCL.Format.Module_Length;
      From, To : Bytes; Found : out Boolean; Occurrence : Natural := 0);

   --  A digest as the module encodes it: a 32-byte CBOR byte string
   --  (head 58 20), words most significant byte first.
   function Encoded_Digest (Words : CCL.Catalog.Descriptor_Digest) return Bytes;
   function Encoded_Digest (Words : CCL.Objects.Schema_Key) return Bytes;
end Module_Patches;
