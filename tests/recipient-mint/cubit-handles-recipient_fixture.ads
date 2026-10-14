--  Hosted test only: handles of several kinds from words, and the words of
--  a derivation, so the scripted kernel can stand behind them. Never part
--  of a CuBit program.
with Interfaces; use Interfaces;
package CuBit.Handles.Recipient_Fixture is
   function Endpoint_Word (Word : Unsigned_64) return Endpoint;
   function Process_Word (Word : Unsigned_64) return Process;
   function Cspace_Word (Word : Unsigned_64) return Cspace;
   function Word_Of (Item : Endpoint) return Unsigned_64;
   function Word_Of (Item : Foreign_Handle) return Unsigned_64;
   function Words_Of (Copy : Derivation) return ABI.Derivation;
end CuBit.Handles.Recipient_Fixture;
