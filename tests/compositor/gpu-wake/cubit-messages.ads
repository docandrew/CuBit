with Interfaces;
--  Hosted stand-in: only the completion entry Desktop_GPU_Wake reads.
package CuBit.Messages is
   type CompletionEntry is record
      token : Interfaces.Unsigned_64 := 0;
      valid : Boolean := False;
   end record;
end CuBit.Messages;
