with Interfaces; use Interfaces;
with Intel_GPU_Live_Ring_Publish;
-- Proof-only hardware boundary. Imported callbacks return arbitrary values;
-- their success, ownership, marker and tail results are NOT assumed true.
-- Global=>null models callbacks not mutating the publisher's private channel;
-- actual DMA visibility/coherence remains a separately validated prerequisite.
-- Pure imported functions also abstract away asynchronous hardware changes;
-- ownership changes during IO are covered by the fault-injection suite, not
-- established by this model. Callback termination is an external assumption.
package Live_Ring_Proof with SPARK_Mode is
   function Owned return Boolean with Import, Global => null;
   procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean)
     with Import, Global => null;
   procedure Read_Tail (Value : out Unsigned_32; OK : out Boolean)
     with Import, Global => null;
   procedure Write_Word (Offset, Value : Unsigned_32; OK : out Boolean)
     with Import, Global => null,
       Pre => Offset >= 384 and Offset < 16384 - 64 and Offset mod 4 = 0;
   function Publish_Words (Offset, Bytes : Unsigned_32) return Boolean
     with Import, Global => null,
       Pre => Offset >= 384 and Bytes in 120 | 384 and
         Offset mod 8 = 0 and Offset <= 16384 - 64 - Bytes;
   procedure Write_Tail (Value : Unsigned_32; OK : out Boolean)
     with Import, Global => null,
       Pre => Value in 504 .. 16384 - 64 and Value mod 8 = 0;
   function Tail_Visible return Boolean with Import, Global => null;
   package Publisher is new Intel_GPU_Live_Ring_Publish
     (Owned, Read_Marker, Read_Tail, Write_Word, Publish_Words,
      Write_Tail, Tail_Visible);
end Live_Ring_Proof;
