with Compositor_Trace_Stream;
package Compositor_Trace_Archive with Pure, SPARK_Mode is
   package S renames Compositor_Trace_Stream;
   package W renames S.W;
   subtype Word is W.Word;
   use type Word, S.Capture;
   -- One self-contained 256-byte event chunk. The byte adapter must store
   -- words little-endian. Checksums detect corruption, not malicious forgery.
   type Chunk is array (Natural range 0 .. 31) of Word;
   type Content is array (Natural range 0 .. 30) of Word;
   function Prefix (Data : Chunk) return Content is
     (Data (0), Data (1), Data (2), Data (3), Data (4), Data (5), Data (6), Data (7), Data (8), Data (9), Data (10), Data (11), Data (12), Data (13), Data (14), Data (15), Data (16), Data (17), Data (18), Data (19), Data (20), Data (21), Data (22), Data (23), Data (24), Data (25), Data (26), Data (27), Data (28), Data (29), Data (30));
   Magic : constant Word := 16#4354_4152_0000_0001#;
   function Valid (Value : S.Capture) return Boolean is
     (Value.Success and then Value.Incarnation /= 0 and then Value.Pid /= 0 and then
      S.P.Is_Publisher (Value.Publisher) and then Value.Batch /= 0 and then
      Value.First_Sequence in 1 .. Word'Last - 3 and then W.Valid (Value.Value));
   -- Other fields are meaningful only when Success is true.
   type Decoded is record
      Success : Boolean := False;
      Sequence : Word := 0;
      Value : S.Capture;
   end record;
   function Checksum (Data : Content) return Word;
   function Decode (Data : Chunk) return Decoded;
   pragma Annotate (GNATprove, Inline_For_Proof, Decode);
   function Encode (Sequence : Word; Value : S.Capture) return Chunk
     with Pre => Sequence in 1 .. Word'Last - 1 and then Valid (Value),
       Post => Decode (Encode'Result).Success and then
         Decode (Encode'Result).Sequence = Sequence and then
         Decode (Encode'Result).Value = Value;
   procedure Lemma_Valid (Data : Chunk) with Ghost,
     Post => (if Decode (Data).Success then Valid (Decode (Data).Value) and then
       Decode (Data).Sequence in 1 .. Word'Last - 1);
private
   function Packet (Data : Chunk) return W.Packet is
     (Data (10), Data (11), Data (12), Data (13), Data (14), Data (15),
      Data (16), Data (17), Data (18), Data (19), Data (20), Data (21),
      Data (22), Data (23), Data (24), Data (25));
   function Reconstruct (Data : Chunk; Event : W.Event) return S.Capture is
     (Success => True, Incarnation => Data (3), Pid => Data (4),
      Publisher => Data (5), Batch => Data (6), First_Sequence => Data (7),
      Producer_Dropped => Data (8), Batch_Gaps => Data (9), Value => Event);
   function Decode_Packet (Data : Chunk; Event : W.Decoded) return Decoded is
     (if not Event.Success then (Success => False, others => <>)
      elsif not Valid (Reconstruct (Data, Event.Value))
      then (Success => False, others => <>)
      else (True, Data (2), Reconstruct (Data, Event.Value)));
   function Decode (Data : Chunk) return Decoded is
     (if Data (0) /= Magic or else Data (1) /= 256 or else
         Data (2) not in 1 .. Word'Last - 1 or else
         (for some I in 26 .. 30 => Data (I) /= 0) or else
         Data (31) /= Checksum (Prefix (Data))
      then (Success => False, others => <>)
      else Decode_Packet (Data, W.Decode (Packet (Data))));
end Compositor_Trace_Archive;
