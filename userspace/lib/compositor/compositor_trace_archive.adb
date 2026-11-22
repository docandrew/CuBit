with Interfaces;
package body Compositor_Trace_Archive with SPARK_Mode is
   function Checksum (Data : Content) return Word is
      Hash : Word := 16#CBF2_9CE4_8422_2325#;
   begin
      -- FNV-1a over the first 248 canonical little-endian bytes.
      for I in 0 .. 30 loop
         for J in 0 .. 7 loop
            Hash := (Hash xor (Interfaces.Shift_Right (Data (I), J * 8) and 255)) * 16#100_0000_01B3#;
         end loop;
      end loop;
      return Hash;
   end Checksum;
   function Encode (Sequence : Word; Value : S.Capture) return Chunk is
      Payload : constant W.Packet := W.Encode (Value.Value);
      Result : Chunk :=
        (0 => Magic, 1 => 256, 2 => Sequence, 3 => Value.Incarnation,
         4 => Value.Pid, 5 => Value.Publisher, 6 => Value.Batch,
         7 => Value.First_Sequence, 8 => Value.Producer_Dropped,
         9 => Value.Batch_Gaps, 10 => Payload (0), 11 => Payload (1),
         12 => Payload (2), 13 => Payload (3), 14 => Payload (4),
         15 => Payload (5), 16 => Payload (6), 17 => Payload (7),
         18 => Payload (8), 19 => Payload (9), 20 => Payload (10),
         21 => Payload (11), 22 => Payload (12), 23 => Payload (13),
         24 => Payload (14), 25 => Payload (15), others => 0);
   begin
      Result (31) := Checksum (Prefix (Result));
      return Result;
   end Encode;
   procedure Lemma_Valid (Data : Chunk) is
   begin
      null;
   end Lemma_Valid;
end Compositor_Trace_Archive;
