--  PBKDF2 (RFC 8018 section 5.2), generic over a keyed HMAC (body).

package body WiFi_Crypto.PBKDF2_G with
   SPARK_Mode => On
is
   pragma Warnings
     (GNATprove, Off, "is set by ""Fin*"" but not used after the call",
      Reason => "a hash or HMAC state is dead after its final step");

   subtype Digest_Index is N32 range 0 .. Digest_Bytes - 1;
   subtype Digest is Byte_Seq (Digest_Index);

   --  INT (i): the block index as a four-octet big-endian integer.
   Block_Index_Bytes : constant := 4;
   subtype Block_Index_Encoding is Byte_Seq (0 .. Block_Index_Bytes - 1);
   subtype Block_Number is N32 range 1 .. Max_Blocks;

   function Encode_Block_Index (I : Block_Number) return Block_Index_Encoding
   is (0, 0, 0, Byte (I));

   procedure Derive
     (DK         : out Byte_Seq;
      Password   : Byte_Seq;
      Salt       : Byte_Seq;
      Iterations : Iteration_Count)
   is
      Block_Count : constant Block_Number :=
        (N32 (DK'Length) + Digest_Bytes - 1) / Digest_Bytes;
      KS     : Keyed_State;
      MS     : Message_State;
      U      : Digest;
      T      : Digest;
      Offset : N32;
      Count  : N32;
   begin
      DK := (others => 0);
      Set_Key (KS, Password);

      for B in 1 .. Block_Count loop
         pragma Warnings (Off, "condition is always True",
                          Reason => "kept as the proof's loop cut point");
         pragma Loop_Invariant (B <= Block_Count);
         pragma Warnings (On, "condition is always True");

         --  U_1 = PRF (P, S || INT (i))
         Start (MS, KS);
         Absorb (MS, Salt);
         Absorb (MS, Encode_Block_Index (B));
         Finish (MS, U);
         T := U;

         --  U_j = PRF (P, U_(j-1)); T_i = U_1 xor ... xor U_c
         for J in 2 .. Iterations loop
            Start (MS, KS);
            Absorb (MS, U);
            Finish (MS, U);
            for I in Digest_Index loop
               T (I) := T (I) xor U (I);
            end loop;
         end loop;

         Offset := (B - 1) * Digest_Bytes;
         Count := N32'Min (Digest_Bytes, N32 (DK'Length) - Offset);
         DK (DK'First + Offset .. DK'First + Offset + Count - 1) :=
           T (0 .. Count - 1);
      end loop;
   end Derive;

end WiFi_Crypto.PBKDF2_G;
