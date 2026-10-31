--  PBKDF2 (RFC 8018 section 5.2), generic over a keyed HMAC.
--
--  The formals are the keyed-state interface of an WiFi_Crypto.HMAC_G
--  instance: the password keys the HMAC once (Set_Key), and every U_j is
--  one Start / Absorb / Finish over that keyed state, so each iteration
--  costs two compression calls rather than four.
--
--  Proof: absence of run-time errors for each instance; the only
--  obligations are the HMAC's preconditions and the output offsets.
--
--  Timing: control flow depends on the iteration count, the salt length
--  and the output length only (all public in every use: WPA2 fixes 4096
--  iterations and a 32-byte output; the salt is the SSID).


generic
   Digest_Bytes  : N32;
   --  The HMAC accepts key and message parts ending at or below this index.
   Max_Data_Last : N32;
   type Keyed_State is private;
   type Message_State is private;
   with procedure Set_Key (KS : out Keyed_State; K : Byte_Seq);
   with procedure Start (MS : out Message_State; KS : Keyed_State);
   with procedure Absorb (MS : in out Message_State; Data : Byte_Seq);
   with procedure Finish (MS : in out Message_State; Output : out Byte_Seq);
package WiFi_Crypto.PBKDF2_G with
   SPARK_Mode => On
is
   --  Output blocks T_1 .. T_n; RFC 8018 allows 2^32 - 1, CuBit's uses need
   --  at most two (WPA2: 32 bytes of SHA-1 output).
   Max_Blocks : constant := 255;
   pragma Assert (Digest_Bytes in 1 .. N32'Last / Max_Blocks);
   Max_Derived_Bytes : constant N32 := Max_Blocks * Digest_Bytes;

   subtype Iteration_Count is N32 range 1 .. N32'Last;
   subtype Derived_Length is N32 range 1 .. Max_Derived_Bytes;

   procedure Derive
     (DK         : out Byte_Seq;
      Password   : Byte_Seq;
      Salt       : Byte_Seq;
      Iterations : Iteration_Count)
   with Global => null,
        Pre    => DK'Length in Derived_Length
                  and then DK'Last < N32'Last - Digest_Bytes
                  and then Password'Last <= Max_Data_Last
                  and then Salt'Last <= Max_Data_Last;

end WiFi_Crypto.PBKDF2_G;
