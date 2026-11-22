--  WiFi_Crypto.Legacy_WPA2 (body). WPA2 key derivation only; see spec.

package body WiFi_Crypto.Legacy_WPA2 with
   SPARK_Mode => On
is
   pragma Warnings
     (GNATprove, Off, "is set by ""Fin*"" but not used after the call",
      Reason => "a hash or HMAC state is dead after its final step");

   procedure Passphrase_To_PMK
     (Output     : out PMK;
      Passphrase : Byte_Seq;
      SSID       : Byte_Seq)
   is
   begin
      PBKDF2_HMAC_SHA1.Derive
        (DK         => Output,
         Password   => Passphrase,
         Salt       => SSID,
         Iterations => PSK_Iterations);
   end Passphrase_To_PMK;

   procedure PRF
     (Output : out Byte_Seq;
      K      : Byte_Seq;
      A      : Byte_Seq;
      B      : Byte_Seq)
   is
      --  Y: the single zero octet between the label and the data.
      Separator : constant Byte_Seq (0 .. 0) := (0 => 0);

      subtype Counter_Value is N32 range 0 .. PRF_Counter_Values - 1;

      Block_Count : constant N32 :=
        (N32 (Output'Length) + SHA1_Digest_Bytes - 1) / SHA1_Digest_Bytes;
      KS      : HMAC_SHA1.Keyed_State;
      MS      : HMAC_SHA1.Message_State;
      H       : HMAC_SHA1.Digest;
      Counter : Byte_Seq (0 .. 0) := (0 => 0);
      Offset  : N32;
      Count   : N32;
   begin
      Output := (others => 0);
      HMAC_SHA1.Set_Key (KS, K);

      for X in Counter_Value range 0 .. Block_Count - 1 loop
         Counter (0) := Byte (X);
         HMAC_SHA1.Start (MS, KS);
         HMAC_SHA1.Absorb (MS, A);
         HMAC_SHA1.Absorb (MS, Separator);
         HMAC_SHA1.Absorb (MS, B);
         HMAC_SHA1.Absorb (MS, Counter);
         HMAC_SHA1.Finish (MS, H);

         Offset := X * SHA1_Digest_Bytes;
         Count := N32'Min (SHA1_Digest_Bytes, N32 (Output'Length) - Offset);
         Output (Output'First + Offset .. Output'First + Offset + Count - 1) :=
           H (0 .. Count - 1);
      end loop;
   end PRF;

end WiFi_Crypto.Legacy_WPA2;
