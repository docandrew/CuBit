--  HMAC (RFC 2104, FIPS 198-1), generic over a streaming hash.
--
--  The algorithm is the streaming branch of SPARKTLSCrypto.MAC.HMAC_SHA_256
--  with the SHA-256 constants (64-byte block, 32-byte digest) lifted into
--  generic formals: the key is hashed when longer than a block, padded
--  with zeros otherwise, XORed into the 0x36 / 0x5C pads, and the inner
--  and outer hashes are streamed through Init / Update / Final so the
--  message is never copied or concatenated. WiFi_Crypto.HMAC_SHA256 and
--  WiFi_Crypto.Legacy_WPA2.HMAC_SHA1 are instances.
--
--  Besides the one-shot Compute, the generic exposes the keyed state
--  (both pads absorbed) and a message stream over it, so callers such as
--  PBKDF2 key the HMAC once and MAC many messages, and multi-part
--  messages (label || data || counter) are MACed without a copy.
--
--  Proof: GNATprove analyses each instance. The only obligations are the
--  hash's own preconditions (Update_Margin restates them) and index
--  arithmetic on the pads.
--
--  Timing: control flow depends on the key length and message lengths
--  only, never on key or message bytes.


generic
   --  Hash input block size in bytes (64 for SHA-1 and SHA-256).
   Block_Bytes : N32;
   --  Hash output size in bytes; at most Block_Bytes.
   Digest_Bytes : N32;
   --  Update accepts Data when Data'Last < N32'Last - Update_Margin.
   Update_Margin : N32;
   type Context is private;
   with procedure Init (Ctx : out Context);
   with procedure Update (Ctx : in out Context; Data : Byte_Seq);
   with procedure Final (Ctx : in out Context; Output : out Byte_Seq);
package WiFi_Crypto.HMAC_G with
   SPARK_Mode => On
is
   pragma Assert (Digest_Bytes in 1 .. Block_Bytes);
   pragma Assert (Block_Bytes <= Update_Margin);

   subtype Digest_Index is N32 range 0 .. Digest_Bytes - 1;
   subtype Digest is Byte_Seq (Digest_Index);

   --  Largest index a message, key or message part may end at.
   Max_Data_Last : constant N32 := N32'Last - Update_Margin - 1;

   --  HMAC keyed with K: the inner and outer hashes after their pads.
   type Keyed_State is private;

   --  A message being MACed under a Keyed_State.
   type Message_State is private;

   procedure Set_Key (KS : out Keyed_State; K : Byte_Seq)
   with Global => null,
        Pre    => K'Last <= Max_Data_Last;

   procedure Start (MS : out Message_State; KS : Keyed_State)
   with Global => null;

   procedure Absorb (MS : in out Message_State; Data : Byte_Seq)
   with Global => null,
        Pre    => Data'Last <= Max_Data_Last;

   procedure Finish (MS : in out Message_State; Output : out Digest)
   with Global => null;

   --  Output := HMAC (K, M).
   procedure Compute (Output : out Digest; M : Byte_Seq; K : Byte_Seq)
   with Global => null,
        Pre    => M'Last <= Max_Data_Last and K'Last <= Max_Data_Last;

private
   type Keyed_State is record
      Inner : Context;
      Outer : Context;
   end record;

   type Message_State is record
      Inner : Context;
      Outer : Context;
   end record;
end WiFi_Crypto.HMAC_G;
