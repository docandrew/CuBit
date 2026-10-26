--  AES-CCM (NIST SP 800-38C), generic over the AES block cipher (body).
--  Formatting follows SP 800-38C Appendix A (B_0, the AAD length prefix,
--  zero padding) and the counter blocks A.3.


package body WiFi_Crypto.AES_CCM_G with
   SPARK_Mode => On
is
   --  B_0 flags (A.2.1): Adata in bit 6, (t - 2) / 2 in bits 3 .. 5,
   --  q - 1 in bits 0 .. 2. Counter-block flags (A.3): q - 1 only.
   Adata_Flag      : constant Byte := 16#40#;
   Tag_Field_Shift : constant := 3;
   Length_Flags    : constant Byte := Length_Field_Bytes - 1;

   subtype Nonce_Position is N32 range Flags_Bytes .. Flags_Bytes + Nonce_Bytes - 1;
   subtype Length_Position is N32 range Nonce_Position'Last + 1 .. Block_Bytes - 1;

   subtype Fill_Count is N32 range 0 .. Block_Bytes;
   subtype Fill_Index is N32 range 0 .. Block_Bytes - 1;

   --  The CBC-MAC accumulator: chaining value and a partial block.
   type MAC_State is record
      X      : Bytes_16;
      Buffer : Bytes_16;
      Fill   : Fill_Count;
   end record;

   subtype Counter_Value is N32 range 0 .. Max_Payload_Bytes / Block_Bytes + 1;

   --  A 16-bit value as two big-endian octets.
   subtype Two_Octets is Byte_Seq (0 .. 1);
   function Big_Endian_16 (V : N32) return Two_Octets
   is (Byte (V / 2 ** Byte_Bits), Byte (V mod 2 ** Byte_Bits))
   with Pre => V < 2 ** (2 * Byte_Bits);

   procedure Encipher_Into_X (S : in out MAC_State; RK : Round_Keys) is
      Input : Bytes_16;
   begin
      for I in Bytes_16'Range loop
         Input (I) := S.X (I) xor S.Buffer (I);
      end loop;
      Cipher (S.X, Input, RK);
   end Encipher_Into_X;

   procedure Feed (S : in out MAC_State; RK : Round_Keys; Data : Byte_Seq)
   with Pre  => S.Fill in Fill_Index,
        Post => S.Fill in Fill_Index
   is
   begin
      for I in Data'Range loop
         pragma Loop_Invariant (S.Fill in Fill_Index);
         S.Buffer (S.Fill) := Data (I);
         S.Fill := S.Fill + 1;
         if S.Fill = Block_Bytes then
            Encipher_Into_X (S, RK);
            S.Fill := 0;
         end if;
      end loop;
   end Feed;

   --  Zero-pad a partial block and absorb it.
   procedure Pad (S : in out MAC_State; RK : Round_Keys)
   with Pre => S.Fill in Fill_Index,
        Post => S.Fill = 0
   is
   begin
      if S.Fill > 0 then
         for I in S.Fill .. Fill_Index'Last loop
            S.Buffer (I) := 0;
         end loop;
         Encipher_Into_X (S, RK);
         S.Fill := 0;
      end if;
   end Pad;

   --  T := the CBC-MAC of B_0 || formatted AAD || formatted payload.
   procedure Compute_MAC
     (T         : out Bytes_16;
      RK        : Round_Keys;
      N         : Nonce;
      AAD       : Byte_Seq;
      Payload   : Byte_Seq;
      Tag_Bytes : Tag_Length)
   with Pre => Payload'Length in Payload_Length
               and then AAD'Length in AAD_Length
   is
      Zero : constant Bytes_16 := (others => 0);
      S  : MAC_State := (X => Zero, Buffer => Zero, Fill => 0);
      Flags : Byte :=
        Shift_Left (Byte ((Tag_Bytes - 2) / 2), Tag_Field_Shift) or Length_Flags;
   begin
      if AAD'Length > 0 then
         Flags := Flags or Adata_Flag;
      end if;
      S.Buffer (0) := Flags;
      S.Buffer (Nonce_Position) := N;
      S.Buffer (Length_Position) := Big_Endian_16 (N32 (Payload'Length));
      Encipher_Into_X (S, RK);

      if AAD'Length > 0 then
         Feed (S, RK, Big_Endian_16 (N32 (AAD'Length)));
         Feed (S, RK, AAD);
         Pad (S, RK);
      end if;
      Feed (S, RK, Payload);
      Pad (S, RK);
      T := S.X;
   end Compute_MAC;

   --  S_i := E (K, A_i) for counter block i.
   procedure Key_Stream_Block
     (S_I : out Bytes_16; RK : Round_Keys; N : Nonce; I : Counter_Value)
   is
      A : Bytes_16 := (others => 0);
   begin
      A (0) := Length_Flags;
      A (Nonce_Position) := N;
      A (Length_Position) := Big_Endian_16 (I);
      Cipher (S_I, A, RK);
   end Key_Stream_Block;

   --  Output := Input xor S_1 || S_2 || ...
   procedure CTR
     (Output : out Byte_Seq;
      RK     : Round_Keys;
      N      : Nonce;
      Input  : Byte_Seq)
   with Pre => Input'Length in Payload_Length
               and then Output'Length = Input'Length
   is
      Block_Count : constant Counter_Value :=
        (N32 (Input'Length) + Block_Bytes - 1) / Block_Bytes;
      S_I    : Bytes_16;
      Offset : N32;
      Count  : N32;
   begin
      Output := (others => 0);
      for I in 1 .. Block_Count loop
         Key_Stream_Block (S_I, RK, N, I);
         Offset := (I - 1) * Block_Bytes;
         Count := N32'Min (Block_Bytes, N32 (Input'Length) - Offset);
         for J in 0 .. Count - 1 loop
            Output (Output'First + Offset + J) :=
              Input (Input'First + Offset + J) xor S_I (J);
         end loop;
      end loop;
   end CTR;

   procedure Encrypt
     (Ciphertext : out Byte_Seq;
      Tag        : out Byte_Seq;
      RK         : Round_Keys;
      N          : Nonce;
      AAD        : Byte_Seq;
      Plaintext  : Byte_Seq)
   is
      T   : Bytes_16;
      S_0 : Bytes_16;
   begin
      Compute_MAC (T, RK, N, AAD, Plaintext, Tag'Length);
      CTR (Ciphertext, RK, N, Plaintext);
      Key_Stream_Block (S_0, RK, N, 0);
      Tag := (others => 0);
      for J in 0 .. N32 (Tag'Length) - 1 loop
         Tag (Tag'First + J) := T (J) xor S_0 (J);
      end loop;
   end Encrypt;

   procedure Decrypt
     (Plaintext  : out Byte_Seq;
      Valid      : out Boolean;
      RK         : Round_Keys;
      N          : Nonce;
      AAD        : Byte_Seq;
      Ciphertext : Byte_Seq;
      Tag        : Byte_Seq)
   is
      Tag_Bytes : constant Tag_Length := Tag'Length;
      T        : Bytes_16;
      S_0      : Bytes_16;
      Expected : Byte_Seq (0 .. Tag_Bytes - 1);
      Received : Byte_Seq (0 .. Tag_Bytes - 1) := (others => 0);
   begin
      CTR (Plaintext, RK, N, Ciphertext);
      Compute_MAC (T, RK, N, AAD, Plaintext, Tag_Bytes);
      Key_Stream_Block (S_0, RK, N, 0);
      for J in Expected'Range loop
         Expected (J) := T (J) xor S_0 (J);
         Received (J) := Tag (Tag'First + J);
      end loop;
      Valid := Equal (Expected, Received);
      if not Valid then
         Plaintext := (others => 0);
      end if;
   end Decrypt;

end WiFi_Crypto.AES_CCM_G;
