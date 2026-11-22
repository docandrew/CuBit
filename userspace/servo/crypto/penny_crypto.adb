with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with SPARKNaCl;                     use SPARKNaCl;
use type SPARKNaCl.I32;
with SPARKNaCl.AES;
with SPARKNaCl.Core;
with SPARKNaCl.Secretbox;
with SPARKTLSCrypto.AES_GCM;
with SPARKTLSCrypto.ChaCha20_Poly1305;
with SPARKTLSCrypto.Ed25519;
with SPARKTLSCrypto.Hashing.SHA1;
with SPARKTLSCrypto.Hashing.SHA256;
with SPARKTLSCrypto.Hashing.SHA384;
with SPARKTLSCrypto.Hashing.SHA512;
with SPARKTLSCrypto.HMAC384;
with SPARKTLSCrypto.MAC;
with SPARKTLSCrypto.P256;
with SPARKTLSCrypto.P256.ECDSA;
with SPARKTLSCrypto.P256.Point;
with SPARKTLSCrypto.P384.ECDSA;
with SPARKTLSCrypto.P384.Point;
with SPARKTLSCrypto.RSA;
with SPARKTLSCrypto.X25519;
with MLKEM;
with MLKEM.ML_KEM_768;
with Penny_ECDSA_Digest;
with SPARKTLSCrypto.AES_NI;
with SPARKTLSCrypto.GHASH_NI;
with SPARKTLSCrypto.CPU;
with SPARKTLSCrypto.AES_GCM_AVX512;
with SPARKTLSCrypto.ChaCha20_AVX512;
with SPARKTLSCrypto.Poly1305_AVX512;

package body Penny_Crypto is

   OK   : constant Result := 1;
   Fail : constant Result := 0;

   function To_Result (B : Boolean) return Result is (if B then OK else Fail);

   function Too_Long (Len : Unsigned_32) return Boolean is (Len > Max_Length);

   type Byte_Seq_Access is access Byte_Seq;
   procedure Free is new Ada.Unchecked_Deallocation (Byte_Seq, Byte_Seq_Access);

   --  A prepared AES-GCM key, plus the raw key for the one case the prepared
   --  entry points don't take (sealing an empty record). Cleared on free.
   type GCM_Context is record
      Prepared : SPARKTLSCrypto.AES_GCM.Prepared_Key;
      Raw      : Bytes_32 := [others => 0];
      Key_Len  : Unsigned_32 := 0;
   end record;
   package GCM_Contexts is new System.Address_To_Access_Conversions (GCM_Context);
   use type GCM_Contexts.Object_Pointer;
   procedure Free is new Ada.Unchecked_Deallocation (GCM_Context, GCM_Contexts.Object_Pointer);

   --  Digests ---------------------------------------------------------------

   function SHA1 (M : Address; M_Len : Unsigned_32; Digest : Address) return Result is
   begin
      if Too_Long (M_Len) then return Fail; end if;
      declare
         Message : Byte_Seq (0 .. N32 (M_Len) - 1) with Import, Address => M;
         Output  : SPARKTLSCrypto.Hashing.SHA1.Digest with Import, Address => Digest;
      begin
         SPARKTLSCrypto.Hashing.SHA1.Hash (Output, Message);
      end;
      return OK;
   end SHA1;

   function SHA256 (M : Address; M_Len : Unsigned_32; Digest : Address) return Result is
   begin
      if Too_Long (M_Len) then return Fail; end if;
      declare
         Message : Byte_Seq (0 .. N32 (M_Len) - 1) with Import, Address => M;
         Output  : SPARKTLSCrypto.Hashing.SHA256.Digest with Import, Address => Digest;
      begin
         SPARKTLSCrypto.Hashing.SHA256.Hash (Output, Message);
      end;
      return OK;
   end SHA256;

   function SHA384 (M : Address; M_Len : Unsigned_32; Digest : Address) return Result is
   begin
      if Too_Long (M_Len) then return Fail; end if;
      declare
         Message : Byte_Seq (0 .. N32 (M_Len) - 1) with Import, Address => M;
         Output  : SPARKTLSCrypto.Hashing.SHA384.Digest with Import, Address => Digest;
      begin
         SPARKTLSCrypto.Hashing.SHA384.Hash (Output, Message);
      end;
      return OK;
   end SHA384;

   function SHA512 (M : Address; M_Len : Unsigned_32; Digest : Address) return Result is
   begin
      if Too_Long (M_Len) then return Fail; end if;
      declare
         Message : Byte_Seq (0 .. N32 (M_Len) - 1) with Import, Address => M;
         Output  : SPARKTLSCrypto.Hashing.SHA512.Digest with Import, Address => Digest;
      begin
         SPARKTLSCrypto.Hashing.SHA512.Hash (Output, Message);
      end;
      return OK;
   end SHA512;

   function HMAC_SHA256 (K : Address; K_Len : Unsigned_32; M : Address; M_Len : Unsigned_32;
                         Mac : Address) return Result is
   begin
      if Too_Long (K_Len) or else Too_Long (M_Len) then return Fail; end if;
      declare
         Key     : Byte_Seq (0 .. N32 (K_Len) - 1) with Import, Address => K;
         Message : Byte_Seq (0 .. N32 (M_Len) - 1) with Import, Address => M;
         Output  : SPARKTLSCrypto.Hashing.SHA256.Digest with Import, Address => Mac;
      begin
         SPARKTLSCrypto.MAC.HMAC_SHA_256 (Output, Message, Key);
      end;
      return OK;
   end HMAC_SHA256;

   function HMAC_SHA384 (K : Address; K_Len : Unsigned_32; M : Address; M_Len : Unsigned_32;
                         Mac : Address) return Result is
   begin
      if Too_Long (K_Len) or else Too_Long (M_Len) then return Fail; end if;
      declare
         Key     : Byte_Seq (0 .. N32 (K_Len) - 1) with Import, Address => K;
         Message : Byte_Seq (0 .. N32 (M_Len) - 1) with Import, Address => M;
         Output  : SPARKTLSCrypto.HMAC384.Digest_384 with Import, Address => Mac;
      begin
         SPARKTLSCrypto.HMAC384.HMAC_SHA_384 (Output, Message, Key);
      end;
      return OK;
   end HMAC_SHA384;

   --  AEAD ------------------------------------------------------------------

   function AES_GCM_Seal (Key : Address; Key_Len : Unsigned_32; Nonce : Address;
                          AAD : Address; AAD_Len : Unsigned_32;
                          Buf : Address; Len : Unsigned_32; Tag : Address) return Result is
   begin
      if Too_Long (AAD_Len) or else Too_Long (Len) or else Key_Len not in 16 | 32 then
         return Fail;
      end if;
      declare
         N    : Bytes_12 with Import, Address => Nonce;
         A    : Byte_Seq (0 .. N32 (AAD_Len) - 1) with Import, Address => AAD;
         Data : Byte_Seq (0 .. N32 (Len) - 1) with Import, Address => Buf;
         T    : Bytes_16 with Import, Address => Tag;
         Empty : constant Byte_Seq (0 .. -1) := [others => 0];
      begin
         --  In place; the in-place entry points take a non-empty buffer.
         if Key_Len = 16 then
            declare
               Raw : Bytes_16 with Import, Address => Key;
               K   : constant AES.AES128_Key := AES.Construct (Raw);
            begin
               if Len > 0 then
                  SPARKTLSCrypto.AES_GCM.Encrypt_InPlace (Data, T, N, K, A);
               else
                  SPARKTLSCrypto.AES_GCM.Encrypt (Data, T, Empty, N, K, A);
               end if;
            end;
         else
            declare
               Raw : Bytes_32 with Import, Address => Key;
               K   : constant AES.AES256_Key := AES.Construct (Raw);
            begin
               if Len > 0 then
                  SPARKTLSCrypto.AES_GCM.Encrypt_InPlace_256 (Data, T, N, K, A);
               else
                  SPARKTLSCrypto.AES_GCM.Encrypt_256 (Data, T, Empty, N, K, A);
               end if;
            end;
         end if;
      end;
      return OK;
   end AES_GCM_Seal;

   function AES_GCM_Open (Key : Address; Key_Len : Unsigned_32; Nonce : Address;
                          AAD : Address; AAD_Len : Unsigned_32;
                          Buf : Address; Len : Unsigned_32; Tag : Address) return Result is
      Valid : Boolean := False;
   begin
      if Too_Long (AAD_Len) or else Too_Long (Len) or else Key_Len not in 16 | 32 then
         return Fail;
      end if;
      declare
         N    : Bytes_12 with Import, Address => Nonce;
         A    : Byte_Seq (0 .. N32 (AAD_Len) - 1) with Import, Address => AAD;
         Data : Byte_Seq (0 .. N32 (Len) - 1) with Import, Address => Buf;
         T    : Bytes_16 with Import, Address => Tag;
         Copy : Byte_Seq_Access := new Byte_Seq'(Data);
      begin
         if Key_Len = 16 then
            declare
               Raw : Bytes_16 with Import, Address => Key;
               K   : constant AES.AES128_Key := AES.Construct (Raw);
            begin
               SPARKTLSCrypto.AES_GCM.Decrypt (Data, Valid, T, Copy.all, N, K, A);
            end;
         else
            declare
               Raw : Bytes_32 with Import, Address => Key;
               K   : constant AES.AES256_Key := AES.Construct (Raw);
            begin
               SPARKTLSCrypto.AES_GCM.Decrypt_256 (Data, Valid, T, Copy.all, N, K, A);
            end;
         end if;
         Free (Copy);
      end;
      return To_Result (Valid);
   end AES_GCM_Open;

   function ChaCha20_Poly1305_Seal (Key : Address; Nonce : Address; AAD : Address; AAD_Len : Unsigned_32;
                                    Buf : Address; Len : Unsigned_32; Tag : Address) return Result is
   begin
      if Too_Long (AAD_Len) or else Too_Long (Len) then return Fail; end if;
      declare
         Raw  : Bytes_32 with Import, Address => Key;
         K    : constant Core.ChaCha20_Key := Core.Construct (Raw);
         N    : Bytes_12 with Import, Address => Nonce;
         A    : Byte_Seq (0 .. N32 (AAD_Len) - 1) with Import, Address => AAD;
         Data : Byte_Seq (0 .. N32 (Len) - 1) with Import, Address => Buf;
         T    : Bytes_16 with Import, Address => Tag;
      begin
         SPARKTLSCrypto.ChaCha20_Poly1305.Encrypt_InPlace (Data, T, Core.ChaCha20_IETF_Nonce (N), K, A);
      end;
      return OK;
   end ChaCha20_Poly1305_Seal;

   function ChaCha20_Poly1305_Open (Key : Address; Nonce : Address; AAD : Address; AAD_Len : Unsigned_32;
                                    Buf : Address; Len : Unsigned_32; Tag : Address) return Result is
      Valid : Boolean := False;
   begin
      if Too_Long (AAD_Len) or else Too_Long (Len) then return Fail; end if;
      declare
         Raw  : Bytes_32 with Import, Address => Key;
         K    : constant Core.ChaCha20_Key := Core.Construct (Raw);
         N    : Bytes_12 with Import, Address => Nonce;
         A    : Byte_Seq (0 .. N32 (AAD_Len) - 1) with Import, Address => AAD;
         Data : Byte_Seq (0 .. N32 (Len) - 1) with Import, Address => Buf;
         T    : Bytes_16 with Import, Address => Tag;
         Copy : Byte_Seq_Access := new Byte_Seq'(Data);
      begin
         SPARKNaCl.Secretbox.Open (Data, Valid, T, Copy.all, Core.ChaCha20_IETF_Nonce (N), K, A);
         Free (Copy);
      end;
      return To_Result (Valid);
   end ChaCha20_Poly1305_Open;

   --  Key agreement ---------------------------------------------------------

   function X25519_Public (SK : Address; PK : Address) return Result is
      N : Bytes_32 with Import, Address => SK;
      Q : Bytes_32 with Import, Address => PK;
   begin
      SPARKTLSCrypto.X25519.Scalar_Mult_Base (Q, N);
      return OK;
   end X25519_Public;

   function X25519_Shared (SK : Address; Peer : Address; Secret : Address) return Result is
      N : Bytes_32 with Import, Address => SK;
      P : Bytes_32 with Import, Address => Peer;
      Q : Bytes_32 with Import, Address => Secret;
   begin
      SPARKTLSCrypto.X25519.Scalar_Mult (Q, N, P);
      --  RFC 7748 6.1: an all-zero result is a low-order peer point.
      return To_Result ((for some B of Q => B /= 0));
   end X25519_Shared;

   function P256_Public (SK : Address; Blind : Address; PK : Address) return Result is
      use SPARKTLSCrypto.P256.Point;
      K   : Bytes_32 with Import, Address => SK;
      B   : Byte_Seq (0 .. 39) with Import, Address => Blind;
      Enc : Byte_Seq (0 .. 64) with Import, Address => PK;
      Pt  : P256_Jacobian;
   begin
      P256_Mulgen_Blinded (Pt, K, B);
      P256_To_Affine (Pt);
      P256_Encode (Enc, Pt);
      return OK;
   end P256_Public;

   function P256_Shared (SK : Address; Peer : Address; Blind : Address; Secret : Address) return Result is
      use SPARKTLSCrypto.P256.Point;
      K     : Bytes_32 with Import, Address => SK;
      P     : Byte_Seq (0 .. 64) with Import, Address => Peer;
      B     : Byte_Seq (0 .. 39) with Import, Address => Blind;
      X     : Bytes_32 with Import, Address => Secret;
      Pt    : P256_Jacobian;
      Valid : SPARKNaCl.U32;
      Enc   : Byte_Seq (0 .. 64);
   begin
      --  As SPARKTLS's own key share: decode (on-curve check), multiply,
      --  and refuse the point at infinity (an all-zero x).
      P256_Decode (Pt, P, Valid);
      if Valid = 0 then return Fail; end if;
      P256_Mul_Blinded (Pt, K, B);
      P256_To_Affine (Pt);
      P256_Encode (Enc, Pt);
      if (for all I in N32 range 1 .. 32 => Enc (I) = 0) then return Fail; end if;
      X := Enc (1 .. 32);
      return OK;
   end P256_Shared;

   function P384_Public (SK : Address; Blind : Address; PK : Address) return Result is
      K   : Byte_Seq (0 .. 47) with Import, Address => SK;
      B   : Byte_Seq (0 .. 55) with Import, Address => Blind;
      Enc : Byte_Seq (0 .. 96) with Import, Address => PK;
   begin
      SPARKTLSCrypto.P384.Point.P384_Mulgen_Blinded (Enc, K, B);
      return OK;
   end P384_Public;

   function P384_Shared (SK : Address; Peer : Address; Blind : Address; Secret : Address) return Result is
      K     : Byte_Seq (0 .. 47) with Import, Address => SK;
      P     : Byte_Seq (0 .. 96) with Import, Address => Peer;
      B     : Byte_Seq (0 .. 55) with Import, Address => Blind;
      X     : Bytes_48 with Import, Address => Secret;
      Valid : Boolean;
   begin
      SPARKTLSCrypto.P384.Point.P384_ECDHE_Blinded (X, Valid, K, P, B);
      return To_Result (Valid);
   end P384_Shared;

   function MLKEM768_KeyGen (D : Address; Z : Address; EK : Address; DK : Address) return Result is
      package KEM renames MLKEM.ML_KEM_768;
      Seed_D : MLKEM.Bytes_32 with Import, Address => D;
      Seed_Z : MLKEM.Bytes_32 with Import, Address => Z;
      E      : KEM.MLKEM_Encapsulation_Key with Import, Address => EK;
      S      : KEM.MLKEM_Decapsulation_Key with Import, Address => DK;
      Pair   : KEM.MLKEM_Key;
   begin
      KEM.MLKEM_KeyGen (Seed_D, Seed_Z, Pair);
      E := Pair.EK;
      S := Pair.DK;
      return OK;
   end MLKEM768_KeyGen;

   function MLKEM768_Decaps (CT : Address; DK : Address; Secret : Address) return Result is
      package KEM renames MLKEM.ML_KEM_768;
      C : KEM.Ciphertext with Import, Address => CT;
      S : KEM.MLKEM_Decapsulation_Key with Import, Address => DK;
      K : MLKEM.Bytes_32 with Import, Address => Secret;
   begin
      KEM.MLKEM_Decaps (C, S, K);
      return OK;
   end MLKEM768_Decaps;

   --  Signatures ------------------------------------------------------------

   function RSA_Lengths_OK (Digest_Len, Modulus_Len, Signature_Len : Unsigned_32) return Boolean is
     (Digest_Len in 32 | 48 | 64
      and then Modulus_Len in 64 .. SPARKTLSCrypto.RSA.Max_RSA_Bytes
      and then Signature_Len = Modulus_Len);

   function RSA_PKCS1_Verify (Digest : Address; Digest_Len : Unsigned_32;
                              Modulus : Address; Modulus_Len : Unsigned_32; Exponent : Unsigned_32;
                              Signature : Address; Signature_Len : Unsigned_32) return Result is
   begin
      if not RSA_Lengths_OK (Digest_Len, Modulus_Len, Signature_Len) then return Fail; end if;
      declare
         H : Byte_Seq (0 .. N32 (Digest_Len) - 1) with Import, Address => Digest;
         N : Byte_Seq (0 .. N32 (Modulus_Len) - 1) with Import, Address => Modulus;
         S : Byte_Seq (0 .. N32 (Signature_Len) - 1) with Import, Address => Signature;
      begin
         return To_Result (SPARKTLSCrypto.RSA.Verify_PKCS1_v1_5
                             (H, N32 (Digest_Len), N, N32 (Modulus_Len), Exponent, S, N32 (Signature_Len)));
      end;
   end RSA_PKCS1_Verify;

   function RSA_PSS_Verify (Digest : Address; Digest_Len : Unsigned_32;
                            Modulus : Address; Modulus_Len : Unsigned_32; Exponent : Unsigned_32;
                            Signature : Address; Signature_Len : Unsigned_32) return Result is
      use SPARKTLSCrypto.RSA;
   begin
      if not RSA_Lengths_OK (Digest_Len, Modulus_Len, Signature_Len) then return Fail; end if;
      declare
         H : Byte_Seq (0 .. N32 (Digest_Len) - 1) with Import, Address => Digest;
         N : Byte_Seq (0 .. N32 (Modulus_Len) - 1) with Import, Address => Modulus;
         S : Byte_Seq (0 .. N32 (Signature_Len) - 1) with Import, Address => Signature;
         Alg : constant PSS_Hash :=
           (case Digest_Len is when 32 => PSS_SHA256, when 48 => PSS_SHA384, when others => PSS_SHA512);
      begin
         return To_Result (Verify_PSS (H, N32 (Digest_Len), Alg, N, N32 (Modulus_Len), Exponent,
                                       S, N32 (Signature_Len)));
      end;
   end RSA_PSS_Verify;

   function ECDSA_P256_Verify (Digest : Address; Digest_Len : Unsigned_32;
                               QX, QY, R, S : Address) return Result is
      use SPARKTLSCrypto.P256.ECDSA;
   begin
      if Digest_Len not in 32 | 48 | 64 then return Fail; end if;
      declare
         H  : Byte_Seq (0 .. N32 (Digest_Len) - 1) with Import, Address => Digest;
         X  : ECDSA_Sig_Half with Import, Address => QX;
         Y  : ECDSA_Sig_Half with Import, Address => QY;
         RR : ECDSA_Sig_Half with Import, Address => R;
         SS : ECDSA_Sig_Half with Import, Address => S;
         E  : constant Byte_Seq := Penny_ECDSA_Digest.Bits2Int (H, 32);
      begin
         return To_Result (Verify (Bytes_32 (E), X, Y, RR, SS));
      end;
   end ECDSA_P256_Verify;

   function ECDSA_P384_Verify (Digest : Address; Digest_Len : Unsigned_32;
                               QX, QY, R, S : Address) return Result is
   begin
      if Digest_Len not in 32 | 48 | 64 then return Fail; end if;
      declare
         H  : Byte_Seq (0 .. N32 (Digest_Len) - 1) with Import, Address => Digest;
         X  : Byte_Seq (0 .. 47) with Import, Address => QX;
         Y  : Byte_Seq (0 .. 47) with Import, Address => QY;
         RR : Byte_Seq (0 .. 47) with Import, Address => R;
         SS : Byte_Seq (0 .. 47) with Import, Address => S;
         E  : constant Byte_Seq := Penny_ECDSA_Digest.Bits2Int (H, 48);
      begin
         return To_Result (SPARKTLSCrypto.P384.ECDSA.Verify (Bytes_48 (E), X, Y, RR, SS));
      end;
   end ECDSA_P384_Verify;

   function Ed25519_Verify (PK : Address; M : Address; M_Len : Unsigned_32; Signature : Address) return Result is
      Valid : Boolean := False;
   begin
      if Too_Long (M_Len) then return Fail; end if;
      declare
         Key     : Bytes_32 with Import, Address => PK;
         Message : Byte_Seq (0 .. N32 (M_Len) - 1) with Import, Address => M;
         Sig     : Byte_Seq (0 .. 63) with Import, Address => Signature;
         --  Open takes the signed message: signature, then message.
         Signed  : Byte_Seq_Access := new Byte_Seq (0 .. 63 + N32 (M_Len));
         Opened  : Byte_Seq_Access := new Byte_Seq (0 .. 63 + N32 (M_Len));
         Length  : I32;
      begin
         Signed (0 .. 63) := Sig;
         Signed (64 .. Signed'Last) := Message;
         SPARKTLSCrypto.Ed25519.Open (Opened.all, Valid, Length, Signed.all, Key);
         Free (Signed);
         Free (Opened);
      end;
      return To_Result (Valid);
   end Ed25519_Verify;

   function Tiers return Unsigned_32 is
      function Bit (Present : Boolean; Tier : Unsigned_32) return Unsigned_32 is
        (if Present then Tier else 0);
   begin
      return Bit (SPARKTLSCrypto.AES_NI.Has_AESNI, Tier_AES_NI)
        or Bit (SPARKTLSCrypto.GHASH_NI.Has_PCLMULQDQ, Tier_PCLMULQDQ)
        or Bit (SPARKTLSCrypto.CPU.Has_BMI2_ADX, Tier_BMI2_ADX)
        or Bit (SPARKTLSCrypto.CPU.Has_AVX2, Tier_AVX2)
        or Bit (SPARKTLSCrypto.AES_GCM_AVX512.Has_AVX512_AES_GCM, Tier_AVX512_AES_GCM)
        or Bit (SPARKTLSCrypto.ChaCha20_AVX512.Has_AVX512_ChaCha20, Tier_AVX512_ChaCha20)
        or Bit (SPARKTLSCrypto.Poly1305_AVX512.Has_AVX512_Poly1305, Tier_AVX512_Poly1305);
   end Tiers;

   --  Prepared AES-GCM ------------------------------------------------------

   function AES_GCM_Prepare (Key : Address; Key_Len : Unsigned_32) return Address is
      C : GCM_Contexts.Object_Pointer;
   begin
      if Key_Len not in 16 | 32 then
         return System.Null_Address;
      end if;
      C := new GCM_Context;
      if Key_Len = 16 then
         declare
            Raw : Bytes_16 with Import, Address => Key;
         begin
            SPARKTLSCrypto.AES_GCM.Prepare_128 (C.Prepared, AES.Construct (Raw));
            C.Raw (0 .. 15) := Raw;
         end;
      else
         declare
            Raw : Bytes_32 with Import, Address => Key;
         begin
            SPARKTLSCrypto.AES_GCM.Prepare_256 (C.Prepared, AES.Construct (Raw));
            C.Raw := Raw;
         end;
      end if;
      C.Key_Len := Key_Len;
      return GCM_Contexts.To_Address (C);
   exception
      when Storage_Error => return System.Null_Address;
   end AES_GCM_Prepare;

   procedure AES_GCM_Free (Context : Address) is
      C : GCM_Contexts.Object_Pointer := GCM_Contexts.To_Pointer (Context);
   begin
      if C /= null then
         SPARKTLSCrypto.AES_GCM.Clear (C.Prepared);
         C.Raw := [others => 0];
         C.Key_Len := 0;
         Free (C);
      end if;
   end AES_GCM_Free;

   function AES_GCM_Seal_Prepared (Context : Address; Nonce : Address;
                                   AAD : Address; AAD_Len : Unsigned_32;
                                   Buf : Address; Len : Unsigned_32; Tag : Address) return Result is
      C : constant GCM_Contexts.Object_Pointer := GCM_Contexts.To_Pointer (Context);
   begin
      if C = null or else Too_Long (AAD_Len) or else Too_Long (Len) then
         return Fail;
      elsif Len = 0 then
         return AES_GCM_Seal (C.Raw'Address, C.Key_Len, Nonce, AAD, AAD_Len, Buf, 0, Tag);
      end if;
      declare
         N    : Bytes_12 with Import, Address => Nonce;
         A    : Byte_Seq (0 .. N32 (AAD_Len) - 1) with Import, Address => AAD;
         Data : Byte_Seq (0 .. N32 (Len) - 1) with Import, Address => Buf;
         T    : Bytes_16 with Import, Address => Tag;
      begin
         SPARKTLSCrypto.AES_GCM.Encrypt_Prepared (Data, T, N, C.Prepared, A);
      end;
      return OK;
   end AES_GCM_Seal_Prepared;

   function AES_GCM_Open_Prepared (Context : Address; Nonce : Address;
                                   AAD : Address; AAD_Len : Unsigned_32;
                                   Buf : Address; Len : Unsigned_32; Tag : Address) return Result is
      C     : constant GCM_Contexts.Object_Pointer := GCM_Contexts.To_Pointer (Context);
      Valid : Boolean := False;
   begin
      if C = null or else Too_Long (AAD_Len) or else Too_Long (Len) then
         return Fail;
      end if;
      declare
         N    : Bytes_12 with Import, Address => Nonce;
         A    : Byte_Seq (0 .. N32 (AAD_Len) - 1) with Import, Address => AAD;
         Data : Byte_Seq (0 .. N32 (Len) - 1) with Import, Address => Buf;
         T    : Bytes_16 with Import, Address => Tag;
      begin
         --  In place; on a forgery every byte of Data is zero (its
         --  postcondition), so no unauthenticated plaintext reaches rustls.
         SPARKTLSCrypto.AES_GCM.Decrypt_Prepared (Data, Valid, T, N, C.Prepared, A);
      end;
      return To_Result (Valid);
   end AES_GCM_Open_Prepared;

end Penny_Crypto;
