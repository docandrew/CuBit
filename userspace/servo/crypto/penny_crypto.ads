------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Penny's cryptography (WEB-004): the C ABI of SPARKTLSCrypto, SPARKNaCl
--  and SPARKMLKEM (pinned, unmodified crates; flake.nix) for Penny's rustls
--  CryptoProvider and its digests (userspace/rust/penny-crypto). Only
--  calls into the crates and the proved bits2int (Penny_ECDSA_Digest): no
--  primitive is written here.
--
--  Every buffer is the caller's, borrowed for the call: an address and a
--  length in bytes (fixed-size buffers by their documented size). Results
--  are 1 for success (a valid signature, an authentic ciphertext) and 0
--  otherwise; a length beyond Max_Length is 0. Random input (scalars,
--  blinds, ML-KEM seeds) comes from the caller (the OS's getrandom).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Interfaces.C;
with System;

package Penny_Crypto with SPARK_Mode => Off is

   --  Every variable-length input: comfortably inside the crates' N32
   --  preconditions (they reserve up to 256 bytes of headroom).
   Max_Length : constant := 2 ** 30;

   subtype Result is Interfaces.C.int;
   subtype Address is System.Address;

   --  Digests: Out is 20, 32, 48 or 64 bytes.
   function SHA1 (M : Address; M_Len : Unsigned_32; Digest : Address) return Result
     with Export, Convention => C, External_Name => "penny_sha1";
   function SHA256 (M : Address; M_Len : Unsigned_32; Digest : Address) return Result
     with Export, Convention => C, External_Name => "penny_sha256";
   function SHA384 (M : Address; M_Len : Unsigned_32; Digest : Address) return Result
     with Export, Convention => C, External_Name => "penny_sha384";
   function SHA512 (M : Address; M_Len : Unsigned_32; Digest : Address) return Result
     with Export, Convention => C, External_Name => "penny_sha512";

   --  HMAC: Out is 32 or 48 bytes.
   function HMAC_SHA256 (K : Address; K_Len : Unsigned_32; M : Address; M_Len : Unsigned_32;
                         Mac : Address) return Result
     with Export, Convention => C, External_Name => "penny_hmac_sha256";
   function HMAC_SHA384 (K : Address; K_Len : Unsigned_32; M : Address; M_Len : Unsigned_32;
                         Mac : Address) return Result
     with Export, Convention => C, External_Name => "penny_hmac_sha384";

   --  AEAD, in place: Buf holds the plaintext (seal) or ciphertext (open)
   --  and receives the other; Tag is 16 bytes, Nonce 12, Key 16 or 32
   --  (AES-GCM) or 32 (ChaCha20-Poly1305). Open writes nothing usable to
   --  Buf unless it returns 1.
   function AES_GCM_Seal (Key : Address; Key_Len : Unsigned_32; Nonce : Address;
                          AAD : Address; AAD_Len : Unsigned_32;
                          Buf : Address; Len : Unsigned_32; Tag : Address) return Result
     with Export, Convention => C, External_Name => "penny_aes_gcm_seal";
   function AES_GCM_Open (Key : Address; Key_Len : Unsigned_32; Nonce : Address;
                          AAD : Address; AAD_Len : Unsigned_32;
                          Buf : Address; Len : Unsigned_32; Tag : Address) return Result
     with Export, Convention => C, External_Name => "penny_aes_gcm_open";
   --  AES-GCM with a prepared key (SPARKTLSCrypto.AES_GCM.Prepared_Key): the
   --  key schedule and GHASH powers are computed once per key, then every
   --  record is sealed or opened in place by the fused stripe kernels.
   --  Prepare returns an opaque context (Null_Address for a bad key length or
   --  a refused allocation); Free clears and frees it. Open_Prepared zeroes
   --  Buf unless it returns 1. Seal_Prepared of an empty record uses the
   --  one-shot path, since the prepared entry point takes a non-empty buffer.
   function AES_GCM_Prepare (Key : Address; Key_Len : Unsigned_32) return Address
     with Export, Convention => C, External_Name => "penny_aes_gcm_prepare";
   procedure AES_GCM_Free (Context : Address)
     with Export, Convention => C, External_Name => "penny_aes_gcm_free";
   function AES_GCM_Seal_Prepared (Context : Address; Nonce : Address;
                                   AAD : Address; AAD_Len : Unsigned_32;
                                   Buf : Address; Len : Unsigned_32; Tag : Address) return Result
     with Export, Convention => C, External_Name => "penny_aes_gcm_seal_prepared";
   function AES_GCM_Open_Prepared (Context : Address; Nonce : Address;
                                   AAD : Address; AAD_Len : Unsigned_32;
                                   Buf : Address; Len : Unsigned_32; Tag : Address) return Result
     with Export, Convention => C, External_Name => "penny_aes_gcm_open_prepared";
   function ChaCha20_Poly1305_Seal (Key : Address; Nonce : Address; AAD : Address; AAD_Len : Unsigned_32;
                                    Buf : Address; Len : Unsigned_32; Tag : Address) return Result
     with Export, Convention => C, External_Name => "penny_chacha20_poly1305_seal";
   function ChaCha20_Poly1305_Open (Key : Address; Nonce : Address; AAD : Address; AAD_Len : Unsigned_32;
                                    Buf : Address; Len : Unsigned_32; Tag : Address) return Result
     with Export, Convention => C, External_Name => "penny_chacha20_poly1305_open";

   --  Key agreement. X25519: 32-byte scalars, points and secrets.
   function X25519_Public (SK : Address; PK : Address) return Result
     with Export, Convention => C, External_Name => "penny_x25519_public";
   function X25519_Shared (SK : Address; Peer : Address; Secret : Address) return Result
     with Export, Convention => C, External_Name => "penny_x25519_shared";
   --  P-256: SK 32, Blind 40, points uncompressed (65), secret the x (32).
   function P256_Public (SK : Address; Blind : Address; PK : Address) return Result
     with Export, Convention => C, External_Name => "penny_p256_public";
   function P256_Shared (SK : Address; Peer : Address; Blind : Address; Secret : Address) return Result
     with Export, Convention => C, External_Name => "penny_p256_shared";
   --  P-384: SK 48, Blind 56, points uncompressed (97), secret 48.
   function P384_Public (SK : Address; Blind : Address; PK : Address) return Result
     with Export, Convention => C, External_Name => "penny_p384_public";
   function P384_Shared (SK : Address; Peer : Address; Blind : Address; Secret : Address) return Result
     with Export, Convention => C, External_Name => "penny_p384_shared";
   --  ML-KEM-768: seeds 32 + 32, EK 1184, DK 2400, ciphertext 1088, secret 32.
   function MLKEM768_KeyGen (D : Address; Z : Address; EK : Address; DK : Address) return Result
     with Export, Convention => C, External_Name => "penny_mlkem768_keygen";
   function MLKEM768_Decaps (CT : Address; DK : Address; Secret : Address) return Result
     with Export, Convention => C, External_Name => "penny_mlkem768_decaps";

   --  Signature verification (1: valid). RSA: big-endian modulus and
   --  signature of the same length (64 .. 1024 bytes), the digest of the
   --  signed message (32, 48 or 64 bytes); PSS hashes the same.
   function RSA_PKCS1_Verify (Digest : Address; Digest_Len : Unsigned_32;
                              Modulus : Address; Modulus_Len : Unsigned_32; Exponent : Unsigned_32;
                              Signature : Address; Signature_Len : Unsigned_32) return Result
     with Export, Convention => C, External_Name => "penny_rsa_pkcs1_verify";
   function RSA_PSS_Verify (Digest : Address; Digest_Len : Unsigned_32;
                            Modulus : Address; Modulus_Len : Unsigned_32; Exponent : Unsigned_32;
                            Signature : Address; Signature_Len : Unsigned_32) return Result
     with Export, Convention => C, External_Name => "penny_rsa_pss_verify";
   --  ECDSA: the message's digest (32, 48 or 64 bytes; bits2int to the
   --  curve), the public point's coordinates and r, s (32 or 48 each).
   function ECDSA_P256_Verify (Digest : Address; Digest_Len : Unsigned_32;
                               QX, QY, R, S : Address) return Result
     with Export, Convention => C, External_Name => "penny_ecdsa_p256_verify";
   function ECDSA_P384_Verify (Digest : Address; Digest_Len : Unsigned_32;
                               QX, QY, R, S : Address) return Result
     with Export, Convention => C, External_Name => "penny_ecdsa_p384_verify";
   --  Ed25519: PK 32, signature 64, the message.
   function Ed25519_Verify (PK : Address; M : Address; M_Len : Unsigned_32; Signature : Address) return Result
     with Export, Convention => C, External_Name => "penny_ed25519_verify";

   --  The hardware paths SPARKTLSCrypto chose from CPUID while elaborating
   --  (so only meaningful after penny_cryptoinit): one bit per tier, set
   --  when that tier's code runs instead of the proved portable code.
   Tier_AES_NI           : constant Unsigned_32 := 2 ** 0;
   Tier_PCLMULQDQ        : constant Unsigned_32 := 2 ** 1;
   Tier_BMI2_ADX         : constant Unsigned_32 := 2 ** 2;
   Tier_AVX2             : constant Unsigned_32 := 2 ** 3;
   Tier_AVX512_AES_GCM   : constant Unsigned_32 := 2 ** 4;
   Tier_AVX512_ChaCha20  : constant Unsigned_32 := 2 ** 5;
   Tier_AVX512_Poly1305  : constant Unsigned_32 := 2 ** 6;
   function Tiers return Unsigned_32
     with Export, Convention => C, External_Name => "penny_crypto_tiers";
end Penny_Crypto;
