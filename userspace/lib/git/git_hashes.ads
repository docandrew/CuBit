------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The hash of a repository's object format: SHA-1 or SHA-256, both from
--  SPARKTLSCrypto (verified; never a hand-written primitive). Names objects
--  (Git_Objects), checks a pack's trailer (Git_Packs) and makes the
--  checksums of the files a clone writes (Git_Clones).
------------------------------------------------------------------------------
pragma Ada_2022;
with Inflate_Streams; use Inflate_Streams;
with Git_Protocol; use Git_Protocol;
private with SPARKTLSCrypto.Hashing.SHA1;
private with SPARKTLSCrypto.Hashing.SHA256;

package Git_Hashes with SPARK_Mode is
   use type Byte;
   type Context is private;

   function Format (C : Context) return Object_Format;

   procedure Start (C : out Context; F : Object_Format)
   with Post => Format (C) = F;
   procedure Update (C : in out Context; Data : Byte_Array)
   with Pre => Data'Last < Positive'Last - 128, Post => Format (C) = Format (C'Old);
   --  The digest, as an object name of the format (zeros after it).
   procedure Finish (C : in out Context; Id : out Object_Id)
   with Post => Format (C) = Format (C'Old)
                  and then (for all K in Id_Bytes (Format (C)) + 1 .. MAXIMUM_ID_BYTES => Id (K) = 0);
private
   type Context is record
      Format : Object_Format := Sha1_Format;
      Sha1 : SPARKTLSCrypto.Hashing.SHA1.Context;
      Sha256 : SPARKTLSCrypto.Hashing.SHA256.Context;
   end record;
   function Format (C : Context) return Object_Format is (C.Format);
end Git_Hashes;
