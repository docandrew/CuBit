------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The object format's hash (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with SPARKNaCl;

package body Git_Hashes with SPARK_Mode is
   package SHA1 renames SPARKTLSCrypto.Hashing.SHA1;
   package SHA256 renames SPARKTLSCrypto.Hashing.SHA256;

   procedure Start (C : out Context; F : Object_Format) is
   begin
      C.Format := F;
      SHA1.Init (C.Sha1);
      SHA256.Init (C.Sha256);
   end Start;

   procedure Update (C : in out Context; Data : Byte_Array) is
   begin
      if Data'Length > 0 then
         case C.Format is
            when Sha1_Format => SHA1.Update (C.Sha1, SPARKNaCl.Byte_Seq (Data));
            when Sha256_Format => SHA256.Update (C.Sha256, SPARKNaCl.Byte_Seq (Data));
         end case;
      end if;
   end Update;

   procedure Finish (C : in out Context; Id : out Object_Id) is
   begin
      Id := NO_ID;
      case C.Format is
         when Sha1_Format =>
            declare
               Digest : SHA1.Digest;
            begin
               SHA1.Final (C.Sha1, Digest);
               for K in 1 .. SHA1_ID_BYTES loop
                  Id (K) := Digest (SPARKNaCl.N32 (K - 1));
                  pragma Loop_Invariant (for all J in SHA1_ID_BYTES + 1 .. MAXIMUM_ID_BYTES => Id (J) = 0);
               end loop;
            end;
         when Sha256_Format =>
            declare
               Digest : SHA256.Digest;
            begin
               SHA256.Final (C.Sha256, Digest);
               for K in 1 .. SHA256_ID_BYTES loop
                  Id (K) := Digest (SPARKNaCl.N32 (K - 1));
               end loop;
            end;
      end case;
   end Finish;
end Git_Hashes;
