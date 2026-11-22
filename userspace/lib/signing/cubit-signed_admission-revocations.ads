-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- Revocation lookup (SIGN-001): membership of a digest in a list sorted
-- strictly ascending, by binary search, proved equal to plain membership.
-------------------------------------------------------------------------------
package CuBit.Signed_Admission.Revocations with SPARK_Mode => On is
   use type Interfaces.Unsigned_8;
   use type SPARKNaCl.Byte_Seq;

   function Less_Spec (A, B : Digest) return Boolean is
     (for some I in Digest'Range =>
        A (I) < B (I) and then (for all J in Digest'First .. I - 1 => A (J) = B (J)))
   with Ghost;

   --  Bytewise (big-endian) order on digests.
   function Less (A, B : Digest) return Boolean
   with Post => Less'Result = Less_Spec (A, B);

   function Sorted (List : Digest_Array) return Boolean is
     (for all I in List'Range =>
        (for all J in List'Range => (if I < J then Less_Spec (List (I), List (J)))))
   with Ghost;

   function Member (List : Digest_Array; Item : Digest) return Boolean is
     (for some E of List => E = Item)
   with Ghost;

   function Contains (List : Digest_Array; Item : Digest) return Boolean
   with Pre  => Sorted (List) and then List'Last < Positive'Last,
        Post => Contains'Result = Member (List, Item);

   function Well_Formed (Set : Revocation_Set) return Boolean is
     (Sorted (Set.Revoked_Keys) and then Sorted (Set.Revoked_Certificates)
      and then Sorted (Set.Revoked_Programs)
      and then Set.Keys < Positive'Last and then Set.Certificates < Positive'Last
      and then Set.Programs < Positive'Last)
   with Ghost;

end CuBit.Signed_Admission.Revocations;
