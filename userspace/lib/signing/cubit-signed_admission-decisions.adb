-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with SPARKTLSCrypto.Hashing.SHA384;

package body CuBit.Signed_Admission.Decisions with SPARK_Mode => On is

   function Copy_Of (File : Bytes; S : Certificate_Span) return X509.Byte_Seq is
      Result : X509.Byte_Seq (0 .. X509.N32 (S.Length) - 1) := [others => 0];
   begin
      for I in Result'Range loop
         Result (I) := File (S.First + File_Index (I));
         pragma Loop_Invariant
           (for all J in 0 .. I => Result (J) = File (S.First + File_Index (J)));
      end loop;
      return Result;
   end Copy_Of;

   function Payload_Digest (File : Bytes; Length : File_Length) return Digest is
     (SPARKTLSCrypto.Hashing.SHA384.Hash (File (0 .. Length - 1)));

   function Statement_Hash (E : Envelope; Leaf : X509.Byte_Seq) return Digest is
     (SPARKTLSCrypto.Hashing.SHA384.Hash
        (Statement_Of (E.Kind, E.Payload_Length, E.Payload_Digest,
                       Certificates.Certificate_Digest (Leaf))));

   function Is_Sorted (List : Digest_Array) return Boolean is
   begin
      for I in List'Range loop
         pragma Loop_Invariant
           (for all K in List'First .. I - 1 =>
              (for all J in List'Range => (if K < J then Revocations.Less_Spec (List (K), List (J)))));
         for J in I + 1 .. List'Last loop
            pragma Loop_Invariant
              (for all M in I + 1 .. J - 1 => Revocations.Less_Spec (List (I), List (M)));
            if not Revocations.Less (List (I), List (J)) then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Is_Sorted;

   function Revocations_Usable (Set : Revocation_Set) return Boolean is
     (Set.Keys < Positive'Last and then Set.Certificates < Positive'Last
      and then Set.Programs < Positive'Last
      and then Is_Sorted (Set.Revoked_Keys) and then Is_Sorted (Set.Revoked_Certificates)
      and then Is_Sorted (Set.Revoked_Programs));

   --  The executable form of Chain_Revoked, with the reason it found.
   procedure Check_Revoked
     (Set : Revocation_Set; Leaf : X509.Byte_Seq; Has_Intermediate : Boolean;
      Intermediate, Anchor_Certificate : X509.Byte_Seq; Payload : Digest;
      Found : out Boolean; Detail : out Refusal_Detail)
   with Pre  => Revocations.Well_Formed (Set)
                and then Certificates.Well_Formed (Leaf) and then Certificates.Well_Formed (Intermediate)
                and then Certificates.Well_Formed (Anchor_Certificate),
        Post => Found = Chain_Revoked (Set, Leaf, Has_Intermediate, Intermediate, Anchor_Certificate, Payload)
   is
      function Key_In (DER : X509.Byte_Seq) return Boolean is
        (Revocations.Contains (Set.Revoked_Keys, Certificates.Key_Digest (DER)))
      with Pre => Revocations.Well_Formed (Set) and then Certificates.Well_Formed (DER);
      function Certificate_In (DER : X509.Byte_Seq) return Boolean is
        (Revocations.Contains (Set.Revoked_Certificates, Certificates.Certificate_Digest (DER)))
      with Pre => Revocations.Well_Formed (Set) and then Certificates.Well_Formed (DER);
   begin
      Found := True;
      if Revocations.Contains (Set.Revoked_Programs, Payload) then
         Detail := Program_Revoked;
      elsif Key_In (Leaf) then
         Detail := Key_Revoked;
      elsif Certificate_In (Leaf) then
         Detail := Certificate_Revoked;
      elsif Has_Intermediate and then Key_In (Intermediate) then
         Detail := Key_Revoked;
      elsif Has_Intermediate and then Certificate_In (Intermediate) then
         Detail := Certificate_Revoked;
      elsif Key_In (Anchor_Certificate) then
         Detail := Key_Revoked;
      elsif Certificate_In (Anchor_Certificate) then
         Detail := Certificate_Revoked;
      else
         Found := False;
         Detail := Key_Revoked;
      end if;
   end Check_Revoked;

   function Verify
     (File : Bytes; Expected : Purpose; Anchors : Anchor_Array; Revoked_Set : Revocation_Set)
      return Verification
   is
      E : constant Envelope := Parse (File);

      function Refuse (Reason : Refusal_Reason; Detail : Refusal_Detail) return Verification is
        ((Verified => False, Reason => Reason, Detail => Detail));
   begin
      --  1. The footer and the envelope, nothing else.
      case E.Status is
         when Unsigned =>
            return Refuse (Not_Signed, E.Detail);
         when Malformed =>
            return Refuse (Envelope_Malformed, E.Detail);
         when Parsed =>
            null;
      end case;
      pragma Assert (Parsed (File, E));
      if E.Kind /= Expected then
         return Refuse (Signature_Invalid, Wrong_Purpose);
      end if;

      --  2. The payload is the one the envelope names.
      if Payload_Digest (File, E.Payload_Length) /= E.Payload_Digest then
         return Refuse (Digest_Mismatch, Payload_Changed);
      end if;

      --  3. The chain, to the first anchor it holds to.
      declare
         Leaf : constant X509.Byte_Seq := Leaf_Of (File, E);
         Intermediate : constant X509.Byte_Seq := Intermediate_Of (File, E);
         Has_Intermediate : constant Boolean := E.Count = 2;
         Found : Natural := 0;
      begin
         if not Certificates.Profile_Holds (Leaf, Certificates.Leaf_Role) then
            return Refuse (Untrusted_Builder, Leaf_Profile);
         elsif Has_Intermediate
           and then not Certificates.Profile_Holds (Intermediate, Certificates.Intermediate_Role)
         then
            return Refuse (Untrusted_Builder, Intermediate_Profile);
         end if;
         for A in Anchors'Range loop
            pragma Loop_Invariant (Found = 0);
            if Certificates.Chain_Holds (Leaf, Has_Intermediate, Intermediate, Anchor_DER (Anchors (A))) then
               Found := A;
               exit;
            end if;
         end loop;
         if Found = 0 then
            return Refuse (Untrusted_Builder, No_Anchor);
         end if;
         pragma Assert
           (Certificates.Chain_Holds (Leaf, Has_Intermediate, Intermediate, Anchor_DER (Anchors (Found))));

         --  4. The statement signature, by the leaf's key.
         if not Certificates.Signature_Holds (Leaf, Statement_Hash (E, Leaf), E.Signature) then
            return Refuse (Signature_Invalid, Statement_Signature);
         end if;

         --  5. Revocations.
         declare
            Is_Revoked : Boolean;
            Why : Refusal_Detail;
         begin
            Check_Revoked
              (Revoked_Set, Leaf, Has_Intermediate, Intermediate, Anchor_DER (Anchors (Found)),
               E.Payload_Digest, Is_Revoked, Why);
            if Is_Revoked then
               return Refuse (Revoked, Why);
            end if;
         end;

         return
           (Verified => True,
            Signer   =>
              (Leaf_Digest    => Certificates.Certificate_Digest (Leaf),
               Name           => Certificates.Subject_Name (Leaf),
               Anchor         => Found,
               Anchor_Digest  => Certificates.Certificate_Digest (Anchor_DER (Anchors (Found))),
               Payload_Digest => E.Payload_Digest,
               Payload_Length => E.Payload_Length));
      end;
   end Verify;

   function Admit
     (V : Verification; Anchors : Anchor_Array; Prefixes : Prefix_Array;
      Identity : Identity_Text; Requested : Requested_Authority) return Admission
   is
   begin
      if not V.Verified then
         return (Admitted => False, Reason => V.Reason, Detail => V.Detail);
      elsif V.Signer.Anchor not in Anchors'Range then
         return (Admitted => False, Reason => Untrusted_Builder, Detail => No_Anchor);
      end if;
      declare
         A : Anchor renames Anchors (V.Signer.Anchor);
      begin
         if not Scopes.In_Scope (Prefixes, A.First_Prefix, A.Last_Prefix, Identity) then
            return (Admitted => False, Reason => Outside_Builder_Scope, Detail => Identity_Outside_Scope);
         elsif not Scopes.Ceiling_Allows (A.Ceiling, Requested) then
            return (Admitted => False, Reason => Outside_Builder_Scope, Detail => Authority_Above_Ceiling);
         end if;
      end;
      return (Admitted => True, Signer => V.Signer);
   end Admit;

end CuBit.Signed_Admission.Decisions;
