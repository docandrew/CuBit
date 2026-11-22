-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- The Linux host's side of signing (SIGN-001, docs/signed-binaries.md
-- section 7), shared by ccl-sign and ccl-build: the signing directory,
-- files written atomically with their modes, randomness and the clock.
-- Everything that decides is in the proved units; this package only reads
-- and writes.
--
-- The signing directory (outside the repository; make signing-key makes
-- it, mode 0700):
--   builder-ca.der   the "built by" CA, the anchor an image names
--   builder-ca.key   its key (0600), used only by init to issue the leaf
--   builder.der      the code-signing leaf
--   builder.key      its key (0600)
--   builder-intermediate.der   optional: a CA between the anchor and the leaf
-- No key is ever printed or logged.
-------------------------------------------------------------------------------
with Ada.Finalization;
with SPARKNaCl;
with X509;
with CuBit.Signed_Admission;
with CuBit.Signing_Keys;

package Host_Signing is

   Signing_Error : exception;

   CA_Certificate_Name : constant String := "builder-ca.der";
   CA_Key_Name : constant String := "builder-ca.key";
   Leaf_Certificate_Name : constant String := "builder.der";
   Leaf_Key_Name : constant String := "builder.key";
   Intermediate_Certificate_Name : constant String := "builder-intermediate.der";

   type Bytes_Access is access SPARKNaCl.Byte_Seq;
   procedure Free (B : in out Bytes_Access);

   --  A whole file, indexed from 0; Signing_Error when it is longer than
   --  Limit or cannot be read.
   function Read_File (Path : String; Limit : Natural := CuBit.Signed_Admission.Max_File_Length)
     return Bytes_Access;

   --  Path's bytes become Data (then Trailer, when given) through a
   --  temporary file in the same directory and a rename; Mode is the new
   --  file's permission bits from creation on.
   Private_File : constant := 8#600#;
   Public_File : constant := 8#644#;
   Executable_File : constant := 8#755#;
   procedure Write_File
     (Path : String; Data : SPARKNaCl.Byte_Seq; Mode : Natural;
      Trailer : SPARKNaCl.Byte_Seq := [1 .. 0 => 0]);
   function Mode_Of (Path : String) return Natural;

   --  Makes Directory (and missing parents), the last with mode 0700, or
   --  checks an existing one is empty; Signing_Error otherwise.
   procedure Make_Private_Directory (Directory : String);

   procedure Random (Output : out SPARKNaCl.Byte_Seq);
   function Mask return CuBit.Signing_Keys.Blind;
   function Now return X509.Date_Time;

   function To_X509 (B : SPARKNaCl.Byte_Seq) return X509.Byte_Seq;
   function Hex (B : SPARKNaCl.Byte_Seq) return String;

   --  A loaded signing directory. The key is wiped when the object goes.
   type Signer is new Ada.Finalization.Limited_Controlled with record
      Key : CuBit.Signing_Keys.Scalar := [others => 0];
      Leaf : CuBit.Signed_Admission.Certificate_Buffer := [others => 0];
      Leaf_Length : X509.N32 := 0;
      Intermediate : CuBit.Signed_Admission.Certificate_Buffer := [others => 0];
      Intermediate_Length : X509.N32 := 0;
      Anchor : CuBit.Signed_Admission.Certificate_Buffer := [others => 0];
      Anchor_Length : X509.N32 := 0;
   end record;
   overriding procedure Finalize (S : in out Signer);

   --  Reads Directory: the leaf key must be strong, be the leaf's key and
   --  pass the pairwise test; the leaf must chain to builder-ca.der.
   --  Signing_Error, naming make signing-key, when there is no directory.
   procedure Load (S : in out Signer; Directory : String);

   function Leaf_DER (S : Signer) return X509.Byte_Seq;
   function Anchor_DER (S : Signer) return X509.Byte_Seq;

   --  Payload signed by S: the envelope and footer to append, after Sign
   --  has checked that the result verifies under S's own anchor.
   function Sign
     (S : Signer; Payload : SPARKNaCl.Byte_Seq;
      Kind : CuBit.Signed_Admission.Purpose := CuBit.Signed_Admission.Program)
      return SPARKNaCl.Byte_Seq;

   --  Whether File (a whole file) carries a footer at all.
   function Has_Footer (File : SPARKNaCl.Byte_Seq) return Boolean;

   --  The verdict on File under the given anchors (Host_Development, every
   --  identity, the hardware ceiling), no revocations.
   type Anchor_List is array (Positive range <>) of Bytes_Access;
   function Verify
     (File : SPARKNaCl.Byte_Seq; Anchors : Anchor_List;
      Kind : CuBit.Signed_Admission.Purpose := CuBit.Signed_Admission.Program)
      return CuBit.Signed_Admission.Verification;

end Host_Signing;
