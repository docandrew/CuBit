------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The typed-parameter binding of one program (docs/ccl-launch-parameters.md):
--  package Parameters, generated with the program's Startup package from its
--  manifest's canonical signature (CCL.Parameter_Signatures).
--
--  Parameters holds the parameter record and every type it reaches as Ada
--  types (Type_Codecs), the signature's Identity, a total Decode of the
--  value's canonical source (SPARK, proved by gnatprove like the setting
--  decoders), and Value, which reads this process's value from its launch
--  block (CuBit.Launch_Parameters) and decodes it, so the program reads
--  Parameters.Value.<field>. Require reads it once and stops the program
--  if it is missing or does not decode; the generated Startup body calls
--  it while elaborating, before the program's main.
--
--  A manifest that declares no parameters gets a Parameters package that
--  declares nothing: such a program accepts no arguments.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Strings.Unbounded;
with CuBit.Parameter_Signatures;

package Parameter_Bindings is
   --  Spec_Path names parameters.ads; the body goes beside it. Program is
   --  the manifest's identity. Root_Name and Signature are empty when the
   --  manifest declares no parameters.
   procedure Emit
     (Spec_Path, Program, Root_Name, Signature : String;
      Digest : CuBit.Parameter_Signatures.Identity;
      Success : out Boolean; Detail : out Ada.Strings.Unbounded.Unbounded_String);
end Parameter_Bindings;
