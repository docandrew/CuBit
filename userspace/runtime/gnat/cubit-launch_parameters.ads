------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  This process's typed parameters, from its launch block
--  (docs/ccl-launch-parameters.md): what the generated package Parameters
--  reads.
--
--  @description
--  Get finds the parameters section (CuBit.Parameter_Sections) in the
--  trailer of the launch block the kernel mapped (after the ring table,
--  before the description), checks it is well formed and carries the
--  identity of the signature the program was built with, and copies the
--  value's canonical source out once (a single fetch: the binding decodes
--  the copy). Refuse reports why a program could not have its parameters
--  and stops it: procmgr refuses such a launch first, so this only catches
--  a launch that bypassed or outran it.
--
--  No secondary stack: services built with No_Secondary_Stack run it too.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with CuBit.Parameter_Sections;
with CuBit.Parameter_Signatures;

package CuBit.Launch_Parameters is

   --  sysexits.h's EX_CONFIG, the exit code of a startup table that falls
   --  short too (CuBit.Startup_Check.Startup_Refused).
   Parameters_Refused : constant Interfaces.Unsigned_64 := 78;

   type Outcome is
     (Found,
      --  The process has no valid launch block (the start code kept none).
      No_Launch_Block,
      --  Its launch block has no parameters section.
      No_Parameters,
      --  The section is malformed.
      Malformed_Section,
      --  The section's value was checked against another signature.
      Other_Signature,
      --  The value is not the canonical source of the parameter record
      --  (reported by the binding after Decode).
      Undecodable_Value);

   procedure Get
     (Expected : CuBit.Parameter_Signatures.Identity;
      Value    : out String;
      Length   : out Natural;
      Result   : out Outcome)
   with Pre  => Value'First = 1 and then Value'Length = CuBit.Parameter_Sections.Maximum_Value_Bytes,
        Post => Length <= Value'Length and then (if Result /= Found then Length = 0);

   --  Print why Program (the manifest's identity) cannot run with its
   --  parameters, and stop with Parameters_Refused.
   procedure Refuse (Program : String; Why : Outcome) with No_Return;

end CuBit.Launch_Parameters;
