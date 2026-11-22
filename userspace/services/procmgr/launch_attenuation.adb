------------------------------------------------------------------------------
--  CuBit procmgr
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The attenuation rule's lemmas: their proofs are their contracts.
------------------------------------------------------------------------------
pragma Ada_2022;
package body Launch_Attenuation with SPARK_Mode is

   procedure Lemma_No_Amplification
     (Masks : Held_Masks; Is_Provider : Boolean; Asked : Rights_Mask; Optional : Boolean) is null;

   procedure Lemma_Read_Only (Asked : Rights_Mask) is null;

end Launch_Attenuation;
