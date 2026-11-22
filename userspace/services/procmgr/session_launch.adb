------------------------------------------------------------------------------
--  CuBit procmgr
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Launch-into-S: the lemmas' proofs are their contracts.
------------------------------------------------------------------------------
pragma Ada_2022;
package body Session_Launch with SPARK_Mode is

   procedure Lemma_No_Badge_From_Envelope (E : Envelope; Asked : LAtt.Rights_Mask) is null;

   procedure Lemma_User_Session_Bounded (K : Request_Kind; Asked : LAtt.Rights_Mask) is null;

end Session_Launch;
