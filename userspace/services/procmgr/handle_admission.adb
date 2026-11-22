------------------------------------------------------------------------------
--  CuBit procmgr
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The launch handle's decisions: the lemma's proof is its contract.
------------------------------------------------------------------------------
pragma Ada_2022;
package body Handle_Admission with SPARK_Mode is

   procedure Lemma_User_Session_Observes_Only (F : Debug_Facts; Gate : Debugging_Gate) is null;

end Handle_Admission;
