------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git.clone on CuBit: Git_Clone_Engine does the work.
------------------------------------------------------------------------------
with Git_Clone_Engine;

package body CCL_Git is
   procedure Start
     (Url, Root, Path, Directory : String; Ceiling : Limits; Now_Ms : Unsigned_64;
      Id : out Unsigned_64; Result : out Start_Result; Why : out Short_Text) renames Git_Clone_Engine.Start;
   procedure Poll (Id : Unsigned_64; Item : out Report; Found : out Boolean) renames Git_Clone_Engine.Poll;
   procedure Cancel (Id : Unsigned_64) renames Git_Clone_Engine.Cancel;
   procedure Forget (Id : Unsigned_64) renames Git_Clone_Engine.Forget;
   function Running return Boolean renames Git_Clone_Engine.Running;
   procedure Pump (Now_Ms : Unsigned_64; Next_Due : out Unsigned_64) renames Git_Clone_Engine.Pump;
end CCL_Git;
