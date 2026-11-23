------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git.clone's clones (CCL.Interfaces.Git), one body per platform: native/
--  is Git_Clone_Engine (tls.svc and the filesystem service, completions in
--  the program's event loop); the Linux preview's host/ has neither
--  (Unavailable), a stand-in, not CuBit.
------------------------------------------------------------------------------
with Interfaces;
with Git_Clone_Reports; use Git_Clone_Reports;

package CCL_Git is
   use Interfaces;

   procedure Start
     (Url, Root, Path, Directory : String; Ceiling : Limits; Now_Ms : Unsigned_64;
      Id : out Unsigned_64; Result : out Start_Result; Why : out Short_Text);
   procedure Poll (Id : Unsigned_64; Item : out Report; Found : out Boolean);
   procedure Cancel (Id : Unsigned_64);
   procedure Forget (Id : Unsigned_64);
   function Running return Boolean;
   procedure Pump (Now_Ms : Unsigned_64; Next_Due : out Unsigned_64);
end CCL_Git;
