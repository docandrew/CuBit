------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The image's system policy as a CCL type: a startup plan's
--  (policy (System_Policy debugging => Debugging.Development)), checked when
--  the plan is compiled (CCL.Typed_Settings, key system.policy) and read by
--  procmgr once at boot (CCL.Setting_Codecs.System_Policy). Fixed for the
--  boot: no running program can change it.
--
--  debugging (DBG-001, docs/debugging.md "The debugging gate"):
--  * Development: Debug_Observe within the session's debug envelope, as
--    built (development images);
--  * Production: debugging that needs the owner's approval per session; the
--    approval path is not built, so procmgr refuses as for Disabled, saying
--    so;
--  * Disabled: procmgr grants no debug-session authority and mints no debug
--    session (STIG and production images, once those image types exist).
--  A plan that declares no policy is a development plan: Development.
------------------------------------------------------------------------------
pragma Ada_2022;

package CCL.Interfaces.System_Policy with Pure, SPARK_Mode is
   TYPE_SOURCE : constant String :=
     "(type Debugging (enum Disabled Production Development)) " &
     "(type System_Policy (record (debugging Debugging Debugging.Development)))";
   ROOT_TYPE_NAME : constant String := "System_Policy";
   --  The typed-setting key the plan's policy is checked under.
   POLICY_KEY : constant String := "system.policy";
end CCL.Interfaces.System_Policy;
