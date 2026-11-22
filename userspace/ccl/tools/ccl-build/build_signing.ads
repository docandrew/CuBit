-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- ccl-build's sign step (SIGN-001, docs/signed-binaries.md section 7): a
-- program's executable, once linked, becomes the same bytes with the
-- envelope appended, signed by the leaf in the signing directory
-- (--signing DIR). Deterministic (RFC 6979), so the signed object is as
-- reproducible as the payload. Two bodies: linux signs with the host key
-- (userspace/lib/signing/host); cubit has no signing key until secrets.svc
-- signs (SIGN-001 slice 5) and refuses.
-------------------------------------------------------------------------------
package Build_Signing is

   Signing_Unavailable : exception;

   --  Output becomes Input signed with the key in Directory; Input must be
   --  unsigned. Raises Signing_Unavailable (with the reason) when it cannot.
   procedure Sign (Directory, Input, Output : String);

end Build_Signing;
