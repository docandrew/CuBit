------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git clone over HTTPS on CuBit (SELF-003 step 2): the clones this
--  program follows, each a Git_Clone_Sessions conversation carried over
--  tls.svc and written through the filesystem service. A plain typed Ada
--  API with no CCL in it: git.clone (CCL_Git_Bindings) is a thin binding
--  over it, and a service another front end calls (jj) can wrap it too.
--
--  @description
--  Each of the three exchanges (gitprotocol-http: GET info/refs, then two
--  POSTs to git-upload-pack) is OPEN (resolve, connect, verify), WRITE
--  (the request, its body after it, a window at a time), then READ after
--  READ until the response is whole, each call submitted with capSubmit
--  and never waited for: the completions arrive in the program's event
--  loop (Deliver). Responses go through the proved HTTP reader
--  (Http_Responses), their bodies to the session, which reads them with
--  the proved git readers and writes the clone under ".DIR.cloning",
--  renamed to DIR only when everything checked out. The advertisement may
--  be redirected (up to the caller's ceiling), each target an https URL
--  tls.svc checks against the program's TLS scope; the POSTs go to the
--  repository the advertisement came from. Any failure removes what was
--  made. Every limit is the caller's.
------------------------------------------------------------------------------
with Interfaces;
with CuBit.Messages;
with Git_Clone_Reports; use Git_Clone_Reports;

package Git_Clone_Engine is
   use Interfaces;

   --  Clones one program follows at once (a named ceiling).
   MAXIMUM_CLONES : constant := 2;

   --  A clone of the repository at Url into Root/Path/Directory. Refused:
   --  a URL that cannot be cloned (https, no query); Not_Granted: no TLS
   --  service. A place that may not be written ends the clone at once
   --  (Write_Failed), before any connection.
   procedure Start
     (Url, Root, Path, Directory : String; Ceiling : Limits; Now_Ms : Unsigned_64;
      Id : out Unsigned_64; Result : out Start_Result; Why : out Short_Text);

   --  Where clone Id is (Found False: not followed).
   procedure Poll (Id : Unsigned_64; Item : out Report; Found : out Boolean);
   procedure Cancel (Id : Unsigned_64);
   --  Let an ended clone go (its row is free again).
   procedure Forget (Id : Unsigned_64);
   function Running return Boolean;
   --  Ends the clones whose deadline passed; Next_Due: milliseconds until
   --  the next deadline (Unsigned_64'Last: none running).
   procedure Pump (Now_Ms : Unsigned_64; Next_Due : out Unsigned_64);

   --  A completion this package submitted? Then it is taken (Consumed).
   procedure Deliver (Receipt : CuBit.Messages.CompletionEntry; Consumed : out Boolean);
end Git_Clone_Engine;
