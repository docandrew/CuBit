------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The native side of CCL_Fetch: the downloads this program follows, each
--  with the window it lends tls.svc, driven by the completions the desktop
--  platform's event loop delivers here (Deliver).
--
--  @description
--  A fetch is OPEN (resolve, connect, verify, one deferred call), WRITE
--  (the GET request), then READ after READ until the body is whole, each
--  submitted with capSubmit, never waited for. What arrives goes through
--  the proved response reader (Http_Responses): its body runs are hashed
--  (SPARKTLSCrypto's SHA-256) and written to a temporary file beside the
--  target, which is renamed only when the body is complete and its digest
--  matches the pin. Redirects (301, 302, 303, 307, 308) are followed up to
--  the caller's ceiling, each to an https URL tls.svc checks again against
--  this program's TLS scope. Any failure shuts the channel and removes the
--  temporary file.
------------------------------------------------------------------------------
with Interfaces;
with CuBit.Messages;
with CCL_Fetch;

package CCL_Fetch_Engine is
   use Interfaces;

   --  A completion this package submitted? Then it is taken (Consumed).
   procedure Deliver (Receipt : CuBit.Messages.CompletionEntry; Consumed : out Boolean);

   procedure Start
     (Url, Root, Path, Name : String; Pinned : Boolean; Pin_Hex : String;
      Existing : CCL_Fetch.Existing_Policy; Max_Bytes : Unsigned_64; Max_Redirects : Natural;
      Deadline_Ms : Positive; Now_Ms : Unsigned_64;
      Id : out Unsigned_64; Result : out CCL_Fetch.Start_Result; Why : out CCL_Fetch.Short_Text);
   procedure Poll (Id : Unsigned_64; Item : out CCL_Fetch.Report; Found : out Boolean);
   procedure Cancel (Id : Unsigned_64);
   procedure Forget (Id : Unsigned_64);
   function Running return Boolean;
   procedure Pump (Now_Ms : Unsigned_64; Next_Due : out Unsigned_64);
end CCL_Fetch_Engine;
