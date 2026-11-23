------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  net.fetch's downloads (CCL.Interfaces.Net), one body per platform:
--  native/ drives tls.svc with submitted calls whose completions arrive in
--  the program's event loop (never blocking it) and writes the place
--  through the filesystem service; the Linux preview's host/ has neither
--  (Unavailable), a stand-in, not CuBit.
------------------------------------------------------------------------------
with Interfaces;

package CCL_Fetch is
   use Interfaces;

   --  Downloads one program follows at once (a named ceiling).
   MAXIMUM_FETCHES : constant := 4;
   MAXIMUM_TEXT : constant := 200;
   MAXIMUM_URL : constant := 8_192;
   SHA256_HEX_DIGITS : constant := 64;

   --  How a fetch ended, in CCL's Fetch_Outcome order.
   type Outcome is
     (Running, Stored, Not_Granted, Exists, Bad_Url, Too_Many_Redirects, Connect_Failed, Tls_Failed,
      Http_Status, Bad_Response, Too_Large, Truncated, Digest_Mismatch, Deadline, Cancelled, Write_Failed);
   subtype Ended is Outcome range Stored .. Outcome'Last;

   type Existing_Policy is (Refuse, Replace);
   type Start_Result is (Started, Not_Granted, Unavailable, Refused);

   type Short_Text is record
      Data : String (1 .. MAXIMUM_TEXT) := [others => ' '];
      Length : Natural range 0 .. MAXIMUM_TEXT := 0;
   end record;
   function Short (Text : String) return Short_Text;

   type Report is record
      Ended_As : Outcome := Running;
      Status : Natural := 0;
      Bytes : Unsigned_64 := 0;
      Expected_Known : Boolean := False;
      Expected : Unsigned_64 := 0;
      Redirects : Natural := 0;
      Sha256 : String (1 .. SHA256_HEX_DIGITS) := [others => ' '];
      Sha256_Known : Boolean := False;
      Url : String (1 .. MAXIMUM_URL) := [others => ' '];
      Url_Length : Natural range 0 .. MAXIMUM_URL := 0;
      Detail : Short_Text;
   end record;

   --  Starts a download of Url into Root/Path/Name. Pin_Hex is the
   --  expected SHA-256 in hex when Pinned. Why says why not (Started:
   --  empty); a URL that cannot be fetched is Refused.
   procedure Start
     (Url, Root, Path, Name : String; Pinned : Boolean; Pin_Hex : String; Existing : Existing_Policy;
      Max_Bytes : Unsigned_64; Max_Redirects : Natural; Deadline_Ms : Positive; Now_Ms : Unsigned_64;
      Id : out Unsigned_64; Result : out Start_Result; Why : out Short_Text);

   --  Where fetch Id is (Found False: not followed).
   procedure Poll (Id : Unsigned_64; Item : out Report; Found : out Boolean);
   procedure Cancel (Id : Unsigned_64);
   --  Let an ended fetch go (its row is free again).
   procedure Forget (Id : Unsigned_64);
   function Running return Boolean;
   --  Ends the fetches whose deadline passed; Next_Due: milliseconds until
   --  the next deadline (Unsigned_64'Last: none running).
   procedure Pump (Now_Ms : Unsigned_64; Next_Due : out Unsigned_64);
end CCL_Fetch;
