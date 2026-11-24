------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  What a git clone over HTTPS is asked to do and how it went: the plain
--  types of Git_Clone_Engine's API (CuBit) and of its stand-ins, shared by
--  everything that starts or watches a clone (git.clone in CCL, a future
--  service for jj).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;

package Git_Clone_Reports with Pure is
   use Interfaces;

   MAXIMUM_TEXT : constant := 200;
   MAXIMUM_URL : constant := 8_192;

   --  How a clone ended, in CCL's Git_Outcome order.
   type Outcome is
     (Running, Cloned, Not_Granted, Target_Exists, Bad_Url, Bad_Directory, Too_Many_Redirects, Connect_Failed,
      Tls_Failed, Http_Status, Bad_Response, Truncated, Deadline, Cancelled, Write_Failed, Not_Smart_Http,
      Bad_Refs, Bad_Fetch, Remote_Error, Bad_Pack, Bad_Delta, Too_Many_Objects, Too_Large, Missing_Object,
      Bad_Object, Path_Too_Long, No_Refs);
   subtype Ended is Outcome range Cloned .. Outcome'Last;

   --  The repository's object format, in CCL's Git_Format order.
   type Object_Format is (Sha1, Sha256);

   type Start_Result is (Started, Not_Granted, Unavailable, Refused);

   type Limits is record
      Maximum_Refs : Natural := 0;
      Maximum_Objects : Natural := 0;
      --  Each response body, and the pack with every object inflated.
      Maximum_Bytes : Unsigned_64 := 0;
      Maximum_Redirects : Natural := 0;
      Deadline_Ms : Positive := 1;
   end record;

   type Short_Text is record
      Data : String (1 .. MAXIMUM_TEXT) := [others => ' '];
      Length : Natural range 0 .. MAXIMUM_TEXT := 0;
   end record;
   function Short (Text : String) return Short_Text is
     ((Data => [for I in 1 .. MAXIMUM_TEXT => (if I <= Text'Length then Text (Text'First + I - 1) else ' ')],
       Length => Natural'Min (Text'Length, MAXIMUM_TEXT)));

   type Report is record
      Ended_As : Outcome := Running;
      --  The last HTTP status.
      Status : Natural := 0;
      Refs : Natural := 0;
      Objects : Natural := 0;
      Files : Natural := 0;
      --  Response body bytes received.
      Bytes : Unsigned_64 := 0;
      Format : Object_Format := Sha1;
      Redirects : Natural := 0;
      --  The repository's URL, after redirects.
      Url : String (1 .. MAXIMUM_URL) := [others => ' '];
      Url_Length : Natural range 0 .. MAXIMUM_URL := 0;
      Detail : Short_Text;
   end record;
end Git_Clone_Reports;
