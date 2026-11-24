------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  net.fetch's URLs (SELF-003 step 1): "https://HOST[:PORT]/PATH[?QUERY]"
--  checked into a host tls.svc may be asked for, a port, and a request
--  target, and the HTTP/1.1 GET request made from them.
--
--  @description
--  RFC 3986 (URI generic syntax) restricted to what net.fetch accepts:
--  * the https scheme only (case-insensitive); never http, so no redirect
--    can downgrade a fetch;
--  * a host that is a DNS name (CuBit.TLS_Scopes.Valid_Name: authority is
--    granted by name, so no IP literal, no IPv6 brackets), lowercased;
--  * no userinfo ("user@host"): credentials never travel in a URL;
--  * a target of RFC 3986 path and query characters (percent-encodings
--    with two hex digits), the fragment dropped.
--  Proved at level 2 (tests/fetch-codecs): every accepted host is a Valid_Name
--  and every target character a Target_Character, so the request has no
--  CR, LF or space where it does not put one itself (no header injection).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.TLS_Scopes;

package Fetch_Urls with SPARK_Mode is

   --  Named ceilings: RFC 9112, section 3 recommends request lines of at
   --  least 8000 octets; tls.svc's names are at most 64 characters.
   MAXIMUM_URL_BYTES : constant := 8_192;
   MAXIMUM_HOST_BYTES : constant := CuBit.TLS_Scopes.Maximum_Name;
   --  RFC 9110, section 4.2.2: https's default port.
   HTTPS_PORT : constant := 443;

   subtype Port_Number is Natural range 1 .. 65_535;
   subtype Host_Length is Natural range 0 .. MAXIMUM_HOST_BYTES;
   subtype Target_Length is Natural range 0 .. MAXIMUM_URL_BYTES;

   type Refusal is
     (No_Refusal,
      Too_Long,          --  beyond MAXIMUM_URL_BYTES
      Not_Https,         --  another scheme, or none
      Has_Userinfo,      --  "user@host"
      Bad_Host,          --  not a DNS name tls.svc can be asked for
      Bad_Port,          --  not 1 .. 65535 in decimal
      Bad_Target,        --  a byte RFC 3986 does not allow in a path or query
      Unsupported_Reference);  --  a relative redirect ("x/y", "../y")

   --  The reason's name (native builds discard enumeration images).
   function Name (Why : Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Too_Long => "Too_Long",
         when Not_Https => "Not_Https",
         when Has_Userinfo => "Has_Userinfo",
         when Bad_Host => "Bad_Host",
         when Bad_Port => "Bad_Port",
         when Bad_Target => "Bad_Target",
         when Unsupported_Reference => "Unsupported_Reference");

   --  RFC 3986 unreserved, sub-delims, ':', '@', '/', '?', and '%' (which
   --  Parse admits only before two hex digits).
   function Target_Character (C : Character) return Boolean is
     (C in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~'
         | '!' | '$' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | ';' | '='
         | ':' | '@' | '/' | '?' | '%');

   type Url is record
      Host : String (1 .. MAXIMUM_HOST_BYTES) := [others => ' '];
      Host_Last : Host_Length := 0;
      Port : Port_Number := HTTPS_PORT;
      Target : String (1 .. MAXIMUM_URL_BYTES) := [others => ' '];
      Target_Last : Target_Length := 0;
   end record;

   function Well_Formed (U : Url) return Boolean is
     (U.Host_Last >= 1 and then CuBit.TLS_Scopes.Valid_Name (U.Host (1 .. U.Host_Last))
      and then (for all C of U.Host (1 .. U.Host_Last) => C not in 'A' .. 'Z')
      and then U.Target_Last >= 1 and then U.Target (1) = '/'
      and then (for all C of U.Target (1 .. U.Target_Last) => Target_Character (C)));

   procedure Parse (Text : String; U : out Url; Why : out Refusal)
   with Pre => Text'Last < Integer'Last,
        Post => (if Why = No_Refusal then Well_Formed (U));

   --  The URL a redirect's Location names, from the URL that answered: an
   --  absolute https URL, a network-path ("//host/x") or an absolute path
   --  ("/x", the same host and port). Anything else is refused.
   procedure Resolve (Base : Url; Location : String; U : out Url; Why : out Refusal)
   with Pre => Well_Formed (Base) and then Location'Last < Integer'Last,
        Post => (if Why = No_Refusal then Well_Formed (U));

   --  "HOST:PORT", as tls.svc's OPEN takes it.
   function Authority (U : Url) return String
   with Pre => Well_Formed (U),
        Post => Authority'Result'Length <= MAXIMUM_HOST_BYTES + 6;

   --  The GET request: the target, Host, identity coding only, and
   --  Connection: close (one request per connection).
   USER_AGENT : constant String := "CuBit-net.fetch/1";
   function Request (U : Url) return String
   with Pre => Well_Formed (U),
        Post => Request'Result'Length <= MAXIMUM_URL_BYTES + 256;

   --  git's smart HTTP (SELF-003 step 2, gitprotocol-http): the two requests
   --  a clone makes of the repository at U, each asking for protocol
   --  version 2 ("Git-Protocol: version=2"): GET U/info/refs?service=
   --  git-upload-pack (the capability advertisement), and POST
   --  U/git-upload-pack with a request body of Body_Length bytes. A
   --  repository URL has no query (its paths are appended to it); a
   --  trailing '/' is dropped first.
   function Git_Repository (U : Url) return Boolean is
     (Well_Formed (U) and then (for all C of U.Target (1 .. U.Target_Last) => C /= '?'));
   type Git_Service is (Info_Refs, Upload_Pack);
   GIT_USER_AGENT : constant String := "CuBit-git/1";
   MAXIMUM_BODY_DIGITS : constant := 10;
   function Git_Request (U : Url; Service : Git_Service; Body_Length : Natural) return String
   with Pre => Git_Repository (U),
        Post => Git_Request'Result'Length <= MAXIMUM_URL_BYTES + 512;

   --  The whole URL again, for reports.
   function Image (U : Url) return String
   with Pre => Well_Formed (U),
        Post => Image'Result'Length <= MAXIMUM_URL_BYTES + 100;
end Fetch_Urls;
