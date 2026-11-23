------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  net.fetch's URLs (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;

package body Fetch_Urls with SPARK_Mode is

   SCHEME : constant String := "https://";
   NETWORK_PATH : constant String := "//";
   CRLF : constant String := ASCII.CR & ASCII.LF;
   MAXIMUM_PORT_DIGITS : constant := 5;

   function Lower (C : Character) return Character renames CuBit.TLS_Scopes.Lower;

   function Hex (C : Character) return Boolean is (C in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F');

   function Starts_With_Ignoring_Case (Text, Prefix : String) return Boolean is
     (Text'Length >= Prefix'Length
      and then (for all K in 0 .. Prefix'Length - 1 =>
                  Lower (Text (Text'First + K)) = Prefix (Prefix'First + K)))
   with Pre => Prefix'Length >= 1;

   --  A port in decimal, without leading zeros.
   function Decimal (N : Port_Number) return String
   with Post => Decimal'Result'Length in 1 .. MAXIMUM_PORT_DIGITS
   is
      Text : String (1 .. MAXIMUM_PORT_DIGITS);
      V : Natural := N;
      First : Positive := MAXIMUM_PORT_DIGITS;
   begin
      for K in reverse 1 .. MAXIMUM_PORT_DIGITS loop
         pragma Loop_Invariant (First in K .. MAXIMUM_PORT_DIGITS and then V <= 65_535);
         Text (K) := Character'Val (Character'Pos ('0') + V mod 10);
         if V > 0 then
            First := K;
         end if;
         V := V / 10;
      end loop;
      return Text (First .. MAXIMUM_PORT_DIGITS);
   end Decimal;

   --  Rest as U's target: "/" first when it is empty or only a query, the
   --  fragment dropped, every byte a Target_Character and every '%' two
   --  hex digits.
   procedure Set_Target (U : in out Url; Rest : String; Why : out Refusal)
   with Pre => Rest'Last < Integer'Last,
        Post => (if Why = No_Refusal then
                   U.Target_Last >= 1 and then U.Target (1) = '/'
                   and then (for all C of U.Target (1 .. U.Target_Last) => Target_Character (C)))
                and then U.Host = U.Host'Old and then U.Host_Last = U.Host_Last'Old and then U.Port = U.Port'Old
   is
      Last : Integer := Rest'Last;
      Rooted : Boolean;
   begin
      for I in Rest'Range loop
         if Rest (I) = '#' then
            Last := I - 1;
            exit;
         end if;
      end loop;
      Rooted := Last >= Rest'First and then Rest (Rest'First) = '/';
      if Last >= Rest'First and then not Rooted and then Rest (Rest'First) /= '?' then
         Why := Bad_Target;
         return;
      end if;
      declare
         Target : constant String := (if Rooted then Rest (Rest'First .. Last) else "/" & Rest (Rest'First .. Last));
      begin
         if Target'Length > MAXIMUM_URL_BYTES then
            Why := Too_Long;
            return;
         end if;
         for I in Target'Range loop
            if not Target_Character (Target (I))
              or else (Target (I) = '%'
                       and then (I > Target'Last - 2 or else not Hex (Target (I + 1))
                                 or else not Hex (Target (I + 2))))
            then
               Why := Bad_Target;
               return;
            end if;
            pragma Loop_Invariant (for all J in Target'First .. I => Target_Character (Target (J)));
         end loop;
         U.Target_Last := Target'Length;
         U.Target (1 .. U.Target_Last) := Target;
         Why := No_Refusal;
      end;
   end Set_Target;

   procedure Parse (Text : String; U : out Url; Why : out Refusal) is
      First, Authority_Last, Colon, Host_Last : Natural;
   begin
      U := (others => <>);
      if Text'Length > MAXIMUM_URL_BYTES then
         Why := Too_Long;
         return;
      elsif not Starts_With_Ignoring_Case (Text, SCHEME) then
         Why := Not_Https;
         return;
      end if;
      First := Text'First + SCHEME'Length;
      Authority_Last := First - 1;
      while Authority_Last < Text'Last and then Text (Authority_Last + 1) not in '/' | '?' | '#' loop
         pragma Loop_Invariant (Authority_Last in First - 1 .. Text'Last - 1);
         pragma Loop_Variant (Increases => Authority_Last);
         Authority_Last := Authority_Last + 1;
      end loop;
      Colon := 0;
      for I in First .. Authority_Last loop
         if Text (I) = '@' then
            Why := Has_Userinfo;
            return;
         elsif Text (I) = ':' and then Colon = 0 then
            Colon := I;
         end if;
         pragma Loop_Invariant (Colon = 0 or else Colon in First .. I);
      end loop;
      Host_Last := (if Colon = 0 then Authority_Last else Colon - 1);
      if Host_Last < First or else Host_Last - First + 1 > MAXIMUM_HOST_BYTES then
         Why := Bad_Host;
         return;
      end if;
      U.Host_Last := Host_Last - First + 1;
      for K in 1 .. U.Host_Last loop
         U.Host (K) := Lower (Text (First + K - 1));
         pragma Loop_Invariant (for all J in 1 .. K => U.Host (J) not in 'A' .. 'Z');
      end loop;
      if not CuBit.TLS_Scopes.Valid_Name (U.Host (1 .. U.Host_Last)) then
         Why := Bad_Host;
         return;
      end if;
      if Colon /= 0 then
         declare
            Digits_Count : constant Natural := Authority_Last - Colon;
            Value : Natural := 0;
         begin
            if Digits_Count not in 1 .. MAXIMUM_PORT_DIGITS then
               Why := Bad_Port;
               return;
            end if;
            for I in Colon + 1 .. Authority_Last loop
               pragma Loop_Invariant (Value <= 99_999);
               if Text (I) not in '0' .. '9' or else Value > 9_999 then
                  Why := Bad_Port;
                  return;
               end if;
               Value := Value * 10 + (Character'Pos (Text (I)) - Character'Pos ('0'));
            end loop;
            if Value not in Port_Number then
               Why := Bad_Port;
               return;
            end if;
            U.Port := Value;
         end;
      end if;
      Set_Target (U, Text (Authority_Last + 1 .. Text'Last), Why);
      --  Checked above; again here, so the result's own proof is local.
      if Why = No_Refusal and then not CuBit.TLS_Scopes.Valid_Name (U.Host (1 .. U.Host_Last)) then
         Why := Bad_Host;
      end if;
   end Parse;

   procedure Resolve (Base : Url; Location : String; U : out Url; Why : out Refusal) is
   begin
      U := Base;
      if Location'Length > MAXIMUM_URL_BYTES - SCHEME'Length then
         U := (others => <>);
         Why := Too_Long;
      elsif Starts_With_Ignoring_Case (Location, SCHEME) then
         Parse (Location, U, Why);
      elsif Location'Length >= NETWORK_PATH'Length
        and then Location (Location'First .. Location'First + 1) = NETWORK_PATH
      then
         Parse ("https:" & Location, U, Why);
      elsif Location'Length >= 1 and then Location (Location'First) = '/' then
         Set_Target (U, Location, Why);
      else
         --  "http://...", "ftp:..." or a relative reference: refused.
         U := (others => <>);
         Why := (if (for some C of Location => C = ':') then Not_Https else Unsupported_Reference);
      end if;
      if Why = No_Refusal and then not CuBit.TLS_Scopes.Valid_Name (U.Host (1 .. U.Host_Last)) then
         Why := Bad_Host;
      end if;
   end Resolve;

   function Host_And_Port (U : Url; Always_Port : Boolean) return String is
     (U.Host (1 .. U.Host_Last) &
        (if Always_Port or else U.Port /= HTTPS_PORT then ":" & Decimal (U.Port) else ""))
   with Pre => U.Host_Last >= 1,
        Post => Host_And_Port'Result'Length <= MAXIMUM_HOST_BYTES + 1 + MAXIMUM_PORT_DIGITS;

   function Authority (U : Url) return String is (Host_And_Port (U, Always_Port => True));

   function Request (U : Url) return String is
     ("GET " & U.Target (1 .. U.Target_Last) & " HTTP/1.1" & CRLF &
      "Host: " & Host_And_Port (U, Always_Port => False) & CRLF &
      "User-Agent: " & USER_AGENT & CRLF &
      "Accept: */*" & CRLF &
      "Accept-Encoding: identity" & CRLF &
      "Connection: close" & CRLF & CRLF);

   --  A body length in decimal, without leading zeros.
   function Body_Decimal (N : Natural) return String
   with Post => Body_Decimal'Result'Length in 1 .. MAXIMUM_BODY_DIGITS
   is
      Text : String (1 .. MAXIMUM_BODY_DIGITS);
      V : Natural := N;
      First : Positive := MAXIMUM_BODY_DIGITS;
   begin
      for K in reverse 1 .. MAXIMUM_BODY_DIGITS loop
         pragma Loop_Invariant (First in K .. MAXIMUM_BODY_DIGITS);
         Text (K) := Character'Val (Character'Pos ('0') + V mod 10);
         if V > 0 then
            First := K;
         end if;
         V := V / 10;
      end loop;
      return Text (First .. MAXIMUM_BODY_DIGITS);
   end Body_Decimal;

   function Git_Request (U : Url; Service : Git_Service; Body_Length : Natural) return String is
      --  The repository's path without a trailing '/'.
      Base_Last : constant Natural :=
        (if U.Target (U.Target_Last) = '/' then U.Target_Last - 1 else U.Target_Last);
      Base : constant String := U.Target (1 .. Base_Last);
      Common : constant String :=
        "Host: " & Host_And_Port (U, Always_Port => False) & CRLF &
        "User-Agent: " & GIT_USER_AGENT & CRLF &
        "Accept-Encoding: identity" & CRLF &
        "Git-Protocol: version=2" & CRLF &
        "Connection: close" & CRLF;
   begin
      case Service is
         when Info_Refs =>
            return "GET " & Base & "/info/refs?service=git-upload-pack HTTP/1.1" & CRLF & Common &
              "Accept: */*" & CRLF & CRLF;
         when Upload_Pack =>
            return "POST " & Base & "/git-upload-pack HTTP/1.1" & CRLF & Common &
              "Accept: application/x-git-upload-pack-result" & CRLF &
              "Content-Type: application/x-git-upload-pack-request" & CRLF &
              "Content-Length: " & Body_Decimal (Body_Length) & CRLF & CRLF;
      end case;
   end Git_Request;

   function Image (U : Url) return String is
     (SCHEME & Host_And_Port (U, Always_Port => False) & U.Target (1 .. U.Target_Last));
end Fetch_Urls;
