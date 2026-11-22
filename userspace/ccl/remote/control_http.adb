package body Control_HTTP with SPARK_Mode is
   function Lower (Text : String) return String is
      Result : String (Text'Range) := Text;
   begin
      for C of Result loop
         if C in 'A' .. 'Z' then C := Character'Val (Character'Pos (C) + 32); end if;
      end loop;
      return Result;
   end Lower;
   function Trim (Text : String) return String is
      First, Last : Natural := 0;
   begin
      for I in Text'Range loop
         if Text (I) /= ' ' then First := I; exit; end if;
      end loop;
      if First = 0 then return ""; end if;
      for I in reverse Text'Range loop
         if Text (I) /= ' ' then Last := I; exit; end if;
      end loop;
      return Text (First .. Last);
   end Trim;
   CRLF : constant String := ASCII.CR & ASCII.LF;
   Origin_Header : constant String :=
     "Access-Control-Allow-Origin: " & Development_Origin & CRLF & "Vary: Origin" & CRLF;
   Closing : constant String := "Connection: close" & CRLF &
     "Cache-Control: no-store" & CRLF & "X-Content-Type-Options: nosniff" & CRLF;
   function Response_Header (Length : Natural) return String is
      Decimal : constant String := Length'Image;
   begin
      return "HTTP/1.1 200 OK" & CRLF & Closing & Origin_Header &
      "Content-Type: application/cbor" & CRLF & "Content-Length: " &
      Decimal (Decimal'First + 1 .. Decimal'Last) & CRLF & CRLF;
   end Response_Header;
   function Preflight_Response return String is
     ("HTTP/1.1 204 No Content" & CRLF & Closing & Origin_Header &
      "Access-Control-Allow-Methods: POST" & CRLF &
      "Access-Control-Allow-Headers: content-type" & CRLF &
      "Content-Length: 0" & CRLF & CRLF);
   function Error_Response return String is
     ("HTTP/1.1 400 Bad Request" & CRLF & Closing & "Content-Length: 0" & CRLF & CRLF);

   procedure Parse (Data : String; Result : out Request) is
      Header_Last : Natural := 0;
      Cursor : Positive := 1;
      type Header_Name is (Host, Origin, Content_Type, Content_Length,
                           Request_Method, Request_Headers);
      Seen : array (Header_Name) of Boolean := [others => False];
      Length : Natural range 0 .. 1100 := 0;
      First_Line : Boolean := True;
      function Token (C : Character) return Boolean is
        (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' |
          '!' | '#' | '$' | '%' | '&' | ''' | '*' | '+' | '-' | '.' |
          '^' | '_' | '`' | '|' | '~');
   begin
      Result := (others => <>);
      for I in 1 .. Data'Length - 3 loop
         if Data (I .. I + 3) = CRLF & CRLF then Header_Last := I + 3; exit; end if;
      end loop;
      if Header_Last = 0 then
         if Data'Length >= Max_Header then Result.State := Rejected; end if;
         return;
      end if;
      Result.State := Rejected;
      if Header_Last > Max_Header then return; end if;
      while Cursor < Header_Last - 1 loop
         declare
            Last : Natural := 0;
         begin
            for I in Cursor .. Header_Last - 1 loop
               if Data (I .. I + 1) = CRLF then Last := I; exit; end if;
            end loop;
            if Last = 0 then return; end if;
            if First_Line then
               if Data (Cursor .. Last - 1) = "POST /ccl HTTP/1.1" then
                  Result.Verb := Submit;
               elsif Data (Cursor .. Last - 1) = "OPTIONS /ccl HTTP/1.1" then
                  Result.Verb := Preflight;
               else return;
               end if;
               First_Line := False;
            else
               declare
                  Colon : Natural := 0;
               begin
                  for I in Cursor .. Last - 1 loop
                     if Data (I) = ':' then Colon := I; exit; end if;
                     if not Token (Data (I)) then return; end if;
                  end loop;
                  if Colon <= Cursor then return; end if;
                  if (for some C of Data (Colon + 1 .. Last - 1) =>
                    C not in ' ' .. '~' | ASCII.HT) then return; end if;
                  declare
                     Name : constant String := Lower (Data (Cursor .. Colon - 1));
                     -- Deliberately require spaces, not exotic HTTP whitespace.
                     Value : constant String := Trim (Data (Colon + 1 .. Last - 1));
                     Known : Boolean := True;
                     Key : Header_Name := Host;
                  begin
                     if Name = "host" then Key := Host;
                     elsif Name = "origin" then Key := Origin;
                     elsif Name = "content-type" then Key := Content_Type;
                     elsif Name = "content-length" then Key := Content_Length;
                     elsif Name = "access-control-request-method" then Key := Request_Method;
                     elsif Name = "access-control-request-headers" then Key := Request_Headers;
                     elsif Name = "transfer-encoding" or Name = "expect" or
                       Name = "content-encoding" or Name = "upgrade" or Name = "trailer"
                     then return;
                     else Known := False;
                     end if;
                     if Known then
                        if Seen (Key) then return; end if;
                        Seen (Key) := True;
                        case Key is
                           when Host => if Value /= "127.0.0.1:18445" then return; end if;
                           when Origin => if Value /= Development_Origin then return; end if;
                           when Content_Type => if Value /= "application/cbor" then return; end if;
                           when Request_Method => if Value /= "POST" then return; end if;
                           when Request_Headers =>
                              if Lower (Value) /= "content-type" then return; end if;
                           when Content_Length =>
                              if Value'Length not in 1 .. 4 then return; end if;
                              for C of Value loop
                                 if C not in '0' .. '9' then return; end if;
                                 declare
                                    Next : constant Natural := Length * 10 + Character'Pos (C) - Character'Pos ('0');
                                 begin
                                    if Next > 1100 then return; end if;
                                    Length := Next;
                                 end;
                              end loop;
                        end case;
                     end if;
                  end;
               end;
            end if;
            Cursor := Last + 2;
         end;
      end loop;
      if not Seen (Host) or not Seen (Origin) then return; end if;
      case Result.Verb is
         when Submit =>
            if not Seen (Content_Type) or not Seen (Content_Length) or Length = 0 or
              Seen (Request_Method) or Seen (Request_Headers) then return; end if;
         when Preflight =>
            if Length /= 0 or not Seen (Request_Method) or not Seen (Request_Headers) then return; end if;
      end case;
      Result.Body_First := Header_Last + 1;
      Result.Body_Length := Length;
      if Data'Length < Header_Last + Length then Result.State := Incomplete;
      elsif Data'Length = Header_Last + Length then Result.State := Ready;
      end if;
   end Parse;
end Control_HTTP;
