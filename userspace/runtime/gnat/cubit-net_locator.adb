------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
package body CuBit.Net_Locator with SPARK_Mode is

   use CuBit.Locators;

   procedure Parse (Text : String; Result : out Target) is
      Authority_Last : Natural;
      P, First, Next : Positive;
      Last : Natural;
      Bracketed, At_End, OK : Boolean;
      Port : Port_Number;
   begin
      Result := (others => <>);
      Split (Text, Authority_Last, P, OK);
      if not OK or else Text (2 .. Authority_Last) /= "net" then
         return;
      end if;

      --  Protocol
      Next_Field (Text, P, First, Last, Next, Bracketed, At_End, OK);
      if not OK or else Bracketed or else At_End then
         return;
      end if;
      if Text (First .. Last) = "tcp" then
         Result.Proto := TCP;
      elsif Text (First .. Last) = "udp" then
         Result.Proto := UDP;
      elsif Text (First .. Last) = "tcp-listen" then
         Result.Proto := TCP_Listen;
      else
         return;
      end if;

      --  Host
      P := Next;
      Next_Field (Text, P, First, Last, Next, Bracketed, At_End, OK);
      if not OK or else At_End or else Last < First then
         return;
      end if;
      if Bracketed then
         --  An IPv6 literal; IPv4 is written dotted, not bracketed.
         if not (for some I in First .. Last => Text (I) = ':') then
            return;
         end if;
         Parse_Address (Text (First .. Last), Result.Address, OK);
         if not OK or else CuBit.Net_Address.Is_Mapped (Result.Address) then
            return;
         end if;
         Result.Is_Address := True;
      elsif (for all I in First .. Last =>
               Is_Digit (Text (I)) or else Text (I) = '.')
      then
         Parse_IPv4 (Text (First .. Last), Result.Address, OK);
         if not OK then
            return;
         end if;
         Result.Is_Address := True;
      elsif Valid_Name (Text (First .. Last)) then
         Result.Name_Len := Last - First + 1;
         Result.Name (1 .. Result.Name_Len) := Text (First .. Last);
      else
         return;
      end if;

      --  Port: the last field.
      P := Next;
      Next_Field (Text, P, First, Last, Next, Bracketed, At_End, OK);
      if not OK or else Bracketed or else not At_End then
         return;
      end if;
      Parse_Port (Text (First .. Last), Port, OK);
      if not OK then
         return;
      end if;
      Result.Port := Port;
      Result.Valid := True;
   end Parse;

end CuBit.Net_Locator;
