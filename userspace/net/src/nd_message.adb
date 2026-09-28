------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body ND_Message with SPARK_Mode is

   procedure Parse
     (B : Bytes; Hop_Limit : Unsigned_8; M : out Message; OK : out Boolean)
   is
      P : Natural := Fixed_Size;
      Option_Bytes : Natural;
      Wanted : Unsigned_8;
   begin
      M := (others => <>);
      OK := False;
      if Hop_Limit /= Required_Hop_Limit or else B'Length < Fixed_Size or else
        B (0) not in Solicitation_Type | Advertisement_Type or else B (1) /= 0
      then
         return;
      end if;
      M.Target := IPv6_Header.Address_At (B, 8);
      if IPv6_Header.Is_Multicast (M.Target) then
         return;
      end if;
      if B (0) = Solicitation_Type then
         M.Of_Kind := Solicitation;
         Wanted := Source_Link_Option;
      else
         M.Of_Kind := Advertisement;
         M.Router := (B (4) and 16#80#) /= 0;
         M.Solicited := (B (4) and 16#40#) /= 0;
         M.Override := (B (4) and 16#20#) /= 0;
         Wanted := Target_Link_Option;
      end if;
      --  Options: type, length in units of eight bytes (never zero).
      while P < B'Length loop
         pragma Loop_Invariant (P >= Fixed_Size and then P < B'Length);
         pragma Loop_Variant (Increases => P);
         if B'Length - P < 2 or else B (P + 1) = 0 then
            return;   --  a truncated option, or one of length zero
         end if;
         Option_Bytes := Natural (B (P + 1)) * 8;
         if Option_Bytes > B'Length - P then
            return;   --  an option past the end of the message
         end if;
         if B (P) = Wanted and then Option_Bytes = 8 and then not M.Has_Link then
            M.Has_Link := True;
            M.Link := [B (P + 2), B (P + 3), B (P + 4),
                       B (P + 5), B (P + 6), B (P + 7)];
         end if;
         P := P + Option_Bytes;
      end loop;
      OK := True;
   end Parse;

end ND_Message;
