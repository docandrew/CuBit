with System;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Net_Link;
with CuBit.Net_Management;
with CuBit.Wifi_Scan;
with CCL_Native_Bindings;

--  wifi.scan on CuBit: for each Wireless interface netstack reports
--  (Interface_Next / Interface_Detail), lend netstack one page and ask
--  for the interface's scan block (Scan_Results); decode it with the
--  proved CuBit.Wifi_Scan.Decode, refusing what does not decode. netstack
--  only copies what the driver published; nothing here starts a scan.
package body CCL_Wifi is
   use Interfaces;
   use type CuBit.Messages.MessageTag;
   use type CuBit.Net_Link.Link_Kind;
   package NL renames CuBit.Net_Link;
   package NM renames CuBit.Net_Management;
   package WS renames CuBit.Wifi_Scan;

   --  netstack's answer labels (as every client spells them).
   REPLY_OK  : constant Unsigned_32 := 16#F000#;
   REPLY_ERR : constant Unsigned_32 := 16#F001#;

   Page : array (0 .. NM.Scan_Page_Bytes / WS.Word_Bytes - 1) of Unsigned_32 := [others => 0]
     with Alignment => NM.Scan_Page_Bytes, Volatile;

   Hex_Digits : constant String (1 .. 16) := "0123456789abcdef";
   Nibble : constant := 16;
   First_Printable : constant := 16#20#;
   Last_Printable  : constant := 16#7E#;
   Backslash       : constant := 16#5C#;

   function Hex_Pair (B : Unsigned_8) return String is
     [Hex_Digits (Natural (B / Nibble) + 1), Hex_Digits (Natural (B mod Nibble) + 1)];

   procedure Fill_BSSID (Item : in out Listed; A : NL.Hardware_Address) is
   begin
      for I in NL.Octet_Index loop
         Item.BSSID (I * 3 + 1 .. I * 3 + 2) := Hex_Pair (A (I));
         if I < NL.Octet_Index'Last then
            Item.BSSID (I * 3 + 3) := ':';
         end if;
      end loop;
   end Fill_BSSID;

   --  Printable ASCII as itself (a backslash doubled), everything else \xNN.
   procedure Fill_SSID (Item : in out Listed; E : WS.BSS_Entry) is
      procedure Put (C : Character) is
      begin
         if Item.SSID_Length < MAXIMUM_SSID_TEXT then
            Item.SSID_Length := Item.SSID_Length + 1;
            Item.SSID (Item.SSID_Length) := C;
         end if;
      end Put;
   begin
      Item.SSID_Length := 0;
      for I in 1 .. E.Length loop
         declare
            B : constant Unsigned_8 := E.SSID (I);
         begin
            if B = Backslash then
               Put ('\'); Put ('\');
            elsif B in First_Printable .. Last_Printable then
               Put (Character'Val (B));
            else
               declare
                  H : constant String := Hex_Pair (B);
               begin
                  Put ('\'); Put ('x'); Put (H (1)); Put (H (2));
               end;
            end if;
         end;
      end loop;
   end Fill_SSID;

   function Request (Op : NM.Operation; W0, W1 : Unsigned_64) return CuBit.Messages.Message is
      M : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      M.tag := (label => NM.Label (Op), length => Unsigned_8 (NM.Request_Words (Op)),
                protocol => 0, ipc => <>, reserved => 0);
      M.words (0) := W0;
      M.words (1) := W1;
      return M;
   end Request;

   procedure Scan
     (Entries : out Listing; Count : out Listed_Count; Total : out Natural; Result : out Result_Kind)
   is
      Netstack : constant CuBit.Handles.Endpoint := CCL_Native_Bindings.Network_Stack;
      Previous : Unsigned_64 := 0;
      M : CuBit.Messages.Message;
      Tag : CuBit.Messages.MessageTag;
   begin
      Entries := [others => (others => <>)];
      Count := 0;
      Total := 0;
      if not CuBit.Handles.Valid (Netstack) then
         Result := Not_Granted;
         return;
      end if;
      Result := Unavailable;
      loop
         M := Request (NM.Interface_Next, Previous, 0);
         Tag := CuBit.Messages.capCall (Netstack, M, CuBit.Messages.Wait_Forever);
         if Tag.label = REPLY_ERR then
            Result := Listed_All;   --  no more interfaces
            return;
         elsif Tag.label /= REPLY_OK then
            return;                 --  netstack did not answer
         end if;
         Result := Listed_All;
         exit when not NL.Valid_Handle_Word (M.words (0)) or else M.words (0) = Previous;
         Previous := M.words (0);
         M := Request (NM.Interface_Detail, Previous, 0);
         Tag := CuBit.Messages.capCall (Netstack, M, CuBit.Messages.Wait_Forever);
         if Tag.label = REPLY_OK then
            declare
               S : constant NM.Decoded_Summary :=
                 NM.Decode_Summary ((M.words (0), M.words (1), M.words (2), M.words (3)));
            begin
               if S.Valid and then S.Item.Kind = NL.Wireless then
                  declare
                     Name : constant NL.Interface_Name := NL.Name_Of (S.Item.Identity, S.Item.Address);
                     Loan : CuBit.Memory_Grants.Grant_Reference;
                     Lent, Revoked : Boolean;
                     Block : WS.Block;
                  begin
                     CuBit.Memory_Grants.Create_Via_Capability
                       (Netstack, Page'Address, 1, True, Loan, Lent);
                     if Lent then
                        M := Request (NM.Scan_Results, Previous, CuBit.Grant_References.Encode (Loan));
                        Tag := CuBit.Messages.capCall (Netstack, M, CuBit.Messages.Wait_Forever);
                        CuBit.Memory_Grants.Revoke (Loan, Revoked);
                        if Tag.label = REPLY_OK then
                           for W in WS.Block_Word_Index loop
                              Block (W) := Page (W);
                           end loop;
                           declare
                              D : constant WS.Decoded_Result := WS.Decode (Block);
                           begin
                              if D.Valid then
                                 Total := Total + D.Item.Summary.Total;
                                 for I in 1 .. D.Item.Summary.Count loop
                                    exit when Count = Wifi.MAXIMUM_LISTED;
                                    declare
                                       E : constant WS.BSS_Entry := D.Item.Entries (I);
                                       Item : Listed;
                                    begin
                                       Item.Interface_Length := Name.Length;
                                       Item.Interface_Name (1 .. Name.Length) := Name.Text (1 .. Name.Length);
                                       Fill_BSSID (Item, E.BSSID);
                                       Fill_SSID (Item, E);
                                       Item.On_Band :=
                                         (case E.On_Band is
                                             when WS.Band_2_4_GHz => Wifi.Band_2_4_GHz,
                                             when WS.Band_5_GHz   => Wifi.Band_5_GHz);
                                       Item.Channel := Natural (E.Channel);
                                       Item.Signal := Integer (E.Signal);
                                       Item.Protection :=
                                         (if E.RSN then Wifi.RSN
                                          elsif WS.Privacy (E) then Wifi.Legacy_Protected
                                          else Wifi.Open);
                                       Item.Capability := E.Capability;
                                       Item.Heard := Natural (E.Heard);
                                       Count := Count + 1;
                                       Entries (Count) := Item;
                                    end;
                                 end loop;
                              end if;
                           end;
                        end if;
                     end if;
                  end;
               end if;
            end;
         end if;
      end loop;
   end Scan;
end CCL_Wifi;
