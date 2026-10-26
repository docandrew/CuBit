--  The Ada side of the differential tests (reference.py has the other):
--    frames <in> <out>: decode each hex line of <in> (an MPDU without FCS)
--      with IEEE80211_Beacons and write one result line per frame;
--    rx <in> <out>: decode each hex line (an RX buffer) with RTW89_RX
--      (descriptor, and PPDU status or C2H header) and write one line;
--    tables <out>: the BB/RF/NCTL entries RTW89_PHY_Tables applies for
--      RFE 1 / cut 1, one "table address data" line each.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with IEEE80211_Beacons;
with RTW89_RX;
with RTW89_PHY_Tables;
with RTW89_Tables_8852B;

package body Corpus is
   use type IEEE80211_Beacons.Status;
   use type RTW89_RX.PPDU_Result;

   Hex : constant String := "0123456789abcdef";

   function Hex2 (B : Unsigned_8) return String is
     [Hex (Natural (B / 16) + 1), Hex (Natural (B mod 16) + 1)];

   function Hex8 (W : Unsigned_32) return String is
     (Hex2 (Unsigned_8 (Shift_Right (W, 24))) & Hex2 (Unsigned_8 (Shift_Right (W, 16) and 16#FF#))
      & Hex2 (Unsigned_8 (Shift_Right (W, 8) and 16#FF#)) & Hex2 (Unsigned_8 (W and 16#FF#)));

   function Image (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   function Nibble (C : Character) return Unsigned_8 is
     (case C is
        when '0' .. '9' => Character'Pos (C) - Character'Pos ('0'),
        when 'a' .. 'f' => Character'Pos (C) - Character'Pos ('a') + 10,
        when 'A' .. 'F' => Character'Pos (C) - Character'Pos ('A') + 10,
        when others => 0);

   procedure Frames (Input, Output : String) is
      Source, Target : File_Type;
   begin
      Open (Source, In_File, Input);
      Create (Target, Out_File, Output);
      while not End_Of_File (Source) loop
         declare
            Text : constant String := Get_Line (Source);
            Length : constant Natural := Natural'Min (Text'Length / 2, IEEE80211_Beacons.Maximum_Frame);
            F : IEEE80211_Beacons.Frame_Bytes (0 .. Length - 1);
            Info : IEEE80211_Beacons.Decoded;
            Result : IEEE80211_Beacons.Status;
            SSID : String (1 .. 2 * IEEE80211_Beacons.SSID_Octets);
         begin
            for I in F'Range loop
               F (I) := Nibble (Text (Text'First + 2 * I)) * 16 + Nibble (Text (Text'First + 2 * I + 1));
            end loop;
            IEEE80211_Beacons.Decode (F, Info, Result);
            Put (Target, IEEE80211_Beacons.Name (Result));
            if Result = IEEE80211_Beacons.Accepted then
               for I in 1 .. Info.Length loop
                  SSID (2 * I - 1 .. 2 * I) := Hex2 (Info.SSID (I));
               end loop;
               Put (Target, " kind=" & (case Info.Kind is
                                             when IEEE80211_Beacons.Beacon => "beacon",
                                             when IEEE80211_Beacons.Probe_Response => "probe")
                    & " bssid=");
               for B of Info.BSSID loop
                  Put (Target, Hex2 (B));
               end loop;
               Put (Target, " ssid=" & (if Info.Has_SSID then "[" & SSID (1 .. 2 * Info.Length) & "]" else "none")
                    & " ds=" & Image (Natural (Info.DS_Channel))
                    & " ht=" & Image (Natural (Info.HT_Primary))
                    & " cap=" & Image (Natural (Info.Capability))
                    & " int=" & Image (Natural (Info.Interval))
                    & " rsn=" & (if Info.RSN then "1" else "0")
                    & " elements=" & Image (Info.Elements)
                    & " skipped=" & Image (Info.Skipped));
            end if;
            New_Line (Target);
         end;
      end loop;
      Close (Source);
      Close (Target);
   end Frames;

   procedure RX (Input, Output : String) is
      Source, Target : File_Type;
      use type RTW89_RX.Status;
      use type RTW89_RX.Packet_Type;
   begin
      Open (Source, In_File, Input);
      Create (Target, Out_File, Output);
      while not End_Of_File (Source) loop
         declare
            Text : constant String := Get_Line (Source);
            B : RTW89_RX.Buffer := [others => 0];
            D : RTW89_RX.Descriptor;
            Size : RTW89_RX.Payload_Length;
            Result : RTW89_RX.Status;
         begin
            for I in 0 .. Natural'Min (Text'Length / 2, RTW89_RX.Buffer_Bytes) - 1 loop
               B (I) := Nibble (Text (Text'First + 2 * I)) * 16 + Nibble (Text (Text'First + 2 * I + 1));
            end loop;
            RTW89_RX.Decode (B, D, Size, Result);
            Put (Target, RTW89_RX.Name (Result));
            if Result = RTW89_RX.Decoded then
               Put (Target, " type=" & Image (RTW89_RX.Packet_Type'Pos (D.Kind))
                    & " first=" & Image (D.Payload_First) & " length=" & Image (D.Length)
                    & " ppdu=" & Image (Natural (D.PPDU_Count)) & " rate=" & Image (Natural (D.Rate))
                    & " crc=" & (if D.CRC_Error then "1" else "0")
                    & " a1=" & (if D.A1_Match then "1" else "0"));
               if D.Kind = RTW89_RX.PPDU_Status then
                  declare
                     R : RTW89_RX.PPDU_Report;
                     P : RTW89_RX.PPDU_Result;
                  begin
                     RTW89_RX.Decode_PPDU (B, D, R, P);
                     Put (Target, " ppdu_status=" & RTW89_RX.Name (P));
                     if P = RTW89_RX.Taken then
                        Put (Target, " class=" & Image (Natural (R.Class))
                             & " rssi_a=" & Image (Natural (R.RSSI_A)) & " rssi_b=" & Image (Natural (R.RSSI_B))
                             & " walked=" & (if R.IEs_Walked then "1" else "0")
                             & " ch_idx=" & Image (Natural (R.Channel_Index)));
                     end if;
                  end;
               elsif D.Kind = RTW89_RX.C2H then
                  declare
                     H : RTW89_RX.C2H_Header;
                     Valid : Boolean;
                  begin
                     RTW89_RX.Decode_C2H (B, D, H, Valid);
                     Put (Target, " c2h=" & (if Valid then Image (Natural (H.Class)) & "/"
                                                 & Image (Natural (H.Func)) else "invalid"));
                  end;
               end if;
            end if;
            New_Line (Target);
         end;
      end loop;
      Close (Source);
      Close (Target);
   end RX;

   Out_Tables : File_Type;
   Current : String (1 .. 7) := "bb     ";

   procedure Record_Entry (E : RTW89_PHY_Tables.Table_Entry) is
   begin
      Put_Line (Out_Tables, Current & " " & Hex8 (E.Address) & " " & Hex8 (E.Data));
   end Record_Entry;

   procedure Load is new RTW89_PHY_Tables.Load (Record_Entry);

   procedure Tables (Output : String) is
      Applied : Natural;
      Status : RTW89_PHY_Tables.Load_Status;
      RFE : constant Unsigned_8 := 1;
      Cut : constant Unsigned_8 := 1;
   begin
      Create (Out_Tables, Out_File, Output);
      Current := "bb     ";
      Load (RTW89_Tables_8852B.BB_Registers, RFE, Cut, Applied, Status);
      Current := "gain   ";
      Load (RTW89_Tables_8852B.BB_Gain, RFE, Cut, Applied, Status);
      Current := "radio_a";
      Load (RTW89_Tables_8852B.Radio_A, RFE, Cut, Applied, Status);
      Current := "radio_b";
      Load (RTW89_Tables_8852B.Radio_B, RFE, Cut, Applied, Status);
      Current := "nctl   ";
      Load (RTW89_Tables_8852B.NCTL, RFE, Cut, Applied, Status);
      Close (Out_Tables);
   end Tables;

end Corpus;
