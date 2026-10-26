--  RTW89_RX (RX buffer info, descriptor, PPDU status, C2H, channel
--  index), RTW89_H2C payloads, RTW89_Channel_Values, and the PHY table
--  walk's applied counts for RFE 1 / cut CBV.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Wifi_Scan;
with RTW89_RX; use RTW89_RX;
with RTW89_H2C;
with RTW89_Channel_Values;
with RTW89_PHY_Tables;
with RTW89_Tables_8852B;
with Test_Support; use Test_Support;

procedure RX_Tests is
   package WS renames CuBit.Wifi_Scan;
   package CV renames RTW89_Channel_Values;
   use type RTW89_H2C.Bytes;
   use type RTW89_PHY_Tables.Load_Status;
   use type CV.Gain_Band;

   procedure Put32 (B : in out Buffer; At_Byte : Natural; W : Unsigned_32) is
   begin
      B (At_Byte) := Unsigned_8 (W and 16#FF#);
      B (At_Byte + 1) := Unsigned_8 (Shift_Right (W, 8) and 16#FF#);
      B (At_Byte + 2) := Unsigned_8 (Shift_Right (W, 16) and 16#FF#);
      B (At_Byte + 3) := Unsigned_8 (Shift_Right (W, 24));
   end Put32;

   --  A single-segment buffer: info word, a long or short descriptor.
   function Make (Kind : Natural; Length : Natural; Long : Boolean; Driver_Info : Natural := 0;
                  Write_Size : Natural := 0; MAC_Info : Boolean := False) return Buffer is
      B : Buffer := [others => 0];
      Descriptor : constant Natural := (if Long then Long_Descriptor else Short_Descriptor);
      Size : constant Natural :=
        (if Write_Size /= 0 then Write_Size else Info_Bytes + Descriptor + Driver_Info * 8 + Length);
   begin
      Put32 (B, 0, Unsigned_32 (Size) or First_Segment or Last_Segment);
      Put32 (B, Descriptor_At, Unsigned_32 (Length) or Shift_Left (Unsigned_32 (Kind), Packet_Type_Shift)
             or Shift_Left (Unsigned_32 (Driver_Info), Driver_Info_Shift)
             or (if Long then Long_Bit else 0) or (if MAC_Info then MAC_Info_Valid_Bit else 0));
      Put32 (B, Descriptor_At + 4, Shift_Left (Unsigned_32'(5), PPDU_Count_Shift)
             or Shift_Left (Unsigned_32'(4), Rate_Shift));           --  ppdu 5, OFDM 6
      return B;
   end Make;

   D : Descriptor;
   Size : Payload_Length;
   Result : Status;
   B : Buffer;
begin
   --  A beacon in a long descriptor with 8 bytes of driver info.
   B := Make (Packet_Type'Pos (WIFI), 100, Long => True, Driver_Info => 1);
   Decode (B, D, Size, Result);
   Check (Result = Decoded and then D.Kind = WIFI and then D.Payload_First = 4 + 32 + 8
          and then D.Length = 100 and then D.PPDU_Count = 5 and then D.Rate = 4,
          "rx: long descriptor, driver info, payload place");
   B := Make (Packet_Type'Pos (C2H), 12, Long => False);
   Put32 (B, 4 + 16, 16#0000_0101#);                  --  MAC / class 0 / function 1
   Put32 (B, 4 + 16 + 4, 12);
   Decode (B, D, Size, Result);
   Check (Result = Decoded and then D.Kind = C2H and then D.Payload_First = 20, "rx: short descriptor C2H");
   declare
      H : C2H_Header;
      Valid : Boolean;
   begin
      Decode_C2H (B, D, H, Valid);
      Check (Valid and then H.Category = 1 and then H.Class = 0 and then H.Func = 1 and then H.Length = 12,
             "c2h: header fields");
      Put32 (B, 4 + 16 + 4, 13);
      Decode_C2H (B, D, H, Valid);
      Check (not Valid, "c2h: length past the payload refused");
   end;
   --  Refusals.
   B := Make (Packet_Type'Pos (WIFI), 100, Long => True, Write_Size => 4 + 32 + 99);
   Decode (B, D, Size, Result);
   Check (Result = Payload_Overrun, "rx: payload past WRITE_SIZE refused");
   B := Make (Packet_Type'Pos (WIFI), 0, Long => True, Write_Size => 4 + 16);
   Decode (B, D, Size, Result);
   Check (Result = Size_Too_Small, "rx: long descriptor past WRITE_SIZE refused");
   B := Make (Packet_Type'Pos (WIFI), 10, Long => False);
   Put32 (B, 0, 16#3FFF# or First_Segment or Last_Segment);
   Decode (B, D, Size, Result);
   Check (Result = Size_Too_Large, "rx: WRITE_SIZE past the buffer refused");
   B := Make (Packet_Type'Pos (WIFI), 10, Long => False);
   Put32 (B, 0, 40 or First_Segment);
   Decode (B, D, Size, Result);
   Check (Result = Multi_Buffer, "rx: first segment without last refused");
   Put32 (B, 0, 40 or Last_Segment);
   Decode (B, D, Size, Result);
   Check (Result = Not_First_Segment, "rx: continuation refused");
   B := Make (Packet_Type'Pos (WIFI), 10, Long => False);
   Put32 (B, Descriptor_At, 10 or Shift_Left (Unsigned_32'(1), Shift_Shift));
   Decode (B, D, Size, Result);
   Check (Result = Shift_Nonzero, "rx: SHIFT set refused");
   B := Make (Packet_Type'Pos (WIFI), 4096 - 36, Long => True);
   Decode (B, D, Size, Result);
   Check (Result = Decoded and then D.Payload_First + D.Length = Buffer_Bytes, "rx: payload to the buffer's end");
   B := Make (15, 0, Long => False);
   Decode (B, D, Size, Result);
   Check (Result = Decoded and then D.Kind = Undefined, "rx: type 15 decodes as Undefined");

   --  PPDU status: MAC info (1 user), then an 8-byte PHY header with
   --  IE01 (32 bytes): total 8 + 4 + 4 (pad) + 8 + 32.
   declare
      Payload : constant := 8 + 4 + 4 + 8 + 32;
      First : constant := 4 + 32;
      R : PPDU_Report;
      P : PPDU_Result;
      Frame : Descriptor;
   begin
      B := Make (Packet_Type'Pos (PPDU_Status), Payload, Long => True, MAC_Info => True);
      Put32 (B, 4 + 12, A1_Match_Bit);                         --  dw3 A1 match
      Put32 (B, First, 1);                                      --  usr_num 1
      Put32 (B, First + 4, 0);                                  --  PLCP length 0
      Put32 (B, First + 8, 1);                                  --  user: MAC id valid
      Put32 (B, First + 16, 13 or Valid_Bit or Shift_Left (Unsigned_32'(5), Length_Shift)
                            or Shift_Left (Unsigned_32'(16#A0#), RSSI_Average_Shift));
      Put32 (B, First + 20, 16#90# or Shift_Left (Unsigned_32'(16#A4#), 8));
      Put32 (B, First + 24, 1 or Shift_Left (Unsigned_32'(16#96#), 8) or Shift_Left (Unsigned_32'(16#06#), 16));
      Decode (B, D, Size, Result);
      Decode_PPDU (B, D, R, P);
      Check (Result = Decoded and then P = Taken and then R.Class = 13 and then R.RSSI_A = 16#90#
             and then R.RSSI_B = 16#A4# and then R.IEs_Walked and then R.Channel_Index = 6
             and then R.Has_RPL and then R.RPL_Average = 16#96#, "ppdu: header, IE01 walk");
      Check (Signal_Of (R) = 16#A4# / 2 - 110, "ppdu: signal from the stronger path");
      Frame := D;
      Frame.Kind := WIFI;
      Check (Matches (Frame, D), "ppdu: same PPDU counter, rate, GI and bandwidth match");
      Frame.PPDU_Count := 6;
      Check (not Matches (Frame, D), "ppdu: another PPDU counter does not");
      --  Length field disagrees.
      Put32 (B, First + 16, 13 or Valid_Bit or Shift_Left (Unsigned_32'(4), Length_Shift));
      Decode_PPDU (B, D, R, P);
      Check (P = PHY_Length_Mismatch, "ppdu: LEN * 8 must be the status length");
      Put32 (B, First + 16, 13 or Shift_Left (Unsigned_32'(5), Length_Shift));
      Decode_PPDU (B, D, R, P);
      Check (P = PHY_Not_Valid, "ppdu: VALID clear refused");
      Put32 (B, First, 5);
      Decode_PPDU (B, D, R, P);
      Check (P = Too_Many_Users, "ppdu: more than 4 users refused");
      Put32 (B, First, 1);
      Put32 (B, First + 4, Shift_Left (Unsigned_32'(200), PLCP_Length_Shift));
      Decode_PPDU (B, D, R, P);
      Check (P = MAC_Info_Short, "ppdu: PLCP header past the payload refused");
      --  An IE of length 0 (type 23) ends the walk; the header stands.
      Put32 (B, First + 4, 0);
      Put32 (B, First + 16, 13 or Valid_Bit or Shift_Left (Unsigned_32'(5), Length_Shift));
      Put32 (B, First + 24, 23);
      Decode_PPDU (B, D, R, P);
      Check (P = Taken and then not R.IEs_Walked and then R.RSSI_A = 16#90#, "ppdu: zero-length IE stops the walk");
   end;
   Check (To_dBm (0) = -110 and then To_dBm (255) = 17, "rssi: raw to dBm range");

   --  Channel index decoding.
   Check (Decode_Channel_Index (16#06#) = (Band_2_4, 6), "ch_idx: 2.4 GHz");
   Check (Decode_Channel_Index (16#22#) = (Band_5, 40), "ch_idx: 5 GHz base 36");
   Check (Decode_Channel_Index (16#58#) = (Band_5, 165), "ch_idx: 5 GHz base 149");
   Check (Decode_Channel_Index (16#11#).Band = None and then Decode_Channel_Index (16#71#).Band = None
          and then Decode_Channel_Index (0).Band = None, "ch_idx: invalid bases, 6 GHz, none");

   --  H2C payloads against rtw89's words (seq-rx.md 4.6).
   Check (RTW89_H2C.Role_Maintain_Create = [16#00#, 16#20#, 16#00#, 16#00#], "h2c: role maintain 0x00002000");
   Check (RTW89_H2C.Join_Info_Disconnected = [16#00#, 16#01#, 16#00#, 16#04#], "h2c: join info 0x04000100");
   declare
      CAM : constant RTW89_H2C.Bytes := RTW89_H2C.Address_CAM ([16#94#, 16#97#, 16#4F#, 16#28#, 16#7C#, 16#87#]);
      Hash : constant Unsigned_8 := 16#94# xor 16#97# xor 16#4F# xor 16#28# xor 16#7C# xor 16#87#;
   begin
      Check (CAM'Length = 60 and then CAM (5 .. 8) = [0, 0, 16#40#, 0]
             and then CAM (9 .. 12) = [1, 0, Hash, 0]
             and then CAM (17 .. 24) = [16#94#, 16#97#, 16#4F#, 16#28#, 16#7C#, 16#87#, 0, 0]
             and then CAM (37 .. 40) = [0, 0, 2, 0] and then CAM (49 .. 52) = [0, 0, 8, 0]
             and then CAM (53 .. 56) = [16#FD#, 0, 0, 0], "h2c: address CAM words 1, 2, 4, 5, 9, 12, 13");
   end;
   declare
      T : constant RTW89_H2C.Bytes := RTW89_H2C.Default_CMAC_Table;
   begin
      Check (T'Length = 68 and then T (1 .. 4) = [16#80#, 0, 0, 0] and then T (25 .. 28) = [0, 0, 16#43#, 0]
             and then T (37 .. 40) = [0, 0, 16#20#, 0] and then T (53 .. 56) = [0, 16#0E#, 0, 0]
             and then T (57 .. 60) = [0, 0, 16#FF#, 16#FF#] and then T (61 .. 64) = [0, 0, 16#0C#, 16#0F#],
             "h2c: default CMAC table");
   end;

   --  Channel values (seq-bbrf.md 3.5, 3.6).
   Check (CV.With_20_MHz (CV.With_Channel (16#01001#, WS.Band_2_4_GHz, 1)) = 16#01C01#
          and then CV.With_Channel (16#01001#, WS.Band_2_4_GHz, 1) = 16#01001#,
          "rf18: channel 1 from the table's value");
   Check (CV.With_20_MHz (CV.With_Channel (16#01C01#, WS.Band_5_GHz, 36)) = 16#11D24#
          and then CV.With_20_MHz (CV.With_Channel (16#11D24#, WS.Band_5_GHz, 165)) = 16#11DA5#
          and then CV.With_20_MHz (CV.With_Channel (16#11DA5#, WS.Band_2_4_GHz, 13)) = 16#01C0D#,
          "rf18: 5 GHz and back");
   Check (CV.With_20_MHz (CV.With_Channel (16#C1C01#, WS.Band_2_4_GHz, 6)) = 16#C1C06#, "rf18: bits 19:18 kept");
   Check (CV.Channel_Index (WS.Band_5_GHz, 36) = 16#20# and then CV.Channel_Index (WS.Band_5_GHz, 48) = 16#26#
          and then CV.Channel_Index (WS.Band_5_GHz, 149) = 16#50#
          and then CV.Channel_Index (WS.Band_5_GHz, 165) = 16#58#
          and then CV.Channel_Index (WS.Band_2_4_GHz, 11) = 16#0B#, "chan_idx: encodings");
   Check (CV.SCO (WS.Band_2_4_GHz, 1) = 109 and then CV.SCO (WS.Band_2_4_GHz, 13) = 106
          and then CV.SCO (WS.Band_5_GHz, 36) = 51 and then CV.SCO (WS.Band_5_GHz, 48) = 50
          and then CV.SCO (WS.Band_5_GHz, 149) = 46 and then CV.SCO (WS.Band_5_GHz, 165) = 45, "sco: values");
   Check (CV.Gain_Band_Of (WS.Band_5_GHz, 48) = CV.Band_5G_Low
          and then CV.Gain_Band_Of (WS.Band_5_GHz, 149) = CV.Band_5G_High, "gain band");

   --  The table walk for this board (RFE 1, cut CBV): seq-bbrf.md 2.1.
   declare
      Applied : Natural;
      Status : RTW89_PHY_Tables.Load_Status;
      procedure Ignore (E : RTW89_PHY_Tables.Table_Entry) is null;
      procedure Count is new RTW89_PHY_Tables.Load (Ignore);
      Sum : Unsigned_32 := 0;
      procedure Add (E : RTW89_PHY_Tables.Table_Entry) is
      begin
         Sum := Sum * 31 + (E.Address xor E.Data);
      end Add;
      procedure Digest is new RTW89_PHY_Tables.Load (Add);
   begin
      Count (RTW89_Tables_8852B.BB_Registers, 1, 1, Applied, Status);
      Check (Status = RTW89_PHY_Tables.Loaded and then Applied = 1013, "tables: BB 1013 entries");
      Count (RTW89_Tables_8852B.Radio_A, 1, 1, Applied, Status);
      Check (Status = RTW89_PHY_Tables.Loaded and then Applied = 947, "tables: radio A 947 of 8295");
      Count (RTW89_Tables_8852B.Radio_B, 1, 1, Applied, Status);
      Check (Status = RTW89_PHY_Tables.Loaded and then Applied = 932, "tables: radio B 932 of 8392");
      Count (RTW89_Tables_8852B.NCTL, 1, 1, Applied, Status);
      Check (Status = RTW89_PHY_Tables.Loaded and then Applied = 1320, "tables: NCTL 1320 entries");
      Sum := 0;
      Digest (RTW89_Tables_8852B.Radio_A, 1, 1, Applied, Status);
      declare
         CBV_Sum : constant Unsigned_32 := Sum;
      begin
         Sum := 0;
         Digest (RTW89_Tables_8852B.Radio_A, 1, 0, Applied, Status);
         Check (Status = RTW89_PHY_Tables.Loaded and then Sum /= CBV_Sum,
                "tables: cut CAV selects another variant");
      end;
      Count (RTW89_Tables_8852B.Radio_A, 16#77#, 1, Applied, Status);
      Check (Status = RTW89_PHY_Tables.No_Configuration, "tables: unknown RFE has no configuration");
   end;
   --  A synthetic table: ELSE with no matched branch stops the load.
   declare
      Applied : Natural;
      Status : RTW89_PHY_Tables.Load_Status;
      procedure Ignore (E : RTW89_PHY_Tables.Table_Entry) is null;
      procedure Count is new RTW89_PHY_Tables.Load (Ignore);
      T : constant RTW89_PHY_Tables.Table :=
        [(16#F001_0001#, 0), (16#8002_0001#, 0), (16#4000_0000#, 0), (16#0000_0010#, 1),
         (16#A000_0000#, 0), (16#0000_0011#, 2), (16#B000_0000#, 0), (16#0000_0012#, 3)];
   begin
      Count (T, 1, 1, Applied, Status);
      Check (Status = RTW89_PHY_Tables.Aborted and then Applied = 0, "tables: ELSE with nothing matched aborts");
   end;
end RX_Tests;
