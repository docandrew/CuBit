with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol; use CuBit.Desktop_Protocol;
with CuBit.Desktop_Protocol.Publication; use CuBit.Desktop_Protocol.Publication;
procedure Publication_Tests is
   W : Wire_Message;
   Count : Natural := 0;
   Extents : constant array (Positive range <>) of Positive_Extent :=
     [1, 2, 127, 256, 1023, 4096, 65_535];
   IDs : constant array (Positive range <>) of Identity := [1, 2, Identity'Last];
   function Accepted (Wire : Wire_Message; Kind : Positive) return Boolean is
     (case Kind is
        when 1 => Decode_Configuration (Wire).Status = Success,
        when 2 => Decode_Stage (Wire).Valid,
        when 3 => Decode_Publish (Wire).Valid,
        when 4 => Decode_Query (Wire, False).Valid,
        when 5 => Decode_Receipt (Wire, Retirement_Label).Status = Success,
        when others => Decode_Query (Wire, True).Valid);
   Packets : constant array (Positive range 1 .. 6) of Wire_Message :=
     [Encode_Configuration ((Success, (1, 1, 1, 1, 1, (1, 1, 4)))),
      Encode_Stage ((1, 1, (0, 1))),
      Encode_Publish ((1, 1, 1, (0, 0, 0, 0), 0)),
      Encode_Query ((1, 0), False),
      Encode_Receipt ((Success, 1, 1), Retirement_Label),
      Encode_Query ((1, 1), True)];
begin
   for Kind in Packets'Range loop
      for Length in Unsigned_8 loop
         W := Packets (Kind); W.Length := Length;
         pragma Assert (Accepted (W, Kind) = (Length = 4));
         W := Packets (Kind); W.Flags := Length;
         pragma Assert (Accepted (W, Kind) = (Length = 0));
      end loop;
      for Reserved in Unsigned_16 loop
         W := Packets (Kind); W.Reserved := Reserved;
         pragma Assert (Accepted (W, Kind) = (Reserved = 0));
      end loop;
      W := Packets (Kind); W.Label := W.Label xor 16#1000#;
      pragma Assert (not Accepted (W, Kind));
   end loop;
   -- Independent wire examples, not just inverse-function agreement.
   W := (Configuration_Label, 4, 0, 0,
     [0, 7, 3 + 5 * 2 ** 16 + 5 * 2 ** 32 + 4 * 2 ** 40,
      4 + 7 * 2 ** 16 + 16 * 2 ** 32]);
   pragma Assert (Decode_Configuration (W) =
     (Success, (7, 3, 5, 5, 4, (4, 7, 16))));
   pragma Assert (Encode_Configuration
     ((Success, (7, 3, 5, 5, 4, (4, 7, 16)))) = W);
   W := (Publish_Label, 4, 0, 0,
     [9, 7 + 11 * 2 ** 32, 123, 1 + 2 * 2 ** 16 + 3 * 2 ** 32 + 4 * 2 ** 48]);
   pragma Assert (Decode_Publish (W) = (True, (9, 7, 11, (1, 2, 3, 4), 123)));
   W := (Stage_Label, 4, 0, 0, [9, 7, 13 * 2 ** 32 + 5, 0]);
   pragma Assert (Decode_Stage (W) = (True, (9, 7, (5, 13))));
   for Label in Receipt_Label loop
      W := (Label, 4, 0, 0, [0, 7, 11, 0]);
      pragma Assert (Decode_Receipt (W, Label) = (Success, 7, 11));
      W.Label := Configuration_Label;
      pragma Assert (Decode_Receipt (W, Label).Status = Invalid_Request);
      W.Label := Label; W.Words (0) := 7;
      pragma Assert (Decode_Receipt (W, Label).Status = Invalid_Request);
      W.Words := [0, 7, 0, 0];
      pragma Assert (Decode_Receipt (W, Label).Status = Invalid_Request);
   end loop;
   for N in Scale_Component loop
      for D in Scale_Component loop
         for X of Extents loop
            for Y of Extents loop
               declare
                  PW : constant Natural := (Natural (X) * N + D - 1) / D;
                  PH : constant Natural := (Natural (Y) * N + D - 1) / D;
               begin
                  if PW <= 65_535 and PH <= 65_535 and
                    Long_Long_Integer (PW) * Long_Long_Integer (PH) * 4 <= Maximum_Buffer_Bytes
                  then
                     for Epoch of IDs loop
                        declare
                           C : constant Configuration :=
                             (Epoch, X, Y, N, D,
                              (Positive_Extent (PW), Positive_Extent (PH), PW * 4));
                        begin
                           pragma Assert (Valid (C));
                           W := Encode_Configuration ((Success, C));
                           pragma Assert (Decode_Configuration (W) = (Success, C));
                           -- The reply must describe the derived density grid.
                           W.Words (3) := W.Words (3) xor 1;
                           pragma Assert (Decode_Configuration (W).Status /= Success);
                           Count := Count + 1;
                        end;
                     end loop;
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   for Epoch of IDs loop
      for Ticket of IDs loop
         declare
            S : constant Stage_Request := (Live_Surface_Name'Last, Epoch, (4095, 2 ** 32 - 1));
            P : constant Publish_Request :=
              (Live_Surface_Name'Last, Epoch, Ticket, (32_767, 32_767, 32_767, 32_767), 0);
            Q : constant Query := (Live_Surface_Name'Last, Ticket);
         begin
            pragma Assert (Decode_Stage (Encode_Stage (S)) = (True, S));
            pragma Assert (Decode_Publish (Encode_Publish (P)) = (True, P));
            pragma Assert (Decode_Query (Encode_Query (Q, True), True) = (True, Q));
            for Label in Receipt_Label loop
               pragma Assert
                 (Decode_Receipt (Encode_Receipt ((Success, Epoch, Ticket), Label), Label) =
                  (Success, Epoch, Ticket));
            end loop;
         end;
      end loop;
   end loop;
   declare
      Q : constant Query := (1, 0);
   begin
      pragma Assert (Decode_Query (Encode_Query (Q, False), False) = (True, Q));
      pragma Assert (not Decode_Query (Encode_Query (Q, False), True).Valid);
   end;
   for Status in Status_Code loop
      if Status /= Success then
         W := Encode_Configuration ((Status => Failure_Status (Status)));
         pragma Assert (Decode_Configuration (W).Status = Status);
         W.Words (1) := 1;
         pragma Assert (Decode_Configuration (W).Status = Invalid_Request);
         for Label in Receipt_Label loop
            W := Encode_Receipt ((Status => Failure_Status (Status)), Label);
            pragma Assert (Decode_Receipt (W, Label).Status = Status);
            W.Words (2) := 1;
            pragma Assert (Decode_Receipt (W, Label).Status = Invalid_Request);
         end loop;
      end if;
   end loop;
   -- Every header flag/length value must be checked before parsing succeeds.
   for B in Unsigned_8 loop
      W := Encode_Stage ((1, 1, (0, 1))); W.Length := B;
      pragma Assert (Decode_Stage (W).Valid = (B = 4));
      W.Length := 4; W.Flags := B;
      pragma Assert (Decode_Stage (W).Valid = (B = 0));
      W := Encode_Publish ((1, 1, 1, (0, 0, 0, 0), 0)); W.Length := B;
      pragma Assert (Decode_Publish (W).Valid = (B = 4));
      W.Length := 4; W.Flags := B;
      pragma Assert (Decode_Publish (W).Valid = (B = 0));
      W := Encode_Configuration ((Success, (1, 1, 1, 1, 1, (1, 1, 4))));
      W.Length := B;
      pragma Assert ((Decode_Configuration (W).Status = Success) = (B = 4));
      W.Length := 4; W.Flags := B;
      pragma Assert ((Decode_Configuration (W).Status = Success) = (B = 0));
   end loop;
   for R in Unsigned_16 loop
      W := Encode_Stage ((1, 1, (0, 1))); W.Reserved := R;
      pragma Assert (Decode_Stage (W).Valid = (R = 0));
   end loop;
   -- Reject out-of-range identities and grant aliases without truncation.
   for I in 0 .. 3 loop
      W := Encode_Stage ((1, 1, (0, 1))); W.Words (I) := Unsigned_64'Last;
      pragma Assert (Decode_Stage (W).Valid = (I = 0));
   end loop;
   W := Encode_Stage ((1, 1, (0, 1))); W.Words (2) := 2 ** 32 + 4096;
   pragma Assert (not Decode_Stage (W).Valid);
   W.Words (2) := 0; pragma Assert (not Decode_Stage (W).Valid);
   W := Encode_Publish ((1, 1, 1, (0, 0, 0, 0), 0)); W.Words (1) := 0;
   pragma Assert (not Decode_Publish (W).Valid);
   W.Words (1) := 1 + Shift_Left (Unsigned_64'(2 ** 31), 32);
   pragma Assert (not Decode_Publish (W).Valid);
   W := Encode_Configuration ((Success, (1, 1, 1, 1, 1, (1, 1, 4))));
   W.Words (2) := W.Words (2) + 2 ** 48;
   pragma Assert (Decode_Configuration (W).Status = Invalid_Request);
   -- Every watermark bit survives; it cannot alter either bounded identity.
   for Epoch of IDs loop
      for Ticket of IDs loop
         for Bit in 0 .. 63 loop
            declare
               P : constant Publish_Request :=
                 (Live_Surface_Name'Last, Epoch, Ticket, (1, 2, 3, 4),
                  Shift_Left (Unsigned_64'(1), Bit));
               Encoded : constant Wire_Message := Encode_Publish (P);
            begin
               pragma Assert (Decode_Publish (Encoded) = (True, P));
               pragma Assert (Encoded.Words (1) = Epoch + Shift_Left (Ticket, 32));
               pragma Assert (Encoded.Words (2) = P.Input_After);
               pragma Assert (Encoded.Words (3) = 1 + 2*2**16 + 3*2**32 + 4*2**48);
            end;
         end loop;
      end loop;
   end loop;
   W := Encode_Publish ((1, 1, 1, (0, 0, 0, 0), Unsigned_64'Last));
   pragma Assert (Decode_Publish (W).Value.Input_After = Unsigned_64'Last);
   -- Old wire format has a zero high-half ticket and is rejected, not guessed.
   W.Words := [1, 1, 1, 0]; pragma Assert (not Decode_Publish (W).Valid);
   W.Words := [1, 2 ** 31 + 2 ** 32, 0, 0];
   pragma Assert (not Decode_Publish (W).Valid);
   W.Words := [1, 1 + 2 ** 63, 0, 0];
   pragma Assert (not Decode_Publish (W).Valid);
   W.Words := [1, 2 ** 32, 0, 0];
   pragma Assert (not Decode_Publish (W).Valid);
   Ada.Text_IO.Put_Line ("PUBLICATION: PASS" & Count'Image &
     " density layouts, identity boundaries, receipts and malformed messages");
end Publication_Tests;
