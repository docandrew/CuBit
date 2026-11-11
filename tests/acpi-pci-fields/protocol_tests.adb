pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_PCI_Protocol;
procedure Protocol_Tests is
   package Q renames ACPI_PCI_Protocol;
   package P renames Q.P;
   use type Q.Encoding_Outcome, Q.Outcome, Q.Operation;
   use type Q.Microsecond_Deadline;
   use type Q.Wire.Time_Instant;
   use type P.Outcome, P.Region, P.T.Access_Width, P.Segment_Number, P.Bus_Number;
   Checks : Natural := 0;
   Epoch : constant Unsigned_64 := Unsigned_64'Last;
   ID : constant Unsigned_64 := 16#9000_0000_0000_002A#;
   Deadline : constant Q.Microsecond_Deadline := Q.No_Finite_Deadline - 1;
   Golden_Region : constant P.Region := ((0,0,2,3),16#20#,16#80#);
   Base : Q.Packet;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
   procedure Rejected (Region : P.Region; Offset : Unsigned_64; Width : P.T.Access_Width;
      Reason : P.Outcome; Write : Boolean := False) is
      Encoded : constant Q.Encoded_Request := Q.Read_Request (Epoch, ID, Region, Offset, Width, Deadline, Write);
   begin Check (Encoded.Status = Q.Rejected_Geometry and then Encoded.Reason = Reason); end Rejected;
begin
   Check (Q.Decode_Request (Q.Header_Request).Valid);
   for Case_Number in 1 .. 4 loop
      declare
         Due : constant Q.Wire.Time_Instant :=
           (case Case_Number is when 1 => 0, when 2 => 25,
            when 3 => Q.Wire.No_Finite_Deadline - 1, when others => Q.Wire.No_Finite_Deadline);
         Parsed : constant Q.Request := Q.Decode_Request (Q.Header_Request ((7, 1), Due));
      begin
         Check (Parsed.Valid and then Parsed.Setup_Deadline = Due);
      end;
   end loop;
   declare Encoded : constant Q.Encoded_Request := Q.Read_Request
     (Epoch, ID, Golden_Region, 16#60#, P.T.Dword_Access, Deadline); begin
      Check (Encoded.Status = Q.Encoded); Base := Encoded.Item;
      -- Golden version-two word, independent of the round-trip assertions.
      Check (Base.Data (2) = 16#0200_40C0_0800_2013#);
   end;
   for Segment_Index in 0 .. 2 loop
      for Bus in P.Bus_Number loop
         declare
            Key : constant P.Bus_Key := ((case Segment_Index is when 0 => 0, when 1 => 7, when others => P.Segment_Number'Last), Bus);
            Header : constant Q.Request := Q.Decode_Request (Q.Header_Request (Key));
            Region : constant P.Region := ((Key.Segment, Key.Bus, 31, 7), 0, P.Function_Bytes);
            Encoded : constant Q.Encoded_Request := Q.Read_Request
              (Epoch, ID, Region, P.Function_Bytes - 4, P.T.Dword_Access, Deadline, Key => Key);
         begin
            Check (Header.Valid and then Header.Key.Segment = Key.Segment and then Header.Key.Bus = Key.Bus);
            Check (Encoded.Status = Q.Encoded);
         end;
      end loop;
   end loop;
   for Bad_Field in 1 .. 2 loop
      declare Bad : Q.Packet := Q.Header_Request; begin
         Bad.Data (Bad_Field) := (if Bad_Field = 1 then 65_536 else 256);
         Check (not Q.Decode_Request (Bad).Valid);
      end;
   end loop;
   for Device in P.Device_Number loop
      for Func in P.Function_Number loop
         for Width in P.T.Access_Width range P.T.Byte_Access .. P.T.Dword_Access loop
            for Last_Base in Boolean loop
               for Last_Offset in Boolean loop
                  declare
                     Region : constant P.Region := ((0,0,Device,Func),(if Last_Base then 4092 else 0),4);
                     Offset : constant Unsigned_64 := (if Last_Offset then 4 - Unsigned_64 (P.T.Octets (Width)) else 0);
                     Encoded : constant Q.Encoded_Request := Q.Read_Request (Epoch, ID, Region, Offset, Width, Deadline);
                  begin
                     Check (Encoded.Status = Q.Encoded);
                     declare Decoded : constant Q.Request := Q.Decode_Request (Encoded.Item); begin
                        Check (Decoded.Valid and then Decoded.Kind = Q.Read and then Decoded.Epoch = Epoch and then Decoded.ID = ID);
                        Check (Decoded.Region = Region and then Decoded.Offset = Offset and then Decoded.Width = Width);
                        Check (Decoded.Deadline = Deadline and then not Decoded.For_Write);
                     end;
                  end;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Rejected ((Golden_Region with delta Device => (1,0,2,3)), 0, P.T.Byte_Access, P.Unsupported_Segment);
   Rejected ((Golden_Region with delta Device => (0,1,2,3)), 0, P.T.Byte_Access, P.Unsupported_Bus);
   Rejected (Golden_Region, 0, P.T.Byte_Access, P.Write_Refused, True);
   Rejected (Golden_Region, 0, P.T.Qword_Access, P.Unsupported_Width);
   Rejected (Golden_Region, 1, P.T.Dword_Access, P.Misaligned);
   Rejected (Golden_Region, 128, P.T.Byte_Access, P.Outside_Region);
   Check (Q.Read_Request (0,ID,Golden_Region,0,P.T.Byte_Access,Deadline).Status = Q.Missing_Identity);
   Check (Q.Read_Request (Epoch,ID,Golden_Region,0,P.T.Byte_Access,0).Status = Q.Invalid_Deadline);
   Check (Q.Read_Request (Epoch,ID,Golden_Region,0,P.T.Byte_Access,Q.No_Finite_Deadline).Status = Q.Invalid_Deadline);
   for Length in Unsigned_8 loop
      if Length /= 4 then Check (not Q.Decode_Request ((Base with delta Length => Length)).Valid); end if;
   end loop;
   for Flags in Unsigned_8 range 1 .. Unsigned_8'Last loop
      Check (not Q.Decode_Request ((Base with delta Flags => Flags)).Valid);
   end loop;
   for Reserved in Unsigned_16 range 1 .. Unsigned_16'Last loop
      Check (not Q.Decode_Request ((Base with delta Reserved => Reserved)).Valid);
   end loop;
   for Bit in 48 .. 55 loop
      declare Bad : Q.Packet := Base; begin
         Bad.Data (2) := Bad.Data (2) or Shift_Left (Unsigned_64'(1), Bit);
         Check (not Q.Decode_Request (Bad).Valid);
      end;
   end loop;
   for Version in Unsigned_64 range 0 .. 255 loop
      if Version /= Q.Version then
         declare Bad : Q.Packet := Base; begin
            Bad.Data (2) := (Bad.Data (2) and 16#00FF_FFFF_FFFF_FFFF#) or Shift_Left (Version,56);
            Check (not Q.Decode_Request (Bad).Valid);
         end;
      end if;
   end loop;
   declare
      Description : constant Q.Packet := Q.Describe_Request (Epoch, ID);
      Response : Q.Packet := Q.Describe_Result (16#E000_0000#, P.Bus_Bytes);
      Input : constant Q.Request := Q.Decode_Request (Description);
      Output : constant Q.Reply := Q.Decode_Reply (Response, Q.Describe);
      Bad : Q.Packet;
   begin
      Check (Input.Valid and then Input.Kind = Q.Describe and then Input.Epoch = Epoch and then Input.ID = ID);
      Check (Output.Valid and then Output.Status = Q.Ready and then Output.First = 16#E000_0000#
        and then Output.Bytes = P.Bus_Bytes);
      for Mutation in 1 .. 9 loop
         Bad := Description;
         case Mutation is
            when 1 => Bad.Length := 0;
            when 2 => Bad.Flags := 1;
            when 3 => Bad.Reserved := 1;
            when 4 => Bad.Data (0) := 0;
            when 5 => Bad.Data (1) := 0;
            when 6 => Bad.Data (2) := 0;
            when 7 => Bad.Data (3) := 1;
            when 8 => Bad.Data (2) := Q.Version + 1;
            when others => Bad.Label := 16#2BF#;
         end case;
         Check (not Q.Decode_Request (Bad).Valid);
      end loop;
      for Mutation in 1 .. 7 loop
         Bad := Response;
         case Mutation is
            when 1 => Bad.Data (1) := 16#E000_0000# + 1;
            when 2 => Bad.Data (1) := Unsigned_64'Last - P.Function_Bytes + 1;
            when 3 => Bad.Data (2) := P.Bus_Bytes - 1;
            when 4 => Bad.Data (2) := P.Bus_Bytes + 1;
            when 5 => Bad.Data (3) := Q.Version + 1;
            when 6 => Bad.Label := Q.Header_Reply;
            when others => Bad.Data (0) := Q.Outcome'Pos (Q.Done);
         end case;
         Check (not Q.Decode_Reply (Bad, Q.Describe).Valid);
      end loop;
      Response := Q.Header_Result (Epoch, ID);
      Check (not Q.Decode_Reply (Response, Q.Describe).Valid);
   end;
   for Kind in Q.Operation loop
      for Code in Q.Outcome loop
         if Code not in Q.Ready | Q.Done and then (Code /= Q.Policy_Refused or else Kind = Q.Read) then
            declare
               Reply : constant Q.Packet := Q.Error_Reply (Kind,Code,P.Misaligned);
               Decoded : constant Q.Reply := Q.Decode_Reply (Reply,Kind);
               Bad : Q.Packet := Reply;
            begin
               Check (Decoded.Valid and then Decoded.Status = Code and then Decoded.Value = 0);
               Bad.Data (1) := 1; Check (not Q.Decode_Reply (Bad,Kind).Valid);
            end;
         end if;
      end loop;
   end loop;
   declare Reply : constant Q.Reply := Q.Decode_Reply (Q.Header_Result (Epoch,ID),Q.Header); begin
      Check (Reply.Valid and then Reply.Status = Q.Ready and then Reply.Epoch = Epoch and then Reply.ID = ID);
   end;
   Check (not Q.Decode_Reply (Q.Header_Result (Epoch,ID),Q.Read).Valid);
   Check (not Q.Decode_Reply (Q.Read_Result (256),Q.Read,P.T.Byte_Access).Valid);
   Check (not Q.Decode_Reply (Q.Read_Result (65_536),Q.Read,P.T.Word_Access).Valid);
   Check (not Q.Decode_Reply (Q.Read_Result (16#1_0000_0000#),Q.Read,P.T.Dword_Access).Valid);
   Check (Q.Decode_Reply (Q.Read_Result (16#FFFF_FFFF#),Q.Read,P.T.Dword_Access).Valid);
   Ada.Text_IO.Put_Line ("PASS PCI protocol checks=" & Checks'Image);
end Protocol_Tests;
