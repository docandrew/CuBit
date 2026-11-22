pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with ACPI_Query_Transport;
with ACPI_Query_Admission;
with ACPI_Protocol;
procedure Query_Codec_Tests is
   package T renames ACPI_Query_Transport;
   package A renames ACPI_Query_Admission;
   use type T.Word, T.Message_Label, T.Header_Byte, T.Reserved_Field;
   use type T.Call_Status, T.Words, T.Reply_Details;
   use type A.Outcome, A.PID.Process_ID;
   Checks : Natural := 0;
   Extent : constant T.Word := 4096;
   High_Reference : constant T.Word := A.References.Encode
     ((A.References.Global_Slot'Last, A.References.Generation'Last));
   High_Sender : constant A.PID.Process_ID := A.PID.From_Word (16#FEDC_BA98_7654_3210#);
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Base_Request return T.Packet is (T.Encode_Request (High_Reference, Extent));
   function Base_Reply return T.Packet is (T.Encode_Reply ((T.Completed, T.Minimum_Reply_Bytes, 512)));
   procedure Reject_Request (Packet : T.Packet; Selected : Boolean := True) is
      Result : constant T.Request_Decode := T.Decode_Request (Packet);
   begin
      Check (Result.Selected = Selected and then not Result.Decoded
        and then Result.Reference_Word = 0 and then Result.Declared_Length = 0,
        "request rejection preserves selection and clears outputs");
   end Reject_Request;
   procedure Reject_Reply (Packet : T.Packet; Length : T.Word := Extent; Selected : Boolean := True) is
      Result : constant T.Reply_Decode := T.Decode_Reply (Packet, Length);
   begin
      Check (Result.Selected = Selected and then not Result.Decoded
        and then Result.Value = (T.Malformed, 0, 0), "reply rejection clears typed result");
   end Reject_Reply;
   procedure Check_Allocation
     (Request_Label, Reply_Label : T.Message_Label; Length : T.Header_Byte; Minimum : T.Word) is
   begin
      Check (Request_Label = 29 and then Reply_Label = 29 and then Length = 4
        and then Minimum = 256, "allocated selectors/framing and existing reply header");
   end Check_Allocation;
   Packet : T.Packet;
begin
   Check_Allocation (T.Request_Label, T.Reply_Label, T.Canonical_Length, T.Minimum_Reply_Bytes);
   Check (High_Reference > T.Maximum_Reply_Number, "fixture exercises unsigned reference high bit");
   for Ref of T.Words'[0, High_Reference, T.Maximum_Reply_Number + 1, T.Word'Last] loop
      declare
         Encoded : constant T.Packet := T.Encode_Request (Ref, T.Word'Last);
         Decoded : constant T.Request_Decode := T.Decode_Request (Encoded);
      begin
         Check (Encoded.Label = 29 and then Encoded.Length = 4 and then Encoded.Flags = 0
           and then Encoded.Reserved = 0 and then Encoded.Data = [Ref, T.Word'Last, 0, 0],
           "exact four-word request with unrestricted reference and length bits");
         Check (Decoded.Selected and then Decoded.Decoded and then Decoded.Reference_Word = Ref
           and then Decoded.Declared_Length = T.Word'Last, "shape decode never narrows opaque words");
      end;
   end loop;
   for Length in T.Header_Byte loop
      if Length /= T.Canonical_Length then
         Packet := Base_Request; Packet.Length := Length; Reject_Request (Packet);
         Packet := Base_Reply; Packet.Length := Length; Reject_Reply (Packet);
      end if;
   end loop;
   for Flags in T.Header_Byte range 1 .. T.Header_Byte'Last loop
      Packet := Base_Request; Packet.Flags := Flags; Reject_Request (Packet);
      Packet := Base_Reply; Packet.Flags := Flags; Reject_Reply (Packet);
   end loop;
   for Reserved in T.Reserved_Field range 1 .. T.Reserved_Field'Last loop
      Packet := Base_Request; Packet.Reserved := Reserved; Reject_Request (Packet);
      Packet := Base_Reply; Packet.Reserved := Reserved; Reject_Reply (Packet);
   end loop;
   for Bit in 0 .. 63 loop
      declare Value : constant T.Word := Interfaces.Shift_Left (T.Word'(1), Bit); begin
         for Index in T.Word_Index range T.Request_Reserved_First .. T.Last_Index loop
            Packet := Base_Request; Packet.Data (Index) := Value; Reject_Request (Packet);
         end loop;
         Packet := Base_Reply; Packet.Data (T.Last_Index) := Value; Reject_Reply (Packet);
      end;
   end loop;
   for Label of T.Words'[0, 28, 30, T.Word (T.Message_Label'Last)] loop
      Packet := Base_Request; Packet.Label := T.Message_Label (Label); Reject_Request (Packet, False);
      Packet := Base_Reply; Packet.Label := T.Message_Label (Label); Reject_Reply (Packet, Selected => False);
   end loop;
   declare Code : T.Word := 0; begin
      for Status in T.Call_Status loop
         declare
            Written : constant T.Word := (if Status = T.Completed then T.Minimum_Reply_Bytes else 0);
            Details : constant T.Reply_Details := (Status, Written, 512);
            Encoded : constant T.Packet := T.Encode_Reply (Details);
            Decoded : constant T.Reply_Decode := T.Decode_Reply (Encoded, Extent);
         begin
            Check (T.Status_Code (Status) = Code and then T.Status_From_Code (Code) = Status,
              "all thirteen allocated stable status numbers");
            Check (Encoded.Data = [Code, Written, 512, 0] and then Encoded.Label = 29,
              "exact reply words with required bytes in word2");
            Check (Decoded.Selected and then Decoded.Decoded and then Decoded.Value = Details,
              "reply status and counts round trip");
            if Status /= T.Completed then
               Check (T.Decode_Reply (Encoded, 0).Decoded, "error replies do not claim any loan payload");
               Packet := Encoded; Packet.Data (T.Written_Index) := 1; Reject_Reply (Packet);
            end if;
         end;
         Code := Code + 1;
      end loop;
      Check (Code = 13, "all status variants exercised");
   end;
   for Written in T.Word range 0 .. T.Minimum_Reply_Bytes - 1 loop
      Packet := Base_Reply; Packet.Data (T.Written_Index) := Written; Reject_Reply (Packet);
   end loop;
   Packet := Base_Reply; Reject_Reply (Packet, T.Minimum_Reply_Bytes - 1);
   Packet.Data (T.Written_Index) := Extent + 1; Reject_Reply (Packet);
   Packet := Base_Reply; Packet.Data (T.Status_Index) := 13; Reject_Reply (Packet);
   Packet.Data (T.Status_Index) := T.Word'Last; Reject_Reply (Packet);
   Packet := Base_Reply; Packet.Data (T.Written_Index) := T.Maximum_Reply_Number + 1; Reject_Reply (Packet);
   Packet := Base_Reply; Packet.Data (T.Required_Index) := T.Maximum_Reply_Number + 1; Reject_Reply (Packet);
   for Status in T.Call_Status loop
      declare
         Bad : constant T.Reply_Details := (Status, T.Word'Last, T.Word'Last);
         Result : constant T.Reply_Decode := T.Decode_Reply (T.Encode_Reply (Bad), Extent);
      begin
         Check (not T.Valid (Bad) and then Result.Decoded and then Result.Value = (T.Invalid_Response, 0, 0),
           "unrepresentable backend counts produce explicit invalid response without truncation");
      end;
   end loop;
   declare
      Bad : constant T.Reply_Details := (T.Denied, 1, 0);
      Result : constant T.Reply_Decode := T.Decode_Reply (T.Encode_Reply (Bad), Extent);
   begin Check (Result.Decoded and then Result.Value = (T.Invalid_Response, 0, 0),
     "nonzero failure payload cannot be encoded as a legitimate failure"); end;
   declare
      Details : constant T.Reply_Details := (T.Denied, 0, T.Maximum_Reply_Number);
      Result : constant T.Reply_Decode := T.Decode_Reply (T.Encode_Reply (Details), Extent);
   begin Check (Result.Decoded and then Result.Value = Details, "largest signed-compatible requirement preserved"); end;
   declare
      D : constant T.Request_Decode := T.Decode_Request (Base_Request);
      Decision : constant A.Decision := A.Admit (High_Sender, ACPI_Protocol.Observer_Tag,
        D.Decoded, D.Reference_Word, D.Declared_Length, Extent, Extent);
   begin
      Check (Decision.Status = A.Admitted and then Decision.Value.Sender = High_Sender
        and then A.References.Encode (Decision.Value.Loan) = High_Reference,
        "real admission preserves full sender and reference after codec");
      Check (A.Admit (High_Sender, 0, D.Decoded, D.Reference_Word, D.Declared_Length,
        Extent, Extent).Status = A.Denied, "word framing does not substitute for kernel observer stamp");
      Check (A.Admit (High_Sender, ACPI_Protocol.Observer_Tag, True, 0, Extent,
        Extent, Extent).Status = A.Invalid_Reference, "shape-valid zero reference remains invalid at admission");
      Check (A.Admit (High_Sender, ACPI_Protocol.Observer_Tag, True, High_Reference, T.Word'Last,
        Extent, Extent).Status = A.Invalid_Extent, "unrestricted length checked before native narrowing/acquisition");
   end;
   Check (Checks >= 132_000, "complete tag-space boundary matrix executed");
   Ada.Text_IO.Put_Line ("ACPI query transport codec checks" & Checks'Image);
end Query_Codec_Tests;
