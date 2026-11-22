with CuBit.Firmware_Data_Exchange; with CuBit.Grant_References; with HID_Offer_Payload;
with Sender_Model; with Producer_Model;
package body CuBit.Handles.IPC is
   use Interfaces; use type HID_Offer_Payload.Decode_Status;
   package S renames Sender_Model; package M renames Producer_Model;
   function Describe (Item : Endpoint) return Description is
   begin
      M.Check (Item = 77 and then not M.Window_Open and then M.Ends = 1, "endpoint discovery only after AML window closes");
      return (Current => True, Kind => ABI.Endpoint, Held => (Read => True),
        Authority_Tag => CuBit.Firmware_Data_Exchange.Authority_Tag, Object => 16#F234_5678_0000_0009#);
   end Describe;
   function Call (Target : Endpoint; Msg : in out CuBit.Messages.Message; Deadline : Unsigned_64)
      return CuBit.Messages.MessageTag is
      P : HID_Offer_Payload.Request; D : HID_Offer_Payload.Decode_Result;
      Request : constant Unsigned_64 := Msg.words (0);
      Index : constant Positive := (if Request = 3 then 1 else 2);
      Ref : constant CuBit.Grant_References.Reference :=
        (slot => (if Index = 1 then 5 else 6), generation => 16#1234_5678#);
   begin
      S.Calls := S.Calls + 1; S.Offer_Calls (Index) := S.Offer_Calls (Index) + 1;
      M.Check (not M.Window_Open and then not M.Pending and then M.Collections = 1 and then M.Ends = 1,
        "no dispatch while AML window or retained results exist");
      M.Check (Request in 3 .. 4 and then Target = 77 and then Deadline = 1100, "full-ID sequence reserves GPIO1/completion2 and preserves original finite deadline");
      M.Check (Msg.tag.label = 16#252# and then Msg.tag.length = 3 and then Msg.tag.protocol = 0
        and then Msg.tag.ipc = 0 and then Msg.tag.reserved = 0, "canonical 0252 call frame");
      M.Check (Msg.words (1) = CuBit.Grant_References.Encode (Ref) and then Msg.words (2) = 552
        and then Msg.words (3) = 0, "canonical readonly grant descriptor");
      if Index = 1 then HID_Offer_Payload.Decode (S.Buffer (0 .. 551), P, D);
      else HID_Offer_Payload.Decode (S.Second_Buffer (0 .. 551), P, D); end if;
      M.Check (D.Status = HID_Offer_Payload.Decoded and then P.Epoch = S.Own_ID
        and then P.Catalog_Revision = 16#FFFF_FFFF_0000_0001#
        and then P.Device_Path.Names (1) = (if Index = 1 then "TPD0" else "TPD1"),
        "actual codec and transport copy preserve per-candidate full-width facts");
      if M.Mode = 20 then M.Boundary := False; end if;
      if (M.Mode = 14 and then S.Calls = 1) or else M.Mode in 15 | 20 then
         Msg.tag.label := CuBit.Messages.Kernel_Reply_First; return Msg.tag;
      end if;
      Msg.tag := (label => 16#252#, length => 4, others => <>);
      Msg.words := [Request, (if M.Mode = 16 then 2 else 1), 0, (if M.Mode = 16 then 0 else Unsigned_64 (Index + 6))];
      if M.Mode = 21 then Msg.words (0) := Request + 1; end if;
      return Msg.tag;
   end Call;
end CuBit.Handles.IPC;
