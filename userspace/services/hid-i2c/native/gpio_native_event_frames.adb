pragma Ada_2022;
with Interfaces;
with GPIO_Channel_Badges;
package body GPIO_Native_Event_Frames is
   package M renames CuBit.Messages;
   package P renames CuBit.Process_IDs;
   use type P.Process_ID, W.Word, W.Operation, W.Decode_Status;
   use type Interfaces.Unsigned_16, M.IPC_Flags;
   function Decode (Publisher, Expected_Sender : P.Process_ID;
      Message : M.Message; Expected_Badge : W.Word) return Decoding is
      Parsed : W.Event_Decoding;
      Length : constant Natural := Natural (Message.tag.length);
   begin
      if Message.tag.label /= W.OP_GPIO_EVENT then return (Outcome => Unrelated); end if;
      if not P.Is_Process (Expected_Sender) or else not GPIO_Channel_Badges.Valid_Word (Expected_Badge)
      then return (Outcome => Invalid_Binding); end if;
      if Publisher /= Expected_Sender then return (Outcome => Wrong_Publisher); end if;
      if Message.authorityTag /= Expected_Badge then return (Outcome => Wrong_Badge); end if;
      if Length > Message.words'Length or else Message.tag.reserved /= 0 or else Message.tag.ipc /= M.No_IPC_Flags
      then return (Outcome => Invalid_Frame); end if;
      declare Words : W.Message_Words (1 .. Length); begin
         for Index in Words'Range loop Words (Index) := Message.words (Index - 1); end loop;
         Parsed := W.Decode_Event (Message.tag.label, W.Word (Message.tag.protocol), Words);
      end;
      return (if Parsed.Status = W.Decoded then (Decoded, Parsed.Value) else (Outcome => Invalid_Frame));
   end Decode;
end GPIO_Native_Event_Frames;
