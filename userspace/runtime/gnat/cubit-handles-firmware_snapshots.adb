pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Handles.Kernel;
with CuBit.Messages;

package body CuBit.Handles.Firmware_Snapshots is
   package K renames CuBit.Handles.Kernel;
   package ABI renames Firmware_Snapshot_ABI;
   use type ABI.Outcome;

   --  A Call on the handle (Firmware_Snapshot_ABI's messages). A handle
   --  that is not a firmware snapshot is not answered as one: that is
   --  Not_Authorized here, as the system call answered it.
   function Call
     (Authority : Firmware_Snapshot;
      Words     : Firmware_Snapshot_ABI.Request_Words;
      Reply     : System.Address) return Unsigned_64
   is
      Answer : ABI.Reply with Import, Address => Reply;
      Msg : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Result : Unsigned_64;
      Outcome : Unsigned_64;
   begin
      Msg.tag.label := Unsigned_32'Mod (Words.Op);
      Msg.tag.length := ABI.Message_Words;
      Msg.words (0) := Words.Table;
      Msg.words (1) := Words.Offset;
      Msg.words (2) := Words.Destination;
      Msg.words (3) := Words.Capacity;
      Result := K.Call (K.Call_Via_Endpoint, K.Word (Authority),
                        Unsigned_64 (System.Storage_Elements.To_Integer (Msg'Address)),
                        CuBit.Messages.Wait_Forever);
      Outcome := Unsigned_64 (Msg.tag.label);
      if Result = 0 or else Result = Unsigned_64'Last
        or else Msg.tag.length /= ABI.Message_Words
        or else not ABI.Is_Outcome_Word (Outcome)
      then
         return ABI.Outcome_Word (ABI.Not_Authorized);
      end if;
      --  Only Ok carries reply words, for a known operation (Well_Formed).
      if ABI.To_Outcome (Outcome) = ABI.Ok and then ABI.Known_Operation (Words.Op) then
         Answer := ABI.Reply_Of
           (ABI.Operation_Of (Words.Op),
            [for I in ABI.Message_Index => Msg.words (Natural (I))]);
      end if;
      return Outcome;
   end Call;
end CuBit.Handles.Firmware_Snapshots;
