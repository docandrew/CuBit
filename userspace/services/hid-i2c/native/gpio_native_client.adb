pragma Ada_2022;
with Interfaces;
with CuBit.Handles.IPC;
with GPIO_Channel_Badges;
package body GPIO_Native_Client is
   package W renames GPIO_Control_Wire;
   package M renames CuBit.Messages;
   package P renames CuBit.Process_IDs;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_64;
   use type M.IPC_Flags;
   use type CuBit.Handles.ABI.Kind;
   function Resolve return Binding is
      D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Controller);
      Sender : P.Process_ID;
   begin
      if not D.Current then return (Endpoint_Refused, D.Why); end if;
      if D.Kind /= CuBit.Handles.ABI.Endpoint then return (Status => Wrong_Endpoint_Kind); end if;
      if not D.Held.Write then return (Status => Missing_Write_Right); end if;
      if not GPIO_Channel_Badges.Valid_Word (D.Authority_Tag) then return (Status => Wrong_Channel_Badge); end if;
      Sender := P.From_Word (D.Object);
      if not P.Is_Process (Sender) then return (Status => Invalid_Endpoint_Identity); end if;
      return (Endpoint_Resolved, Sender, D.Authority_Tag);
   end Resolve;
   procedure Submit (Item : W.Request; Token : W.Word; Accepted : out Boolean) is
      Data : constant W.Message_Words := W.Encode (Item);
      Message : M.Message := M.NULL_MESSAGE;
   begin
      Message.tag.label := W.Label_Of (Item.Kind);
      Message.tag.protocol := M.Protocol_Flags (W.Protocol_Version);
      Message.tag.length := Interfaces.Unsigned_8 (Data'Length);
      for I in Data'Range loop Message.words (I) := Data (I); end loop;
      Accepted := M.capSubmit (Controller, Message, Token);
   end Submit;
   procedure Receive (S : in out Controls.State; Item : M.CompletionEntry;
      Now : W.Word; Clock_Available : Boolean; Result : out Controls.Receive_Result)
   is
      Delivery : Controls.Delivery_Status;
      Length : Natural := Natural (Item.msg.tag.length);
   begin
      Result := Controls.Unrelated; if not Item.valid then return; end if;
      Delivery := (if Item.status = M.COMPLETION_OK then Controls.Delivered else Controls.Failed);
      if Length > Item.msg.words'Length or else Item.msg.tag.reserved /= 0
        or else Item.msg.tag.ipc /= M.No_IPC_Flags then Length := 0; end if;
      declare Data : W.Message_Words (1 .. Length); begin
         for I in Data'Range loop Data (I) := Item.msg.words (I - 1); end loop;
         Controls.Receive (S, Item.token, P.From_Word (Item.from), Delivery,
            Item.msg.tag.label, W.Word (Item.msg.tag.protocol), Data,
            Now, Clock_Available, Result);
      end;
   end Receive;
end GPIO_Native_Client;
