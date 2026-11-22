pragma Ada_2022;
with Interfaces;
with CuBit.Memory_Grants;
with CuBit.Handles.IPC;
package body HID_I2C_Native_Client is
   package W renames I2C_Transfer_Wire;
   package M renames CuBit.Messages;
   use type Interfaces.Unsigned_16;
   use type CuBit.Handles.ABI.Kind;
   use type M.IPC_Flags;
   function Resolve return Binding is
      Description : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Controller);
      Sender : CuBit.Process_IDs.Process_ID;
   begin
      if not Description.Current then return (Endpoint_Refused, Description.Why); end if;
      if Description.Kind /= CuBit.Handles.ABI.Endpoint then return (Status => Wrong_Endpoint_Kind); end if;
      Sender := CuBit.Process_IDs.From_Word (Description.Object);
      if not CuBit.Process_IDs.Is_Process (Sender) then return (Status => Invalid_Endpoint_Identity); end if;
      return (Endpoint_Resolved, Sender);
   end Resolve;
   procedure Submit (Item : W.Request; Token : W.Word; Accepted : out Boolean) is
      Words : constant W.Request_Words := W.Encode (Item);
      Message : M.Message := M.NULL_MESSAGE;
   begin
      Message.tag.label := W.Transfer_Operation;
      Message.tag.protocol := M.Protocol_Flags (W.Protocol_Version);
      Message.tag.length := Interfaces.Unsigned_8 (W.Request_Word_Count);
      for N in Words'Range loop Message.words (N) := Words (N); end loop;
      Accepted := M.capSubmit (Controller, Message, Token);
   end Submit;
   procedure Revoke (Item : CuBit.Grant_References.Reference; Accepted : out Boolean) is
   begin CuBit.Memory_Grants.Revoke (Item, Accepted); end Revoke;
   function Retired (Item : CuBit.Grant_References.Reference) return Boolean is
     (CuBit.Memory_Grants.Retirement_Confirmed (Item));
   procedure Receive
     (S : in out Transactions.State; Entry_Value : M.CompletionEntry;
      Now : W.Word; Clock_Available : Boolean;
      Result : out Transactions.Completion_Result)
   is
      Delivery : Transactions.Delivery_Status;
      Length : Natural := Natural (Entry_Value.msg.tag.length);
   begin
      Result := Transactions.Unrelated;
      if not Entry_Value.valid then return; end if;
      case Entry_Value.status is
         when M.COMPLETION_OK => Delivery := Transactions.Delivered;
         when M.COMPLETION_TARGET_DIED => Delivery := Transactions.Target_Died;
         when M.COMPLETION_CANCELLED => Delivery := Transactions.Cancelled;
         when M.COMPLETION_QUEUE_OVERFLOW => Delivery := Transactions.Queue_Overflow;
         when others => Delivery := Transactions.Transport_Error;
      end case;
      if Length > Entry_Value.msg.words'Length or else Entry_Value.msg.tag.reserved /= 0 or else Entry_Value.msg.tag.ipc /= M.No_IPC_Flags then Length := 0; end if;
      declare
         Words : W.Message_Words (1 .. Length);
      begin
         for N in Words'Range loop Words (N) := Entry_Value.msg.words (N - 1); end loop;
         Transactions.Receive (S, Entry_Value.token,
           CuBit.Process_IDs.From_Word (Entry_Value.from), Delivery,
           Entry_Value.msg.tag.label, W.Word (Entry_Value.msg.tag.protocol),
           Words, Now, Clock_Available, Result);
      end;
   end Receive;
end HID_I2C_Native_Client;
