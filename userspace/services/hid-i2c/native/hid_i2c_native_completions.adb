pragma Ada_2022;
with Interfaces;
with CuBit.Process_IDs;
package body HID_I2C_Native_Completions is
   package M renames CuBit.Messages;
   package C renames Reset.Controls;
   package GW renames C.W;
   use type Interfaces.Unsigned_16, Interfaces.Unsigned_64, M.IPC_Flags;
   procedure Receive (S : in out Device.State; Input_State : in out Input.State; Entry_Value : M.CompletionEntry;
      Now : W.Word; Clock_Available : Boolean; Result : out Report) is
      Length : Natural := Natural (Entry_Value.msg.tag.length);
      I2C_Delivery : Device.T.Delivery_Status;
      GPIO_Delivery : C.Delivery_Status;
   begin
      Result := (others => <>);
      if not Entry_Value.valid then return; end if;
      Result.Frame_Valid := Length <= Entry_Value.msg.words'Length and then
         Entry_Value.msg.tag.reserved = 0 and then Entry_Value.msg.tag.ipc = M.No_IPC_Flags;
      if not Result.Frame_Valid then Length := 0; end if;
      if Entry_Value.token /= 0 and then Entry_Value.token = Input.Control_Token (Input_State) then
         Result.Routed_To := GPIO_Input_Control;
         GPIO_Delivery := (if Entry_Value.status = M.COMPLETION_OK then C.Delivered else C.Failed);
         declare Words : GW.Message_Words (1 .. Length); begin
            for Index in Words'Range loop Words (Index) := Entry_Value.msg.words (Index - 1); end loop;
            Input.Receive_Control (Input_State, Entry_Value.token, CuBit.Process_IDs.From_Word (Entry_Value.from),
               GPIO_Delivery, Entry_Value.msg.tag.label, GW.Word (Entry_Value.msg.tag.protocol),
               Words, Now, Clock_Available, Result.GPIO_Result);
         end;
      elsif Entry_Value.token /= 0 and then Entry_Value.token = Reset.Observe.Token then
         Result.Routed_To := GPIO_Reset_Control;
         GPIO_Delivery := (if Entry_Value.status = M.COMPLETION_OK then C.Delivered else C.Failed);
         declare Words : GW.Message_Words (1 .. Length); begin
            for Index in Words'Range loop Words (Index) := Entry_Value.msg.words (Index - 1); end loop;
            Reset.Receive (Entry_Value.token, CuBit.Process_IDs.From_Word (Entry_Value.from),
               GPIO_Delivery, Entry_Value.msg.tag.label, GW.Word (Entry_Value.msg.tag.protocol),
               Words, Result.GPIO_Result);
         end;
      else
         Result.Routed_To := I2C_Transfer;
         case Entry_Value.status is
            when M.COMPLETION_OK => I2C_Delivery := Device.T.Delivered;
            when M.COMPLETION_TARGET_DIED => I2C_Delivery := Device.T.Target_Died;
            when M.COMPLETION_CANCELLED => I2C_Delivery := Device.T.Cancelled;
            when M.COMPLETION_QUEUE_OVERFLOW => I2C_Delivery := Device.T.Queue_Overflow;
            when others => I2C_Delivery := Device.T.Transport_Error;
         end case;
         declare Words : W.Message_Words (1 .. Length); begin
            for Index in Words'Range loop Words (Index) := Entry_Value.msg.words (Index - 1); end loop;
            Device.Receive (S, Entry_Value.token, CuBit.Process_IDs.From_Word (Entry_Value.from),
               I2C_Delivery, Entry_Value.msg.tag.label, W.Word (Entry_Value.msg.tag.protocol),
               Words, Now, Clock_Available, Result.I2C_Result);
         end;
      end if;
   end Receive;
end HID_I2C_Native_Completions;
