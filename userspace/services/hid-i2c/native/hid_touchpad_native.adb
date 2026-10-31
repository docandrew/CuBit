pragma Ada_2022;
with Interfaces;
with GPIO_Native_Event_Frames;
package body HID_Touchpad_Native is
   package M renames CuBit.Messages;
   package P renames CuBit.Process_IDs;
   package GW renames GPIO_Control_Wire;
   package T renames App.Bus.Transactions;
   package Frames renames GPIO_Native_Event_Frames;
   use type Interfaces.Unsigned_16, Interfaces.Unsigned_64, M.IPC_Flags;
   use type Live.Owner.Phase, App.Device.Interrupt_Result;
   function Drained return Boolean is (Live.Quiescent and then App.Count = 0);
   procedure Receive_Completion (Entry_Value : M.CompletionEntry;
      Now : W.Word; Clock_Available : Boolean; Result : out Completion_Report) is
      Length : Natural := Natural (Entry_Value.msg.tag.length);
      GPIO_Delivery : C.Delivery_Status;
      I2C_Delivery : T.Delivery_Status;
   begin
      Result := (others => <>); if not Entry_Value.valid then return; end if;
      Result.Frame_Valid := Length <= Entry_Value.msg.words'Length and then
         Entry_Value.msg.tag.reserved = 0 and then Entry_Value.msg.tag.ipc = M.No_IPC_Flags;
      if not Result.Frame_Valid then Length := 0; end if;
      if Entry_Value.token /= 0 and then
        (Entry_Value.token = Live.Control_Token or else Entry_Value.token = Reset.Observe.Token)
      then
         GPIO_Delivery := (if Entry_Value.status = M.COMPLETION_OK then C.Delivered else C.Failed);
         declare Words : GW.Message_Words (1 .. Length); begin
            for Index in Words'Range loop Words (Index) := Entry_Value.msg.words (Index - 1); end loop;
            if Entry_Value.token = Live.Control_Token then
               Result.Routed_To := GPIO_Input_Control;
               Live.Receive_Control (Entry_Value.token, P.From_Word (Entry_Value.from), GPIO_Delivery,
                  Entry_Value.msg.tag.label, W.Word (Entry_Value.msg.tag.protocol), Words, Now, Clock_Available, Result.GPIO_Result);
            else
               Result.Routed_To := GPIO_Reset_Control;
               Reset.Receive (Entry_Value.token, P.From_Word (Entry_Value.from), GPIO_Delivery,
                  Entry_Value.msg.tag.label, W.Word (Entry_Value.msg.tag.protocol), Words, Result.GPIO_Result);
            end if;
         end;
      else
         Result.Routed_To := I2C_Transfer;
         case Entry_Value.status is
            when M.COMPLETION_OK => I2C_Delivery := T.Delivered;
            when M.COMPLETION_TARGET_DIED => I2C_Delivery := T.Target_Died;
            when M.COMPLETION_CANCELLED => I2C_Delivery := T.Cancelled;
            when M.COMPLETION_QUEUE_OVERFLOW => I2C_Delivery := T.Queue_Overflow;
            when others => I2C_Delivery := T.Transport_Error;
         end case;
         declare Words : W.Message_Words (1 .. Length); begin
            for Index in Words'Range loop Words (Index) := Entry_Value.msg.words (Index - 1); end loop;
            App.Receive (Entry_Value.token, P.From_Word (Entry_Value.from), I2C_Delivery,
               Entry_Value.msg.tag.label, W.Word (Entry_Value.msg.tag.protocol), Words, Now, Clock_Available, Result.I2C_Result);
         end;
      end if;
   end Receive_Completion;
   procedure Receive_Event (Publisher : P.Process_ID; Message : M.Message; Result : out Event_Result) is
      Parsed : constant Frames.Decoding := Frames.Decode (Publisher, Reset.Expected_Sender, Message, Expected_Badge);
      Latched : App.Device.Interrupt_Result;
      Buffered : Live.Owner.Event_Result;
   begin
      case Parsed.Outcome is
         when Frames.Unrelated => Result := Unrelated;
         when Frames.Invalid_Binding => Result := Invalid_Binding;
         when Frames.Wrong_Publisher => Result := Wrong_Publisher;
         when Frames.Wrong_Badge => Result := Wrong_Badge;
         when Frames.Invalid_Frame => Result := Invalid_Frame;
         when Frames.Decoded =>
            if Live.Observe.At_Phase = Live.Owner.Cold then
               App.Reset_Interrupt (Parsed.Value.Lease, Latched);
               Result := (if Latched = App.Device.Latched then Reset_Latched else Unrelated);
            else
               Live.Receive_Event (Parsed.Value, Buffered);
               Result := (case Buffered is when Live.Owner.Buffered => Input_Buffered,
                  when Live.Owner.Duplicate => Input_Duplicate, when Live.Owner.Unrelated => Unrelated);
            end if;
      end case;
   end Receive_Event;
   function Wake_Deadline (Now, Otherwise : W.Word; Include_Output : Boolean)
      return W.Word is
      Hardware : constant W.Word := Reset.Wake_Deadline
        (Now, Live.Wake_Deadline (Now, Otherwise));
   begin
      return (if Include_Output then App.Wake_Deadline (Now, Hardware) else Hardware);
   end Wake_Deadline;
   procedure Step (Now : W.Word; Clock_Available : Boolean) is
   begin App.Step (Now, Clock_Available); Live.Step (Now, Clock_Available); end Step;
end HID_Touchpad_Native;
