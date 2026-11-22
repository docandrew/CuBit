pragma Ada_2022;
with System; with System.Storage_Elements;
with CuBit.Handles.IPC; with CuBit.Channel_Clients; with CuBit.Channel_Protocol;
with CuBit.Deadlines; with I2C_Channel;
package body HID_I2C_Native_Buffers is
   use System.Storage_Elements;
   use type P.Process_ID, CuBit.Handles.ABI.Kind, System.Address, W.Word;
   package Channels is new CuBit.Channel_Clients (I2C_Channel.RC, I2C_Channel.Element);
   use type Channels.Open_Outcome, Channels.Submit_Result, Channels.Take_Outcome;
   Channel : Channels.Client;
   Receipt : Channels.Retirement_Receipt;
   Attempted, Transport_Attempted, Closing, Armed, Have_Tag : Boolean := False;
   Pending_Tag : Channels.Tag := Channels.Tags.No_Tag;
   Pending_Token : W.Word := 0;
   package body Endpoint_Client is
      Cache_Attempted : Boolean := False;
      Cached : Binding;
      function Resolve return Binding is
         Sender : P.Process_ID;
      begin
         if Cache_Attempted then return Cached; end if;
         Cache_Attempted := True;
         declare Description : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Controller); begin
            if not Description.Current then Cached := (Endpoint_Refused, Description.Why);
            elsif Description.Kind /= CuBit.Handles.ABI.Endpoint then Cached := (Status => Wrong_Endpoint_Kind);
            else
               Sender := P.From_Word (Description.Object);
               Cached := (if P.Is_Process (Sender) then (Endpoint_Resolved, Sender)
                          else (Status => Invalid_Endpoint_Identity));
            end if;
         end;
         return Cached;
      end Resolve;
   end Endpoint_Client;
   procedure Open_Channel (Expected_Peer : P.Process_ID; Deadline : W.Millisecond; Ready : out Boolean) is
      Binding : constant Endpoint_Client.Binding := Endpoint_Client.Resolve;
      Outcome : Channels.Open_Outcome;
      Refusal : CuBit.Channel_Protocol.Open_Refusal;
      use type Endpoint_Client.Binding_Status;
   begin
      Ready := False; if Attempted then return; end if;
      Attempted := True;
      if Binding.Status /= Endpoint_Client.Endpoint_Resolved or else Binding.Sender /= Expected_Peer then return; end if;
      Transport_Attempted := True;
      Channels.Open (Channel, Controller, CuBit.Deadlines.At_Monotonic (Deadline), Outcome, Refusal);
      Ready := Outcome = Channels.Connected;
   end Open_Channel;
   procedure Store_Write (Data : D.Bytes; Stored : out Boolean) is
      View : constant I2C_Channel.RC.Payload_View := Channels.Write_Payload (Channel);
      Offset : Storage_Offset := 0;
   begin
      Stored := False;
      if Closing or else not Channels.Is_Open (Channel) or else not View.Writable
        or else View.Base = System.Null_Address or else Data'Length > Natural (Maximum_Transfer)
        or else Data'Length > View.Bytes then return; end if;
      for B of Data loop
         declare Item : D.Byte with Import, Volatile, Address => View.Base + Offset; begin Item := B; end;
         Offset := Offset + 1;
      end loop;
      Stored := True;
   end Store_Write;
   procedure Load_Read (Data : out D.Bytes; Loaded : out Boolean) is
      View : constant I2C_Channel.RC.Payload_View := Channels.Read_Payload (Channel);
      Offset : Storage_Offset := 0;
   begin
      Loaded := False;
      if Closing or else not Channels.Is_Open (Channel) or else View.Writable
        or else View.Base = System.Null_Address or else Data'Length > Natural (Maximum_Transfer)
        or else Data'Length > View.Bytes then Data := [others => 0]; return; end if;
      for B of Data loop
         declare Item : D.Byte with Import, Volatile, Address => View.Base + Offset; begin B := Item; end;
         Offset := Offset + 1;
      end loop;
      Loaded := True;
   end Load_Read;
   procedure Submit_Header (Header : I2C_Channel_Wire.Raw_Request; Token : W.Word; Accepted : out Boolean) is
      Result : Channels.Submit_Result;
      Number : Channels.Tag;
   begin
      Accepted := False;
      if Closing or else Have_Tag or else Token = 0 then return; end if;
      Channels.Submit (Channel, Header, Number, Result);
      if Result = Channels.Queued then
         Pending_Tag := Number; Pending_Token := Token; Have_Tag := True; Accepted := True;
      end if;
   end Submit_Header;
   procedure Poll_Answer (Token : W.Word; Words : out W.Completion_Words; Available, Ended : out Boolean) is
      Outcome : Channels.Take_Outcome;
   begin
      Words := [others => 0]; Available := False; Ended := False;
      if Closing or else not Channels.Is_Open (Channel) or else not Have_Tag or else Token /= Pending_Token then
         Ended := True; return;
      end if;
      Channels.Try_Take (Channel, Pending_Tag, Words, Outcome);
      case Outcome is
         when Channels.Answered => Have_Tag := False; Available := True;
         when Channels.Ended => Ended := True;
         when Channels.Pending => null;
      end case;
   end Poll_Answer;
   procedure Resume is
   begin if Armed then Channels.Disarm (Channel); Armed := False; end if; end Resume;
   procedure Prepare_Wait (Ready : out Boolean) is
   begin
      Ready := True;
      if Closing or else not Have_Tag then return; end if;
      if not Channels.Is_Open (Channel) then Ready := False; return; end if;
      if Armed then Ready := False; return; end if;
      Channels.Arm (Channel, Armed); Ready := Armed;
   end Prepare_Wait;
   procedure Close_Channel is
   begin
      if Closing then return; end if;
      Closing := True; Resume;
      Channels.Close (Channel, Receipt);
   end Close_Channel;
   function Retirement_Confirmed return Boolean is
     (Closing and then (not Transport_Attempted or else Channels.Retirement_Confirmed (Receipt)));
   procedure Grant_Notice (Item : CuBit.Control_Events.Event) is
      Ended : Boolean;
   begin
      Channels.Handle_Event (Channel, Item, Ended);
      -- Close_Channel subsequently captures the helper's retained receipt,
      -- even if this authenticated event already detached the channel views.
   end Grant_Notice;
end HID_I2C_Native_Buffers;
