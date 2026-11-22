pragma Ada_2022;
with CuBit.Handles; with CuBit.Process_IDs; with CuBit.Control_Events;
with HID_I2C_Descriptors; with HID_I2C_Buffered_Client;
with I2C_Channel_Wire; with I2C_Transfer_Wire;
generic
   Maximum_Transfer : I2C_Transfer_Wire.Capacity;
   Controller : CuBit.Handles.Endpoint;
package HID_I2C_Native_Buffers is
   package W renames I2C_Transfer_Wire;
   package D renames HID_I2C_Descriptors;
   package P renames CuBit.Process_IDs;
   package Endpoint_Client is
      type Binding_Status is (Endpoint_Resolved, Endpoint_Refused, Wrong_Endpoint_Kind, Invalid_Endpoint_Identity);
      type Binding (Status : Binding_Status := Endpoint_Refused) is record
         case Status is
            when Endpoint_Resolved => Sender : P.Process_ID;
            when Endpoint_Refused => Why : CuBit.Handles.Refusal;
            when others => null;
         end case;
      end record;
      function Resolve return Binding;
      -- One exact endpoint inspection, cached for this non-reconnecting session.
   end Endpoint_Client;
   procedure Open_Channel (Expected_Peer : P.Process_ID; Deadline : W.Millisecond; Ready : out Boolean);
   procedure Store_Write (Data : D.Bytes; Stored : out Boolean);
   procedure Load_Read (Data : out D.Bytes; Loaded : out Boolean);
   procedure Submit_Header (Header : I2C_Channel_Wire.Raw_Request; Token : W.Word; Accepted : out Boolean);
   procedure Poll_Answer (Token : W.Word; Words : out W.Completion_Words; Available, Ended : out Boolean);
   procedure Close_Channel;
   function Retirement_Confirmed return Boolean;
   package Client is new HID_I2C_Buffered_Client
     (Maximum_Transfer, Open_Channel, Store_Write, Load_Read, Submit_Header,
      Poll_Answer, Close_Channel, Retirement_Confirmed);
   procedure Prepare_Wait (Ready : out Boolean);
   procedure Resume;
   procedure Grant_Notice (Item : CuBit.Control_Events.Event);
   -- Sole native pump calls Resume before stepping, Prepare_Wait immediately
   -- before waiting, and Grant_Notice only after kernel publisher/frame checks.
   -- CCL endpoint target identity must remain stable for the capability's life;
   -- acquiring peer pages is mediated by that exact admitted endpoint.
end HID_I2C_Native_Buffers;
