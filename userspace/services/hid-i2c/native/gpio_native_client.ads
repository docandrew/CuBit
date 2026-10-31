pragma Ada_2022;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
with GPIO_Client_Controls;
generic
   Controller : CuBit.Handles.Endpoint;
package GPIO_Native_Client is
   type Binding_Status is (Endpoint_Resolved, Endpoint_Refused,
      Wrong_Endpoint_Kind, Invalid_Endpoint_Identity, Missing_Write_Right, Wrong_Channel_Badge);
   type Binding (Status : Binding_Status := Endpoint_Refused) is record
      case Status is
         when Endpoint_Resolved =>
            Sender : CuBit.Process_IDs.Process_ID;
            Badge : GPIO_Control_Wire.Word;
         when Endpoint_Refused => Why : CuBit.Handles.Refusal;
         when others => null;
      end case;
   end record;
   function Resolve return Binding;
   procedure Submit (Item : GPIO_Control_Wire.Request;
      Token : GPIO_Control_Wire.Word; Accepted : out Boolean);
   package Controls is new GPIO_Client_Controls (Submit);
   procedure Receive (S : in out Controls.State;
      Item : CuBit.Messages.CompletionEntry;
      Now : GPIO_Control_Wire.Word; Clock_Available : Boolean;
      Result : out Controls.Receive_Result);
   -- Endpoint from generated Startup; Resolve once, then retain its opaque
   -- process lifetime. Owner dispatches only real completion entries here.
   -- No polling another token's queue, peer Call, implicit retry or event-route
   -- authority creation. GPIO event-sink admission is separate setup work.
end GPIO_Native_Client;
