with I2C_Channel_Controller; with CuBit.Messages; with CuBit.Control_Events; with CuBit.Deadlines; with CuBit.Process_IDs;
generic with package Device is new I2C_Channel_Controller (<>);
package I2C_Native_Channel_Service is
   package Channels is
      type Wake_Kind is (Work_Waiting, Message_Received);
      type Wake is record Kind : Wake_Kind := Work_Waiting; From : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process; Msg : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE; end record;
   end Channels;
   procedure Configure (Per_Client, Total, Service : Positive; Ready : out Boolean);
   procedure Handle_Open (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Is_Open : out Boolean; Reply : out CuBit.Messages.Message; Opened : out Boolean);
   procedure Turn;
   function Stopped return Boolean;
   function Retained return Boolean;
   procedure On_Event (Publisher : CuBit.Messages.Process_ID; Event : CuBit.Control_Events.Event);
   procedure Sleep (Deadline : CuBit.Deadlines.Deadline; Woke : out Channels.Wake);
end I2C_Native_Channel_Service;
