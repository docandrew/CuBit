pragma Ada_2022;
with I2C_Channel_Controller; with I2C_Channel;
with CuBit.Channel_Servers; with CuBit.Channel_Limits;
with CuBit.Messages; with CuBit.Control_Events; with CuBit.Deadlines;
with Interfaces;
generic
   with package Device is new I2C_Channel_Controller (<>);
package I2C_Native_Channel_Service is
   -- Internal admission bridges. From and Badge are kernel-stamped open
   -- metadata supplied by Channel_Servers, never channel payload words.
   function Admit (Offer : Positive; From : CuBit.Messages.Process_ID;
                   Badge : Interfaces.Unsigned_64) return Boolean;
   function Can_Serve (Offer : Positive) return Boolean;
   package Channels is new CuBit.Channel_Servers (I2C_Channel.RC, 1, Admit, Can_Serve);
   procedure Configure
     (Per_Client, Total : CuBit.Channel_Limits.Channel_Limit;
      Service : CuBit.Channel_Limits.Channel_Class; Ready : out Boolean);
   procedure Handle_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Reply : out CuBit.Messages.Message; Opened : out Boolean);
   procedure Turn;
   function Stopped return Boolean;
   function Retained return Boolean;
   procedure On_Event
     (Publisher : CuBit.Messages.Process_ID; Event : CuBit.Control_Events.Event);
   procedure Sleep (Deadline : CuBit.Deadlines.Deadline; Woke : out Channels.Wake);
   -- Main owns clock/launch/MMIO startup and diagnostic logging. It starts
   -- Device first, then configures this singleton from validated manifest
   -- limits, dispatches received open replies and authentic event metadata,
   -- and calls Turn between bounded timed sleeps. No synchronous peer call.
end I2C_Native_Channel_Service;
