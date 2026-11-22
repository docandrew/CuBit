pragma Ada_2022;
with I2C_Channel_Turn; with I2C_Channel_Badges; with I2C_Transfer_Wire;
with CuBit.Process_IDs; with Intel_LPSS_I2C_Profile;
package body I2C_Native_Channel_Service is
   package RC renames I2C_Channel.RC;
   package W renames I2C_Transfer_Wire;
   package B renames I2C_Channel_Badges;
   use type Device.Controller.Phase, B.Decode_Status, B.Address_Mode, B.Hertz;
   S : Channels.Server;
   Started, Attempted : Boolean := False;
   procedure Next_Channel (Found : out Boolean; Index : out Channels.Index) is
   begin Channels.Next (S, Found, Index); end Next_Channel;
   procedure Take (Index : Channels.Index; Item : out RC.Queues.Submission; Got : out Boolean) is
   begin Channels.Take (S, Index, Item, Got); end Take;
   procedure Served (Index : Channels.Index) is
   begin Channels.Served (S, Index); end Served;
   function Badge_Of (Index : Channels.Index) return W.Word is (Channels.Badge_Of (S, Index));
   function Input_Of (Index : Channels.Index) return RC.Payload_View is (Channels.Read_Payload (S, Index));
   function Output_Of (Index : Channels.Index) return RC.Payload_View is (Channels.Write_Payload (S, Index));
   procedure Complete
     (Index : Channels.Index; Tag : RC.Token; Answer : W.Completion_Words; Published : out Boolean) is
   begin
      Published := Channels.Is_Active (S, Index) and then Channels.Owed (S, Index) > 0;
      if not Published then return; end if;
      Channels.Complete (S, Index, Tag, Answer);
      Channels.Ring_Now (S, Index);
   end Complete;
   procedure Find_Ending
     (Event : CuBit.Control_Events.Event; Found : out Boolean; Index : out Channels.Index) is
   begin Channels.Find_Ending_Channel (S, Event, Found, Index); end Find_Ending;
   procedure Close_Ending
     (Event : CuBit.Control_Events.Event; Expected : Channels.Index; Closed : out Boolean) is
      Index : Channels.Index;
      Ended : Boolean;
   begin
      Channels.Handle_Event (S, Event, Ended, Index);
      Closed := Ended and then Index = Expected;
   end Close_Ending;
   package Loop_Turn is new I2C_Channel_Turn
     (Device, Channels.Index, Next_Channel, Take, Served, Badge_Of, Input_Of, Output_Of,
      Complete, Find_Ending, Close_Ending);
   use type Loop_Turn.Phase;
   function Stopped return Boolean is (Loop_Turn.Observe.At_Phase = Loop_Turn.Stopped);
   function Retained return Boolean is (Loop_Turn.Observe.At_Phase = Loop_Turn.Retained);
   function Admit (Offer : Positive; From : CuBit.Messages.Process_ID; Badge : Interfaces.Unsigned_64)
      return Boolean is
      Channel : constant B.Decoding := B.Decode (Badge);
   begin
      return Started and then not Loop_Turn.Observe.Stop_Requested and then not Retained
        and then Offer = 1 and then CuBit.Process_IDs.Is_Process (From)
        and then Channel.Status = B.Decoded and then Channel.Value.Mode = B.Seven_Bit
        and then Channel.Value.Maximum_Hz <= B.Hertz (Intel_LPSS_I2C_Profile.Fast_Plus_Maximum);
   end Admit;
   function Can_Serve (Offer : Positive) return Boolean is
     (Offer = 1 and then Started and then not Retained and then not Stopped
      and then not Loop_Turn.Observe.Stop_Requested);
   procedure Configure
     (Per_Client, Total : CuBit.Channel_Limits.Channel_Limit;
      Service : CuBit.Channel_Limits.Channel_Class; Ready : out Boolean) is
   begin
      Ready := False;
      if Attempted then return; end if;
      Attempted := True;
      if Device.Observe.At_Phase /= Device.Controller.Available then return; end if;
      Channels.Configure (S, [1 => (I2C_Channel.Element, Per_Client, Total, Service)], Started);
      Ready := Started;
   end Configure;
   procedure Handle_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Reply : out CuBit.Messages.Message; Opened : out Boolean) is
      Index : Channels.Index;
   begin
      Is_Open := False; Opened := False; Reply := CuBit.Messages.NULL_MESSAGE;
      if not Started then return; end if;
      Channels.Handle_Open (S, From, Request, Is_Open, Reply, Opened, Index);
   end Handle_Open;
   procedure Turn is
   begin
      if not Started then return; end if;
      Channels.Refresh (S);
      Loop_Turn.Turn;
   end Turn;
   procedure On_Event
     (Publisher : CuBit.Messages.Process_ID; Event : CuBit.Control_Events.Event) is
   begin if Started then Loop_Turn.On_Event (Publisher, Event); end if; end On_Event;
   procedure Sleep (Deadline : CuBit.Deadlines.Deadline; Woke : out Channels.Wake) is
   begin
      Woke := (others => <>);
      if Started then Channels.Sleep (S, Deadline, Woke); end if;
   end Sleep;
end I2C_Native_Channel_Service;
