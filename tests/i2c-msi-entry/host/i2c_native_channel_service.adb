with Interfaces;
with Fixture;
package body I2C_Native_Channel_Service is
   use type CuBit.Control_Events.Event_Kind, CuBit.Process_IDs.Process_ID;
   use type Interfaces.Unsigned_64;
   procedure Configure (Per_Client, Total, Service : Positive; Ready : out Boolean) is
      pragma Unreferenced (Per_Client, Total, Service);
   begin Ready := True; end Configure;
   procedure Handle_Open (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Is_Open : out Boolean; Reply : out CuBit.Messages.Message; Opened : out Boolean) is
      pragma Unreferenced (From, Request);
   begin Fixture.Open_Calls := Fixture.Open_Calls + 1; Is_Open := False; Reply := CuBit.Messages.NULL_MESSAGE; Opened := False; end Handle_Open;
   procedure Turn is
   begin
      Fixture.Turns := Fixture.Turns + 1;
      if Fixture.Turns > 5 then raise Program_Error with "unbounded main turns"; end if;
      Device.Advance;
      if Fixture.Retained then return; end if;
      if Fixture.Stop_Pending or else (Fixture.Waits > 0) or else (Fixture.Poll_Sleeps > 0) then
         if not Fixture.Masked then raise Program_Error with "stop while armed"; end if;
         Fixture.Payload := False; Fixture.Stopped := True;
      elsif Fixture.Scenario /= 16 then Fixture.Payload := True;
      end if;
   end Turn;
   function Stopped return Boolean is (Fixture.Stopped);
   function Retained return Boolean is (Fixture.Retained);
   procedure On_Event (Publisher : CuBit.Messages.Process_ID; Event : CuBit.Control_Events.Event) is
   begin
      Fixture.Event_Calls := Fixture.Event_Calls + 1;
      if Publisher /= CuBit.Process_IDs.No_Process then raise Program_Error with "peer event forwarded"; end if;
      if Event.Kind = CuBit.Control_Events.Control then Fixture.Stop_Pending := True; end if;
   end On_Event;
   procedure Sleep (Deadline : CuBit.Deadlines.Deadline; Woke : out Channels.Wake) is

   begin
      if CuBit.Deadlines.Kernel_Word (Deadline) /= 5001 then raise Program_Error with "idle finite health deadline"; end if;
      Fixture.Idle_Sleeps := Fixture.Idle_Sleeps + 1;
      Woke := (Kind => Channels.Message_Received, From => CuBit.Process_IDs.No_Process, Msg => CuBit.Messages.NULL_MESSAGE);
      Woke.Msg.tag.label := CuBit.Control_Events.Control_Label; Woke.Msg.tag.length := CuBit.Control_Events.Control_Words; Woke.Msg.words (0) := CuBit.Control_Events.Control_Kind'Enum_Rep (CuBit.Control_Events.Stop);
   end Sleep;
end I2C_Native_Channel_Service;
