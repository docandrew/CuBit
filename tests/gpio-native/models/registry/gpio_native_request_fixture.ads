pragma Ada_2022;
with CuBit.Handles;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
with GPIO_Request_Dispatch;
with GPIO_Native_Request_IO;
with GPIO_Native_Dispatch;
with GPIO_Service_Loop;
with GPIO_Native_Transport;
with GPIO_Native_Channel_Fixture;
with GPIO_Native_Communities;
package GPIO_Native_Request_Fixture is
   -- Semantic-only composition. Native startup seals/parks the pin inventory;
   -- authenticated runtime admin then enrolls clients. No fixture grants authority.
   function Startup_Parked return Boolean is (True); -- semantic fixture only
   package Registry is new GPIO_Service_Loop
     (CuBit.Handles.Reply, GPIO_Native_Transport.Event_Sink,
      GPIO_Native_Channel_Fixture.Channels, 2, GPIO_Native_Transport.Close, Startup_Parked, GPIO_Native_Communities.Admit_Pin);
   function Authorized (Caller : CuBit.Process_IDs.Process_ID;
      Badge : GPIO_Control_Wire.Word) return Boolean;
   procedure Offer (Caller : CuBit.Process_IDs.Process_ID;
      Badge : GPIO_Control_Wire.Word; Item : GPIO_Control_Wire.Request;
      Reply : CuBit.Handles.Reply; Taken : out Boolean);
   package Dispatch is new GPIO_Request_Dispatch
     (CuBit.Handles.Reply, Authorized, GPIO_Native_Request_IO.Save,
      GPIO_Native_Request_IO.Answer_Saved, GPIO_Native_Request_IO.Answer_Current, Offer);
   package Native is new GPIO_Native_Dispatch (Dispatch);
end GPIO_Native_Request_Fixture;
