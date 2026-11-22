pragma Ada_2022;
with CuBit.Handles;
with Native_Community_Fixture;
with GPIO_Channel_Owner;
with GPIO_Native_Transport;
package GPIO_Native_Channel_Fixture is
   -- Semantic-only composition with the actual private runtime handle types.
   -- This fixture is not a native launcher or a claim that IRQ delivery works.
   package Channels is new GPIO_Channel_Owner
     (Native_Community_Fixture.Leases, 32, CuBit.Handles.Reply,
      GPIO_Native_Transport.Event_Sink, GPIO_Native_Transport.Answer,
      GPIO_Native_Transport.Try_Event);
end GPIO_Native_Channel_Fixture;
