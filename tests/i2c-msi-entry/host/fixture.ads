with Interfaces; with Launch_Configuration;
package Fixture is
   Done : exception;
   Scenario : Positive := 1;
   Turns, Binds, Prepares, Arms, Rejects, Waits, Poll_Sleeps, Health_Sleeps,
     Idle_Sleeps, Open_Calls, Reply_Calls, Event_Calls, Ready_Calls : Natural := 0;
   Polled : Natural := 0;
   Now : Interfaces.Unsigned_64 := 1;
   Masked, Payload, Stop_Pending, Stopped, Retained : Boolean := False;
   Supplied, Peer_Event_Sent, Clock_OK, Prepare_OK, Arm_OK : Boolean := True;
   Deadline_Seen, Reply_Label : Interfaces.Unsigned_64 := 0;
   Ready_Label : Interfaces.Unsigned_32 := 0;
   Delivery : Launch_Configuration.Interrupt_Delivery := Launch_Configuration.MSI;
   Vectors : Launch_Configuration.Vector_Count := 1;
   procedure Reset (Kind : Positive);
end Fixture;
