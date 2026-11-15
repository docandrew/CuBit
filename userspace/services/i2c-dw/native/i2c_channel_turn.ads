pragma Ada_2022;
with I2C_Channel_Controller; with I2C_Channel;
with I2C_Transfer_Wire; with CuBit.Control_Events; with CuBit.Process_IDs;
generic
   with package Device is new I2C_Channel_Controller (<>);
   type Channel_Index is range <>;
   with procedure Next_Channel (Found : out Boolean; Index : out Channel_Index);
   with procedure Take (Index : Channel_Index; Item : out I2C_Channel.RC.Queues.Submission; Got : out Boolean);
   with procedure Served (Index : Channel_Index);
   with function Badge_Of (Index : Channel_Index) return I2C_Transfer_Wire.Word;
   with function Input_Of (Index : Channel_Index) return I2C_Channel.RC.Payload_View;
   with function Output_Of (Index : Channel_Index) return I2C_Channel.RC.Payload_View;
   with procedure Complete
     (Index : Channel_Index; Tag : I2C_Channel.RC.Token;
      Answer : I2C_Transfer_Wire.Completion_Words; Published : out Boolean);
   with procedure Find_Ending
     (Event : CuBit.Control_Events.Event; Found : out Boolean; Index : out Channel_Index);
   with procedure Close_Ending
     (Event : CuBit.Control_Events.Event; Expected : Channel_Index; Closed : out Boolean);
package I2C_Channel_Turn is
   type Phase is (Idle, Running, Stopped, Retained);
   type Report is record
      At_Phase : Phase := Idle;
      Stop_Requested, Transfer_Pending, End_Pending : Boolean := False;
      Submitted, Completed, Rejected, Ignored_Events : I2C_Transfer_Wire.Word := 0;
   end record;
   function Observe return Report;
   procedure Turn;
   procedure On_Event
     (Publisher : CuBit.Process_IDs.Process_ID; Event : CuBit.Control_Events.Event);
   -- Publisher is syscall metadata, not an event word. Peer events cannot
   -- close mappings or request Stop. Kernel grant events for active work are
   -- copied and deferred until the actual owner permits payload release.
   -- Call Turn after startup reaches Available. The surrounding native pump
   -- refreshes/arms the server and handles channel-open replies. All callbacks
   -- are bounded/non-raising, serialized on that same event loop. Complete
   -- publishes the answer and rings its client; no callback waits on a peer.
end I2C_Channel_Turn;
