pragma Ada_2022;
with CuBit.Handles;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
package GPIO_Native_Transport is
   type Event_Sink is private;
   No_Event_Sink : constant Event_Sink;
   type Resolution_Status is (Resolved, Endpoint_Refused, Wrong_Kind,
      Wrong_Sink_Rights, Invalid_Identity);
   type Resolution (Status : Resolution_Status := Endpoint_Refused) is record
      case Status is
         when Resolved =>
            Sink : Event_Sink;
            Owner : CuBit.Process_IDs.Process_ID;
            Badge : GPIO_Control_Wire.Word;
         when Endpoint_Refused => Why : CuBit.Handles.Refusal;
         when others => null;
      end case;
   end record;
   function Resolve (Target : CuBit.Handles.Endpoint) return Resolution;
   procedure Answer (Authority : CuBit.Handles.Reply;
      Value : GPIO_Control_Wire.Completion; Delivered : out Boolean)
      with Pre => GPIO_Control_Wire.Valid (Value);
   procedure Try_Event (Sink : Event_Sink;
      Value : GPIO_Control_Wire.Event; Accepted : out Boolean);
   procedure Close (Sink : Event_Sink; Closed : out Boolean);
   -- Resolve only a launcher-installed destination-limited endpoint. Destination
   -- and badge come from kernel Describe of that exact Write-only handle;
   -- there is no raw-PID sink
   -- constructor. Event_Sink is a typed local wrapper, not a new capability kind.
   -- Kernel checks current endpoint lifetime and Write on every event send.
   -- Answer consumes saved reply on either result; no retry or peer wait.
private
   type Event_Sink is record
      Authority : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Destination : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
   end record;
   No_Event_Sink : constant Event_Sink := (others => <>);
end GPIO_Native_Transport;
