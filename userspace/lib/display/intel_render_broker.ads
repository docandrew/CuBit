with Interfaces; use Interfaces;
with CuBit.Messages;
with Intel_GPU_Broker_Launches;
with Intel_Render_Admission_Dispatch;

-- Single-owner service-loop adapter. The caller reserves these reply slots,
-- the GPU source and application source slots for this object's lifetime.
-- Begin_Request must run before receiving another message (implicit reply).
generic
   Driver_Source : CuBit.Messages.CapabilitySlot;
   First_Token, Last_Token : Unsigned_64;
   with function Reply_Slot (ID : Positive)
     return CuBit.Messages.CapabilitySlot;
package Intel_Render_Broker is
   subtype Ticket is Intel_GPU_Broker_Launches.Ticket;
   type Broker is limited private;
   procedure Begin_Request
     (Object : in out Broker; Expected_Launcher, Sender, Stamped_Tag : Unsigned_64;
      Request : CuBit.Messages.Message; Now, Deadline : Unsigned_64;
      ID : out Ticket);
   -- ID=0: no reply was saved and caller retains immediate rejection duty.
   -- ID/=0: adapter owns the one-shot reply, including admission rejection.
   procedure Step (Object : in out Broker; Now : Unsigned_64);
   procedure Complete
     (Object : in out Broker; Receipt : CuBit.Messages.CompletionEntry;
      Now : Unsigned_64; Consumed : out Boolean);
   procedure Cancel (Object : in out Broker; ID : Ticket);
   function Runnable (Object : Broker) return Boolean;
   function Next_Deadline (Object : Broker) return Unsigned_64;
   function State (Object : Broker; ID : Ticket)
     return Intel_GPU_Broker_Launches.Phase;
private
   package L renames Intel_GPU_Broker_Launches;
   package D is new Intel_Render_Admission_Dispatch
     (L.Capacity, First_Token, Last_Token);
   type Admission_Array is array (Positive range 1 .. L.Capacity) of D.Ticket;
   type Slot_Array is array (Positive range 1 .. L.Capacity)
     of CuBit.Messages.CapabilitySlot;
   type Broker is limited record
      Launches : L.Ledger;
      Admissions : D.Dispatcher;
      IDs : Admission_Array := [others => 0];
      Replies : Slot_Array := [others => 0];
      Cursor : Positive range 1 .. L.Capacity := 1;
   end record;
end Intel_Render_Broker;
