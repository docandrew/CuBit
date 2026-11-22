with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Messages;
with Intel_GPU_Broker_Launches;
with Intel_Render_Admission_Dispatch;

-- Single-owner service-loop adapter for procmgr's render admissions
-- (CAP-002, PLAN §9 item 6). procmgr submits an admission request that
-- accepts a capability; the request's reply is saved here (a reply handle)
-- and answered once the driver admitted or refused the session: an admitted
-- one with a copy of Render_Source badged with the session tag (Read, Write
-- and Grant, so procmgr can install a copy into its child), the others with
-- no capability. Begin_Request must run before receiving another message
-- (it takes the current reply authority).
generic
   -- devmgr's endpoint for its own submissions to the driver.
   with function Driver return CuBit.Handles.Endpoint;
   -- The driver's identity (completions are matched on it).
   with function Driver_PID return Unsigned_64;
   -- devmgr's untagged endpoint to the driver, with Grant: what admitted
   -- applications get badged copies of.
   with function Render_Source return CuBit.Handles.Endpoint;
   First_Token, Last_Token : Unsigned_64;
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
   type Reply_Array is array (Positive range 1 .. L.Capacity)
     of CuBit.Handles.Reply;
   type Broker is limited record
      Launches : L.Ledger;
      Admissions : D.Dispatcher;
      IDs : Admission_Array := [others => 0];
      Replies : Reply_Array := [others => CuBit.Handles.No_Reply];
      Cursor : Positive range 1 .. L.Capacity := 1;
   end record;
end Intel_Render_Broker;
