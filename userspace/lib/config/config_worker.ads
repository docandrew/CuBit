with CCL.Objects.Persistence;
with Config_Worker_Protocol;
with Config_Worker_Storage;

--  The service shell owns authenticated receive/grant snapshots. This generic
--  executes one validated request against its exclusively owned database.
generic
   with procedure Invoke
     (Action : Config_Worker_Protocol.Operation;
      Name, Context : String; Expected_Revision : Config_Worker_Protocol.Number;
      Schema : CCL.Objects.Schema_Key; Input : CCL.Objects.Persistence.Packet;
      Output : out Config_Worker_Storage.Reply);
package Config_Worker with SPARK_Mode is
   type State is limited private;
   function Needs_Recovery (Worker : State) return Boolean;
   procedure Handle
     (Worker : in out State; Request : Config_Worker_Protocol.Frame;
      Contract : CCL.Objects.Binding; Response : out Config_Worker_Protocol.Frame;
      Accepted : out Boolean)
     with Post => (if Accepted then Config_Worker_Protocol.Valid_Reply (Response, Request, Contract));
   -- No reset: after uncertain storage/invalid backend output, dispose of the
   -- database connection and create a replacement worker/session. Repeating a
   -- request on this instance cannot issue another database operation.
private
   type State is limited record
      Recovery_Required : Boolean := False;
   end record;
   function Needs_Recovery (Worker : State) return Boolean is (Worker.Recovery_Required);
end Config_Worker;
