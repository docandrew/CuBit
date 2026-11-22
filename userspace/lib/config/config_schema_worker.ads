with CCL.Objects;
with Config_Schema_Protocol;

-- Pure executor. The adapter authenticates Config and snapshots/releases the
-- grant before calling this. Database callbacks own any durable I/O and CBOR.
generic
   with procedure Invoke
     (Action : Config_Schema_Protocol.Operation; Name, Context : String;
      Contract : CCL.Objects.Binding; Recovered : out CCL.Objects.Binding;
      Result : out Config_Schema_Protocol.Reply_Kind);
package Config_Schema_Worker with SPARK_Mode is
   type State is limited private;
   function Needs_Recovery (Object : State) return Boolean;
   procedure Handle
     (Object : in out State; Request : Config_Schema_Protocol.Frame;
      Response : out Config_Schema_Protocol.Frame; Accepted : out Boolean)
     with Post => (if Accepted then Config_Schema_Protocol.Valid_Reply (Response, Request));
private
   type State is limited record
      Failed : Boolean := False;
   end record;
   function Needs_Recovery (Object : State) return Boolean is (Object.Failed);
end Config_Schema_Worker;
