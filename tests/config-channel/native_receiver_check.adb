with Interfaces;
with CCL.Objects.Persistence;
with Config_Database;
with Config_Database.Schemas;
with Config_Schema_Protocol;
with Config_Worker_Protocol;
with Config_Worker_Storage;
with Config_Worker_Receiver;

procedure Native_Receiver_Check
  (Database : System.Address; Owner_Endpoint : CuBit.Messages.CapabilitySlot;
   Expected_Source, Sender : CuBit.Messages.Process_ID;
   Request : CuBit.Messages.Message;
   Reply : out CuBit.Messages.Message)
is
   use type Interfaces.Unsigned_64;
   function Authorized (Source : CuBit.Messages.Process_ID; Tag : Interfaces.Unsigned_64)
      return Boolean is (Source = Expected_Source and Tag = 77);
   procedure Invoke
     (Action : Config_Worker_Protocol.Operation; Name, Context : String;
      Expected_Revision : Config_Worker_Protocol.Number;
      Schema : CCL.Objects.Schema_Key; Input : CCL.Objects.Persistence.Packet;
      Output : out Config_Worker_Storage.Reply) is
   begin
      Config_Database.Invoke (Database, Action, Name, Context, Expected_Revision,
                              Schema, Input, Output);
   end Invoke;
   procedure Invoke_Type
     (Action : Config_Schema_Protocol.Operation; Name, Context : String;
      Contract : CCL.Objects.Binding; Recovered : out CCL.Objects.Binding;
      Result : out Config_Schema_Protocol.Reply_Kind) is
   begin
      Config_Database.Schemas.Invoke (Database, Action, Name, Context, Contract, Recovered, Result);
   end Invoke_Type;
   package Receiver is new Config_Worker_Receiver (Owner_Endpoint, Authorized, Invoke, Invoke_Type);
   Worker : Receiver.State;
begin
   Receiver.Handle (Worker, Sender, Request, Reply);
end Native_Receiver_Check;
