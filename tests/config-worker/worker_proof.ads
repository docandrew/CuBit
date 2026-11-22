with CCL.Objects.Persistence;
with Config_Worker_Protocol;
with Config_Worker_Storage;
with Config_Worker;

--  Prove the instantiated production executor against arbitrary backend
--  scalars/bytes. This is a concrete SPARK callback, not an assumed contract,
--  imported function, database model or proof of the Rust implementation.
package Worker_Proof with SPARK_Mode is
   Backend : Config_Worker_Storage.Reply;
   procedure Invoke
     (Action : Config_Worker_Protocol.Operation;
      Name, Context : String; Expected_Revision : Config_Worker_Protocol.Number;
      Schema : CCL.Objects.Schema_Key; Input : CCL.Objects.Persistence.Packet;
      Output : out Config_Worker_Storage.Reply);
   package Executor is new Config_Worker (Invoke);
end Worker_Proof;
