package body Worker_Proof with SPARK_Mode is
   procedure Invoke
     (Action : Config_Worker_Protocol.Operation;
      Name, Context : String; Expected_Revision : Config_Worker_Protocol.Number;
      Schema : CCL.Objects.Schema_Key; Input : CCL.Objects.Persistence.Packet;
      Output : out Config_Worker_Storage.Reply)
   is
      pragma Unreferenced (Action, Name, Context, Expected_Revision, Schema, Input);
   begin
      Output := Backend;
   end Invoke;
end Worker_Proof;
