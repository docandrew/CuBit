package body Schema_Proof with SPARK_Mode is
   procedure Invoke
     (Action : Config_Schema_Protocol.Operation; Name, Context : String;
      Contract : CCL.Objects.Binding; Recovered : out CCL.Objects.Binding;
      Result : out Config_Schema_Protocol.Reply_Kind)
   is
      pragma Unreferenced (Action, Name, Context, Contract);
   begin
      Recovered := Backend_Contract;
      Result := Backend_Kind;
   end Invoke;
end Schema_Proof;
