with CCL.Objects;
with Config_Schema_Protocol;
with Config_Schema_Worker;

package Schema_Proof with SPARK_Mode is
   Backend_Kind : Config_Schema_Protocol.Reply_Kind;
   Backend_Contract : CCL.Objects.Binding;
   procedure Invoke
     (Action : Config_Schema_Protocol.Operation; Name, Context : String;
      Contract : CCL.Objects.Binding; Recovered : out CCL.Objects.Binding;
      Result : out Config_Schema_Protocol.Reply_Kind);
   package Worker is new Config_Schema_Worker (Invoke);
end Schema_Proof;
