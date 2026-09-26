with CCL.Objects;
with Config_Schema_Protocol;

-- Private in-process adapter. Same exclusive Database lifetime contract as
-- the parent. Namespace authority has already been checked by Config; neither
-- this adapter nor stored metadata grants authority or installs live bindings.
package Config_Database.Schemas is
   type Creation is (Created, Already_Exists, Definition_Conflict, Rejected, Uncertain);
   type Recovery is (Loaded, Absent, Load_Failed);
   procedure Create
     (Database : System.Address; Name, Context : String;
      Contract : CCL.Objects.Binding; Result : out Creation);
   procedure Recover
     (Database : System.Address; Name, Context : String;
      Contract : out CCL.Objects.Binding; Result : out Recovery);
   procedure Invoke
     (Database : System.Address; Action : Config_Schema_Protocol.Operation;
      Name, Context : String; Contract : CCL.Objects.Binding;
      Recovered : out CCL.Objects.Binding; Result : out Config_Schema_Protocol.Reply_Kind);
   -- Uncertain/Load_Failed requires abandoning this worker session, including
   -- semantic validation failures discovered only by Ada after the Rust call.
end Config_Database.Schemas;
