with Interfaces; use Interfaces;

package body Config_Database is
   use type System.Address;
   procedure Execute (Database, Input, Output : System.Address)
     with Import, Convention => C, External_Name => "cubit_config_database_execute";

   procedure Invoke
     (Context_Handle : System.Address;
      Action : Config_Worker_Protocol.Operation;
      Name, Context : String; Expected_Revision : Config_Worker_Protocol.Number;
      Schema : CCL.Objects.Schema_Key; Input : CCL.Objects.Persistence.Packet;
      Output : out Config_Worker_Storage.Reply)
   is
      Call : Request;
   begin
      Output := (others => <>);
      if Context_Handle = System.Null_Address or else
        not Config_Worker_Protocol.Valid_Name (Name) or else
        not Config_Worker_Protocol.Valid_Name (Context)
      then return; end if;
      Call.Action := Config_Worker_Protocol.Operation'Enum_Rep (Action);
      Call.Name_Length := Name'Length;
      Call.Context_Length := Context'Length;
      Call.Name (1 .. Name'Length) := Name;
      Call.Context (1 .. Context'Length) := Context;
      Call.Expected_Revision := Expected_Revision;
      Call.Schema := Schema;
      Call.Input_Length := Unsigned_64 (Input.Length);
      Call.Input := Input.Data'Address;
      Execute (Context_Handle, Call'Address, Output'Address);
   end Invoke;
end Config_Database;
