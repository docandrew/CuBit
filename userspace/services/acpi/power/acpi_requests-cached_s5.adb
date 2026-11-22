pragma Ada_2022;
package body ACPI_Requests.Cached_S5 with SPARK_Mode is
   function Current_Types (Server : State; Item : Snapshot) return Cache.Copied_Types is
     (Cache.Current_Types (Server.Boot, Item));
   procedure Prepare (Server : in out State; Item : in out Snapshot;
      Registers : Cache.Objects.Register_Model; Budget : Natural;
      Result : out Cache.Objects.Query_Report) is
   begin Cache.Prepare (Server.Boot, Item, Registers, Budget, Result); end Prepare;
end ACPI_Requests.Cached_S5;
