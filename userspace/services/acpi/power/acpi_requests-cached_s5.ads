pragma Ada_2022;
with ACPI_Bootstrap.Cached_S5;
package ACPI_Requests.Cached_S5 with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   package Cache renames ACPI_Bootstrap.Cached_S5;
   subtype Snapshot is Cache.Snapshot;
   procedure Invalidate (Item : in out Snapshot) renames Cache.Invalidate;
   procedure Retire (Item : in out Snapshot) renames Cache.Retire;
   function Current_Types (Server : State; Item : Snapshot) return Cache.Copied_Types
     with Pre => Valid (Server);
   procedure Prepare (Server : in out State; Item : in out Snapshot;
      Registers : Cache.Objects.Register_Model; Budget : Natural;
      Result : out Cache.Objects.Query_Report)
     with Pre => Valid (Server), Post => Valid (Server)
       and then Revision (Server) = Revision (Server)'Old and then Result.Charged <= Budget;
end ACPI_Requests.Cached_S5;
