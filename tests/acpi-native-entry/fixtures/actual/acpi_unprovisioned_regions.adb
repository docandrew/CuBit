pragma Ada_2022;
package body ACPI_Unprovisioned_Regions with SPARK_Mode is
   use type Core.Namespace.Object_Kind, Core.Values.Access_Status;
   procedure Initialize
     (Service : aliased in out Core.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced (Budget);
      Item : Core.Node_Descriptor;
      Status : Core.Values.Access_Status;
   begin
      Charged := 0; Outcome := ACPI_Device_Policy.Regions_Unavailable;
      Core.Describe_Node (Service, Core.Namespace.Root, Item, Status);
      if Status /= Core.Values.Available then return; end if;
      for Node in Core.Namespace.Node_ID range 1 .. Core.Namespace_Node_Count (Service) loop
         Core.Describe_Node (Service, Node, Item, Status);
         if Status = Core.Values.Available then
            if Core.Values.Node_Kind (Item) in Core.Namespace.Operation_Region_Object | Core.Namespace.Uninitialized_Region_Object then return; end if;
         elsif Status /= Core.Values.Invalid_Value then return;
         end if;
      end loop;
      Outcome := ACPI_Device_Policy.No_Regions_Required;
   end Initialize;
end ACPI_Unprovisioned_Regions;
