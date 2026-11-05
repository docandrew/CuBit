pragma Ada_2022;
package body ACPI_Native_Data is
   package body Session is
      procedure Initialize (Destination : CuBit.Handles.Endpoint; Symbol : A.Endpoint_ID;
         Deadline : W.Time_Instant; Accepted : in out Boolean) is
         pragma Unreferenced (Destination, Symbol, Deadline, Accepted);
      begin raise Program_Error with "disabled firmware-data Initialize called"; end Initialize;
      procedure Inspect (Is_Ready, Is_Pending : in out Boolean;
         Observation : in out A.Observation; Capacity : in out ACPI_Capacity_Diagnostics.Report) is
         pragma Unreferenced (Is_Ready, Is_Pending, Observation, Capacity);
      begin raise Program_Error with "disabled firmware-data Inspect called"; end Inspect;
      procedure Describe (Target : A.Target; Geometry : in out L.Register_Geometry; Accepted : in out Boolean) is
         pragma Unreferenced (Target, Geometry, Accepted);
      begin raise Program_Error with "disabled firmware-data Describe called"; end Describe;
      procedure Authorize (Target : A.Target; For_Write : Boolean; Accepted : in out Boolean) is
         pragma Unreferenced (Target, For_Write, Accepted);
      begin raise Program_Error with "disabled firmware-data Authorize called"; end Authorize;
      procedure Dispatch (Item : A.Request; Reply : in out A.Response) is
         pragma Unreferenced (Item, Reply);
      begin raise Program_Error with "disabled firmware-data Dispatch called"; end Dispatch;
      procedure Resolve (Item : ACPI_Service.Namespace.Operation_Region_Attributes;
         Geometry : L.Register_Geometry; Target : in out A.Target; Accepted : in out Boolean) is
         pragma Unreferenced (Item, Geometry, Target, Accepted);
      begin raise Program_Error with "disabled firmware-data Resolve called"; end Resolve;
      procedure Begin_Field (Requires_Lock, For_Write : Boolean; Deadline : W.Time_Instant;
         Ticket : ACPI_Global_Lock_Protocol.Ticket; Accepted : in out Boolean) is
         pragma Unreferenced (Requires_Lock, For_Write, Deadline, Ticket, Accepted);
      begin raise Program_Error with "disabled firmware-data Begin called"; end Begin_Field;
      procedure End_Field (Accepted : in out Boolean) is
         pragma Unreferenced (Accepted);
      begin raise Program_Error with "disabled firmware-data End called"; end End_Field;
      procedure Cleanup (Accepted : in out Boolean) is
         pragma Unreferenced (Accepted);
      begin raise Program_Error with "disabled firmware-data Cleanup called"; end Cleanup;
   end Session;
end ACPI_Native_Data;
