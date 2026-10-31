pragma Ada_2022;
package body ACPI_Log_Lifetime with SPARK_Mode is
   use type CuBit.Process_IDs.Process_ID;
   function Related (Logstore : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Control_Events.Event) return Boolean is
     (CuBit.Process_IDs.Is_Process (Logstore) and then
        Item.Kind in CuBit.Control_Events.Grant_Revoked | CuBit.Control_Events.Grant_Returned
        and then Item.Peer = Logstore);
end ACPI_Log_Lifetime;
