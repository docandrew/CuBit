pragma Ada_2022;
pragma SPARK_Mode (On);
with ACPI_Local_Shutdown;
with Shutdown_Model;
package Shutdown_Instance is new ACPI_Local_Shutdown
  (Shutdown_Model.Close_Consumers, Shutdown_Model.Close_Local,
   Shutdown_Model.Begin_Stop, Shutdown_Model.Maintain, Shutdown_Model.Cleanup,
   Shutdown_Model.May_Exit, Shutdown_Model.View);
