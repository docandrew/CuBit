pragma Ada_2022;
with ACPI_Requests; with CuBit.Messages; with ACPI_One_Service.Power_Preparation;
package ACPI_Native_Power_Preparation is
   procedure Dispatch (Server : in out ACPI_Requests.State; Request : CuBit.Messages.Message;
      Runtime_Selected : Boolean; Reply : out CuBit.Messages.Message; Handled : out Boolean;
      Cached_Shutdown_Selected : Boolean := False);
   procedure Invalidate;
   procedure Retire;
   function Current_Types (Server : ACPI_Requests.State)
      return ACPI_One_Service.Power_Preparation.Controller.Copied_Types;
end ACPI_Native_Power_Preparation;
