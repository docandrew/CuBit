pragma Ada_2022;
with CuBit.Handles;
with DW_I2C_Transfer;
with DW_I2C_Owner;
with I2C_Native_Clock;
with I2C_Native_Registers;
with I2C_Mapping_Policy;
generic
   Capacity : DW_I2C_Transfer.Limit;
package I2C_Native_Controller is
   package Registers renames I2C_Native_Registers.Window;
   package Controller is new DW_I2C_Owner
     (Capacity, I2C_Native_Clock.Read,
      Registers.Reset_Read, Registers.Reset_Write,
      Registers.Config_Read, Registers.Config_Write,
      Registers.Data_Read, Registers.Data_Write);
   procedure Initialize
     (Authority : CuBit.Handles.Device_Memory;
      Facts : Controller.Startup; Virtual : I2C_Mapping_Policy.Address;
      State : in out Controller.State; Mapping : out Registers.Report;
      Admission : out Controller.Admission; Started : out Boolean);
   -- One initialization attempt on one actual controller. The caller supplies
   -- generated Startup.Registers and immutable validated launch facts, then
   -- schedules Controller.Advance. Started does NOT mean hardware startup is
   -- complete: inspect Controller.Observe until Available or Faulted.
   -- On refusal the caller stops; it never retries reset or mapping implicitly.
end I2C_Native_Controller;
