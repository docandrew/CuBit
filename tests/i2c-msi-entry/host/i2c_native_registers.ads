with CuBit.Handles; with I2C_Mapping_Policy;
with Intel_LPSS_I2C_Reset; with DW_I2C_Configuration; with DW_I2C_Registers;
package I2C_Native_Registers is
   package Window is
      type Outcome is (Bound, Refused);
      type Report is record Status : Outcome := Refused; end record;
      procedure Bind (Authority : CuBit.Handles.Device_Memory; Physical, Bytes, Virtual : I2C_Mapping_Policy.Address; Result : out Report);
      function Ready return Boolean;
      procedure Reset_Read (ID : Intel_LPSS_I2C_Reset.Register_ID; Value : out Intel_LPSS_I2C_Reset.Word; Status : out Intel_LPSS_I2C_Reset.Access_Status);
      procedure Reset_Write (ID : Intel_LPSS_I2C_Reset.Register_ID; Value : Intel_LPSS_I2C_Reset.Word; Status : out Intel_LPSS_I2C_Reset.Access_Status);
      procedure Config_Read (ID : DW_I2C_Configuration.Register_ID; Value : out DW_I2C_Configuration.Word; Status : out DW_I2C_Configuration.Access_Status);
      procedure Config_Write (ID : DW_I2C_Configuration.Register_ID; Value : DW_I2C_Configuration.Word; Status : out DW_I2C_Configuration.Access_Status);
      procedure Data_Read (ID : DW_I2C_Registers.Read_Register; Value : out DW_I2C_Registers.Word; Status : out DW_I2C_Registers.Access_Status);
      procedure Data_Write (ID : DW_I2C_Registers.Write_Register; Value : DW_I2C_Registers.Word; Status : out DW_I2C_Registers.Access_Status);
   end Window;
end I2C_Native_Registers;
