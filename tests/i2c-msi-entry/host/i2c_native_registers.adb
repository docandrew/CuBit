with Fixture;
package body I2C_Native_Registers is
   package body Window is
      procedure Bind (Authority : CuBit.Handles.Device_Memory; Physical, Bytes, Virtual : I2C_Mapping_Policy.Address; Result : out Report) is
         pragma Unreferenced (Authority, Physical, Bytes, Virtual);
      begin Fixture.Binds := Fixture.Binds + 1; Result := (Status => Bound); end Bind;
      function Ready return Boolean is (True);
      procedure Reset_Read (ID : Intel_LPSS_I2C_Reset.Register_ID; Value : out Intel_LPSS_I2C_Reset.Word; Status : out Intel_LPSS_I2C_Reset.Access_Status) is
      pragma Unreferenced (ID, Value, Status);
   begin raise Program_Error with "unexpected Reset_Read"; end Reset_Read;
      procedure Reset_Write (ID : Intel_LPSS_I2C_Reset.Register_ID; Value : Intel_LPSS_I2C_Reset.Word; Status : out Intel_LPSS_I2C_Reset.Access_Status) is
      pragma Unreferenced (ID, Value, Status);
   begin raise Program_Error with "unexpected Reset_Write"; end Reset_Write;
      procedure Config_Read (ID : DW_I2C_Configuration.Register_ID; Value : out DW_I2C_Configuration.Word; Status : out DW_I2C_Configuration.Access_Status) is
      pragma Unreferenced (ID, Value, Status);
   begin raise Program_Error with "unexpected Config_Read"; end Config_Read;
      procedure Config_Write (ID : DW_I2C_Configuration.Register_ID; Value : DW_I2C_Configuration.Word; Status : out DW_I2C_Configuration.Access_Status) is
      pragma Unreferenced (ID, Value, Status);
   begin raise Program_Error with "unexpected Config_Write"; end Config_Write;
      procedure Data_Read (ID : DW_I2C_Registers.Read_Register; Value : out DW_I2C_Registers.Word; Status : out DW_I2C_Registers.Access_Status) is
      pragma Unreferenced (ID, Value, Status);
   begin raise Program_Error with "unexpected Data_Read"; end Data_Read;
      procedure Data_Write (ID : DW_I2C_Registers.Write_Register; Value : DW_I2C_Registers.Word; Status : out DW_I2C_Registers.Access_Status) is
      pragma Unreferenced (ID, Value, Status);
   begin raise Program_Error with "unexpected Data_Write"; end Data_Write;
   end Window;
end I2C_Native_Registers;
