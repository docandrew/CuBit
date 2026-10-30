pragma Ada_2022;
with I2C_Mapping_Policy;
with DW_I2C_Registers;
with DW_I2C_Configuration;
with Intel_LPSS_I2C_Reset;
generic
   type Authority_Type is private;
   with procedure Inspect
     (Authority : Authority_Type; Result : out I2C_Mapping_Policy.Description);
   with procedure Map_One_Page
     (Authority : Authority_Type; Physical, Virtual : I2C_Mapping_Policy.Address;
      Mapped : out Boolean);
package I2C_Register_Window with SPARK_Mode => Off is
   package P renames I2C_Mapping_Policy;
   package R renames DW_I2C_Registers;
   package C renames DW_I2C_Configuration;
   package L renames Intel_LPSS_I2C_Reset;
   type Outcome is (Bound, Already_Attempted, Policy_Refused, Map_Refused);
   type Report is record
      Status : Outcome := Policy_Refused;
      Admission : P.Outcome := P.No_Current_Authority;
   end record;
   procedure Bind
     (Authority : Authority_Type; Physical, BAR_Bytes, Virtual : P.Address;
      Result : out Report);
   function Ready return Boolean;
   -- One process-lifetime mapping attempt; no public byte-offset/address IO.
   -- Native actuals must inspect/map the same typed handle and preserve its
   -- lifetime, UC device mapping and exclusivity. This is not an IPC interface.
   procedure Reset_Read (ID : L.Register_ID; Value : out L.Word; Status : out L.Access_Status);
   procedure Reset_Write (ID : L.Register_ID; Value : L.Word; Status : out L.Access_Status);
   procedure Config_Read (ID : C.Register_ID; Value : out C.Word; Status : out C.Access_Status);
   procedure Config_Write (ID : C.Register_ID; Value : C.Word; Status : out C.Access_Status);
   procedure Data_Read (ID : R.Read_Register; Value : out R.Word; Status : out R.Access_Status);
   procedure Data_Write (ID : R.Write_Register; Value : R.Word; Status : out R.Access_Status);
end I2C_Register_Window;
