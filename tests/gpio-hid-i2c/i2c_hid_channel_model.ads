pragma Ada_2022;
with DW_I2C_Transfer; with HID_I2C_Descriptors; with I2C_Channel_Wire; with I2C_Transfer_Wire;
generic
   with procedure Read_Input (Index : DW_I2C_Transfer.Count; Value : out DW_I2C_Transfer.Byte; Accepted : out Boolean);
   with procedure Write_Output (Index : DW_I2C_Transfer.Count; Value : DW_I2C_Transfer.Byte; Accepted : out Boolean);
package I2C_HID_Channel_Model is
   package W renames I2C_Transfer_Wire;
   procedure Initialize;
   procedure Submit (Header : I2C_Channel_Wire.Raw_Request; Token : W.Word; Accepted : out Boolean);
   procedure Serve (Now : W.Word; Input_Data : HID_I2C_Descriptors.Bytes;
      Token : out W.Word; Completion : out W.Completion; Available : out Boolean);
   procedure Inject_Transfer_Abort;
   function Faulted return Boolean;
   function Controller_May_Be_Enabled return Boolean;
   function Register_Accesses return Natural;
   function Checks return Natural;
   function Transactions return Natural;
   -- Hosted queued metadata and byte views, actual borrowed owner/config/transfer,
   -- modeled MMIO/clock/FIFO. No grant identities or legacy request protocol.
end I2C_HID_Channel_Model;
