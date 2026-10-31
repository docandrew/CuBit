pragma Ada_2022;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with HID_I2C_Descriptors;
with I2C_Transfer_Wire;
generic
   with procedure Read_Input (Caller : CuBit.Process_IDs.Process_ID;
      Reference : CuBit.Grant_References.Reference; Data : out HID_I2C_Descriptors.Bytes;
      Copied : out Boolean);
   with procedure Write_Output (Caller : CuBit.Process_IDs.Process_ID;
      Reference : CuBit.Grant_References.Reference; Data : HID_I2C_Descriptors.Bytes;
      Copied : out Boolean);
package I2C_HID_Server_Model is
   package W renames I2C_Transfer_Wire;
   procedure Initialize;
   procedure Submit (Item : W.Request; Token : W.Word; Accepted : out Boolean);
   procedure Serve (Now : W.Word; Input_Data : HID_I2C_Descriptors.Bytes;
      Token : out W.Word; Completion : out W.Completion);
   procedure Inject_Transfer_Abort;
   function Faulted return Boolean;
   function Controller_May_Be_Enabled return Boolean;
   function Register_Accesses return Natural;
   function Checks return Natural;
   function Transactions return Natural;
   -- Hosted queued transport/MMIO/FIFO fixture around the real I2C request,
   -- owner and transfer engines. Input_Data describes device FIFO bytes, not
   -- a canned completion. Copy callbacks remain modeled grant acquisitions.
end I2C_HID_Server_Model;
