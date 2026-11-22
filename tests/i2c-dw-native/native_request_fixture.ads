with Interfaces;
with CuBit.Process_IDs;
with CuBit.Handles;
with DW_I2C_Request_Types;
with I2C_Native_Controller;
with I2C_Native_Requests;
with I2C_Native_Loop;
with I2C_Configure_Replies;
package Native_Request_Fixture is
   package Device is new I2C_Native_Controller (Capacity => 64);
   function Authorized (Caller : CuBit.Process_IDs.Process_ID;
      Badge : Interfaces.Unsigned_64) return Boolean;
   procedure Answer (Reply : CuBit.Handles.Reply;
      Item : DW_I2C_Request_Types.Response; Delivered : out Boolean);
   package Service is new I2C_Native_Requests (Device.Controller, 64, Authorized, Answer);
   procedure Report_Startup
     (Item : I2C_Configure_Replies.Reply; Delivered : out Boolean);
   package Loop_Service is new I2C_Native_Loop
     (Device, Authorized, Service.Grants.Copy.Read_Input,
      Service.Grants.Copy.Write_Output, Answer, Report_Startup);
end Native_Request_Fixture;
