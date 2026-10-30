with I2C_Native_Results;
with I2C_Transfer_Wire;
package body Native_Request_Fixture is
   function Authorized (Caller : CuBit.Process_IDs.Process_ID;
      Badge : Interfaces.Unsigned_64) return Boolean is
      pragma Unreferenced (Caller, Badge);
   begin return False; end Authorized;
   procedure Answer (Reply : CuBit.Handles.Reply;
      Item : DW_I2C_Request_Types.Response; Delivered : out Boolean) is
      Encoded : constant I2C_Transfer_Wire.Completion_Words :=
        I2C_Transfer_Wire.Encode_Completion (I2C_Native_Results.To_Wire (Item));
      pragma Unreferenced (Reply, Encoded);
   begin Delivered := False; end Answer;
   procedure Report_Startup
     (Item : I2C_Configure_Replies.Reply; Delivered : out Boolean) is
      pragma Unreferenced (Item);
   begin Delivered := False; end Report_Startup;
end Native_Request_Fixture;
