pragma Ada_2022;
with Interfaces;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with CuBit.Handles;
with DW_I2C_Transfer;
with DW_I2C_Owner;
with DW_I2C_Requests;
with DW_I2C_Request_Types;
with I2C_Native_Grant_Copy;
generic
   with package Controller is new DW_I2C_Owner (<>);
   Capacity : DW_I2C_Transfer.Limit;
   with function Authorized
     (Caller : CuBit.Process_IDs.Process_ID; Badge : Interfaces.Unsigned_64) return Boolean;
   with procedure Answer
     (Reply : CuBit.Handles.Reply; Item : DW_I2C_Request_Types.Response; Delivered : out Boolean);
package I2C_Native_Requests is
   package Grants is new I2C_Native_Grant_Copy (Controller.Bytes);
   package Requests is new DW_I2C_Requests
     (Controller, Capacity, CuBit.Process_IDs.Process_ID, CuBit.Grant_References.Reference,
      CuBit.Handles.Reply, Authorized, Grants.Copy.Read_Input, Grants.Copy.Write_Output, Answer);
   -- Native loop obtains Reply from successful IPC.Save_Reply, and passes the
   -- kernel-stamped requester/authorityTag. Answer formats the agreed wire
   -- response then calls IPC.Answer once. Do not feed message words as identity.
   -- This composition deliberately supplies no permissive authentication or
   -- invented wire/catalog IDs; both native policy callbacks are required.
end I2C_Native_Requests;
