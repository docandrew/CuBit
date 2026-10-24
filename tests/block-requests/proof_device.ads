with Interfaces; use Interfaces;
with System;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with CuBit.Block_Device_Server;

--  A SPARK instance of the server: proves the generic body against its
--  formals' contracts (any driver gets only valid transfers and advertised
--  flushes). The actuals record the last call for the hosted tests.
package Proof_Device with SPARK_Mode is
   Last_Item : Transfer_Request;
   Transfers, Flush_Calls : Natural := 0;
   Short_By : Unsigned_64 := 0;
   Flush_Result : Boolean := True;

   procedure Transfer
     (State : Device_State; Item : Transfer_Request;
      Buffer : System.Address; Moved : out Unsigned_64)
     with Pre => Valid_Transfer (State, Item),
          Global => (Output => Last_Item, In_Out => Transfers, Input => Short_By);
   procedure Flush (State : Device_State; Durable : out Boolean)
     with Pre => Flushes (State),
          Global => (In_Out => Flush_Calls, Input => Flush_Result);

   package Server is new CuBit.Block_Device_Server (Transfer, Flush);
end Proof_Device;
