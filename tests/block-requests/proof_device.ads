with Interfaces; use Interfaces;
with System;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with CuBit.Block_Work; use CuBit.Block_Work;
with CuBit.Block_Sync_Driver;

--  A device behind the shared synchronous block driver (DEV-002). The
--  actuals assert what every driver may rely on (only valid transfers and
--  advertised flushes reach it) and record the last call for the tests.
package Proof_Device is
   --  The device state the driver serves (set by the tests' Handle).
   Current : Device_State := (Presence => Faulted);
   Last_Item : Transfer_Request;
   Transfers, Flush_Calls : Natural := 0;
   Short_By : Unsigned_64 := 0;
   Flush_Result : Boolean := True;

   procedure Transfer
     (Item : Transfer_Request; Buffer : System.Address; Moved : out Unsigned_64);
   procedure Flush (Durable : out Boolean);

   package Server is new CuBit.Block_Sync_Driver
     (Writes => Any_Client_Writes, Transfer => Transfer, Flush => Flush);
end Proof_Device;
