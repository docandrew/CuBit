with Interfaces; use Interfaces;
with System;

--  Block.Device.V1 server for an exclusively owned, bounded RAM image.
--  The bootstrap driver owns storage lifetime; callers only receive grants.
package Ram_Device is
   Block_Size : constant := 512;
   Maximum_Transfer_Blocks : constant := 1024;
   procedure Initialize
     (Base : System.Address; Bytes : Unsigned_64; Success : out Boolean);
   --  Blocks until a Block.Device.V1 request arrives and answers it
   --  (CuBit.Block_Driver: a synchronous device, so each request costs one
   --  receive and one reply).
   procedure Serve_One;
end Ram_Device;
