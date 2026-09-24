with Interfaces; use Interfaces;
with System;
with CuBit.Messages;

--  Block.Device.V1 server for an exclusively owned, bounded RAM image.
--  The bootstrap driver owns storage lifetime; callers only receive grants.
package Ram_Device is
   Block_Size : constant := 512;
   Maximum_Transfer_Blocks : constant := 1024;
   procedure Initialize
     (Base : System.Address; Bytes : Unsigned_64; Success : out Boolean);
   procedure Handle
     (Sender : CuBit.Messages.ProcessID;
      Request : CuBit.Messages.Message;
      Response : out CuBit.Messages.Message);
end Ram_Device;
