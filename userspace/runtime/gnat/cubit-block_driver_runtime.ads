------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The IPC and clock calls CuBit.Block_Driver makes. Hosted tests replace
--  this body with a fixture; the native body calls the kernel.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;

package CuBit.Block_Driver_Runtime is

   --  The next pending service request, without blocking.
   procedure Next_Request
     (Sender : out Process_ID; Request : out Message; Found : out Boolean);

   --  Blocks until a service request arrives.
   procedure Wait_Request (Sender : out Process_ID; Request : out Message);

   --  Saves the current request's reply capability, to answer it later.
   procedure Keep_Reply (Saved : out CuBit.Handles.Reply; Kept : out Boolean);

   --  Answers a kept request; the saved reply is used up either way.
   procedure Answer (Saved : CuBit.Handles.Reply; Response : Message);

   --  Answers the current request at once.
   procedure Respond (To : Process_ID; Response : Message);

   --  Microseconds on a monotonic clock.
   function Now return Unsigned_64;

   --  Less than Micros microseconds have passed since Since (a Now value).
   function Within (Since, Micros : Unsigned_64) return Boolean;
end CuBit.Block_Driver_Runtime;
