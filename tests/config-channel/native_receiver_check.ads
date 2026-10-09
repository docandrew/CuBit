with System;
with CuBit.Messages;

--  Compile-only fixture: instantiate the actual generic receiver against the
--  native syscall/runtime and Rust FFI adapter. Not a service entry or VM test.
procedure Native_Receiver_Check
  (Database : System.Address; Owner_Endpoint : CuBit.Messages.CapabilitySlot;
   Expected_Source, Sender : CuBit.Messages.Process_ID;
   Request : CuBit.Messages.Message;
   Reply : out CuBit.Messages.Message);
