with Interfaces; use Interfaces;
with System;
--  Hosted stand-in for CuBit.Messages: what Intel_GPU_Diagnostics uses,
--  with a scripted completion queue, clock and logstore registration.
package CuBit.Messages is
   type Protocol_Flags is mod 2 ** 4;
   type IPC_Flags is record
      Accepts_Capability, Capability_Transferred, Capability_Refused,
      Capability_Offered : Boolean := False;
   end record;
   No_IPC_Flags : constant IPC_Flags := (others => False);
   type Tag_Type is record
      label, length : Unsigned_64 := 0;
      protocol : Protocol_Flags := 0;
      ipc : IPC_Flags := No_IPC_Flags;
      reserved : Unsigned_64 := 0;
   end record;
   subtype MessageTag is Tag_Type;
   function Flag_Byte (Tag : Tag_Type) return Unsigned_8 is
     (Unsigned_8 (Tag.protocol)
      + (if Tag.ipc.Accepts_Capability then 16 else 0)
      + (if Tag.ipc.Capability_Transferred then 32 else 0)
      + (if Tag.ipc.Capability_Refused then 64 else 0)
      + (if Tag.ipc.Capability_Offered then 128 else 0));
   type Word_Array is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : Tag_Type;
      words : Word_Array := [others => 0];
   end record;
   NULL_MESSAGE : constant Message := (others => <>);
   type CompletionEntry is record
      token, status : Unsigned_64 := 0;
      msg : Message;
   end record;
   COMPLETION_OK : constant Unsigned_64 := 0;
   COMPLETION_FAILED : constant Unsigned_64 := 1;

   type Process_ID is new Unsigned_64;
   No_Process : constant Process_ID := 0;
   DRIVER_LOGSTORE : constant := 7;
   --  The scripted world.
   Now : Unsigned_64 := 0;
   Logstore : Process_ID := No_Process;
   Serial_Lines : Natural := 0;
   Last_Serial : String (1 .. 512) := [others => ' '];
   Last_Serial_Length : Natural := 0;
   function Serial_Text return String is (Last_Serial (1 .. Last_Serial_Length));

   procedure debugPrint (Text : String);
   function Registered_Driver (Driver : Natural) return Process_ID;
   procedure Inject (Value : CompletionEntry);
   function Queued return Natural;
   function Poll_Completion (Result : System.Address) return Unsigned_64;
end CuBit.Messages;
