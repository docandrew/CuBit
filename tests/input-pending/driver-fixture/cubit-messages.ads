with Interfaces; use Interfaces;
package CuBit.Messages is
   type MessageTag is record
      label : Unsigned_32 := 0;
      length, flags, reserved : Unsigned_32 := 0;
   end record;
   type MessageWords is array (0 .. 3) of Unsigned_64;
   type Message is record
      tag : MessageTag;
      authorityTag : Unsigned_64 := 0;
      words : MessageWords := (others => 0);
   end record;
   subtype Process_ID is Unsigned_64;
   No_Process : constant Process_ID := 0;
   Wait_Forever : constant Unsigned_64 := Unsigned_64'Last;
   SYSINFO_REGISTERED_DRIVER : constant Unsigned_64 := 1;
   DRIVER_KEYBOARD : constant Unsigned_64 := 2;
   DRIVER_MOUSE : constant Unsigned_64 := 3;
   SYSCALL_SLEEP : constant Unsigned_64 := 4;
   SYSCALL_GETTIME : constant Unsigned_64 := 5;
   SYSCALL_EXIT : constant Unsigned_64 := 6;
   type Activity_Result is (Work_Available, Deadline_Reached, Unavailable);
   Finished : exception;
   procedure Verify;
   procedure debugPrint (Text : String);
   function syscall (Number : Unsigned_64; Arg : Unsigned_64 := 0) return Unsigned_64;
   function getInfo (Number, Arg : Unsigned_64) return Unsigned_64;
   function portOutp8 (Port : Unsigned_16; Value : Unsigned_8) return Unsigned_64;
   function portInp8 (Port : Unsigned_16) return Unsigned_64;
   function capSend (Slot : Unsigned_64; Msg : Message; Deadline : Unsigned_64) return MessageTag;
   function Registered_Driver (Driver : Unsigned_64) return Process_ID;
   function trySendEvent (Dest : Process_ID; Msg : Message) return Boolean;
   function Wait_Event return Message;
   function Poll_Event (Msg : out Message) return Boolean;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result;
end CuBit.Messages;
