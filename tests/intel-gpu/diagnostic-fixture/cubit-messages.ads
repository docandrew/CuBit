with Interfaces; use Interfaces;
with System;
package CuBit.Messages is
   type Tag_Type is record
      label, length, flags, reserved : Unsigned_64 := 0;
   end record;
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
   SYSCALL_GETTIME, SYSINFO_REGISTERED_DRIVER, DRIVER_LOGSTORE : constant := 1;
   Now : Unsigned_64 := 0;
   Submissions : Natural := 0;
   procedure debugPrint (Text : String);
   function syscall (Op : Natural) return Unsigned_64;
   function getInfo (Op, Arg : Natural) return Unsigned_64;
   function capSubmit (Slot : Natural; Msg : Message; Token : Unsigned_64) return Boolean;
   procedure Inject (Value : CompletionEntry);
   function Poll_Completion (Result : System.Address) return Unsigned_64;
end CuBit.Messages;
