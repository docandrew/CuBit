with Interfaces; use Interfaces;
with System;
package CuBit.Messages is
   type Tag_Type is record
      label : Unsigned_32 := 0;
      length, flags : Unsigned_8 := 0;
      reserved : Unsigned_16 := 0;
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
   type Activity_Result is (Idle);
   COMPLETION_OK : constant Unsigned_64 := 0;
   SYSCALL_GETTIME : constant := 1;
   Response : Message;
   Bad_Token, Bad_Status, Submit_Fails, Pending, Retry_First : Boolean := False;
   Submissions, Polls : Natural := 0;
   Now, Step : Unsigned_64 := 0;
   Last_Token : Unsigned_64 := 0;
   function syscall (Op : Natural) return Unsigned_64;
   function capSubmit (Slot : Natural; Msg : Message; Token : Unsigned_64) return Boolean;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result;
   function Poll (Result : System.Address) return Unsigned_64;
end CuBit.Messages;
