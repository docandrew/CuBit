pragma Ada_2022;
package body CuBit.Messages is
 function reply (replyTo : Process_ID; msg : Message) return Unsigned_64 is
 pragma Unreferenced(replyTo);
 begin Last_Reply:=msg; Plain_Replies:=Plain_Replies+1; return 1; end;
end CuBit.Messages;
