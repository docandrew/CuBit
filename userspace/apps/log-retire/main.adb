with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Memory_Grants;

--  Fault-injection fixture only; never included in normal startup profiles.
--  Uses the test endpoint role, not the production logstore role. Deliberately
--  exits with an acquired page and an unanswered asynchronous publication.
procedure Main is
   package G renames CuBit.Memory_Grants;
   From : ProcessID;
   Request : Message;
   Ref : G.Grant_Reference;
   Address : System.Address;
   Acquired : Boolean;
   Ignore : Unsigned_64;
begin
   Ignore := registerDriver (DRIVER_IPCTEST);
   if Ignore = Unsigned_64'Last then
      debugPrint ("TEST: FAIL log-retire registration" & ASCII.LF);
      return;
   end if;
   receive (From, Request);
   if Request.tag.label /= CuBit.Log_Protocol.Operation'Enum_Rep
     (CuBit.Log_Protocol.Publish) or else Request.tag.length /= 4 or else
     Request.words (0) > G.MAXIMUM_GLOBAL_SLOT or else
     Request.words (1) not in 1 .. G.MAXIMUM_GENERATION or else
     Request.words (2) not in CuBit.Log_Records.Header_Bytes ..
       Unsigned_64 (CuBit.Log_Records.Wire_Count'Last)
   then
      debugPrint ("TEST: FAIL log-retire request" & ASCII.LF);
      return;
   end if;
   Ref := (Request.words (0), Request.words (1));
   G.Acquire (Ref, From, 0, Request.words (2), G.Read_Access,
              Address, Acquired);
   if not Acquired then
      debugPrint ("TEST: FAIL log-retire acquire" & ASCII.LF);
      return;
   end if;
   debugPrint ("TEST: log collector exiting with acquired grant" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT);
end Main;
