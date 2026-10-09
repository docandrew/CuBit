with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Grant_References;
with CuBit.Log_Publish_Rings;
with CuBit.Memory_Grants;

--  Fault-injection fixture only; never included in normal startup profiles.
--  Uses the test endpoint role, not the production logstore role. Deliberately
--  exits holding a publisher's channel region, its open unanswered.
procedure Main is
   package G renames CuBit.Memory_Grants;
   package CP renames CuBit.Channel_Protocol;
   From : Process_ID;
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
   if Request.tag.label /= CP.OP_OPEN_PRODUCING or else Request.tag.length /= CP.Open_Words
     or else not CuBit.Grant_References.Valid_Wire (Request.words (3))
   then
      debugPrint ("TEST: FAIL log-retire request" & ASCII.LF);
      return;
   end if;
   Ref := CuBit.Grant_References.Decode (Request.words (3));
   G.Acquire (Ref, From, 0,
              Unsigned_64 (CP.Region_Pages (CuBit.Log_Publish_Rings.CONTRACT))
                * CuBit.Channel_Contracts.Page_Bytes,
              G.Read_Access, Address, Acquired);
   if not Acquired then
      debugPrint ("TEST: FAIL log-retire acquire" & ASCII.LF);
      return;
   end if;
   debugPrint ("TEST: log collector exiting with acquired grant" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT);
end Main;
