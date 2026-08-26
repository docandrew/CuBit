with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Protocols; use CuBit.Protocols;

procedure Main is
   use ASCII;

   REPLY_OK     : constant Unsigned_32 := 16#F000#;
   REPLY_ERROR  : constant Unsigned_32 := 16#F001#;

   From   : ProcessID;
   Request : Message;
   Response : Message;
   Ignore : Unsigned_64;
begin
   debugPrint ("ccl-test-host: starting" & LF);
   Ignore := registerDriver (DRIVER_CCL_TEST);
   if Ignore = Unsigned_64'Last then
      debugPrint ("ccl-test-host: registration failed" & LF);
      return;
   end if;
   debugPrint ("ccl-test-host: registered" & LF);

   loop
      receive (From, Request);
      Response := NULL_MESSAGE;
      if Request.tag.label = CCL_TEST_OP_INCREMENT and then
        Request.tag.length >= 1
      then
         debugPrint ("ccl-test-host: import invoked" & LF);
         Response.tag :=
           (label => REPLY_OK, length => 1, flags => 0, badge => 0);
         Response.words (0) := Request.words (0) + 1;
      else
         Response.tag :=
           (label => REPLY_ERROR, length => 0, flags => 0, badge => 0);
      end if;
      Ignore := reply (From, Response);
   end loop;
end Main;
