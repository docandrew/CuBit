with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Protocols; use CuBit.Protocols;

procedure Main with SPARK_Mode => On is
   use ASCII;

   REPLY_OK    : constant Unsigned_32 := 16#F000#;
   REPLY_ERROR : constant Unsigned_32 := 16#F001#;
   MAX_CCL_MILLISECONDS : constant Unsigned_64 :=
     Unsigned_64 (Integer_64'Last);

   From     : ProcessID;
   Request  : Message;
   Response : Message;
   Ignore   : Unsigned_64;
   Now_Ms   : Unsigned_64;
begin
   debugPrint ("clock: starting" & LF);
   Ignore := registerDriver (DRIVER_CLOCK);
   if Ignore = Unsigned_64'Last then
      debugPrint ("clock: registration failed" & LF);
      return;
   end if;
   debugPrint ("clock: registered" & LF);

   loop
      receive (From, Request);
      Response := NULL_MESSAGE;
      if Request.tag.label = CLOCK_OP_MONOTONIC_MS and then
        Request.tag.length = 1 and then Request.words (0) = 0
      then
         Now_Ms := syscall (SYSCALL_GETTIME);
         Response.tag :=
           (label => REPLY_OK, length => 1, flags => 0, badge => 0);
         Response.words (0) := Unsigned_64'Min
           (Now_Ms, MAX_CCL_MILLISECONDS);
         debugPrint ("clock: monotonic query" & LF);
      else
         Response.tag :=
           (label => REPLY_ERROR, length => 0, flags => 0, badge => 0);
      end if;
      Ignore := reply (From, Response);
   end loop;
end Main;
