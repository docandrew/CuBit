------------------------------------------------------------------------------
--  CuBit IPC benchmark server
--
--  Responds immediately to synchronous calls and async submitted requests.
--  Keep this intentionally boring: benchmark clients should measure kernel IPC
--  and scheduler behavior, not app-side business logic.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages; use CuBit.Messages;

procedure main is
   use ASCII;

   OP_BENCH_ECHO : constant Unsigned_32 := 16#0910#;
   OP_BENCH_DIE  : constant Unsigned_32 := 16#0911#;
   REPLY_OK      : constant Unsigned_32 := 16#F000#;
   XOR_MAGIC     : constant Unsigned_64 := 16#C0B1_7000_BE11#;

   from : ProcessID;
   msg  : Message;
   ret  : Unsigned_64;

   function makeReply (value : Unsigned_64) return Message is
      replyMsg : Message := NULL_MESSAGE;
   begin
      replyMsg.tag := (label  => REPLY_OK,
                       length => 3,
                       flags  => 0,
                       badge  => 0);
      replyMsg.words (0) := value;
      replyMsg.words (1) := value xor XOR_MAGIC;
      replyMsg.words (2) := Unsigned_64 (from);
      return replyMsg;
   end makeReply;

begin
   ret := registerDriver (DRIVER_IPCTEST);
   if ret = Unsigned_64'Last then
      debugPrint ("BENCH: FAIL ipc server-register" & LF);
      loop
         ret := syscall (SYSCALL_SLEEP, 1000);
      end loop;
   end if;

   debugPrint ("bench-ipc-server: registered" & LF);

   receive (from, msg);

   loop
      if msg.tag.label = OP_BENCH_ECHO then
         declare
            replyMsg : Message := makeReply (msg.words (0));
         begin
            replyWait (from, replyMsg, from, msg);
         end;
      elsif msg.tag.label = OP_BENCH_DIE then
         ret := syscall (SYSCALL_EXIT);
      else
         declare
            replyMsg : Message := NULL_MESSAGE;
         begin
            replyMsg.tag := (label  => 16#F001#,
                             length => 1,
                             flags  => 0,
                             badge  => 0);
            replyMsg.words (0) := Unsigned_64 (msg.tag.label);
            replyWait (from, replyMsg, from, msg);
         end;
      end if;
   end loop;
end main;
