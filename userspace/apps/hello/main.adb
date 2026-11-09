pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Log_Records;
with CuBit.Logging;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Streams;
with Startup;

--  The device-build test's new app (tests/device-builds): its sources and
--  its manifest are built on CuBit, every step there. It says hello once
--  on its greeting outlet (the console's card when the console started it,
--  tests/console-run-staged), in the log and on the kernel console, then
--  exits 0: a CCL call, (hello.run (Hello_Parameters)), as much as an app.
procedure Main is
   use type CuBit.Streams.StreamId;

   Greeting : constant String := "hello: hello from an app built on CuBit";

   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Flush_Wait_Ms : constant := 2_000;
   Exit_Success : constant := 0;

   procedure Say (Text : String) is
      Made : constant CuBit.Log_Records.Decoded := CuBit.Log_Records.Make (Text);
      Accepted, Drained : Boolean := False;
   begin
      debugPrint (Text & ASCII.LF);
      if Made.Success then
         CuBit.Logging.Emit (Writer, Made.Value, Accepted);
         CuBit.Logging.Flush (Writer, Drained, Wait_Ms => Flush_Wait_Ms);
      end if;
   end Say;

   --  The outlet its manifest declares: the ring its launcher lent it, or
   --  one of its own when it was started without one (app replace).
   Outlet : constant CuBit.Streams.StreamId := CuBit.Streams.Open_Outlet ("com.cubit.hello.greeting");
   Ignore : Unsigned_64;
begin
   if Outlet /= CuBit.Streams.NO_STREAM then
      CuBit.Streams.streamPrint (Outlet, Greeting & ASCII.LF);
   end if;
   Say (Greeting);
   Ignore := syscall (SYSCALL_EXIT, Exit_Success);
end Main;
