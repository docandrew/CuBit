pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Log_Records;
with CuBit.Logging;
with Startup;

--  Native regression for LOG-003: a program that publishes a few records
--  and exits. Its records do not name it in their text, so once it has
--  gone only logstore's record of who it was (from procmgr, which started
--  it) can: (logs.recent "log-exit-probe") must still return them, and the
--  Boot Logs app must still show its name (CCL_CONSOLE_DEMO=log-exited).
procedure Main is
   package L renames CuBit.Log_Records;

   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);

   --  Agreed with tests/headless/run.sh (CCL_CONSOLE_DEMO=log-exited).
   Stages : constant := 3;
   Flush_Wait_Ms : constant := 2_000;
   Exit_Success : constant := 0;
   Exit_Failure : constant := 1;

   Submitted, Drained : Boolean := True;
   Ignore : Unsigned_64;

   function Image (Value : Natural) return String is
      Raw : constant String := Natural'Image (Value);
   begin
      return Raw (Raw'First + 1 .. Raw'Last);
   end Image;
begin
   for Stage in 1 .. Stages loop
      declare
         Made : constant L.Decoded :=
           L.Make ("exit probe stage " & Image (Stage) & " of " & Image (Stages));
         Accepted : Boolean := False;
      begin
         if Made.Success then
            CuBit.Logging.Emit (Writer, Made.Value, Accepted);
         end if;
         Submitted := Submitted and then Accepted;
      end;
   end loop;
   CuBit.Logging.Flush (Writer, Drained, Wait_Ms => Flush_Wait_Ms);
   if not (Submitted and then Drained) then
      debugPrint ("log-exit-probe: FAIL logstore did not take its records" & ASCII.LF);
      Ignore := syscall (SYSCALL_EXIT, Exit_Failure);
   end if;
   debugPrint ("log-exit-probe: published " & Image (Stages) & " records, exiting" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, Exit_Success);
end Main;
