pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Logging;
with Startup;

--  The chatty source of the log-retention headless test (LOG-002): once
--  log-retention-check has published its boot records, write thousands of
--  records (paced inside logstore's publication budget, so they are kept,
--  not shed), then say it is done.
procedure Main is
   package P renames CuBit.Log_Protocol;
   package L renames CuBit.Log_Records;
   use type P.Status;

   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Reader : CuBit.Logging.Reader := CuBit.Logging.Bound_To (Startup.Log_Observer);

   --  Agreed with log-retention-check (userspace/apps/log-retention-check).
   Quiet_Done_Text : constant String := "log-retention: quiet boot done";
   Flood_Done_Text : constant String := "log-flood: done";

   Flood_Records : constant := 4_000;
   --  Records per burst, and the pause after each: 200 per 250 ms stays
   --  under logstore's 1000-a-second refill (Log_Budgets).
   Burst_Records : constant := 200;
   Burst_Pause_Ms : constant := 250;
   Poll_Ms : constant := 20;
   Wait_Limit_Ms : constant Unsigned_64 := 30_000;
   Flush_Wait_Ms : constant := 2_000;

   Ignore : Unsigned_64;
   Result : P.Status;
   Value : P.Event;
   Lost : Unsigned_64;
   Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   Ready : Boolean := False;
   Submitted, Drained : Boolean;

   procedure Fail (Name : String) is
   begin
      debugPrint ("TEST: FAIL log-retention: log-flood: " & Name & ASCII.LF);
      Ignore := syscall (SYSCALL_EXIT, 1);
      loop
         Ignore := syscall (SYSCALL_SLEEP, 1000);
      end loop;
   end Fail;

   function Image (Value : Natural) return String is
      Raw : constant String := Natural'Image (Value);
   begin
      return Raw (Raw'First + 1 .. Raw'Last);
   end Image;

   procedure Publish (Text : String; Level : L.Severity) is
      Made : constant L.Decoded := L.Make (Text, Level);
   begin
      if not Made.Success then
         Fail ("make record");
      end if;
      CuBit.Logging.Emit (Writer, Made.Value, Submitted);
   end Publish;
begin
   --  Wait for the quiet source's boot records (its Warning marker).
   CuBit.Logging.Subscribe (Reader, Result, Minimum => L.Warning);
   if Result /= P.OK then
      Fail ("subscribe");
   end if;
   while not Ready loop
      CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
      if Result = P.OK and then L.Text (Value.Data) = Quiet_Done_Text then
         Ready := True;
      elsif Result = P.Empty then
         if syscall (SYSCALL_GETTIME) - Started > Wait_Limit_Ms then
            Fail ("the quiet source never finished its boot records");
         end if;
         Ignore := syscall (SYSCALL_SLEEP, Poll_Ms);
      elsif Result not in P.OK | P.Gap then
         Fail ("read");
      end if;
   end loop;
   CuBit.Logging.Close (Reader, Result);
   for I in 1 .. Flood_Records loop
      Publish ("log-flood: record " & Image (I), L.Information);
      if I mod Burst_Records = 0 then
         CuBit.Logging.Flush (Writer, Drained, Wait_Ms => Flush_Wait_Ms);
         Ignore := syscall (SYSCALL_SLEEP, Burst_Pause_Ms);
      end if;
   end loop;
   CuBit.Logging.Flush (Writer, Drained, Wait_Ms => Flush_Wait_Ms);
   debugPrint ("log-flood: wrote " & Image (Flood_Records) & " records, ring shed" &
               Unsigned_64'Image (CuBit.Logging.Dropped (Writer)) & ASCII.LF);
   Publish (Flood_Done_Text, L.Warning);
   CuBit.Logging.Flush (Writer, Drained, Wait_Ms => Flush_Wait_Ms);
   if not Drained then
      Fail ("logstore did not take the done record");
   end if;
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
