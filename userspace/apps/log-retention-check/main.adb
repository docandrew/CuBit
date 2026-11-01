pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Logging;
with Startup;

--  Native regression for LOG-002: one chatty source must not erase a quiet
--  source's boot history. This program is the quiet source: it publishes a
--  few boot records (as acpi-backend does), log-flood then writes thousands,
--  and afterwards
--  - a subscription filtered to this process (what (logs.recent "name")
--    does, CCL_Log_IO.Recent) still replays every boot record, with no gap;
--  - a full-history subscription replays the retained window in publish
--    order with no gap, more records than one subscriber queue holds.
procedure Main is
   package P renames CuBit.Log_Protocol;
   package L renames CuBit.Log_Records;
   use type P.Status;

   Writer : CuBit.Logging.Publisher := CuBit.Logging.Bound_To (Startup.Logstore);
   Reader : CuBit.Logging.Reader := CuBit.Logging.Bound_To (Startup.Log_Observer);

   --  Agreed with log-flood (userspace/apps/log-flood).
   Quiet_Done_Text : constant String := "log-retention: quiet boot done";
   Flood_Done_Text : constant String := "log-flood: done";
   Flood_Prefix : constant String := "log-flood: record ";
   Stage_Prefix : constant String := "log-retention: stage ";

   Quiet_Records : constant := 10;
   --  The flood's records a full replay must at least hold: its boot
   --  window and newest records in logstore (Log_Fanout.Boot_Records and
   --  Newest_Records), the newest being its done record.
   Least_Flood_Retained : constant := 64;
   Poll_Ms : constant := 20;
   Wait_Limit_Ms : constant Unsigned_64 := 60_000;
   Flush_Wait_Ms : constant := 2_000;
   --  Empty reads in a row, Poll_Ms apart, that end a replay: logstore
   --  refills the stream as it is read, so one Empty is not the end.
   Settle_Polls : constant := 10;

   Ignore : Unsigned_64;
   Result : P.Status;
   Value : P.Event;
   Lost : Unsigned_64;
   Own_Pid : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   --  log-flood's process, from its done record.
   Flood_Source : Unsigned_64 := 0;
   Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   Submitted, Drained, Done : Boolean := False;

   procedure Fail (Name : String) is
   begin
      debugPrint ("TEST: FAIL log-retention: " & Name & ASCII.LF);
      Ignore := syscall (SYSCALL_EXIT, 1);
      loop
         Ignore := syscall (SYSCALL_SLEEP, 1000);
      end loop;
   end Fail;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Fail (Name);
      end if;
   end Check;

   function Image (Value : Natural) return String is
      Raw : constant String := Natural'Image (Value);
   begin
      return Raw (Raw'First + 1 .. Raw'Last);
   end Image;

   function Starts (Text, Prefix : String) return Boolean is
     (Text'Length >= Prefix'Length and then
      Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix);

   procedure Publish (Text : String; Level : L.Severity) is
      Made : constant L.Decoded := L.Make (Text, Level);
   begin
      Check (Made.Success, "make record");
      CuBit.Logging.Emit (Writer, Made.Value, Submitted);
      Check (Submitted, "record written into the ring");
      CuBit.Logging.Flush (Writer, Drained, Wait_Ms => Flush_Wait_Ms);
      Check (Drained, "logstore took the record");
   end Publish;
begin
   for Stage in 1 .. Quiet_Records loop
      Publish (Stage_Prefix & Image (Stage) & " ok", L.Information);
   end loop;
   Check (CuBit.Logging.Dropped (Writer) = 0, "boot records accepted");
   Publish (Quiet_Done_Text, L.Warning);

   --  Wait for the flood to end (Warnings only, so this queue stays small).
   CuBit.Logging.Subscribe (Reader, Result, Minimum => L.Warning);
   Check (Result = P.OK, "subscribe for the flood's end");
   while not Done loop
      CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
      if Result = P.OK and then L.Text (Value.Data) = Flood_Done_Text then
         Done := True;
         Flood_Source := Value.Source;
      elsif Result = P.Empty then
         Check (syscall (SYSCALL_GETTIME) - Started <= Wait_Limit_Ms, "the flood never ended");
         Ignore := syscall (SYSCALL_SLEEP, Poll_Ms);
      else
         Check (Result in P.OK | P.Gap, "read while waiting");
      end if;
   end loop;
   CuBit.Logging.Close (Reader, Result);
   Check (Result = P.OK, "close the waiting subscription");

   --  (logs.recent "<this program>"): the same reads CCL_Log_IO.Recent makes.
   declare
      Next_Stage : Positive := 1;
   begin
      CuBit.Logging.Subscribe (Reader, Result, Source => Own_Pid);
      Check (Result = P.OK, "subscribe to the quiet source");
      for Step in 1 .. P.Observer_Queue_Records + 1 loop
         CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
         exit when Result not in P.OK | P.Gap;
         Check (Result = P.OK, "quiet source replay reports no loss");
         Check (Value.Source = Own_Pid, "only the quiet source replayed");
         if Next_Stage <= Quiet_Records then
            Check (L.Text (Value.Data) = Stage_Prefix & Image (Next_Stage) & " ok",
                   "quiet boot record" & Natural'Image (Next_Stage) & " replayed in order");
            Next_Stage := Next_Stage + 1;
         end if;
      end loop;
      Check (Result = P.Empty, "quiet source replay ends");
      Check (Next_Stage = Quiet_Records + 1,
             "quiet source kept" & Natural'Image (Next_Stage - 1) & " of" &
             Natural'Image (Quiet_Records) & " boot records after the flood");
      CuBit.Logging.Close (Reader, Result);
      Check (Result = P.OK, "close the quiet source subscription");
      debugPrint ("log-retention: quiet source replayed all" & Natural'Image (Quiet_Records) &
                  " boot records after the flood" & ASCII.LF);
      debugPrint ("TEST: PASS log-retention-quiet-source" & ASCII.LF);
   end;

   --  Full history: the retained window, in publish order, with no gap.
   declare
      Total, Quiet_Seen, Flood_Seen, Empties : Natural := 0;
      Previous_Ms : Unsigned_64 := 0;
      Saw_Flood_Done : Boolean := False;
   begin
      CuBit.Logging.Subscribe (Reader, Result);
      Check (Result = P.OK, "subscribe to every source");
      while Empties < Settle_Polls loop
         CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
         if Result = P.Empty then
            Empties := Empties + 1;
            Ignore := syscall (SYSCALL_SLEEP, Poll_Ms);
         else
            Check (Result /= P.Gap, "full replay reports no loss (gap of" &
                   Unsigned_64'Image (Lost) & ")");
            Check (Result = P.OK, "full replay read");
            Empties := 0;
            Total := Total + 1;
            Check (Value.Monotonic_Ms >= Previous_Ms, "full replay in publish order");
            Previous_Ms := Value.Monotonic_Ms;
            declare
               Text : constant String := L.Text (Value.Data);
            begin
               if Value.Source = Own_Pid and then Starts (Text, Stage_Prefix) then
                  Quiet_Seen := Quiet_Seen + 1;
               elsif Value.Source = Flood_Source then
                  Flood_Seen := Flood_Seen + 1;
                  Saw_Flood_Done := Text = Flood_Done_Text;
                  Check (Saw_Flood_Done or else Starts (Text, Flood_Prefix), "flood record text");
               end if;
            end;
         end if;
      end loop;
      CuBit.Logging.Close (Reader, Result);
      debugPrint ("log-retention: full replay" & Natural'Image (Total) & " records, quiet" &
                  Natural'Image (Quiet_Seen) & ", flood" & Natural'Image (Flood_Seen) & ASCII.LF);
      Check (Quiet_Seen = Quiet_Records, "full replay holds the quiet boot records");
      Check (Flood_Seen >= Least_Flood_Retained, "full replay holds the flood's retained window");
      Check (Saw_Flood_Done, "full replay holds the flood's last record");
      debugPrint ("TEST: PASS log-retention-full-history" & ASCII.LF);
   end;
   debugPrint ("TEST: PASS log-retention" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
