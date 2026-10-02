with Interfaces; use Interfaces;
with CCL_Manifest_Bindings;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.Logging;
with Mesa_Probe_Log;
procedure Main is
   package P renames CuBit.Log_Protocol;
   use type P.Status;
   Reader : CuBit.Logging.Reader
     (CapabilitySlot (CCL_Manifest_Bindings.Slot_Log_Observer));
   Value : P.Event;
   Result : P.Status;
   Lost, Ignore, Source : Unsigned_64 := 0;
   Count : Natural;
   function Probe return Unsigned_32
     with Import, Convention => C, External_Name => "cubit_log_smoke";
   function Burst return Unsigned_32
     with Import, Convention => C, External_Name => "cubit_log_burst";
   Summaries : Natural;
   procedure Check (OK : Boolean; Name : String) is
   begin
      if not OK then
         debugPrint ("TEST: FAIL mesa-log-bridge: " & Name & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 1);
         loop
            Ignore := syscall (SYSCALL_SLEEP, 1000);
         end loop;
      end if;
   end Check;
begin
   CuBit.Logging.Subscribe (Reader, Result);
   Check (Result = P.OK, "observer subscription");
   for Attempt in 1 .. 256 loop
      CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
      exit when Result = P.Empty;
      Check (Result = P.OK and Lost = 0, "initial drain");
   end loop;
   Check (Result = P.Empty, "bounded initial drain");
   for Round in 1 .. 3 loop
      -- Real wrapper returns only after grant and CQE retirement.
      Check ((if Round = 3 then Burst else Probe) = 73,
             "C result preserved after publisher retirement");
      Count := 0;
      Summaries := 0;
      for Attempt in 1 .. 256 loop
         CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
         exit when Result = P.Empty;
         Check (Result = P.OK and Lost = 0, "record delivery");
         if CuBit.Log_Records.Text (Value.Data) =
           (if Round = 3 then "MESA-LOG paced burst record"
            else "MESA-LOG native bridge record") then
            Check (P.Is_Publisher (Value.Publication_Tag) and Value.Source /= 0,
                   "authenticated publisher metadata");
            if Source = 0 then Source := Value.Source; end if;
            Check (Value.Source = Source, "stable publisher identity");
            Count := Count + 1;
         elsif CuBit.Log_Records.Text (Value.Data) =
           "MESA-LOG bridge dropped(hex)=0000000000000000" then
            Check (Value.Source = Source, "summary source identity");
            Summaries := Summaries + 1;
         else
            Check (Value.Source /= Source or Source = 0,
                   "invalid record was not published");
         end if;
      end loop;
      Check (Result = P.Empty and Count = (if Round = 3 then 80 else 1)
             and Summaries = 1, "all records and zero-loss summary delivered");
      debugPrint ("MESA-LOG round" & Round'Image & " delivered and retired" & ASCII.LF);
   end loop;
   CuBit.Logging.Close (Reader, Result);
   Check (Result = P.OK, "observer close");
   debugPrint ("TEST: PASS mesa-log-bridge native delivery (NO GPU)" & ASCII.LF);
end Main;
