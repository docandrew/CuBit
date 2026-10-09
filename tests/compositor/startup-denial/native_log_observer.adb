with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
procedure Main is
   package P renames CuBit.Log_Protocol;
   package L renames CuBit.Log_Records;
   use type P.Status;
   Reader : CuBit.Logging.Reader;
   Value : P.Event;
   Result : P.Status;
   Lost, Ignore, Source, Deadline : Unsigned_64;
   Step : Natural := 0;
   Backend_Seen, Failure_Seen : Boolean := False;
   procedure Check (OK : Boolean; Why : String) is
   begin
      if not OK then
         debugPrint ("TEST: FAIL desktop log channel: " & Why & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT, 1);
         loop Ignore := syscall (SYSCALL_SLEEP, 1000); end loop;
      end if;
   end Check;
   function Starts (Text, Prefix : String) return Boolean is
     (Text'Length >= Prefix'Length and then
      Text (Text'First .. Text'First + Prefix'Length - 1) = Prefix);
begin
   Deadline := syscall (SYSCALL_GETTIME) + 30000;
   loop
      Source := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DESKTOP);
      exit when Source /= 0;
      Check (syscall (SYSCALL_GETTIME) < Deadline, "desktop registration timeout");
      Ignore := syscall (SYSCALL_SLEEP, 10);
   end loop;
   CuBit.Logging.Subscribe (Reader, Result, Source => Source);
   Check (Result = P.OK, "observer subscription");
   loop
      Check (syscall (SYSCALL_GETTIME) < Deadline, "startup records timeout");
      for Batch in 1 .. 64 loop
         CuBit.Logging.Read_Next (Reader, Value, Lost, Result);
         exit when Result = P.Empty;
         Check (Result = P.OK and then Lost = 0, "stream gap or read failure");
         Check (Value.Source = Source and then P.Is_Publisher (Value.Publication_Tag),
                "source or publication authority");
         declare Text : constant String := L.Text (Value.Data);
         begin
            if Text = "DESKTOP-CHECKPOINT: ENTERED" then
               Check (Step = 0, "ENTERED order/duplicate"); Step := 1;
            elsif Text = "DESKTOP-CHECKPOINT: INIT_BEFORE" then
               Check (Step = 1, "INIT_BEFORE order/duplicate"); Step := 2;
            elsif Text = "DESKTOP-CHECKPOINT: INIT_AFTER" then
               Check (Step = 2, "INIT_AFTER order/duplicate"); Step := 3;
            elsif Text = "DESKTOP-CHECKPOINT: SELECTED" then
               Check (Step = 3 and then Backend_Seen, "SELECTED order/duplicate"); Step := 4;
            elsif Starts (Text, "DESKTOP-VULKAN: setup unavailable stage=") then
               Check (Text = "DESKTOP-VULKAN: setup unavailable stage=admission" and then
                 Step = 3 and then not Backend_Seen and then not Failure_Seen,
                 "wrong, duplicate or reordered failed stage");
               Failure_Seen := True;
            elsif Starts (Text, "DESKTOP-VULKAN: startup=SOFTWARE") or else
              Starts (Text, "DESKTOP-VULKAN: startup=READY") then
               Check (Step = 3 and then not Backend_Seen and then Failure_Seen, "backend order/duplicate or missing failed stage");
               Backend_Seen := True;
            end if;
         end;
      end loop;
      exit when Step = 4 and then Backend_Seen;
      Ignore := syscall (SYSCALL_SLEEP, 1);
   end loop;
   CuBit.Logging.Close (Reader, Result);
   Check (Result = P.OK, "observer close");
   debugPrint ("TEST: PASS desktop log channel authenticated startup/backend stage=admission" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT, 0);
end Main;
