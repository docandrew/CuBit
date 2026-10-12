with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Protocol;
with CuBit.Log_Records;
with CuBit.System_Facts;
with Desktop_Startup_Clock;
package body Desktop_Bootstrap_Logs is
   use Interfaces;
   package P renames CuBit.Log_Protocol;
   use type P.Status;
   use type CuBit.Log_Records.Severity;

   Reader     : CuBit.Logging.Reader;
   Status     : P.Status := P.Unavailable;
   Next_Try   : Unsigned_64 := 0;
   Kept       : Desktop_Bootstrap_Notes.Notes;
   --  Records read per Gather turn before the clock is checked again.
   Batch : constant := 16;

   procedure Gather (Budget_Ms : Unsigned_64) is
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Event   : P.Event;
      Lost    : Unsigned_64;
   begin
      if Status not in P.OK | P.Empty | P.Gap then
         if Started < Next_Try or else Registered_Driver (DRIVER_LOGSTORE) = No_Process then
            return;
         end if;
         Next_Try := Started + Retry_Ms;
         --  Information and up: Warnings plus the kernel's physical map.
         CuBit.Logging.Subscribe (Reader, Status, CuBit.Log_Records.Information,
                                  Limit_Ms => Call_Limit_Ms);
         if Status not in P.OK | P.Empty | P.Gap then
            return;
         end if;
      end if;
      loop
         for I in 1 .. Batch loop
            CuBit.Logging.Read_Next (Reader, Event, Lost, Status);
            exit when Status /= P.OK;
            declare
               Text : constant String := CuBit.Log_Records.Text (Event.Data);
               Important : constant Boolean :=
                 CuBit.Log_Records.Level (Event.Data) >= CuBit.Log_Records.Warning;
            begin
               if Text'Last < Positive'Last then
                  Desktop_Bootstrap_Notes.Note (Kept, Important, Text);
                  if Important or else Desktop_Bootstrap_Notes.Is_Map_Record (Text) then
                     debugPrint ("DESKTOP-BOOTSTRAP-NOTE: " & Text & ASCII.LF);
                  end if;
               end if;
            end;
         end loop;
         exit when Status /= P.OK;
         declare
            Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
         begin
            exit when Now < Started or else Now - Started >= Budget_Ms;
         end;
      end loop;
   end Gather;

   function Current return Desktop_Bootstrap_Notes.Notes is (Kept);

   Gathered_Ms : Unsigned_64 := 0;
   --  Pause between gathering turns while waiting (milliseconds).
   Wait_Step_Ms : constant := 20;

   function Kernel_Denials return Unsigned_64 is (CuBit.System_Facts.Device_Memory_Denials);
   function Kernel_Kills return Unsigned_64 is (CuBit.System_Facts.Process_Kills);

   function Gathered_At_Ms return Unsigned_64 is (Gathered_Ms);

   procedure Gather_Before_Bootstrap is
      Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Now     : Unsigned_64;
      Ignore  : Unsigned_64;
   begin
      loop
         Gather (Call_Limit_Ms);
         Now := syscall (SYSCALL_GETTIME);
         exit when Now < Started or else Now - Started >= Maximum_Wait_Ms;
         exit when Now - Started >= Minimum_Wait_Ms and then
           Unsigned_64 (Kept.Highest_Denial) >= Kernel_Denials and then
           Unsigned_64 (Kept.Highest_Kill) >= Kernel_Kills;
         Ignore := syscall (SYSCALL_SLEEP, Wait_Step_Ms);
      end loop;
      Gathered_Ms := Desktop_Startup_Clock.Elapsed_Ms;
      debugPrint ("DESKTOP-BOOTSTRAP: logs as of t=" & Unsigned_64'Image (Gathered_Ms) &
        " ms; kernel device-memory denials" & Unsigned_64'Image (Kernel_Denials) &
        ", highest in the log" & Natural'Image (Kept.Highest_Denial) &
        "; process kills" & Unsigned_64'Image (Kernel_Kills) &
        ", highest in the log" & Natural'Image (Kept.Highest_Kill) & ASCII.LF);
   end Gather_Before_Bootstrap;
end Desktop_Bootstrap_Logs;
