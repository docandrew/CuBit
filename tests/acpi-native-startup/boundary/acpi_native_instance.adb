with Interfaces; use Interfaces;
with AML_Decode;
with ACPI_Clock_Source;
with ACPI_Bootstrap;
with ACPI_Native_Delays;
with ACPI_Fatal_Diagnostics;
with ACPI_Snapshot_Startup;
with ACPI_Native_Snapshot_Load;
with ACPI_Native_Server;
with ACPI_Native_HID_Detection;
with ACPI_Native_Power_Detection;
with ACPI_Native_Log;
with ACPI_Native_Module_Notes;
with ACPI_Requests;
with ACPI_Startup_Diagnostics;
with ACPI_Native_EC_Boot_Notes;
with CuBit.Messages;
with Startup;
package body ACPI_Native_Instance is
   package L renames ACPI_Snapshot_Startup;
   use type L.Outcome;
   use type ACPI_Native_Server.Stop_Reason;
   use type L.State_Access;
   use type L.R.Tick;
   -- Native monotonic samples and snapshot deadlines are microseconds.
   Startup_Timeout_Microseconds : constant L.R.Tick := 10_000_000;
   Owner : L.State_Access;
   Started : Boolean := False;
   function Hex (Value : Unsigned_64) return String is
      Hex_Digits : constant String := "0123456789ABCDEF";
      Text : String (1 .. 16);
      Left : Unsigned_64 := Value;
   begin
      for I in reverse Text'Range loop
         Text (I) := Hex_Digits (Natural (Left and 16#F#) + 1);
         Left := Shift_Right (Left, 4);
      end loop;
      return Text;
   end Hex;
   procedure Start is
      Clock : AML_Decode.Integer_Value;
      Available : Boolean;
      Result : ACPI_Native_Snapshot_Load.Report;
      Registration_Result : Unsigned_64;
      Reason : ACPI_Native_Server.Stop_Reason;
      Detection : ACPI_Native_HID_Detection.Report;
      Power : ACPI_Native_Power_Detection.Report;
      Delay_Accepted : Boolean;
      Devices : ACPI_Bootstrap.Device_Report;
      procedure Attempt is
      begin
      ACPI_Native_Log.Info ("acpi: stage service-start ok");
      ACPI_Clock_Source.Read (Clock, Available);
      if not Available or else L.R.Tick (Clock) > L.R.Tick'Last - Startup_Timeout_Microseconds then
         ACPI_Native_Log.Warning ("acpi: stage snapshot fail CLOCK_UNAVAILABLE");
         ACPI_Native_Log.Warning ("acpi: bring-up reached service-start (1/4)");
         return;
      end if;
      -- CuBit's allocator logs and stops the process on native OOM; it does
      -- not raise Storage_Error. No partial owner can then enter this runner.
      ACPI_Native_Delays.Begin_Startup
        (Unsigned_64 (L.R.Tick (Clock) + Startup_Timeout_Microseconds), Delay_Accepted);
      if not Delay_Accepted then
         ACPI_Native_Log.Warning ("acpi: stage delay-budget fail ALREADY_CLOSED");
         return;
      end if;
      ACPI_Native_Snapshot_Load.Load
        (L.R.Tick (Clock) + Startup_Timeout_Microseconds, Owner, Result);
      if Result.Startup.Fatal.Present then
         ACPI_Native_Log.Critical
           (ACPI_Fatal_Diagnostics.Format (Result.Startup.Fatal));
      end if;
      ACPI_Native_Module_Notes.Observe (Result.Startup.Module_Errors);
      if Result.Startup.Status /= L.Loaded or else Owner = null then
         ACPI_Native_Log.Warning
           ("acpi: stage snapshot fail " & Result.Startup.Status'Image &
            " table=" & Natural'Image (Result.Startup.Table) &
            " admission=" & Result.Startup.Admission'Image &
            " copy=" & Result.Startup.Copy.Status'Image);
         if Result.Startup.Status = L.Startup_Refused then
            ACPI_Native_Log.Warning
              (ACPI_Startup_Diagnostics.Format (Result.Startup.Completion));
         end if;
         if Result.Last_Copy.Available then
            ACPI_Native_Log.Warning
              ("acpi: stage snapshot-detail fail " & Result.Startup.Status'Image &
               " last-copy-status=" & Result.Last_Copy.Status'Image &
               " table=0x" & Hex (Unsigned_64 (Result.Last_Copy.Table)) &
               " offset=0x" & Hex (Unsigned_64 (Result.Last_Copy.Offset)) &
               " copied=0x" & Hex (Unsigned_64 (Result.Last_Copy.Copied)));
         end if;
         ACPI_Native_Log.Warning ("acpi: bring-up reached service-start (1/4)");
         return;
      end if;
      ACPI_Native_Log.Info
        ("acpi: stage snapshot ok tables=" & L.Count'Image (Result.Startup.Catalog.Tables) &
         " bytes=" & L.Count'Image (Result.Startup.Catalog.Total_Bytes));
      ACPI_Native_Log.Info (ACPI_Startup_Diagnostics.Format (Result.Startup.Completion));
      ACPI_Native_EC_Boot_Notes.Observe (Owner.all);
      Devices := ACPI_Requests.Device_Completion (Owner.all);
      if ACPI_Startup_Diagnostics.Device_Initialization_Completed (Devices) then
         ACPI_Native_Log.Info (ACPI_Startup_Diagnostics.Format (Devices));
      else
         ACPI_Native_Log.Warning (ACPI_Startup_Diagnostics.Format (Devices));
      end if;
      ACPI_Native_HID_Detection.Run (Owner.all, Detection);
      ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Owner.all));
      if Detection.Cleanup_Required or else Detection.Recovery_Needed then
         ACPI_Native_Log.Warning ("acpi: bring-up reached hid-discovery (3/4)");
         -- Do not proceed to a serving owner after uncertain retained cleanup.
         return;
      end if;
      ACPI_Native_Power_Detection.Run (Owner.all, Power);
      ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Owner.all));
      if Power.Recovery_Needed then
         ACPI_Native_Log.Warning ("acpi: bring-up reached power-discovery (3/4)");
         return;
      end if;
      ACPI_Native_Delays.Close_Startup;
      Registration_Result := CuBit.Messages.registerDriver
        (CuBit.Messages.DRIVER_ACPI, Startup.Registration);
      if Registration_Result = Unsigned_64'Last then
         ACPI_Native_Log.Warning ("acpi: stage registration fail REFUSED");
         ACPI_Native_Log.Warning ("acpi: bring-up reached startup (3/4)");
         return;
      end if;
      ACPI_Native_Log.Info ("acpi: stage registration ok observer-only");
      ACPI_Native_Log.Info ("acpi: bring-up reached registration (4/4) hardware-pending");
      ACPI_Native_Server.Run (Owner.all, Reason);
      ACPI_Native_Module_Notes.Observe (ACPI_Requests.Module_Errors (Owner.all));
      if Reason = ACPI_Native_Server.Stop_Requested then
         ACPI_Native_Log.Info ("acpi: stage serve ok STOP_REQUESTED");
      else
         ACPI_Native_Log.Warning ("acpi: stage serve fail " & Reason'Image);
      end if;
      end Attempt;
   begin
      if Started then return; end if;
      Started := True;
      -- Expected failures return typed outcomes from Attempt. The native
      -- runtime terminates unexpected failures; it has no exception handlers.
      Attempt;
      ACPI_Native_Delays.Close_Startup;
   end Start;
end ACPI_Native_Instance;
