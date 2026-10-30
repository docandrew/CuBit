with Interfaces; use Interfaces;
with AML_Decode;
with ACPI_Clock_Source;
with ACPI_Fatal_Diagnostics;
with ACPI_Snapshot_Startup;
with ACPI_Native_Snapshot_Load;
with ACPI_Native_Server;
with ACPI_Native_Log;
with CuBit.Messages;
with Startup;
package body ACPI_Native_Instance is
   package L renames ACPI_Snapshot_Startup;
   use type L.Outcome;
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
   begin
      if Started then return; end if;
      Started := True;
      ACPI_Native_Log.Info ("acpi: stage service-start ok");
      ACPI_Clock_Source.Read (Clock, Available);
      if not Available or else L.R.Tick (Clock) > L.R.Tick'Last - Startup_Timeout_Microseconds then
         ACPI_Native_Log.Warning ("acpi: stage snapshot fail CLOCK_UNAVAILABLE");
         ACPI_Native_Log.Warning ("acpi: bring-up reached service-start (1/4)");
         return;
      end if;
      -- CuBit's allocator logs and stops the process on native OOM; it does
      -- not raise Storage_Error. No partial owner can then enter this runner.
      ACPI_Native_Snapshot_Load.Load
        (L.R.Tick (Clock) + Startup_Timeout_Microseconds, Owner, Result);
      if Result.Startup.Fatal.Present then
         begin
            ACPI_Native_Log.Critical
              (ACPI_Fatal_Diagnostics.Format (Result.Startup.Fatal));
         exception
            when others => null; -- Preserve startup outcome; never replay AML.
         end;
      end if;
      if Result.Startup.Status /= L.Loaded or else Owner = null then
         ACPI_Native_Log.Warning
           ("acpi: stage snapshot fail " & Result.Startup.Status'Image &
            " table=" & Natural'Image (Result.Startup.Table) &
            " admission=" & Result.Startup.Admission'Image &
            " copy=" & Result.Startup.Copy.Status'Image);
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
      ACPI_Native_Log.Info
        ("acpi: stage startup ok " & Result.Startup.Completion.Stage'Image & " hardware-pending");
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
      ACPI_Native_Log.Warning ("acpi: stage serve fail " & Reason'Image);
   exception
      when others =>
         -- This attempt is terminal. Never serve/retry a partial import or
         -- replay AML after an unexpected adapter/allocator/assertion failure.
         ACPI_Native_Log.Warning ("acpi: stage service fail UNEXPECTED_EXCEPTION");
   end Start;
end ACPI_Native_Instance;
