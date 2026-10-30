pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with AML_Decode;
with ACPI_Native_Instance;
with ACPI_Snapshot_Startup;
with ACPI_Fatal_Diagnostics;
with CuBit.Messages;
with Owner_Test_Control;
procedure Owner_Tests is
   package T renames Owner_Test_Control;
   package L renames ACPI_Snapshot_Startup;
   use type T.Scenario;
   use type L.Outcome;
   use type L.R.Tick;
   use type T.Level;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   Critical_Seen : Natural := 0;
   Load_Expected, Completed, Should_Register, Should_Serve, Wanted_Fatal : Boolean;
begin
   if Ada.Command_Line.Argument_Count /= 3 then raise Program_Error with "three model parameters required"; end if;
   T.Selected := T.Scenario'Value (Ada.Command_Line.Argument (1));
   T.Width := AML_Decode.Integer_Width'Value (Ada.Command_Line.Argument (2));
   T.Broken_Critical := Boolean'Value (Ada.Command_Line.Argument (3));
   ACPI_Native_Instance.Start;
   Load_Expected := T.Selected not in T.Clock_Unavailable | T.Clock_Overflow;
   Check (T.Clock_Reads = 1, "one initial monotonic sample");
   Check (T.Loads = Boolean'Pos (Load_Expected), "one load only when clock representable");
   Check (T.Native_Reads = 0, "no real native adapter executed by owner model");
   if Load_Expected then
      Check (T.Passed_Deadline = 10_000_010, "finite microsecond deadline");
   else
      Check (T.Copies = 0, "no copy on unavailable clock");
   end if;
   Completed := Load_Expected and then T.Selected /= T.Load_Exception and then T.Last_Load.Status = L.Loaded;
   Should_Register := Completed;
   Should_Serve := Completed and then T.Selected /= T.Register_Refused;
   Wanted_Fatal := Load_Expected and then T.Selected in T.Module_Fatal .. T.Device_Fatal;
   Check (T.Registrations = Boolean'Pos (Should_Register), "no registration before complete owner");
   Check (T.Serves = Boolean'Pos (Should_Serve), "only registered complete owner served");
   if Should_Register then
      Check (T.Registered_Role = CuBit.Messages.DRIVER_ACPI, "named catalog role");
      Check (T.Registered_At > T.Load_Completed_At, "registration follows complete load");
   end if;
   if Should_Serve then Check (T.Served_At > T.Registered_At, "serve follows registration"); end if;
   Check (T.Critical_Attempts = Boolean'Pos (Wanted_Fatal), "exactly one fatal log attempt");
   for I in 1 .. T.Log_Count loop
      if T.Log (I).Severity = T.Critical then
         Critical_Seen := Critical_Seen + 1;
         Check (T.Log (I).Text (1 .. T.Log (I).Length) =
                ACPI_Fatal_Diagnostics.Format (T.Last_Load.Fatal), "copied exact Fatal diagnostic");
      end if;
   end loop;
   Check (Critical_Seen = T.Critical_Attempts, "Critical severity retained even if logger raises");
   declare
      Loads : constant Natural := T.Loads;
      Registers : constant Natural := T.Registrations;
      Serves : constant Natural := T.Serves;
      Copies : constant Natural := T.Copies;
      Clocks : constant Natural := T.Clock_Reads;
      Logs : constant Natural := T.Log_Count;
   begin
      ACPI_Native_Instance.Start;
      Check (T.Loads = Loads and T.Registrations = Registers and T.Serves = Serves and
        T.Copies = Copies and T.Clock_Reads = Clocks and T.Log_Count = Logs,
        "second Start never replays any effect, including failure and logger exception");
   end;
   Ada.Text_IO.Put_Line ("Native owner checks" & Checks'Image);
end Owner_Tests;
