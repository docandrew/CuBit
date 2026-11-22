pragma Ada_2022;
with Interfaces;
with ACPI_Source_Paths;with ACPI_Power_Identity_Protocol;
with ACPI_Requests;with ACPI_Cached_Power_Values;
with ACPI_Cached_Battery_Values;
with ACPI_Native_Server;
-- Trusted serialized native owner: fixed families, SAME runtime/Core. The
-- required health actual attests the actual admitted event sources, not a
-- notification payload or observer-supplied flag. Orderly source masking/stop
-- must keep this true until captured receipts are routed; admission to new
-- AML is closed separately by Begin_Stop. No implicit source enable.
generic
   with function Sources_Healthy return Boolean;
package ACPI_Native_Consumers is
   type Family is (Lid, AC_Source, Battery, HID_I2C);
   type Failure is (None, Not_Ready, Start_Refused, Family_Refused,
      Publication_Overlap, Fact_Too_Long, Source_Unhealthy);
   type Diagnostic_Line is record
      Text : String (1 .. 192) := [others => ' '];
      Last : Natural range 0 .. 192 := 0;
   end record;
   type Diagnostic_Lines is array (Positive range 1 .. 4) of Diagnostic_Line;
   type Report is record
      Started, Stopping, Closed : Boolean := False;
      Why : Failure := None;
      At_Family : Family := Lid;
      Refusal_Count : Natural := 0;
      Diagnostics : Diagnostic_Lines;
   end record;
   function Observe return Report;
   -- Copy existing lid/AC caches only, using one trusted native clock read.
   -- No AML, refresh, receipt claim/ACK, logging, IPC or hardware operation.
   -- Unavailable views have empty values; ages use the native clock epoch.
   procedure Read_Power (Server : ACPI_Requests.State;
      Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Power_Values.Snapshot)
     with Pre => ACPI_Requests.Valid (Server);
   procedure Start (Server : ACPI_Requests.State; Accepted : out Boolean)
     with Pre=>ACPI_Requests.Valid(Server);
   -- All four consumers see the SAME retained receipt before caller ACK.
   -- Routing stays available during stopping; it performs no AML or logging.
   procedure Route (Server : ACPI_Requests.State; Routed : out Boolean)
     with Pre=>ACPI_Requests.Valid(Server);
   -- One family step per call. A pending copied fact prevents another sample
   -- from replacing it. Caller pumps/routs notifications BEFORE this routine.
   procedure Advance (Server : in out ACPI_Requests.State;
      Result : out ACPI_Native_Server.Owner_Outcome)
     with Pre=>ACPI_Requests.Valid(Server);
   -- Complete copied inventory; never refreshes or returns Core identities.
   -- Unavailable inventories and unsampled entries carry empty payloads.
   procedure Read_Batteries (Server : ACPI_Requests.State;
      Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Battery_Values.View)
     with Pre => ACPI_Requests.Valid (Server);
   -- Complete generation-bound copied source names; no clock, AML or refresh.
   -- Index0 is the measured AC source; positive indices are battery slots.
   procedure Read_Identity(Server:ACPI_Requests.State;Index:Natural;Expected:ACPI_Cached_Power_Values.Generation;
      Current:out ACPI_Cached_Power_Values.Generation;Code:out ACPI_Power_Identity_Protocol.Result_Code;
      Value:out ACPI_Source_Paths.Path) with Pre=>ACPI_Requests.Valid(Server);
   -- Metadata-only explicit reader demand. Preview reads the trusted clock;
   -- Commit accepts only that same observation under the same serialized owner.
   procedure Preview_Battery_Refresh(Server:ACPI_Requests.State;Now:out Interfaces.Unsigned_64;Allowed:out Boolean);
   procedure Commit_Battery_Refresh(Server:ACPI_Requests.State;Now:Interfaces.Unsigned_64;
      Ticket:out Interfaces.Unsigned_64;Accepted:out Boolean);
   function Battery_Refresh_Ready(Ticket:Interfaces.Unsigned_64) return Boolean;
   Maximum_Battery_Facts:constant Positive:=ACPI_Cached_Battery_Values.Maximum_Batteries;
   Maximum_HID_Facts:constant Positive:=16;
   Single_Family_Facts:constant Positive:=2;
   Maximum_Facts:constant Positive:=Maximum_Battery_Facts+Maximum_HID_Facts+Single_Family_Facts;
   Maximum_Fact_Characters : constant Positive := 192;
   subtype Fact_Line is String(1..Maximum_Fact_Characters);
   subtype Fact_Length is Natural range 0..Maximum_Fact_Characters;
   -- No logger/IPC here. Return one complete copied observation only while
   -- actual runtime/Fields/Core are quiet and no receipt is outstanding.
   procedure Take_Fact (Server : ACPI_Requests.State; Line : out Fact_Line;
      Last : out Fact_Length; Present : out Boolean)
     with Pre=>ACPI_Requests.Valid(Server);
   function Facts_Pending return Boolean;
   procedure Begin_Stop;
   -- Stop admission first, drain receipts/facts, then invalidate consumers.
   -- Does not retire hardware, the EC engine, SCI or _REG ownership.
   procedure Close (Server : ACPI_Requests.State; Completed : out Boolean)
     with Pre=>ACPI_Requests.Valid(Server);
   function Quiet (Server : ACPI_Requests.State) return Boolean
     with Pre=>ACPI_Requests.Valid(Server);
end ACPI_Native_Consumers;
