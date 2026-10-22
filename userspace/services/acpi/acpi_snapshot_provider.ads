pragma Ada_2022;
with ACPI_Requests;
with ACPI_Service;
package ACPI_Snapshot_Provider with SPARK_Mode is
   subtype Revision is ACPI_Requests.Revision_Number;
   type Table_Descriptor is record
      ID : Positive;
      Kind : ACPI_Service.Table_Kind;
      Length : Positive;
   end record;
   type Catalog is array (Positive range <>) of Table_Descriptor;
   type Cleanup_State is (Clear, Pending, Uncertain);
   type Progress is (Not_Started, Running, Waiting_For_Cleanup, Completed,
                     Rejected, Unknown_Outcome, Invalid_Response, Local_Limit);
   type Completion_Pages is array (Natural range 0 .. 4) of ACPI_Requests.Words;
   type Observation is record
      Available : Boolean := False;
      Pages : Completion_Pages := [others => [others => 0]];
   end record;
   -- These are trusted synchronous adapters. Known means the reply belongs to
   -- this exact call on the authenticated endpoint, not merely matching words.
   -- Import binds Index to immutable bytes matching Item, acquires via the
   -- provider capability, and reports provider-owned grant retirement/reclaim knowledge independently.
   -- Native_Blocks hides its Pending state from the wire; this callback must
   -- never invent a receiver cleanup acknowledgment. No
   -- payload PID/address supplies authority. An unknown cleanup cannot be
   -- treated as released. One Driver owns the provider grant adapter and mutation endpoint exclusively.
   generic
      Max_Tables : Positive := 32;
      with procedure Exchange (Request : ACPI_Requests.Packet;
         Reply : out ACPI_Requests.Packet; Known : out Boolean);
      with procedure Import_Table (Index : Positive; Token : Revision;
         Item : Table_Descriptor; Reply : out ACPI_Requests.Packet;
         Known : out Boolean; Cleanup : out Cleanup_State);
      with procedure Release_Provider_Grant (Released : out Boolean);
   package Driver is
      type State is limited private;
      procedure Begin_Catalog (S : in out State; Items : Catalog);
      -- One operation per call. No mutation is automatically retried. Once a
      -- mutation outcome is unknown/rejected, this state cannot restart.
      procedure Advance (S : in out State);
      -- Only retires/reclaims the outstanding provider grant; never repeats admission.
      procedure Retry_Cleanup (S : in out State);
      function Status (S : State) return Progress;
      function Cleanup (S : State) return Cleanup_State;
      function Accepted_Tables (S : State) return Natural;
      function Remote_Status (S : State) return ACPI_Requests.Outcome;
      -- Independent diagnostic observation. Even Complete here does not change
      -- Unknown_Outcome to Completed or establish which caller ran Finish.
      procedure Observe_Completion (Result : out Observation);
   private
      type Step is (Dormant, Phase_Query, Capacity_Query, Start, Importing,
                    Finish, Stopped);
      type Storage is array (Positive range 1 .. Max_Tables) of Table_Descriptor;
      type State is limited record
         Used : Boolean := False;
         At_Step : Step := Dormant;
         Result : Progress := Not_Started;
         Loan : Cleanup_State := Clear;
         Items : Storage := [others => (1, ACPI_Service.Description, 1)];
         Count : Natural range 0 .. Max_Tables := 0;
         Accepted : Natural range 0 .. Max_Tables := 0;
         Version : Revision := 0;
         Remote : ACPI_Requests.Outcome := ACPI_Requests.OK;
      end record;
   end Driver;
end ACPI_Snapshot_Provider;
