pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Protocol;
generic
   type Lease is limited private;
   with function Admission_Open return Boolean;
   with function Needs_Cleanup (Item : Lease) return Boolean;
   with procedure Begin_Window (Deadline_Microseconds : out Interfaces.Unsigned_64;
      Budget : out ACPI_Global_Lock_Protocol.Attempt_Budget; Available : out Boolean);
   with procedure Acquire (Item : in out Lease; Deadline_Microseconds : Interfaces.Unsigned_64;
      Budget : ACPI_Global_Lock_Protocol.Attempt_Budget;
      Result : out ACPI_Global_Lock_Protocol.Lock_Outcome);
   with procedure Check_Access (Item : Lease; Result : out ACPI_Global_Lock_Protocol.Lock_Outcome);
   with procedure Release (Item : in out Lease; Result : out ACPI_Global_Lock_Protocol.Lock_Outcome);
   Maximum_Depth : Positive;
-- One retained whole-Field scope over the SAME local physical lock owner.
-- No transport, new physical lock, ticket decoding or deadline extension.
package ACPI_Local_Field_Scope with SPARK_Mode is
   package P renames ACPI_Global_Lock_Protocol;
   type Depth_Count is new Natural range 0 .. Maximum_Depth;
   type Statistics is record
      Depth : Depth_Count := 0;
      Working, Faulted, Held, Cleanup_Required : Boolean := False;
      First_Failure : P.Lock_Outcome := P.Configured;
   end record;
   type Context is limited private;
   function Observe (State : Context) return Statistics;
   function Ready (State : Context) return Boolean;
   function Pending (State : Context) return Boolean;
   procedure Begin_Field (State : in out Context; Requires_Lock, For_Write : Boolean;
      Accepted : out Boolean);
   procedure Check_Datum (State : in out Context; Accepted : out Boolean);
   -- Every scalar callback must check this immediately before its effect.
   -- Target authority, write policy and exact geometry remain provider duties.
   procedure End_Field (State : in out Context; Accepted : out Boolean);
   -- End unwinds exactly one ACCEPTED Begin, including after a data failure.
   -- A rejected child Begin opens no frame and must not consume its parent.
   procedure Mark_Fault (State : in out Context);
   procedure Cleanup (State : in out Context; Completed : out Boolean);
   -- Explicit stop/cleanup at depth zero. Never resumes or retries AML/data.
   -- Uncertain lock outcomes retain the original lease solely for maintenance.
private
   subtype Position is Depth_Count range 1 .. Depth_Count'Last;
   type Frames is array (Position) of Boolean;
   type Context is limited record
      Info : Statistics;
      Owners : Frames := [others => False];
      Item : Lease;
   end record;
end ACPI_Local_Field_Scope;
