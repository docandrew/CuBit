pragma Ada_2022;
with Interfaces;
-- Process-local deterministic bookkeeping, not source-control authority.
-- Each instance owns one queue/ticket type. Caller serializes all operations.
generic
   Maximum_Delivery_ID : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
package ACPI_GPE_Pending with SPARK_Mode,
   Abstract_State => Queue_State, Initializes => Queue_State is
   Maximum_Source_Number : constant := 255;
   type Source_Number is range 0 .. Maximum_Source_Number;
   Source_Count : constant Positive := Source_Number'Pos (Source_Number'Last) + 1;
   subtype Pending_Count is Natural range 0 .. Source_Count;
   type Source_Set is array (Source_Number) of Boolean;
   No_Sources : constant Source_Set := [others => False];
   type Phase is (Fresh, Accepting, Stopping, Faulted, Stopped);
   type Ticket is private;
   No_Ticket : constant Ticket;
   function Is_Issued (Item : Ticket) return Boolean with Global => null;
   function Source (Item : Ticket) return Source_Number
     with Global => null, Pre => Is_Issued (Item);
   type Capture_Result is (Captured, Coalesced, Not_Enrolled, Not_Accepting, Source_Active);
   type Claim_Result is (Claimed, Empty, Busy, Closed, Exhausted);
   type Finish_Result is (Completed, Failure_Retained, Stale, Closed);
   type Retire_Result is (Retired, Stale, Not_Stopping);
   type View is record
      Stage : Phase := Fresh;
      Pending : Pending_Count := 0;
      Active, Ever_Faulted : Boolean := False;
   end record;
   function Observe return View with Global => (Input => Queue_State);
   function Enrolled (Number : Source_Number) return Boolean with Global => (Input => Queue_State);
   function Pending (Number : Source_Number) return Boolean with Global => (Input => Queue_State);
   -- Start is one-shot, including an empty enrollment. No source is enabled.
   procedure Start (Sources : Source_Set; Accepted : out Boolean)
     with Always_Terminates, Global => (In_Out => Queue_State);
   -- Trusted caller has already completed masking this attributable source.
   -- Coalesced adds no delivery. Source_Active is not a fresh receipt; caller
   -- keeps it masked and handles the attribution failure outside this queue.
   procedure Capture (Number : Source_Number; Result : out Capture_Result)
     with Always_Terminates, Global => (In_Out => Queue_State);
   -- One active receipt at a time; a rotating cursor bounds starvation. IDs
   -- never wrap. Exhaustion retains queued work and stops new dispatch.
   procedure Claim (Item : out Ticket; Result : out Claim_Result)
     with Always_Terminates, Global => (In_Out => Queue_State),
       Post => (if Result = Claimed then Is_Issued (Item) else Item = No_Ticket);
   -- Healthy is the caller's logical completion result, not proof of physical
   -- quiescence. A failed receipt stays active and cannot later become success.
   procedure Finish (Item : Ticket; Healthy : Boolean; Result : out Finish_Result)
     with Always_Terminates, Global => (In_Out => Queue_State);
   procedure Begin_Stop with Always_Terminates, Global => (In_Out => Queue_State);
   -- Logical retirement only after the enclosing owner has independently
   -- reconciled its hardware/runtime/result obligations. Neither operation
   -- performs cleanup or provides evidence permitting physical resource release.
   procedure Discard_Queued (Count : out Pending_Count)
     with Always_Terminates, Global => (In_Out => Queue_State);
   procedure Retire_Active (Item : Ticket; Result : out Retire_Result)
     with Always_Terminates, Global => (In_Out => Queue_State);
private
   type Ticket is record
      ID : Interfaces.Unsigned_64 := 0;
      Number : Source_Number := Source_Number'First;
   end record;
   No_Ticket : constant Ticket := (others => <>);
end ACPI_GPE_Pending;
