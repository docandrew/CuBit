pragma Ada_2022;
with ACPI_Firmware_Data_Requests;
generic
   type Transport is limited private;
   type Capability is private;
   No_Capability : Capability;
   with function Present (Item : Capability) return Boolean;
   with procedure Clock
     (Now : out ACPI_Firmware_Data_Requests.IPC.Time_Instant; Available : out Boolean);
   -- Transport is permanently bound to its trusted devmgr startup endpoint.
   -- No endpoint, sender PID or grant address comes from request words.
   with procedure Exchange
     (Channel : in out Transport; Deadline : ACPI_Firmware_Data_Requests.IPC.Time_Instant;
      Request : ACPI_Firmware_Data_Requests.IPC.Packet;
      Reply : out ACPI_Firmware_Data_Requests.IPC.Packet;
      Received : out Capability; Delivered : out ACPI_Firmware_Data_Requests.IPC.Delivery);
   with procedure Inspect
     (Item : Capability; Evidence : out ACPI_Firmware_Data_Requests.Grant_Evidence);
   -- Clear only after positive disposal. Failure retains the exact token.
   with procedure Dispose (Item : in out Capability; Closed : out Boolean);
package ACPI_Firmware_Data_Client with SPARK_Mode is
   package R renames ACPI_Firmware_Data_Requests;
   package P renames R.P;
   package IPC renames R.IPC;
   type State is limited private;
   type Outcome is (Prepared, Not_Ready, Reentered, Attempt_Stopped,
      Completion_Unknown, Refused, Grant_Ready, Cleanup_Required,
      Cleanup_Complete, Grant_Taken, Refusal_Recorded);
   type Observation is record
      Request : R.Observation;
      Working, Retained_Capability, Cleanup_Due : Boolean;
      Last_Result : R.Outcome;
   end record;
   function Observe (Context : State) return Observation;
   procedure Prepare (Context : in out State; Extent : P.Envelope; Result : out Outcome);
   procedure Attempt (Context : in out State; Channel : in out Transport;
      Deadline : IPC.Time_Instant; Result : out Outcome);
   procedure Retry_Disposal (Context : in out State; Result : out Outcome);
   -- Move ownership into the caller's already reserved record. No allocation,
   -- mapping or data access. The mapper/data scope must freshly check current
   -- capability rights and its own deadline before using this authority.
   procedure Take_Grant (Context : in out State; Item : out Capability; Result : out Outcome);
   procedure Record_Refusal (Context : in out State; Result : out Outcome);
private
   type State is limited record
      Request : R.State;
      Held : Capability := No_Capability;
      Working, Cleanup_Due : Boolean := False;
      Last_Result : R.Outcome := R.Not_Ready;
   end record;
end ACPI_Firmware_Data_Client;
