with ACPI_Requests;
with CuBit.Messages;
with ACPI_Native_Events;
with Interfaces;
package ACPI_Native_Server is
   type Stop_Reason is (Wait_Unavailable, Unexpected_Completion, Stop_Requested,
      Owner_Work_Refused, Owner_Maintenance_Stopped);
   procedure Run (Server : in out ACPI_Requests.State; Reason : out Stop_Reason);
   type Owner_Outcome is (Owner_Idle, Owner_Pending, Owner_Refused);
   procedure Idle_Owner (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
   procedure No_Maintenance (Wake_Deadline_Milliseconds : out Interfaces.Unsigned_64;
      May_Continue : out Boolean);
   generic
      with procedure Dispatch_Message
        (Server : in out ACPI_Requests.State; From : CuBit.Messages.Process_ID;
         Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);
      with procedure Handle_Event
        (Publisher : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message;
         Stop : out Boolean; Handled : out Boolean) is ACPI_Native_Events.Handle;
      -- One bounded,serialized local step per turn. Pending means runnable
      -- work remains: never return Pending merely while waiting for hardware.
      -- No blocking peer IPC or reentry. Refused stops before new dispatch;
      -- retained state/cleanup must stay owned,with no automatic replay/reset.
      with procedure Advance_Owner
        (Server : in out ACPI_Requests.State; Result : out Owner_Outcome) is Idle_Owner;
      -- One bounded local maintenance step after owner work and before IPC.
      -- Returns an absolute kernel wait-clock deadline, with Wait_Forever
      -- explicit for no scheduled work. Never convert lease microseconds
      -- here or restart deadlines on spurious wakes. False stops admission
      -- before dispatch, retaining the caller's owner/cleanup state.
      with procedure Maintain_Owner
        (Wake_Deadline_Milliseconds : out Interfaces.Unsigned_64;
         May_Continue : out Boolean) is No_Maintenance;
   package With_Dispatch is
      procedure Serve (Server : in out ACPI_Requests.State; Reason : out Stop_Reason);
   end With_Dispatch;
end ACPI_Native_Server;
