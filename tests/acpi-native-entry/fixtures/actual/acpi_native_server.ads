with ACPI_Requests;
with CuBit.Messages;
with ACPI_Native_Events;
package ACPI_Native_Server is
   type Stop_Reason is (Wait_Unavailable, Unexpected_Completion, Stop_Requested, Owner_Work_Refused);
   procedure Run (Server : in out ACPI_Requests.State; Reason : out Stop_Reason);
   type Owner_Outcome is (Owner_Idle, Owner_Pending, Owner_Refused);
   procedure Idle_Owner (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
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
   package With_Dispatch is
      procedure Serve (Server : in out ACPI_Requests.State; Reason : out Stop_Reason);
   end With_Dispatch;
end ACPI_Native_Server;
