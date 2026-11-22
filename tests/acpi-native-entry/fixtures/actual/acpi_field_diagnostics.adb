pragma Ada_2022;
package body ACPI_Field_Diagnostics with SPARK_Mode is
   procedure Remember (First : in out Report; Item : Report) is
   begin
      if not Present (First) then First := Item; end if;
   end Remember;
   function Name (Item : Cause) return String is
     (case Item is
      when No_Failure => "UNCLASSIFIED",
      when Startup_Deadline => "STARTUP_DEADLINE",
      when Startup_Clock_Unavailable => "STARTUP_CLOCK_UNAVAILABLE",
      when Startup_Clock_Reversed => "STARTUP_CLOCK_REVERSED",
      when Lease_Deadline => "LEASE_DEADLINE",
      when Lease_Clock_Unavailable => "LEASE_CLOCK_UNAVAILABLE",
      when Lease_Clock_Reversed => "LEASE_CLOCK_REVERSED",
      when IPC_Deadline => "IPC_DEADLINE",
      when IPC_Clock_Unavailable => "IPC_CLOCK_UNAVAILABLE",
      when IPC_Clock_Reversed => "IPC_CLOCK_REVERSED",
      when Transport_Not_Sent => "TRANSPORT_NOT_SENT",
      when Transport_Unknown => "TRANSPORT_UNKNOWN",
      when Malformed_Reply => "MALFORMED_REPLY",
      when Backend_Refused => "BACKEND_REFUSED",
      when Backend_Read_Failed => "BACKEND_READ_FAILED",
      when Backend_Late => "BACKEND_LATE",
      when Invalid_Request => "INVALID_REQUEST",
      when Invalid_Deadline => "INVALID_DEADLINE",
      when Peer_Changed => "PEER_CHANGED",
      when Capacity_Exhausted => "CAPACITY_EXHAUSTED",
      when Reentered => "REENTERED",
      when Lease_Unavailable => "LEASE_UNAVAILABLE",
      when Scope_Unavailable => "SCOPE_UNAVAILABLE",
      when Provider_Uncertain => "PROVIDER_UNCERTAIN",
      when Window_Unavailable => "WINDOW_UNAVAILABLE",
      when Deadline_Overflow => "DEADLINE_OVERFLOW");
   function Name (Item : Point) return String is
     (case Item is
      when No_Point => "NONE",
      when Startup_Window => "STARTUP_WINDOW",
      when Lease_Window => "LEASE_WINDOW",
      when Acquire_Lease => "ACQUIRE_LEASE",
      when Validate_Request => "VALIDATE_REQUEST",
      when Before_Select_Lease => "SELECT_LEASE",
      when Before_Select_Clock => "SELECT_CLOCK",
      when Select_Exchange => "SELECT_EXCHANGE",
      when Select_Reply => "SELECT_REPLY",
      when Before_Read_Lease => "READ_LEASE",
      when Before_Read_Clock => "READ_CLOCK",
      when Read_Exchange => "READ_EXCHANGE",
      when Read_Reply => "READ_REPLY",
      when After_Read => "READ_POSTCHECK",
      when Read_Guard => "READ_GUARD",
      when End_Lease => "FIELD_END",
      when Release_Lease => "RELEASE_LEASE",
      when Cleanup_Lease => "CLEANUP_LEASE",
      when Scope_State => "SCOPE_STATE",
      when Provider_Dispatch => "PROVIDER_DISPATCH");
   function Format (Item : Report) return String is
     ("cause=" & Name (Item.Reason) & " at=" & Name (Item.At_Point));
   function Elapsed_Milliseconds (Start, Sample : Microseconds; Available : Boolean) return String is
     (if not Available or else Sample < Start then "UNKNOWN"
      else Word'Image (Word ((Sample - Start) / Microseconds_Per_Millisecond)));
end ACPI_Field_Diagnostics;
