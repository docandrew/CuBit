pragma Ada_2022;
with Interfaces;
with HID_I2C_Descriptors;
package HID_I2C_Bring_Up with SPARK_Mode is
   package D renames HID_I2C_Descriptors;
   use type D.Byte;
   use type D.Decode_Status;
   type Tick is new Interfaces.Unsigned_64;
   type Reset_Ticket is new Interfaces.Unsigned_64;
   No_Reset_Ticket : constant Reset_Ticket := 0;
   type Transport_Status is (Transferred, Unavailable, Denied, Timed_Out,
                            Bus_Error, Cancelled);
   type Stage is (HID_Descriptor, Power_Command, Reset_Command,
                  Reset_Completion, Report_Descriptor);
   type Stage_Count is range 0 .. 5;
   type Failure is (None, Clock_Unavailable, Clock_Reversed, Deadline_Expired,
      Transport_Failed, Bad_Completion_Count, Incomplete_Transfer,
      Descriptor_Refused, Capacity_Refused, Reset_Not_Acknowledged,
      Invalid_Reset_Ticket, Reset_Release_Failed);
   type Report is record
      -- Only transport acquisition, not HID semantic readiness or input.
      Descriptor_Retrieved : Boolean := False;
      Completed : Stage_Count := 0;
      At_Stage : Stage := HID_Descriptor;
      Reason : Failure := None;
      Transport : Transport_Status := Unavailable;
      Cleanup : Transport_Status := Transferred;
      Reset_Still_Armed : Boolean := False;
      Ticket : Reset_Ticket := No_Reset_Ticket;
      Descriptor : D.Decode_Result;
      Capacity : D.Capacity_Status := D.Empty_Report;
   end record;
   generic
      -- Trusted adapters bound to ONE already-authorized device. They must not
      -- reset/reenter this operation or synchronously call back into ACPI.
      -- Transfer is a combined write/read when both spans are nonempty; empty
      -- Write_Data requests a bare input read. Deadline is the same absolute
      -- monotonic bound for every operation, never a fresh per-step timeout.
      with procedure Clock (Now : out Tick; Available : out Boolean);
      with procedure Transfer
        (Write_Data : D.Bytes; Read_Data : out D.Bytes; Deadline : Tick;
         Written, Read : out Natural; Status : out Transport_Status);
      -- Arm/latch a fresh IRQ generation BEFORE writing RESET. Early completion
      -- during that write must remain available to Await_Interrupt. Success
      -- requires a nonzero ticket. Release is bounded local cleanup, no IO.
      with procedure Arm_Reset (Deadline : Tick; Ticket : out Reset_Ticket;
                               Status : out Transport_Status);
      with procedure Await_Interrupt (Ticket : Reset_Ticket; Deadline : Tick;
                                     Status : out Transport_Status);
      with procedure Release_Reset (Ticket : Reset_Ticket; Status : out Transport_Status);
      -- Owner maps each event to one logstore/serial stage line and emits the
      -- final report summary. Trace must be bounded and must not reenter.
      with procedure Trace (Item : Report; OK : Boolean);
   procedure Run
     (Descriptor_Register : D.Register_Number; Deadline : Tick;
      Report_Data, Input_Scratch : out D.Bytes; Output_Capacity : D.Byte_Count;
      Result : out Report)
     with Post => (for all Byte of Input_Scratch => Byte = 0)
       and then (if Result.Descriptor_Retrieved then
         Result.Completed = Stage_Count'Last and then Result.Reason = None
         and then Result.Descriptor.Status = D.Decoded
         else (for all Byte of Report_Data => Byte = 0))
       and then (if Result.Reset_Still_Armed then Result.Ticket /= No_Reset_Ticket);
   -- Caller provides disjoint buffers and capacities explicitly. On failure all output buffers
   -- are zero; on success only Report_Data's descriptor prefix is populated.
   -- No implicit retries, compatibility quirks, input publication or register
   -- grant follows. Power/reset may already have taken effect on failure;
   -- cleanup status and ticket tell the owner what still needs resolution.
end HID_I2C_Bring_Up;
