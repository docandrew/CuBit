with Intel_GPU_Diagnostics;
with Intel_GPU_Wait_Report;
with Interfaces; use Interfaces;
with Intel_GPU_Map_Report;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.Devices;
with Intel_GPU_Supervisor;
with Intel_GPU_GGTT_Access;
package body Intel_GPU_GGTT_Mapping is
   Attempted, Mapped : Boolean := False;
   Mapped_Bytes : Unsigned_64 := 0;
   function Ready return Boolean is (Mapped);
   function Bytes return Unsigned_64 is (Mapped_Bytes);

   function Prepare
     (Owner, Reset_Complete : Boolean;
      Register_Base, Table_Bytes : Unsigned_64) return String
   is
      Token : constant Unsigned_64 := 16#4947_0033#;
      Chunk_Bytes : constant Unsigned_64 := 2 * 1024 * 1024;
      Physical, Started, Previous, Now : Unsigned_64;
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Granted : Boolean := False;
      Region : CuBit.Handles.Device_Memory := CuBit.Handles.No_Device_Memory;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      if Attempted then return "already-attempted"; end if;
      Attempted := True;
      if not Owner or else not Reset_Complete or else Register_Base = 0 or else
        Register_Base mod 4096 /= 0 or else
        Register_Base > Unsigned_64'Last - 16#100_0000# or else
        Table_Bytes not in 2_097_152 | 4_194_304 | 8_388_608
      then return "owner-reset-or-extent-invalid"; end if;
      Physical := Register_Base + 16#80_0000#;
      Started := syscall (SYSCALL_GETTIME);
      if Started = Unsigned_64'Last then return "clock-unavailable"; end if;
      Previous := Started;
      Msg.tag := (label => Intel_GPU_GGTT_Access.Write_Request_Label, length => 0,
                  protocol => 0, ipc => <>, reserved => 0);
      if not Intel_GPU_Supervisor.Submit_Accepting (Msg, Token) then return "submit-failed"; end if;
      for Poll in 1 .. 30_000 loop
         Now := syscall (SYSCALL_GETTIME);
         if Now = Unsigned_64'Last or else Now < Previous or else
           Now - Started >= 30_000
         then return "grant-timeout-or-invalid-clock"; end if;
         Previous := Now;
         if Intel_GPU_Diagnostics.Poll_Driver (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Intel_GPU_Supervisor.Reply_Tag (Receipt) = (label => 16#F002#, length => 0, protocol => 0, ipc => <>, reserved => 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               -- Only explicit startup-busy is retryable. Denial, malformed
               -- replies, timeout or lost replies never retry a consumed grant.
               if not Intel_GPU_Supervisor.Submit_Accepting (Msg, Token) then return "retry-failed"; end if;
            else
               Granted := Receipt.status = COMPLETION_OK and then
                 Intel_GPU_Supervisor.Reply_Tag (Receipt) = (label => 16#F000#, length => 2, protocol => 0, ipc => <>, reserved => 0) and then
                 Receipt.msg.words = [Physical, Table_Bytes, 0, 0];
               Region := Intel_GPU_Supervisor.Granted_Region (Receipt);
               Granted := Granted and then CuBit.Handles.Valid (Region);
               exit;
            end if;
         end if;
         Intel_GPU_Wait_Report.Pulse (Intel_GPU_Wait_Report.Devmgr_Grant);
         Activity := Wait_For_Activity_Until (Now + 1);
      end loop;
      if not Granted then return "grant-denied-or-poll-exhausted"; end if;
      -- UC/NX MAP_DEVICE aliases, distinct from the read-only inspection view.
      -- Every supported size is a multiple of 2MiB. Keep calls below the
      -- kernel's 1024-page limit and retain partial maps if a later call fails.
      for Chunk in Unsigned_64 range 0 .. Table_Bytes / Chunk_Bytes - 1 loop
         if not CuBit.Handles.Devices.Map_Device
           (Region, Physical + Chunk * Chunk_Bytes,
            Virtual_Base + Chunk * Chunk_Bytes, 512, Read_Only => False)
         then return Intel_GPU_Map_Report.Denied (Physical + Chunk * Chunk_Bytes, Chunk_Bytes) &
              " (partial mappings retained)"; end if;
      end loop;
      Mapped_Bytes := Table_Bytes;
      Mapped := True;
      return "ready (NO PTE writes)";
   end Prepare;
end Intel_GPU_GGTT_Mapping;
