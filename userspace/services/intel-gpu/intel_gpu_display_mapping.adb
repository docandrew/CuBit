with Intel_GPU_Diagnostics;
with Intel_GPU_Wait_Report;
with Interfaces; use Interfaces;
with Intel_GPU_Map_Report;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.Devices;
with Intel_GPU_Supervisor;
with Intel_GPU_Display_Pages;
package body Intel_GPU_Display_Mapping is
   Attempted, Mapped : Boolean := False;
   function Ready return Boolean is (Mapped);
   function Prepare
     (Owner : Boolean; Register_Base : Unsigned_64) return String
   is
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Started : Unsigned_64;
      Previous, Now : Unsigned_64;
      Granted : Boolean;
      Region : CuBit.Handles.Device_Memory := CuBit.Handles.No_Device_Memory;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      if Attempted then return "already-attempted"; end if;
      Attempted := True;
      if not Owner or else Register_Base = 0 or else
        Register_Base mod 4096 /= 0 or else
        Register_Base > Unsigned_64'Last - 16#47000#
      then return "owner-or-BAR-invalid"; end if;
      Started := syscall (SYSCALL_GETTIME);
      if Started = Unsigned_64'Last then return "clock-unavailable"; end if;
      Previous := Started;
      for Index in Intel_GPU_Display_Pages.Page_Index loop
         declare
            Token : constant Unsigned_64 := 16#4947_0030# + Unsigned_64 (Index);
         begin
            Msg.tag := (label => Intel_GPU_Display_Pages.Request_Label, length => 1,
                        protocol => 0, ipc => <>, reserved => 0);
            Msg.words := [Unsigned_64 (Index), 0, 0, 0];
            if not Intel_GPU_Supervisor.Submit_Accepting (Msg, Token) then return "submit-failed"; end if;
            Granted := False;
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
                     if not Intel_GPU_Supervisor.Submit_Accepting (Msg, Token) then return "retry-failed"; end if;
                  else
                     Granted := Receipt.status = COMPLETION_OK and then
                       Intel_GPU_Supervisor.Reply_Tag (Receipt) = (label => 16#F000#, length => 0, protocol => 0, ipc => <>, reserved => 0) and then
                       Receipt.msg.words = [0, 0, 0, 0];
                     Region := Intel_GPU_Supervisor.Granted_Region (Receipt);
                     Granted := Granted and then CuBit.Handles.Valid (Region);
                     exit;
                  end if;
               end if;
               Intel_GPU_Wait_Report.Pulse (Intel_GPU_Wait_Report.Devmgr_Grant);
               Activity := Wait_For_Activity_Until (Now + 1);
            end loop;
            if not Granted then return "grant-denied-or-poll-exhausted"; end if;
            if not CuBit.Handles.Devices.Map_Device
           (Region, Register_Base + Intel_GPU_Display_Pages.Offset (Index),
            Virtual_Base + Unsigned_64 (Index) * 4096, 1, Read_Only => False)
            then return Intel_GPU_Map_Report.Denied
                   (Register_Base + Intel_GPU_Display_Pages.Offset (Index), 4096) &
                 " (partial mappings retained)"; end if;
         end;
      end loop;
      Mapped := True;
      return "ready (NO power writes/reference)";
   end Prepare;
end Intel_GPU_Display_Mapping;
