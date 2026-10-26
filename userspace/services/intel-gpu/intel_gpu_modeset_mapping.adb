with Intel_GPU_Diagnostics;
with Intel_GPU_Wait_Report;
with Interfaces; use Interfaces;
with Intel_GPU_Map_Report;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.Devices;
with Intel_GPU_Supervisor;
package body Intel_GPU_Modeset_Mapping is
   package PP renames Intel_GPU_Modeset_Pages;
   Attempted, Mapped : array (PP.Page) of Boolean := [others => False];
   Grant_Deadline_Ms : constant := 30;
   Token_Base : constant Unsigned_64 := 16#4947_0060#;
   Retry_Label : constant Unsigned_32 := 16#F002#;
   Granted_Label : constant Unsigned_32 := 16#F000#;
   function Ready (Item : PP.Page) return Boolean is (Mapped (Item));
   function Prepare
     (Owner : Boolean; Register_Base : Unsigned_64; Item : PP.Page)
      return String
   is
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Token : constant Unsigned_64 := Token_Base + PP.Index (Item);
      Started, Previous, Now : Unsigned_64;
      Granted : Boolean := False;
      Region : CuBit.Handles.Device_Memory := CuBit.Handles.No_Device_Memory;
      Activity : Activity_Result;
      pragma Unreferenced (Activity);
   begin
      if Attempted (Item) then return "already-attempted"; end if;
      Attempted (Item) := True;
      if not Owner or else Register_Base = 0 or else
        Register_Base mod PP.Page_Bytes /= 0 or else
        Register_Base > Unsigned_64'Last - PP.Offset (Item) - PP.Page_Bytes
      then return "owner-or-BAR-invalid"; end if;
      Started := syscall (SYSCALL_GETTIME);
      if Started = Unsigned_64'Last then return "clock-unavailable"; end if;
      Previous := Started;
      Msg.tag := (label => PP.Request_Label, length => 1, protocol => 0, ipc => <>, reserved => 0);
      Msg.words := [PP.Index (Item), 0, 0, 0];
      if not Intel_GPU_Supervisor.Submit_Accepting (Msg, Token) then return "submit-failed"; end if;
      loop
         Now := syscall (SYSCALL_GETTIME);
         if Now = Unsigned_64'Last or else Now < Previous or else
           Now - Started >= Grant_Deadline_Ms
         then return "grant-timeout-or-invalid-clock"; end if;
         Previous := Now;
         if Intel_GPU_Diagnostics.Poll_Driver (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Intel_GPU_Supervisor.Reply_Tag (Receipt) = (label => Retry_Label, length => 0, protocol => 0, ipc => <>, reserved => 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               if not Intel_GPU_Supervisor.Submit_Accepting (Msg, Token) then return "retry-failed"; end if;
            else
               Granted := Receipt.status = COMPLETION_OK and then
                 Intel_GPU_Supervisor.Reply_Tag (Receipt) = (label => Granted_Label, length => 0, protocol => 0, ipc => <>, reserved => 0) and then
                 Receipt.msg.words = [0, 0, 0, 0];
               Region := Intel_GPU_Supervisor.Granted_Region (Receipt);
               Granted := Granted and then CuBit.Handles.Valid (Region);
               exit;
            end if;
         end if;
         Intel_GPU_Wait_Report.Pulse (Intel_GPU_Wait_Report.Devmgr_Grant);
         Activity := Wait_For_Activity_Until (Now + 1);
      end loop;
      if not Granted then return "grant-denied"; end if;
      if not CuBit.Handles.Devices.Map_Device
           (Region, Register_Base + PP.Offset (Item),
            PP.Address (Item), 1, Read_Only => False)
      then return Intel_GPU_Map_Report.Denied (Register_Base + PP.Offset (Item), 4096); end if;
      Mapped (Item) := True;
      return "ready (NO power reference)";
   end Prepare;
end Intel_GPU_Modeset_Mapping;
