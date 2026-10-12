with Interfaces; use Interfaces;
with Intel_GPU_Map_Report;
with CuBit.Messages; use CuBit.Messages;
package body Intel_GPU_Pipe_Mapping is
   package PP renames Intel_GPU_Pipe_Pages;
   Attempted, Mapped : array (PP.Page_Index) of Boolean := [others => False];
   Broker_Slot : constant CapabilitySlot := 15;
   Grant_Deadline_Ms : constant := 30;
   Token_Base : constant Unsigned_64 := 16#4947_0040#;
   Retry_Label : constant Unsigned_32 := 16#F002#;
   Granted_Label : constant Unsigned_32 := 16#F000#;
   function Ready (Item : PP.Page_Index) return Boolean is (Mapped (Item));
   function Prepare
     (Owner : Boolean; Register_Base : Unsigned_64; Item : PP.Page_Index)
      return String
   is
      Msg : Message := NULL_MESSAGE;
      Receipt : aliased CompletionEntry;
      Token : constant Unsigned_64 := Token_Base + Unsigned_64 (Item);
      Started, Previous, Now : Unsigned_64;
      Granted : Boolean := False;
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
      Msg.tag := (PP.Request_Label, 1, 0, 0);
      Msg.words := [Unsigned_64 (Item), 0, 0, 0];
      if not capSubmit (Broker_Slot, Msg, Token) then return "submit-failed"; end if;
      loop
         Now := syscall (SYSCALL_GETTIME);
         if Now = Unsigned_64'Last or else Now < Previous or else
           Now - Started >= Grant_Deadline_Ms
         then return "grant-timeout-or-invalid-clock"; end if;
         Previous := Now;
         if Poll_Completion (Receipt'Address) /= 0 and then Receipt.token = Token then
            if Receipt.status = COMPLETION_OK and then
              Receipt.msg.tag = (Retry_Label, 0, 0, 0) and then
              Receipt.msg.words = [0, 0, 0, 0]
            then
               if not capSubmit (Broker_Slot, Msg, Token) then return "retry-failed"; end if;
            else
               Granted := Receipt.status = COMPLETION_OK and then
                 Receipt.msg.tag = (Granted_Label, 0, 0, 0) and then
                 Receipt.msg.words = [0, 0, 0, 0];
               exit;
            end if;
         end if;
         Activity := Wait_For_Activity_Until (Now + 1);
      end loop;
      if not Granted then return "grant-denied"; end if;
      if syscall (SYSCALL_MAP_DEVICE, Register_Base + PP.Offset (Item),
                  PP.Address (Item), 1, 0) /= 0
      then return Intel_GPU_Map_Report.Denied (Register_Base + PP.Offset (Item), 4096); end if;
      Mapped (Item) := True;
      return "ready (NO power reference)";
   end Prepare;
end Intel_GPU_Pipe_Mapping;
