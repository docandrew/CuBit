package body XHCI_Endpoint_Recovery with SPARK_Mode => On is

   procedure Halted (R : in out Pipe_Recovery; Code : Completion_Code) is
   begin
      if Soft_Retryable (Code) and then R.Soft < Soft_Limit then
         R.Current := Need_Soft_Reset;
         R.Soft := R.Soft + 1;
      elsif R.Hard < Hard_Limit then
         R.Current := Need_Reset;
         R.Hard := R.Hard + 1;
      else
         R.Current := Need_Port_Reset;
      end if;
   end Halted;

   procedure Issued (R : in out Pipe_Recovery) is
   begin
      R.Current := (case R.Current is
                      when Need_Soft_Reset => Await_Soft_Reset,
                      when Need_Reset => Await_Reset,
                      when Need_Dequeue => Await_Dequeue,
                      when others => Await_Clear_Halt);
   end Issued;

   procedure Completed (R : in out Pipe_Recovery; Code : Completion_Code) is
      Stopped : constant Boolean := Code = CC_SUCCESS or else Code = CC_CONTEXT_STATE_ERROR;
   begin
      case R.Current is
         when Await_Soft_Reset =>
            R.Current := (if Stopped then Need_Doorbell else Need_Port_Reset);
         when Await_Reset =>
            R.Current := (if Stopped then Need_Dequeue else Need_Port_Reset);
         when Await_Dequeue =>
            R.Current := (if Code = CC_SUCCESS then Need_Clear_Halt else Need_Port_Reset);
         when others =>
            R.Current := Need_Requeue;
      end case;
   end Completed;

   procedure Rang (R : in out Pipe_Recovery) is
   begin
      R.Current := Running;
   end Rang;

   procedure Delivered (R : in out Pipe_Recovery) is
   begin
      R.Soft := 0;
      R.Hard := 0;
   end Delivered;

   procedure Escalate (R : in out Pipe_Recovery) is
   begin
      R.Current := Need_Port_Reset;
   end Escalate;

end XHCI_Endpoint_Recovery;
