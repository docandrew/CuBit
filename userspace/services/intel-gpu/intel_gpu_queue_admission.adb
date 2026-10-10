package body Intel_GPU_Queue_Admission with SPARK_Mode is

   function Decide
     (D : Q.Descriptor; View : Session_View; Quiescing : Boolean; Now : Microseconds;
      Capacity : Positive) return Decision
   is
      Operation : constant Q.Opcode := Decode_Opcode (D.Operation);
      Result : Decision := (Kind => Reject, Cause => Q.Bad_Opcode, Operation => Operation,
                            Has_Context => False, Context => 0);
   begin
      if Context_Named (D, View) then
         Result.Has_Context := True;
         Result.Context := D.Context;
      end if;
      if Operation = Q.Invalid then
         return Result;
      elsif Operation not in Q.Execute | Q.Signal then
         Result.Cause := Q.Unsupported_Opcode;
         return Result;
      elsif D.Flags /= 0 then
         Result.Cause := Q.Bad_Flags;
         return Result;
      elsif not Context_Named (D, View) then
         Result.Cause := Q.Bad_Context;
         return Result;
      elsif not View (D.Context).Taking then
         -- The record's detail is the context's own fault reason.
         Result.Kind := Refuse_Faulted;
         Result.Cause := Q.None;
         return Result;
      elsif View (D.Context).Accepted = Value'Last or else
        Value (D.Signal_Value) /= View (D.Context).Accepted + 1
      then
         Result.Cause := Q.Signal_Mismatch;
         return Result;
      elsif not Wait_Valid (D.Wait_1_Context, D.Wait_1_Value, View) or else
        not Wait_Valid (D.Wait_2_Context, D.Wait_2_Value, View)
      then
         Result.Cause := Q.Bad_Wait;
         return Result;
      elsif not Batch_Valid (D) then
         Result.Cause := Q.Bad_Batch;
         return Result;
      end if;
      pragma Assert (Well_Formed (D, View));
      Result.Cause := Q.None;
      if Now >= Microseconds (D.Deadline) then
         Result.Kind := Expire;
         Result.Cause := Q.Deadline;
      elsif Wait_Lost (D.Wait_1_Context, D.Wait_1_Value, View) or else
        Wait_Lost (D.Wait_2_Context, D.Wait_2_Value, View)
      then
         Result.Kind := Refuse_Lost;
         Result.Cause := Q.Device_Fault;
      elsif not Wait_Reached (D.Wait_1_Context, D.Wait_1_Value, View) or else
        not Wait_Reached (D.Wait_2_Context, D.Wait_2_Value, View)
      then
         Result.Kind := Await_Waits;
      elsif View (D.Context).Owed >= Capacity then
         Result.Kind := Await_Capacity;
      elsif Quiescing then
         Result.Kind := Await_Quiesce;
      else
         Result.Kind := Admit;
      end if;
      return Result;
   end Decide;

end Intel_GPU_Queue_Admission;
