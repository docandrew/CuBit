pragma Ada_2022;

package body Devmgr_Phase_Declarations with SPARK_Mode is

   function Decide
     (Known : Declaration_Array; Approved : Boolean;
      Owner, Request, Epoch, Status_Word : Unsigned_64) return Verdict
   is
      Result : Verdict;
   begin
      --  The declared status, for the caller's log line even when refused.
      for S in CX.Phase_Status loop
         if CX.Phase_Status'Enum_Rep (S) = Status_Word then
            Result.Status := S;
         end if;
      end loop;
      if not Approved then
         Result.Answer := CX.Not_Approved;
         return Result;
      elsif Request = 0 or else Epoch = No_Epoch or else not CX.Valid_Status (Status_Word) then
         Result.Answer := CX.Malformed;
         return Result;
      end if;
      Result.Answer := CX.Received;
      --  The first declaration for this (sender, epoch) stands.
      for K in Known'Range loop
         pragma Loop_Invariant
           (Result.Answer = CX.Received and then not Result.Record_It and then not Result.Release
            and then (for all J in Known'First .. K - 1 =>
                        not (Known (J).Owner = Owner and then Known (J).Epoch = Epoch)));
         if Known (K).Owner = Owner and then Known (K).Epoch = Epoch then
            if Known (K).Status /= Result.Status then
               Result.Answer := CX.Conflicting;
               Result.Conflict := True;
               Result.Earlier := Known (K).Status;
            end if;
            return Result;
         end if;
      end loop;
      Result.Record_It := True;
      Result.Release := CX.Ran_To_End (Result.Status);
      return Result;
   end Decide;

end Devmgr_Phase_Declarations;
