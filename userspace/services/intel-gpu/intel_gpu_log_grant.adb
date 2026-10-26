------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Intel_GPU_Log_Grant with SPARK_Mode is

   function Plus (Now, Interval : Milliseconds) return Milliseconds is
     (if Now <= Milliseconds'Last - Interval then Now + Interval
      else Milliseconds'Last)
   with Post => Plus'Result >= Now;

   function Bump (Count : Event_Count) return Event_Count is
     (if Count < Event_Count'Last then Count + 1 else Count);

   procedure Not_Registered (S : in out State; Now : Milliseconds) is
   begin
      S.Next_Request := Plus (Now, Retry_Interval (S));
   end Not_Registered;

   procedure Submitted (S : in out State; Now : Milliseconds; Accepted : Boolean) is
   begin
      S.Requests := Bump (S.Requests);
      if Accepted then
         S.Current := Requested;
      else
         S.Submit_Failures := Bump (S.Submit_Failures);
         S.Next_Request := Plus (Now, Retry_Interval (S));
      end if;
   end Submitted;

   procedure Answered (S : in out State; Now : Milliseconds; Kind : Reply_Kind) is
   begin
      case Kind is
         when Granted =>
            S.Current := Attached;
            S.Attached_At := Now;
            S.Report_Due := True;
            return;
         when Busy      => S.Busy_Replies := Bump (S.Busy_Replies);
         when Refused   => S.Refusals := Bump (S.Refusals);
         when Malformed => S.Malformed_Replies := Bump (S.Malformed_Replies);
      end case;
      S.Current := Idle;
      S.Next_Request := Plus (Now, Retry_Interval (S));
   end Answered;

   procedure Observe_Time (S : in out State; Now : Milliseconds) is
   begin
      if not S.Deadline_Passed and then S.Current /= Attached
        and then Elapsed (S, Now) >= Availability_Deadline_Ms
      then
         S.Deadline_Passed := True;
         S.Notice_Due := True;
      end if;
   end Observe_Time;

   procedure Take_Notice (S : in out State; Due : out Boolean) is
   begin
      Due := S.Notice_Due;
      S.Notice_Due := False;
   end Take_Notice;

   procedure Take_Report (S : in out State; Due : out Boolean) is
   begin
      Due := S.Report_Due;
      S.Report_Due := False;
   end Take_Report;

end Intel_GPU_Log_Grant;
