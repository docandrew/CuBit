with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Update;
procedure VM_Update_Async_Tests is
begin
   for Fault in 0 .. 6 loop
      declare
         Live : Boolean := True;
         Calls, Publications, Invalidations, Resumes : Natural := 0;
         function Owner return Boolean is (Live);
         procedure Probe;
         procedure Drain (OK : out Boolean) is
         begin Calls := Calls + 1; Probe; OK := Fault /= 6; end Drain;
         procedure Publish (OK : out Boolean) is
         begin pragma Assert (False); OK := False; end Publish;
         procedure Invalidate (OK : out Boolean) is
         begin
            Calls := Calls + 1; Invalidations := Invalidations + 1;
            Probe; OK := Fault /= 2;
         end Invalidate;
         procedure Resume (OK : out Boolean) is
         begin
            Calls := Calls + 1; Resumes := Resumes + 1;
            Probe; OK := Fault /= 3;
         end Resume;
         package VM is new Intel_GPU_VM_Update (Owner, Drain, Publish, Invalidate, Resume);
         use type VM.Result;
         use type VM.Phase;
         Object : VM.State;
         procedure Publication (Finished, OK : out Boolean) is
         begin
            Calls := Calls + 1; Publications := Publications + 1; Probe;
            Finished := Publications = 12;
            OK := Fault /= 1 or Publications /= 7;
            if Fault = 5 and Publications = 7 then VM.Fail (Object); end if;
         end Publication;
         procedure Advance is new VM.Advance (Publication);
         procedure Probe is
            Done, Accepted : Boolean;
            Result : VM.Result;
            Before : constant Natural := Calls;
         begin
            pragma Assert (not VM.Can_Submit (Object));
            Advance (Object, Done, Result);
            pragma Assert (Done and Result = VM.Rejected and Calls = Before);
            VM.Begin_Update (Object, 0, Accepted, Result);
            pragma Assert (not Accepted and Result = VM.Rejected);
            VM.Execute (Object, 0, Result); pragma Assert (Result = VM.Rejected);
         end Probe;
         Accepted, Finished : Boolean;
         Status : VM.Result;
         Before, Turns : Natural := 0;
      begin
         VM.Begin_Update (Object, 1, Accepted, Status);
         pragma Assert (not Accepted and VM.Current_Phase (Object) = VM.Idle);
         VM.Begin_Update (Object, 0, Accepted, Status); pragma Assert (Accepted);
         loop
            pragma Assert (not VM.Can_Submit (Object) and VM.Generation (Object) = 0);
            Before := Calls; Turns := Turns + 1; pragma Assert (Turns <= 16);
            Advance (Object, Finished, Status);
            pragma Assert (Calls - Before <= 1);
            exit when Finished;
            if Fault = 4 and Publications = 7 then Live := False; end if;
         end loop;
         pragma Assert ((Status = VM.Complete) = (Fault = 0));
         if Fault = 0 then
            pragma Assert (Publications = 12 and Invalidations = 1 and Resumes = 1);
            pragma Assert (VM.Can_Submit (Object) and VM.Generation (Object) = 1);
         else
            pragma Assert (VM.Current_Phase (Object) = VM.Quarantined);
            pragma Assert (not VM.Can_Submit (Object) and VM.Generation (Object) = 0);
            Before := Calls;
            Live := True;
            Advance (Object, Finished, Status);
            pragma Assert (Finished and Status = VM.Rejected and Calls = Before);
            VM.Begin_Update (Object, 0, Accepted, Status); pragma Assert (not Accepted);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("async VM updates PASS7: bounded publication yields, closed admission, one epoch, reentry rejection, terminal failures");
end VM_Update_Async_Tests;
