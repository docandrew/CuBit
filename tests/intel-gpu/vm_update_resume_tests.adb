with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Update;
procedure VM_Update_Resume_Tests is
begin
   for Fault in 0 .. 5 loop
      declare
         Live : Boolean := True;
         Calls, Resumes, Invalidations : Natural := 0;
         function Owner return Boolean is (Live);
         procedure Simple (OK : out Boolean) is
         begin Calls := Calls + 1; OK := True; end Simple;
         procedure Invalidate (OK : out Boolean) is
         begin
            Calls := Calls + 1; Invalidations := Invalidations + 1;
            OK := Fault /= 5;
         end Invalidate;
         procedure No_Resume (OK : out Boolean) is
         begin pragma Assert (False); OK := False; end No_Resume;
         package VM is new Intel_GPU_VM_Update
           (Owner, Simple, Simple, Invalidate, No_Resume);
         use type VM.Result, VM.Phase;
         Object : VM.State;
         procedure Probe;
         procedure Publish (Finished, OK : out Boolean) is
         begin Simple (OK); Finished := True; end Publish;
         procedure Resume (Finished, OK : out Boolean) is
         begin
            Calls := Calls + 1; Resumes := Resumes + 1;
            pragma Assert (Invalidations = 1);
            Probe;
            Finished := Resumes = 9;
            OK := not (Fault = 1 and Resumes = 4);
            if Fault = 3 and Resumes = 4 then Live := False; end if;
            if Fault = 4 and Resumes = 4 then VM.Fail (Object); end if;
         end Resume;
         procedure Advance is new VM.Advance (Publish, Resume);
         procedure Probe is
            Done, Accepted : Boolean;
            Status : VM.Result;
            Before : constant Natural := Calls;
         begin
            pragma Assert (not VM.Can_Submit (Object) and VM.Generation (Object) = 0);
            Advance (Object, Done, Status);
            pragma Assert (Done and Status = VM.Rejected and Calls = Before);
            VM.Begin_Update (Object, 0, Accepted, Status);
            pragma Assert (not Accepted);
         end Probe;
         Accepted, Done : Boolean;
         Status : VM.Result;
         Before, Turns : Natural := 0;
      begin
         VM.Begin_Update (Object, 0, Accepted, Status); pragma Assert (Accepted);
         loop
            pragma Assert (not VM.Can_Submit (Object) and VM.Generation (Object) = 0);
            Before := Calls; Turns := Turns + 1; pragma Assert (Turns <= 12);
            Advance (Object, Done, Status);
            pragma Assert (Calls <= Before + 1);
            exit when Done;
            if Fault = 2 and Resumes = 4 then Live := False; end if;
         end loop;
         if Fault = 0 then
            pragma Assert (Status = VM.Complete and Resumes = 9);
            pragma Assert (VM.Can_Submit (Object) and VM.Generation (Object) = 1);
         else
            pragma Assert (VM.Current_Phase (Object) = VM.Quarantined);
            pragma Assert (not VM.Can_Submit (Object) and VM.Generation (Object) = 0);
            pragma Assert (Resumes = (if Fault = 5 then 0 else 4));
            Live := True; Before := Calls;
            Advance (Object, Done, Status);
            pragma Assert (Done and Status = VM.Rejected and Calls = Before);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("resumable VM finalization PASS6: one callback/turn, closed submission, final epoch, reentry and sticky failures (mock GPU)");
end VM_Update_Resume_Tests;
