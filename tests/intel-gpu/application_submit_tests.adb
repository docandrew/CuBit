with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Application_Submit;
with Intel_GPU_Image_Consumers;
with Intel_GPU_Image_Lease;
procedure Application_Submit_Tests is
   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;
   Owned, Mapped : Boolean := True;
   Lose_During_Batch : Boolean := False;
   Batch_Calls : Natural := 0;
   Expected_GPU : Unsigned_64 := 16#20000#;
   Expected_Offset : Unsigned_64 := 0;
   Expected_Bytes : Unsigned_64 := 4096;
   Step, Failure, Lose_At, Quarantines : Natural := 0;
   Previous : Unsigned_32 := 1;
   Nested_Enabled, Nested_Active : Boolean := False;
   Nested_Attempts : Natural := 0;
   procedure Try_Nested;
   type Completion_Attempt is limited record
      Armed : Boolean := False;
      Expected : Unsigned_32 := 0;
   end record;
   function Ready return Boolean is
   begin
      Try_Nested;
      return Owned;
   end Ready;
   function Batch (Handle, GPU, Offset, Bytes : Unsigned_64) return Boolean is
   begin
      Batch_Calls := Batch_Calls + 1;
      Try_Nested;
      pragma Assert (Handle = 1 and GPU = Expected_GPU and
                     Offset = Expected_Offset and Bytes = Expected_Bytes);
      if Lose_During_Batch then Owned := False; end if;
      return Mapped;
   end Batch;
   procedure Advance (Expected : Positive; OK : out Boolean) is
   begin
      Step := Step + 1;
      Try_Nested;
      pragma Assert (Step = Expected);
      if Step = Lose_At then Owned := False; end if;
      OK := Step /= Failure;
   end Advance;
   procedure Arm (Attempt : in out Completion_Attempt;
                  Before, After : Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (not Attempt.Armed);
      Attempt.Armed := True;
      Attempt.Expected := After;
      pragma Assert (Before = Previous and After = Previous + 1);
      Advance (1, OK);
   end Arm;
   procedure Enable (OK : out Boolean) is begin Advance (2, OK); end Enable;
   procedure Publish (GPU : Unsigned_64; Sequence : Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (GPU = 16#20000# and Sequence = Previous + 1);
      Advance (3, OK);
   end Publish;
   procedure Notify (OK : out Boolean) is begin Advance (4, OK); end Notify;
   procedure Wait_GPU (Attempt : in out Completion_Attempt; OK : out Boolean) is
   begin
      pragma Assert (Attempt.Armed and Attempt.Expected = Previous + 1);
      Advance (5, OK);
   end Wait_GPU;
   procedure Disable (OK : out Boolean) is begin Advance (6, OK); end Disable;
   procedure Quarantine is begin Quarantines := Quarantines + 1; end Quarantine;
   package S is new Intel_GPU_Application_Submit
     (Ready, Batch, Completion_Attempt, Arm, Enable, Publish, Notify, Wait_GPU, Disable, Quarantine);
   use type S.Phase;
   use type S.Result;
   Status : S.Result;
   Completion : Unsigned_32;
   Nested_Object : S.State;
   procedure Try_Nested is
      Result : S.Result;
      Value : Unsigned_32;
   begin
      if not Nested_Enabled or Nested_Active then return; end if;
      Nested_Active := True;
      pragma Assert (not S.Completion_Confirmed (Nested_Object, 2));
      Nested_Attempts := Nested_Attempts + 1;
      S.Execute (Nested_Object, 1, 16#20000#, 0, 4096, Result, Value);
      pragma Assert (Result = S.Rejected and Value = 0);
      Nested_Active := False;
   end Try_Nested;
begin
   for Mode in 0 .. 2 loop
      for Point in 1 .. 6 loop
         declare
            Object : S.State;
         begin
            Owned := True; Mapped := True; Step := 0; Quarantines := 0; Previous := 1;
            Failure := (if Mode = 1 then Point else 0);
            Lose_At := (if Mode = 2 then Point else 0);
            S.Execute (Object, 1, 16#20000#, 0, 4096, Status, Completion);
            pragma Assert (Status = S.Rejected and Step = 0 and Completion = 0);
            pragma Assert (not S.Completion_Confirmed (Object, 2));
            S.Initialize (Object, True);
            pragma Assert (not S.Completion_Confirmed (Object, 1));
            S.Execute (Object, 1, 16#20000#, 0, 4096, Status, Completion);
            if Mode = 0 then
               pragma Assert (Status = S.Complete and Completion = 2 and Step = 6);
               for Iteration in 3 .. 20 loop
                  Step := 0; Previous := Unsigned_32 (Iteration - 1);
                  S.Execute (Object, 1, 16#20000#, 0, 4096, Status, Completion);
                  pragma Assert (Status = S.Complete and Completion = Unsigned_32 (Iteration));
                  pragma Assert (S.Completion_Confirmed (Object, 2));
                  pragma Assert (S.Completion_Confirmed (Object, Completion));
                  pragma Assert (not S.Completion_Confirmed (Object, Completion + 1));
               end loop;
            else
               pragma Assert (Status = S.Faulted and Completion = 0 and Step = Point);
               pragma Assert (S.Current (Object) = S.Failed and S.Last_Completed (Object) = 1);
               pragma Assert (not S.Completion_Confirmed (Object, 2));
               pragma Assert (Quarantines = 1);
               Owned := True;
               S.Initialize (Object, True);
               S.Execute (Object, 1, 16#20000#, 0, 4096, Status, Completion);
               pragma Assert (Status = S.Rejected and Step = Point and Quarantines = 1);
            end if;
         end;
      end loop;
   end loop;
   declare
      Object : S.State;
   begin
      Owned := True; Step := 0; Quarantines := 0;
      S.Initialize (Object, True);
      for GPU of Unsigned_64_Array'(0, 1, 2 ** 48, Unsigned_64'Last) loop
         S.Execute (Object, 1, GPU, 0, 4096, Status, Completion);
         pragma Assert (Status = S.Rejected and Step = 0 and Completion = 0);
      end loop;
      for Bytes of Unsigned_64_Array'(0, 1, 16 * 1024 * 1024 + 4, Unsigned_64'Last) loop
         S.Execute (Object, 1, 16#20000#, 0, Bytes, Status, Completion);
         pragma Assert (Status = S.Rejected and Step = 0 and Completion = 0);
      end loop;
      for Offset of Unsigned_64_Array'(1, 16 * 1024 * 1024, Unsigned_64'Last - 7) loop
         S.Execute (Object, 1, 16#20000#, Offset, 4096, Status, Completion);
         pragma Assert (Status = S.Rejected and Step = 0 and Completion = 0);
      end loop;
      for Handle of Unsigned_64_Array'(0, 2 ** 32, Unsigned_64'Last) loop
         S.Execute (Object, Handle, 16#20000#, 0, 4096, Status, Completion);
         pragma Assert (Status = S.Rejected and Step = 0 and Completion = 0);
      end loop;
      Mapped := False;
      S.Execute (Object, 1, 16#20000#, 0, 4096, Status, Completion);
      pragma Assert (Status = S.Batch_Denied and Step = 0 and S.Current (Object) = S.Idle);
      Owned := False;
      S.Execute (Object, 1, 16#20000#, 0, 4096, Status, Completion);
      pragma Assert (Status = S.Faulted and Quarantines = 1);
   end;
   declare
      Object : S.State;
   begin
      Owned := True; Quarantines := 0;
      S.Initialize (Object, False);
      S.Initialize (Object, True);
      pragma Assert (S.Current (Object) = S.Failed and Quarantines = 1);
   end;
   -- A BO lookup can lose ownership whether it returns success or denial.
   -- Neither outcome permits even arming the completion record afterwards.
   for Mapping_Result in Boolean loop
      declare
         Object : S.State;
      begin
         Owned := True; Mapped := Mapping_Result; Lose_During_Batch := True;
         Step := 0; Quarantines := 0; Batch_Calls := 0;
         S.Initialize (Object, True);
         S.Execute (Object, 1, 16#20000#, 0, 4096, Status, Completion);
         pragma Assert (Status = S.Faulted and Completion = 0 and Step = 0);
         pragma Assert (Batch_Calls = 1 and Quarantines = 1);
         pragma Assert (S.Current (Object) = S.Failed and S.Last_Completed (Object) = 1);
      end;
   end loop;
   Lose_During_Batch := False;
   -- Half-open extents may end exactly at the limit, but must not cross it.
   -- Deny the mapping deliberately: these cases test preflight, not execution.
   declare
      Object : S.State;
   begin
      Owned := True; Mapped := False; Step := 0; Quarantines := 0;
      S.Initialize (Object, True);
      Expected_GPU := 2 ** 48 - 8; Expected_Bytes := 8;
      Batch_Calls := 0;
      S.Execute (Object, 1, Expected_GPU, 0, 8, Status, Completion);
      pragma Assert (Status = S.Batch_Denied and Batch_Calls = 1);
      S.Execute (Object, 1, Expected_GPU, 0, 12, Status, Completion);
      pragma Assert (Status = S.Rejected and Batch_Calls = 1);
      Expected_GPU := 16#20000#;
      Expected_Bytes := 16 * 1024 * 1024;
      S.Execute (Object, 1, Expected_GPU, 0, Expected_Bytes, Status, Completion);
      pragma Assert (Status = S.Batch_Denied and Batch_Calls = 2);
      Expected_Bytes := 8; Expected_Offset := 16 * 1024 * 1024 - 8;
      S.Execute (Object, 1, Expected_GPU, Expected_Offset, 8, Status, Completion);
      pragma Assert (Status = S.Batch_Denied and Batch_Calls = 3);
      S.Execute (Object, 1, Expected_GPU, Expected_Offset, 12, Status, Completion);
      pragma Assert (Status = S.Rejected and Batch_Calls = 3);
      pragma Assert (Step = 0 and Quarantines = 0 and Completion = 0);
      pragma Assert (S.Current (Object) = S.Idle and S.Last_Completed (Object) = 1);
   end;
   Owned := True; Mapped := True; Step := 0; Quarantines := 0;
   Failure := 0; Lose_At := 0; Previous := 1;
   Expected_GPU := 16#20000#; Expected_Offset := 0; Expected_Bytes := 4096;
   S.Initialize (Nested_Object, True);
   Nested_Enabled := True;
   S.Execute (Nested_Object, 1, 16#20000#, 0, 4096, Status, Completion);
   pragma Assert (Status = S.Complete and Completion = 2 and Nested_Attempts = 15);
   pragma Assert (Step = 6 and Quarantines = 0 and S.Current (Nested_Object) = S.Idle);
   pragma Assert (S.Completion_Confirmed (Nested_Object, 2));
   pragma Assert (not S.Completion_Confirmed (Nested_Object, 0));
   pragma Assert (not S.Completion_Confirmed (Nested_Object, 1));
   pragma Assert (not S.Completion_Confirmed (Nested_Object, 3));
   Ada.Text_IO.Put_Line ("Application submission ordering PASS: failures quarantine; no premature completion or nested admission");
   Nested_Enabled := False;
   declare
      A, B, Faulted : S.State;
      Receipt, Other, Failed_Receipt : S.Completion_Receipt;
      package C renames Intel_GPU_Image_Consumers;
      Consumers : C.Ledger;
      Read_Obligation : C.Obligation;
      Key : constant Intel_GPU_Image_Lease.Identity := (1, 2, 3, 4, 5, 6, 0, 7);
      Accepted : Boolean;
   begin
      Previous := 1; Step := 0; Failure := 0; Lose_At := 0;
      S.Initialize (A, True);
      S.Initialize (B, True);
      S.Initialize (Faulted, True);
      pragma Assert (not S.Receipt_Confirmed (A, Receipt));
      C.Open (Consumers, Key, Accepted); pragma Assert (Accepted);
      C.Reserve (Consumers, Key, C.GPU, Read_Obligation, Accepted); pragma Assert (Accepted);
      S.Execute_With_Receipt (A, 1, 16#20000#, 0, 4096, Receipt, Status);
      pragma Assert (Status = S.Complete and S.Receipt_Confirmed (A, Receipt));
      pragma Assert (S.Receipt_Sequence (A, Receipt) = 2);
      Step := 0;
      S.Execute_With_Receipt (B, 1, 16#20000#, 0, 4096, Other, Status);
      pragma Assert (Status = S.Complete and S.Receipt_Confirmed (B, Other));
      pragma Assert (not S.Receipt_Confirmed (B, Receipt));
      pragma Assert (S.Receipt_Sequence (B, Receipt) = 0);
      pragma Assert (not S.Receipt_Confirmed (A, Other));
      C.Stop (Consumers, Key, Accepted); pragma Assert (Accepted);
      C.Complete (Consumers, Key, Read_Obligation, S.Receipt_Confirmed (A, Other), Accepted);
      pragma Assert (not Accepted and not C.Drained (Consumers, Key));
      C.Complete (Consumers, Key, Read_Obligation, S.Receipt_Confirmed (A, Receipt), Accepted);
      pragma Assert (Accepted and C.Drained (Consumers, Key));
      Step := 0;
      S.Execute_With_Receipt (A, 1, 16#20000#, 0, 4096, Receipt, Status);
      pragma Assert (Status = S.Rejected and Step = 0);
      Failure := 6; Step := 0;
      S.Execute_With_Receipt (Faulted, 1, 16#20000#, 0, 4096, Failed_Receipt, Status);
      pragma Assert (Status = S.Faulted and not S.Receipt_Confirmed (Faulted, Failed_Receipt));
   end;
   Ada.Text_IO.Put_Line ("Submission receipt PASS: exact root, equal-sequence foreign rejection, single attempt, disable failure retains uncertainty");
end Application_Submit_Tests;
