with Ada.Text_IO;
with CCL.Types;
with CCL.Objects;
with Config_Objects; use Config_Objects;

procedure Durable_Tests is
   use type Number;
   use type CCL.Objects.Image;
   use type CCL.Objects.Build_Result;
   Types : CCL.Types.Registry;
   Contract : CCL.Objects.Binding;
   Schema : constant CCL.Objects.Schema_Key := [1, 2, 3, 4];
   Old_Value, New_Value, Bad_Value, Output : CCL.Objects.Image;
   Built : CCL.Objects.Build_Result;
   Valid : Boolean;
   Count : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Count := Count + 1;
      if not Condition then raise Program_Error with "durable check" & Count'Image; end if;
   end Check;
   procedure Read_Check (S : State; Expected : Read_Result; Rev : Number; Value : CCL.Objects.Image) is
      R : Read_Result;
      N : Number;
      V : CCL.Objects.Image;
   begin
      Read (S, Schema, V, N, R);
      Check (R = Expected and N = Rev);
      if Expected in Found | Stale then Check (V = Value); end if;
   end Read_Check;
   procedure Start (S : in out State; With_Value : Boolean := True; Rev : Number := 1) is
      R : Outcome;
      OK : Boolean;
   begin
      Initialize (S, Contract, OK); Check (OK);
      Attach (S, 10, R); Check (R = Accepted);
      Begin_Load (S, 1, R); Check (R = Accepted);
      Finish_Load (S, 10, 1, (if With_Value then Loaded else Absent),
                   (if With_Value then Rev else 0), Old_Value, R);
      Check (R = (if With_Value then Published else Accepted));
   end Start;
begin
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, Schema, Contract, Valid); Check (Valid);
   Old_Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Old_Value, CCL.Objects.Integer_Cell (41), Built); Check (Built = CCL.Objects.Added);
   New_Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (New_Value, CCL.Objects.Integer_Cell (42), Built); Check (Built = CCL.Objects.Added);
   Bad_Value := New_Value; Bad_Value.Cells (2).First := 99;

   -- Every completion outcome and plausible revision. An authenticated reply
   -- is not sufficient if its operation, session, token or revision disagrees.
   for Completion in Commit_Outcome loop
      for Saved in Number range 0 .. 4 loop
         declare
            S : State;
            R : Outcome;
            Expected : Number;
            Available : Boolean;
         begin
            Start (S);
            Begin_Commit (S, New_Value, 0, 2, R); Check (R = Revision_Conflict);
            Begin_Commit (S, Bad_Value, 1, 2, R); Check (R = Invalid_Value);
            Begin_Commit (S, New_Value, 1, 2, R); Check (R = Accepted);
            Read_Check (S, Found, 1, Old_Value);
            Export_Pending (S, Output, Expected, Available);
            Check (Available and Output = New_Value and Expected = 1);
            Output.Cells (1).First := 123; -- exported copy cannot mutate candidate
            Begin_Commit (S, Old_Value, 1, 3, R); Check (R = Busy);
            Attach (S, 11, R); Check (R = Busy);
            Finish_Load (S, 10, 2, Loaded, 2, New_Value, R); Check (R = Ignored);
            Finish_Commit (S, 9, 2, Completion, Saved, R); Check (R = Ignored);
            Finish_Commit (S, 10, 1, Completion, Saved, R); Check (R = Ignored);
            Finish_Commit (S, 10, 3, Completion, Saved, R); Check (R = Ignored);
            Check (Status (S) = Committing and Pending_Request (S) = 2);
            Finish_Commit (S, 10, 2, Completion, Saved, R);
            if Completion = Committed and Saved = 2 then
               Check (R = Published and Status (S) = Ready);
               Read_Check (S, Found, 2, New_Value);
            elsif Completion = Definitely_Rejected and Saved = 1 then
               Check (R = Rejected and Status (S) = Ready);
               Read_Check (S, Found, 1, Old_Value);
            else
               Check (R = Needs_Recovery and Status (S) = Recovery_Required);
               Read_Check (S, Stale, 1, Old_Value);
            end if;
            Check (Pending_Request (S) = 0);
            Finish_Commit (S, 10, 2, Committed, 2, R); Check (R = Ignored);
            Export_Pending (S, Output, Expected, Available); Check (not Available);
         end;
      end loop;
   end loop;

   -- Commit actually happened but its acknowledgement was lost. Do not retry
   -- or call the old cache fresh; load from a newly authorized worker session.
   declare
      S : State;
      R : Outcome;
      OK : Boolean;
   begin
      Start (S);
      Begin_Commit (S, New_Value, 1, 2, R); Check (R = Accepted);
      Lose_Worker (S, 9); Check (Status (S) = Committing);
      Lose_Worker (S, 10); Check (Status (S) = Recovery_Required);
      Read_Check (S, Stale, 1, Old_Value);
      Begin_Commit (S, New_Value, 1, 3, R); Check (R = Needs_Recovery);
      Attach (S, 10, R); Check (R = Invalid_Request);
      Attach (S, 11, R); Check (R = Accepted);
      Initialize (S, Contract, OK); Check (not OK);
      Begin_Load (S, 2, R); Check (R = Invalid_Request);
      Begin_Load (S, 3, R); Check (R = Accepted);
      Finish_Load (S, 10, 3, Loaded, 2, New_Value, R); Check (R = Ignored);
      Finish_Commit (S, 10, 2, Committed, 2, R); Check (R = Ignored);
      Finish_Load (S, 11, 3, Loaded, 2, New_Value, R); Check (R = Published);
      Read_Check (S, Found, 2, New_Value);
      Begin_Commit (S, Old_Value, 2, 3, R); Check (R = Invalid_Request);
      Begin_Commit (S, Old_Value, 2, 4, R); Check (R = Accepted);
      Finish_Commit (S, 11, 4, Committed, 3, R); Check (R = Published);
      Read_Check (S, Found, 3, Old_Value);
   end;

   -- Hostile recovery: absent previously-published data, older revisions,
   -- inconsistent contents at the same revision, malformed/schema-wrong data.
   for Case_Number in 1 .. 8 loop
      declare
         S : State;
         R : Outcome;
         V : CCL.Objects.Image := Old_Value;
         Saved : Number := 2;
         Completion : Load_Outcome := Loaded;
      begin
         Start (S, Rev => 2);
         Lose_Worker (S, 10); Attach (S, 11, R); Check (R = Accepted);
         Begin_Load (S, 2, R); Check (R = Accepted);
         case Case_Number is
            when 1 => Saved := 0; Completion := Absent;
            when 2 => Saved := 1;
            when 3 => V := New_Value;
            when 4 => V := Bad_Value;
            when 5 => V.Schema (1) := 99;
            when 6 => Saved := Maximum_Revision + 1;
            when 7 => Saved := Number'Last;
            when others => Completion := Failed;
         end case;
         Finish_Load (S, 11, 2, Completion, Saved, V, R); Check (R = Needs_Recovery);
         Read_Check (S, Stale, 2, Old_Value);
      end;
   end loop;
   declare
      S, Empty_Store, Fresh : State;
      R : Outcome;
      OK : Boolean;
   begin
      Read_Check (Fresh, Unavailable, 0, Old_Value);
      Attach (Fresh, 1, R); Check (R = Not_Bound);
      Begin_Load (Fresh, 1, R); Check (R = Not_Bound);
      Initialize (Fresh, Contract, OK); Check (OK);
      Attach (Fresh, 0, R); Check (R = Invalid_Request);
      Begin_Commit (Fresh, New_Value, 0, 1, R); Check (R = Needs_Recovery);
      Start (S, Rev => Maximum_Revision);
      Begin_Commit (S, New_Value, Maximum_Revision, 2, R); Check (R = Revision_Exhausted);
      Start (Empty_Store, False);
      Read_Check (Empty_Store, Missing, 0, Old_Value);
      Begin_Commit (Empty_Store, New_Value, 0, 0, R); Check (R = Invalid_Request);
      Begin_Commit (Empty_Store, New_Value, 0, Number'Last, R); Check (R = Invalid_Request);
      Begin_Commit (Empty_Store, New_Value, 0, Number'Last - 1, R); Check (R = Accepted);
      Finish_Commit (Empty_Store, 10, Number'Last - 1, Definitely_Rejected, 0, R); Check (R = Rejected);
      Begin_Commit (Empty_Store, New_Value, 0, Number'Last - 1, R); Check (R = Invalid_Request);
      Begin_Commit (Empty_Store, New_Value, 0, 1, R); Check (R = Invalid_Request);
      Begin_Commit (Empty_Store, New_Value, 0, Number'Last, R); Check (R = Invalid_Request);
   end;
   Ada.Text_IO.Put_Line ("Typed Config durable publication: PASS" & Count'Image & " checks");
end Durable_Tests;
