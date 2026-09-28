with Ada.Text_IO;
with Config_Activation;
with Config_Authority;
with Config_Authority_Wire;
with CCL.Declarations;

procedure Activation_Tests is
   package C renames Config_Activation;
   package A renames Config_Authority;
   use type C.Number;
   use type C.Phase;
   use type C.Result;
   use type C.Review_View;
   use type A.Install_Result;
   Authority : A.Authority_State;
   Object : C.State;
   Request, Invalid_Request : C.Commit_Request;
   View, Original : C.Review_View;
   Status : C.Result;
   ID, Old_ID : C.Number;
   Allowed : Boolean;
   Checks : Natural := 0;
   Name : constant String := "desktop.appearance.v1";
   Source : constant String :=
     "(system-config v1 (setting ""desktop.appearance.v1"" (concat ""1"" ""00"")))";
   Review_Rights : constant A.Rights :=
     [A.Read_Config | A.Activate_Config => True, A.Write_Config => False];

   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "activation check" & Checks'Image; end if;
   end Check;

   procedure Install (Subject : A.Subject_ID; Scope : String; Rights : A.Rights) is
      Rules : A.Rule_Set;
      Added : Boolean;
      Result : A.Install_Result;
   begin
      A.Append (Rules, Scope, Rights, Added); Check (Added);
      A.Install (Authority, Subject, Rules, Result); Check (Result = A.Installed);
   end Install;

   procedure Propose (Base : C.Number := 7) is
   begin
      C.Propose (Object, Name, Source, Base, ID, Status);
      Check (Status = C.Accepted and ID /= 0 and C.Current_Phase (Object) = C.Candidate);
   end Propose;

   procedure Approve is
   begin
      C.Approve (Object, Authority, 42, ID, Status);
      Check (Status = C.Accepted and C.Current_Phase (Object) = C.Reviewed);
   end Approve;

   procedure Begin_Commit (Base : C.Number := 7) is
   begin
      C.Begin_Commit (Object, Authority, 42, ID, Base, 123, Request, Status);
      Check (Status = C.Accepted and C.Valid (Request));
      Check (C.Current_Phase (Object) = C.Committing);
      Check (C.Snapshot (Request).ID = ID and C.Snapshot (Request).Base = Base);
      Check (C.Reviewer (Request) = 42 and
             C.Grant_Revision (Request) = A.Revision (Authority, 42));
   end Begin_Commit;
begin
   Check (not C.Valid (Request));
   Check (C.Current_Phase (Object) = C.Empty);
   C.Inspect (Object, Authority, 42, View, Allowed);
   Check (not Allowed and View.ID = 0 and View.Source.Length = 0);
   C.Begin_Commit (Object, Authority, 42, 0, 7, 123, Request, Status);
   Check (Status = C.Wrong_Phase and not C.Valid (Request));

   -- A legacy mask3 grant, including wildcard scope, cannot authorize activation.
   declare
      Wire : String (1 .. Config_Authority_Wire.Entry_Bytes) := [others => Character'Val (0)];
      Rules : A.Rule_Set;
      Result : A.Install_Result;
   begin
      Wire (1) := Character'Val (3);
      Config_Authority_Wire.Decode (Wire, Rules, Allowed); Check (Allowed);
      A.Install (Authority, 42, Rules, Result); Check (Result = A.Installed);
      Check (A.Allows (Authority, 42, Name, A.Write_Config));
      Check (not A.Allows (Authority, 42, Name, A.Activate_Config));
      Wire (1) := Character'Val (4);
      Config_Authority_Wire.Decode (Wire, Rules, Allowed); Check (not Allowed);
   end;
   Propose;
   C.Approve (Object, Authority, 42, ID, Status); Check (Status = C.Denied);
   Install (42, "", A.Read_Write);
   C.Approve (Object, Authority, 42, ID, Status); Check (Status = C.Denied);
   Install (42, "desktop.other", Review_Rights);
   C.Approve (Object, Authority, 42, ID, Status); Check (Status = C.Denied);
   Install (42, "desktop.appearance", [A.Activate_Config => True, others => False]);
   C.Approve (Object, Authority, 42, ID, Status); Check (Status = C.Denied);
   Install (42, "desktop.appearance", Review_Rights);
   C.Inspect (Object, Authority, 42, View, Allowed); Check (Allowed);
   Check (View.Source.Data (1 .. View.Source.Length) = Source);
   Check (View.Setting.Value.Data (1 .. View.Setting.Value.Length) = "100");
   Original := View;
   View.Source.Data (1) := 'X'; -- UI edits cannot mutate the owned proposal.
   C.Inspect (Object, Authority, 42, View, Allowed);
   Check (Allowed and View = Original);
   C.Inspect (Object, Authority, 99, View, Allowed);
   Check (not Allowed and View.Source.Length = 0 and View.Setting.Value.Length = 0);
   Approve;
   C.Begin_Commit (Object, Authority, 99, ID, 7, 123, Request, Status);
   Check (Status = C.Denied and not C.Valid (Request));
   Check (C.Current_Phase (Object) = C.Reviewed);
   C.Begin_Commit (Object, Authority, 42, ID, 7, 0, Request, Status);
   Check (Status = C.Denied and not C.Valid (Request));

   -- Replacement of an otherwise identical grant set invalidates the review.
   Install (42, "desktop.appearance", Review_Rights);
   C.Begin_Commit (Object, Authority, 42, ID, 7, 123, Request, Status);
   Check (Status = C.Stale_Review and not C.Valid (Request));
   Check (C.Current_Phase (Object) = C.Candidate);
   Approve;
   A.Revoke (Authority, 42);
   C.Inspect (Object, Authority, 42, View, Allowed);
   Check (not Allowed and View.ID = 0);
   C.Begin_Commit (Object, Authority, 42, ID, 7, 123, Request, Status);
   Check (Status = C.Stale_Review and not C.Valid (Request));
   Install (42, "desktop.appearance", Review_Rights);
   C.Begin_Commit (Object, Authority, 42, ID, 7, 123, Request, Status);
   Check (Status = C.Wrong_Phase); -- Regrant does not resurrect approval.
   Approve;
   C.Begin_Commit (Object, Authority, 42, ID, 8, 123, Request, Status);
   Check (Status = C.Stale_Review and not C.Valid (Request));
   Approve;

   -- New source (even same evaluated value) has a new review identity.
   Old_ID := ID;
   Propose;
   Check (ID > Old_ID);
   C.Approve (Object, Authority, 42, Old_ID, Status); Check (Status = C.Stale_Review);
   Approve;
   C.Propose (Object, Name, "(", 7, ID, Status);
   Check (Status = C.Invalid_Source and ID = 0 and C.Current_Phase (Object) = C.Empty);
   C.Begin_Commit (Object, Authority, 42, Old_ID, 7, 123, Request, Status);
   Check (Status = C.Wrong_Phase and not C.Valid (Request));
   C.Propose (Object, "desktop.other", Source, 7, ID, Status);
   Check (Status = C.Wrong_Target and ID = 0);
   C.Propose (Object, Name, Source, C.Number'Last, ID, Status);
   Check (Status = C.Identity_Exhausted and ID = 0);
   C.Propose (Object, Name,
     "(system-config v1 (setting ""a"" ""x"") (setting ""b"" ""y""))", 7, ID, Status);
   Check (Status = C.Invalid_Source and ID = 0);
   C.Propose (Object, Name, "(startup v1)", 7, ID, Status);
   Check (Status = C.Invalid_Source and ID = 0);
   C.Propose (Object, Name, "", 7, ID, Status);
   Check (Status = C.Invalid_Source and ID = 0);
   C.Propose (Object, Name, String'(1 .. CCL.Declarations.MAX_SOURCE + 1 => 'x'), 7, ID, Status);
   Check (Status = C.Invalid_Source and ID = 0);
   declare
      Shifted : constant String (500 .. 499 + Source'Length) := Source;
      Extreme : constant String (Integer'Last - Source'Length + 1 .. Integer'Last) := Source;
   begin
      C.Propose (Object, Name, Shifted, 7, ID, Status);
      Check (Status = C.Accepted);
      C.Propose (Object, Name, Extreme, 7, ID, Status);
      Check (Status = C.Accepted);
   end;

   Propose; Approve; Begin_Commit;
   Check (C.Snapshot (Request).ID /= Original.ID);
   Check (C.Snapshot (Request).Setting.Value.Data (1 .. C.Snapshot (Request).Setting.Value.Length) = "100");
   Check (C.Snapshot (Request).Source.Data (1 .. C.Snapshot (Request).Source.Length) = Source);
   Old_ID := ID;
   C.Propose (Object, Name, Source, 7, ID, Status);
   Check (Status = C.Busy and ID = 0); ID := Old_ID;
   C.Begin_Commit (Object, Authority, 42, ID, 7, 123, Invalid_Request, Status);
   Check (Status = C.Wrong_Phase and not C.Valid (Invalid_Request));
   C.Complete_Application (Object, ID, 8, 123, C.Succeeded, Status);
   Check (Status = C.Ignored and C.Current_Phase (Object) = C.Committing);
   C.Complete_Storage (Object, ID + 1, C.Stored, 8, Status);
   Check (Status = C.Ignored and C.Selected_Revision (Object) = 0);
   -- Acceptance is the authorization linearization point. Revocation blocks
   -- later review/readback, not completion of an already accepted transaction.
   A.Revoke (Authority, 42);
   Check (C.Valid (Request) and C.Reviewer (Request) = 42);
   C.Inspect (Object, Authority, 42, View, Allowed);
   Check (not Allowed and View.ID = 0);
   C.Complete_Storage (Object, ID, C.Stored, 8, Status);
   Check (Status = C.Accepted and C.Current_Phase (Object) = C.Selected);
   Check (C.Selected_Revision (Object) = 8); -- Stored is NOT applied.
   Old_ID := ID;
   C.Propose (Object, Name, Source, 8, ID, Status);
   Check (Status = C.Busy and ID = 0); ID := Old_ID;
   C.Complete_Storage (Object, ID, C.Not_Stored, 0, Status);
   Check (Status = C.Ignored and C.Current_Phase (Object) = C.Selected);
   C.Complete_Application (Object, ID, 8, 124, C.Succeeded, Status);
   Check (Status = C.Ignored); -- A replacement consumer cannot acknowledge old work.
   C.Complete_Application (Object, ID, 9, 123, C.Succeeded, Status);
   Check (Status = C.Ignored);
   C.Complete_Application (Object, ID + 1, 8, 123, C.Succeeded, Status);
   Check (Status = C.Ignored);
   C.Complete_Application (Object, ID, 8, 123, C.Succeeded, Status);
   Check (Status = C.Accepted and C.Current_Phase (Object) = C.Applied);
   C.Complete_Storage (Object, ID, C.Stored, 8, Status); Check (Status = C.Ignored);
   Install (42, "desktop.appearance", Review_Rights);

   -- A definite rollback consumes the review; another proposal/review is needed.
   Propose (8); Approve; Begin_Commit (8);
   C.Complete_Storage (Object, ID, C.Not_Stored, 0, Status);
   Check (Status = C.Accepted and C.Current_Phase (Object) = C.Commit_Rejected);
   C.Begin_Commit (Object, Authority, 42, ID, 8, 123, Invalid_Request, Status);
   Check (Status = C.Wrong_Phase);
   Propose (8); Approve; Begin_Commit (8);
   C.Complete_Storage (Object, ID, C.Indeterminate, 0, Status);
   Check (Status = C.Accepted and C.Current_Phase (Object) = C.Commit_Uncertain);
   C.Propose (Object, Name, Source, 8, ID, Status); Check (Status = C.Busy);

   -- Malformed success, boundary arithmetic, and apply failure are fail-closed.
   for Case_No in 1 .. 4 loop
      declare
         Other : C.State;
         Token : C.Number;
         Base : constant C.Number := (if Case_No = 4 then C.Number'Last - 1 else 7);
         Revision : constant C.Number :=
           (case Case_No is when 1 => 0, when 2 => 9, when 3 => 8, when others => C.Number'Last);
      begin
         C.Propose (Other, Name, Source, Base, Token, Status); Check (Status = C.Accepted);
         C.Approve (Other, Authority, 42, Token, Status); Check (Status = C.Accepted);
         C.Begin_Commit (Other, Authority, 42, Token, Base, 123, Request, Status);
         Check (Status = C.Accepted);
         C.Complete_Storage (Other, Token, C.Stored, Revision, Status); Check (Status = C.Accepted);
         if Case_No <= 2 then
            Check (C.Current_Phase (Other) = C.Commit_Uncertain);
         else
            Check (C.Current_Phase (Other) = C.Selected);
            C.Complete_Application (Other, Token, Revision, 123, C.Failed, Status);
            Check (Status = C.Accepted and C.Current_Phase (Other) = C.Apply_Failed);
         end if;
         C.Propose (Other, Name, Source, Base, Token, Status); Check (Status = C.Busy);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Config reviewed activation: PASS" & Checks'Image & " checks");
end Activation_Tests;
