package body Config_Activation with SPARK_Mode is
   use type CCL.Configurations.Profile_Kind;

   function Corresponds
     (Object : State; Target, Source : String; Base : Number) return Boolean is
     (Object.View.Source.Data (1 .. Object.View.Source.Length) = Source and then
      Object.View.Setting.Key.Data (1 .. Object.View.Setting.Key.Length) = Target and then
      Object.View.Base = Base);

   function Acknowledgement_Matches
     (Object : State; ID, Revision, Consumer_Instance : Number) return Boolean is
     (Object.Mode = Selected and then ID = Object.View.ID and then
      Revision = Object.Revision and then Consumer_Instance /= 0 and then
      Consumer_Instance = Object.Consumer);

   function May_Review
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID) return Boolean is
     (Object.View.ID /= 0 and then
      Config_Authority.Allows
        (Authority, Subject,
         Object.View.Setting.Key.Data (1 .. Object.View.Setting.Key.Length),
         Config_Authority.Read_Config) and then
      Config_Authority.Allows
        (Authority, Subject,
         Object.View.Setting.Key.Data (1 .. Object.View.Setting.Key.Length),
         Config_Authority.Activate_Config));

   function Live_Review
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; ID, Base : Number) return Boolean is
     (ID /= 0 and then ID = Object.View.ID and then Base = Object.View.Base and then
      Subject = Object.Subject and then Object.Authority_Revision /= 0 and then
      Object.Authority_Revision = Config_Authority.Revision (Authority, Subject) and then
      May_Review (Object, Authority, Subject));

   function Authorized_Review
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; ID, Base : Number) return Boolean is
     (Live_Review (Object, Authority, Subject, ID, Base));

   function Bound_To (Request : Commit_Request; Object : State) return Boolean is
     (Request.Present and then Request.View = Object.View and then
      Request.Subject = Object.Subject and then
      Request.Authority_Revision = Object.Authority_Revision);

   procedure Propose
     (Object : in out State; Target, Source : String; Base : Number;
      ID : out Number; Status : out Result)
   is
      Compiled : CCL.Configurations.Compilation_Result;
   begin
      ID := 0;
      Status := Busy;
      if Object.Mode in Committing | Commit_Uncertain | Selected | Apply_Failed then
         return;
      end if;
      Object.Mode := Empty;
      Object.View := (others => <>);
      Object.Subject := Config_Authority.No_Subject;
      Object.Authority_Revision := 0;
      Object.Consumer := 0;
      Object.Revision := 0;
      Status := Identity_Exhausted;
      if Object.Last_ID = Number'Last or else Base = Number'Last then return; end if;
      Status := Invalid_Source;
      if Source'Length not in 1 .. CCL.Declarations.MAX_SOURCE then return; end if;
      CCL.Configurations.Compile (Source, Compiled);
      if not Compiled.Success or else Compiled.Plan.Kind /= CCL.Configurations.System_Profile
        or else Compiled.Plan.Setting_Count /= 1
      then return; end if;
      Status := Wrong_Target;
      if Compiled.Plan.Settings (1).Key.Data
        (1 .. Compiled.Plan.Settings (1).Key.Length) /= Target
      then return; end if;
      Object.Last_ID := Object.Last_ID + 1;
      Object.View :=
        (ID => Object.Last_ID, Base => Base, Source => (others => <>),
         Setting => Compiled.Plan.Settings (1));
      Object.View.Source.Data (1 .. Source'Length) := Source;
      Object.View.Source.Length := Source'Length;
      Object.Mode := Candidate;
      ID := Object.View.ID;
      Status := Accepted;
   end Propose;

   procedure Inspect
     (Object : State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; View : out Review_View; Allowed : out Boolean) is
   begin
      View := (others => <>);
      Allowed := Object.View.ID /= 0 and then Config_Authority.Allows
        (Authority, Subject,
         Object.View.Setting.Key.Data (1 .. Object.View.Setting.Key.Length),
         Config_Authority.Read_Config);
      if Allowed then View := Object.View; end if;
   end Inspect;

   procedure Approve
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; ID : Number; Status : out Result) is
   begin
      Status := Wrong_Phase;
      if Object.Mode not in Candidate | Reviewed then return; end if;
      Status := Stale_Review;
      if ID = 0 or else ID /= Object.View.ID then return; end if;
      Status := Denied;
      if not May_Review (Object, Authority, Subject) then return; end if;
      Object.Subject := Subject;
      Object.Authority_Revision := Config_Authority.Revision (Authority, Subject);
      Object.Mode := Reviewed;
      Status := Accepted;
   end Approve;

   procedure Begin_Commit
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Subject : Subject_ID; ID, Base, Consumer_Instance : Number;
      Request : out Commit_Request; Status : out Result) is
   begin
      Request := (others => <>);
      Status := Wrong_Phase;
      if Object.Mode /= Reviewed then return; end if;
      Status := Denied;
      if Subject /= Object.Subject or else Consumer_Instance = 0 then return; end if;
      Status := Stale_Review;
      if not Live_Review (Object, Authority, Subject, ID, Base) then
         Object.Mode := Candidate;
         Object.Subject := Config_Authority.No_Subject;
         Object.Authority_Revision := 0;
         return;
      end if;
      Object.Mode := Committing;
      Object.Consumer := Consumer_Instance;
      Request := (Present => True, View => Object.View,
                  Subject => Object.Subject, Authority_Revision => Object.Authority_Revision);
      Status := Accepted;
   end Begin_Commit;

   procedure Complete_Storage
     (Object : in out State; ID : Number; Outcome : Storage_Outcome;
      Revision : Number; Status : out Result) is
   begin
      Status := Ignored;
      if Object.Mode /= Committing or else ID /= Object.View.ID then return; end if;
      Status := Accepted;
      case Outcome is
         when Not_Stored => Object.Mode := Commit_Rejected;
         when Indeterminate => Object.Mode := Commit_Uncertain;
         when Stored =>
            --  Base is admitted below Last by Propose. Subtraction avoids
            --  overflow even when this completion is hostile/malformed.
            if Revision /= 0 and then Revision - 1 = Object.View.Base then
               Object.Revision := Revision;
               Object.Mode := Selected;
            else
               Object.Mode := Commit_Uncertain;
            end if;
      end case;
   end Complete_Storage;

   procedure Complete_Application
     (Object : in out State; ID, Revision, Consumer_Instance : Number;
      Outcome : Application_Outcome; Status : out Result) is
   begin
      Status := Ignored;
      if Object.Mode /= Selected or else ID /= Object.View.ID or else
        Revision /= Object.Revision or else Consumer_Instance = 0 or else
        Consumer_Instance /= Object.Consumer
      then return; end if;
      Object.Mode := (if Outcome = Succeeded then Applied else Apply_Failed);
      Status := Accepted;
   end Complete_Application;
end Config_Activation;
