pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Sample_Cache;
with ACPI_Observation_Values;
procedure Cache_Policy_Tests is
   package Values renames ACPI_Observation_Values;
   package Lid is new ACPI_Sample_Cache (Values.Lid_Sample, Values.Empty_Lid_Sample);
   package Power is new ACPI_Sample_Cache (Values.Power_Source_Sample, Values.Empty_Power_Source_Sample);
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   generic
      with package Cache is new ACPI_Sample_Cache (<>);
      Good, Other : Cache.Sample_Type;
   procedure Exercise;
   procedure Exercise is
      use type Cache.Revision_Number, Cache.Tick;
      use type Cache.Cache_Status, Cache.Availability, Cache.Sample_Type;
      use type Cache.Publication_Result, Cache.Invalidation_Result, Cache.View;
      Data : Cache.State;
      At_Limit : Cache.State (Cache.Revision_Number'Last);
      Last_Publication : Cache.State (Cache.Revision_Number'Last - 2);
      Pending_At_Limit : Cache.State (Cache.Revision_Number'Last - 1);
      Invalidation : Cache.Invalidation_Result;
      Publication : Cache.Publication_Result;
      View, Previous : Cache.View;
      Time : constant Cache.Clock_Reading := (True, 100);
      procedure Read is
      begin View := Cache.Read (Data, Time, 0); end Read;
      procedure Publish (Kind : Cache.Publication := Cache.Sampled) is
      begin Cache.Publish (Data, Cache.Revision (Data), Kind, Good, Time, Publication); end Publish;
   begin
      Read;
      Check (Cache.Revision (Data) = 0 and then Cache.Status (Data) = Cache.Never_Sampled
        and then not Cache.Can_Publish (Data), "constructor starts empty at exact revision");
      Check (View.Status = Cache.Not_Ready and then View.Value = Cache.Empty_Sample
        and then not View.Time.Available and then View.Time.Value = 0, "initial sample/time empty");
      Previous := View; Publish; Read;
      Check (Publication = Cache.Not_Pending and then View = Previous, "cannot publish without pending invalidation");
      Cache.Invalidate (Data, Invalidation); Read;
      Check (Invalidation = Cache.Invalidated and then Cache.Revision (Data) = 1
        and then Cache.Status (Data) = Cache.Pending and then Cache.Can_Publish (Data), "invalidate advances once to pending");
      Check (View.Value = Cache.Empty_Sample and then not View.Time.Available, "invalidation clears sample/time");
      Previous := View;
      Cache.Publish (Data, 0, Cache.Sampled, Other, Time, Publication); Read;
      Check (Publication = Cache.Stale_Revision and then View = Previous, "stale producer cannot overwrite pending revision");
      Cache.Publish (Data, 1, Cache.Sampled, Good, (False, Cache.Tick'Last), Publication); Read;
      Check (Publication = Cache.Published and then Cache.Revision (Data) = 2, "unknown clock does not repeat publication");
      Check (View.Status = Cache.Clock_Unknown and then View.Value = Cache.Empty_Sample
        and then not View.Time.Available and then View.Time.Value = 0, "unavailable timestamp normalized despite nonzero supplied tick");
      Cache.Publish (Data, 1, Cache.Sampled, Other, Time, Publication);
      Check (Publication = Cache.Stale_Revision, "stale takes priority over non-pending status");
      Cache.Publish (Data, 2, Cache.Sampled, Other, Time, Publication);
      Check (Publication = Cache.Not_Pending, "current producer cannot republish ready sample");
      Cache.Invalidate (Data, Invalidation); Publish; Read;
      Check (Publication = Cache.Published and then View.Status = Cache.Available and then View.Value = Good,
        "fresh publication returns exact owned record");
      Previous := View;
      Cache.Publish (Data, Cache.Revision (Data) - 1, Cache.Sampled, Other, (True, 200), Publication); Read;
      Check (Publication = Cache.Stale_Revision and then View = Previous, "stale publication preserves actual payload and timestamp");
      Cache.Publish (Data, Cache.Revision (Data), Cache.Sampled, Other, (True, 200), Publication); Read;
      Check (Publication = Cache.Not_Pending and then View = Previous, "non-pending publication preserves payload and timestamp");
      View := Cache.Read (Data, (True, 110), 10);
      Check (View.Status = Cache.Available and then View.Value = Good, "age limit is inclusive");
      View := Cache.Read (Data, (True, 111), 10);
      Check (View.Status = Cache.Too_Old and then View.Value = Cache.Empty_Sample, "expired sample payload suppressed");
      View := Cache.Read (Data, (True, 99), Cache.Tick'Last);
      Check (View.Status = Cache.Clock_Regressed and then View.Value = Cache.Empty_Sample, "regression cannot wrap into freshness");
      View := Cache.Read (Data, (False, Cache.Tick'Last), Cache.Tick'Last);
      Check (View.Status = Cache.Clock_Unknown and then View.Value = Cache.Empty_Sample, "unavailable now cannot certify freshness");
      View := Cache.Read (Data, (True, Cache.Tick'Last), Cache.Tick'Last - 100);
      Check (View.Status = Cache.Available and then View.Value = Good, "full64-bit age boundary does not narrow or wrap");
      Cache.Invalidate (Data, Invalidation); Publish (Cache.Known_Absent); Read;
      Check (View.Status = Cache.Missing and then View.Value = Cache.Empty_Sample, "known absence carries no supplied payload");
      View := Cache.Read (Data, (True, 101), 0);
      Check (View.Status = Cache.Too_Old and then View.Value = Cache.Empty_Sample, "cached absence also expires");
      Cache.Invalidate (Data, Invalidation); Publish (Cache.Acquisition_Failed); Read;
      Check (View.Status = Cache.Failure and then View.Value = Cache.Empty_Sample, "acquisition failure clears old sample");
      View := Cache.Read (Data, (False, 0), 0);
      Check (View.Status = Cache.Failure, "failure outcome precedes clock classification");
      Check (Cache.Revision (At_Limit) = Cache.Revision_Number'Last and then not Cache.Can_Publish (At_Limit),
        "max initial revision is represented exactly");
      Cache.Publish (At_Limit, Cache.Revision_Number'Last, Cache.Sampled, Good, Time, Publication);
      Check (Publication = Cache.Not_Pending and then Cache.Status (At_Limit) = Cache.Never_Sampled,
        "initial max non-pending cache retains normal status priority");
      Cache.Invalidate (At_Limit, Invalidation);
      Check (Invalidation = Cache.Counter_Exhausted and then Cache.Status (At_Limit) = Cache.Exhausted
        and then Cache.Revision (At_Limit) = Cache.Revision_Number'Last, "invalidation at max is terminal without wrap");
      Cache.Publish (At_Limit, 0, Cache.Sampled, Good, Time, Publication);
      Check (Publication = Cache.Counter_Exhausted, "terminal exhaustion precedes stale revision");
      Cache.Invalidate (At_Limit, Invalidation);
      Check (Invalidation = Cache.Counter_Exhausted and then Cache.Revision (At_Limit) = Cache.Revision_Number'Last,
        "repeated exhausted invalidation never wraps");
      Cache.Invalidate (Last_Publication, Invalidation);
      Cache.Publish (Last_Publication, Cache.Revision (Last_Publication), Cache.Sampled, Good, Time, Publication);
      View := Cache.Read (Last_Publication, Time, 0);
      Check (Publication = Cache.Published and then Cache.Revision (Last_Publication) = Cache.Revision_Number'Last
        and then View.Status = Cache.Available and then View.Value = Good, "last permitted publication reaches max exactly");
      Cache.Invalidate (Last_Publication, Invalidation); View := Cache.Read (Last_Publication, Time, 0);
      Check (View.Status = Cache.Counter_Exhausted and then View.Value = Cache.Empty_Sample
        and then not View.Time.Available and then View.Time.Value = 0, "exhaustion clears previous payload and time");
      Cache.Invalidate (Pending_At_Limit, Invalidation);
      Check (Invalidation = Cache.Invalidated and then Cache.Revision (Pending_At_Limit) = Cache.Revision_Number'Last
        and then Cache.Status (Pending_At_Limit) = Cache.Pending and then not Cache.Can_Publish (Pending_At_Limit),
        "pending last revision is distinct from publishable state");
      Cache.Publish (Pending_At_Limit, Cache.Revision_Number'Last, Cache.Sampled, Good, Time, Publication);
      Check (Publication = Cache.Counter_Exhausted and then Cache.Status (Pending_At_Limit) = Cache.Exhausted,
        "pending at max refuses publication and clears terminal state");
   end Exercise;
   procedure Lid_Checks is new Exercise (Lid, (Values.Open, 1), (Values.Closed, 0));
   procedure Power_Checks is new Exercise (Power, (Values.Online, 1), (Values.Offline, 0));
begin
   Lid_Checks; Power_Checks;
   Ada.Text_IO.Put_Line ("ACPI observation cache policy checks" & Checks'Image);
end Cache_Policy_Tests;
