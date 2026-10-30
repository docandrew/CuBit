with Interfaces;
generic
   type Sample_Type is private;
   Empty_Sample : Sample_Type;
   -- Sample_Type must contain owned values, not borrowed interpreter handles.
package ACPI_Sample_Cache with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use type Interfaces.Unsigned_64;
   subtype Revision_Number is Interfaces.Unsigned_64 range 0 .. Interfaces.Unsigned_64 (Interfaces.Integer_64'Last);
   type Tick is new Interfaces.Unsigned_64;
   type Clock_Reading is record
      Available : Boolean := False;
      Value : Tick := 0;
   end record;
   type Cache_Status is (Never_Sampled, Pending, Ready, Absent, Failed, Exhausted);
   type State (Initial_Revision : Revision_Number := 0) is limited private;
   function Revision (Cache : State) return Revision_Number;
   function Status (Cache : State) return Cache_Status;
   function Can_Publish (Cache : State) return Boolean;
   type Invalidation_Result is (Invalidated, Counter_Exhausted);
   procedure Invalidate (Cache : in out State; Result : out Invalidation_Result)
     with Post => Revision (Cache) >= Revision (Cache)'Old
       and then Status (Cache) in Pending | Exhausted;
   type Publication is (Sampled, Known_Absent, Acquisition_Failed);
   type Publication_Result is (Published, Stale_Revision, Not_Pending, Counter_Exhausted);
   -- Expected is an owner freshness token, not an authority or device identity.
   -- Only a Pending revision can publish once. Events/rebinds advance revision.
   -- Counter exhaustion is terminal, clears the sample and never wraps.
   procedure Publish (Cache : in out State; Expected : Revision_Number;
      Kind : Publication; Value : Sample_Type; At_Time : Clock_Reading;
      Result : out Publication_Result)
     with Post => (if Result = Published then Revision (Cache) = Revision (Cache)'Old + 1
                   else Revision (Cache) = Revision (Cache)'Old)
       and then (if Result = Published then Status (Cache) =
         (case Kind is when Sampled => Ready, when Known_Absent => Absent,
                       when Acquisition_Failed => Failed))
       and then (if Result in Stale_Revision | Not_Pending then Status (Cache) = Status (Cache)'Old)
       and then (if Result = Counter_Exhausted then Status (Cache) = Exhausted);
   type Availability is (Not_Ready, Available, Missing, Failure, Too_Old,
                        Clock_Unknown, Clock_Regressed, Counter_Exhausted);
   type View is record
      Revision : Revision_Number := 0;
      Status : Availability := Not_Ready;
      Time : Clock_Reading;
      Value : Sample_Type := Empty_Sample;
   end record;
   -- Pure observation: unavailable/stale/absent outcomes contain Empty_Sample.
   -- A cached absence also expires. A missing/regressing clock cannot certify
   -- freshness. One owner must serialize mutation and use one clock epoch/unit.
   function Read (Cache : State; Now : Clock_Reading; Maximum_Age : Tick) return View
     with Post => Read'Result.Revision = Revision (Cache)
       and then (if Read'Result.Status /= Available then Read'Result.Value = Empty_Sample)
       and then (if Read'Result.Status in Available | Missing then
         Now.Available and then Read'Result.Time.Available
         and then Now.Value >= Read'Result.Time.Value
         and then Now.Value - Read'Result.Time.Value <= Maximum_Age);
private
   type State (Initial_Revision : Revision_Number := 0) is limited record
      Version : Revision_Number := Initial_Revision;
      Current : Cache_Status := Never_Sampled;
      Sample_Time : Clock_Reading;
      Value : Sample_Type := Empty_Sample;
   end record;
   function Revision (Cache : State) return Revision_Number is (Cache.Version);
   function Status (Cache : State) return Cache_Status is (Cache.Current);
   function Can_Publish (Cache : State) return Boolean is
     (Cache.Current = Pending and then Cache.Version < Revision_Number'Last);
end ACPI_Sample_Cache;
