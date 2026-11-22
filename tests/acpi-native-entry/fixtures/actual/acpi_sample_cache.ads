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
   type State (Initial_Revision : Revision_Number := 0) is limited private
     with Default_Initial_Condition => Revision (State) = State.Initial_Revision
       and then Status (State) = Never_Sampled
       and then Stored_Value (State) = Empty_Sample
       and then Stored_Time (State) = (Available => False, Value => 0);
   function Revision (Cache : State) return Revision_Number;
   function Status (Cache : State) return Cache_Status;
   function Can_Publish (Cache : State) return Boolean;
   -- Ghost observations specify payload/time preservation without exposing
   -- runtime access to the private cache or permitting borrowed sample data.
   function Stored_Value (Cache : State) return Sample_Type with Ghost;
   function Stored_Time (Cache : State) return Clock_Reading with Ghost;
   type Invalidation_Result is (Invalidated, Counter_Exhausted);
   procedure Invalidate (Cache : in out State; Result : out Invalidation_Result)
     with Post => Result =
         (if Status (Cache)'Old = Exhausted or else Revision (Cache)'Old = Revision_Number'Last
          then Counter_Exhausted else Invalidated)
       and then Revision (Cache) =
         (if Result = Invalidated then Revision (Cache)'Old + 1 else Revision (Cache)'Old)
       and then Status (Cache) = (if Result = Invalidated then Pending else Exhausted)
       and then Stored_Value (Cache) = Empty_Sample
       and then Stored_Time (Cache) = (Available => False, Value => 0);
   type Publication is (Sampled, Known_Absent, Acquisition_Failed);
   type Publication_Result is (Published, Stale_Revision, Not_Pending, Counter_Exhausted);
   -- Expected is an owner freshness token, not an authority or device identity.
   -- Only a Pending revision can publish once. Events/rebinds advance revision.
   -- Counter exhaustion is terminal, clears the sample and never wraps.
   procedure Publish (Cache : in out State; Expected : Revision_Number;
      Kind : Publication; Value : Sample_Type; At_Time : Clock_Reading;
      Result : out Publication_Result)
     with Post => Result =
         (if Status (Cache)'Old = Exhausted then Counter_Exhausted
          elsif Expected /= Revision (Cache)'Old then Stale_Revision
          elsif Status (Cache)'Old /= Pending then Not_Pending
          elsif Revision (Cache)'Old = Revision_Number'Last then Counter_Exhausted
          else Published)
       and then Revision (Cache) =
         (if Result = Published then Revision (Cache)'Old + 1 else Revision (Cache)'Old)
       and then Status (Cache) =
         (if Result = Published then
            (case Kind is when Sampled => Ready, when Known_Absent => Absent, when Acquisition_Failed => Failed)
          elsif Result = Counter_Exhausted then Exhausted else Status (Cache)'Old)
       and then Stored_Value (Cache) =
         (if Result = Published then (if Kind = Sampled then Value else Empty_Sample)
          elsif Result = Counter_Exhausted and then Status (Cache)'Old /= Exhausted then Empty_Sample
          else Stored_Value (Cache)'Old)
       and then Stored_Time (Cache) =
         (if Result = Published then
            (Available => At_Time.Available, Value => (if At_Time.Available then At_Time.Value else 0))
          elsif Result = Counter_Exhausted and then Status (Cache)'Old /= Exhausted then
            (Available => False, Value => 0)
          else Stored_Time (Cache)'Old);
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
       and then Read'Result.Time = Stored_Time (Cache)
       and then Read'Result.Status =
         (case Status (Cache) is
          when Never_Sampled | Pending => Not_Ready,
          when Exhausted => Counter_Exhausted,
          when Failed => Failure,
          when Ready | Absent =>
            (if not Now.Available or else not Stored_Time (Cache).Available then Clock_Unknown
             elsif Now.Value < Stored_Time (Cache).Value then Clock_Regressed
             elsif Now.Value - Stored_Time (Cache).Value > Maximum_Age then Too_Old
             elsif Status (Cache) = Absent then Missing else Available))
       and then Read'Result.Value =
         (if Read'Result.Status = Available then Stored_Value (Cache) else Empty_Sample);
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
   function Stored_Value (Cache : State) return Sample_Type is (Cache.Value);
   function Stored_Time (Cache : State) return Clock_Reading is (Cache.Sample_Time);
end ACPI_Sample_Cache;
