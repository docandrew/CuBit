with CCL.Configurations;
with CCL.Resource_Sections;
with CCL.Scheduling_Limits;

--  startup's decision for one program: what its manifest requests (a decoded
--  .cubit.resources section) intersected with what the startup policy
--  approves for it (docs/ccl-driver-manifests.md). A request is granted only
--  when both allow it; everything else is denied with a reason to log and
--  audit. Deciding grants nothing: startup mints the granted entries.
package CCL.Startup_Grants with SPARK_Mode => On is
   use CCL.Configurations;
   use CCL.Resource_Sections;

   type Decision is
     (Granted, Device_Not_Approved, Scheduling_Not_Approved,
      Scheduling_Exceeds_Ceiling);
   type Decision_Array is array (Positive range 1 .. MAX_ENTRIES) of Decision;

   function Device_Approved (Policy : Launch_Entry) return Boolean is
     (Policy.Mode = Per_Device and then Policy.Approve_Device);

   function Within_Ceiling (Policy : Launch_Entry; Item : Resource) return Boolean is
     (Policy.Approve_Scheduling
      and then Item.Amount in 1 .. Scheduling_Limits.MAX_MICROSECONDS
      and then Item.Extra in 1 .. Scheduling_Limits.MAX_MICROSECONDS
      and then Scheduling_Limits.Covers
                 (Policy.Scheduling_Budget, Policy.Scheduling_Period,
                  Scheduling_Limits.Microseconds (Item.Amount),
                  Scheduling_Limits.Microseconds (Item.Extra)));

   --  The intended meaning of a grant, entry by entry.
   function Allowed (Policy : Launch_Entry; Item : Resource) return Boolean is
     (case Item.Kind is
         when Device_Resource => Device_Approved (Policy),
         when Scheduling => Within_Ceiling (Policy, Item));

   procedure Decide
     (Policy : Launch_Entry; Plan : Section_Plan; Decisions : out Decision_Array)
   with Post =>
     (for all I in 1 .. Plan.Count =>
        (Decisions (I) = Granted) = Allowed (Policy, Plan.Entries (I)))
     and then (for all I in Plan.Count + 1 .. MAX_ENTRIES =>
                 Decisions (I) /= Granted);
end CCL.Startup_Grants;
