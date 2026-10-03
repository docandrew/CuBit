with Vulkan_Scene;
with Vulkan_Owned_Targets;
with Vulkan_Frame;
-- Records an immutable captured scene into an acquired owned target. The caller
-- records audited source preparation before this step and any final
-- barriers afterwards. Owned-target initial/retained layout preparation is
-- recorded here from confirmed per-target damage history. Recorded does not mean submitted or GPU/display ready.
package Vulkan_Scene_Recording with SPARK_Mode is
   package O renames Vulkan_Owned_Targets;
   package V renames O.V;
   package P renames O.P;
   package D renames Vulkan_Frame.D;
   use type V.Phase, P.Slot, P.Ticket, D.State;
   type Outcome is (Recorded, Cancelled, Quarantined);
   procedure Record_Scene
     (Scene : Vulkan_Scene.State; Targets : O.State;
      Submission : in out V.State; Pool : in out P.State;
      Damage : in out D.State; Result : out Outcome)
     with Pre => P.Valid (Pool) and P.Rendering (Pool) and
       V.Current (Submission) = V.Recording and not V.Pass_Started (Submission) and
       V.Complete_Frame (Submission) and D.Valid (Damage) and not D.Faulted (Damage) and
       D.Active (Damage) = P.Writer (Pool).Buffer and D.Active (Damage) /= 0,
       Post => P.Valid (Pool) and D.Valid (Damage) and
         (case Result is
            when Recorded => Damage = Damage'Old and V.Current (Submission) = V.Recording and V.Pass_Finished (Submission) and
              V.Complete_Frame (Submission) and P.Rendering (Pool) and
              P.Writer (Pool) = P.Writer (Pool'Old),
            when Cancelled => V.Quiescent (Submission) and not P.Faulted (Pool) and
              P.Writer (Pool) = P.None and D.Active (Damage) = 0,
            when Quarantined => P.Faulted (Pool) and D.Faulted (Damage));
end Vulkan_Scene_Recording;
