with Compositor_Source_Region;
with Compositor_Target_Damage;
package Vulkan_Submission.Regions with SPARK_Mode is
   package G renames Compositor_Affine.G;
   package D renames Compositor_Target_Damage;
   use type D.P.Slot;
   -- The complete source image stays pinned by its existing ticket. A region
   -- neither creates a separate owner nor permits early backing retirement.
   procedure Draw_Output (S : in out State; Source : Source_Ticket;
      Screen : G.Output; Surface : G.Logical_Rectangle; Damage : G.Physical_Rectangle;
      Region : Compositor_Source_Region.Rectangle; Over, Straight_Alpha : Boolean;
      Result : out Vulkan_Affine_Binding.Outcome)
     with Pre => Current (S) = Recording and Pass_Active (S),
       Post => Same_Sources (S, S'Old) and Current (S) = Recording and Pass_Active (S) and
         Draws (S) = Draws (S'Old) +
           (if Complete_Frame (S'Old) and Draws (S'Old) < Maximum_Draws then 1 else 0) and
         (if Result = Vulkan_Affine_Binding.Rejected then not Complete_Frame (S)
          else Complete_Frame (S) and Draws (S) = Draws (S'Old) + 1);
   procedure Replay (S : in out State; Damage : D.State; Source : Source_Ticket;
      Screen : G.Output; Surface : G.Logical_Rectangle; Clip : G.Physical_Rectangle;
      Region : Compositor_Source_Region.Rectangle; Over, Straight_Alpha : Boolean;
      Accepted : out Boolean)
     with Pre => Current (S) = Recording and Pass_Active (S) and
       D.Valid (Damage) and not D.Faulted (Damage) and D.Active (Damage) /= 0,
       Post => Current (S) = Recording and Pass_Active (S) and
         Same_Sources (S, S'Old) and Draws (S) >= Draws (S'Old) and
         Draws (S) <= Draws (S'Old) + D.D.Capacity and Accepted = Complete_Frame (S);
end Vulkan_Submission.Regions;
