with Vulkan_Backdrop_Binding;
with Compositor_Target_Damage;
-- Wallpaper uses the same bounded submission and retained source tickets as
-- ordinary layers. No independent queue, resource owner, or completion signal.
package Vulkan_Submission.Backdrops with SPARK_Mode is
   package B renames Vulkan_Backdrop_Binding;
   package D renames Compositor_Target_Damage;
   use type B.Outcome, D.P.Slot;
   procedure Draw_Output
     (S : in out State; Source : Source_Ticket;
      W, H, Source_W, Source_H : B.B.G.Physical_Extent;
      Mode : B.B.S.Placement; Damage : B.B.G.Physical_Rectangle;
      Result : out B.Outcome)
     with Pre => Current (S) = Recording and Pass_Active (S),
       Post => Same_Sources (S, S'Old) and Current (S) = Recording and Pass_Active (S) and
         Draws (S) = Draws (S'Old) +
           (if Complete_Frame (S'Old) and Draws (S'Old) < Maximum_Draws then 1 else 0) and
         (if Result = B.Rejected then not Complete_Frame (S)
          else Complete_Frame (S) and Draws (S) = Draws (S'Old) + 1);
   -- One bounded attempt for each captured damage rectangle, intersected with
   -- the current physical UI clip. Placement itself remains output-relative.
   procedure Replay
     (S : in out State; Damage : D.State; Source : Source_Ticket;
      W, H, Source_W, Source_H : B.B.G.Physical_Extent;
      Mode : B.B.S.Placement; Clip : B.B.G.Physical_Rectangle;
      Accepted : out Boolean)
     with Pre => Current (S) = Recording and Pass_Active (S) and
       D.Valid (Damage) and not D.Faulted (Damage) and D.Active (Damage) /= 0,
       Post => Current (S) = Recording and Pass_Active (S) and
         Same_Sources (S, S'Old) and Draws (S) >= Draws (S'Old) and
         Draws (S) <= Draws (S'Old) + D.D.Capacity and
         Accepted = Complete_Frame (S);
end Vulkan_Submission.Backdrops;
