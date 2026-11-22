with Compositor_Upload; with Vulkan_Upload_Owner; with Vulkan_Owned_Source;
package Vulkan_Upload_Recording with SPARK_Mode is
   package U renames Vulkan_Upload_Owner; package C renames U.C; package V renames U.V;
   package S renames Vulkan_Owned_Source; package G renames Compositor_Upload;
   use type V.Phase;
   -- The private source must belong to Index and have no other descriptor alias.
   -- Producer writes are finished; owners stay held until cancellation/completion.
   -- Discard is a trusted layout observation. A cold partial transfer does not
   -- initialize the whole image or grant permission to import/publish it.
   procedure Record_Transfer (Submission : in out V.State; Context : C.State;
      Upload : U.State; Source : S.State; Index : V.Source_Slot;
      Plan : G.Plan; Discard : Boolean; Accepted : out Boolean)
     with Pre => V.Current (Submission) = V.Recording and not V.Pass_Started (Submission),
       Post => V.Current (Submission) = V.Recording and not V.Pass_Started (Submission) and
         V.Same_Sources (Submission, Submission'Old) and
         Accepted = V.Complete_Frame (Submission) and
         V.Draws (Submission) = V.Draws (Submission'Old) +
            (if V.Complete_Frame (Submission'Old) and V.Draws (Submission'Old) < V.Maximum_Draws then 1 else 0);
end Vulkan_Upload_Recording;
