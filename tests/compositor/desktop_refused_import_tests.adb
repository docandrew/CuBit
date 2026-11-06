with CuBit.Handles.Fixture;
with Ada.Text_IO;
with Interfaces;
with Compositor_Formats; with Compositor_Source_Content;
with Desktop_GPU_Scene; with Desktop_GPU_Scene.Images;
with Desktop_Image_Registry;
with Desktop_Image_Source; with Desktop_Vulkan_Startup; with Vulkan_Device_Mock;
-- BUG-001 (laptop-15): the GPU driver refused the desktop's VM bind for a
-- new window's surface and the whole desktop died. The device now reports a
-- capacity refusal as a failed allocation, nothing else (anv: Mesa's
-- vkAllocateMemory fails, the image and its memory are destroyed). Over the
-- native mocks: two windows on screen, a third opens and its backing is
-- refused. The frame still completes with a placeholder for the new window,
-- the scene and its sources are never quarantined (an Unsafe completion
-- exits the desktop), and the window is drawn once memory is granted again.
procedure Desktop_Refused_Import_Tests is
   package G renames Desktop_GPU_Scene;
   package D renames Desktop_Vulkan_Startup;
   package R renames Desktop_Image_Registry;
   package I renames Desktop_Image_Source;
   package C renames Compositor_Source_Content;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   use type U32, G.Outcome, G.Phase, R.Capacity_Pressure, C.Content_Version;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Refuse_Next (Count : U32) with Import, Convention => C, External_Name => "image_mock_refuse_next";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   procedure Real_Mapping (On : U32) with Import, Convention => C, External_Name => "upload_mock_real_mapping";
   Edge : constant := 16;
   type Buffer is array (0 .. Edge * Edge - 1) of aliased U32;
   Pixels : array (1 .. 3) of Buffer := (others => (others => 0));
   Screen : constant Desktop_GPU_Scene.V.A.G.Output := (32, 24, Desktop_GPU_Scene.V.A.G.Unrotated, (1, 1), 0, 0);
   S : G.State;
   Reg : R.State;
   OK : Boolean;
   Result : G.Outcome;
   Progress : I.Outcome;
   Checks : Natural := 0;
   Refused : Natural;
   function Image_Of (W : Positive) return Compositor_Formats.Image is
     ((Pixels (W) (0)'Address, Edge, Edge, Edge * 4, 0));
   procedure Check (Condition : Boolean; What : String) is
   begin
      pragma Assert (Condition, What);
      Checks := Checks + 1;
   end Check;
   --  One frame of Windows windows; True when it completed. A cold capture
   --  (an upload) is driven and recaptured, as the desktop does.
   procedure Frame (Windows : Positive) is
   begin
      for Attempt in 1 .. 4 loop
         G.Begin_Frame (S, Screen, 0, OK); Check (OK, "begin frame");
         for W in 1 .. Windows loop
            G.Images.Capture (S, Reg, C.Source_Key (W), 1, Image_Of (W),
              Edge * Edge * 4, (0, 0, 32, 24), (0, 0, 32, 24), OK);
            Check (G.Current (S) /= G.Quarantined, "a refused import never quarantines the scene");
         end loop;
         while R.Upload_Work (Reg) loop R.Poll (Reg, Progress); end loop;
         G.Finish (S, Result);
         Check (Result in G.Pending | G.Retry, "frame submits or retries, never fails");
         exit when Result = G.Pending;
      end loop;
      Check (Result = G.Pending, "frame submitted");
      G.Poll (S, Result); Check (Result = G.Complete, "frame completed");
      Check (not R.Faulted (Reg), "sources never faulted");
   end Frame;
begin
   Reset; Context_Set (0, 0); Pipeline_Set (0, 0);
   Vulkan_Device_Mock.Set (True, True, 0); Image_Set (4096, 1, 0, 0, 0);
   Upload_Set (65536, 1, 0, 0, 0, 0); Real_Mapping (1);
   D.Initialize (CuBit.Handles.Fixture.Endpoint_Of (25)); D.Configure_Targets (32, 24, 1, 4096 * 200, OK);
   Check (OK, "targets");
   D.Prepare_Pipeline (OK); Check (OK, "pipeline");
   D.Configure_Upload (65536, OK); Check (OK, "upload");
   Frame (2); Frame (2);
   Check (R.Resident_Keys (Reg) = 2 and G.Placeholder_Draws (S) = 0, "two windows resident");
   --  The third window opens; every allocation for it is refused.
   Refuse_Next (U32'Last);
   Frame (3);
   Check (G.Placeholder_Draws (S) > 0, "the refused window is a placeholder");
   Check (R.Last_Pressure (Reg) = R.Memory_Exhausted, "reported as memory pressure");
   Check (R.Resident_Keys (Reg) = 3, "the new window keeps its key for the next frame");
   Frame (3);
   --  Memory again: the window is drawn.
   Refused := G.Placeholder_Draws (S);
   Check (Refused >= 2, "a placeholder in every refused frame");
   Refuse_Next (0);
   Frame (3); Frame (3);
   Check (G.Placeholder_Draws (S) = Refused, "the window is drawn once its backing is granted");
   Check (R.Last_Pressure (Reg) = R.None, "no pressure once granted");
   G.Close (S, OK); Check (OK, "scene closes");
   R.Close (Reg, True, OK); Check (OK, "sources close");
   D.Stop;
   Ada.Text_IO.Put_Line ("PASS desktop refused import:" & Checks'Image &
     " checks; a refused window backing is a placeholder, frames complete, nothing quarantined (native mocks)");
end Desktop_Refused_Import_Tests;
