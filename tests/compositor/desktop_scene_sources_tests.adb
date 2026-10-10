with Ada.Text_IO;
with Interfaces; with System;
with Compositor_Formats; with Compositor_Source_Content;
with Desktop_GPU_Scene; with Desktop_GPU_Scene.Images; with Desktop_GPU_Scene.Drawing;
with Desktop_Image_Registry;
with Desktop_Image_Source; with Desktop_Vulkan_Startup; with Vulkan_Device_Mock;
with Vulkan_Submission; with Vulkan_Scene;
-- Whole scenes over the native mocks: twelve client windows (more than the
-- former eight client slots) drawn every frame while every window publishes
-- a new version each frame. The old runtime software switch fired on a
-- repaint-only completion with slot pressure; here every capture must see
-- no pressure, steady frames must submit and complete, and no device
-- allocation may happen after warm-up.
procedure Desktop_Scene_Sources_Tests is
   package G renames Desktop_GPU_Scene;
   package D renames Desktop_Vulkan_Startup;
   package R renames Desktop_Image_Registry;
   package I renames Desktop_Image_Source;
   package V renames Vulkan_Submission;
   package C renames Compositor_Source_Content;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   use type U32, U64, G.Outcome, G.Phase, G.Capture_Failure, R.Capacity_Pressure, C.Content_Version;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   function Image_Binds return U32 with Import, Convention => C, External_Name => "image_mock_binds";
   function Image_Releases return U32 with Import, Convention => C, External_Name => "image_mock_releases";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   procedure Real_Mapping (On : U32) with Import, Convention => C, External_Name => "upload_mock_real_mapping";
   Windows : constant := 12;
   Edge : constant := 16;
   Frames : constant := 120;
   type Buffer is array (0 .. Edge * Edge - 1) of aliased U32;
   Pixels : array (1 .. Windows, 0 .. 1) of Buffer := (others => (others => (others => 0)));
   Screen : constant Vulkan_Scene.A.G.Output := (32, 24, Vulkan_Scene.A.G.Unrotated, (1, 1), 0, 0);
   S : G.State;
   Reg : R.State;
   OK : Boolean;
   Result : G.Outcome;
   Version : C.Content_Version := 1;
   Completed, Cold : Natural := 0;
   Binds, Releases : U32 := 0;
   Progress : I.Outcome;
   function Image_Of (W, B : Natural) return Compositor_Formats.Image is
     ((Pixels (W, B) (0)'Address, Edge, Edge, Edge * 4, 0));
begin
   Reset; Context_Set (0, 0); Pipeline_Set (0, 0);
   Vulkan_Device_Mock.Set (True, True, 0); Image_Set (4096, 1, 0, 0, 0);
   Upload_Set (65536, 1, 0, 0, 0, 0); Real_Mapping (1);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, 4096 * 200, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK);
   D.Configure_Upload (65536, OK); pragma Assert (OK);
   for Frame in 1 .. Frames loop
      -- Every window publishes a new version into its other buffer.
      Version := Version + 1;
      for W in 1 .. Windows loop
         R.Note_Change (Reg, C.Source_Key (W), (Natural (Version) mod Edge, Natural (Version) mod Edge + 1));
      end loop;
      loop
         G.Begin_Frame (S, Screen, 0, OK); pragma Assert (OK);
         for W in 1 .. Windows loop
            G.Images.Capture (S, Reg, C.Source_Key (W), Version, Image_Of (W, Natural (Version) mod 2),
              Edge * Edge * 4, (0, 0, 32, 24), (0, 0, 32, 24), OK);
            pragma Assert (R.Last_Pressure (Reg) = R.None);
         end loop;
         -- As Images.Complete: uploads started by this capture finish first.
         while R.Upload_Work (Reg) loop R.Poll (Reg, Progress); end loop;
         G.Finish (S, Result);
         exit when Result = G.Pending;
         -- Cold: the scene was discarded; drive uploads, then recapture.
         if Result /= G.Retry then Ada.Text_IO.Put_Line ("frame" & Frame'Image & " result " & Result'Image & " phase " & G.Current (S)'Image); end if;
         pragma Assert (Result = G.Retry, "cold capture retries, never fails");
         Cold := Cold + 1;
      end loop;
      G.Poll (S, Result); pragma Assert (Result = G.Complete);
      Completed := Completed + 1;
      if Frame = 2 then Binds := Image_Binds; Releases := Image_Releases; end if;
   end loop;
   pragma Assert (Completed = Frames and R.Resident_Keys (Reg) = Windows);
   pragma Assert (Image_Binds = Binds and Image_Releases = Releases,
     "no device allocation or free after warm-up");
   -- Scene capacity: an 1800-layer frame (900 fills, each clip + fill) (well past the former 512-layer
   -- cap, e.g. a full repaint with the Apps menu open) submits; one layer
   -- beyond the budget is reported as Layer_Limit, not as a cold upload.
   for Layers in 1 .. 2 loop
      G.Begin_Frame (S, Screen, 0, OK); pragma Assert (OK);
      for N in 1 .. (if Layers = 1 then 900 else Vulkan_Scene.Maximum_Layers + 1) loop
         G.Drawing.Fill (S, (0, 0, 1, 1), 16#00FF_0000#, OK);
      end loop;
      G.Finish (S, Result);
      if Layers = 1 then
         pragma Assert (Result = G.Pending and G.Last_Failure (S) = G.No_Failure);
         G.Poll (S, Result); pragma Assert (Result = G.Complete);
      else
         pragma Assert (Result = G.Rejected and G.Last_Failure (S) = G.Layer_Limit);
      end if;
   end loop;
   pragma Assert (G.Peak_Layers (S) = Vulkan_Scene.Maximum_Layers);
   G.Close (S, OK); pragma Assert (OK);
   R.Close (Reg, True, OK); pragma Assert (OK);
   D.Stop;
   Ada.Text_IO.Put_Line ("PASS desktop scene sources:" & Frames'Image & " frames x" & Windows'Image &
     " windows, every window republished per frame, no slot pressure, all frames complete," &
     Cold'Image & " cold recaptures, zero allocations after warm-up; 1800-layer scene submits, overflow reports Layer_Limit");
end Desktop_Scene_Sources_Tests;
