with Ada.Text_IO; with Interfaces;
with Desktop_Glyph_Residency; with Desktop_Vulkan_Startup; with Vulkan_Device_Mock;
with Vulkan_Glyph_Sources; with Vulkan_Submission; with Vulkan_Scene;
procedure Desktop_Glyph_Uncertain_Tests is
   package R renames Desktop_Glyph_Residency; package D renames Desktop_Vulkan_Startup;
   package V renames Vulkan_Submission;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type U32, R.Outcome, R.C.Lease, V.Source_Ticket, D.Frame_Result, D.Poll_Result;
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Metadata_Set (Value : U32) with Import, Convention => C, External_Name => "source_metadata_mock_set";
   procedure Font_Set (Value : U32) with Import, Convention => C, External_Name => "residency_font_set";
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   S : R.State;
   Source : V.Source_Ticket;
   Extra : R.C.Lease;
   Result : R.Outcome; OK : Boolean;
   New_Key : constant Vulkan_Glyph_Sources.Key := (1, 65, (5, 4));
   Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, (5, 4), 0, 0));
   Frame : D.Frame_Result; Poll : D.Poll_Result;
begin
   Reset; Context_Set (0, 0); Vulkan_Device_Mock.Set (True, True, 0); Pipeline_Set (0, 0);
   Image_Set (4096, 1, 0, 0, 0); Metadata_Set (0); Font_Set (0);
   Upload_Set (4096, 1, 0, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, 140 * 4096, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK); D.Configure_Upload (2048, OK); pragma Assert (OK);

   R.Acquire (S, New_Key, Source, Extra, Result); pragma Assert (Result = R.Uploading);
   R.Poll (S, Result); pragma Assert (Result = R.Available);
   R.Acquire (S, New_Key, Source, Extra, Result); pragma Assert (Result = R.Available);
   D.Capture_Glyph (Scene, New_Key, (0, 0, 32, 17), 16#FFFFFFFF#, OK); pragma Assert (OK);
   Vulkan_Scene.Seal (Scene, OK); pragma Assert (OK);
   D.Render (Scene, Frame); pragma Assert (Frame = D.Submitted);
   Set (3, 2); D.Poll_Frame (Poll); pragma Assert (Poll = D.GPU_Failed);
   pragma Assert (not D.Frame_Pending);
   R.Release (S, Extra, True); pragma Assert (R.Held (S, Extra));
   R.Close (S, OK); pragma Assert (not OK and R.Charged (S) > 0);
   Ada.Text_IO.Put_Line ("PASS uncertain frame retains glyph reader and allocations");
end Desktop_Glyph_Uncertain_Tests;
