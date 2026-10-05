with Vulkan_Scene;
with Ada.Command_Line; with Ada.Text_IO; with Interfaces;
with Desktop_Glyph_Upload; with Desktop_Vulkan_Startup;
with Vulkan_Glyph_Sources; with Vulkan_Device_Mock;
with Vulkan_Owned_Targets; with Vulkan_Submission;
with System;
procedure Desktop_Glyph_Upload_Tests is
   package D renames Desktop_Vulkan_Startup;
   package A renames Vulkan_Owned_Targets.A;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type Vulkan_Scene.Phase, U32, D.Source_Result, D.Poll_Result, Vulkan_Submission.Source_Ticket, System.Address;
   Case_No : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Metadata_Set (Value : U32) with Import, Convention => C, External_Name => "source_metadata_mock_set";
   procedure Font_Set (Value : U32) with Import, Convention => C, External_Name => "glyph_upload_mock_set";
   function Font_Calls return U32 with Import, Convention => C, External_Name => "glyph_upload_mock_calls";
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   Lease : A.Ticket;
   Key : constant Vulkan_Glyph_Sources.Key := (0, 65, (5, 4));
   Result : D.Source_Result; Poll : D.Poll_Result; OK : Boolean;
   Advance : Natural;
   Source : Vulkan_Submission.Source_Ticket;
   Retired : System.Address;
   Scene : Vulkan_Scene.State;
   Other_Key : Vulkan_Glyph_Sources.Key := Key;
   procedure Capture (Expected : Boolean; Scale : Vulkan_Scene.A.G.UI_Scale; Value : Vulkan_Glyph_Sources.Key) is
   begin
      Scene := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, Scale, 0, 0));
      D.Capture_Glyph (Scene, Value, (0, 0, 32, 17), 16#FFFFFFFF#, OK);
      pragma Assert (OK = Expected);
      pragma Assert (Vulkan_Scene.Current (Scene) = (if Expected then Vulkan_Scene.Collecting else Vulkan_Scene.Rejected));
   end Capture;
begin
   Reset; Context_Set (0, 0); Vulkan_Device_Mock.Set (True, True, 0);
   Pipeline_Set (0, 0); Image_Set (4096, 1, 0, 0, 0); Metadata_Set (0); Font_Set (U32 (Case_No));
   Upload_Set (4096, 1, 0, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, 32768, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK);
   D.Configure_Upload ((if Case_No = 4 then 128 else 2048), OK); pragma Assert (OK);
   D.Allocate_Backing (127, (if Case_No = 6 then 41 else 40), 22, Case_No /= 5, Lease, Result);
   pragma Assert (Result = D.Source_Accepted);
   Desktop_Glyph_Upload.Start (127, Lease, Key, Advance, Result);
   if Case_No in 1 .. 7 then
      pragma Assert (Result = D.Source_Rejected and not D.Upload_Pending);
      pragma Assert (Font_Calls = (if Case_No <= 3 or Case_No = 7 then 1 else 0));
      D.Import_Backing (127, Lease, Source, Result);
      pragma Assert (Source = Vulkan_Submission.No_Source and Result = D.Source_Rejected);
   else
      pragma Assert (Result = D.Source_Accepted and Advance = 20 and D.Upload_Pending and Font_Calls = 1);
      Desktop_Glyph_Upload.Start (127, Lease, Key, Advance, Result);
      pragma Assert (Result = D.Source_Busy and Font_Calls = 1);
      D.Import_Backing (127, Lease, Source, Result);
      pragma Assert (Source = Vulkan_Submission.No_Source);
      D.Bind_Glyph (127, Lease, Key, Source, OK); pragma Assert (not OK);
      D.Poll_Upload (Poll); pragma Assert (Poll = D.Completed);
      D.Import_Backing (127, Lease, Source, Result); pragma Assert (Result = D.Source_Accepted);
      D.Bind_Glyph (126, Lease, Key, Source, OK); pragma Assert (not OK);
      Other_Key.Scale := (1, 1);
      D.Bind_Glyph (127, Lease, Other_Key, Source, OK); pragma Assert (not OK);
      D.Bind_Glyph (127, Lease, Key, Source, OK); pragma Assert (OK and D.Glyph_Source (Key) = Source);
      D.Bind_Glyph (127, Lease, Key, Source, OK); pragma Assert (not OK);
      Capture (True, Key.Scale, Key);
      Capture (False, (1, 1), Key);
      Other_Key := Key; Other_Key.Code := 66;
      Capture (False, Key.Scale, Other_Key);
      D.Release_Source (Source, Retired); pragma Assert (Retired /= System.Null_Address);
      pragma Assert (D.Glyph_Source (Key) = Vulkan_Submission.No_Source);
      Capture (False, Key.Scale, Key);
      D.Bind_Glyph (127, Lease, Key, Source, OK); pragma Assert (not OK);
   end if;
   D.Release_Backing (127, Lease, OK); pragma Assert (OK);
   D.Stop; pragma Assert (D.Charged_Bytes = 0 and Vulkan_Device_Mock.Closes = 1);
   Ada.Text_IO.Put_Line ("PASS direct glyph upload policy scenario" & Natural'Image (Case_No));
end Desktop_Glyph_Upload_Tests;
