with Ada.Command_Line; with Ada.Text_IO; with Interfaces;
with Desktop_Vulkan_Startup; with Vulkan_Device_Mock; with Vulkan_Upload_Owner;
with Vulkan_Owned_Targets; with Vulkan_Scene;
procedure Desktop_Vulkan_Upload_Tests is
   package S renames Desktop_Vulkan_Startup;
   package M renames Vulkan_Device_Mock;
   package U renames Vulkan_Upload_Owner;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type U32, U.Phase, S.Source_Result, S.Frame_Result, S.Poll_Result;
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Health_Set (Value : U32) with Import, Convention => C, External_Name => "device_mock_health_set";
   procedure Metadata_Set (Value : U32) with Import, Convention => C, External_Name => "upload_metadata_mock_set";
   function Metadata_Calls return U32 with Import, Convention => C, External_Name => "upload_metadata_mock_calls";
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   function Binds return U32 with Import, Convention => C, External_Name => "upload_mock_binds";
   function Releases return U32 with Import, Convention => C, External_Name => "upload_mock_releases";
   OK, Released : Boolean; Calls : U32;
   Lease : Vulkan_Owned_Targets.A.Ticket; Source : S.Source_Result;
   Frame : S.Frame_Result; Poll : S.Poll_Result;
   Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, (1, 1), 0, 0));
begin
   Reset; Context_Set (0, 0); M.Set (True, True, 0); Pipeline_Set (0, 0);
   Image_Set (4096, 1, 0, 0, 0); Metadata_Set (0);
   S.Configure_Upload (384, OK); pragma Assert (not OK and Metadata_Calls = 0);
   S.Initialize (25); S.Configure_Upload (384, OK); pragma Assert (not OK and Metadata_Calls = 0);
   S.Configure_Targets (32, 24, 1, (if Scenario = 1 then 12288 else 36864), OK); pragma Assert (OK);
   S.Configure_Upload (384, OK); pragma Assert (not OK and Metadata_Calls = 0);
   S.Prepare_Pipeline (OK); pragma Assert (OK);
   Upload_Set (4096, 1, (if Scenario = 3 then 2 elsif Scenario = 9 then 1 else 0),
      (if Scenario = 4 then 2 elsif Scenario = 10 then 1 else 0),
      (if Scenario = 5 then 2 else 0), (if Scenario = 8 then 1 else 0));
   if Scenario = 2 then Metadata_Set (1); end if;
   S.Configure_Upload (384, OK);
   pragma Assert (Metadata_Calls = 1 and S.Configured_Limit = (if Scenario = 1 then 12288 else 36864));
   if Scenario in 1 .. 4 | 8 .. 10 then
      pragma Assert (not OK and S.Upload_Capacity = 0);
      pragma Assert (S.Charged_Bytes = (if Scenario in 4 | 8 then 16384 else 12288));
      if Scenario in 1 .. 3 | 9 then pragma Assert (Binds = 0); end if;
      if Scenario in 3 | 4 | 8 then
         pragma Assert (S.Upload_Phase = U.Quarantined);
         S.Configure_Upload (384, OK); pragma Assert (not OK and Metadata_Calls = 1);
      elsif Scenario /= 1 then
         -- Clean rejection permits a new attempt, without resetting owners.
         Metadata_Set (0); Upload_Set (4096, 1, 0, 0, 0, 0);
         S.Configure_Upload (384, OK); pragma Assert (OK and S.Charged_Bytes = 16384);
      end if;
   else
      pragma Assert (OK and S.Upload_Capacity = 384 and S.Charged_Bytes = 16384);
      S.Configure_Upload (768, OK); pragma Assert (not OK and Metadata_Calls = 1 and S.Upload_Capacity = 384);
      if Scenario = 0 then
         -- All nine accounting slots and all eight context children coexist.
         for Index in S.Backing_Slot loop
            S.Allocate_Backing (Index, 32, 24, False, Lease, Source);
            pragma Assert (Source = S.Source_Accepted);
         end loop;
         pragma Assert (S.Charged_Bytes = 36864);
         for Cycle in 1 .. 32 loop
            S.Release_Upload (Released); pragma Assert (Released and S.Charged_Bytes = 32768);
            S.Configure_Upload (384, OK); pragma Assert (OK and S.Charged_Bytes = 36864);
         end loop;
      elsif Scenario = 6 then
         Vulkan_Scene.Seal (Scene, OK); pragma Assert (OK);
         S.Render (Scene, Frame); pragma Assert (Frame = S.Submitted);
         S.Release_Upload (Released); pragma Assert (not Released and Releases = 0);
         S.Stop; pragma Assert (M.Closes = 0 and S.Upload_Phase = U.Live and Releases = 0);
         S.Poll_Frame (Poll); pragma Assert (Poll = S.Completed);
      elsif Scenario = 7 then
         Health_Set (1); S.Check_Health (OK); pragma Assert (not OK);
         S.Release_Upload (Released); pragma Assert (not Released and Releases = 0);
      else
         S.Release_Upload (Released); pragma Assert (not Released and S.Upload_Phase = U.Quarantined);
      end if;
   end if;
   Calls := Metadata_Calls;
   S.Stop;
   pragma Assert (M.Closes = (if Scenario in 3 | 4 | 5 | 7 | 8 then 0 else 1));
   if Scenario not in 3 | 4 | 5 | 7 | 8 then pragma Assert (S.Charged_Bytes = 0); end if;
   S.Configure_Upload (384, OK); pragma Assert (not OK and Metadata_Calls = Calls);
   Ada.Text_IO.Put_Line ("PASS Desktop upload admission scenario" & Natural'Image (Scenario));
end Desktop_Vulkan_Upload_Tests;
