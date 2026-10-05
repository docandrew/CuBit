with System.Storage_Elements;
with Ada.Command_Line; with Ada.Text_IO; with Interfaces; with System;
with Desktop_Vulkan_Startup; with Vulkan_Device_Mock; with Vulkan_Owned_Targets;
with Vulkan_Scene; with Vulkan_Submission; with Compositor_Upload;
procedure Desktop_Vulkan_Writer_Tests is
   package S renames Desktop_Vulkan_Startup; package M renames Vulkan_Device_Mock;
   package A renames Vulkan_Owned_Targets.A; package V renames Vulkan_Submission;
   package G renames Compositor_Upload;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type U32, S.Source_Result, S.Frame_Result, S.Poll_Result, S.Write_Ticket,
     A.Ticket, V.Source_Ticket, System.Address;
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   function Calls (Index : U32) return U32 with Import, Convention => C, External_Name => "submission_mock_calls";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Health_Set (Value : U32) with Import, Convention => C, External_Name => "device_mock_health_set";
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   procedure Record_Set (Value : U32) with Import, Convention => C, External_Name => "desktop_upload_record_set";
   Lease, Other_Lease, New_Lease : A.Ticket;
   T, Old, Rejected : S.Write_Ticket;
   Plan, Ignored : G.Plan;
   Mapping, Nothing, Key : System.Address;
   Source : V.Source_Ticket; Result : S.Source_Result;
   Frame : S.Frame_Result; Poll : S.Poll_Result;
   OK : Boolean; Seen : U32;
   Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, (1, 1), 0, 0));
begin
   Reset; Context_Set (0, 0); M.Set (True, True, 0); Pipeline_Set (0, 0);
   Image_Set (4096, 1, 0, 0, 0); Upload_Set (4096, 1, 0, 0, 0, 0); Record_Set (0);
   S.Initialize (25); S.Configure_Targets (32, 24, 1, 32768, OK); pragma Assert (OK);
   S.Prepare_Pipeline (OK); pragma Assert (OK); S.Configure_Upload (128, OK); pragma Assert (OK);
   S.Allocate_Backing (0, 32, 3, False, Lease, Result); pragma Assert (Result = S.Source_Accepted);
   S.Allocate_Backing (1, 32, 3, True, Other_Lease, Result); pragma Assert (Result = S.Source_Accepted);
   pragma Assert (S.Charged_Bytes = 24576);
   if Scenario = 13 then
      S.Import_Owned_Source (5, System.Storage_Elements.To_Address (555), Source, Result);
      pragma Assert (Result = S.Source_Accepted);
      S.Begin_Write (0, Lease, T, Plan, Mapping, Result); pragma Assert (Result = S.Source_Accepted);
      Set (10, 2); -- Would quarantine submission if descriptor retirement ran.
      S.Release_Source (Source, Key);
      pragma Assert (Key = System.Null_Address and Calls (10) = 0 and S.Source_Held (Source) and S.Write_Active (T));
      S.Cancel_Write (T, True, OK); pragma Assert (OK);
      Set (10, 0); S.Release_Source (Source, Key); pragma Assert (Key /= System.Null_Address);
      S.Stop; pragma Assert (M.Closes = 1 and S.Charged_Bytes = 0);
      Ada.Text_IO.Put_Line ("PASS Desktop writer excludes uncertain descriptor retirement"); return;
   end if;
   Vulkan_Scene.Seal (Scene, OK); pragma Assert (OK);
   S.Import_Backing (0, Lease, Source, Result); pragma Assert (Source = V.No_Source and Calls (9) = 0);
   S.Import_Owned_Source (0, System.Null_Address, Source, Result); pragma Assert (Source = V.No_Source and Calls (9) = 0);
   S.Begin_Write (0, Other_Lease, T, Plan, Mapping, Result); pragma Assert (T = S.No_Write and Mapping = System.Null_Address);
   for Row in 0 .. 2 loop
      S.Begin_Write (0, Lease, T, Plan, Mapping, Result);
      pragma Assert (Result = S.Source_Accepted and Mapping /= System.Null_Address and G.Area (Plan).Y = Row and G.Area (Plan).Height = 1);
      S.Release_Upload (OK); pragma Assert (not OK);
      S.Release_Backing (0, Lease, OK); pragma Assert (not OK);
      S.Restart_Content (0, Lease, OK); pragma Assert (not OK);
      S.Begin_Write (1, Other_Lease, Rejected, Ignored, Nothing, Result);
      pragma Assert (Rejected = S.No_Write and Nothing = System.Null_Address and Result = S.Source_Busy);
      S.Render (Scene, Frame); pragma Assert (Frame = S.Deferred);
      if Row = 0 then
         Old := T; S.Cancel_Write (T, True, OK); pragma Assert (OK);
         S.Begin_Write (0, Lease, T, Plan, Mapping, Result); pragma Assert (Result = S.Source_Accepted and T /= Old);
         S.Cancel_Write (Old, True, OK); pragma Assert (not OK);
         S.Submit_Write (Old, True, Result); pragma Assert (Result = S.Source_Rejected);
         if Scenario = 1 then
            S.Stop; pragma Assert (M.Closes = 0 and S.Charged_Bytes = 24576);
            S.Cancel_Write (T, True, OK); pragma Assert (OK); exit;
         elsif Scenario = 3 then
            S.Cancel_Write (T, False, OK); pragma Assert (not OK); exit;
         elsif Scenario in 4 | 5 then
            Record_Set (1); if Scenario = 5 then Set (4, 2); end if;
         elsif Scenario = 6 then Set (1, 2);
         elsif Scenario = 7 then Set (2, 2);
         elsif Scenario = 10 then Health_Set (1); S.Check_Health (OK); pragma Assert (not OK);
         elsif Scenario = 12 then Set (0, 2);
         end if;
      end if;
      S.Submit_Write (T, Scenario /= 2, Result);
      if Scenario = 4 and Row = 0 then
         pragma Assert (Result = S.Source_Rejected and not S.Upload_Pending);
         Record_Set (0); S.Begin_Write (0, Lease, T, Plan, Mapping, Result);
         pragma Assert (Result = S.Source_Accepted and G.Area (Plan).Y = 0);
         S.Submit_Write (T, True, Result);
      end if;
      if Scenario in 2 | 5 | 6 | 7 | 10 | 12 then pragma Assert (Result = S.Source_Unsafe); exit; end if;
      pragma Assert (Result = S.Source_Accepted and S.Upload_Pending and not S.Frame_Pending);
      Seen := Calls (3); S.Poll_Frame (Poll); pragma Assert (Poll = S.Idle and Calls (3) = Seen);
      S.Cancel_Write (T, True, OK); pragma Assert (not OK);
      S.Release_Upload (OK); pragma Assert (not OK);
      S.Import_Backing (0, Lease, Source, Result); pragma Assert (Source = V.No_Source and Calls (9) = 0);
      if Scenario = 11 then S.Stop; pragma Assert (M.Closes = 0 and S.Charged_Bytes = 24576); end if;
      Set (3, 1);
      for N in 1 .. 100 loop S.Poll_Upload (Poll); pragma Assert (Poll = S.Pending); end loop;
      Set (3, (if Scenario = 9 then 2 else 0)); S.Poll_Upload (Poll);
      if Scenario = 9 then pragma Assert (Poll = S.GPU_Failed); exit; end if;
      pragma Assert (Poll = S.Completed and not S.Upload_Pending);
      S.Poll_Upload (Poll); pragma Assert (Poll = S.Idle);
      exit when Scenario = 11;
      if Row < 2 then
         S.Import_Backing (0, Lease, Source, Result); pragma Assert (Source = V.No_Source and Calls (9) = 0);
      end if;
      if Scenario = 0 and Row = 0 then
         -- Completed chunks yield to a frame without publishing partial content.
         S.Render (Scene, Frame); pragma Assert (Frame = S.Submitted and S.Frame_Pending and not S.Upload_Pending);
         Seen := Calls (3); S.Poll_Upload (Poll); pragma Assert (Poll = S.Idle and Calls (3) = Seen);
         S.Begin_Write (0, Lease, Rejected, Ignored, Nothing, Result);
         pragma Assert (Rejected = S.No_Write and Nothing = System.Null_Address and Result = S.Source_Busy);
         S.Poll_Frame (Poll); pragma Assert (Poll = S.Completed);
      end if;
   end loop;
   if Scenario in 0 | 4 | 8 then
      S.Import_Source (0, Mapping, Source, Result); pragma Assert (Source = V.No_Source and Calls (9) = 0);
      S.Import_Backing (0, Lease, Source, Result); pragma Assert (Result = S.Source_Accepted and Source /= V.No_Source);
      S.Restart_Content (0, Lease, OK); pragma Assert (not OK);
      S.Release_Source (Source, Key); pragma Assert (Key /= System.Null_Address);
      S.Restart_Content (0, Lease, OK); pragma Assert (OK);
      S.Import_Backing (0, Lease, Source, Result); pragma Assert (Source = V.No_Source);
      S.Release_Backing (0, Lease, OK); pragma Assert (OK);
      S.Allocate_Backing (0, 32, 3, False, New_Lease, Result); pragma Assert (Result = S.Source_Accepted and Lease /= New_Lease);
      S.Begin_Write (0, New_Lease, T, Plan, Mapping, Result); pragma Assert (Result = S.Source_Accepted);
      S.Submit_Write (Old, True, Result); pragma Assert (Result = S.Source_Rejected);
      S.Cancel_Write (T, True, OK); pragma Assert (OK);
   end if;
   S.Stop;
   if Scenario in 0 | 1 | 4 | 8 | 11 then pragma Assert (M.Closes = 1 and S.Charged_Bytes = 0);
   else pragma Assert (M.Closes = 0 and S.Charged_Bytes = 24576);
   end if;
   Ada.Text_IO.Put_Line ("PASS Desktop writer lifecycle" & Natural'Image (Scenario));
end Desktop_Vulkan_Writer_Tests;
