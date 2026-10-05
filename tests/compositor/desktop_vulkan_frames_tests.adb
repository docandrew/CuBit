with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with Desktop_Vulkan_Startup;
with Vulkan_Device_Mock;
with Vulkan_Scene;
procedure Desktop_Vulkan_Frames_Tests is
   package S renames Desktop_Vulkan_Startup;
   package M renames Vulkan_Device_Mock;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   use type S.Frame_Result, S.Poll_Result, U32;
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   procedure Context_Set (Create, Release : U32)
     with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32)
     with Import, Convention => C, External_Name => "image_mock";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   function Calls (Index : U32) return U32 with Import, Convention => C, External_Name => "submission_mock_calls";
   procedure Bind_Fault (Value : U32) with Import, Convention => C, External_Name => "owned_target_mock_bind";
   procedure Prepare_Fault (Value : U32) with Import, Convention => C, External_Name => "owned_target_mock_prepare";
   procedure Health_Set (Value : U32) with Import, Convention => C, External_Name => "device_mock_health_set";
   procedure Pipeline_Set (Create, Close : U32)
     with Import, Convention => C, External_Name => "pipeline_mock_set";
   function Pipeline_Closes return U32
     with Import, Convention => C, External_Name => "pipeline_mock_closes";
   Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, (1, 1), 0, 0));
   OK : Boolean;
   Frame : S.Frame_Result;
   Poll : S.Poll_Result;
begin
   Pipeline_Set (0, 0);
   Reset; Context_Set (0, 0); M.Set (True, True, 0);
   Image_Set (4096, 1, 0, 0, 0); Bind_Fault (0);
   Vulkan_Scene.Append_Physical_Fill (Scene, (1, 1, 4, 3), 16#ABCDEF#, OK); pragma Assert (OK);
   Vulkan_Scene.Seal (Scene, OK); pragma Assert (OK);
   S.Render (Scene, Frame); pragma Assert (Frame = S.Rejected and Calls (2) = 0);
   S.Initialize (25); S.Configure_Targets (32, 24, 1, 16384, OK); pragma Assert (OK);
   S.Prepare_Pipeline (OK); pragma Assert (OK and S.Pipeline_Ready);
   if Scenario = 3 then Set (2, 2); end if;
   if Scenario = 5 then Prepare_Fault (2); end if;
   S.Render (Scene, Frame);
   if Scenario = 5 then
      pragma Assert (Frame = S.Rejected and Calls (2) = 0 and Calls (4) = 1);
   elsif Scenario = 3 then
      pragma Assert (Frame = S.Failed and Calls (2) = 1);
   else
      pragma Assert (Frame = S.Submitted and S.Frame_Pending and Calls (2) = 1);
      for N in 1 .. 100 loop
         S.Render (Scene, Frame);
         pragma Assert (Frame = S.Deferred and Calls (2) = 1 and Calls (0) = 1);
      end loop;
      S.Damage_Output ((1, 1, 4, 3), OK); pragma Assert (OK);
      S.Damage_Output ((0, 0, 33, 24), OK); pragma Assert (not OK);
      if Scenario = 1 then
         S.Stop; pragma Assert (M.Closes = 0 and Pipeline_Closes = 0 and S.Charged_Bytes = 12288);
         S.Render (Scene, Frame); pragma Assert (Frame = S.Rejected and Calls (2) = 1);
      end if;
      if Scenario = 4 then
         Health_Set (1); S.Check_Health (OK); pragma Assert (not OK);
         S.Poll_Frame (Poll); pragma Assert (Poll = S.GPU_Failed and Calls (3) = 0);
      elsif Scenario = 2 then
         Set (3, 2); S.Poll_Frame (Poll); pragma Assert (Poll = S.GPU_Failed);
      else
         if Scenario = 1 then
            Set (3, 1);
            for N in 1 .. 100 loop
               S.Poll_Frame (Poll); pragma Assert (Poll = S.Pending and Calls (3) = U32 (N));
            end loop;
            Set (3, 0);
         end if;
         S.Poll_Frame (Poll); pragma Assert (Poll = S.Completed and not S.Frame_Pending);
         if Scenario /= 1 then
            S.Render (Scene, Frame); pragma Assert (Frame = S.Submitted and Calls (2) = 2);
            S.Poll_Frame (Poll); pragma Assert (Poll = S.Completed);
         end if;
      end if;
   end if;
   S.Stop;
   if Scenario in 2 | 3 | 4 then
      pragma Assert (M.Closes = 0 and Pipeline_Closes = 0 and S.Charged_Bytes = 12288);
      S.Render (Scene, Frame); pragma Assert (Frame /= S.Submitted and Calls (2) = 1);
   else
      pragma Assert (M.Closes = 1 and Pipeline_Closes = 1 and S.Charged_Bytes = 0);
   end if;
   Ada.Text_IO.Put_Line ("PASS Desktop bounded frame scenario" & Natural'Image (Scenario));
end Desktop_Vulkan_Frames_Tests;
