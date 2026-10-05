with System;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with Desktop_Vulkan_Startup;
with Vulkan_Device_Mock;
with Vulkan_Scene;
procedure Desktop_Vulkan_Presentation_Tests is
   package S renames Desktop_Vulkan_Startup;
   package M renames Vulkan_Device_Mock;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   use type S.Frame_Result, S.Poll_Result, S.Capture_Admission, U32;
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
   function Last_Pass return System.Address with Import, Convention => C, External_Name => "submission_mock_last_pass";
   use type System.Address, S.Presentation_Ticket, U64;
   Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, (1, 1), 0, 0));
   Front, Pending, Latest, None, Bad : S.Presentation_Ticket;
   Front_Image, Pending_Image : System.Address;
   OK : Boolean; Frame : S.Frame_Result; Poll : S.Poll_Result;
   procedure Check_Admission (Expected : S.Capture_Admission) is
      Before : array (U32 range 0 .. 13) of U32;
      Held_Front : constant S.Presentation_Ticket := S.Presentation_Front;
      Held_Pending : constant S.Presentation_Ticket := S.Presentation_Pending;
      Bytes : constant Natural := S.Charged_Bytes;
   begin
      for I in Before'Range loop Before (I) := Calls (I); end loop;
      for I in 1 .. 32 loop
         pragma Assert (S.Admit_Capture (Vulkan_Scene.Output (Scene)) = Expected);
      end loop;
      for I in Before'Range loop pragma Assert (Before (I) = Calls (I)); end loop;
      pragma Assert (S.Presentation_Front = Held_Front and
                     S.Presentation_Pending = Held_Pending and S.Charged_Bytes = Bytes);
   end Check_Admission;
   procedure Draw is
   begin
      Check_Admission (S.Capture_Allowed);
      S.Damage_Output ((0, 0, 32, 24), OK); pragma Assert (OK);
      S.Render (Scene, Frame); pragma Assert (Frame = S.Submitted);
      Check_Admission (S.Capture_Busy);
      S.Poll_Frame (Poll); pragma Assert (Poll = S.Completed);
      Check_Admission (S.Capture_Allowed);
   end Draw;
begin
   Pipeline_Set (0, 0); Reset; Context_Set (0, 0); M.Set (True, True, 0);
   Image_Set (4096, 1, 0, 0, 0); Bind_Fault (0);
   Vulkan_Scene.Seal (Scene, OK); pragma Assert (OK);
   S.Initialize (25); S.Configure_Targets (32, 24, 7, 16384, OK); pragma Assert (OK);
   Check_Admission (S.Capture_Unavailable);
   S.Prepare_Pipeline (OK); pragma Assert (OK);
   Check_Admission (S.Capture_Allowed);
   S.Take_Presentation (None); pragma Assert (None = S.No_Presentation);
   Draw; Front_Image := Last_Pass;
   S.Take_Presentation (Front); pragma Assert (Front /= S.No_Presentation and Front.Epoch = 7);
   S.Confirm_Presentation (Front, S.No_Presentation, True, OK); pragma Assert (OK and S.Presentation_Front = Front);
   Draw; Pending_Image := Last_Pass; pragma Assert (Pending_Image /= Front_Image);
   S.Take_Presentation (Pending); pragma Assert (Pending /= S.No_Presentation and Pending.Buffer /= Front.Buffer);
   for I in 1 .. 64 loop
      Draw; pragma Assert (Last_Pass /= Front_Image and Last_Pass /= Pending_Image);
      S.Take_Presentation (None); pragma Assert (None = S.No_Presentation and S.Presentation_Pending = Pending);
   end loop;
   if Scenario in 5 | 9 then
      if Scenario = 9 then
         S.Stop; pragma Assert (M.Closes = 0 and S.Charged_Bytes = 12288);
         Check_Admission (if S.Presentation_Faulted then S.Capture_Uncertain else S.Capture_Unavailable);
      end if;
      S.Cancel_Presentation (Pending, True, OK);
      pragma Assert (OK and not S.Presentation_Faulted and S.Presentation_Pending = S.No_Presentation and S.Presentation_Front = Front);
      if Scenario = 5 then
         S.Take_Presentation (Latest); pragma Assert (Latest /= S.No_Presentation and Latest.Serial > Pending.Serial + 1);
         S.Confirm_Presentation (Latest, Front, True, OK); pragma Assert (OK);
         -- The cancelled slot and prior front can be used again, while the new
         -- visible target remains protected until output retirement.
         for I in 1 .. 64 loop Draw; end loop;
         S.Retire_Presentation (Latest, True, OK); pragma Assert (OK);
      else
         S.Retire_Presentation (Front, True, OK); pragma Assert (OK);
      end if;
      S.Stop; pragma Assert (M.Closes = 1 and S.Charged_Bytes = 0);
      Ada.Text_IO.Put_Line ("PASS confirmed presentation cancellation preserves front/latest frame and permits cleanup scenario" & Scenario'Image); return;
   end if;
   if Scenario /= 0 then
      Bad := Pending;
      case Scenario is
         when 1 => Bad.Epoch := Bad.Epoch + 1; S.Confirm_Presentation (Bad, Front, True, OK);
         when 2 => S.Confirm_Presentation (Pending, S.No_Presentation, True, OK);
         when 3 => S.Confirm_Presentation (Pending, Front, False, OK);
         when 4 => S.Retire_Presentation (Pending, True, OK);
         when 6 => Bad.Serial := Bad.Serial + 1; S.Cancel_Presentation (Bad, True, OK);
         when 7 => S.Cancel_Presentation (Pending, False, OK);
         when others => S.Cancel_Presentation (S.No_Presentation, True, OK);
      end case;
      pragma Assert (not OK and S.Presentation_Faulted and S.Presentation_Front = Front and S.Presentation_Pending = Pending);
      Check_Admission (S.Capture_Uncertain);
      S.Take_Presentation (None); pragma Assert (None = S.No_Presentation);
      S.Render (Scene, Frame); pragma Assert (Frame /= S.Submitted);
      S.Stop; pragma Assert (M.Closes = 0 and S.Charged_Bytes = 12288);
         Check_Admission (if S.Presentation_Faulted then S.Capture_Uncertain else S.Capture_Unavailable);
      Ada.Text_IO.Put_Line ("PASS stale/uncertain display evidence retains all target storage scenario" & Scenario'Image); return;
   end if;
   S.Confirm_Presentation (Pending, Front, True, OK); pragma Assert (OK);
   S.Take_Presentation (Latest); pragma Assert (Latest /= S.No_Presentation and Latest.Serial > Pending.Serial + 1);
   S.Confirm_Presentation (Latest, Pending, True, OK); pragma Assert (OK);
   S.Stop; pragma Assert (M.Closes = 0 and S.Charged_Bytes = 12288 and S.Presentation_Front = Latest);
   Check_Admission (S.Capture_Unavailable);
   S.Retire_Presentation (Latest, True, OK); pragma Assert (OK);
   S.Stop; pragma Assert (M.Closes = 1 and S.Charged_Bytes = 0);
   Ada.Text_IO.Put_Line ("PASS GPU owner presentation: one pending, protected front, 64 latest-frame replacements, exact latch and final retirement");
end Desktop_Vulkan_Presentation_Tests;
