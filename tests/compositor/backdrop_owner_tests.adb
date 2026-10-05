with Ada.Text_IO; with Ada.Command_Line; with Interfaces;
with Desktop_Backdrop_Owner; with Vulkan_Scene; with Desktop_Vulkan_Startup;
with Vulkan_Owned_Targets; with Vulkan_Submission; with Vulkan_Device_Mock;
with Wallpaper_Assets; with CuBit.Appearance; with System;
procedure Backdrop_Owner_Tests is
   package D renames Desktop_Vulkan_Startup;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type D.Source_Result, D.Poll_Result, Vulkan_Submission.Source_Ticket, System.Address, U32;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (I, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   function Calls (I : U32) return U32 with Import, Convention => C, External_Name => "submission_mock_calls";
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Upload_Set (Bytes : U64; Types, Prep, Bind, Release, Null_Map : U32) with Import, Convention => C, External_Name => "upload_mock_set";
   package O renames Desktop_Backdrop_Owner;
   use type O.Outcome, O.Phase, D.Frame_Result;
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   Asset : constant CuBit.Appearance.Background :=
     (if Scenario = 1 then CuBit.Appearance.Cubie else CuBit.Appearance.Wallpaper);
   Chunks : constant Positive := (if Scenario = 1 then 144 else 72);
   Owner, Other : O.State; Source, Again : Vulkan_Submission.Source_Ticket;
   Result : O.Outcome; Poll : D.Poll_Result; OK : Boolean;
   Queues : U32; Frame : D.Frame_Result;
   Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, (1, 1), 0, 0));
begin
   Reset; Context_Set (0, 0); Pipeline_Set (0, 0); Image_Set (4096, 1, 0, 0, 0);
   Vulkan_Device_Mock.Set (True, True, 0); Upload_Set (65536, 1, 0, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, 1048576, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK); D.Configure_Upload (65536, OK); pragma Assert (OK);
   O.Acquire (Owner, 128, Asset, Source, Result); pragma Assert (Result = O.Pending and Source = Vulkan_Submission.No_Source);
   O.Close (Owner, True, OK); pragma Assert (not OK);
   O.Acquire (Other, 128, Asset, Again, Result); pragma Assert (Result /= O.Available and Again = Vulkan_Submission.No_Source);
   if Scenario = 2 then
      Set (3, 2); O.Poll (Owner, Result); pragma Assert (Result = O.Unsafe and O.Current (Owner) = O.Quarantined);
      O.Close (Owner, True, OK); pragma Assert (not OK and D.Charged_Bytes > 65536);
      O.Acquire (Owner, 128, Asset, Source, Result); pragma Assert (Result = O.Unsafe and Source = Vulkan_Submission.No_Source);
      Ada.Text_IO.Put_Line ("PASS retained wallpaper owner quarantines uncertain completion without refund"); return;
   end if;
   for Chunk in 1 .. Chunks loop
      Set (3, 1);
      for I in 1 .. 10 loop O.Poll (Owner, Result); pragma Assert (Result = O.Pending); end loop;
      Set (3, 0); O.Poll (Owner, Result);
      pragma Assert (Result = (if Chunk = Chunks then O.Available else O.Pending));
   end loop;
   Queues := Calls (2); pragma Assert (Queues = U32 (Chunks));
   for I in 1 .. 1000 loop
      O.Acquire (Owner, 128, Asset, Source, Result);
      pragma Assert (Result = O.Available and D.Source_Held (Source) and Calls (2) = Queues);
   end loop;
   O.Acquire (Owner, 129, Asset, Again, Result); pragma Assert (Result = O.Rejected and Again = Vulkan_Submission.No_Source);
   O.Close (Owner, False, OK); pragma Assert (not OK and D.Source_Held (Source));
   Vulkan_Scene.Seal (Scene, OK); pragma Assert (OK);
   D.Render (Scene, Frame); pragma Assert (Frame = D.Submitted);
   O.Close (Owner, True, OK); pragma Assert (not OK and D.Source_Held (Source));
   D.Poll_Frame (Poll); pragma Assert (Poll = D.Completed);
   O.Close (Owner, True, OK); pragma Assert (OK and O.Current (Owner) = O.Closed);
   O.Acquire (Owner, 128, Asset, Again, Result); pragma Assert (Result = O.Rejected);
   O.Close (Other, True, OK); pragma Assert (OK); D.Stop; pragma Assert (D.Charged_Bytes = 0);
   Ada.Text_IO.Put_Line ("PASS retained wallpaper owner:" & Chunks'Image & " chunks, 1000 cache hits without upload, capture/frame retirement gates, complete refund");
end Backdrop_Owner_Tests;
