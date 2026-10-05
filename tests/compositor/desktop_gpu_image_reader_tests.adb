with Ada.Command_Line; with Ada.Text_IO; with Interfaces;
with System; with System.Storage_Elements;
with Desktop_GPU_Scene; with Desktop_Vulkan_Startup; with Vulkan_Device_Mock;
with Vulkan_Submission; with Vulkan_Scene;
procedure Desktop_GPU_Image_Reader_Tests is
   package G renames Desktop_GPU_Scene; package D renames Desktop_Vulkan_Startup;
   package V renames Vulkan_Submission;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   use type G.Outcome, G.Phase, D.Source_Result, D.Source_Reader, V.Source_Ticket, System.Address, U32;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   function Calls (Index : U32) return U32 with Import, Convention => C, External_Name => "submission_mock_calls";
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   S : G.State;
   Source, Other : V.Source_Ticket; Imported : D.Source_Result;
   Result : G.Outcome; OK : Boolean; Released : System.Address;
   Address : constant System.Address := System.Storage_Elements.To_Address (55);
   Screen : constant Vulkan_Scene.A.G.Output := (32, 24, Vulkan_Scene.A.G.Unrotated, (1, 1), 0, 0);
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   Readers : array (1 .. Natural (V.Source_Slot'Last) + 1) of D.Source_Reader;
   Old, Extra : D.Source_Reader;
   procedure Capture is
   begin
      G.Begin_Frame (S, Screen, 0, OK); pragma Assert (OK);
      for I in 1 .. 200 loop
         G.Append (S, (Source, (0, 0, 32, 24), False, False, 0, Vulkan_Scene.Textured), OK);
         pragma Assert (OK and G.Image_Reader_Count (S) = 1);
      end loop;
   end Capture;
begin
   Reset; Context_Set (0, 0); Pipeline_Set (0, 0);
   Vulkan_Device_Mock.Set (True, True, 0); Image_Set (4096, 1, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, 16384, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK);
   D.Import_Owned_Source (128, Address, Source, Imported); pragma Assert (Imported = D.Source_Accepted);
   if Scenario = 3 then
      for I in Readers'Range loop
         D.Pin_Source (Source, Readers (I)); pragma Assert (D.Reader_Held (Readers (I)));
      end loop;
      D.Pin_Source (Source, Extra); pragma Assert (Extra = D.No_Source_Reader);
      Old := Readers (1);
      D.Unpin_Source (Old, False, OK); pragma Assert (not OK and D.Reader_Held (Old));
      D.Unpin_Source (Old, True, OK); pragma Assert (OK);
      D.Pin_Source (Source, Readers (1)); pragma Assert (D.Reader_Held (Readers (1)) and Readers (1) /= Old);
      D.Unpin_Source (Old, True, OK); pragma Assert (not OK and D.Reader_Held (Readers (1)));
      D.Unpin_Source (D.No_Source_Reader, True, OK); pragma Assert (not OK);
      D.Release_Source (Source, Released); pragma Assert (Released = System.Null_Address and Calls (10) = 0);
      for I in Readers'Range loop D.Unpin_Source (Readers (I), True, OK); pragma Assert (OK); end loop;
      pragma Assert (not D.Source_Pinned (Source));
   else
      Capture;
      D.Release_Source (Source, Released); pragma Assert (Released = System.Null_Address and Calls (10) = 0);
      if Scenario = 2 then
         D.Stop; pragma Assert (Vulkan_Device_Mock.Closes = 0 and G.Image_Reader_Count (S) = 1);
         G.Discard (S, Result); pragma Assert (Result = G.Retry and G.Image_Reader_Count (S) = 0);
      else
         G.Finish (S, Result); pragma Assert (Result = G.Pending);
         Set (3, 1);
         for I in 1 .. 100 loop
            G.Poll (S, Result); pragma Assert (Result = G.Pending and G.Image_Reader_Count (S) = 1);
            D.Release_Source (Source, Released); pragma Assert (Released = System.Null_Address and Calls (10) = 0);
         end loop;
         if Scenario = 1 then
            Set (3, 2); G.Poll (S, Result); pragma Assert (Result = G.Unsafe and G.Image_Reader_Count (S) = 1);
            D.Release_Source (Source, Released); pragma Assert (Released = System.Null_Address and D.Source_Pinned (Source));
            G.Close (S, OK); pragma Assert (not OK); D.Stop; pragma Assert (Vulkan_Device_Mock.Closes = 0);
            Ada.Text_IO.Put_Line ("PASS uncertain GPU completion retains captured image reader"); return;
         end if;
         Set (3, 0); G.Poll (S, Result); pragma Assert (Result = G.Complete and G.Image_Reader_Count (S) = 0);
         Capture; G.Discard (S, Result); pragma Assert (Result = G.Retry and not D.Source_Pinned (Source));
      end if;
   end if;
   D.Release_Source (Source, Released); pragma Assert (Released = Address);
   if Scenario /= 2 then
      D.Import_Owned_Source (128, Address, Other, Imported); pragma Assert (Imported = D.Source_Accepted and Other /= Source);
      G.Begin_Frame (S, Screen, 0, OK); pragma Assert (OK);
      G.Append (S, (Source, (0, 0, 32, 24), False, False, 0, Vulkan_Scene.Textured), OK); pragma Assert (not OK);
      G.Finish (S, Result); pragma Assert (Result = G.Rejected and G.Image_Reader_Count (S) = 0);
      D.Release_Source (Other, Released); pragma Assert (Released = Address);
   end if;
   G.Close (S, OK); pragma Assert (OK); D.Stop; pragma Assert (Vulkan_Device_Mock.Closes = 1);
   Ada.Text_IO.Put_Line ("PASS scene image reader scenario" & Scenario'Image);
end Desktop_GPU_Image_Reader_Tests;
