with Ada.Command_Line; with Ada.Text_IO; with Interfaces;
with System; with System.Storage_Elements;
with Desktop_Vulkan_Startup; with Vulkan_Device_Mock;
with Vulkan_Scene; with Vulkan_Submission;
procedure Desktop_Vulkan_Sources_Tests is
   package S renames Desktop_Vulkan_Startup;
   package M renames Vulkan_Device_Mock;
   package V renames Vulkan_Submission;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   use type U32, S.Source_Result, S.Frame_Result, S.Poll_Result, V.Source_Ticket, System.Address;
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   function Calls (Index : U32) return U32 with Import, Convention => C, External_Name => "submission_mock_calls";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Health_Set (Value : U32) with Import, Convention => C, External_Name => "device_mock_health_set";
   function Addr (N : Natural) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
   Ticket, Other : V.Source_Ticket;
   Result : S.Source_Result;
   Released : System.Address;
   OK : Boolean;
   Frame : S.Frame_Result;
   Poll : S.Poll_Result;
   Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, (1, 1), 0, 0));
begin
   Reset; Context_Set (0, 0); M.Set (True, True, 0); Pipeline_Set (0, 0); Image_Set (4096, 1, 0, 0, 0);
   S.Import_Owned_Source (5, Addr (55), Ticket, Result);
   pragma Assert (Result = S.Source_Rejected and Calls (9) = 0);
   S.Initialize (25); S.Configure_Targets (32, 24, 1, 16384, OK); pragma Assert (OK);
   S.Prepare_Pipeline (OK); pragma Assert (OK);
   if Scenario in 1 | 2 then Set (9, U32 (Scenario)); end if;
   S.Import_Owned_Source (5, Addr (55), Ticket, Result);
   if Scenario in 1 | 2 then
      pragma Assert (Ticket = V.No_Source);
      pragma Assert (Result = (if Scenario = 1 then S.Source_Rejected else S.Source_Unsafe));
   else
      pragma Assert (Result = S.Source_Accepted and S.Source_Held (Ticket));
      S.Import_Owned_Source (5, Addr (56), Other, Result);
      pragma Assert (Other = V.No_Source and Calls (9) = 1);
      Vulkan_Scene.Append (Scene, (Ticket, (0, 0, 32, 24), False, False, 0, Vulkan_Scene.Textured), OK);
      pragma Assert (OK); Vulkan_Scene.Seal (Scene, OK); pragma Assert (OK);
      if Scenario = 5 then
         S.Release_Source (Ticket, Released); pragma Assert (Released = Addr (55));
         S.Import_Owned_Source (5, Addr (56), Other, Result);
         pragma Assert (Result = S.Source_Accepted and Other /= Ticket);
         S.Release_Source (Ticket, Released); pragma Assert (Released = System.Null_Address and Calls (10) = 1);
         S.Render (Scene, Frame); pragma Assert (Frame = S.Rejected and Calls (2) = 0);
         S.Release_Source (Other, Released); pragma Assert (Released = Addr (56));
      else
         S.Render (Scene, Frame); pragma Assert (Frame = S.Submitted);
         S.Import_Owned_Source (6, Addr (56), Other, Result);
         pragma Assert (Result = S.Source_Busy and Calls (9) = 1);
         S.Release_Source (Ticket, Released);
         pragma Assert (Released = System.Null_Address and Calls (10) = 0 and S.Source_Held (Ticket));
         if Scenario = 4 then
            Health_Set (1); S.Check_Health (OK); pragma Assert (not OK);
            S.Release_Source (Ticket, Released); pragma Assert (Released = System.Null_Address and Calls (10) = 0);
         else
            S.Poll_Frame (Poll); pragma Assert (Poll = S.Completed);
            S.Stop; pragma Assert (M.Closes = 0 and S.Source_Held (Ticket));
            if Scenario = 3 then Set (10, 2); end if;
            S.Release_Source (Ticket, Released);
            if Scenario = 3 then pragma Assert (Released = System.Null_Address and S.Source_Held (Ticket));
            else pragma Assert (Released = Addr (55) and not S.Source_Held (Ticket));
            end if;
         end if;
      end if;
   end if;
   S.Stop;
   pragma Assert (M.Closes = (if Scenario in 2 | 3 | 4 then 0 else 1));
   Ada.Text_IO.Put_Line ("PASS Desktop retained source scenario" & Natural'Image (Scenario));
end Desktop_Vulkan_Sources_Tests;
