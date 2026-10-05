with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with Desktop_Vulkan_Startup;
with Vulkan_Device_Mock;
procedure Desktop_Vulkan_Pipeline_Tests is
   package S renames Desktop_Vulkan_Startup;
   package M renames Vulkan_Device_Mock;
   subtype U32 is Interfaces.Unsigned_32;
   use type U32;
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   procedure Context_Set (Create, Release : U32)
     with Import, Convention => C, External_Name => "context_mock_set";
   procedure Set (Create, Close : U32)
     with Import, Convention => C, External_Name => "pipeline_mock_set";
   function Creates return U32 with Import, Convention => C, External_Name => "pipeline_mock_creates";
   function Closes return U32 with Import, Convention => C, External_Name => "pipeline_mock_closes";
   OK : Boolean;
begin
   Context_Set (0, 0); M.Set (True, True, 0);
   Set ((if Scenario = 1 then 1 elsif Scenario = 2 then 2 else 0),
        (if Scenario = 3 then 2 else 0));
   S.Prepare_Pipeline (OK); pragma Assert (not OK and Creates = 0);
   S.Initialize (if Scenario = 4 then 0 else 25);
   S.Prepare_Pipeline (OK);
   pragma Assert (OK = (Scenario in 0 | 3));
   pragma Assert (Creates = (if Scenario = 4 then 0 else 1));
   S.Prepare_Pipeline (OK); pragma Assert (not OK and Creates <= 1);
   S.Stop;
   if Scenario in 2 | 3 then
      pragma Assert (M.Closes = 0 and not S.Pipeline_Ready);
   else pragma Assert (M.Closes = (if Scenario = 4 then 0 else 1));
   end if;
   pragma Assert (Closes = (if Scenario in 0 | 3 then 1 else 0));
   S.Stop; S.Prepare_Pipeline (OK);
   pragma Assert (not OK and Creates <= 1 and Closes <= 1);
   Ada.Text_IO.Put_Line ("PASS Desktop pipeline context ownership" & Natural'Image (Scenario));
end Desktop_Vulkan_Pipeline_Tests;
