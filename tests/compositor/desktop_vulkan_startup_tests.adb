with Ada.Text_IO;
with Interfaces;
with Desktop_Vulkan_Startup;
with Vulkan_Device_Mock;
with Vulkan_Device_Owner;
procedure Desktop_Vulkan_Startup_Tests is
   package S renames Desktop_Vulkan_Startup;
   package M renames Vulkan_Device_Mock;
   use type Vulkan_Device_Owner.Phase, Interfaces.Unsigned_32;
   procedure Context_Set (Create, Release : Interfaces.Unsigned_32)
     with Import, Convention => C, External_Name => "context_mock_set";
   Usable : Boolean;
begin
   M.Set (True, True, 0); Context_Set (0, 0);
   S.Initialize (25);
   pragma Assert (S.Current = Vulkan_Device_Owner.Ready and M.Starts = 1);
   S.Initialize (26);
   pragma Assert (M.Starts = 1);
   S.Check_Health (Usable); pragma Assert (Usable);
   S.Stop;
   pragma Assert (S.Current = Vulkan_Device_Owner.Retired and M.Closes = 1);
   S.Initialize (25); S.Stop;
   S.Check_Health (Usable);
   pragma Assert (not Usable and M.Starts = 1 and M.Closes = 1);
   Ada.Text_IO.Put_Line ("PASS Desktop singleton device/context startup, health and retirement");
end Desktop_Vulkan_Startup_Tests;
