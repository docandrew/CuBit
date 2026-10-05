with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with System;
with System.Storage_Elements;
with Desktop_Vulkan_Startup;
with Vulkan_Device_Mock;
with Vulkan_Device_Owner;
with Vulkan_Owned_Targets;
with Vulkan_Frame;
procedure Desktop_Vulkan_Targets_Tests is
   package S renames Desktop_Vulkan_Startup;
   package M renames Vulkan_Device_Mock;
   package D renames Vulkan_Device_Owner;
   package O renames Vulkan_Owned_Targets;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   use type D.Phase, O.Phase, U32;
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   procedure Context_Set (Create, Release : U32)
     with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32)
     with Import, Convention => C, External_Name => "image_mock";
   function Binds return U32 with Import, Convention => C, External_Name => "image_mock_binds";
   function Releases return U32 with Import, Convention => C, External_Name => "image_mock_releases";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Bind_Fault (Value : U32)
     with Import, Convention => C, External_Name => "owned_target_mock_bind";
   procedure Health_Set (Value : U32)
     with Import, Convention => C, External_Name => "device_mock_health_set";
   function Addr (N : Natural) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
   Requests : constant Vulkan_Frame.Targets := (Addr (1), Addr (2), Addr (3));
   OK : Boolean;
   function Metadata_Calls return U32
     with Import, Convention => C, External_Name => "device_targets_metadata_calls";
   Before, Metadata_Before : U32;
   Bytes : Natural;
begin
   Reset; Context_Set (0, 0); M.Set (True, True, 0);
   Image_Set (4096, 1, 0, 0, (if Scenario = 3 then 2 else 0));
   Bind_Fault (if Scenario = 2 then 2 else 0);
   -- A premature request must not allocate or consume the one target attempt.
   S.Prepare_Targets (Requests, Addr (44), 1, 16384, 1, OK);
   pragma Assert (not OK and Binds = 0 and S.Target_Phase = O.Fresh);
   S.Configure_Targets (32, 24, 1, 16384, OK);
   pragma Assert (not OK and Metadata_Calls = 0);
   S.Initialize (if Scenario = 4 then 0 else 25);
   if Scenario >= 6 then
      S.Configure_Targets (32, (if Scenario = 7 then 0 else 24), 1, 16384, OK);
      pragma Assert (Metadata_Calls = 1);
   else
      S.Prepare_Targets (Requests, Addr (44), 1,
                        (if Scenario = 1 then 8192 else 16384), 1, OK);
   end if;
   if Scenario = 4 then
      pragma Assert (not OK and Binds = 0 and S.Charged_Bytes = 0);
   elsif Scenario in 1 | 7 then
      pragma Assert (not OK and S.Target_Phase = O.Closed and S.Charged_Bytes = 0);
   elsif Scenario = 2 then
      pragma Assert (not OK and S.Target_Phase = O.Quarantined and S.Charged_Bytes = 12288);
   else
      pragma Assert (OK and S.Target_Phase = O.Live and S.Charged_Bytes = 12288);
   end if;
   Before := Binds; Bytes := S.Charged_Bytes;
   S.Prepare_Targets (Requests, Addr (45), 2, Natural'Last, 1, OK);
   pragma Assert (not OK and Binds = Before and S.Charged_Bytes = Bytes);
   Metadata_Before := Metadata_Calls;
   S.Configure_Targets (32, 24, 2, Natural'Last, OK);
   pragma Assert (not OK and Metadata_Calls = Metadata_Before and S.Charged_Bytes = Bytes);
   if Scenario = 5 then
      Health_Set (1); S.Check_Health (OK);
      pragma Assert (not OK and S.Current = D.Quarantined);
   end if;
   S.Stop;
   if Scenario in 2 | 3 | 5 then
      pragma Assert (M.Closes = 0 and S.Charged_Bytes > 0);
      if Scenario = 5 then pragma Assert (Releases = 0); end if;
   else
      pragma Assert (S.Current = D.Retired and S.Charged_Bytes = 0);
      pragma Assert (M.Closes = (if Scenario = 4 then 0 else 1));
   end if;
   Before := Releases; S.Stop;
   pragma Assert (Releases = Before);
   Ada.Text_IO.Put_Line ("PASS Desktop target ownership scenario" & Natural'Image (Scenario));
end Desktop_Vulkan_Targets_Tests;
