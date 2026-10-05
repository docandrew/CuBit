with Ada.Text_IO;
with Interfaces;
with System;
with System.Storage_Elements;
with Vulkan_Device_Owner;
with Vulkan_Device_Mock;
procedure Vulkan_Device_Health_Tests is
   package D renames Vulkan_Device_Owner;
   package C renames D.C;
   package V renames D.V;
   package M renames Vulkan_Device_Mock;
   use type Interfaces.Unsigned_32, D.Phase, C.Phase, V.Phase;
   procedure Submission_Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Context_Set (Create, Release : Interfaces.Unsigned_32)
     with Import, Convention => C, External_Name => "context_mock_set";
   procedure Set_Health (Value : Interfaces.Unsigned_32)
     with Import, Convention => C, External_Name => "device_mock_health_set";
   function Checks return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "device_mock_health_checks";
begin
   for Failure in 0 .. 2 loop
      declare
         Device : D.State;
         Context : C.State;
         Submission : V.State := V.Open (System.Null_Address);
         Child : C.Child;
         OK : Boolean;
      begin
         M.Set (True, True, 0); Context_Set (0, 0);
         D.Check_Health (Device, OK);
         pragma Assert (not OK and Checks = 0);
         D.Start (Device, Context, Submission, 25);
         C.Register_Child (Context, Child);
         if Failure = 2 then
            -- Isolate pending GPU work from the separate child-retention case.
            C.Retire_Child (Context, Child, True);
            Submission_Reset;
            V.Begin_Record (Submission, OK); pragma Assert (OK);
            V.Begin_Scene (Submission, System.Storage_Elements.To_Address (512), 1, 1, OK);
            pragma Assert (OK);
            V.End_Scene (Submission, OK); pragma Assert (OK);
            V.Seal (Submission, OK); pragma Assert (OK);
            V.Submit (Submission, OK); pragma Assert (OK);
            pragma Assert (V.Current (Submission) = V.Pending);
         end if;
         D.Check_Health (Device, OK);
         pragma Assert (OK and Checks = 1 and D.Current (Device) = D.Ready);
         Set_Health (Interfaces.Unsigned_32 (Failure));
         D.Check_Health (Device, OK);
         pragma Assert (Checks = 2 and D.Owns_Device (Device));
         pragma Assert (if Failure = 2 then C.Empty (Context) else C.Held (Context, Child));
         if Failure /= 0 then
            pragma Assert (not OK and D.Current (Device) = D.Quarantined);
            Set_Health (0);
            D.Check_Health (Device, OK);
            pragma Assert (not OK and Checks = 2);
            D.Close (Device, Context, Submission);
            pragma Assert (M.Closes = 0 and C.Current (Context) = C.Live);
            if Failure = 2 then pragma Assert (V.Current (Submission) = V.Pending); end if;
            D.Start (Device, Context, Submission, 25);
            pragma Assert (M.Starts = 1 and D.Current (Device) = D.Quarantined);
         else
            pragma Assert (OK);
            D.Close (Device, Context, Submission);
            pragma Assert (M.Closes = 0);
            C.Retire_Child (Context, Child, True);
            D.Close (Device, Context, Submission);
            D.Check_Health (Device, OK);
            pragma Assert (not OK and Checks = 2 and M.Closes = 1);
         end if;
      end;
   end loop;
   declare
      Device : D.State;
      Context : C.State;
      Submission : V.State := V.Open (System.Null_Address);
      OK : Boolean;
   begin
      M.Set (True, True, 0);
      D.Start (Device, Context, Submission, 0);
      D.Check_Health (Device, OK);
      pragma Assert (not OK and Checks = 0 and M.Starts = 0);
   end;
   Ada.Text_IO.Put_Line ("PASS device health: sticky loss, held children, no retry, no software/closed IPC");
end Vulkan_Device_Health_Tests;
