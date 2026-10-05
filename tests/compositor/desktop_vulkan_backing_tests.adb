with Compositor_Upload;
with Ada.Command_Line; with Ada.Text_IO; with Interfaces;
with System; with System.Storage_Elements;
with Desktop_Vulkan_Startup; with Vulkan_Device_Mock;
with Vulkan_Scene; with Vulkan_Submission; with Vulkan_Owned_Targets;
procedure Desktop_Vulkan_Backing_Tests is
   package S renames Desktop_Vulkan_Startup;
   package M renames Vulkan_Device_Mock;
   package V renames Vulkan_Submission;
   package A renames Vulkan_Owned_Targets.A;
   package I renames Vulkan_Owned_Targets.I;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type U32, S.Source_Result, S.Frame_Result, S.Poll_Result, V.Source_Ticket,
     System.Address, A.Ticket, I.Phase;
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   function Binds return U32 with Import, Convention => C, External_Name => "image_mock_binds";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Health_Set (Value : U32) with Import, Convention => C, External_Name => "device_mock_health_set";
   procedure Metadata_Set (Value : U32) with Import, Convention => C, External_Name => "source_metadata_mock_set";
   function Metadata_Calls return U32 with Import, Convention => C, External_Name => "source_metadata_mock_calls";
   function Addr (N : Natural) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   Write : S.Write_Ticket; Plan : Compositor_Upload.Plan; Mapping : System.Address;
   Leases : array (S.Backing_Slot) of A.Ticket;
   Lease, Old : A.Ticket;
   Ticket : V.Source_Ticket;
   Result : S.Source_Result;
   Released_Key : System.Address;
   OK, Released : Boolean;
   Frame : S.Frame_Result; Poll : S.Poll_Result;
   Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, (1, 1), 0, 0));
begin
   Reset; Context_Set (0, 0); M.Set (True, True, 0); Pipeline_Set (0, 0);
   Image_Set (4096, 1, 0, 0, 0); Metadata_Set (0);
   S.Allocate_Backing (0, 32, 24, False, Lease, Result);
   pragma Assert (Lease = A.No_Ticket and Result = S.Source_Rejected and Metadata_Calls = 0);
   S.Initialize (25); S.Configure_Targets (32, 24, 1, (if Scenario = 1 then 12288 else 32768), OK);
   pragma Assert (OK); S.Prepare_Pipeline (OK); pragma Assert (OK);
   Image_Set (4096, 1, (if Scenario = 3 then 2 else 0), (if Scenario = 4 then 2 else 0),
      (if Scenario = 5 then 2 else 0));
   if Scenario = 2 then Metadata_Set (1); end if;
   S.Allocate_Backing (0, 32, 24, False, Lease, Result);
   if Scenario in 1 .. 4 then
      pragma Assert (Lease = A.No_Ticket);
      pragma Assert (Result = (if Scenario in 3 | 4 then S.Source_Unsafe else S.Source_Rejected));
      pragma Assert (S.Charged_Bytes = (if Scenario = 4 then 16384 else 12288));
      if Scenario in 1 .. 3 then pragma Assert (Binds = 0); end if;
   else
      pragma Assert (Result = S.Source_Accepted and Lease /= A.No_Ticket and S.Charged_Bytes = 16384);
      if Scenario = 0 then
         Leases (0) := Lease;
         for Index in S.Backing_Slot range 1 .. 4 loop
            S.Allocate_Backing (Index, 32, 24, True, Leases (Index), Result);
            pragma Assert (Result = S.Source_Accepted);
         end loop;
         pragma Assert (S.Charged_Bytes = 32768 and Metadata_Calls = 5);
         S.Allocate_Backing (0, 64, 48, False, Old, Result);
         pragma Assert (Result = S.Source_Rejected and Metadata_Calls = 5);
         for Cycle in 1 .. 32 loop
            Old := Leases (0); S.Release_Backing (0, Old, Released); pragma Assert (Released);
            S.Allocate_Backing (0, U32 (Cycle + 32), 24, False, Leases (0), Result);
            pragma Assert (Result = S.Source_Accepted and Leases (0) /= Old and S.Charged_Bytes = 32768);
            S.Release_Backing (0, Old, Released);
            pragma Assert (not Released and S.Backing_Phase (0) = I.Live);
         end loop;
      elsif Scenario = 6 then
         Vulkan_Scene.Seal (Scene, OK); pragma Assert (OK);
         S.Render (Scene, Frame); pragma Assert (Frame = S.Submitted);
         S.Release_Backing (0, Lease, Released); pragma Assert (not Released);
         S.Allocate_Backing (1, 32, 24, False, Old, Result);
         pragma Assert (Result = S.Source_Busy and Metadata_Calls = 1);
         S.Stop; pragma Assert (M.Closes = 0 and S.Backing_Phase (0) = I.Live);
         S.Poll_Frame (Poll); pragma Assert (Poll = S.Completed);
      elsif Scenario = 7 then
         Health_Set (1); S.Check_Health (OK); pragma Assert (not OK);
         S.Release_Backing (0, Lease, Released); pragma Assert (not Released);
         S.Allocate_Backing (1, 32, 24, False, Old, Result);
         pragma Assert (Result = S.Source_Unsafe and Metadata_Calls = 1);
      elsif Scenario = 8 then
         -- Complete the owned upload before importing its reserved descriptor.
         Upload_Set (4096, 1, 0, 0, 0, 0); S.Configure_Upload (4096, OK); pragma Assert (OK);
         S.Begin_Write (0, Lease, Write, Plan, Mapping, Result); pragma Assert (Result = S.Source_Accepted);
         S.Submit_Write (Write, True, Result); pragma Assert (Result = S.Source_Accepted);
         S.Poll_Upload (Poll); pragma Assert (Poll = S.Completed);
         S.Import_Backing (0, Lease, Ticket, Result);
         pragma Assert (Result = S.Source_Accepted);
         S.Release_Backing (0, Lease, Released); pragma Assert (not Released);
         S.Stop; pragma Assert (M.Closes = 0 and S.Backing_Phase (0) = I.Live);
         S.Release_Source (Ticket, Released_Key); pragma Assert (Released_Key = Addr (400));
         S.Release_Backing (0, Lease, Released); pragma Assert (Released);
      else
         S.Release_Backing (0, Lease, Released);
         pragma Assert (not Released and S.Backing_Phase (0) = I.Quarantined);
      end if;
   end if;
   S.Stop;
   pragma Assert (M.Closes = (if Scenario in 3 | 4 | 5 | 7 then 0 else 1));
   if Scenario not in 3 | 4 | 5 | 7 then pragma Assert (S.Charged_Bytes = 0); end if;
   S.Allocate_Backing (1, 32, 24, False, Lease, Result);
   pragma Assert (Lease = A.No_Ticket and Result /= S.Source_Accepted);
   Ada.Text_IO.Put_Line ("PASS Desktop bounded backing scenario" & Natural'Image (Scenario));
end Desktop_Vulkan_Backing_Tests;
