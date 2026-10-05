with Ada.Text_IO; with Ada.Command_Line; with Interfaces;
with Desktop_Icon_Upload; with Desktop_Icon_Pixels; with Desktop_Icons; with Desktop_Vulkan_Startup;
with Vulkan_Owned_Targets; with Vulkan_Submission; with Vulkan_Device_Mock;
 with System;
procedure Icon_Upload_Tests is
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
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   Lease : Vulkan_Owned_Targets.A.Ticket; Source : Vulkan_Submission.Source_Ticket;
   Result : D.Source_Result; Poll : D.Poll_Result; OK : Boolean; Released : System.Address;
begin
   Reset; Context_Set (0, 0); Pipeline_Set (0, 0); Image_Set (4096, 1, 0, 0, 0);
   Vulkan_Device_Mock.Set (True, True, 0); Upload_Set (65536, 1, 0, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, 1048576, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK); D.Configure_Upload (128, OK); pragma Assert (OK);
   D.Allocate_Backing (128, 24, (if Scenario = 1 then 23 else 24), False, Lease, Result);
   pragma Assert (Result = D.Source_Accepted);
   for Chunk in 1 .. 24 loop
      D.Import_Backing (128, Lease, Source, Result);
      pragma Assert (Result /= D.Source_Accepted and Source = Vulkan_Submission.No_Source);
      Desktop_Icon_Upload.Start (128, Lease, (Desktop_Icon_Pixels.Application, Desktop_Icons.Start), Result);
      if Scenario = 1 then
         pragma Assert (Result = D.Source_Rejected and Calls (2) = 0);
         D.Release_Backing (128, Lease, OK); pragma Assert (OK); D.Stop;
         Ada.Text_IO.Put_Line ("PASS mismatched icon shape cancels without submission"); return;
      end if;
      pragma Assert (Result = D.Source_Accepted);
      Desktop_Icon_Upload.Start (128, Lease, (Desktop_Icon_Pixels.Application, Desktop_Icons.Start), Result);
      pragma Assert (Result = D.Source_Busy);
      if Scenario = 2 then
         Set (3, 2); D.Poll_Upload (Poll); pragma Assert (Poll = D.GPU_Failed);
         D.Release_Backing (128, Lease, OK); pragma Assert (not OK);
         D.Import_Backing (128, Lease, Source, Result); pragma Assert (Source = Vulkan_Submission.No_Source);
         Ada.Text_IO.Put_Line ("PASS uncertain icon transfer retains backing and refuses import"); return;
      end if;
      Set (3, 1);
      for I in 1 .. 10 loop D.Poll_Upload (Poll); pragma Assert (Poll = D.Pending); end loop;
      Set (3, 0); D.Poll_Upload (Poll); pragma Assert (Poll = D.Completed);
   end loop;
   D.Import_Backing (128, Lease, Source, Result); pragma Assert (Result = D.Source_Accepted);
   D.Release_Source (Source, Released); pragma Assert (Released /= System.Null_Address);
   D.Release_Backing (128, Lease, OK); pragma Assert (OK); D.Stop;
   pragma Assert (D.Charged_Bytes = 0);
   Ada.Text_IO.Put_Line ("PASS 24 icon chunks, 240 pending polls, busy backpressure, complete-only import and refund");
end Icon_Upload_Tests;
