with Ada.Text_IO;
with Interfaces; with System; with System.Storage_Elements;
with Compositor_Upload; with Desktop_Vulkan_Startup; with Vulkan_Device_Mock;
with Vulkan_Submission; with Vulkan_Owned_Targets;
procedure Desktop_Strided_Tests is
   package D renames Desktop_Vulkan_Startup;
   package V renames Vulkan_Submission;
   package A renames Vulkan_Owned_Targets.A;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   use type U32, D.Source_Result, D.Poll_Result, A.Ticket, V.Source_Ticket, System.Address;
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Metadata_Set (Value : U32) with Import, Convention => C, External_Name => "source_metadata_mock_set";
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   Leases : array (D.Backing_Slot) of A.Ticket;
   Sources : array (D.Backing_Slot) of V.Source_Ticket;
   Extra : A.Ticket;
   Foreign, Rejected : V.Source_Ticket;
   Result : D.Source_Result;
   Poll : D.Poll_Result;
   OK : Boolean;
   Mapping, Retired : System.Address;
   Write : D.Write_Ticket;
   Plan : Compositor_Upload.Plan;
   Budget : constant := (4 + V.Source_Capacity) * 4096;
   function Addr (N : Natural) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (N)));
begin
   Reset; Context_Set (0, 0); Vulkan_Device_Mock.Set (True, True, 0);
   Pipeline_Set (0, 0); Image_Set (4096, 1, 0, 0, 0); Metadata_Set (0);
   Upload_Set (4096, 1, 0, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, Budget, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK);
   D.Configure_Upload (4096, OK); pragma Assert (OK);
   -- External descriptors and Desktop allocations cannot overlap in either direction.
   D.Import_Owned_Source (0, Addr (10000), Foreign, Result);
   pragma Assert (Result = D.Source_Accepted);
   D.Allocate_Backing (0, 41, 17, False, Extra, Result);
   pragma Assert (Result = D.Source_Rejected and Extra = A.No_Ticket);
   D.Release_Source (Foreign, Retired); pragma Assert (Retired = Addr (10000));
   for Index in D.Backing_Slot loop
      D.Allocate_Backing (Index, 41, 17, Natural (Index) < 128, Leases (Index), Result);
      pragma Assert (Result = D.Source_Accepted);
      D.Import_Source (Index, Addr (20000 + Natural (Index)), Rejected, Result);
      pragma Assert (Result = D.Source_Rejected and Rejected = V.No_Source);
      D.Import_Owned_Source (Index, Addr (20000 + Natural (Index)), Rejected, Result);
      pragma Assert (Result = D.Source_Rejected and Rejected = V.No_Source);
      D.Import_Backing (Index, Leases (Index), Rejected, Result);
      pragma Assert (Result = D.Source_Rejected and Rejected = V.No_Source);
      D.Begin_Write (Index, Leases (Index), Write, Plan, Mapping, Result, Row_Pixels => 40);
      pragma Assert (Result = D.Source_Rejected and Mapping = System.Null_Address);
      D.Begin_Write (Index, Leases (Index), Write, Plan, Mapping, Result, Row_Pixels => 48);
      pragma Assert (Compositor_Upload.Row_Length (Plan) = 48 and Compositor_Upload.Area (Plan).Height = 17);
      pragma Assert (Result = D.Source_Accepted and Mapping /= System.Null_Address);
      D.Submit_Write (Write, True, Result); pragma Assert (Result = D.Source_Accepted);
      D.Poll_Upload (Poll); pragma Assert (Poll = D.Completed);
      D.Import_Backing (Index, Leases (Index), Sources (Index), Result);
      pragma Assert (Result = D.Source_Accepted and D.Source_Held (Sources (Index)));
      pragma Assert (D.Charged_Bytes = (5 + Natural (Index)) * 4096);
   end loop;
   pragma Assert (D.Charged_Bytes = Budget and D.Configured_Limit = Budget);
   for Index in D.Backing_Slot loop
      pragma Assert (D.Source_Held (Sources (Index)));
      D.Release_Backing (Index, Leases (Index), OK); pragma Assert (not OK);
      D.Release_Source (Sources (Index), Retired); pragma Assert (Retired /= System.Null_Address);
      D.Release_Backing (Index, Leases (Index), OK); pragma Assert (OK);
   end loop;
   pragma Assert (D.Charged_Bytes = 4 * 4096);
   -- A confirmed closed slot can change role without reviving its old lease.
   D.Import_Owned_Source (V.Source_Slot'Last, Addr (30000), Foreign, Result);
   pragma Assert (Result = D.Source_Accepted);
   D.Import_Backing (V.Source_Slot'Last, Leases (V.Source_Slot'Last), Rejected, Result);
   pragma Assert (Result = D.Source_Rejected and Rejected = V.No_Source);
   D.Release_Source (Foreign, Retired); pragma Assert (Retired = Addr (30000));
   D.Stop; pragma Assert (D.Charged_Bytes = 0 and Vulkan_Device_Mock.Closes = 1);
   Ada.Text_IO.Put_Line ("PASS Desktop strided 41-pixel images in 48-pixel rows: 128 masks + 2 backdrops + 2 icon atlases + 16 client images, upload/import/retirement, exact 152-allocation budget, external ownership exclusion and slot role reuse");
end Desktop_Strided_Tests;
