with Desktop_Vulkan_Startup; with Vulkan_Owned_Targets; with Vulkan_Submission;
with Vulkan_Device_Owner; with Vulkan_Scene; with Compositor_Upload;
package body Desktop_Real_Bridge is
   package D renames Desktop_Vulkan_Startup;
   Source_Mask : Boolean := False;
   use type Interfaces.C.int;
   Lease : Vulkan_Owned_Targets.A.Ticket;
   Write : D.Write_Ticket;
   Source : Vulkan_Submission.Source_Ticket;
   use type D.Source_Result, D.Frame_Result, D.Poll_Result, D.Write_Ticket,
     Vulkan_Submission.Source_Ticket, Vulkan_Device_Owner.Phase, System.Address;
   function Open return Interfaces.C.int is
      OK : Boolean; Result : D.Source_Result;
   begin
      D.Initialize (25); if D.Current /= Vulkan_Device_Owner.Ready then return 1; end if;
      D.Configure_Targets (32, 24, 1, 1024 * 1024, OK); if not OK then return 2; end if;
      D.Prepare_Pipeline (OK); if not OK then return 3; end if;
      D.Configure_Upload (384, OK); if not OK then return 4; end if;
      D.Allocate_Backing (0, 32, 24, False, Lease, Result);
      return (if Result = D.Source_Accepted then 0 else 5);
   end Open;
   function Begin_Chunk (First, Rows : access Interfaces.Unsigned_32) return System.Address is
      Plan, Ignored : Compositor_Upload.Plan; Mapping, Nothing : System.Address;
      Rejected : D.Write_Ticket; Result : D.Source_Result; Released : Boolean;
   begin
      D.Begin_Write (0, Lease, Write, Plan, Mapping, Result, Row_Pixels => 48);
      if Result /= D.Source_Accepted then return System.Null_Address; end if;
      First.all := Interfaces.Unsigned_32 (Compositor_Upload.Area (Plan).Y);
      Rows.all := Interfaces.Unsigned_32 (Compositor_Upload.Area (Plan).Height);
      D.Begin_Write (0, Lease, Rejected, Ignored, Nothing, Result);
      pragma Assert (Result = D.Source_Busy and Rejected = D.No_Write and Nothing = System.Null_Address);
      D.Release_Upload (Released); pragma Assert (not Released);
      D.Release_Backing (0, Lease, Released); pragma Assert (not Released);
      return Mapping;
   end Begin_Chunk;
   function Submit return Interfaces.C.int is
      Result : D.Source_Result; Poll : D.Poll_Result; Released : Boolean;
   begin
      D.Submit_Write (Write, True, Result);
      if Result /= D.Source_Accepted then return 1; end if;
      pragma Assert (D.Upload_Pending and not D.Frame_Pending);
      D.Poll_Frame (Poll); pragma Assert (Poll = D.Idle);
      D.Release_Upload (Released); pragma Assert (not Released);
      return 0;
   end Submit;
   function Poll_Upload return Interfaces.C.int is
      Result : D.Poll_Result;
   begin
      D.Poll_Upload (Result);
      return (case Result is when D.Completed => 0, when D.Pending => 1, when others => 2);
   end Poll_Upload;
   function Import_Image return Interfaces.C.int is
      Result : D.Source_Result;
   begin
      D.Import_Backing (0, Lease, Source, Result);
      return (if Result = D.Source_Accepted then 0 else 1);
   end Import_Image;
   function Render return Interfaces.C.int is
      Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, (1, 1), 0, 0), 16#204060#);
      OK : Boolean; Result : D.Frame_Result; Poll : D.Poll_Result;
   begin
      Vulkan_Scene.Append (Scene, (Source, (0, 0, 32, 24), False, Source_Mask, 16#FFFF0000#, Vulkan_Scene.Textured), OK);
      if not OK then return 1; end if;
      Vulkan_Scene.Seal (Scene, OK); if not OK then return 2; end if;
      -- Source content changed in this fixture's full-output textured quad.
      -- Upload completion alone does not identify where the source is placed.
      D.Damage_Output ((0, 0, 32, 24), OK); if not OK then return 4; end if;
      D.Render (Scene, Result); if Result /= D.Submitted then return 3; end if;
      pragma Assert (D.Frame_Pending and not D.Upload_Pending);
      D.Poll_Upload (Poll); pragma Assert (Poll = D.Idle);
      return 0;
   end Render;
   function Poll_Frame return Interfaces.C.int is
      Result : D.Poll_Result;
   begin
      D.Poll_Frame (Result);
      return (case Result is when D.Completed => 0, when D.Pending => 1, when others => 2);
   end Poll_Frame;
   function Restart return Interfaces.C.int is
      Key : System.Address; OK : Boolean;
   begin
      D.Release_Source (Source, Key); if Key = System.Null_Address then return 1; end if;
      D.Restart_Content (0, Lease, OK); return (if OK then 0 else 2);
   end Restart;
   function Reconfigure (Mask : Interfaces.C.int) return Interfaces.C.int is
      Key : System.Address; OK : Boolean; Result : D.Source_Result;
   begin
      D.Release_Source (Source, Key); if Key = System.Null_Address then return 1; end if;
      D.Release_Backing (0, Lease, OK); if not OK then return 2; end if;
      Source_Mask := Mask /= 0;
      D.Allocate_Backing (0, 32, 24, Source_Mask, Lease, Result);
      return (if Result = D.Source_Accepted then 0 else 3);
   end Reconfigure;
   function Close return Interfaces.C.int is
      Key : System.Address;
   begin
      D.Release_Source (Source, Key); if Key = System.Null_Address then return 1; end if;
      D.Stop;
      return (if D.Current = Vulkan_Device_Owner.Retired and D.Charged_Bytes = 0 then 0 else 2);
   end Close;
end Desktop_Real_Bridge;
