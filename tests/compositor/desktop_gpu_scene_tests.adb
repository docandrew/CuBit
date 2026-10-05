with Ada.Command_Line; with Ada.Text_IO; with Interfaces;
with Desktop_GPU_Scene; with Desktop_Glyph_Residency; with Desktop_Vulkan_Startup; with Vulkan_Device_Mock;
with Vulkan_Glyph_Sources; with Vulkan_Submission; with Vulkan_Scene;
procedure Desktop_GPU_Scene_Tests is
   package G renames Desktop_GPU_Scene;
   package R renames Desktop_Glyph_Residency; package D renames Desktop_Vulkan_Startup;
   package V renames Vulkan_Submission;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type U32, R.Outcome, R.C.Lease, V.Source_Ticket, D.Frame_Result, D.Poll_Result, D.Capture_Admission;
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Metadata_Set (Value : U32) with Import, Convention => C, External_Name => "source_metadata_mock_set";
   procedure Font_Set (Value : U32) with Import, Convention => C, External_Name => "residency_font_set";
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   S : G.State;
   use type G.Phase, G.Outcome;
   Scenario : constant Natural := (if Ada.Command_Line.Argument_Count = 0 then 0 else Natural'Value (Ada.Command_Line.Argument (1)));
   Key : constant Vulkan_Glyph_Sources.Key := (0, 65, (5, 4));
   Screen : constant Vulkan_Scene.A.G.Output := (32, 24, Vulkan_Scene.A.G.Unrotated, (5, 4), 0, 0);
   Result : G.Outcome; OK : Boolean;
   function Calls return U32 with Import, Convention => C, External_Name => "residency_font_calls";
   procedure Start is
   begin
      G.Begin_Frame (S, Screen, 16#FF000000#, OK); pragma Assert (OK);
   end Start;
   procedure Glyph is
   begin
      G.Add_Glyph (S, Key, (0, 0, 32, 17), 16#FFFFFFFF#, OK);
   end Glyph;
begin
   pragma Assert (D.Admit_Capture (Screen) = D.Capture_Unavailable);
   Reset; Context_Set (0, 0); Vulkan_Device_Mock.Set (True, True, 0); Pipeline_Set (0, 0);
   Image_Set (4096, 1, 0, 0, 0); Metadata_Set (0); Font_Set (0);
   Upload_Set (4096, 1, 0, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, 140 * 4096, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK); D.Configure_Upload (2048, OK); pragma Assert (OK);
   pragma Assert (D.Admit_Capture (Screen) = D.Capture_Allowed);
   G.Begin_Frame (S, (64, 24, Vulkan_Scene.A.G.Unrotated, (5, 4), 0, 0), 0, OK);
   pragma Assert (not OK and G.Current (S) = G.Idle and Calls = 0 and G.Reader_Count (S) = 0);
   pragma Assert (D.Admit_Capture ((32, 25, Vulkan_Scene.A.G.Unrotated, (5, 4), 0, 0)) = D.Capture_Unavailable);
   Start; Glyph; pragma Assert (not OK and G.Reader_Count (S) = 0);
   G.Finish (S, Result); pragma Assert (Result = G.Pending and G.Current (S) = G.Uploading);
   G.Begin_Frame (S, Screen, 0, OK); pragma Assert (not OK);
   pragma Assert (D.Admit_Capture (Screen) = D.Capture_Busy);
   G.Close (S, OK); pragma Assert (not OK);
   Set (3, 1);
   for I in 1 .. 100 loop G.Poll (S, Result); pragma Assert (Result = G.Pending); end loop;
   Set (3, 0); G.Poll (S, Result); pragma Assert (Result = G.Retry and G.Current (S) = G.Idle);
   Start;
   G.Append (S, (Kind => Vulkan_Scene.Physical_Solid, Surface => (0, 0, 32, 24), Tint => 16#FF123456#, others => <>), OK); pragma Assert (OK);
   for I in 1 .. 200 loop Glyph; pragma Assert (OK); end loop;
   pragma Assert (G.Reader_Count (S) = 1 and G.Layer_Count (S) = 201 and Calls = 1);
   G.Append (S, (Kind => Vulkan_Scene.Set_Clip, Surface => (0, 0, 16, 16), others => <>), OK); pragma Assert (OK);
   G.Append (S, (Kind => Vulkan_Scene.Solid, Surface => (0, 0, 8, 8), Tint => 16#FFFF0000#, others => <>), OK); pragma Assert (OK);
   G.Append (S, (Kind => Vulkan_Scene.Reset_Clip, others => <>), OK); pragma Assert (OK);
   G.Finish (S, Result); pragma Assert (Result = G.Pending and G.Current (S) = G.Submitted);
   G.Discard (S, Result); pragma Assert (Result = G.Pending and G.Reader_Count (S) = 1);
   pragma Assert (D.Admit_Capture (Screen) = D.Capture_Busy);
   G.Close (S, OK); pragma Assert (not OK);
   Set (3, 1);
   for I in 1 .. 100 loop G.Poll (S, Result); pragma Assert (Result = G.Pending and G.Reader_Count (S) = 1); end loop;
   if Scenario = 1 then
      Set (3, 2); G.Poll (S, Result); pragma Assert (Result = G.Unsafe and G.Reader_Count (S) = 1);
      pragma Assert (D.Admit_Capture (Screen) = D.Capture_Uncertain);
      G.Close (S, OK); pragma Assert (not OK and D.Charged_Bytes = 5 * 4096);
      G.Begin_Frame (S, Screen, 0, OK); pragma Assert (not OK);
      Ada.Text_IO.Put_Line ("PASS uncertain scene retains glyph and rejects reuse"); return;
   end if;
   Set (3, 0); G.Poll (S, Result); pragma Assert (Result = G.Complete and G.Current (S) = G.Idle and G.Reader_Count (S) = 0);
   Start; Glyph; pragma Assert (OK);
   for I in 1 .. Vulkan_Scene.Maximum_Layers loop
      G.Append (S, (Kind => Vulkan_Scene.Solid, Surface => (0, 0, 1, 1), others => <>), OK);
   end loop;
   pragma Assert (not OK);
   G.Finish (S, Result); pragma Assert (Result = G.Rejected and G.Current (S) = G.Idle and G.Reader_Count (S) = 0);
   Start; Glyph; pragma Assert (OK);
   G.Append (S, (Kind => Vulkan_Scene.Glyph_Mask, others => <>), OK); pragma Assert (not OK);
   G.Finish (S, Result); pragma Assert (Result = G.Rejected and G.Reader_Count (S) = 0);
   Start; Glyph; pragma Assert (OK); G.Discard (S, Result); pragma Assert (Result = G.Retry and G.Reader_Count (S) = 0);
   G.Close (S, OK); pragma Assert (OK and G.Current (S) = G.Closed and D.Charged_Bytes = 4 * 4096);
   D.Stop; pragma Assert (D.Charged_Bytes = 0);
   pragma Assert (D.Admit_Capture (Screen) = D.Capture_Unavailable);
   Ada.Text_IO.Put_Line ("PASS whole scene: cold upload, 200 repeated glyphs/one lease, mixed layers, pending retention, overflow rejection and shutdown");
end Desktop_GPU_Scene_Tests;
