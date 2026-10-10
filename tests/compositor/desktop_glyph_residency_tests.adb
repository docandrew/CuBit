with Ada.Command_Line; with Ada.Text_IO; with Interfaces; with System;
with Desktop_Glyph_Residency; with Desktop_Vulkan_Startup; with Vulkan_Device_Mock;
with Vulkan_Glyph_Sources; with Vulkan_Submission; with Vulkan_Scene;
procedure Desktop_Glyph_Residency_Tests is
   package R renames Desktop_Glyph_Residency; package D renames Desktop_Vulkan_Startup;
   package V renames Vulkan_Submission;
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type U32, R.Outcome, R.C.Lease, V.Source_Ticket, D.Frame_Result, D.Poll_Result;
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   function Image_Binds return U32 with Import, Convention => C, External_Name => "image_mock_binds";
   function Image_Releases return U32 with Import, Convention => C, External_Name => "image_mock_releases";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : U32) with Import, Convention => C, External_Name => "submission_mock_set";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Metadata_Set (Value : U32) with Import, Convention => C, External_Name => "source_metadata_mock_set";
   procedure Font_Set (Value : U32) with Import, Convention => C, External_Name => "residency_font_set";
   function Font_Calls return U32 with Import, Convention => C, External_Name => "residency_font_calls";
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   Scenario : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   S : R.State;
   Keys : array (0 .. 127) of Vulkan_Glyph_Sources.Key;
   Readers : array (0 .. 127) of R.C.Lease;
   Source, Old_Source : V.Source_Ticket;
   Extra, Old : R.C.Lease;
   Result : R.Outcome; OK : Boolean;
   New_Key : constant Vulkan_Glyph_Sources.Key := (1, 65, (5, 4));
   Scene : Vulkan_Scene.State := Vulkan_Scene.Open ((32, 24, Vulkan_Scene.A.G.Unrotated, (5, 4), 0, 0));
   Frame : D.Frame_Result; Poll : D.Poll_Result;
   Binds, Releases : U32;
begin
   Reset; Context_Set (0, 0); Vulkan_Device_Mock.Set (True, True, 0); Pipeline_Set (0, 0);
   Image_Set (4096, 1, 0, 0, 0); Metadata_Set (0); Font_Set (if Scenario = 1 then 1 else 0);
   Upload_Set (4096, 1, 0, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, 140 * 4096, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK); D.Configure_Upload (2048, OK); pragma Assert (OK);
   if Scenario in 3 | 4 then Image_Set (4096, 1, (if Scenario = 3 then 2 else 0), (if Scenario = 4 then 2 else 0), 0); end if;
   if Scenario /= 0 then
      R.Acquire (S, New_Key, Source, Extra, Result);
      if Scenario = 1 then
         pragma Assert (Result = R.Rejected and R.Charged (S) = 0);
         R.Close (S, OK); pragma Assert (OK); D.Stop;
         pragma Assert (D.Charged_Bytes = 0 and Vulkan_Device_Mock.Closes = 1);
      else
         if Scenario in 3 | 4 then pragma Assert (Result = R.Unsafe);
         else
            pragma Assert (Result = R.Uploading);
            if Scenario = 2 then Set (3, 2); else Set (9, 2); end if;
            R.Poll (S, Result); pragma Assert (Result = R.Unsafe);
         end if;
         pragma Assert (R.Charged (S) > 0);
         R.Close (S, OK); pragma Assert (not OK);
         R.Acquire (S, New_Key, Source, Extra, Result); pragma Assert (Result /= R.Available);
      end if;
      Ada.Text_IO.Put_Line ("PASS glyph residency failure" & Natural'Image (Scenario));return;
   end if;
   -- Renderer start: every cell allocated together, then no glyph's first
   -- use allocates (the NUC showed ~29 VM updates when a menu first opened).
   declare Prepared : Natural; Before : constant U32 := Image_Binds; begin
      R.Prepare_Cells (S, (5, 4), Prepared);
      pragma Assert (Prepared = 128 and Image_Binds = Before + 128);
      R.Prepare_Cells (S, (5, 4), Prepared);
      pragma Assert (Prepared = 0 and Image_Binds = Before + 128);
      Binds := Image_Binds;
   end;
   for I in Keys'Range loop
      Keys (I) := (I / 95, 32 + I mod 95, (5, 4));
      R.Acquire (S, Keys (I), Source, Readers (I), Result);
      pragma Assert (Result = R.Uploading and R.Pending (S));
      R.Acquire (S, Keys (I), Source, Extra, Result); pragma Assert (Result = R.Uploading and Font_Calls = U32 (I + 1));
      R.Poll (S, Result); pragma Assert (Result = R.Available and not R.Pending (S));
      R.Acquire (S, Keys (I), Source, Readers (I), Result);
      pragma Assert (Result = R.Available and R.Held (S, Readers (I)) and D.Source_Held (Source));
      if I = 0 then Old_Source := Source; end if;
   end loop;
   pragma Assert (Font_Calls = 128 and R.Charged (S) = 128 * 1056 and D.Charged_Bytes = 132 * 4096);
   pragma Assert (Image_Binds = Binds, "first-use glyphs reuse prepared cells");
   R.Acquire (S, Keys (0), Source, Extra, Result); pragma Assert (Result = R.Deferred and Font_Calls = 128);
   R.Acquire (S, New_Key, Source, Extra, Result); pragma Assert (Result = R.Deferred and Font_Calls = 128);
   R.Release (S, Readers (0), False); pragma Assert (R.Held (S, Readers (0)));
   D.Capture_Glyph (Scene, Keys (0), (0, 0, 32, 17), 16#FFFFFFFF#, OK); pragma Assert (OK);
   Vulkan_Scene.Seal (Scene, OK); pragma Assert (OK); D.Render (Scene, Frame); pragma Assert (Frame = D.Submitted);
   R.Release (S, Readers (0), True); pragma Assert (R.Held (S, Readers (0)));
   D.Poll_Frame (Poll); pragma Assert (Poll = D.Completed);
   Old := Readers (0); R.Release (S, Old, True); pragma Assert (not R.Held (S, Old));
   Binds := Image_Binds; Releases := Image_Releases;
   R.Acquire (S, New_Key, Source, Extra, Result); pragma Assert (Result = R.Uploading and Font_Calls = 129);
   -- Eviction rasterizes into the victim's retained cell allocation.
   pragma Assert (Image_Binds = Binds and Image_Releases = Releases);
   pragma Assert (not D.Source_Held (Old_Source));
   R.Poll (S, Result); pragma Assert (Result = R.Available);
   R.Acquire (S, New_Key, Source, Readers (0), Result); pragma Assert (Result = R.Available and Readers (0) /= Old);
   R.Release (S, Old, True); pragma Assert (R.Held (S, Readers (0)));
   for I in 1 .. Readers'Last loop R.Release (S, Readers (I), True); end loop;
   -- Steady-state text: 300 more glyph misses on a full cache evict and
   -- re-rasterize in place without one device allocation or free.
   for N in 0 .. 299 loop
      declare
         K : constant Vulkan_Glyph_Sources.Key := (N mod 2, 32 + (N * 7) mod 95, (5, 4));
      begin
         R.Acquire (S, K, Source, Extra, Result);
         if Result = R.Uploading then R.Poll (S, Result); pragma Assert (Result = R.Available);
            R.Acquire (S, K, Source, Extra, Result);
         end if;
         pragma Assert (Result = R.Available);
         R.Release (S, Extra, True); pragma Assert (not R.Held (S, Extra));
      end;
   end loop;
   pragma Assert (Image_Binds = Binds and Image_Releases = Releases);
   R.Close (S, OK); pragma Assert (not OK);
   for Reader of Readers loop R.Release (S, Reader, True); end loop;
   R.Close (S, OK); pragma Assert (OK and R.Charged (S) = 0 and D.Charged_Bytes = 4 * 4096);
   D.Stop; pragma Assert (D.Charged_Bytes = 0 and Vulkan_Device_Mock.Closes = 1);
   Ada.Text_IO.Put_Line ("PASS 128 resident/pinned glyphs: no duplicate uploads, bounded saturation, frame-safe eviction into retained cells (300 misses, zero allocations), stale reader rejection and accounted shutdown");
end Desktop_Glyph_Residency_Tests;
