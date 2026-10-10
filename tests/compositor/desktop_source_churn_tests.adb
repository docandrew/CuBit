with Ada.Text_IO;
with Interfaces; with System; with System.Storage_Elements;
with Compositor_Formats; with Compositor_Source_Content;
with Desktop_Image_Registry; with Desktop_Image_Source;
with Desktop_Vulkan_Startup; with Vulkan_Device_Mock; with Vulkan_Submission;
-- Hosted model of the Vulkan Desktop client-source path over the native
-- mocks: persistent per-surface GPU images, in-place row-band updates, LRU
-- slot reuse and memory-pressure eviction. The mocks count every device
-- allocation (image bind) and release, so "no GPU allocation in steady
-- state" is checked directly, not inferred. No GPU executes here.
procedure Desktop_Source_Churn_Tests is
   package D renames Desktop_Vulkan_Startup;
   package R renames Desktop_Image_Registry;
   package I renames Desktop_Image_Source;
   package V renames Vulkan_Submission;
   package C renames Compositor_Source_Content;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;
   use type U32, U64, I.Outcome, R.Capacity_Pressure, V.Source_Ticket, V.Source_Slot, System.Address,
     C.Content_Version, C.Source_Key;
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   function Image_Binds return U32 with Import, Convention => C, External_Name => "image_mock_binds";
   function Image_Releases return U32 with Import, Convention => C, External_Name => "image_mock_releases";
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Metadata_Set (Value : U32) with Import, Convention => C, External_Name => "source_metadata_mock_set";
   procedure Upload_Set (Bytes : U64; Types, Prepare, Bind, Release, Null_Map : U32)
     with Import, Convention => C, External_Name => "upload_mock_set";
   procedure Real_Mapping (On : U32) with Import, Convention => C, External_Name => "upload_mock_real_mapping";
   function Staging return System.Address with Import, Convention => C, External_Name => "upload_mock_staging";
   procedure Record_Set (Value : U32) with Import, Convention => C, External_Name => "desktop_upload_record_set";
   function Record_Rows return U32 with Import, Convention => C, External_Name => "desktop_upload_record_rows";
   function Record_Y return U32 with Import, Convention => C, External_Name => "desktop_upload_record_last_y";
   function Record_Height return U32 with Import, Convention => C, External_Name => "desktop_upload_record_last_height";
   function Record_Discard return U32 with Import, Convention => C, External_Name => "desktop_upload_record_last_discard";

   Width : constant := 256;
   Height : constant := 512;
   Staging_Bytes : constant := 64 * 1024;
   Rows_Per_Chunk : constant := Staging_Bytes / (Width * 4);
   Slots : constant := V.Client_Slots;
   Small : constant := 32;
   type Pixel_Array is array (Natural range <>) of aliased U32;
   type Big_Buffer is array (0 .. Width * Height - 1) of aliased U32;
   type Small_Buffer is array (0 .. Small * Small - 1) of aliased U32;
   Publication : array (0 .. 1) of Big_Buffer := (others => (others => 0));
   Resized : Big_Buffer := (others => 0);
   Windows : array (1 .. Slots + 4) of Small_Buffer := (others => (others => 0));
   Budget : constant := (4 + 2 * V.Source_Capacity) * 4096;
   Reg : R.State;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & What); raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;
   function Image_Of (Pixels : System.Address; W, H : Natural) return Compositor_Formats.Image is
     ((Pixels, U32 (W), U32 (H), U32 (W * 4), 0));
   -- Drive one source to Available the way frames do: Ensure, then poll
   -- uploads between attempts. Bounded; never waits.
   procedure Drive (Key : C.Source_Key; Version : C.Content_Version;
      Image : Compositor_Formats.Image; Ticket : out V.Source_Ticket;
      Result : out I.Outcome; Attempts : Positive := 64) is
      Progress : I.Outcome;
   begin
      for N in 1 .. Attempts loop
         R.Ensure (Reg, Key, Version, Image, Natural (Image.Pitch * Image.Height), Ticket, Result);
         exit when Result not in I.Pending | I.Deferred;
         if R.Upload_Work (Reg) then R.Poll (Reg, Progress); end if;
      end loop;
   end Drive;
   Ticket : V.Source_Ticket;
   Result : I.Outcome;
   OK, Safe : Boolean;
   Binds, Releases : U32;
   Allocated : U64;
begin
   Reset; Context_Set (0, 0); Vulkan_Device_Mock.Set (True, True, 0);
   Pipeline_Set (0, 0); Image_Set (4096, 1, 0, 0, 0); Metadata_Set (0);
   Upload_Set (Staging_Bytes, 1, 0, 0, 0, 0); Real_Mapping (1); Record_Set (0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, Budget, OK); Check (OK, "targets");
   D.Prepare_Pipeline (OK); Check (OK, "pipeline");
   D.Configure_Upload (Staging_Bytes, OK); Check (OK, "upload staging");

   -- 1. Steady state: 200 publications of one surface, alternating between
   --    its two client buffers, allocate and free nothing after the first.
   for B in Publication'Range loop
      for P in Big_Buffer'Range loop Publication (B) (P) := U32 (P) + U32 (B) * 16#0100_0000#; end loop;
   end loop;
   Binds := Image_Binds; Allocated := D.Backings_Allocated;
   Drive (1, 1, Image_Of (Publication (0) (0)'Address, Width, Height), Ticket, Result);
   Check (Result = I.Available and D.Source_Held (Ticket), "first publication resident");
   Check (Image_Binds = Binds + 1 and D.Backings_Allocated = Allocated + 1, "one allocation for the surface");
   Check (Record_Rows = Height, "first pass copies every row");
   Binds := Image_Binds; Releases := Image_Releases; Allocated := D.Backings_Allocated;
   for Version in C.Content_Version range 2 .. 201 loop
      declare
         Buffer : constant Natural := Natural (Version mod 2);
         Previous : constant Natural := 1 - Buffer;
         First : constant Natural := Natural (Version * 37) mod (Height - 1);
         Last : constant Natural := Natural'Min (Height, First + 1 + Natural (Version * 13) mod 150);
         Rows_Before : constant U32 := Record_Rows;
      begin
         -- The client repaints the damaged rows into its next buffer.
         for Y in First .. Last - 1 loop
            for X in 0 .. Width - 1 loop
               Publication (Buffer) (Y * Width + X) := U32 (Version) * 16#10000# + U32 (Y);
            end loop;
         end loop;
         R.Note_Change (Reg, 1, (First, Last));
         Drive (1, Version, Image_Of (Publication (Buffer) (0)'Address, Width, Height), Ticket, Result);
         Check (Result = I.Available and D.Source_Held (Ticket), "publication resident");
         Check (Record_Rows - Rows_Before = U32 (Last - First), "only the damaged rows are copied");
         Check (Record_Discard = 0, "band update keeps the rest of the image");
         Check (Record_Y + Record_Height = U32 (Last), "last chunk ends at the band end");
         -- The last chunk's staging bytes are exactly the new buffer's rows.
         declare
            Copied : Pixel_Array (0 .. Natural (Record_Height) * Width - 1)
              with Import, Address => Staging;
         begin
            for Row in 0 .. Natural (Record_Height) - 1 loop
               Check (Copied (Row * Width) = Publication (Buffer) ((Natural (Record_Y) + Row) * Width),
                 "staging holds the new version's rows");
            end loop;
         end;
         -- Returning the previous buffer needs no GPU work at all.
         R.Forget (Reg, Publication (Previous) (0)'Address, Safe);
         Check (Safe, "previous client buffer returns immediately");
      end;
   end loop;
   Check (Image_Binds = Binds and Image_Releases = Releases and D.Backings_Allocated = Allocated,
     "200 publications: zero GPU allocations or frees");

   -- 2. Only an extent change replaces the allocation (one free, one bind).
   Drive (1, 202, Image_Of (Resized (0)'Address, Width, Height / 2), Ticket, Result);
   Check (Result = I.Available, "resized surface resident");
   Check (Image_Binds = Binds + 1 and Image_Releases = Releases + 1, "resize replaces exactly once");
   Binds := Image_Binds; Releases := Image_Releases;
   for Version in C.Content_Version range 203 .. 302 loop
      R.Note_Change (Reg, 1, (0, 1));
      Drive (1, Version, Image_Of (Resized (0)'Address, Width, Height / 2), Ticket, Result);
      Check (Result = I.Available, "post-resize publication");
   end loop;
   Check (Image_Binds = Binds and Image_Releases = Releases, "steady again after resize");

   -- 3. More windows than the old 8 client slots: no slot pressure.
   for W in 2 .. 12 loop
      Drive (C.Source_Key (W), 1, Image_Of (Windows (W) (0)'Address, Small, Small), Ticket, Result);
      Check (Result = I.Available and R.Last_Pressure (Reg) = R.None, "12 windows resident");
   end loop;
   Check (R.Resident_Keys (Reg) = 12, "one slot per window");
   -- 4. Beyond every slot: the least recently used idle window is evicted.
   for W in 13 .. Slots + 4 loop
      Drive (C.Source_Key (W), 1, Image_Of (Windows (W) (0)'Address, Small, Small), Ticket, Result);
      Check (Result = I.Available and R.Last_Pressure (Reg) = R.None, "LRU eviction instead of pressure");
   end loop;
   Check (R.Resident_Keys (Reg) = Slots, "all slots in use");
   -- The surface drawn longest ago (key 1) was evicted; drawing it again
   -- re-creates it with a full copy and still reports no pressure.
   Record_Set (0);
   Drive (1, 302, Image_Of (Resized (0)'Address, Width, Height / 2), Ticket, Result);
   Check (Result = I.Available and R.Last_Pressure (Reg) = R.None and Record_Rows = Height / 2,
     "evicted surface returns with one full copy");

   -- 5. A single scene reading every slot is the only slot pressure; it
   --    defers (cold) instead of failing, and clears when readers retire.
   declare
      Readers : array (V.Client_Slot) of D.Source_Reader;
      Index : V.Client_Slot := V.Client_Slot'First;
      Fresh : C.Source_Key := 100;
   begin
      for W in 1 .. Slots + 4 loop
         declare
            Image : constant Compositor_Formats.Image :=
              (if W = 1 then Image_Of (Resized (0)'Address, Width, Height / 2)
               else Image_Of (Windows (W) (0)'Address, Small, Small));
         begin
            Drive (C.Source_Key (W), (if W = 1 then 302 else 1), Image, Ticket, Result);
            Check (Result = I.Available, "scene source resident");
            D.Pin_Source (Ticket, Readers (Index));
            exit when Index = V.Client_Slot'Last;
            Index := Index + 1;
         end;
      end loop;
      Drive (Fresh, 1, Image_Of (Windows (1) (0)'Address, Small, Small), Ticket, Result, Attempts => 2);
      Check (Result = I.Deferred and R.Last_Pressure (Reg) = R.Slots_Full and not R.Faulted (Reg),
        "every slot read by one scene defers");
      for N in V.Client_Slot loop
         D.Unpin_Source (Readers (N), True, OK); Check (OK, "reader retired");
      end loop;
      Drive (Fresh, 1, Image_Of (Windows (1) (0)'Address, Small, Small), Ticket, Result);
      Check (Result = I.Available and R.Last_Pressure (Reg) = R.None, "pressure clears with readers");
   end;

   -- 6. Retired surfaces free their image once idle; nothing else moves.
   Releases := Image_Releases;
   R.Retire (Reg, 100); R.Collect (Reg);
   Check (Image_Releases = Releases + 1 and R.Resident_Keys (Reg) = Slots - 1, "retired surface freed");
   R.Close (Reg, True, Safe); Check (Safe and R.Resident_Keys (Reg) = 0, "registry closes");
   D.Stop;
   Ada.Text_IO.Put_Line ("PASS desktop source churn:" & Checks'Image &
     " checks; 200 publications with zero GPU allocations/frees, band-only copies," &
     " resize replaces once, 20 windows over" & Slots'Image & " slots without pressure, LRU/retire");
end Desktop_Source_Churn_Tests;
