with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Client_Canvas_Geometry;
with Client_Raster;

package body CuBit.UI.GPU_Canvas is

   package S renames GPU_Canvas_Slots;
   package G renames Client_Canvas_Geometry;
   use type S.Decision, S.Slot_Phase, S.Generation_Count, GPU_Canvas_Device.Device_Status;

   Bytes_Per_Pixel : constant := 4;
   --  Targets in the C glue (GPU_CANVAS_TARGETS in gpu_canvas_vk.h).
   Glue_Targets : constant := 8;
   pragma Compile_Time_Error
     (Canvas_Limit * S.Slot_Count /= Glue_Targets,
      "gpu_canvas_vk.h GPU_CANVAS_TARGETS must equal Canvas_Limit * Slot_Count");

   --  Placeholder while no frame exists or no GPU is available.
   Background : constant Color := 16#1B1F2A#;
   Placeholder_Text : constant Color := 16#9AA5B5#;
   Unavailable_Message : constant String := "GPU rendering unavailable";
   Failed_Message : constant String := "GPU rendering failed";

   --  C glue (gpu_canvas_vk.c).
   Glue_OK : constant Unsigned_32 := 0;
   Glue_Pending : constant Unsigned_32 := 1;

   type Glue_Target is record
      Image, View, Ready : Unsigned_64 := 0;
      Width, Height, Format, Pitch : Unsigned_32 := 0;
   end record with Convention => C;

   function Glue_Open (Device : access constant Device_View) return Unsigned_32
     with Import, Convention => C, External_Name => "gpu_canvas_vk_open";
   function Glue_Create (Index, Width, Height : Unsigned_32; T : access Glue_Target) return Unsigned_32
     with Import, Convention => C, External_Name => "gpu_canvas_vk_create";
   function Glue_Present (Index : Unsigned_32) return Unsigned_32
     with Import, Convention => C, External_Name => "gpu_canvas_vk_present";
   function Glue_Poll (Index : Unsigned_32) return Unsigned_32
     with Import, Convention => C, External_Name => "gpu_canvas_vk_poll";
   function Glue_Pixels (Index : Unsigned_32) return System.Address
     with Import, Convention => C, External_Name => "gpu_canvas_vk_pixels";
   procedure Glue_Destroy (Index : Unsigned_32)
     with Import, Convention => C, External_Name => "gpu_canvas_vk_destroy";
   procedure Glue_Idle
     with Import, Convention => C, External_Name => "gpu_canvas_vk_idle";
   procedure Glue_Close
     with Import, Convention => C, External_Name => "gpu_canvas_vk_close";

   --  Process-wide: which canvas numbers are taken, and the glue's state.
   type Number_Flags is array (Canvas_Number) of Boolean;
   Taken : Number_Flags := [others => False];
   Glue_Opened : Boolean := False;
   Device_Cache : aliased Device_View;

   function Index (C : Canvas_Widget; Slot : Slot_Index) return Unsigned_32 is
     (Unsigned_32 (C.Number * S.Slot_Count + Natural (Slot) - 1));

   function Open_Count return Natural is
      Result : Natural := 0;
   begin
      for Number_Taken of Taken loop
         if Number_Taken then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Open_Count;

   --  The rectangle R of Parent in physical pixels, clamped to Parent (not
   --  to its repair clip). Empty when R lies outside Parent.
   function Physical (Parent : CuBit.UI.Canvas; R : CuBit.UI.Rect) return CuBit.UI.Rect is
      Right, Bottom : Natural;
      Left, Top : Natural;
   begin
      if Is_Empty (R) or else R.x >= Parent.width or else R.y >= Parent.height then
         return (others => 0);
      end if;
      Right := G.Clamped_End (R.x, R.w, Parent.width);
      Bottom := G.Clamped_End (R.y, R.h, Parent.height);
      if Parent.densityNumerator = Parent.densityDenominator then
         return (R.x, R.y, Right - R.x, Bottom - R.y);
      end if;
      if Right > G.Logical_Edge'Last - Parent.originX or else
        Bottom > G.Logical_Edge'Last - Parent.originY
      then
         return (others => 0);
      end if;
      Left := G.Relative (Parent.originX, R.x, Parent.densityNumerator, Parent.densityDenominator);
      Top := G.Relative (Parent.originY, R.y, Parent.densityNumerator, Parent.densityDenominator);
      Right := G.Relative (Parent.originX, Right, Parent.densityNumerator, Parent.densityDenominator);
      Bottom := G.Relative (Parent.originY, Bottom, Parent.densityNumerator, Parent.densityDenominator);
      return (Left, Top, Right - Left, Bottom - Top);
   end Physical;

   function Intersect (A, B : CuBit.UI.Rect) return CuBit.UI.Rect is
      Left : constant Natural := Natural'Max (A.x, B.x);
      Top : constant Natural := Natural'Max (A.y, B.y);
      Right : constant Natural := Natural'Min (A.x + A.w, B.x + B.w);
      Bottom : constant Natural := Natural'Min (A.y + A.h, B.y + B.h);
   begin
      if Right <= Left or else Bottom <= Top then
         return (others => 0);
      end if;
      return (Left, Top, Right - Left, Bottom - Top);
   end Intersect;

   procedure Release_Storage (C : in out Canvas_Widget) is
   begin
      for Slot in Slot_Index loop
         if C.Slots.Phases (Slot) /= S.Empty then
            Glue_Destroy (Index (C, Slot));
         end if;
      end loop;
      S.Released (C.Slots);
      C.Targets := [others => <>];
   end Release_Storage;

   procedure Open (C : in out Canvas_Widget) is
      Status : GPU_Canvas_Device.Device_Status;
   begin
      if C.Opened then
         return;
      end if;
      for N in Canvas_Number loop
         if not Taken (N) then
            Taken (N) := True;
            C.Number := N;
            C.Opened := True;
            exit;
         end if;
      end loop;
      if not C.Opened then
         C.Status := Failed;
         return;
      end if;
      C.Slots := (others => <>);
      GPU_Canvas_Device.Open (Device_Cache, Status);
      case Status is
         when GPU_Canvas_Device.Ready =>
            if not Glue_Opened then
               Glue_Opened := Glue_Open (Device_Cache'Access) = Glue_OK;
            end if;
            C.Status := (if Glue_Opened then Ready else Failed);
         when GPU_Canvas_Device.Unavailable =>
            C.Status := Unavailable;
         when others =>
            C.Status := Failed;
      end case;
   end Open;

   function State (C : Canvas_Widget) return Availability is (C.Status);

   function Device (C : Canvas_Widget) return Device_View is
     (if C.Status = Ready then Device_Cache else (others => <>));

   procedure Set_Area (C : in out Canvas_Widget; Parent : CuBit.UI.Canvas; Area : CuBit.UI.Rect) is
      Pixels : constant CuBit.UI.Rect := Physical (Parent, Area);
   begin
      C.Logical := Area;
      C.Slots.Wanted :=
        (Width => Natural'Min (Pixels.w, S.Maximum_Extent),
         Height => Natural'Min (Pixels.h, S.Maximum_Extent));
   end Set_Area;

   function Area (C : Canvas_Widget) return CuBit.UI.Rect is (C.Logical);

   procedure Set_Visible (C : in out Canvas_Widget; Visible : Boolean) is
   begin
      C.Slots.Visible := Visible;
   end Set_Visible;

   procedure Reallocate (C : in out Canvas_Widget; OK : out Boolean) is
      Made : aliased Glue_Target;
   begin
      Release_Storage (C);
      OK := True;
      for Slot in Slot_Index loop
         if Glue_Create (Index (C, Slot), Unsigned_32 (C.Slots.Wanted.Width),
                         Unsigned_32 (C.Slots.Wanted.Height), Made'Access) /= Glue_OK
           or else Made.Format /= Target_Format
         then
            OK := False;
         else
            C.Targets (Slot) :=
              (Image => Vulkan_Handle (Made.Image), View => Vulkan_Handle (Made.View),
               Ready => Vulkan_Handle (Made.Ready), Width => C.Slots.Wanted.Width,
               Height => C.Slots.Wanted.Height, Format => Made.Format, Slot => Slot,
               Generation => C.Slots.Generation + 1);
         end if;
      end loop;
      if OK then
         S.Reallocated (C.Slots);
      else
         for Slot in Slot_Index loop
            Glue_Destroy (Index (C, Slot));
         end loop;
         C.Targets := [others => <>];
         C.Status := Failed;
      end if;
   end Reallocate;

   procedure Acquire (C : in out Canvas_Widget; T : out Target; Status : out Acquire_Status) is
      Choice : S.Choice;
      OK : Boolean;
   begin
      T := (others => <>);
      if C.Status /= Ready then
         Status := Unavailable;
         return;
      end if;
      Choice := S.Choose (C.Slots);
      if Choice.Kind = S.Reallocate then
         Reallocate (C, OK);
         if not OK then
            Status := Unavailable;
            return;
         end if;
         Choice := S.Choose (C.Slots);
      end if;
      case Choice.Kind is
         when S.Grant =>
            S.Acquire (C.Slots, Choice.Slot);
            T := C.Targets (Choice.Slot);
            Status := Acquired;
         when S.Hidden =>
            Status := Hidden;
         when S.Busy | S.Reallocate =>
            Status := Busy;
      end case;
   end Acquire;

   function Lent (C : Canvas_Widget; T : Target) return Boolean is
     (C.Status = Ready and then C.Slots.Phases (T.Slot) = S.Acquired and then
      T.Generation = C.Slots.Generation and then T.Image = C.Targets (T.Slot).Image);

   procedure Present (C : in out Canvas_Widget; T : Target) is
   begin
      if not Lent (C, T) then
         return;
      end if;
      if Glue_Present (Index (C, T.Slot)) = Glue_OK then
         S.Present (C.Slots, T.Slot);
      else
         C.Status := Failed;
      end if;
   end Present;

   procedure Cancel (C : in out Canvas_Widget; T : Target) is
   begin
      if Lent (C, T) then
         S.Cancel (C.Slots, T.Slot);
      end if;
   end Cancel;

   procedure Poll (C : in out Canvas_Widget; New_Frame : out Boolean) is
      Result : Unsigned_32;
   begin
      New_Frame := False;
      if C.Status /= Ready then
         return;
      end if;
      for Slot in Slot_Index loop
         if C.Slots.Phases (Slot) = S.Copying then
            Result := Glue_Poll (Index (C, Slot));
            if Result = Glue_OK then
               S.Complete (C.Slots, Slot);
               New_Frame := True;
            elsif Result /= Glue_Pending then
               C.Status := Failed;
               return;
            end if;
         end if;
      end loop;
   end Poll;

   procedure Paint (C : in out Canvas_Widget; Into : CuBit.UI.Canvas) is
      Pixels : constant CuBit.UI.Rect := Physical (Into, C.Logical);
      Clip : constant CuBit.UI.Rect :=
        (if Into.clipEnabled then Intersect (Pixels, Physical (Into, Into.clip)) else Pixels);
      Front : Slot_Index;
      Source : System.Address := System.Null_Address;
      Width, Height : Natural := 0;
   begin
      if Is_Empty (Clip) or else Into.addr = System.Null_Address or else
        Into.pitch < Bytes_Per_Pixel or else Into.pitch mod Bytes_Per_Pixel /= 0
      then
         return;
      end if;
      if C.Status = Ready and then S.Count (C.Slots, S.Front) > 0 then
         Front := S.Front_Slot (C.Slots);
         Source := Glue_Pixels (Index (C, Front));
         Width := C.Targets (Front).Width;
         Height := C.Targets (Front).Height;
      end if;
      if Source = System.Null_Address then
         Fill_Rect (Into, C.Logical, Background);
         if C.Status in Unavailable | Failed then
            declare
               Message : constant String :=
                 (if C.Status = Unavailable then Unavailable_Message else Failed_Message);
               Text_W : constant Natural := UI_Text_Width (Message);
               Text_H : constant Natural := UI_Text_Height;
            begin
               if C.Logical.w > Text_W and then C.Logical.h > Text_H then
                  Draw_UI_Text (Into, C.Logical.x + (C.Logical.w - Text_W) / 2,
                                C.Logical.y + (C.Logical.h - Text_H) / 2,
                                Message, Placeholder_Text, Background);
               end if;
            end;
         end if;
         return;
      end if;
      declare
         --  The part of the frame that lands inside the repair clip.
         Shown : constant CuBit.UI.Rect :=
           Intersect (Clip, (Pixels.x, Pixels.y, Natural'Min (Width, Pixels.w),
                             Natural'Min (Height, Pixels.h)));
         Pitch : constant Positive := Into.pitch / Bytes_Per_Pixel;
      begin
         if Width < Pixels.w or else Height < Pixels.h then
            Fill_Rect (Into, C.Logical, Background);  --  frame older than a grow
         end if;
         if not Is_Empty (Shown) and then Shown.x + Shown.w <= Pitch then
            declare
               Target_Pixels : Client_Raster.Pixels
                 (0 .. (Shown.h - 1) * Pitch + Shown.x + Shown.w - 1)
                 with Import, Address => Into.addr + Storage_Offset (Shown.y * Into.pitch);
               Source_Pixels : constant Client_Raster.Pixels (0 .. Width * Height - 1)
                 with Import, Address => Source;
            begin
               Client_Raster.Copy_Block
                 (Target_Pixels, Pitch, (Shown.x, 0, Shown.w, Shown.h),
                  Source_Pixels, Width, Shown.x - Pixels.x, Shown.y - Pixels.y);
            end;
         end if;
      end;
      S.Painted (C.Slots);
   end Paint;

   procedure Close (C : in out Canvas_Widget) is
   begin
      if not C.Opened then
         return;
      end if;
      if S.In_Flight (C.Slots) then
         Glue_Idle;
      end if;
      Release_Storage (C);
      Taken (C.Number) := False;
      C.Opened := False;
      C.Status := Closed;
      if Open_Count = 0 then
         if Glue_Opened then
            Glue_Close;
            Glue_Opened := False;
         end if;
         GPU_Canvas_Device.Close;
      end if;
   end Close;

end CuBit.UI.GPU_Canvas;
