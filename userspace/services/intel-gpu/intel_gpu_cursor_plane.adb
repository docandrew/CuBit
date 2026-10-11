with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Cursor_Plane is
   use type CS.Readiness;
   Configured : Boolean := False;
   Staging_Address, Surface_CPU_First : Unsigned_64 := 0;
   Surface_Bases : CS.Bases := [others => 0];
   Width_Limit, Height_Limit : Positive := 1;
   Buffer_Word, Watermark_Word : Unsigned_32 := 0;

   --  Desired state.
   Defined : Boolean := False;
   Empty_Image : Boolean := True;
   Current : CP.Image;
   Position_X, Position_Y : Integer := 0;
   --  Hardware state as last written: the armed base and whether enabled.
   Armed_Base : Unsigned_32 := 0;
   Is_Enabled : Boolean := False;

   Stats : Counters;

   procedure Count (Item : in out Natural) is
   begin
      if Item < Natural'Last then Item := Item + 1; end if;
   end Count;

   package DDB renames Intel_GPU_Cursor_DDB;
   use type DDB.Outcome;

   --  Pipe page offsets (i915 v6.16 skl_universal_plane_regs.h,
   --  intel_cursor_regs.h, i915_reg.h): plane 1, the cursor, the counter.
   Frame_Count_Offset    : constant Unsigned_32 := 16#040#;
   Plane_Surface_Offset  : constant Unsigned_32 := 16#19C#;
   Plane_WM_Offset       : constant Unsigned_32 := 16#240#;
   Plane_SAGV_Offset     : constant Unsigned_32 := 16#258#;
   Plane_SAGV_Trans_Offset : constant Unsigned_32 := 16#25C#;
   Plane_Trans_Offset    : constant Unsigned_32 := 16#268#;
   Plane_Buffer_Offset   : constant Unsigned_32 := 16#27C#;
   Plane_Stride          : constant Unsigned_32 := 16#100#;
   Cursor_SAGV_Offset    : constant Unsigned_32 := 16#158#;
   Cursor_SAGV_Trans_Offset : constant Unsigned_32 := 16#15C#;
   Cursor_Trans_Offset   : constant Unsigned_32 := 16#168#;
   Level_Stride          : constant Unsigned_32 := 4;
   --  Two vblanks at 24 Hz, with margin.
   Latch_Deadline_Ms : constant := 250;
   Latch_Frames : constant := 2;

   function Watermark_Offset (Cursor : Boolean; Slot : DDB.Watermark_Slot) return Unsigned_32 is
     (case Slot is
        when DDB.Level_0 .. DDB.Level_5 =>
          (if Cursor then CS.Watermark_Offset else Plane_WM_Offset) +
            Level_Stride * Unsigned_32 (DDB.Watermark_Slot'Pos (Slot)),
        when DDB.SAGV => (if Cursor then Cursor_SAGV_Offset else Plane_SAGV_Offset),
        when DDB.SAGV_Transition =>
          (if Cursor then Cursor_SAGV_Trans_Offset else Plane_SAGV_Trans_Offset),
        when DDB.Transition => (if Cursor then Cursor_Trans_Offset else Plane_Trans_Offset));

   Snap_Before, Snap_After : DDB_Snapshot;
   Last_Plan : DDB.Plan_Result;
   function Before return DDB_Snapshot is (Snap_Before);
   function After return DDB_Snapshot is (Snap_After);
   function Plan_State return DDB.Outcome is (Last_Plan.State);
   function Plane_Levels_Disabled return Natural is (Last_Plan.Plane_Levels_Disabled);

   procedure Snapshot (Item : out DDB_Snapshot; Success : out Boolean) is
      function Valid (Word : Unsigned_32) return Boolean is (Word /= Unsigned_32'Last);
   begin
      Item.Plane_Buffer := Read (Plane_Buffer_Offset);
      Item.Cursor_Buffer := Read (CS.Buffer_Config_Offset);
      Item.Plane_Surface := Read (Plane_Surface_Offset);
      Success := Valid (Item.Plane_Buffer) and then Valid (Item.Cursor_Buffer) and then
        Valid (Item.Plane_Surface);
      for P in Item.Other_Buffers'Range loop
         Item.Other_Buffers (P) :=
           Read (Plane_Buffer_Offset + Plane_Stride * Unsigned_32 (P - 1));
         Success := Success and then Valid (Item.Other_Buffers (P));
      end loop;
      for S in DDB.Watermark_Slot loop
         Item.Plane_WM (S) := Read (Watermark_Offset (False, S));
         Item.Cursor_WM (S) := Read (Watermark_Offset (True, S));
         Success := Success and then Valid (Item.Plane_WM (S)) and then Valid (Item.Cursor_WM (S));
      end loop;
   end Snapshot;

   --  True once PIPE_FRMCOUNT has advanced Latch_Frames past its value now.
   --  All ones is a valid count (the counter wraps); an unreadable page reads
   --  a constant, never advances, and ends in the deadline.
   function Await_Latch return Boolean is
      First : constant Unsigned_32 := Read (Frame_Count_Offset);
      Started : constant Unsigned_64 := Now_Ms;
      Now : Unsigned_64;
   begin
      if Started = Unsigned_64'Last then
         return False;
      end if;
      loop
         if Read (Frame_Count_Offset) - First >= Latch_Frames then
            return True;
         end if;
         Now := Now_Ms;
         if Now = Unsigned_64'Last or else Now < Started or else
           Now - Started >= Latch_Deadline_Ms
         then
            return False;
         end if;
         Pause;
      end loop;
   end Await_Latch;

   procedure Allocate (Single_Pipe : Boolean; Result : out CS.Readiness) is
      Written : Boolean := True;
      W : Boolean;
   begin
      Result := CS.Allocation_Refused;
      Last_Plan := DDB.Plan (Snap_Before.Plane_Buffer, Snap_Before.Other_Buffers,
                             Snap_Before.Plane_WM, Snap_Before.Cursor_Buffer, Single_Pipe);
      if Last_Plan.State /= DDB.Ready then
         return;
      end if;
      --  Step 1: plane 1 gives up its tail; armed by PLANE_SURF.
      for S in DDB.Watermark_Slot loop
         Write (Watermark_Offset (False, S), Last_Plan.Plane_WM (S), W);
         Written := Written and then W;
      end loop;
      Write (Plane_Buffer_Offset, Last_Plan.Plane_Buffer, W);
      Written := Written and then W;
      Write (Plane_Surface_Offset, Snap_Before.Plane_Surface, W);
      Written := Written and then W;
      if not Written then return; end if;
      if not Await_Latch then
         Result := CS.Latch_Timeout;
         return;
      end if;
      --  Step 2: the freed blocks go to the (still disabled) cursor; the
      --  CURBASE write of the disable sequence arms them.
      for S in DDB.Watermark_Slot loop
         Write (Watermark_Offset (True, S), Last_Plan.Cursor_WM (S), W);
         Written := Written and then W;
      end loop;
      Write (CS.Buffer_Config_Offset, Last_Plan.Cursor_Buffer, W);
      Written := Written and then W;
      if not Written or else not Program (CP.Disabled) then return; end if;
      if not Await_Latch then
         Result := CS.Latch_Timeout;
         return;
      end if;
      Result := CS.Ready_Driver_Allocation;
   end Allocate;

   procedure Configure
     (Staging, Surfaces_CPU : Unsigned_64; Surfaces_GGTT : Unsigned_32;
      Pipe_Width, Pipe_Height : Positive; Single_Pipe : Boolean;
      Result : out CS.Readiness)
   is
      Written : Boolean;
      Read_OK : Boolean;
   begin
      Configured := False;
      Result := CS.No_Allocation;
      if Staging = 0 or else Surfaces_CPU = 0 or else Surfaces_GGTT = 0 or else
        Surfaces_GGTT mod CP.VTd_Base_Alignment /= 0 or else
        Unsigned_64 (Surfaces_GGTT) > Unsigned_64 (Unsigned_32'Last) -
          Unsigned_64 (CS.Surface_Count) * CS.Surface_Bytes
      then
         return;
      end if;
      for I in CS.Surface_Index loop
         Surface_Bases (I) := Surfaces_GGTT +
           Unsigned_32 (CS.Surface_Index'Pos (I) - CS.Surface_Index'Pos (CS.Surface_Index'First)) * CS.Surface_Bytes;
      end loop;
      Snapshot (Snap_Before, Read_OK);
      Snap_After := Snap_Before;
      Buffer_Word := Snap_Before.Cursor_Buffer;
      Watermark_Word := Snap_Before.Cursor_WM (DDB.Level_0);
      if not Read_OK then
         return;
      end if;
      Result := CS.Classify (Buffer_Word, Watermark_Word);
      case Result is
         when CS.No_Allocation =>
            if Buffer_Word /= 0 then return; end if;
            Allocate (Single_Pipe, Result);
            Snapshot (Snap_After, Read_OK);
            if Result /= CS.Ready_Driver_Allocation then return; end if;
         when CS.Ready_Fallback_Watermark =>
            --  Double-buffered: armed by the first CURBASE write (Show).
            Write (CS.Watermark_Offset,
                   CS.Fallback_Watermark (CS.Allocation (Buffer_Word)), Written);
            if not Written then
               Result := CS.No_Allocation;
               return;
            end if;
         when others => null;
      end case;
      Staging_Address := Staging;
      Surface_CPU_First := Surfaces_CPU;
      Width_Limit := Pipe_Width;
      Height_Limit := Pipe_Height;
      Defined := False;
      Is_Enabled := False;
      Armed_Base := 0;
      Configured := True;
   end Configure;

   function Ready return Boolean is (Configured);
   function Image return CP.Image is (Current);
   function Enabled return Boolean is (Is_Enabled);
   function Buffer_Config return Unsigned_32 is (Buffer_Word);
   function Watermark return Unsigned_32 is (Watermark_Word);

   procedure Take (Item : out Counters) is
   begin
      Item := Stats;
      Stats := (others => 0);
   end Take;

   function On_Pipe (X, Y : Integer) return Boolean is
     (X in Integer (CP.Position'First) .. Integer (CP.Position'Last) and then
      Y in Integer (CP.Position'First) .. Integer (CP.Position'Last) and then
      CP.Valid_Position (Current, CP.Position (X), CP.Position (Y),
                         Width_Limit, Height_Limit));

   function Values (X, Y : Integer) return CP.Register_Values is
     (CP.Encode (Current, CP.Position (X), CP.Position (Y), Display_Version,
                 VTd_Workaround => True))
     with Pre => On_Pipe (X, Y);

   --  Disable: CURCNTR mode zero. The base write (zero) arms it.
   procedure Disable (Success : out Boolean) is
   begin
      Success := Program (CP.Disabled);
      if Success then
         Is_Enabled := False;
         Armed_Base := 0;
      end if;
   end Disable;

   --  Enable or re-shape at the current position (already On_Pipe).
   procedure Arm_Full (Success : out Boolean) is
   begin
      Success := Program (Values (Position_X, Position_Y));
      if Success then
         Is_Enabled := True;
         Armed_Base := Current.Base;
      end if;
   end Arm_Full;

   type Staged_Pixels is array (0 .. CS.Edge * CS.Edge - 1) of Unsigned_32;

   procedure Show (X, Y : Integer; Result : out Status) is
      Staged : Staged_Pixels with Import, Volatile,
        Address => To_Address (Integer_Address (Staging_Address));
      Bounds : CS.Extent;
      Live : Unsigned_32;
      Target : CS.Surface_Index;
      Next : CP.Image;
      Success : Boolean;
   begin
      Result := Bad_State;
      if not Configured then
         Count (Stats.Failures);
         return;
      end if;
      --  Bounding extent of the nonzero pixels (premultiplied: transparent
      --  pixels are all zero).
      for Row in 0 .. CS.Edge - 1 loop
         for Column in 0 .. CS.Edge - 1 loop
            if Staged (Row * CS.Edge + Column) /= 0 then
               Bounds.Width := Integer'Max (Bounds.Width, Column + 1);
               Bounds.Height := Integer'Max (Bounds.Height, Row + 1);
            end if;
         end loop;
      end loop;
      Position_X := X;
      Position_Y := Y;
      if Bounds.Width = 0 or else Bounds.Height = 0 then
         --  Nothing to show: defined, but the plane stays off.
         Disable (Success);
         if not Success then Count (Stats.Failures); return; end if;
         Defined := True;
         Empty_Image := True;
         Count (Stats.Shows);
         Result := Accepted;
         return;
      end if;
      Live := Read (CS.Live_Base_Offset);
      if Live = Unsigned_32'Last then
         Count (Stats.Failures);
         return;
      end if;
      Target := CS.Pick (Surface_Bases, Armed_Base, Live);
      Next := CS.Fit (Bounds);
      Next.Base := Surface_Bases (Target);
      declare
         Pitch : constant Natural := CP.Pixels (Next.Width);
         First : constant Unsigned_64 := Surface_CPU_First +
           Unsigned_64 (CS.Surface_Index'Pos (Target) - CS.Surface_Index'Pos (CS.Surface_Index'First)) * CS.Surface_Bytes;
         type Surface_Pixels is array (0 .. Pitch * Next.Height - 1) of Unsigned_32;
         Surface : Surface_Pixels with Import, Volatile,
           Address => To_Address (Integer_Address (First));
      begin
         for Row in 0 .. Next.Height - 1 loop
            for Column in 0 .. Pitch - 1 loop
               Surface (Row * Pitch + Column) := Staged (Row * CS.Edge + Column);
            end loop;
         end loop;
         if not Flush (First, Unsigned_64 (Surface_Pixels'Length) * CS.Bytes_Per_Pixel) then
            Count (Stats.Failures);
            return;
         end if;
      end;
      Current := Next;
      Defined := True;
      Empty_Image := False;
      if On_Pipe (X, Y) then
         Arm_Full (Success);
      else
         Disable (Success);
         Count (Stats.Off_Pipe);
      end if;
      if not Success then
         Count (Stats.Failures);
         return;
      end if;
      Count (Stats.Shows);
      Result := Accepted;
   end Show;

   procedure Move_To (X, Y : Integer; Result : out Status) is
      Success : Boolean := True;
   begin
      Result := Bad_State;
      if not Configured or else not Defined then
         Count (Stats.Failures);
         return;
      end if;
      Position_X := X;
      Position_Y := Y;
      Count (Stats.Moves);
      if not Empty_Image then
         if On_Pipe (X, Y) then
            if Is_Enabled then
               Success := Move (Values (X, Y));
               if Success then Count (Stats.Move_Writes); end if;
            else
               Arm_Full (Success);
            end if;
         elsif Is_Enabled then
            Disable (Success);
            Count (Stats.Off_Pipe);
         end if;
      end if;
      if not Success then
         Count (Stats.Failures);
         return;
      end if;
      Result := Accepted;
   end Move_To;

   procedure Hide (Result : out Status) is
      Success : Boolean;
   begin
      Result := Bad_State;
      if not Configured then
         Count (Stats.Failures);
         return;
      end if;
      Disable (Success);
      if not Success then
         Count (Stats.Failures);
         return;
      end if;
      Defined := False;
      Count (Stats.Hides);
      Result := Accepted;
   end Hide;
end Intel_GPU_Cursor_Plane;
