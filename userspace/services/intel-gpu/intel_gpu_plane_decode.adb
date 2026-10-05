with Intel_GPU_Plane_Control;
package body Intel_GPU_Plane_Decode with SPARK_Mode is
   function Decode
     (Before, After : Sample; Table_Bytes : Unsigned_64) return Decoded
   is
      use Intel_GPU_Plane_Control;
      Fields : Control := From_Word (Before.Control);
      Pitch : constant Stride_Register := Stride_From_Word (Before.Stride);
      Dimensions : constant Size_Register := Size_From_Word (Before.Size);
      Origin : constant Offset_Register := Offset_From_Word (Before.Offset);
      Surface : constant Surface_Register := Surface_From_Word (Before.Surface);
      Live : constant Live_Surface_Register :=
        Live_Surface_From_Word (Before.Live_Surface);
      Footprint : Intel_GPU_Scanout_Range.Extent;
   begin
      if Before.Control = Unsigned_32'Last or else
        Before.Stride = Unsigned_32'Last or else Before.Size = Unsigned_32'Last or else
        Before.Offset = Unsigned_32'Last or else Before.Surface = Unsigned_32'Last or else
        Before.Live_Surface = Unsigned_32'Last
      then return (Invalid_Read, (others => <>)); end if;
      if Before /= After then return (Changing, (others => <>)); end if;
      if Fields.Enabled = 0 then
         return (Disabled, (others => <>));
      end if;
      -- These fields do not alter the supported linear RGB footprint.
      -- Bit 3 permits global double-buffer-update suspension; leave hardware
      -- unchanged. All other control fields remain conservatively checked.
      Fields.Enabled := 0;
      Fields.RGB_Order := 0;
      Fields.Allow_Update_Disable := 0;
      if Fields.Pixel_Format /= 8 then
         return (Unsupported, (others => <>));
      end if;
      Fields.Pixel_Format := 0;
      if To_Word (Fields) /= 0 or else
        Pitch.Reserved /= 0 or else
        Dimensions.Reserved_13 /= 0 or else Dimensions.Reserved_29 /= 0 or else
        Origin.Reserved_13 /= 0 or else Origin.Reserved_29 /= 0 or else
        Surface.Reserved_0 /= 0 or else Surface.Reserved_4 /= 0 or else
        Live.Reserved /= 0
      then return (Unsupported, (others => <>)); end if;
      if Surface.Base_Page /= Live.Base_Page then
         return (Changing, (others => <>));
      end if;
      -- PLANE_SIZE stores dimension minus one; linear PLANE_STRIDE is in
      -- 64-byte units. Offsets are source pixels, not destination position.
      Footprint := Intel_GPU_Scanout_Range.Linear
        (Table_Bytes, Unsigned_64 (Surface.Base_Page) * 4096,
         Unsigned_64 (Pitch.Cache_Lines) * 64,
         Unsigned_64 (Dimensions.Width_Minus_One) + 1,
         Unsigned_64 (Dimensions.Height_Minus_One) + 1,
         Unsigned_64 (Origin.Start_X),
         Unsigned_64 (Origin.Start_Y), 4);
      if not Footprint.Valid then
         return (Invalid_Geometry, (others => <>));
      end if;
      return (Linear_Ready, Footprint);
   end Decode;
   function Plan_Linear_Flip
     (Before, After : Sample; Table_Bytes, Target_First, Target_Bytes : Unsigned_64)
      return Flip_Plan is
      use Intel_GPU_Plane_Control;
      Current : constant Decoded := Decode (Before, After, Table_Bytes);
      Candidate : Sample := Before;
      Surface : Surface_Register := (others => <>);
      Proposed : Decoded;
      Empty : Flip_Plan;
   begin
      -- Check wide values before narrowing into the 20-bit page field. The
      -- full range must fit 32-bit display addressing and the GGTT aperture.
      if Current.State /= Linear_Ready or else
        Target_First mod 4096 /= 0 or else Target_First >= 2 ** 32 or else
        Target_Bytes = 0 or else Target_Bytes mod 4096 /= 0 or else
        Target_Bytes > 2 ** 32 - Target_First
      then return Empty; end if;
      if Target_First >= Table_Bytes / 8 * 4096 or else
        Target_Bytes > Table_Bytes / 8 * 4096 - Target_First
      then return Empty; end if;
      Surface.Base_Page := Bits_20 (Target_First / 4096);
      -- MMIO proposal: no ring-flip source or reserved bits are copied.
      Candidate.Surface := Surface_To_Word (Surface);
      -- Synthetic live value for geometry decoding ONLY, not latch evidence.
      Candidate.Live_Surface := Candidate.Surface;
      Proposed := Decode (Candidate, Candidate, Table_Bytes);
      if Proposed.State /= Linear_Ready or else Proposed.Memory.Bytes > Target_Bytes then
         return Empty;
      end if;
      -- Keep both old/new GGTT ranges distinct during the pending flip.
      -- Physical alias exclusion remains the allocation owner's obligation.
      if Target_First < Current.Memory.First + Current.Memory.Bytes and then
        Current.Memory.First < Target_First + Target_Bytes
      then return Empty; end if;
      return (True, Candidate.Surface, Proposed.Memory);
   end Plan_Linear_Flip;
end Intel_GPU_Plane_Decode;
