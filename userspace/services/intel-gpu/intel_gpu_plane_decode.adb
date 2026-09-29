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
end Intel_GPU_Plane_Decode;
