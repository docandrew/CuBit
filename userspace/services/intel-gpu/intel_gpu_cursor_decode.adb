with Intel_GPU_Cursor_Control;
package body Intel_GPU_Cursor_Decode with SPARK_Mode is
   use type Intel_GPU_Cursor_Control.Bits_6;
   function Decode (Before, After : Sample; Table_Bytes : Unsigned_64)
     return Decoded
   is
      Width : Unsigned_64;
      Memory : Intel_GPU_Scanout_Range.Extent;
   begin
      if Before.Control = Unsigned_32'Last or else Before.Base = Unsigned_32'Last
        or else Before.Live_Base = Unsigned_32'Last or else Before.FBC_Control = Unsigned_32'Last
      then return (Invalid_Read, (others => <>)); end if;
      if Before /= After then return (Changing, (others => <>)); end if;
      -- All six mode bits must be zero. Reserved nonzero modes are not
      -- evidence that a cursor is disabled, even if Linux's sparse mask
      -- omits those reserved bits.
      if Intel_GPU_Cursor_Control.From_Word (Before.Control).Mode_Select = 0
      then return (Disabled, (others => <>)); end if;
      if Before.Control not in 16#22# | 16#23# | 16#27# or else Before.FBC_Control /= 0 then
         return (Unsupported, (others => <>));
      end if;
      if Before.Base /= Before.Live_Base then return (Changing, (others => <>)); end if;
      Width := (case Before.Control is when 16#27# => 64,
                 when 16#22# => 128, when others => 256);
      Memory := Intel_GPU_Scanout_Range.Linear
        (Table_Bytes, Unsigned_64 (Before.Base), Width * 4, Width, Width, 0, 0, 4);
      if not Memory.Valid then return (Invalid_Geometry, (others => <>)); end if;
      -- Linear's public contract bounds the range but does not expose its
      -- exact length. Check it before promising the cursor-specific extent.
      if Memory.Bytes /= Width * Width * 4 then
         return (Invalid_Geometry, (others => <>));
      end if;
      return (Ready, Memory);
   end Decode;
end Intel_GPU_Cursor_Decode;
