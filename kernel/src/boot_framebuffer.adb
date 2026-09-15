pragma Ada_2022;
package body Boot_Framebuffer with SPARK_Mode is
   subtype Wire_Size is Address range 0 .. 2 ** 32 - 1;
   -- Keep the geometry proof in a signed numeric domain, separate from wire
   -- format/tag decoding and its modular integer conversions.
   function Admit_Geometry
     (Base : Address; Width, Height : Extent; Row_Bytes : Wire_Size;
      Physical_Limit : Address; Budget : Byte_Count) return Result
     with Post =>
       (if Admit_Geometry'Result.State = Success then
          Valid (Admit_Geometry'Result.Value) and then
          Admit_Geometry'Result.Value.Base = Base and then
          Admit_Geometry'Result.Value.Width = Width and then
          Admit_Geometry'Result.Value.Height = Height and then
          Address (Admit_Geometry'Result.Value.Pitch) = Row_Bytes and then
          Admit_Geometry'Result.Value.Bytes <= Budget and then
          Admit_Geometry'Result.Value.Map_Limit <= Physical_Limit)
   is
      First, Limit, Bytes, Rounded : Address;
      Pitch : Byte_Count;
   begin
      if Row_Bytes = 0 or else Row_Bytes mod 4 /= 0 or else
        Row_Bytes < Address (Width) * 4
      then
         return (State => Invalid_Geometry);
      end if;
      Bytes := Row_Bytes * Address (Height);
      if Bytes > Address (Budget) then
         return (State => Budget_Exceeded);
      end if;
      Pitch := Byte_Count (Row_Bytes);
      if Base = 0 or else Base >= Physical_Limit or else Bytes > Physical_Limit - Base then
         return (State => Invalid_Address);
      end if;
      Limit := Base + Bytes;
      First := Base - Base mod Page_Bytes;
      Rounded := Limit - Limit mod Page_Bytes;
      if Limit mod Page_Bytes /= 0 then
         if Page_Bytes > Physical_Limit - Rounded then
            return (State => Invalid_Address);
         end if;
         Rounded := Rounded + Page_Bytes;
      end if;
      return (State => Success,
              Value => (Base => Base, Map_First => First, Map_Limit => Rounded,
                        Width => Width, Height => Height, Pitch => Pitch,
                        Bytes => Byte_Count (Bytes), Format => BGRX_8888));
   end Admit_Geometry;

   function Decode (Raw : Raw_Description; Physical_Limit : Address;
                    Budget : Byte_Count) return Result
   is
   begin
      if Raw.Kind = 2 then
         -- The existing EGA adapter only implements this fixed text layout.
         if Raw.Base = 16#B8000# and then Raw.Width = 80 and then
           Raw.Height = 25 and then Raw.Pitch = 160 and then Raw.Depth = 16
         then
            return (State => Text_Mode);
         end if;
         return (State => Unsupported_Format);
      end if;
      if Raw.Kind /= 1 or else Raw.Depth /= 32 or else
        Raw.Red_Position /= 16 or else Raw.Red_Size /= 8 or else
        Raw.Green_Position /= 8 or else Raw.Green_Size /= 8 or else
        Raw.Blue_Position /= 0 or else Raw.Blue_Size /= 8
      then
         return (State => Unsupported_Format);
      end if;
      if Raw.Width not in 1 .. Unsigned_32 (Extent'Last) or else
        Raw.Height not in 1 .. Unsigned_32 (Extent'Last)
      then
         return (State => Invalid_Geometry);
      end if;
      if Raw.Base not in 1 .. Unsigned_64 (Address'Last) then
         return (State => Invalid_Address);
      end if;
      return Admit_Geometry (Address (Raw.Base), Extent (Raw.Width),
        Extent (Raw.Height), Wire_Size (Raw.Pitch), Physical_Limit, Budget);
   end Decode;

   function Pixel_Offset (Item : Description; X, Y : Natural) return Natural is
   begin
      return Natural (Address (Y) * Address (Item.Pitch) + Address (X) * 4);
   end Pixel_Offset;
end Boot_Framebuffer;
