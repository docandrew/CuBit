with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages;
with Desktop_Logs;
with Desktop_Wallpaper;

package body Desktop_Wallpaper_Layers is
   use type System.Address;
   use type CuBit.Appearance.Preferences;

   Pixel_Bytes : constant := 4;
   Page_Bytes  : constant := 4_096;
   --  The largest layer: one 8K output (also bounds every offset below).
   Maximum_Side : constant := 8_192;
   LF : constant Character := Character'Val (10);

   type Retained is record
      Base   : System.Address := System.Null_Address;
      Bytes  : Unsigned_64 := 0;          --  whole pages
      Width  : Natural := 0;
      Height : Natural := 0;
      Style  : CuBit.Appearance.Preferences := CuBit.Appearance.Default;
      Built  : Boolean := False;
   end record;
   Layers : array (Layer_Index) of Retained;
   --  Report one allocation failure, not one per frame.
   Allocation_Reported : Boolean := False;

   function Memcpy (Target, Source : System.Address; Bytes : Storage_Count) return System.Address
     with Import, Convention => C, External_Name => "memcpy";

   procedure Release (Layer : Layer_Index) is
      L : Retained renames Layers (Layer);
      Result : Unsigned_64;
   begin
      if L.Base /= System.Null_Address then
         Result := CuBit.Messages.syscall
           (CuBit.Messages.SYSCALL_RELEASE_OWNED_MEMORY,
            Unsigned_64 (To_Integer (L.Base)), L.Bytes);
         if Result /= 0 then
            Desktop_Logs.Warn ("desktop: wallpaper layer release failed output=" & Layer'Image);
         end if;
      end if;
      L := (others => <>);
   end Release;

   --  Make Layer hold Style at Width x Height; False if it cannot.
   function Ensure (Layer : Layer_Index; Width, Height : Positive;
                    Style : CuBit.Appearance.Preferences) return Boolean
   is
      L : Retained renames Layers (Layer);
      Needed : constant Unsigned_64 :=
        (Unsigned_64 (Width) * Unsigned_64 (Height) * Pixel_Bytes + Page_Bytes - 1)
          / Page_Bytes * Page_Bytes;
      Raw : Unsigned_64;
      Started : Unsigned_64;
   begin
      if L.Built and then L.Width = Width and then L.Height = Height and then L.Style = Style then
         return True;
      end if;
      if L.Base /= System.Null_Address and then L.Bytes /= Needed then
         Release (Layer);
      end if;
      if L.Base = System.Null_Address then
         Raw := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_ALLOCATE_OWNED_MEMORY, Needed);
         if Raw = 0 or else Raw = Unsigned_64'Last or else Raw mod Page_Bytes /= 0 then
            if not Allocation_Reported then
               Allocation_Reported := True;
               Desktop_Logs.Warn ("desktop: wallpaper layer unavailable bytes=" & Needed'Image &
                                  "; resampling damage instead");
            end if;
            return False;
         end if;
         L.Base := To_Address (Integer_Address (Raw));
         L.Bytes := Needed;
      end if;
      Started := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME);
      Desktop_Wallpaper.Paint (L.Base, Width, Height, Width * Pixel_Bytes, 0, 0, Width, Height, Style);
      L.Width := Width;
      L.Height := Height;
      L.Style := Style;
      L.Built := True;
      Desktop_Logs.Write ("desktop: wallpaper layer built output=" & Layer'Image & " size=" &
        Width'Image & " x" & Height'Image & " ms=" &
        Unsigned_64'Image (CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME) - Started) & LF);
      return True;
   end Ensure;

   procedure Paint
     (Layer : Layer_Index; Target : System.Address;
      Width, Height, Pitch : Positive; X, Y, W, H : Natural;
      Style : CuBit.Appearance.Preferences)
   is
      Ignore : System.Address;
   begin
      --  The same caller guards as Desktop_Wallpaper.Paint, before any
      --  offset is formed.
      if Width > Maximum_Side or else Height > Maximum_Side or else
        Pitch / Pixel_Bytes < Width or else Pitch > Natural'Last / Height or else
        X >= Width or else Y >= Height or else W = 0 or else H = 0 or else
        W > Width - X or else H > Height - Y
      then
         return;
      end if;
      if not Ensure (Layer, Width, Height, Style) then
         Desktop_Wallpaper.Paint (Target, Width, Height, Pitch, X, Y, W, H, Style);
         return;
      end if;
      for Row in Y .. Y + H - 1 loop
         Ignore := Memcpy
           (Target + Storage_Offset (Row * Pitch + X * Pixel_Bytes),
            Layers (Layer).Base + Storage_Offset ((Row * Width + X) * Pixel_Bytes),
            Storage_Count (W * Pixel_Bytes));
      end loop;
   end Paint;
end Desktop_Wallpaper_Layers;
