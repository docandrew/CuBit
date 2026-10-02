with Interfaces;
with Compositor_Glyph_FFI;
with System.Storage_Elements;
package body Compositor_Glyph_Storage with SPARK_Mode => Off is
   use type M.Arena.Token, System.Address;
   function Occupied (S : State) return Flags is
      Result : Flags;
   begin
      for I in Slot loop Result (I) := S.Backing (I).Token /= M.Arena.No_Token; end loop;
      return Result;
   end Occupied;
   function Has (S : State; I : Slot) return Boolean is (S.Backing (I).Token /= M.Arena.No_Token);
   function Pixels (S : State; I : Slot) return System.Address is (S.Backing (I).Address);
   function Capacity (S : State; I : Slot) return Natural is (S.Backing (I).Size);
   procedure Allocate (S : in out State; I : Slot; Layout : L.Layout; Success : out Boolean) is
   begin
      Success := False;
      if Has (S, I) or else not L.Valid (Layout) then return; end if;
      M.Reserve (S.Memory, Layout.Bytes, S.Backing (I).Token, S.Backing (I).Address, S.Backing (I).Size);
      Success := Has (S, I);
      if Success then S.Backing (I).Raster := Layout; end if;
   end Allocate;
   procedure Rasterize (S : in out State; I : Slot; Face, Code : Natural;
                        Layout : L.Layout; Advance : out Natural; Success : out Boolean) is
   begin
      Advance := 0; Success := False;
      if not Has (S, I) or else not L.Same_Raster (Layout, S.Backing (I).Raster) then return; end if;
      Compositor_Glyph_FFI.Rasterize (Interfaces.Unsigned_32 (Face), Interfaces.Unsigned_32 (Code),
        Layout, Pixels (S, I), Interfaces.Unsigned_64 (Capacity (S, I)), Advance, Success);
   end Rasterize;
   function Can_Paint (S : State; I : Slot; Screen : L.G.Output) return Boolean is
     (Has (S, I) and then L.Same_Raster (S.Backing (I).Raster, L.Plan (Screen.Scale)) and then
      Capacity (S, I) >= S.Backing (I).Raster.Bytes and then Pixels (S, I) /= System.Null_Address);
   procedure Paint (S : in out State; I : Slot; Screen : L.G.Output;
                    Origin : L.G.Logical_Point; Damage : L.G.Physical_Rectangle;
                    Target : in out Software.Pixels; Pitch : Positive; Tint : Software.Word;
                    Success : out Boolean) is
      use System.Storage_Elements;
      Source_First, Target_First, Source_Bytes, Target_Bytes : Integer_Address;
   begin
      Success := False;
      if not Can_Paint (S, I, Screen) or else not Software.Fits_Target (Screen, Target, Pitch) or else
        M.Address_Of (S.Memory, S.Backing (I).Token) /= Pixels (S, I) then return; end if;
      Source_First := To_Integer (Pixels (S, I)); Target_First := To_Integer (Target'Address);
      Source_Bytes := Integer_Address (S.Backing (I).Raster.Bytes);
      Target_Bytes := (Integer_Address (Target'Last) + 1) * 4;
      if Source_Bytes > Integer_Address'Last - Source_First or else
        Target_Bytes > Integer_Address'Last - Target_First or else
        (Source_First < Target_First + Target_Bytes and Target_First < Source_First + Source_Bytes)
      then return; end if;
      declare
         Mask : Software.Bytes (0 .. S.Backing (I).Raster.Bytes - 1)
           with Import, Address => Pixels (S, I);
      begin
         Software.Paint (Screen, Origin, Damage, Mask, Target, Pitch, Tint);
      end;
      Success := True;
   end Paint;
   procedure Release (S : in out State; I : Slot; Success : out Boolean) is
   begin
      M.Release (S.Memory, S.Backing (I).Token, True, Success);
      if Success then S.Backing (I) := (others => <>); end if;
   end Release;
end Compositor_Glyph_Storage;
