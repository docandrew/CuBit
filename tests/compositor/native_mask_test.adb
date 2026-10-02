with Interfaces; use Interfaces;
with System;
with Mesa_FFI;
with Mesa_Affine_FFI;
with Mesa_Mask_FFI;
with Mesa_Cache;
with Mesa_Masks;
with Mesa_Binding.Affine;
with Compositor_Formats;
with Compositor_Policy;
with Compositor_Glyph_Layout;
with Compositor_Glyph_FFI;
with Compositor_Glyph_Cache;
with Compositor_Glyph_Memory;
with Compositor_Affine;
with Compositor_Sampling;
function Native_Mask_Test (Shared : Boolean := False) return Boolean is
   package L renames Compositor_Glyph_Layout;
   package A renames Compositor_Affine;
   package S renames Compositor_Sampling;
   package G renames S.G;
   package Cache is new Compositor_Glyph_Cache;
   package Memory renames Compositor_Glyph_Memory;
   package Views renames Mesa_Cache;
   View_State : Views.State;
   Color_Index : Views.Source_Slot;
   use type Memory.Arena.Token;
   use type Cache.Token, Cache.Lease;
   use type System.Address;
   use type G.Logical_Coordinate;
   type Bytes is array (Natural range <>) of Unsigned_8;
   type Pixels is array (Natural range <>) of Unsigned_32;
   Store : Memory.State;
   Backing : Memory.Arena.Token;
   Mask_Pixels : System.Address;
   Mask_Capacity : Natural;
   Original : Bytes (0 .. 4095);
   Target : aliased Pixels (0 .. 84 * 72 - 1) with Alignment => 64;
   Color : aliased Pixels (0 .. 0) := (0 => 16#FF23_4567#);
   TI : aliased Mesa_FFI.Image := (Target'Address, 80, 72, 84 * 4, 1);
   CI : aliased Mesa_FFI.Image := (Color'Address, 1, 1, 4, 0);
   Context, Src, Dst, Color_Source : System.Address;
   Screen : G.Output := (Width => 80, Height => 72, others => <>);
   Surface : constant G.Logical_Rectangle := (-4, 4, 28, 21);
   Scales : constant array (1 .. 4) of G.UI_Scale := ((1, 1), (5, 4), (3, 2), (2, 1));
   Tints : constant array (1 .. 5) of Unsigned_32 :=
     (16#FFFF_FFFF#, 16#FF27_91D3#, 16#8027_91D3#, 16#0027_91D3#, 16#FF00_0000#);
   Sentinel : constant Unsigned_32 := 16#FF12_3456#;
   Advance : Natural;
   OK : Boolean;
   Count : Unsigned_32 := 0;
   Cached : Cache.State := Cache.Open (4_096);
   Ticket : Cache.Token;
   Reading : Cache.Lease;
   Accepted : Boolean;
   procedure Report (Shared : Unsigned_32) with Import, Convention => C, External_Name => "compositor_mask_report";
   procedure Mismatch (Frame, Index, Actual, Expected : Unsigned_32)
     with Import, Convention => C, External_Name => "compositor_test_mismatch";
   function Channel (P : Unsigned_32; C : Natural) return Natural is
     (Natural (Shift_Right (P, C * 8) and 255));
   function Blended (Coverage : Unsigned_8; Tint : Unsigned_32) return Unsigned_32 is
      Alpha : constant Natural := Natural (Coverage) * Channel (Tint, 3);
      Result : Unsigned_32 := 0;
   begin
      for C in 0 .. 3 loop
         declare
            Foreground : constant Natural := (if C = 3 then 255 else Channel (Tint, C));
            V : constant Natural := (Foreground * Alpha + Channel (Sentinel, C) * (65025 - Alpha) + 32512) / 65025;
         begin Result := Result or Shift_Left (Unsigned_32 (V), C * 8); end;
      end loop;
      return Result;
   end Blended;
   procedure Draw_Shared_Color (D : A.Draw; Success : out Boolean) is
      function Fits (Source, Target : Compositor_Formats.Image) return Boolean is
        (Source.Pixels /= Target.Pixels and Target.Writable = 1 and
         Target.Width = Unsigned_32 (Screen.Width) and Target.Height = Unsigned_32 (Screen.Height) and
         A.Valid (D, Screen.Width, Screen.Height));
      procedure Draw (Library : in out Mesa_Binding.Context; Target, Source : System.Address;
                      Result : out Compositor_Policy.Completion) is
      begin
         Mesa_Binding.Affine.Render (Library, Target, Source, D, Screen.Width, Screen.Height, Result);
      end Draw;
      procedure Execute is new Views.Render_Checked (Fits, Draw);
   begin
      Execute (View_State, 0, Color_Index, Success);
   end Draw_Shared_Color;
begin
   if Shared then
      Views.Initialize (View_State, True);
      Views.Ensure (View_State, 0, TI, Target'Length * 4, OK);
      if not OK then return False; end if;
      Views.Ensure_Source (View_State, CI, 4, Color_Index, OK);
      if not OK then return False; end if;
   else
      Context := Mesa_FFI.Create;
      if Context = System.Null_Address then return False; end if;
      Dst := Mesa_FFI.Import_Image (Context, TI'Access);
      Color_Source := Mesa_FFI.Import_Image (Context, CI'Access);
      if Dst = System.Null_Address or Color_Source = System.Null_Address then return False; end if;
   end if;
   for Face in 0 .. 1 loop
      for Scale of Scales loop
         Screen.Scale := Scale;
         declare Layout : constant L.Layout := L.Plan (Scale);
         begin
            Cache.Reserve (Cached, (Face, Character'Pos ('W'), Scale), Ticket);
            if Ticket = Cache.No_Token or else Cache.Charged (Cached) /= Layout.Bytes then return False; end if;
            Memory.Reserve (Store, Layout.Bytes, Backing, Mask_Pixels, Mask_Capacity);
            if Backing = Memory.Arena.No_Token or else Mask_Pixels = System.Null_Address or else
              Mask_Capacity < Layout.Bytes or else Mask_Capacity > Original'Length
            then return False; end if;
            declare
               Mask : Bytes (0 .. Mask_Capacity - 1) with Address => Mask_Pixels;
            begin
            Mask := (others => 16#A5#);
            Compositor_Glyph_FFI.Rasterize
              (Unsigned_32 (Face), Character'Pos ('W'), Layout, Mask'Address, Mask'Length, Advance, OK);
            if not OK then return False; end if;
            if Shared then
               for Hit in 1 .. 2 loop
                  Mesa_Masks.Ensure (View_State, Views.Mask_Slot'First, Mask'Address, Layout, Mask'Length, OK);
                  if not OK then return False; end if;
               end loop;
            else
               if Mesa_Mask_FFI.Import_Mask (Context, Mask'Address, Layout, Unsigned_64 (Layout.Bytes - 1)) /= System.Null_Address then return False; end if;
               Src := Mesa_Mask_FFI.Import_Mask (Context, Mask'Address, Layout, Mask'Length);
               if Src = System.Null_Address then return False; end if;
            end if;
            Cache.Publish (Cached, Ticket, True);
            for Rotation in G.Orientation loop
               Screen.Rotation := Rotation;
               declare Plan : constant A.Result := A.Plan (Screen, Surface, Over => True);
               begin
                  if not Plan.Visible then return False; end if;
                  for Damage in 1 .. 2 loop
                     declare
                        Clipped : constant A.Result := A.Clip (Plan.Value, Screen.Width, Screen.Height,
                          (if Damage = 1 then (0, 0, 80, 72) else (2, 7, 78, 68)));
                     begin
                        if Clipped.Visible then
                           for Tint of Tints loop
                              -- Mutate already-imported storage between draws. This catches
                              -- snapshot imports and stale texture caches without reimporting.
                              for Y in 0 .. Layout.Height - 1 loop
                                 for X in 0 .. Layout.Width - 1 loop
                                    Mask (Y * Layout.Pitch + X) := 255 - Mask (Y * Layout.Pitch + X);
                                 end loop;
                              end loop;
                              Original (Mask'Range) := Mask;
                              Target := (others => Sentinel);
                              declare D : aliased A.Draw := Clipped.Value;
                              begin
                                 Cache.Acquire (Cached, Ticket, Reading);
                                 if Reading = Cache.No_Lease then return False; end if;
                                 if Shared then
                                    Mesa_Masks.Render (View_State, 0, Views.Mask_Slot'First, D, Screen.Width, Screen.Height, Tint, OK);
                                    if not OK then return False; end if;
                                 elsif Mesa_Mask_FFI.Render (Context, Dst, Src, D'Access, Screen.Width, Screen.Height, Tint) /= 0 then return False;
                                 end if;
                                 -- Withhold confirmation despite synchronous success: policy
                                 -- must retain the read until its exact completion is supplied.
                                 Cache.Complete (Cached, Reading, False);
                                 Cache.Begin_Retirement (Cached, Ticket, Accepted);
                                 if Accepted or else not Cache.Active (Cached, Reading) then return False; end if;
                                 Cache.Complete (Cached, Reading, True);
                                 if Cache.Reader_Count (Cached) /= 0 then return False; end if;
                                 for I in Target'Range loop
                                    declare
                                       X : constant Natural := I mod 84;
                                       Y : constant Natural := I / 84;
                                       Expected : Unsigned_32 := Sentinel;
                                    begin
                                       if X < 80 and then Unsigned_32 (X) in D.Clip_X .. D.Clip_X + D.Clip_W - 1 and then
                                         Unsigned_32 (Y) in D.Clip_Y .. D.Clip_Y + D.Clip_H - 1
                                       then
                                          declare M : constant S.Sample := S.Map
                                            (Screen, (G.Pixel_Index (X), G.Pixel_Index (Y)), Surface,
                                             G.Physical_Extent (Layout.Width), G.Physical_Extent (Layout.Height));
                                          begin
                                             if not M.Valid then return False; end if;
                                             Expected := Blended (Mask (Natural (M.Y) * Layout.Pitch + Natural (M.X)), Tint);
                                          end;
                                       end if;
                                       for C in 0 .. 3 loop
                                          if abs (Channel (Expected, C) - Channel (Target (I), C)) >
                                            (if Expected = Sentinel then 0 else 2)
                                          then Mismatch (Count, Unsigned_32 (I), Target (I), Expected); return False; end if;
                                       end loop;
                                    end;
                                 end loop;
                              end;
                              if Mask /= Original (Mask'Range) then return False; end if;
                              Count := Count + 1;
                           end loop;
                        end if;
                     end;
                  end loop;
                  declare D : aliased A.Draw := Plan.Value;
                  begin
                     Target := (others => Sentinel);
                     if not Shared then
                     if Mesa_Affine_FFI.Render (Context, Dst, Src, D'Access, Screen.Width, Screen.Height) /= 1 or else
                       Mesa_Mask_FFI.Render (Context, Dst, Color_Source, D'Access, Screen.Width, Screen.Height, Tints (1)) /= 1 or else
                       Mesa_Mask_FFI.Render (Context, Src, Src, D'Access, Screen.Width, Screen.Height, Tints (1)) /= 1
                     then return False; end if;
                     D.Over := 0;
                     if Mesa_Mask_FFI.Render (Context, Dst, Src, D'Access, Screen.Width, Screen.Height, Tints (1)) /= 1 then return False; end if;
                     for Pixel of Target loop if Pixel /= Sentinel then return False; end if; end loop;
                     end if;
                     -- A color draw after mask draws must restore the regular shader.
                     D.Over := 0;
                     if Shared then
                        Draw_Shared_Color (D, OK);
                        if not OK then return False; end if;
                     elsif Mesa_Affine_FFI.Render (Context, Dst, Color_Source, D'Access, Screen.Width, Screen.Height) /= 0 then return False;
                     end if;
                     for Y in Natural (D.Clip_Y) .. Natural (D.Clip_Y + D.Clip_H - 1) loop
                        for X in Natural (D.Clip_X) .. Natural (D.Clip_X + D.Clip_W - 1) loop
                           if Target (Y * 84 + X) /= Color (0) then return False; end if;
                        end loop;
                     end loop;
                     if Shared then
                        Views.Forget_Targets (View_State);
                        if not Views.Can_Retire (View_State) or else Views.Empty (View_State, Views.Mask_Slot'First) then return False; end if;
                        Views.Ensure (View_State, 0, TI, Target'Length * 4, OK);
                        if not OK then return False; end if;
                     end if;
                  end;
               end;
            end loop;
            Cache.Begin_Retirement (Cached, Ticket, Accepted);
            if not Accepted then return False; end if;
            if Shared then
               Views.Forget (View_State, Views.Mask_Slot'First);
               if not Views.Can_Retire (View_State) or else not Views.Empty (View_State, Views.Mask_Slot'First) then return False; end if;
            elsif Mesa_FFI.Release (Context, Src) /= 0 then return False;
            end if;
            Cache.Retired (Cached, Ticket, False);
            if Cache.Charged (Cached) /= Layout.Bytes then return False; end if;
            Memory.Release (Store, Backing, False, OK);
            if OK or else Memory.Address_Of (Store, Backing) /= Mask_Pixels then return False; end if;
            -- Foreign import retired; release the actual arena run before refund.
            Memory.Release (Store, Backing, True, OK);
            if not OK or else Memory.Address_Of (Store, Backing) /= System.Null_Address then return False; end if;
            Cache.Retired (Cached, Ticket, True);
            if Cache.Charged (Cached) /= 0 or else Cache.Current (Cached, Ticket) then return False; end if;
            end;
         end;
      end loop;
   end loop;
   if Shared then
      Views.Shutdown (View_State);
      if not Views.Can_Retire (View_State) or else not Views.Views_Clear (View_State) then return False; end if;
   else
      if Mesa_FFI.Release (Context, Dst) /= 0 or else Mesa_FFI.Release (Context, Color_Source) /= 0 then return False; end if;
      Mesa_FFI.Destroy (Context);
   end if;
   if Count /= 320 then return False; end if;
   Report (Boolean'Pos (Shared));
   return True;
end Native_Mask_Test;
