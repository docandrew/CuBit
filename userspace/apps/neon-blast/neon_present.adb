------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;

package body Neon_Present is
   package Res renames Neon_Resolution;

   --  Buffer Into holds at least Cells items (grown, never shrunk).
   generic
      type Item is private;
      type Buffer is array (Natural range <>) of Item;
      type Buffer_Access is access Buffer;
   procedure Hold_Buffer (Into : in out Buffer_Access; Cells : Positive);
   procedure Hold_Buffer (Into : in out Buffer_Access; Cells : Positive) is
      procedure Free is new Ada.Unchecked_Deallocation (Buffer, Buffer_Access);
   begin
      if Into = null or else Into'Length < Cells then
         Free (Into);
         Into := new Buffer (0 .. Cells - 1);
      end if;
   end Hold_Buffer;
   procedure Hold is new Hold_Buffer (Unsigned_32, Neon_Upscale.Pixel_Array, Pixel_Access);
   procedure Hold is new Hold_Buffer (Natural, Neon_Upscale.Index_Array, Index_Access);
   procedure Hold is new Hold_Buffer (Neon_Upscale.Weight, Neon_Upscale.Weight_Array, Weight_Access);

   function Elapsed (Since : Unsigned_64) return Res.Microseconds is
      Now : constant Unsigned_64 := Now_Us;
   begin
      if Now <= Since then
         return 0;
      elsif Now - Since >= Unsigned_64 (Res.Maximum_Measure_Us) then
         return Res.Maximum_Measure_Us;
      end if;
      return Res.Microseconds (Now - Since);
   end Elapsed;

   procedure Paint
     (P : in out Presenter; R : in out Neon_Soft.Renderer; F : in out Neon_Scene.Frame;
      G : Neon_Game.Game; Target : System.Address; Width, Height, Pitch_Pixels : Positive)
   is
      At_Level : constant Res.Level := Res.Current (P.Controller);
      Inner_Width : constant Positive := Res.Internal (Width, At_Level);
      Inner_Height : constant Positive := Res.Internal (Height, At_Level);
      Began : constant Unsigned_64 := Now_Us;
      Render_Us, Scale_Us : Res.Microseconds := 0;
   begin
      if Inner_Width = Width and then Inner_Height = Height then
         Lock_Game;
         Neon_Scene.Build (F, G, Width, Height);
         Unlock_Game;
         Neon_Soft.Render (R, F, Target, Width, Height, Pitch_Pixels);
         Render_Us := Elapsed (Began);
      else
         Hold (P.Frame, Inner_Width * Inner_Height);
         Hold (P.Rows, 2 * Width);
         Hold (P.Left, Width);
         Hold (P.Right, Width);
         Hold (P.Weights, Width);
         Lock_Game;
         Neon_Scene.Build (F, G, Inner_Width, Inner_Height);
         Unlock_Game;
         declare
            Scanlines : constant Boolean := F.Scanlines;
            Window : Neon_Upscale.Pixel_Array (0 .. Pitch_Pixels * Height - 1)
              with Import, Address => Target;
            Scaling : Unsigned_64;
         begin
            --  Scanlines at the window's resolution, not scaled from the
            --  internal one.
            F.Scanlines := False;
            Neon_Soft.Render (R, F, P.Frame.all'Address, Inner_Width, Inner_Height, Inner_Width);
            F.Scanlines := Scanlines;
            Render_Us := Elapsed (Began);
            Scaling := Now_Us;
            Neon_Upscale.Scale
              (P.Frame.all, Inner_Width, Inner_Height, Window, Width, Height, Pitch_Pixels,
               P.Rows.all, P.Left.all, P.Right.all, P.Weights.all, Scanlines);
            Scale_Us := Elapsed (Scaling);
         end;
      end if;
      P.Latest :=
        (Output_Width => Width, Output_Height => Height,
         Internal_Width => Inner_Width, Internal_Height => Inner_Height,
         Scale => Res.Scale_Of (At_Level), Render_Us => Render_Us, Scale_Us => Scale_Us);
      Res.Note_Frame (P.Controller, Width, Height, Render_Us, Scale_Us);
   end Paint;

end Neon_Present;
