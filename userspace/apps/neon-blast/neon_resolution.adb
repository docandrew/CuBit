------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Neon_Resolution with SPARK_Mode is

   Whole : constant := 100;
   --  The smoothing: a new measurement weighs one part in Smoothing_Parts.
   Smoothing_Parts : constant := 4;

   --  Microsecond-pixel products: at most a second times 2 ** 28 pixels.
   type Work is range 0 .. 2**62;
   subtype Pixel_Work is Work range 1 .. Work (Maximum_Extent) * Work (Maximum_Extent);

   function Internal (Output : Extent; At_Level : Level) return Extent is
     (Positive'Max (1, Output * Scale_Of (At_Level) / Whole));

   function Floor_For (Output_Height : Extent) return Level is
   begin
      for L in reverse Level loop
         if L = Native or else Internal (Output_Height, L) >= Minimum_Lines then
            return L;
         end if;
      end loop;
      return Native;
   end Floor_For;

   function Pixels (Width, Height : Extent; At_Level : Level) return Pixel_Work is
     (Work (Internal (Width, At_Level)) * Work (Internal (Height, At_Level)));

   function Clamped (Value : Work) return Microseconds is
     (if Value >= Work (Maximum_Measure_Us) then Maximum_Measure_Us else Microseconds (Value));

   --  The render time at To, from the smoothed one at From (same window).
   function Render_At (C : Controller; Width, Height : Extent; From, To : Level) return Microseconds is
     (Clamped (Work (C.Render_Avg) * Pixels (Width, Height, To) / Pixels (Width, Height, From)));

   --  Render plus scale at To.
   function Predicted (C : Controller; Width, Height : Extent; From, To : Level) return Natural is
     (Render_At (C, Width, Height, From, To) + (if To = Native then 0 else C.Scale_Avg));

   function Smoothed (Average, Sample : Microseconds) return Microseconds is
     ((Average * (Smoothing_Parts - 1) + Sample) / Smoothing_Parts);

   procedure Count (N : in out Change_Count) is
   begin
      if N < Change_Count'Last then
         N := N + 1;
      end if;
   end Count;

   --  Move to To; the smoothed render time follows by pixel count.
   procedure Change (C : in out Controller; Width, Height : Extent; To : Level) is
   begin
      C.Render_Avg := Render_At (C, Width, Height, C.At_Level, To);
      if To < C.At_Level then
         Count (C.Up);
      else
         Count (C.Down);
      end if;
      C.At_Level := To;
      C.Over := 0;
      C.Room := 0;
      C.Settle := Settle_Frames;
   end Change;

   procedure Note_Frame
     (C : in out Controller; Output_Width, Output_Height : Extent; Render_Us, Scale_Us : Microseconds)
   is
      Floor : constant Level := Floor_For (Output_Height);
      Total : Natural;
      Target : Level;
   begin
      --  A resized window: the render time scales with its pixels.
      if Output_Width /= C.Last_Width or else Output_Height /= C.Last_Height then
         if C.Measured then
            C.Render_Avg :=
              Clamped (Work (C.Render_Avg) * Pixels (Output_Width, Output_Height, C.At_Level)
                       / Pixels (C.Last_Width, C.Last_Height, C.At_Level));
         end if;
         C.Last_Width := Output_Width;
         C.Last_Height := Output_Height;
         C.Over := 0;
         C.Room := 0;
      end if;
      if C.At_Level > Floor then
         Change (C, Output_Width, Output_Height, Floor);
      end if;
      if C.Settle > 0 then
         C.Settle := C.Settle - 1;
         return;
      end if;

      if C.Measured then
         C.Render_Avg := Smoothed (C.Render_Avg, Render_Us);
      else
         C.Render_Avg := Render_Us;
         C.Measured := True;
      end if;
      if C.At_Level /= Native then
         if C.Scale_Known then
            C.Scale_Avg := Smoothed (C.Scale_Avg, Scale_Us);
         else
            C.Scale_Avg := Scale_Us;
            C.Scale_Known := True;
         end if;
      end if;

      Total := C.Render_Avg + (if C.At_Level = Native then 0 else C.Scale_Avg);
      if Total > Budget_Us then
         C.Room := 0;
         --  Over only while each frame itself is over too: one slow frame
         --  lifts the average for a few frames but does not drop.
         if Render_Us + Scale_Us <= Budget_Us then
            C.Over := 0;
         elsif C.Over < Drop_After then
            C.Over := C.Over + 1;
         end if;
         if C.Over = Drop_After and then C.At_Level < Floor then
            --  The finest level predicted to fit, or the floor.
            Target := Floor;
            for L in C.At_Level + 1 .. Floor loop
               if Predicted (C, Output_Width, Output_Height, C.At_Level, L) <= Budget_Us then
                  Target := L;
                  exit;
               end if;
               pragma Loop_Invariant (C.At_Level < Floor and then Target = Floor);
            end loop;
            Change (C, Output_Width, Output_Height, Target);
         end if;
      else
         C.Over := 0;
         if C.At_Level > Native
           and then Predicted (C, Output_Width, Output_Height, C.At_Level, C.At_Level - 1) <= Raise_Below_Us
         then
            if C.Room < Raise_After then
               C.Room := C.Room + 1;
            end if;
            if C.Room = Raise_After then
               Change (C, Output_Width, Output_Height, C.At_Level - 1);
            end if;
         else
            C.Room := 0;
         end if;
      end if;
   end Note_Frame;

end Neon_Resolution;
