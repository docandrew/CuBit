with Ada.Environment_Variables;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with CuBit.UI.Theme_CCL;

package body Text_Host_Support is
   BYTES_PER_PIXEL : constant := 4;
   CHANNEL_BITS : constant := 8;
   CHANNEL_MASK : constant := 16#FF#;
   PUMP_BUDGET : constant := 100_000;
   START : constant Time := Clock;

   function New_Surface (Width, Height : Positive) return Surface is
     ((Width, Height, new Pixels'(0 .. Width * Height - 1 => 0)));

   function Canvas (S : Surface) return CuBit.UI.Canvas is
     ((addr => S.Image.all'Address, width => S.Width, height => S.Height, pitch => S.Width * BYTES_PER_PIXEL,
       others => <>));

   function Canvas (S : Surface; Damage : CuBit.UI.Rect) return CuBit.UI.Canvas is
     (CuBit.UI.With_Clip (Canvas (S), Damage));

   function Pixel (S : Surface; X, Y : Natural) return Unsigned_32 is (S.Image (Y * S.Width + X));

   procedure Save_PPM (S : Surface; Path : String) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      File : File_Type;
      Header : constant String :=
        "P6" & ASCII.LF & Natural'Image (S.Width) & Natural'Image (S.Height) & ASCII.LF & "255" & ASCII.LF;
      Row : Stream_Element_Array (1 .. Stream_Element_Offset (S.Width * 3));
   begin
      Create (File, Out_File, Path);
      for C of Header loop
         Stream_Element_Array'Write (Stream (File), [1 => Stream_Element (Character'Pos (C))]);
      end loop;
      for Y in 0 .. S.Height - 1 loop
         for X in 0 .. S.Width - 1 loop
            declare
               P : constant Unsigned_32 := Pixel (S, X, Y);
               Base : constant Stream_Element_Offset := Stream_Element_Offset (X * 3);
            begin
               Row (Base + 1) := Stream_Element (Shift_Right (P, 2 * CHANNEL_BITS) and CHANNEL_MASK);
               Row (Base + 2) := Stream_Element (Shift_Right (P, CHANNEL_BITS) and CHANNEL_MASK);
               Row (Base + 3) := Stream_Element (P and CHANNEL_MASK);
            end;
         end loop;
         Stream_Element_Array'Write (Stream (File), Row);
      end loop;
      Close (File);
   end Save_PPM;

   function Now_Us return Unsigned_64 is (Unsigned_64 (To_Duration (Clock - START) * 1_000_000));

   procedure Install_Themes (Scheme : CuBit.Appearance.Color_Scheme) is
      use type CuBit.Appearance.Color_Scheme;
   begin
      for Each in CuBit.Appearance.Color_Scheme loop
         declare
            Name : constant String :=
              (if Each = CuBit.Appearance.Alloy_Light then "TEXT_THEME_LIGHT" else "TEXT_THEME_DARK");
            Fallback : constant CuBit.UI.Theme := CuBit.UI.Default_Palette (Each);
            Loaded : CuBit.UI.Theme_CCL.Result;
         begin
            CuBit.UI.Install_Palette (Each, Fallback);
            if Ada.Environment_Variables.Exists (Name) then
               declare
                  Source : constant String := Ada.Environment_Variables.Value (Name);
               begin
                  if Source'Length in 1 .. CuBit.UI.Theme_CCL.Maximum_Source then
                     CuBit.UI.Theme_CCL.Load (Source, Fallback, Loaded);
                     if Loaded.Success then
                        CuBit.UI.Install_Palette (Each, Loaded.Value);
                     else
                        Ada.Text_IO.Put_Line ("text: invalid " & Name & "; using the built-in palette");
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;
      CuBit.UI.Set_Color_Scheme (Scheme);
   end Install_Themes;

   procedure Frame (Shell : in out Text_Shell.Shell; Target : Surface) is
      Damage : CuBit.UI.Rect;
   begin
      Text_Shell.Set_Clock (Shell, Now_Us);
      Text_Shell.Set_Bounds (Shell, Bounds (Target));
      Text_Shell.Take_Damage (Shell, Damage);
      if not CuBit.UI.Is_Empty (Damage) then
         Text_Shell.Render (Shell, Canvas (Target, Damage), Bounds (Target));
      end if;
   end Frame;

   function Settle (Shell : in out Text_Shell.Shell; Target : Surface; Timeout_Ms : Positive := 60_000) return Boolean
   is
      Deadline : constant Time := Clock + Milliseconds (Timeout_Ms);
      Busy, Changed : Boolean;
   begin
      loop
         Text_Shell.Set_Clock (Shell, Now_Us);
         Text_Shell.Pump (Shell, PUMP_BUDGET, Busy, Changed);
         Frame (Shell, Target);
         exit when not Busy;
         if Clock > Deadline then
            return False;
         end if;
      end loop;
      return True;
   end Settle;
end Text_Host_Support;
