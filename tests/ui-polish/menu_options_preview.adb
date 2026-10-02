with Ada.Command_Line;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
procedure Menu_Options_Preview is
   package IO renames Ada.Streams.Stream_IO;
   Width : constant := 1080;
   Height : constant := 600;
   Pixels : aliased array (0 .. Width * Height - 1) of Color := [others => 0];
   C : constant Canvas := (addr => Pixels'Address, width => Width, height => Height,
     pitch => Width * 4, others => <>);
   F : IO.File_Type;
   RGB : Stream_Element_Array (1 .. Width * Height * 3);
   At_Byte : Stream_Element_Offset := 1;
   procedure Bar (X, Y, Option, State : Natural; T : Theme) is
      R : constant Rect := (X, Y, 320, 30);
      Cursor : Natural := X + 4;
      Anchor : Natural := X;
   begin
      Draw_Menu_Bar (C, R, T);
      for I in 1 .. 3 loop
         declare
            Caption : constant String := (case I is when 1 => "File", when 2 => "Edit", when others => "View");
            W : constant Natural := UI_Text_Width (Caption) + 20;
         begin
            if I = 2 then Anchor := Cursor; end if;
            Draw_Menu_Title (C, (Cursor, Y, W, 30), T,
              I = 2 and State = 1, I = 2 and State = 2, Caption);
            if Option = 2 and I < 3 then
               Fill_Rect (C, (Cursor + W - 1, Y + 7, 1, 16), T.shadow);
               Fill_Rect (C, (Cursor + W, Y + 7, 1, 16), T.highlight);
            end if;
            Cursor := Cursor + W;
         end;
      end loop;
      if Option = 3 then
         Stroke_Rect (C, R, T.highlight, T.shadow);
      end if;
      if State = 2 then
         Fill_Rect (C, (Anchor, Y + 30, 196, 62), T.panel);
         Stroke_Rect (C, (Anchor, Y + 30, 196, 62), T.highlight, T.shadow);
         Draw_UI_Text (C, Anchor + 12, Y + 37, "Select address", T.text, T.panel);
         Fill_Rect (C, (Anchor + 3, Y + 60, 190, 27), T.selection);
         Draw_UI_Text (C, Anchor + 12, Y + 64, "Settings...", T.selectionText, T.selection);
      end if;
   end Bar;
begin
   for Dark in Boolean loop
      declare
         Y : constant Natural := (if Dark then 300 else 0);
         T : constant Theme := (if Dark then CuBit_Alloy_Dark else CuBit_Alloy);
      begin
         Fill_Rect (C, (0, Y, Width, 300), T.panel);
         for Option in 1 .. 3 loop
            declare
               X : constant Natural := (Option - 1) * 360 + 20;
               Caption : constant String := (case Option is
                 when 1 => "1. Classic flat",
                 when 2 => "2. Subtle separators",
                 when others => "3. Framed menu strip");
            begin
               Draw_UI_Text (C, X, Y + 15, Caption, T.text, T.panel);
               for State in 0 .. 2 loop
                  Draw_UI_Text (C, X, Y + 42 + State * 60,
                    (case State is when 0 => "Idle", when 1 => "Hover", when others => "Open"), T.muted, T.panel);
                  Bar (X, Y + 60 + State * 60, Option, State, T);
               end loop;
            end;
         end loop;
      end;
   end loop;
   for P of Pixels loop
      RGB (At_Byte) := Stream_Element (Shift_Right (P, 16) and 255);
      RGB (At_Byte + 1) := Stream_Element (Shift_Right (P, 8) and 255);
      RGB (At_Byte + 2) := Stream_Element (P and 255);
      At_Byte := At_Byte + 3;
   end loop;
   IO.Create (F, IO.Out_File, Ada.Command_Line.Argument (1));
   String'Write (IO.Stream (F), "P6" & ASCII.LF & "1080 600" & ASCII.LF & "255" & ASCII.LF);
   IO.Write (F, RGB); IO.Close (F);
end Menu_Options_Preview;
