with Ada.Text_IO; with Compositor_Upload;
procedure Compositor_Upload_Tests is
   package G renames Compositor_Upload;
   P : G.Plan; OK, Expected : Boolean; Count : Natural := 0;
   Position, Pitch, Bytes : Natural;
   Caps : constant array (1 .. 4) of G.Byte_Count := (0, 31, 128, 384);
   First, Chunks : Natural;
begin
   for Kind in G.Pixel_Format loop
      Bytes := G.Pixel_Bytes (Kind);
      for W in G.Edge range 0 .. 9 loop
         for H in G.Edge range 0 .. 7 loop
            for X in G.Edge range 0 .. 2 loop
               for Y in G.Edge range 0 .. 2 loop
                  for Row in G.Edge range 0 .. 10 loop
                     for Offset in G.Byte_Count range 0 .. 5 loop
                        for Cap of Caps loop
                           Expected := W > 0 and H > 0 and X + W <= 8 and Y + H <= 6 and
                             (Row = 0 or Row >= W) and Offset mod 4 = 0;
                           Pitch := (if Row = 0 then W else Row);
                           if Expected then
                              Position := Offset;
                              for R in 1 .. H loop
                                 for C in 1 .. W loop
                                    for B in 1 .. Bytes loop
                                       Position := Position + 1;
                                       if Position > Cap then Expected := False; end if;
                                    end loop;
                                 end loop;
                                 Position := Position + (Pitch - W) * Bytes;
                              end loop;
                           end if;
                           G.Make (8, 6, Cap, (X, Y, W, H), Offset, Row, Kind, P, OK);
                           pragma Assert (OK = Expected);
                           if OK then pragma Assert (G.End_Byte (P) <= Cap); end if;
                           Count := Count + 1;
                        end loop;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      First := 0; Chunks := 0;
      while First < 2160 loop
         G.Row_Chunk (3840, 2160, G.Edge (First), 65536, Kind, P, OK);
         pragma Assert (OK and G.Area (P).Y = First and G.Area (P).Height > 0);
         First := First + G.Area (P).Height; Chunks := Chunks + 1;
      end loop;
      pragma Assert (First = 2160 and Chunks > 1);
      G.Row_Chunk (65535, 65535, 0, 1, Kind, P, OK); pragma Assert (not OK);
      G.Make (65535, 65535, G.Byte_Count'Last, (0, 0, 65535, 65535), 0, 0, Kind, P, OK);
      pragma Assert (not OK);
   end loop;
   Ada.Text_IO.Put_Line ("PASS upload geometry:" & Natural'Image (Count) & " enumerated address cases, 4K row tiling, undersized staging and extreme extents");
end Compositor_Upload_Tests;
