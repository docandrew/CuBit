with Ada.Text_IO;
with Interfaces;
with System;
with System.Storage_Elements;
with Desktop_Icons;
with Desktop_Window_Icons;
with Desktop_Icon_Pixels;
with Desktop_Icon_Mapping;
with Compositor_Upload;
procedure Icon_Mapping_Tests is
   package P renames Desktop_Icon_Pixels;
   package U renames Compositor_Upload;
   use System.Storage_Elements;
   use type Interfaces.Unsigned_32, P.Pixels;
   Sentinel : constant Interfaces.Unsigned_32 := 16#DEADBEEF#;
   --  Room for the largest icon edge with two pixels of row padding.
   Room : constant := Desktop_Icons.MAX_EDGE * (Desktop_Icons.MAX_EDGE + 2) + 4;
   Bytes : constant := 4 * Room;
   subtype Guard_Pixels is P.Pixels (0 .. Room + 1);
   procedure Check (Item : P.Asset) is
      Guarded : Guard_Pixels := (others => Sentinel);
      Plan : U.Plan;
      OK, Done : Boolean;
      N : constant Positive := P.Size (Item);
   begin
      U.Make (N, N, Bytes, (0, 0, N, N), 12, N + 2, U.BGRA8, Plan, OK);
      pragma Assert (OK);
      Desktop_Icon_Mapping.Copy_Chunk (Item, Guarded (1)'Address, Bytes, Plan, Done);
      pragma Assert (Done);
      for I in Guarded'Range loop
         if I >= 4 and then (I - 4) / (N + 2) < N and then (I - 4) mod (N + 2) < N then
            pragma Assert (Guarded (I) = P.Pixel (Item, (I - 4) mod (N + 2), (I - 4) / (N + 2)));
         else pragma Assert (Guarded (I) = Sentinel); end if;
      end loop;
      Guarded := (others => Sentinel);
      Desktop_Icon_Mapping.Copy_Chunk (Item, System.Null_Address, Bytes, Plan, Done);
      pragma Assert (not Done);
      Desktop_Icon_Mapping.Copy_Chunk (Item, Guarded (1)'Address + 1, Bytes, Plan, Done);
      pragma Assert (not Done);
      Desktop_Icon_Mapping.Copy_Chunk (Item, Guarded (1)'Address, 3, Plan, Done);
      pragma Assert (not Done);
      Desktop_Icon_Mapping.Copy_Chunk (Item, Guarded (1)'Address, Bytes - 1, Plan, Done);
      pragma Assert (not Done and Guarded = Guard_Pixels'(others => Sentinel));
   end Check;
begin
   for I in Desktop_Icons.Icon_ID loop
      for E in Desktop_Icons.Edge_Index loop Check ((P.Application, I, E)); end loop;
   end loop;
   for I in Desktop_Window_Icons.Icon_ID loop Check ((P.Window_Control, I)); end loop;
   Ada.Text_IO.Put_Line ("PASS icon mapping: direct mapped writes, all assets, external/padding guards, null/misaligned/short rejection");
end Icon_Mapping_Tests;
