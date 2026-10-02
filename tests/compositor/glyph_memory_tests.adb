with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;
with Compositor_Glyph_Memory;
with Compositor_Glyph_Layout;
with Compositor_Glyph_FFI;
procedure Glyph_Memory_Tests is
   package M renames Compositor_Glyph_Memory;
   package A renames M.Arena;
   package L renames Compositor_Glyph_Layout;
   package Byte_Access is new System.Address_To_Access_Conversions (Unsigned_8);
   use type System.Address, A.Token;
   Store : M.State;
   T, Previous, Neighbor : A.Token;
   P, Guard : System.Address;
   Capacity, Guard_Capacity, Advance : Natural;
   OK : Boolean;
   function At_Byte (Base : System.Address; I : Natural) return Byte_Access.Object_Pointer is
     (Byte_Access.To_Pointer (Base + Storage_Offset (I)));
begin
   pragma Assert (M.Address_Of (Store, A.No_Token) = System.Null_Address);
   M.Reserve (Store, L.Maximum_Bytes, Neighbor, Guard, Guard_Capacity);
   for I in 0 .. Guard_Capacity - 1 loop At_Byte (Guard, I).all := 16#7B#; end loop;
   Previous := A.No_Token;
   for Face in 0 .. 1 loop
      for N in 1 .. 16 loop
         for D in 1 .. 16 loop
            declare Layout : constant L.Layout := L.Plan ((L.G.Scale_Component (N), L.G.Scale_Component (D)));
            begin
               M.Reserve (Store, Layout.Bytes, T, P, Capacity);
               pragma Assert (T /= A.No_Token and P /= System.Null_Address and
                 Capacity >= Layout.Bytes and Capacity - Layout.Bytes < 128);
               pragma Assert (To_Integer (P) mod 128 = 0);
               if Previous /= A.No_Token then
                  M.Release (Store, Previous, True, OK);
                  pragma Assert (not OK and M.Address_Of (Store, Previous) = System.Null_Address and M.Address_Of (Store, T) = P);
               end if;
               for I in 0 .. Capacity - 1 loop At_Byte (P, I).all := 16#A5#; end loop;
               Compositor_Glyph_FFI.Rasterize
                 (Unsigned_32 (Face), Character'Pos ('W'), Layout, P, Unsigned_64 (Capacity), Advance, OK);
               pragma Assert (OK and Advance in 1 .. Layout.Width);
               for Y in 0 .. Layout.Height - 1 loop
                  for X in Layout.Width .. Layout.Pitch - 1 loop
                     pragma Assert (At_Byte (P, Y * Layout.Pitch + X).all = 16#A5#);
                  end loop;
               end loop;
               for I in Layout.Bytes .. Capacity - 1 loop pragma Assert (At_Byte (P, I).all = 16#A5#); end loop;
               for I in 0 .. Guard_Capacity - 1 loop pragma Assert (At_Byte (Guard, I).all = 16#7B#); end loop;
               M.Release (Store, T, False, OK);
               pragma Assert (not OK and M.Address_Of (Store, T) = P);
               M.Release (Store, T, True, OK);
               pragma Assert (OK and M.Address_Of (Store, T) = System.Null_Address);
               Previous := T;
            end;
         end loop;
      end loop;
   end loop;
   M.Release (Store, Neighbor, True, OK); pragma Assert (OK);
   declare Held : array (1 .. 3) of A.Token;
   begin
      for I in Held'Range loop
         M.Reserve (Store, L.Maximum_Bytes, Held (I), P, Capacity); pragma Assert (P /= System.Null_Address);
      end loop;
      M.Reserve (Store, L.Maximum_Bytes, T, P, Capacity);
      pragma Assert (T = A.No_Token and P = System.Null_Address and Capacity = 0);
      for Item of Held loop M.Release (Store, Item, True, OK); pragma Assert (OK); end loop;
   end;
   Put_Line ("GLYPH-MEMORY: PASS 512 direct arena rasters, neighbor/padding guards, stale and withheld release, fixed backing exhaustion");
   Put_Line ("Backing bytes=" & Natural'Image (A.Backing_Bytes) & "; total object bytes=" & Natural'Image (M.State'Size / 8));
end Glyph_Memory_Tests;
