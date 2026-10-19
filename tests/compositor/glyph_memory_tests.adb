with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;
with Compositor_Glyph_Memory;
with Compositor_Glyph_Layout;
with Compositor_Glyph_FFI;
--  Glyph mask storage from CuAlloc (hosted: CuAlloc_Host over Linux): every
--  density's stacked phase mask rasterizes into its block with row padding
--  and a neighbouring block untouched; release returns null; an exhausted
--  heap answers null instead of stopping the owner.
procedure Glyph_Memory_Tests is
   package M renames Compositor_Glyph_Memory;
   package L renames Compositor_Glyph_Layout;
   package Byte_Access is new System.Address_To_Access_Conversions (Unsigned_8);
   use type System.Address;
   procedure Set_Quota (Bytes : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_test_set_quota";
   function Committed return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_test_committed";
   P, Guard : System.Address;
   Capacity, Guard_Capacity, Advance : Natural;
   OK : Boolean;
   Cases : Natural := 0;
   function At_Byte (Base : System.Address; I : Natural) return Byte_Access.Object_Pointer is
     (Byte_Access.To_Pointer (Base + Storage_Offset (I)));
begin
   M.Reserve (L.Maximum_Bytes, Guard, Guard_Capacity);
   pragma Assert (Guard /= System.Null_Address and Guard_Capacity = L.Maximum_Bytes);
   for I in 0 .. Guard_Capacity - 1 loop At_Byte (Guard, I).all := 16#7B#; end loop;
   for Face in 0 .. 1 loop
      for N in 1 .. 16 loop
         for D in 1 .. 16 loop
            declare Layout : constant L.Layout := L.Plan ((L.G.Scale_Component (N), L.G.Scale_Component (D)));
            begin
               M.Reserve (Layout.Bytes, P, Capacity);
               pragma Assert (P /= System.Null_Address and Capacity = Layout.Bytes);
               pragma Assert (To_Integer (P) mod M.Alignment = 0);
               for I in 0 .. Capacity - 1 loop At_Byte (P, I).all := 16#A5#; end loop;
               Compositor_Glyph_FFI.Rasterize
                 (Unsigned_32 (Face), Character'Pos ('W'), Layout, P, Unsigned_64 (Capacity), Advance, OK);
               pragma Assert (OK and Advance in 1 .. Layout.Width);
               for Y in 0 .. L.Strip_Height (Layout) - 1 loop
                  for X in Layout.Width .. Layout.Pitch - 1 loop
                     pragma Assert (At_Byte (P, Y * Layout.Pitch + X).all = 16#A5#);
                  end loop;
               end loop;
               --  Every phase holds ink.
               for Phase in L.Phase loop
                  declare Ink : Natural := 0;
                  begin
                     for I in 0 .. L.Phase_Bytes (Layout) - 1 loop
                        if At_Byte (P, Phase * L.Phase_Bytes (Layout) + I).all /= 16#A5# then
                           Ink := Ink + Natural (At_Byte (P, Phase * L.Phase_Bytes (Layout) + I).all);
                        end if;
                     end loop;
                     pragma Assert (Ink > 0);
                  end;
               end loop;
               M.Release (P);
               pragma Assert (P = System.Null_Address);
               Cases := Cases + 1;
            end;
         end loop;
      end loop;
   end loop;
   for I in 0 .. Guard_Capacity - 1 loop pragma Assert (At_Byte (Guard, I).all = 16#7B#); end loop;
   M.Release (Guard);
   M.Release (Guard);  --  null: nothing
   --  Nothing more may be committed; a block larger than any freed one.
   Set_Quota (Committed);
   M.Reserve (64 * L.Maximum_Bytes, P, Capacity);
   pragma Assert (P = System.Null_Address and Capacity = 0);
   Set_Quota (Unsigned_64'Last);
   Put_Line ("GLYPH-MEMORY: PASS" & Cases'Image & " CuAlloc phase-mask rasters, padding and neighbour guards, null on exhaustion");
end Glyph_Memory_Tests;
