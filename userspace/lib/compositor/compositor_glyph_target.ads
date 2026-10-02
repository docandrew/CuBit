with Compositor_Glyph_Renderer;
with Compositor_Formats;
with Mesa_Cache;
-- Narrow target-mapping bridge. Actual writable mapping authority and absence
-- of physical aliases remain the output owner's obligations. Geometry, mask
-- selection, leases and painting are delegated to checked SPARK code.
package Compositor_Glyph_Target with SPARK_Mode is
   package R renames Compositor_Glyph_Renderer;
   package F renames Compositor_Formats;
   use type F.Word;
   function Supported (Target : F.Image; Capacity : F.Byte_Count; Screen : R.P.G.Output) return Boolean is
     (F.Valid (Target, Capacity) and then Target.Writable = 1 and then
      Target.Width = F.Word (Screen.Width) and then Target.Height = F.Word (Screen.Height));
   procedure Paint (S : in out R.State; Views : in out Mesa_Cache.State;
                    Target : F.Image; Capacity : F.Byte_Count; Key : R.C.Key;
                    Screen : R.P.G.Output; Origin : R.P.G.Logical_Point;
                    Damage : R.P.G.Physical_Rectangle; Tint : F.Word; Success : out Boolean)
     with Pre => R.Valid (S), Post => R.Valid (S);
end Compositor_Glyph_Target;
