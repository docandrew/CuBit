pragma SPARK_Mode (On);
with Compositor_Glyph_Cache;
with Compositor_Glyph_Layout;
package Glyph_Cache_Model is new Compositor_Glyph_Cache
  (Slot_Count => Compositor_Glyph_Layout.Repertoire);
