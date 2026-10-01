pragma SPARK_Mode (On);
with System;
with Compositor_Cache;
with Mesa_Binding;
package Mesa_Cache is new Compositor_Cache
  (Mesa_Binding.Context, System.Address, System.Null_Address,
   Mesa_Binding.Start, Mesa_Binding.Import_View, Mesa_Binding.Render_View,
   Mesa_Binding.Release_View, Mesa_Binding.Stop);
