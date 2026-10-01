pragma Warnings (Off);
pragma Ada_95;
pragma Restrictions (No_Exception_Handlers);
pragma Restrictions (No_Exception_Propagation);
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is


   GNAT_Version : constant String :=
                    "GNAT Version: 16.1.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   GNAT_Version_Address : constant System.Address := GNAT_Version'Address;
   pragma Export (C, GNAT_Version_Address, "__gnat_version_address");

   Ada_Main_Program_Name : constant String := "_ada_main" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure main;
   pragma Export (C, main, "main");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.img_int%s
   --  system.img_int%b
   --  system.img_lli%s
   --  system.img_lli%b
   --  system.machine_code%s
   --  system.parameters%s
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.secondary_stack%s
   --  system.secondary_stack%b
   --  system.unsigned_types%s
   --  system.img_llu%s
   --  system.img_llu%b
   --  ccl%s
   --  compositor_damage%s
   --  compositor_damage%b
   --  compositor_elapsed%s
   --  compositor_formats%s
   --  compositor_presentation%s
   --  compositor_presentation%b
   --  cubit%s
   --  cubit.appearance%s
   --  cubit.appearance%b
   --  cubit.click_sequences%s
   --  cubit.click_sequences%b
   --  cubit.config_protocol%s
   --  cubit.config_protocol%b
   --  cubit.display_geometry%s
   --  cubit.display_geometry%b
   --  cubit.display_layouts%s
   --  cubit.display_layouts%b
   --  cubit.display_arrangement%s
   --  cubit.display_arrangement%b
   --  cubit.grant_references%s
   --  cubit.desktop_protocol%s
   --  cubit.desktop_protocol%b
   --  cubit.display_protocol%s
   --  cubit.display_protocol%b
   --  cubit.graphics_metrics%s
   --  cubit.graphics_metrics%b
   --  cubit.timing_histograms%s
   --  cubit.timing_histograms%b
   --  desktop_composition%s
   --  desktop_composition%b
   --  ccl.bounded_stacks%s
   --  ccl.bounded_stacks%b
   --  ccl.checked_arithmetic%s
   --  ccl.checked_arithmetic%b
   --  ccl.execution_budgets%s
   --  ccl.execution_budgets%b
   --  ccl.handler_references%s
   --  ccl.handler_references%b
   --  ccl.ownership%s
   --  ccl.ownership%b
   --  ccl.imports%s
   --  ccl.imports%b
   --  ccl.secondary_arrays%s
   --  ccl.secondary_arrays%b
   --  ccl.secondary_stacks%s
   --  ccl.secondary_stacks%b
   --  ccl.text_operations%s
   --  ccl.text_operations%b
   --  ccl.types%s
   --  ccl.types%b
   --  ccl.ownership.bytecode%s
   --  ccl.ownership.bytecode%b
   --  ccl.resource_policies%s
   --  ccl.resource_policies%b
   --  ccl.types.correspondence%s
   --  ccl.types.correspondence%b
   --  ccl.objects%s
   --  ccl.objects%b
   --  ccl.objects.catalog%s
   --  ccl.objects.catalog%b
   --  ccl.objects.views%s
   --  ccl.objects.views%b
   --  ccl.resources%s
   --  ccl.resources%b
   --  ccl.vm%s
   --  ccl.vm%b
   --  ccl.host_values%s
   --  ccl.host_values%b
   --  ccl.catalog%s
   --  ccl.catalog%b
   --  ccl.objects.values%s
   --  ccl.objects.values%b
   --  ccl.language%s
   --  ccl.language%b
   --  ccl.declarations%s
   --  ccl.declarations%b
   --  cubit.fonts%s
   --  cubit.fonts%b
   --  cubit.messages%s
   --  cubit.messages%b
   --  cubit.audio_control%s
   --  cubit.audio_control%b
   --  cubit.clocks%s
   --  cubit.clocks%b
   --  cubit.desktop_messages%s
   --  cubit.desktop_messages%b
   --  cubit.graphics_metrics_io%s
   --  cubit.graphics_metrics_io%b
   --  cubit.input%s
   --  cubit.input%b
   --  cubit.memory_grants%s
   --  cubit.memory_grants%b
   --  cubit.config%s
   --  cubit.config%b
   --  cubit.monotonic%s
   --  cubit.monotonic%b
   --  cubit.theme%s
   --  desktop_compositor%s
   --  desktop_compositor%b
   --  desktop_cursors%s
   --  desktop_icons%s
   --  desktop_launch%s
   --  desktop_launch%b
   --  desktop_timing_policy%s
   --  desktop_wallpaper%s
   --  desktop_wallpaper%b
   --  desktop_window_icons%s
   --  font8x16%s
   --  cubit.ui%s
   --  cubit.ui%b
   --  cubit.ui.theme_data%s
   --  cubit.ui.theme_data%b
   --  cubit.ui.theme_ccl%s
   --  cubit.ui.theme_ccl%b
   --  desktop_settings%s
   --  desktop_settings%b
   --  presentation_test_policy%s
   --  main%b
   --  END ELABORATION ORDER

end ada_main;
