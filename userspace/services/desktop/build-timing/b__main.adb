pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__main.adb");
pragma Suppress (Overflow_Check);

package body ada_main is

   E011 : Short_Integer; pragma Import (Ada, E011, "cubit__appearance_E");
   E024 : Short_Integer; pragma Import (Ada, E024, "cubit__click_sequences_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "cubit__timing_histograms_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "ccl__bounded_stacks_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "ccl__checked_arithmetic_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "ccl__execution_budgets_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "ccl__handler_references_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "ccl__ownership_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "ccl__imports_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "ccl__secondary_arrays_E");
   E099 : Short_Integer; pragma Import (Ada, E099, "ccl__secondary_stacks_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "ccl__text_operations_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "ccl__types_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "ccl__ownership__bytecode_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "ccl__resource_policies_E");
   E087 : Short_Integer; pragma Import (Ada, E087, "ccl__types__correspondence_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "ccl__objects_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "ccl__objects__catalog_E");
   E107 : Short_Integer; pragma Import (Ada, E107, "ccl__objects__views_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "ccl__resources_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "ccl__vm_E");
   E091 : Short_Integer; pragma Import (Ada, E091, "ccl__host_values_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "ccl__catalog_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "ccl__objects__values_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "ccl__language_E");
   E069 : Short_Integer; pragma Import (Ada, E069, "ccl__declarations_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "cubit__fonts_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "cubit__messages_E");
   E013 : Short_Integer; pragma Import (Ada, E013, "cubit__audio_control_E");
   E026 : Short_Integer; pragma Import (Ada, E026, "cubit__clocks_E");
   E035 : Short_Integer; pragma Import (Ada, E035, "cubit__desktop_messages_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "cubit__graphics_metrics_io_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "cubit__input_E");
   E032 : Short_Integer; pragma Import (Ada, E032, "cubit__memory_grants_E");
   E028 : Short_Integer; pragma Import (Ada, E028, "cubit__config_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "cubit__monotonic_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "desktop_compositor_E");
   E128 : Short_Integer; pragma Import (Ada, E128, "desktop_launch_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "desktop_wallpaper_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "cubit__ui_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "cubit__ui__theme_data_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "cubit__ui__theme_ccl_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "desktop_settings_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);


   procedure adainit is
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");

      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      null;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;


      Cubit.Appearance'Elab_Spec;
      E011 := E011 + 1;
      Cubit.Click_Sequences'Elab_Spec;
      E024 := E024 + 1;
      Cubit.Timing_Histograms'Elab_Spec;
      E061 := E061 + 1;
      E083 := E083 + 1;
      E073 := E073 + 1;
      E105 := E105 + 1;
      E075 := E075 + 1;
      E079 := E079 + 1;
      E077 := E077 + 1;
      E109 := E109 + 1;
      E099 := E099 + 1;
      E101 := E101 + 1;
      E085 := E085 + 1;
      E097 := E097 + 1;
      E117 := E117 + 1;
      E087 := E087 + 1;
      CCL.OBJECTS'ELAB_BODY;
      E081 := E081 + 1;
      E115 := E115 + 1;
      CCL.OBJECTS.VIEWS'ELAB_SPEC;
      CCL.OBJECTS.VIEWS'ELAB_BODY;
      E107 := E107 + 1;
      CCL.RESOURCES'ELAB_SPEC;
      E093 := E093 + 1;
      CCL.VM'ELAB_SPEC;
      CCL.VM'ELAB_BODY;
      E095 := E095 + 1;
      E091 := E091 + 1;
      E113 := E113 + 1;
      E089 := E089 + 1;
      CCL.LANGUAGE'ELAB_BODY;
      E071 := E071 + 1;
      E069 := E069 + 1;
      E047 := E047 + 1;
      E015 := E015 + 1;
      E013 := E013 + 1;
      E026 := E026 + 1;
      E035 := E035 + 1;
      E051 := E051 + 1;
      Cubit.Input'Elab_Spec;
      E056 := E056 + 1;
      E032 := E032 + 1;
      E028 := E028 + 1;
      E058 := E058 + 1;
      E123 := E123 + 1;
      E128 := E128 + 1;
      E132 := E132 + 1;
      E063 := E063 + 1;
      E119 := E119 + 1;
      E066 := E066 + 1;
      E130 := E130 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_main");

   procedure main is
      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      adainit;
      Ada_Main_Program;
   end;

--  BEGIN Object file/option list
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/compositor_damage.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/compositor_elapsed.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/compositor_formats.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/compositor_presentation.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/cubit-appearance.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/cubit-display_geometry.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/cubit-display_layouts.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/cubit-display_arrangement.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/desktop_composition.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-bounded_stacks.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-checked_arithmetic.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-execution_budgets.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-handler_references.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-ownership.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-imports.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-secondary_arrays.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-secondary_stacks.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-text_operations.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-types.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-ownership-bytecode.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-resource_policies.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-types-correspondence.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-objects.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-objects-catalog.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-objects-views.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-resources.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-vm.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-host_values.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-catalog.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-objects-values.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-language.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/ccl-declarations.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/cubit-fonts.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/cubit-theme.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/desktop_compositor.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/desktop_cursors.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/desktop_icons.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/desktop_launch.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/desktop_timing_policy.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/desktop_wallpaper.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/desktop_window_icons.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/font8x16.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/cubit-ui.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/cubit-ui-theme_data.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/cubit-ui-theme_ccl.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/desktop_settings.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/presentation_test_policy.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-timing/main.o
   --   -L/home/doc/git/cubit/userspace/services/desktop/build-timing/
   --   -L/home/doc/git/cubit/userspace/services/desktop/build-timing/
   --   -L/home/doc/git/cubit/userspace/rust/build/font-native/
   --   -L/home/doc/git/cubit/userspace/runtime/adalib/
--  END Object file/option list   

end ada_main;
