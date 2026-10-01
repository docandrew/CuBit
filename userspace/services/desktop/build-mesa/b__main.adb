pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__main.adb");
pragma Suppress (Overflow_Check);

package body ada_main is

   E010 : Short_Integer; pragma Import (Ada, E010, "cubit__appearance_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "cubit__click_sequences_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "ccl__bounded_stacks_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ccl__checked_arithmetic_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "ccl__execution_budgets_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "ccl__handler_references_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "ccl__ownership_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "ccl__imports_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "ccl__secondary_arrays_E");
   E094 : Short_Integer; pragma Import (Ada, E094, "ccl__secondary_stacks_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "ccl__text_operations_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "ccl__types_E");
   E092 : Short_Integer; pragma Import (Ada, E092, "ccl__ownership__bytecode_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "ccl__resource_policies_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "ccl__types__correspondence_E");
   E076 : Short_Integer; pragma Import (Ada, E076, "ccl__objects_E");
   E110 : Short_Integer; pragma Import (Ada, E110, "ccl__objects__catalog_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "ccl__objects__views_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "ccl__resources_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "ccl__vm_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "ccl__host_values_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "ccl__catalog_E");
   E084 : Short_Integer; pragma Import (Ada, E084, "ccl__objects__values_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "ccl__language_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "ccl__declarations_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "compositor_cache_E");
   E046 : Short_Integer; pragma Import (Ada, E046, "cubit__fonts_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "cubit__messages_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "cubit__audio_control_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "cubit__clocks_E");
   E034 : Short_Integer; pragma Import (Ada, E034, "cubit__desktop_messages_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "cubit__graphics_metrics_io_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "cubit__input_E");
   E031 : Short_Integer; pragma Import (Ada, E031, "cubit__memory_grants_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "cubit__config_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "desktop_launch_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "desktop_wallpaper_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "cubit__ui_E");
   E114 : Short_Integer; pragma Import (Ada, E114, "cubit__ui__theme_data_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "cubit__ui__theme_ccl_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "desktop_settings_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "mesa_binding_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "desktop_compositor_E");

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
      E010 := E010 + 1;
      Cubit.Click_Sequences'Elab_Spec;
      E023 := E023 + 1;
      E078 := E078 + 1;
      E068 := E068 + 1;
      E100 := E100 + 1;
      E070 := E070 + 1;
      E074 := E074 + 1;
      E072 := E072 + 1;
      E104 := E104 + 1;
      E094 := E094 + 1;
      E096 := E096 + 1;
      E080 := E080 + 1;
      E092 := E092 + 1;
      E112 := E112 + 1;
      E082 := E082 + 1;
      CCL.OBJECTS'ELAB_BODY;
      E076 := E076 + 1;
      E110 := E110 + 1;
      CCL.OBJECTS.VIEWS'ELAB_SPEC;
      CCL.OBJECTS.VIEWS'ELAB_BODY;
      E102 := E102 + 1;
      CCL.RESOURCES'ELAB_SPEC;
      E088 := E088 + 1;
      CCL.VM'ELAB_SPEC;
      CCL.VM'ELAB_BODY;
      E090 := E090 + 1;
      E086 := E086 + 1;
      E108 := E108 + 1;
      E084 := E084 + 1;
      CCL.LANGUAGE'ELAB_BODY;
      E066 := E066 + 1;
      E064 := E064 + 1;
      E122 := E122 + 1;
      E046 := E046 + 1;
      E014 := E014 + 1;
      E012 := E012 + 1;
      E025 := E025 + 1;
      E034 := E034 + 1;
      E050 := E050 + 1;
      Cubit.Input'Elab_Spec;
      E055 := E055 + 1;
      E031 := E031 + 1;
      E027 := E027 + 1;
      E132 := E132 + 1;
      E136 := E136 + 1;
      E058 := E058 + 1;
      E114 := E114 + 1;
      E061 := E061 + 1;
      E134 := E134 + 1;
      E127 := E127 + 1;
      Desktop_Compositor'Elab_Body;
      E118 := E118 + 1;
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
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/compositor_damage.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/compositor_formats.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/compositor_policy.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/compositor_presentation.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/cubit-appearance.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/cubit-display_geometry.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/cubit-display_layouts.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/cubit-display_arrangement.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/desktop_composition.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-bounded_stacks.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-checked_arithmetic.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-execution_budgets.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-handler_references.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-ownership.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-imports.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-secondary_arrays.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-secondary_stacks.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-text_operations.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-types.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-ownership-bytecode.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-resource_policies.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-types-correspondence.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-objects.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-objects-catalog.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-objects-views.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-resources.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-vm.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-host_values.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-catalog.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-objects-values.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-language.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/ccl-declarations.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/compositor_cache.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/cubit-fonts.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/cubit-theme.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/desktop_cursors.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/desktop_icons.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/desktop_launch.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/desktop_wallpaper.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/desktop_window_icons.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/font8x16.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/cubit-ui.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/cubit-ui-theme_data.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/cubit-ui-theme_ccl.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/desktop_settings.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/mesa_ffi.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/mesa_binding.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/mesa_cache.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/desktop_compositor.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/presentation_test_policy.o
   --   /home/doc/git/cubit/userspace/services/desktop/build-mesa/main.o
   --   -L/home/doc/git/cubit/userspace/services/desktop/build-mesa/
   --   -L/home/doc/git/cubit/userspace/services/desktop/build-mesa/
   --   -L/home/doc/git/cubit/userspace/rust/build/font-native/
   --   -L/home/doc/git/cubit/userspace/runtime/adalib/
--  END Object file/option list   

end ada_main;
