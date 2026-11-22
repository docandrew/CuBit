package body Vulkan_Submission_FFI with SPARK_Mode => Off is
   function Native_Fill (Borrowed : System.Address; Width, Height, Left, Top, Right, Bottom, RGB : Code) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_submission_fill";
   procedure Fill (Borrowed : System.Address; Width, Height, Left, Top, Right, Bottom, RGB : Code; Result : out Code) is
   begin Result := Native_Fill (Borrowed, Width, Height, Left, Top, Right, Bottom, RGB); end Fill;
   function Native_Start (Borrowed : System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_submission_start";
   function Native_Seal (Borrowed : System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_submission_seal";
   function Native_Submit (Borrowed : System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_submission_submit";
   function Native_Poll (Borrowed : System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_submission_poll";
   function Native_Cancel (Borrowed : System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_submission_cancel";
   function Native_Matches (Borrowed, Draw : System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_submission_matches";
   function Native_Begin_Scene (Borrowed, Pass : System.Address; Width, Height : Code) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_submission_begin_scene";
   function Native_End_Scene (Borrowed : System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_submission_end_scene";
   procedure Begin_Scene (Borrowed, Pass : System.Address; Width, Height : Code; Result : out Code) is
   begin Result := Native_Begin_Scene (Borrowed, Pass, Width, Height); end Begin_Scene;
   procedure End_Scene (Borrowed : System.Address; Result : out Code) is
   begin Result := Native_End_Scene (Borrowed); end End_Scene;
   procedure Start (Borrowed : System.Address; Result : out Code) is
   begin Result := Native_Start (Borrowed); end Start;
   procedure Seal (Borrowed : System.Address; Result : out Code) is
   begin Result := Native_Seal (Borrowed); end Seal;
   procedure Submit (Borrowed : System.Address; Result : out Code) is
   begin Result := Native_Submit (Borrowed); end Submit;
   procedure Poll (Borrowed : System.Address; Result : out Code) is
   begin Result := Native_Poll (Borrowed); end Poll;
   procedure Cancel (Borrowed : System.Address; Result : out Code) is
   begin Result := Native_Cancel (Borrowed); end Cancel;
   procedure Matches (Borrowed, Draw : System.Address; Result : out Code) is
   begin Result := Native_Matches (Borrowed, Draw); end Matches;
   function Native_Import_Source (Description : System.Address; Draw : out System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_source_import";
   function Native_Release_Source (Draw : System.Address) return Code
     with Import, Convention => C, External_Name => "cubit_vulkan_source_release";
   procedure Import_Source (Description : System.Address; Draw : out System.Address; Result : out Code) is
   begin Result := Native_Import_Source (Description, Draw); end Import_Source;
   procedure Release_Source (Draw : System.Address; Result : out Code) is
   begin Result := Native_Release_Source (Draw); end Release_Source;
end Vulkan_Submission_FFI;
