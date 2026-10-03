with Interfaces.C;
with Interfaces;
with System;
package Vulkan_Submission_Test_Bridge is
   type Input is record
      W, H, N, D, Rotation, X, Y, L, T, R, B, DL, DT, DR, DB, Over, Mask : Interfaces.C.int;
      Tint : Interfaces.Unsigned_32;
   end record with Convention => C;
   procedure Open (Context : System.Address) with Export, Convention => C, External_Name => "test_submission_open";
   function Import_Source (Description, Expected_Context : System.Address) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_import_source";
   function Import_Mask (Description, Expected_Context : System.Address) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_import_mask";
   function Release_Mask return System.Address
     with Export, Convention => C, External_Name => "test_submission_release_mask";
   function Register_Source (Context : System.Address) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_register_source";
   function Release_Source return System.Address
     with Export, Convention => C, External_Name => "test_submission_release_source";
   function Initialize_Targets (Description : System.Address) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_initialize_targets";
   function Close_Targets return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_close_targets";
   procedure Set_Targets (A, B, C : System.Address)
     with Export, Convention => C, External_Name => "test_submission_set_targets";
   function Target_Index return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_target_index";
   procedure Display_Tick (Latch_Now : Interfaces.C.int)
     with Export, Convention => C, External_Name => "test_submission_display_tick";
   procedure Finish_Display with Export, Convention => C, External_Name => "test_submission_finish_display";
   procedure Damage_Region (Left, Top, Right, Bottom : Interfaces.C.int)
     with Export, Convention => C, External_Name => "test_submission_damage";
   function Repaint_Count return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_repaint_count";
   procedure Repaint_Box (Index : Interfaces.C.int; Left, Top, Right, Bottom : out Interfaces.C.int)
     with Export, Convention => C, External_Name => "test_submission_repaint_box";
   function Start return Interfaces.C.int with Export, Convention => C, External_Name => "test_submission_start";
   function Begin_Scene (Pass : System.Address; Width, Height : Interfaces.Unsigned_32)
     return Interfaces.C.int with Export, Convention => C, External_Name => "test_submission_begin_scene";
   function End_Scene return Interfaces.C.int with Export, Convention => C, External_Name => "test_submission_end_scene";
   function Cancel return Interfaces.C.int with Export, Convention => C, External_Name => "test_submission_cancel";
   function Finish return Interfaces.C.int with Export, Convention => C, External_Name => "test_submission_finish";
   function Poll return Interfaces.C.int with Export, Convention => C, External_Name => "test_submission_poll";
   function Budget return Interfaces.C.int with Export, Convention => C, External_Name => "test_submission_budget";
   function Releasable return Interfaces.C.int with Export, Convention => C, External_Name => "test_submission_releasable";
   function Draw (Borrowed : System.Address; V : access constant Input)
     return Interfaces.C.int with Export, Convention => C, External_Name => "test_affine_and_record";
   procedure Capture_Begin (V : access constant Input)
     with Export, Convention => C, External_Name => "test_submission_capture_begin";
   function Capture (V : access constant Input) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_capture";
   function Capture_Backdrop (V : access constant Input; Mode : Interfaces.C.int) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_capture_backdrop";
   function Capture_Fill (V : access constant Input) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_capture_fill";
   function Capture_Gradient (V : access constant Input; Bottom : Interfaces.Unsigned_32) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_capture_gradient";
   function Capture_Clip (V : access constant Input; Reset : Interfaces.C.int) return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_capture_clip";
   function Capture_End return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_capture_end";
   function Replay return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_submission_replay_scene";
end Vulkan_Submission_Test_Bridge;
