------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The boot timeline's user-space vocabulary (BOOT-001,
--  docs/boot-timeline.md): milestone codes. Marking one is a Call on the
--  owner's notification handle (CuBit.Handles.Devices.Mark_Milestone);
--  reading the timeline is the granted publication "boot-timeline".
--
--  @description
--  The kernel's definitions are shared/boot/boot_timeline_layout.ads. This
--  copy exists because the Desktop's sealed build copies only userspace
--  library directories; tests/boot-timeline checks that both agree.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.Boot_Milestone_Codes with
   Pure, SPARK_Mode
is
   type Milestone is
     (Startup_Profile_Started,   --  procmgr begins init.ccl
      Startup_Profile_Launched,  --  procmgr launched every init.ccl entry
      Display_Leased,            --  display.svc grants output zero's lease
      First_Frame_Presented,     --  Desktop's first frame published
      First_Input_Accepted);     --  Desktop accepted its first input event
   for Milestone use
     (Startup_Profile_Started  => 0,
      Startup_Profile_Launched => 1,
      Display_Leased           => 2,
      First_Frame_Presented    => 3,
      First_Input_Accepted     => 4);
end CuBit.Boot_Milestone_Codes;
