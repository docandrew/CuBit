------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The boot timeline's user-space vocabulary (BOOT-001,
--  docs/boot-timeline.md): milestone codes, the kernel call and procmgr's
--  process-observer request that reads the timeline.
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
   --  Boot_Timeline_Layout.System_Call_Number and Operation.
   System_Call    : constant := 133;
   Read_Operation : constant := 0;
   Mark_Operation : constant := 1;
   Read_Chunk_Bytes : constant := 256;

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

   --  procmgr's process-observer role (CuBit.Process_Observer) also reads
   --  the timeline. Request: words (0) a grant reference of one writable
   --  page the caller owns, words (1) a page index into the timeline image.
   --  Reply label and words (0) a CuBit.Process_Observer.Status, words (1)
   --  bytes written, words (2) the image's size once a page reaches its
   --  end (0 while more follows).
   Timeline_Label : constant := 16#0108#;
   Page_Bytes     : constant := 4_096;
end CuBit.Boot_Milestone_Codes;
