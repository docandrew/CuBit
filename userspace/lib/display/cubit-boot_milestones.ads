------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Mark a boot milestone in the kernel's boot timeline (BOOT-001,
--  docs/boot-timeline.md).
--
--  @description
--  Each milestone is marked once per process: after the first call, Mark is
--  one load and one branch, so it can sit on a hot path. The kernel accepts
--  a mark only with the owning service's registration notification
--  (Authority, Write; CAP-002: named by handle), and keeps the first.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Boot_Milestone_Codes;
with CuBit.Handles;

package CuBit.Boot_Milestones is
   subtype Milestone is CuBit.Boot_Milestone_Codes.Milestone;
   procedure Mark (Item : Milestone; Authority : CuBit.Handles.Notification);
end CuBit.Boot_Milestones;
