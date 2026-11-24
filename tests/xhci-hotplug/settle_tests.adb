--  The boot discovery window's end (XHCI_Boot_Settle): which port states
--  are still moving, and when the window may end. The fixed 3 s window it
--  replaced held devmgr's boot on the IdeaPad (tests/xhci-bringup).
with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_Registers; use XHCI_Registers;
with XHCI_Boot_Settle; use XHCI_Boot_Settle;

procedure Settle_Tests is
   Checks : Natural := 0;

   function Port (Connected : Boolean; State : Link_State; Speed : Port_Speed := SPEED_NONE;
                  Resetting : Boolean := False; Changes : Unsigned_32 := 0) return Unsigned_32 is
     ((if Connected then PORTSC_CCS else 0) or (if Resetting then PORTSC_PR else 0) or
      PORTSC_PP or Changes or
      Shift_Left (Unsigned_32 (State), PORTSC_PLS_SHIFT) or
      Shift_Left (Unsigned_32 (Speed), PORTSC_SPEED_SHIFT));

   procedure Expect (Condition : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Ada.Text_IO.Put_Line ("settle-tests: FAIL " & Message);
         raise Program_Error;
      end if;
   end Expect;
begin
   --  QEMU's snapshot of a USB 2.0 device waiting for its reset
   --  (portsc=0x00020EE1: ccs=1 pls=polling spd=high chg=c): still.
   Expect (not Moving (16#0002_0EE1#), "a connected USB 2.0 port in Polling is still");
   Expect (not Moving (Port (False, LINK_RX_DETECT)), "an empty SuperSpeed port is still");
   Expect (not Moving (Port (True, LINK_U0, SPEED_SUPER)), "an enabled link is still");
   Expect (Moving (Port (False, LINK_POLLING)), "a SuperSpeed link training is moving");
   Expect (Moving (Port (True, LINK_U0, SPEED_SUPER, Resetting => True)), "a reset is moving");
   Expect (Moving (Port (True, LINK_RECOVERY, SPEED_SUPER)), "recovery is moving");
   Expect (Moving (Port (True, LINK_RESUME, SPEED_HIGH)), "resume is moving");

   --  The change bits are not part of what is compared.
   Expect (Attachment (Port (True, LINK_U0, SPEED_HIGH, Changes => PORTSC_CHANGE_BITS)) =
           Attachment (Port (True, LINK_U0, SPEED_HIGH)), "change bits ignored");
   Expect (Attachment (Port (True, LINK_U0, SPEED_HIGH)) /= Attachment (Port (False, LINK_RX_DETECT)),
           "a connection is a change");
   Expect (Attachment (Port (False, LINK_POLLING)) /= Attachment (Port (False, LINK_RX_DETECT)),
           "a link state is a change");

   --  The window: quiet ports end it after Quiet_Ms; a moving port keeps
   --  it open up to Limit_Ms.
   Expect (Decide (0, 0, False) = Keep_Waiting, "not before the first quiet interval");
   Expect (Decide (Quiet_Ms - 1, Quiet_Ms - 1, False) = Keep_Waiting, "quiet just short");
   Expect (Decide (Quiet_Ms, Quiet_Ms, False) = Ports_Quiet, "quiet ends the window");
   Expect (Decide (1_000, Quiet_Ms, True) = Keep_Waiting, "a moving port holds it");
   Expect (Decide (Limit_Ms - 1, 0, True) = Keep_Waiting, "below the ceiling");
   Expect (Decide (Limit_Ms, 0, True) = Window_Ceiling, "the ceiling ends it");
   Expect (Decide (Limit_Ms, Limit_Ms, False) = Window_Ceiling, "the ceiling wins");
   Ada.Text_IO.Put_Line ("settle-tests: PASS" & Natural'Image (Checks) & " checks");
end Settle_Tests;
