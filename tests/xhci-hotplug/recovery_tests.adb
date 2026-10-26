--  Halted interrupt-endpoint recovery (XHCI_Endpoint_Recovery), device
--  roles for composite HID devices (USB_Device_Roles), the controller
--  identity word and quirk table, and the completion mailboxes' room and
--  slot clearing.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_Registers; use XHCI_Registers;
with XHCI_Endpoint_Recovery; use XHCI_Endpoint_Recovery;
with USB_Configurations;
with USB_Device_Roles; use USB_Device_Roles;
with XHCI_Controller_Identity;
with XHCI_Completions;

procedure Recovery_Tests is
   R : Pipe_Recovery;
   type Code_List is array (Positive range <>) of Completion_Code;
   Halting : constant Code_List :=
     [CC_STALL, CC_BABBLE, CC_TRANSACTION_ERROR, CC_SPLIT_TRANSACTION_ERROR];
   package CI renames XHCI_Controller_Identity;
   use type CI.Identity;
   use type USB_Configurations.Decode_Result;

   --  A model device and controller for the ladder: data toggles, and a
   --  fault that persists until the port is reset (the IdeaPad's mouse:
   --  transaction errors through soft and hard recovery, 2026-10-25).
   type Rung is (Soft, Hard, Port);
   Host_Toggle, Device_Toggle : Natural range 0 .. 1 := 0;
   Faulty : Boolean := False;
   History : array (1 .. 16) of Rung;
   Rungs : Natural := 0;

   --  Run the step R is at to completion against the model.
   procedure Run_Rung is
   begin
      Rungs := Rungs + 1;
      case R.Current is
         when Need_Soft_Reset =>
            History (Rungs) := Soft;
            Issued (R);
            Completed (R, CC_SUCCESS);   --  TSP=1: toggles kept
            pragma Assert (R.Current = Need_Doorbell, "soft retry rings the doorbell");
            Rang (R);
         when Need_Reset =>
            History (Rungs) := Hard;
            Issued (R);
            Host_Toggle := 0;            --  TSP=0
            Completed (R, CC_SUCCESS);
            Issued (R);
            Completed (R, CC_SUCCESS);
            pragma Assert (R.Current = Need_Clear_Halt,
                           "every hard reset resynchronizes the device toggle");
            Issued (R);
            Device_Toggle := 0;          --  CLEAR_FEATURE(ENDPOINT_HALT)
            Completed (R, CC_SUCCESS);
            pragma Assert (R.Current = Need_Requeue);
            Rang (R);
            pragma Assert (Host_Toggle = Device_Toggle, "toggles agree after a hard reset");
         when Need_Port_Reset =>
            History (Rungs) := Port;
            Faulty := False;             --  re-enumeration: a fresh device
            Host_Toggle := 0;
            Device_Toggle := 0;
            R := (others => <>);
         when others =>
            raise Program_Error;
      end case;
      pragma Assert (R.Current = Running);
   end Run_Rung;

   --  A Logitech-style wireless receiver: boot keyboard (if 0), boot mouse
   --  (if 1), vendor HID (if 2).
   Receiver : constant USB_Configurations.Bytes :=
     [9, 2, 84, 0, 3, 1, 0, 16#A0#, 49,
      9, 4, 0, 0, 1, 3, 1, 1, 0,   9, 16#21#, 16#11#, 1, 0, 1, 16#22#, 59, 0,
      7, 5, 16#81#, 3, 8, 0, 8,
      9, 4, 1, 0, 1, 3, 1, 2, 0,   9, 16#21#, 16#11#, 1, 0, 1, 16#22#, 148, 0,
      7, 5, 16#82#, 3, 8, 0, 2,
      9, 4, 2, 0, 1, 3, 0, 0, 0,   9, 16#21#, 16#11#, 1, 0, 1, 16#22#, 54, 0,
      7, 5, 16#83#, 3, 32, 0, 2];
   C : USB_Configurations.Configuration;
   Decoded : USB_Configurations.Decode_Result;
   S : Selection;
   Id : CI.Identity;
   Q : CI.Quirk_Set;
   Boxes : XHCI_Completions.Mailboxes;
   Route : XHCI_Completions.Route_Result;
   use type XHCI_Completions.Route_Result;
   function Transfer (Slot, DCI : Unsigned_32) return XHCI_Completions.Event is
     ((ParameterLo => 0, ParameterHi => 0, Status => 0,
       Control => Slot_Field (Slot) or Endpoint_Field (DCI) or Type_Field (TRB_TRANSFER_EVENT)));
begin
   --  Classification.
   pragma Assert (Classify (CC_SUCCESS, False) = Deliver);
   pragma Assert (Classify (CC_SHORT_PACKET, False) = Deliver, "short packets carry reports");
   for Code of Halting loop
      pragma Assert (Classify (Code, False) = Recover, "halting errors recover");
   end loop;
   pragma Assert (Classify (CC_MISSED_SERVICE, False) = Replace);
   pragma Assert (Classify (CC_DATA_BUFFER_ERROR, False) = Replace);
   pragma Assert (Classify (CC_DATA_BUFFER_ERROR, True) = Recover, "context says halted");
   pragma Assert (Classify (CC_STOPPED, False) = Ignore);

   --  The ladder: transaction errors that persist through three soft
   --  retries and two hard resets are recovered by the port reset.
   Faulty := True;
   Device_Toggle := 1;   --  the device toggled; the host did not (desync)
   while Faulty loop
      Halted (R, CC_TRANSACTION_ERROR);
      Run_Rung;
   end loop;
   pragma Assert (Rungs = Soft_Limit + Hard_Limit + 1);
   pragma Assert (History (1 .. Rungs) = [Soft, Soft, Soft, Hard, Hard, Port]);
   Delivered (R);
   --  A stall (the device halted itself) skips the soft rung.
   Rungs := 0;
   Halted (R, CC_STALL);
   pragma Assert (R.Current = Need_Reset, "a stall goes straight to the hard reset");
   Run_Rung;
   --  A report in between clears the counts: errors after it start soft.
   Halted (R, CC_TRANSACTION_ERROR);
   Run_Rung;
   Delivered (R);
   pragma Assert (R.Soft = 0 and then R.Hard = 0, "a delivered report resets the ladder");
   Halted (R, CC_TRANSACTION_ERROR);
   pragma Assert (R.Current = Need_Soft_Reset);
   --  A refused step escalates to the port reset.
   R := (others => <>);
   Halted (R, CC_BABBLE);
   Issued (R);
   Completed (R, CC_CONTEXT_STATE_ERROR);
   pragma Assert (R.Current = Need_Dequeue, "already-stopped endpoint is fine");
   Issued (R);
   Completed (R, CC_TRB_ERROR);
   pragma Assert (R.Current = Need_Port_Reset, "a refused dequeue escalates");
   pragma Assert (Port_Reset_Delay (0) = 1_000 and then Port_Reset_Delay (2) = 4_000);
   R := (others => <>);

   --  The composite receiver: both roles, not the keyboard alone.
   USB_Configurations.Decode (Receiver, C, Decoded);
   pragma Assert (Decoded = USB_Configurations.Decoded);
   pragma Assert (C.Mouse.Present and then C.Keyboard.Present);
   S := Select_Roles (C, Mouse_Free => True, Keyboard_Free => True, Storage_Free => True);
   pragma Assert (S.Kind = HID_Role and then S.Mouse and then S.Keyboard,
                  "composite receiver drives its mouse too");
   pragma Assert (C.Mouse.Number = 1 and then C.Mouse.Input.Address = 16#82#);
   S := Select_Roles (C, Mouse_Free => True, Keyboard_Free => False, Storage_Free => True);
   pragma Assert (S.Kind = HID_Role and then S.Mouse and then not S.Keyboard);
   S := Select_Roles (C, Mouse_Free => False, Keyboard_Free => False, Storage_Free => True);
   pragma Assert (S.Kind = Unsupported);

   --  Identity word round trip and the Alder Lake-N quirks.
   Id := (Vendor => 16#8086#, Device => 16#54ED#, Revision => 1);
   pragma Assert (CI.Identity_Of (CI.Encode (2, Id)) = Id);
   pragma Assert (CI.Mode_Of (CI.Encode (2, Id)) = 2);
   pragma Assert (CI.Well_Formed (CI.Encode (2, Id)));
   pragma Assert (not CI.Well_Formed (16#0001_0000_0000_0002#));
   Q := CI.Quirks (Id, 16#0120#);
   pragma Assert (Q (CI.Intel_Host) and Q (CI.Reset_To_Default) and Q (CI.Default_PM_Runtime_Allow));
   Q := CI.Quirks ((Vendor => 16#1B36#, Device => 16#000D#, Revision => 1), 16#0100#);
   pragma Assert (not (for some B of Q => B), "QEMU's controller has no quirk");

   --  Mailboxes: Has_Room predicts Queue_Full; Clear_Slot empties a slot.
   XHCI_Completions.Clear (Boxes);
   for I in 1 .. XHCI_Completions.Queue_Depth loop
      pragma Assert (XHCI_Completions.Has_Room (Boxes, Transfer (2, 3)));
      XHCI_Completions.Route (Boxes, Transfer (2, 3), Route);
      pragma Assert (Route = XHCI_Completions.Queued);
   end loop;
   pragma Assert (not XHCI_Completions.Has_Room (Boxes, Transfer (2, 3)));
   pragma Assert (XHCI_Completions.Has_Room (Boxes, Transfer (2, 5)));
   XHCI_Completions.Clear_Slot (Boxes, 2);
   pragma Assert (XHCI_Completions.Pending (Boxes, 2, 3) = 0);
   pragma Assert (XHCI_Completions.Has_Room (Boxes, Transfer (2, 3)));
   Ada.Text_IO.Put_Line ("recovery-tests: PASS");
end Recovery_Tests;
