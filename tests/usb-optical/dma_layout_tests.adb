with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_DMA_Layout; use XHCI_DMA_Layout;
with XHCI_Capabilities;
with XHCI_Registers;

procedure DMA_Layout_Tests is
   type Page_Map is array (Natural range 0 .. Page_Count'Last - 1) of Boolean;
   Occupied : Page_Map;
   Layout : Allocation;

   procedure Claim (Offset : Unsigned_64; Pages : Positive := 1) is
      First : constant Natural := Natural (Offset / PAGE_SIZE);
   begin
      pragma Assert (Offset mod PAGE_SIZE = 0);
      pragma Assert (First + Pages <= XHCI_DMA_Layout.Pages (Layout));
      for Page in First .. First + Pages - 1 loop
         pragma Assert (not Occupied (Page));
         Occupied (Page) := True;
      end loop;
   end Claim;

   Device_Offsets : constant array (Natural range 0 .. 7) of Unsigned_64 :=
     [DEVICE_CONTEXT_OFFSET, INPUT_CONTEXT_OFFSET, EP0_RING_OFFSET,
      HID_RING_OFFSET, DESCRIPTOR_OFFSET, HID_REPORT_OFFSET,
      BULK_OUT_RING_OFFSET, BULK_IN_RING_OFFSET];

   --  Every page of the plan for Count scratchpads and Slots slots is
   --  claimed exactly once, and the allocation is the smallest power of two.
   procedure Check (Count : XHCI_Capabilities.Scratchpad_Buffer_Count; Slots : Slot_Count) is
   begin
      Layout := Plan (Count, Slots);
      pragma Assert (Layout.Slots = Slots);
      pragma Assert (Bytes (Layout) = 2 ** Layout.Order * PAGE_SIZE);
      pragma Assert (Count * 8 <= Layout.Pointer_Pages * 4096);
      pragma Assert (Layout.Used_Pages <= Pages (Layout));
      pragma Assert (Layout.Used_Pages > Pages (Layout) / 2);
      Occupied := [others => False];
      Claim (DCBAA_OFFSET);
      Claim (COMMAND_RING_OFFSET);
      Claim (EVENT_RING_OFFSET);
      Claim (ERST_OFFSET);
      Claim (BULK_DATA_OFFSET, BULK_DATA_PAGES);
      Claim (BULK_CBW_OFFSET);
      Claim (BULK_CSW_OFFSET);
      for Slot in 1 .. Slots loop
         for Offset of Device_Offsets loop
            Claim (Offset + Unsigned_64 (Slot - 1) * DEVICE_STRIDE);
         end loop;
      end loop;
      if Layout.Pointer_Pages > 0 then
         Claim (Layout.Scratch_Array, Layout.Pointer_Pages);
      end if;
      for Buffer_Number in 1 .. Count loop
         Claim (Layout.Scratch_First + Unsigned_64 (Buffer_Number - 1) * PAGE_SIZE);
      end loop;
      pragma Assert
        (Layout.Scratch_First + Unsigned_64 (Count) * PAGE_SIZE =
           Unsigned_64 (Layout.Used_Pages) * PAGE_SIZE);
      for Page in 0 .. Layout.Used_Pages - 1 loop
         pragma Assert (Occupied (Page));
      end loop;
   end Check;

   type Count_List is array (Positive range <>) of XHCI_Capabilities.Scratchpad_Buffer_Count;
   type Slot_List is array (Positive range <>) of Slot_Count;
   Ceiling : Unsigned_64;
   Fitting : Natural;
   Word : Unsigned_64;
begin
   --  Laptop reports 0x22 = 34 pages. Verify that the decoded requirement
   --  fits the same constants used for native allocation and ring addressing.
   pragma Assert (XHCI_Capabilities.Scratchpad_Count (16#1020_0000#) = 34);
   pragma Assert (Plan (0, 8).Pointer_Pages = 0);
   pragma Assert (Plan (34, 8).Order = 7);
   pragma Assert (Plan (512, 8).Pointer_Pages = 1);
   pragma Assert (Plan (513, 8).Pointer_Pages = 2);

   --  LIM-001: the slots come from HCSPARAMS1 MaxSlots, not a fixed 8.
   --  QEMU's qemu-xhci reports 64; Intel PCH controllers report 64; the
   --  protocol allows 255. The old layout held 8 slots: slot 9 had no area.
   pragma Assert (XHCI_Registers.Max_Slots (16#0800_0840#) = 64);
   pragma Assert (XHCI_Registers.Max_Slots (16#0800_08FF#) = 255);
   Layout := Plan (34, 64);
   pragma Assert (Layout.Slots = 64);
   pragma Assert (Layout.Scratch_Array = DEVICE_CONTEXT_OFFSET + 64 * DEVICE_STRIDE);
   Check (34, 9);
   Check (0, PROTOCOL_MAX_SLOTS);
   Check (XHCI_Capabilities.Scratchpad_Buffer_Count'Last, PROTOCOL_MAX_SLOTS);
   pragma Assert
     (Plan (XHCI_Capabilities.Scratchpad_Buffer_Count'Last, PROTOCOL_MAX_SLOTS).Order =
        Allocation_Order'Last);

   --  Every slot count with representative scratchpad counts, and every
   --  scratchpad count with representative slot counts.
   for Slots in Slot_Count loop
      for Count of Count_List'(0, 1, 34, 511, 512, 513, 1023)
      loop
         Check (Count, Slots);
      end loop;
   end loop;
   for Count in XHCI_Capabilities.Scratchpad_Buffer_Count loop
      for Slots of Slot_List'(1, 8, 9, 64, 255) loop
         Check (Count, Slots);
      end loop;
   end loop;

   --  The manifest's 16 MiB ceiling holds every protocol-legal controller.
   pragma Assert
     (Slots_Within (XHCI_Capabilities.Scratchpad_Buffer_Count'Last, PROTOCOL_MAX_SLOTS,
                    16_777_216) = PROTOCOL_MAX_SLOTS);
   --  A smaller ceiling gives the most slots that fit, never more.
   for Order in Allocation_Order loop
      Ceiling := 2 ** Order * PAGE_SIZE;
      for Count of Count_List'(0, 34, 1023) loop
         Fitting := Slots_Within (Count, PROTOCOL_MAX_SLOTS, Ceiling);
         if Fitting > 0 then
            pragma Assert (Bytes (Plan (Count, Fitting)) <= Ceiling);
         end if;
         if Fitting < PROTOCOL_MAX_SLOTS then
            pragma Assert (Fitting = 0 or else Bytes (Plan (Count, Fitting + 1)) > Ceiling);
         end if;
      end loop;
   end loop;
   pragma Assert (Slots_Within (0, 64, 4096) = 0);

   --  The configure word carries pages, scratchpads and slots unchanged.
   for Slots of Slot_List'(1, 9, 64, 255) loop
      Word := Configuration_Word (256, Plan (1023, Slots), 1023);
      pragma Assert ((Word and CONFIG_PAGES_MASK) = 256);
      pragma Assert (Scratchpads_Of (Word) = 1023);
      pragma Assert (Slots_Of (Word) = Unsigned_64 (Slots));
   end loop;
   Ada.Text_IO.Put_Line
     ("XHCI-DMA-LAYOUT: PASS 1..255 slots x 0..1023 scratchpads, minimal allocation, no page overlap");
end DMA_Layout_Tests;
