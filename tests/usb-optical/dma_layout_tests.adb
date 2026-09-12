with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_DMA_Layout; use XHCI_DMA_Layout;
with XHCI_Capabilities;

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
begin
   --  Laptop reports 0x22 = 34 pages. Verify that the decoded requirement
   --  fits the same constants used for native allocation and ring addressing.
   pragma Assert (XHCI_Capabilities.Scratchpad_Count (16#1020_0000#) = 34);
   pragma Assert (Plan (0).Pointer_Pages = 0);
   pragma Assert (Plan (34).Order = 7);
   pragma Assert (Plan (128).Order = 8);
   pragma Assert (Plan (512).Pointer_Pages = 1);
   pragma Assert (Plan (513).Pointer_Pages = 2);
   pragma Assert (Plan (1023).Order = 11);

   for Count in XHCI_Capabilities.Scratchpad_Buffer_Count loop
      Layout := Plan (Count);
      pragma Assert (Bytes (Layout) = 2 ** Layout.Order * PAGE_SIZE);
      pragma Assert (Count * 8 <= Layout.Pointer_Pages * 4096);
      pragma Assert (Layout.Used_Pages <= Pages (Layout));
      pragma Assert (Layout.Used_Pages > Pages (Layout) / 2);
      Occupied := [others => False];
      Claim (DCBAA_OFFSET);
      Claim (COMMAND_RING_OFFSET);
      Claim (EVENT_RING_OFFSET);
      Claim (ERST_OFFSET);
      if Layout.Pointer_Pages > 0 then
         Claim (SCRATCH_ARRAY_OFFSET, Layout.Pointer_Pages);
      end if;
      for Buffer_Number in 1 .. Count loop
         Claim (Layout.Scratch_First + Unsigned_64 (Buffer_Number - 1) * PAGE_SIZE);
      end loop;
      for Slot in 1 .. MAX_DEVICE_SLOTS loop
         for Offset of Device_Offsets loop
            Claim (Offset + Unsigned_64 (Slot - 1) * DEVICE_STRIDE);
         end loop;
      end loop;
      Claim (BULK_DATA_OFFSET, BULK_DATA_PAGES);
      Claim (BULK_CBW_OFFSET);
      Claim (BULK_CSW_OFFSET);
      pragma Assert
        (Layout.Scratch_First + Unsigned_64 (Count) * PAGE_SIZE =
           Unsigned_64 (Layout.Used_Pages) * PAGE_SIZE);
   end loop;
   Ada.Text_IO.Put_Line
     ("XHCI-DMA-LAYOUT: PASS 0..1023 scratchpads, minimal allocation, no page overlap");
end DMA_Layout_Tests;
