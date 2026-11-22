------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;
with RTW89_Registers; use RTW89_Registers;
with RTW89_MMIO; use RTW89_MMIO;
with RTW89_Layout; use RTW89_Layout;
with RTW89_Descriptors; use RTW89_Descriptors;

package body RTW89_Transmit is

   --  The release report's queue selector (pci.h RTW89_PCI_RPP_QSEL [12:8]).
   Report_QSel_Shift : constant := 8;
   Report_QSel_Mask  : constant Unsigned_32 := 16#1F#;
   Report_Status_Shift : constant := 13;
   Report_Status_Mask  : constant Unsigned_32 := 16#7#;

   Ring_Of : constant array (Data_Channel) of TX_Channel :=
     [Management_Channel => CH8, Data_Best_Effort => ACH0];
   WD_Channel : constant array (Data_Channel) of RTW89_TX.Channel :=
     [Management_Channel => RTW89_TX.B0MG, Data_Best_Effort => RTW89_TX.ACH0];

   type Busy_Slots is array (0 .. TX_Slots - 1) of Boolean;
   Busy : array (Data_Channel) of Busy_Slots := [others => [others => False]];
   Host_Next : array (Data_Channel) of Natural := [others => 0];
   Counted : Statistics;

   procedure Reset is
   begin
      Busy := [others => [others => False]];
      Host_Next := [others => 0];
   end Reset;

   function Free_Slot (On : Data_Channel) return Natural is
   begin
      for S in Busy (On)'Range loop
         if not Busy (On) (S) then
            return S;
         end if;
      end loop;
      return TX_Slots;
   end Free_Slot;

   function Has_Room (On : Data_Channel) return Boolean is (Free_Slot (On) < TX_Slots);

   function At_Offset (DMA : RTW89_Bringup.Region; Offset : Natural) return System.Address is
     (To_Address (Integer_Address (DMA.Virtual + Unsigned_64 (Offset))));

   procedure Send (DMA : RTW89_Bringup.Region; On : Data_Channel; Frame : Frame_Bytes;
                   R : RTW89_TX.Request; Sent : out Boolean) is
      Slot : constant Natural := Free_Slot (On);
   begin
      Sent := False;
      if Slot = TX_Slots or else Frame'Length not in 1 .. Maximum_TX_Frame then
         Counted.No_Room := Counted.No_Room + 1;
         return;
      end if;
      declare
         Base : constant Natural := TX_Slot_Offset (On, Slot);
         Frame_Bus : constant Bus_Address := Bus_Address (DMA.Physical + Unsigned_64 (Base + Frame_In_Slot));
         Page : constant RTW89_TX.Page :=
           RTW89_TX.Fill ((R with delta On => WD_Channel (On), Size => Frame'Length, Frame_Bus => Frame_Bus,
                                        Page_Sequence => Slot));
         Page_Cells : RTW89_TX.Page with Import, Volatile, Address => At_Offset (DMA, Base);
         Frame_Cells : Frame_Bytes (0 .. Frame'Length - 1) with Import, Volatile,
           Address => At_Offset (DMA, Base + Frame_In_Slot);
         BD : constant BD_Words := TX_BD (RTW89_TX.Page_Bytes, Bus_Address (DMA.Physical + Unsigned_64 (Base)));
         BD_At : constant Natural := TX_Ring_Offset (Ring_Of (On)) + Host_Next (On) * BD_Bytes;
         Low : Unsigned_32 with Import, Volatile, Address => At_Offset (DMA, BD_At);
         High : Unsigned_32 with Import, Volatile, Address => At_Offset (DMA, BD_At + 4);
      begin
         for I in Frame'Range loop
            Frame_Cells (I) := Frame (I);
         end loop;
         Page_Cells := Page;
         Low := BD.Low;
         High := BD.High;
         Busy (On) (Slot) := True;
         Host_Next (On) := (Host_Next (On) + 1) mod TX_Ring_Entries;
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         Write16 (TX_Rings (Ring_Of (On)).Index, Unsigned_16 (Host_Next (On)));
      end;
      Counted.Sent := Counted.Sent + 1;
      Sent := True;
   end Send;

   procedure Released (Word : Unsigned_32) is
      QSel : constant Unsigned_32 := Shift_Right (Word, Report_QSel_Shift) and Report_QSel_Mask;
      Sequence : constant RTW89_TX.WP_Sequence := RTW89_TX.Report_Sequence (Word);
      On : Data_Channel;
   begin
      Counted.Reports := Counted.Reports + 1;
      declare
         S : constant TX_Status := TX_Status (Shift_Right (Word, Report_Status_Shift) and Report_Status_Mask);
      begin
         Counted.By_Status (S) := Counted.By_Status (S) + 1;
      end;
      if QSel = RTW89_TX.QSel_Management then
         On := Management_Channel;
      elsif QSel = RTW89_TX.QSel_Best_Effort then
         On := Data_Best_Effort;
      else
         Counted.Unknown_Reports := Counted.Unknown_Reports + 1;
         return;
      end if;
      if Sequence < TX_Slots then
         Busy (On) (Sequence) := False;
      else
         Counted.Unknown_Reports := Counted.Unknown_Reports + 1;
      end if;
   end Released;

   function Counts return Statistics is (Counted);

end RTW89_Transmit;
