------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Unified, authority-scoped hardware inventory and diagnostics
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Devices;
with CuBit.Messages; use CuBit.Messages;
with CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Labels;
with CuBit.UI.State;
with CuBit.UI.Trees;
with CuBit.UI.Widgets;

procedure main is
   use ASCII;
   use type CuBit.Devices.Device_Kind;
   use type CuBit.Devices.Driver_State;

   INITIAL_WIDTH  : constant Positive := 900;
   INITIAL_HEIGHT : constant Positive := 580;
   CAP_SLOT_DEVICE_INSPECTION : constant CapabilitySlot := 22;

   CONTROL_REFRESH    : constant CuBit.UI.Controls.Control_ID := 1;
   CONTROL_SPLITTER   : constant CuBit.UI.Controls.Control_ID := 2;
   CONTROL_SCROLLBAR  : constant CuBit.UI.Controls.Control_ID := 3;
   CONTROL_TREE_FIRST : constant CuBit.UI.Controls.Control_ID := 10;

   NODE_COMPUTER : constant Natural := 1;
   NODE_PLATFORM : constant Natural := 2;
   NODE_CPU      : constant Natural := 3;
   NODE_MEMORY   : constant Natural := 4;
   NODE_PCI      : constant Natural := 5;
   NODE_INPUT    : constant Natural := 6;
   NODE_PS2      : constant Natural := 7;
   NODE_PCI_FIRST : constant Natural := 100;
   NODE_USB_MOUSE : constant Natural := 300;

   MAX_LOCAL_DEVICES : constant Positive := CuBit.Devices.MAX_PCI_DEVICES;
   type Device_Record is record
      valid     : Boolean := False;
      bus       : Unsigned_8 := 0;
      slot      : Unsigned_8 := 0;
      functionN : Unsigned_8 := 0;
      kind      : CuBit.Devices.Device_Kind := CuBit.Devices.Other_Device;
      state     : CuBit.Devices.Driver_State := CuBit.Devices.Unclaimed;
      vendorID  : Unsigned_16 := 0;
      deviceID  : Unsigned_16 := 0;
      classCode : Unsigned_16 := 0;
      progIf    : Unsigned_8 := 0;
      driverPID : Unsigned_64 := 0;
   end record;
   type Device_Array is array (Positive range 1 .. MAX_LOCAL_DEVICES) of
     Device_Record;

   type Diagnostic_Record is record
      valid             : Boolean := False;
      decodedReports    : Unsigned_64 := 0;
      motionReports     : Unsigned_64 := 0;
      buttonTransitions : Unsigned_32 := 0;
      completionErrors  : Unsigned_32 := 0;
      lastReport        : Unsigned_32 := 0;
      lastLength        : Unsigned_8 := 0;
      lastCompletion    : Unsigned_8 := 0;
      interruptMode     : CuBit.Devices.Interrupt_Mode :=
        CuBit.Devices.Interrupt_Polling;
   end record;

   type Key_Array is array (Positive range 1 .. MAX_LOCAL_DEVICES + 8) of
     Natural;

   win : CuBit.UI.App.Window;
   ui : CuBit.UI.State.UI_State;
   controls : CuBit.UI.Controls.Control_Map;
   ignore : Unsigned_64;
   devices : Device_Array;
   deviceCount : Natural range 0 .. MAX_LOCAL_DEVICES := 0;
   inventoryOverflow : Boolean := False;
   inventoryAvailable : Boolean := False;
   diagnostic : Diagnostic_Record;
   selectedNode : Natural := NODE_COMPUTER;
   treeFocused : Boolean := True;
   treeScroll : Natural := 0;
   splitPosition : Natural := 340;
   computerExpanded : Boolean := True;
   platformExpanded : Boolean := True;
   pciExpanded : Boolean := True;
   inputExpanded : Boolean := True;
   xhciExpanded : Boolean := True;
   visibleKeys : Key_Array := (others => 0);
   visibleKeyCount : Natural range 0 .. Key_Array'Length := 0;
   lastTreeBounds : CuBit.UI.Rect := (others => 0);

   KEY_UP    : constant Unsigned_64 := 16#48#;
   KEY_DOWN  : constant Unsigned_64 := 16#50#;
   KEY_LEFT  : constant Unsigned_64 := 16#4B#;
   KEY_RIGHT : constant Unsigned_64 := 16#4D#;
   KEY_HOME  : constant Unsigned_64 := 16#47#;
   KEY_END   : constant Unsigned_64 := 16#4F#;
   KEY_F5    : constant Unsigned_64 := 16#3F#;

   function Hex_Digit (value : Unsigned_64) return Character is
      symbols : constant String := "0123456789ABCDEF";
   begin
      return symbols (Natural (value and 16#F#) + 1);
   end Hex_Digit;

   function Hex (value : Unsigned_64; width : Positive) return String is
      result : String (1 .. width);
   begin
      for i in result'Range loop
         result (i) := Hex_Digit
           (Shift_Right (value, (result'Last - i) * 4));
      end loop;
      return result;
   end Hex;

   function Decimal (value : Unsigned_64) return String is
      raw : constant String := Unsigned_64'Image (value);
   begin
      return raw (raw'First + 1 .. raw'Last);
   end Decimal;

   function Kind_Name (kind : CuBit.Devices.Device_Kind) return String is
   begin
      case kind is
         when CuBit.Devices.Storage_Controller => return "Storage controller";
         when CuBit.Devices.Network_Controller => return "Network controller";
         when CuBit.Devices.Display_Controller => return "Display controller";
         when CuBit.Devices.Audio_Controller   => return "Audio controller";
         when CuBit.Devices.USB_Controller     => return "USB controller";
         when CuBit.Devices.Other_Device       => return "PCI device";
      end case;
   end Kind_Name;

   function Kind_Icon
      (kind : CuBit.Devices.Device_Kind)
       return CuBit.UI.Trees.Tree_Item_Icon
   is
   begin
      case kind is
         when CuBit.Devices.Storage_Controller =>
            return CuBit.UI.Trees.Storage_Icon;
         when CuBit.Devices.Network_Controller =>
            return CuBit.UI.Trees.Network_Icon;
         when CuBit.Devices.Display_Controller =>
            return CuBit.UI.Trees.Display_Icon;
         when CuBit.Devices.Audio_Controller =>
            return CuBit.UI.Trees.Audio_Icon;
         when CuBit.Devices.USB_Controller =>
            return CuBit.UI.Trees.Input_Icon;
         when CuBit.Devices.Other_Device =>
            return CuBit.UI.Trees.Device_Icon;
      end case;
   end Kind_Icon;

   function State_Name (state : CuBit.Devices.Driver_State) return String is
   begin
      case state is
         when CuBit.Devices.Unclaimed       => return "Unclaimed";
         when CuBit.Devices.Driver_Starting => return "Starting";
         when CuBit.Devices.Driver_Active   => return "Active";
         when CuBit.Devices.Driver_Failed   => return "Failed";
      end case;
   end State_Name;

   function Interrupt_Name
      (mode : CuBit.Devices.Interrupt_Mode) return String
   is
   begin
      case mode is
         when CuBit.Devices.Interrupt_Polling => return "Polling";
         when CuBit.Devices.Interrupt_MSI     => return "MSI";
         when CuBit.Devices.Interrupt_MSIX    => return "MSI-X";
      end case;
   end Interrupt_Name;

   function Device_Label (item : Device_Record) return String is
   begin
      return Kind_Name (item.kind) & "  " &
        Hex (Unsigned_64 (item.vendorID), 4) & ":" &
        Hex (Unsigned_64 (item.deviceID), 4);
   end Device_Label;

   procedure Load_Inventory is
      msg : Message;
      tag : MessageTag;
      count : Natural;
      locationWord : Unsigned_64;
      identityWord : Unsigned_64;
      kindRep : Unsigned_64;
      stateRep : Unsigned_64;
      modeRep : Unsigned_64;
   begin
      deviceCount := 0;
      inventoryAvailable := False;
      devices := (others => (others => <>));

      msg := NULL_MESSAGE;
      msg.tag := (label => CuBit.Devices.OP_INVENTORY_COUNT,
                  length => 0, flags => 0, badge => 0);
      tag := capCall (CAP_SLOT_DEVICE_INSPECTION, msg);
      if tag.label /= CuBit.Devices.REPLY_OK then
         return;
      end if;
      inventoryAvailable := True;
      debugPrint ("devices: inventory snapshot ready" & LF);
      inventoryOverflow := msg.words (1) /= 0;
      if msg.words (0) > Unsigned_64 (MAX_LOCAL_DEVICES) then
         count := MAX_LOCAL_DEVICES;
      else
         count := Natural (msg.words (0));
      end if;

      for index in 1 .. count loop
         msg := NULL_MESSAGE;
         msg.tag := (label => CuBit.Devices.OP_INVENTORY_ITEM,
                     length => 1, flags => 0, badge => 0);
         msg.words (0) := Unsigned_64 (index);
         tag := capCall (CAP_SLOT_DEVICE_INSPECTION, msg);
         if tag.label = CuBit.Devices.REPLY_OK then
            locationWord := msg.words (0);
            identityWord := msg.words (1);
            kindRep := Shift_Right (locationWord, 24) and 16#FF#;
            stateRep := Shift_Right (locationWord, 32) and 16#FF#;
            if kindRep <= Unsigned_64
                 (CuBit.Devices.Device_Kind'Enum_Rep
                    (CuBit.Devices.Device_Kind'Last)) and then
               stateRep <= Unsigned_64
                 (CuBit.Devices.Driver_State'Enum_Rep
                    (CuBit.Devices.Driver_State'Last))
            then
               deviceCount := deviceCount + 1;
               devices (deviceCount) :=
                 (valid => True,
                  bus => Unsigned_8 (locationWord and 16#FF#),
                  slot => Unsigned_8 (Shift_Right (locationWord, 8) and 16#FF#),
                  functionN => Unsigned_8
                    (Shift_Right (locationWord, 16) and 16#FF#),
                  kind => CuBit.Devices.Device_Kind'Enum_Val
                    (Natural (kindRep)),
                  state => CuBit.Devices.Driver_State'Enum_Val
                    (Natural (stateRep)),
                  vendorID => Unsigned_16 (identityWord and 16#FFFF#),
                  deviceID => Unsigned_16
                    (Shift_Right (identityWord, 16) and 16#FFFF#),
                  classCode => Unsigned_16
                    (Shift_Right (identityWord, 32) and 16#FFFF#),
                  progIf => Unsigned_8
                    (Shift_Right (identityWord, 48) and 16#FF#),
                  driverPID => msg.words (2));
            end if;
         end if;
      end loop;

      msg := NULL_MESSAGE;
      msg.tag := (label => CuBit.Devices.OP_XHCI_DIAGNOSTICS,
                  length => 0, flags => 0, badge => 0);
      tag := capCall (CAP_SLOT_DEVICE_INSPECTION, msg);
      if tag.label = CuBit.Devices.REPLY_OK then
         modeRep := Shift_Right (msg.words (3), 48) and 16#FF#;
         diagnostic :=
           (valid => (Shift_Right (msg.words (3), 56) and 1) /= 0,
            decodedReports => msg.words (0),
            motionReports => msg.words (1),
            buttonTransitions => Unsigned_32
              (msg.words (2) and 16#FFFF_FFFF#),
            completionErrors => Unsigned_32
              (Shift_Right (msg.words (2), 32)),
            lastReport => Unsigned_32 (msg.words (3) and 16#FFFF_FFFF#),
            lastLength => Unsigned_8
              (Shift_Right (msg.words (3), 32) and 16#FF#),
            lastCompletion => Unsigned_8
              (Shift_Right (msg.words (3), 40) and 16#FF#),
            interruptMode =>
              (if modeRep = 1 then CuBit.Devices.Interrupt_MSI
               elsif modeRep = 2 then CuBit.Devices.Interrupt_MSIX
               else CuBit.Devices.Interrupt_Polling));
      else
         diagnostic := (others => <>);
      end if;
   end Load_Inventory;

   function XHCI_Index return Natural is
   begin
      for index in 1 .. deviceCount loop
         if devices (index).kind = CuBit.Devices.USB_Controller then
            return index;
         end if;
      end loop;
      return 0;
   end XHCI_Index;

   procedure Rebuild_Visible_Keys is
      procedure Add (key : Natural) is
      begin
         visibleKeyCount := visibleKeyCount + 1;
         visibleKeys (visibleKeyCount) := key;
      end Add;
      usbIndex : Natural;
   begin
      visibleKeyCount := 0;
      visibleKeys := (others => 0);
      Add (NODE_COMPUTER);
      if computerExpanded then
         Add (NODE_PLATFORM);
         if platformExpanded then
            Add (NODE_CPU);
            Add (NODE_MEMORY);
         end if;
         Add (NODE_PCI);
         if pciExpanded then
            for index in 1 .. deviceCount loop
               Add (NODE_PCI_FIRST + index - 1);
               if devices (index).kind = CuBit.Devices.USB_Controller and then
                  xhciExpanded and then diagnostic.valid
               then
                  Add (NODE_USB_MOUSE);
               end if;
            end loop;
         end if;
         Add (NODE_INPUT);
         if inputExpanded then
            Add (NODE_PS2);
         end if;
      end if;
      usbIndex := XHCI_Index;
      if usbIndex = 0 and then selectedNode = NODE_USB_MOUSE then
         selectedNode := NODE_COMPUTER;
      end if;
   end Rebuild_Visible_Keys;

   procedure Toggle_Selected is
      index : Natural;
   begin
      case selectedNode is
         when NODE_COMPUTER => computerExpanded := not computerExpanded;
         when NODE_PLATFORM => platformExpanded := not platformExpanded;
         when NODE_PCI      => pciExpanded := not pciExpanded;
         when NODE_INPUT    => inputExpanded := not inputExpanded;
         when others =>
            if selectedNode >= NODE_PCI_FIRST and then
               selectedNode < NODE_PCI_FIRST + deviceCount
            then
               index := selectedNode - NODE_PCI_FIRST + 1;
               if devices (index).kind = CuBit.Devices.USB_Controller and then
                  diagnostic.valid
               then
                  xhciExpanded := not xhciExpanded;
               end if;
            end if;
      end case;
      Rebuild_Visible_Keys;
   end Toggle_Selected;

   procedure Expand_Selected is
      index : Natural;
   begin
      case selectedNode is
         when NODE_COMPUTER => computerExpanded := True;
         when NODE_PLATFORM => platformExpanded := True;
         when NODE_PCI      => pciExpanded := True;
         when NODE_INPUT    => inputExpanded := True;
         when others =>
            if selectedNode >= NODE_PCI_FIRST and then
               selectedNode < NODE_PCI_FIRST + deviceCount
            then
               index := selectedNode - NODE_PCI_FIRST + 1;
               if devices (index).kind = CuBit.Devices.USB_Controller and then
                  diagnostic.valid
               then
                  xhciExpanded := True;
               end if;
            end if;
      end case;
      Rebuild_Visible_Keys;
   end Expand_Selected;

   procedure Collapse_Or_Select_Parent is
      index : Natural;
   begin
      case selectedNode is
         when NODE_COMPUTER => computerExpanded := False;
         when NODE_PLATFORM =>
            if platformExpanded then
               platformExpanded := False;
            else
               selectedNode := NODE_COMPUTER;
            end if;
         when NODE_PCI =>
            if pciExpanded then
               pciExpanded := False;
            else
               selectedNode := NODE_COMPUTER;
            end if;
         when NODE_INPUT =>
            if inputExpanded then
               inputExpanded := False;
            else
               selectedNode := NODE_COMPUTER;
            end if;
         when NODE_CPU | NODE_MEMORY => selectedNode := NODE_PLATFORM;
         when NODE_PS2 => selectedNode := NODE_INPUT;
         when NODE_USB_MOUSE =>
            index := XHCI_Index;
            if index > 0 then selectedNode := NODE_PCI_FIRST + index - 1; end if;
         when others =>
            if selectedNode >= NODE_PCI_FIRST and then
               selectedNode < NODE_PCI_FIRST + deviceCount
            then
               index := selectedNode - NODE_PCI_FIRST + 1;
               if devices (index).kind = CuBit.Devices.USB_Controller and then
                  diagnostic.valid and then xhciExpanded
               then
                  xhciExpanded := False;
               else
                  selectedNode := NODE_PCI;
               end if;
            end if;
      end case;
      Rebuild_Visible_Keys;
   end Collapse_Or_Select_Parent;

   procedure Move_Selection (downward : Boolean) is
      position : Natural := 0;
   begin
      Rebuild_Visible_Keys;
      for index in 1 .. visibleKeyCount loop
         if visibleKeys (index) = selectedNode then
            position := index;
            exit;
         end if;
      end loop;
      if position = 0 and then visibleKeyCount > 0 then
         selectedNode := visibleKeys (1);
      elsif downward and then position < visibleKeyCount then
         selectedNode := visibleKeys (position + 1);
      elsif not downward and then position > 1 then
         selectedNode := visibleKeys (position - 1);
      end if;
   end Move_Selection;

   procedure Ensure_Selected_Visible (visibleRows : Natural) is
      position : Natural := 0;
   begin
      for index in 1 .. visibleKeyCount loop
         if visibleKeys (index) = selectedNode then
            position := index;
            exit;
         end if;
      end loop;
      if position > 0 then
         if position <= treeScroll then
            treeScroll := position - 1;
         elsif position > treeScroll + visibleRows then
            treeScroll := position - visibleRows;
         end if;
      end if;
   end Ensure_Selected_Visible;

   procedure Draw_Details
      (c : CuBit.UI.Canvas; bounds : CuBit.UI.Rect)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.CuBit_Alloy;
      content : CuBit.UI.Rect;
      y : Natural;
      index : Natural := 0;
      badgeBounds : CuBit.UI.Rect;

      procedure Pair (key, value : String) is
      begin
         if y + 22 <= content.y + content.h then
            CuBit.UI.Widgets.Key_Value
              (c, (x => content.x, y => y, w => content.w, h => 20),
               colors, key, value);
            y := y + 25;
         end if;
      end Pair;
   begin
      CuBit.UI.Widgets.Group_Box (c, bounds, colors, "Details", content, 12);
      y := content.y;
      if selectedNode >= NODE_PCI_FIRST and then
         selectedNode < NODE_PCI_FIRST + deviceCount
      then
         index := selectedNode - NODE_PCI_FIRST + 1;
      end if;

      if index > 0 then
         CuBit.UI.Labels.Label
           (c, (x => content.x, y => y, w => content.w, h => 24), colors,
            Kind_Name (devices (index).kind));
         y := y + 34;
         badgeBounds := (x => content.x, y => y, w => 90, h => 20);
         CuBit.UI.Widgets.Badge
           (c, badgeBounds, colors, State_Name (devices (index).state),
            (if devices (index).state = CuBit.Devices.Driver_Active
             then CuBit.UI.Widgets.Badge_Good
             elsif devices (index).state = CuBit.Devices.Driver_Failed
             then CuBit.UI.Widgets.Badge_Danger
             else CuBit.UI.Widgets.Badge_Neutral));
         y := y + 34;
         Pair ("Location", "PCI " &
               Hex (Unsigned_64 (devices (index).bus), 2) & ":" &
               Hex (Unsigned_64 (devices (index).slot), 2) & "." &
               Hex (Unsigned_64 (devices (index).functionN), 1));
         Pair ("Vendor / device",
               Hex (Unsigned_64 (devices (index).vendorID), 4) & ":" &
               Hex (Unsigned_64 (devices (index).deviceID), 4));
         Pair ("Class / interface",
               Hex (Unsigned_64 (devices (index).classCode), 4) & " / " &
               Hex (Unsigned_64 (devices (index).progIf), 2));
         Pair ("Driver process", (if devices (index).driverPID = 0 then
               "None" else "PID " & Decimal (devices (index).driverPID)));
         Pair ("Authority", "Inspection only");
         if devices (index).kind = CuBit.Devices.USB_Controller then
            Pair ("Interrupt delivery", Interrupt_Name (diagnostic.interruptMode));
            Pair ("HID reports", Decimal (diagnostic.decodedReports));
            Pair ("Completion errors", Decimal
                  (Unsigned_64 (diagnostic.completionErrors)));
         end if;
      elsif selectedNode = NODE_USB_MOUSE then
         CuBit.UI.Labels.Label
           (c, (x => content.x, y => y, w => content.w, h => 24), colors,
            "USB boot mouse");
         y := y + 34;
         CuBit.UI.Widgets.Badge
           (c, (x => content.x, y => y, w => 74, h => 20), colors,
            (if diagnostic.valid then "Active" else "Waiting"),
            (if diagnostic.valid then CuBit.UI.Widgets.Badge_Good
             else CuBit.UI.Widgets.Badge_Neutral));
         y := y + 34;
         Pair ("Decoded reports", Decimal (diagnostic.decodedReports));
         Pair ("Motion reports", Decimal (diagnostic.motionReports));
         Pair ("Button transitions", Decimal
               (Unsigned_64 (diagnostic.buttonTransitions)));
         Pair ("Completion errors", Decimal
               (Unsigned_64 (diagnostic.completionErrors)));
         Pair ("Last report", "0x" &
               Hex (Unsigned_64 (diagnostic.lastReport), 8));
         Pair ("Report length", Decimal (Unsigned_64 (diagnostic.lastLength)));
         Pair ("Completion code", Decimal
               (Unsigned_64 (diagnostic.lastCompletion)));
         Pair ("Interrupt delivery", Interrupt_Name (diagnostic.interruptMode));
      elsif selectedNode = NODE_CPU then
         CuBit.UI.Labels.Label
           (c, (x => content.x, y => y, w => content.w, h => 24), colors,
            "Processors");
         y := y + 34;
         Pair ("Logical processors", Decimal (getInfo (SYSINFO_NUM_CPUS)));
         Pair ("Architecture", "x86-64");
         Pair ("Authority", "Inspection only");
      elsif selectedNode = NODE_MEMORY then
         CuBit.UI.Labels.Label
           (c, (x => content.x, y => y, w => content.w, h => 24), colors,
            "Physical memory");
         y := y + 34;
         Pair ("Total", Decimal (getInfo (1601) / (1024 * 1024)) & " MiB");
         Pair ("Available", Decimal (getInfo (1600) / (1024 * 1024)) & " MiB");
         Pair ("Authority", "Inspection only");
      elsif selectedNode = NODE_PS2 then
         CuBit.UI.Labels.Label
           (c, (x => content.x, y => y, w => content.w, h => 24), colors,
            "Legacy input controller");
         y := y + 34;
         Pair ("Driver", "ps2.drv");
         Pair ("Keyboard route", "desktop session");
         Pair ("Pointer route", "desktop session");
         Pair ("Authority", "Inspection only");
      else
         CuBit.UI.Labels.Label
           (c, (x => content.x, y => y, w => content.w, h => 24), colors,
            (if selectedNode = NODE_PCI then "PCI bus"
             elsif selectedNode = NODE_PLATFORM then "Platform"
             elsif selectedNode = NODE_INPUT then "Input devices"
             else "This computer"));
         y := y + 34;
         Pair ("Inventory source", "devmgr.svc");
         Pair ("PCI functions", Decimal (Unsigned_64 (deviceCount)));
         Pair ("Access", "Read-only snapshot");
         Pair ("Policy", "No ambient hardware authority");
      end if;
   end Draw_Details;

   procedure Render
      (win : in out CuBit.UI.App.Window; damage : CuBit.UI.Rect)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.CuBit_Alloy;
      c : constant CuBit.UI.Canvas := CuBit.UI.App.Canvas (win, damage);
      full : constant CuBit.UI.Rect := CuBit.UI.App.Full_Rect (win);
      toolbar : CuBit.UI.Rect;
      status : CuBit.UI.Rect;
      workspace : CuBit.UI.Rect;
      treePane, detailPane : CuBit.UI.Rect;
      treeContent, frameContent : CuBit.UI.Rect;
      treeViewport, scrollBounds : CuBit.UI.Rect;
      refreshResult, scrollResult, itemResult : CuBit.UI.Widget_Result;
      visibleRows, maxScroll : Natural := 0;
      rowOrdinal : Natural := 0;
      drawnRows : Natural := 0;

      procedure Item
         (key : Natural; label : String; depth : Natural;
          hasChildren, expanded : Boolean;
          icon : CuBit.UI.Trees.Tree_Item_Icon;
          lastSibling : Boolean := False;
          ancestors : Unsigned_64 := 0)
      is
         row : CuBit.UI.Rect;
      begin
         rowOrdinal := rowOrdinal + 1;
         if rowOrdinal > treeScroll and then drawnRows < visibleRows then
            row := (x => treeViewport.x,
                    y => treeViewport.y + drawnRows * CuBit.UI.Trees.TREE_ROW_HEIGHT,
                    w => treeViewport.w,
                    h => CuBit.UI.Trees.TREE_ROW_HEIGHT);
            CuBit.UI.Trees.Tree_Item
              (c, ui, controls, CONTROL_TREE_FIRST + drawnRows,
               row, row, colors, label, key, selectedNode,
               depth, expanded, hasChildren, icon, treeFocused,
               lastSibling, ancestors, itemResult, retainedInput => True);
            drawnRows := drawnRows + 1;
         end if;
      end Item;
   begin
      CuBit.UI.State.Begin_Frame (ui);
      CuBit.UI.Controls.Clear (controls);
      CuBit.UI.Fill_Rect (c, full, colors.face);

      toolbar := (x => 8, y => 8, w => full.w - 16, h => 38);
      status := (x => 8, y => full.h - 30, w => full.w - 16, h => 22);
      workspace := (x => 8, y => 52, w => full.w - 16, h => full.h - 88);
      CuBit.UI.Widgets.Toolbar (c, toolbar, colors);
      CuBit.UI.Widgets.Button
         (c, ui, controls, CONTROL_REFRESH,
         (x => toolbar.x + 7, y => toolbar.y + 6, w => 82, h => 26),
         toolbar, colors, "Refresh", refreshResult, retainedInput => True);
      CuBit.UI.Labels.Label
        (c, (x => toolbar.x + 105, y => toolbar.y + 9,
             w => toolbar.w - 220, h => 20), colors,
         "Unified hardware inventory and diagnostics", muted => True);
      CuBit.UI.Widgets.Badge
        (c, (x => toolbar.x + toolbar.w - 105, y => toolbar.y + 8,
             w => 96, h => 21), colors, "Inspect only",
         CuBit.UI.Widgets.Badge_Good);

      CuBit.UI.Widgets.Split_Pane
        (c, ui, controls, CONTROL_SPLITTER, workspace, workspace, colors,
         vertical => True, position => splitPosition,
         first => treePane, second => detailPane,
         splitterSize => 7, minFirst => 245, minSecond => 320,
         retainedInput => True);
      CuBit.UI.Widgets.Group_Box
        (c, treePane, colors, "Hardware", treeContent, 8);
      CuBit.UI.Trees.View_Frame
        (c, treeContent, colors, treeFocused, frameContent);
      lastTreeBounds := treeContent;

      if frameContent.w > 17 then
         treeViewport :=
           (x => frameContent.x, y => frameContent.y,
            w => frameContent.w - 16, h => frameContent.h);
         scrollBounds :=
           (x => frameContent.x + frameContent.w - 14,
            y => frameContent.y, w => 14, h => frameContent.h);
      else
         treeViewport := frameContent;
         scrollBounds := (others => 0);
      end if;
      visibleRows := treeViewport.h / CuBit.UI.Trees.TREE_ROW_HEIGHT;
      Rebuild_Visible_Keys;
      if visibleKeyCount > visibleRows then
         maxScroll := visibleKeyCount - visibleRows;
      end if;
      treeScroll := Natural'Min (treeScroll, maxScroll);
      Item (NODE_COMPUTER, "This computer", 0, True, computerExpanded,
            CuBit.UI.Trees.Computer_Icon);
      if computerExpanded then
         Item (NODE_PLATFORM, "Platform", 1, True, platformExpanded,
               CuBit.UI.Trees.Bus_Icon, ancestors => 1);
         if platformExpanded then
            Item (NODE_CPU, "Processors", 2, False, False,
                  CuBit.UI.Trees.Device_Icon, ancestors => 3);
            Item (NODE_MEMORY, "Physical memory", 2, False, False,
                  CuBit.UI.Trees.Device_Icon, True, ancestors => 1);
         end if;
         Item (NODE_PCI,
               "PCI bus (" & Decimal (Unsigned_64 (deviceCount)) & ")",
               1, True, pciExpanded, CuBit.UI.Trees.Bus_Icon,
               ancestors => 1);
         if pciExpanded then
            for index in 1 .. deviceCount loop
               Item
                 (NODE_PCI_FIRST + index - 1, Device_Label (devices (index)),
                  2,
                  devices (index).kind = CuBit.Devices.USB_Controller and then
                    diagnostic.valid,
                  xhciExpanded, Kind_Icon (devices (index).kind),
                  lastSibling => index = deviceCount,
                  ancestors => 3);
               if devices (index).kind = CuBit.Devices.USB_Controller and then
                  diagnostic.valid and then xhciExpanded
               then
                  Item (NODE_USB_MOUSE, "USB boot mouse", 3, False, False,
                        CuBit.UI.Trees.Input_Icon, True, ancestors => 3);
               end if;
            end loop;
         end if;
         Item (NODE_INPUT, "Legacy input", 1, True, inputExpanded,
               CuBit.UI.Trees.Input_Icon, True);
         if inputExpanded then
            Item (NODE_PS2, "PS/2 controller", 2, False, False,
                  CuBit.UI.Trees.Input_Icon, True);
         end if;
      end if;

      if maxScroll > 0 then
         CuBit.UI.Widgets.Vertical_Scrollbar
           (c, ui, controls, CONTROL_SCROLLBAR, scrollBounds, treeContent,
            colors, 0, maxScroll, treeScroll, scrollResult,
            retainedInput => True);
      end if;
      Draw_Details (c, detailPane);

      CuBit.UI.Draw_Status_Bar
        (c, status, colors,
         (if inventoryAvailable
          then "Ready. Arrow keys navigate; Left/Right collapse or expand."
          else "Device inventory authority unavailable."),
         (if inventoryOverflow then "inventory truncated"
          elsif diagnostic.valid then "live USB diagnostics"
          else "snapshot"));
      CuBit.UI.State.Finish_Frame (ui);
   end Render;

   procedure Handle_Event
      (win : in out CuBit.UI.App.Window;
       event : CuBit.UI.App.Input_Event;
       dirty : in out CuBit.UI.Rect;
       running : in out Boolean)
   is
      x, y : Natural;
      wheel : Integer;
      maxScroll : Natural := 0;
      visibleRows : Natural := 1;
      hit : CuBit.UI.Controls.Control_ID;
      selectionMoved : Boolean := False;
      oldScroll : Natural;
   begin
      if event.kind = CuBit.UI.App.INPUT_KEY_DOWN then
         if event.payload0 = CuBit.UI.App.KEY_ESC then
            running := False;
         elsif event.payload0 = KEY_F5 then
            Load_Inventory;
            dirty := CuBit.UI.App.Full_Rect (win);
         elsif treeFocused and then event.payload0 = KEY_UP then
            Move_Selection (False);
            selectionMoved := True;
         elsif treeFocused and then event.payload0 = KEY_DOWN then
            Move_Selection (True);
            selectionMoved := True;
         elsif treeFocused and then event.payload0 = KEY_RIGHT then
            Expand_Selected;
            selectionMoved := True;
         elsif treeFocused and then event.payload0 = KEY_LEFT then
            Collapse_Or_Select_Parent;
            selectionMoved := True;
         elsif treeFocused and then event.payload0 = KEY_HOME then
            Rebuild_Visible_Keys;
            if visibleKeyCount > 0 then selectedNode := visibleKeys (1); end if;
            selectionMoved := True;
         elsif treeFocused and then event.payload0 = KEY_END then
            Rebuild_Visible_Keys;
            if visibleKeyCount > 0 then
               selectedNode := visibleKeys (visibleKeyCount);
            end if;
            selectionMoved := True;
         end if;
         if selectionMoved then
            Rebuild_Visible_Keys;
            if lastTreeBounds.h > 8 then
               visibleRows := Natural'Max
                 (1, (lastTreeBounds.h - 8) /
                    CuBit.UI.Trees.TREE_ROW_HEIGHT);
            end if;
            Ensure_Selected_Visible (visibleRows);
            dirty := CuBit.UI.App.Full_Rect (win);
         end if;
      elsif event.kind = CuBit.UI.App.INPUT_CONFIGURE then
         dirty := CuBit.UI.App.Full_Rect (win);
      elsif event.kind = CuBit.UI.App.INPUT_POINTER_DOWN then
         x := Natural (event.payload0 and 16#FFFF_FFFF#);
         y := Natural (Shift_Right (event.payload0, 32));
         if treeFocused /= CuBit.UI.Point_In_Rect (x, y, lastTreeBounds) then
            treeFocused := CuBit.UI.Point_In_Rect (x, y, lastTreeBounds);
            dirty := CuBit.UI.Union_Rect (dirty, lastTreeBounds);
         end if;
      elsif event.kind = CuBit.UI.App.INPUT_POINTER_UP then
         x := Natural (event.payload0 and 16#FFFF_FFFF#);
         y := Natural (Shift_Right (event.payload0, 32));
         hit := CuBit.UI.Controls.Hit (controls, x, y);
         if hit = CONTROL_REFRESH and then
           CuBit.UI.Controls.Take_Activated (controls, hit)
         then
            Load_Inventory;
            dirty := CuBit.UI.App.Full_Rect (win);
         elsif hit >= CONTROL_TREE_FIRST and then
           CuBit.UI.Controls.Take_Activated (controls, hit)
         then
            declare
               ordinal : constant Natural :=
                 treeScroll + (hit - CONTROL_TREE_FIRST) + 1;
            begin
               Rebuild_Visible_Keys;
               if ordinal <= visibleKeyCount then
                  selectedNode := visibleKeys (ordinal);
                  --  Leaf nodes make this a no-op; expandable nodes retain
                  --  the existing one-click selection/disclosure behavior.
                  Toggle_Selected;
                  dirty := CuBit.UI.App.Full_Rect (win);
               end if;
            end;
         end if;
      elsif event.kind = CuBit.UI.App.INPUT_POINTER_WHEEL and then treeFocused
      then
         wheel := CuBit.UI.App.Pointer_Wheel_Delta (event);
         Rebuild_Visible_Keys;
         if lastTreeBounds.h > 8 then
            visibleRows := Natural'Max
              (1, (lastTreeBounds.h - 8) /
                 CuBit.UI.Trees.TREE_ROW_HEIGHT);
         end if;
         if visibleKeyCount > visibleRows then
            maxScroll := visibleKeyCount - visibleRows;
         end if;
         oldScroll := treeScroll;
         if wheel > 0 then
            if treeScroll > 0 then treeScroll := treeScroll - 1; end if;
         elsif wheel < 0 then
            treeScroll := Natural'Min (treeScroll + 1, maxScroll);
         end if;
         if treeScroll /= oldScroll then
            dirty := CuBit.UI.Union_Rect (dirty, lastTreeBounds);
         end if;
      end if;
   end Handle_Event;

   procedure Run_UI is new CuBit.UI.App.Run
     (ui => ui, controls => controls,
      Render => Render, Handle_Event => Handle_Event);

begin
   debugPrint ("devices: starting read-only hardware inspector" & LF);
   Load_Inventory;
   declare
      ok : Boolean;
      flags : constant Unsigned_64 :=
        CuBit.UI.App.WINDOW_FLAG_DECORATED or
        CuBit.UI.App.WINDOW_FLAG_RESIZABLE or
        CuBit.UI.App.WINDOW_FLAG_MINIMIZABLE or
        CuBit.UI.App.WINDOW_FLAG_MAXIMIZABLE or
        CuBit.UI.App.WINDOW_FLAG_CLOSEABLE;
   begin
      CuBit.UI.App.Open
        (win, INITIAL_WIDTH, INITIAL_HEIGHT, flags, ok, title => "Devices");
      if not ok then
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
      debugPrint ("devices: native window ready" & LF);
   end;
   Run_UI (win);
   CuBit.UI.App.Close (win);
   ignore := syscall (SYSCALL_EXIT, 0);
end main;
