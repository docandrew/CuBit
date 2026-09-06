------------------------------------------------------------------------------
--  CuBit Files
--  Read-only native browser for explicitly granted filesystem roots.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Labels;
with CuBit.UI.State;
with CuBit.UI.Tables;
with CuBit.UI.Widgets;

procedure main is
   use ASCII;
   use type CuBit.UI.Scrollbar_Part;

   INITIAL_WIDTH : constant Positive := 860;
   INITIAL_HEIGHT : constant Positive := 540;
   PAGE_SIZE : constant Unsigned_64 := 4096;
   CAP_SLOT_FILESYSTEM : constant CapabilitySlot := 22;
   MAXIMUM_ITEMS : constant Positive := 128;
   ROW_HEIGHT : constant Positive := 24;

   CONTROL_REFRESH : constant CuBit.UI.Controls.Control_ID := 1;
   CONTROL_SCROLLBAR : constant CuBit.UI.Controls.Control_ID := 2;
   CONTROL_FIRST_COLUMN : constant CuBit.UI.Controls.Control_ID := 3;
   CONTROL_SECOND_COLUMN : constant CuBit.UI.Controls.Control_ID := 4;
   CONTROL_ROW_FIRST : constant CuBit.UI.Controls.Control_ID := 100;

   type Local_Item is record
      name : String (1 .. MAXIMUM_DIRECTORY_NAME_BYTES);
      nameLength : Natural range 0 .. MAXIMUM_DIRECTORY_NAME_BYTES := 0;
      kind : Unsigned_8 := DIRECTORY_KIND_UNKNOWN;
      flags : Unsigned_8 := 0;
      sizeBytes : Unsigned_64 := 0;
   end record;
   type Item_Array is array (Positive range 1 .. MAXIMUM_ITEMS) of Local_Item;

   win : CuBit.UI.App.Window;
   ui : CuBit.UI.State.UI_State;
   controls : CuBit.UI.Controls.Control_Map;
   items : Item_Array;
   itemCount : Natural range 0 .. MAXIMUM_ITEMS := 0;
   selectedItem : Natural range 0 .. MAXIMUM_ITEMS := 0;
   scrollRow : Natural := 0;
   lastListBounds : CuBit.UI.Rect := (others => 0);
   sourceName : String (1 .. 16) := (others => ' ');
   sourceNameLength : Natural range 0 .. sourceName'Length := 0;
   loadSucceeded : Boolean := False;
   listTruncated : Boolean := False;
   rawBuffer : Unsigned_64 := 0;
   pageAddress : Unsigned_64 := 0;
   pageGrant : CuBit.Memory_Grants.Grant_Reference;
   pageGrantReady : Boolean := False;
   firstFrameLogged : Boolean := False;
   firstWheelLogged : Boolean := False;
   firstScrollbarLogged : Boolean := False;
   firstScrollbarDragLogged : Boolean := False;
   ignore : Unsigned_64;
   tableColumns : CuBit.UI.Table_Column_Layout :=
     (First_Width => 480, Second_Width => 130, Cell_Padding => 7);
   type Column_Drag_State is
     (No_Column_Drag, First_Column_Drag, Second_Column_Drag);
   activeColumnDrag : Column_Drag_State := No_Column_Drag;

   KEY_UP : constant Unsigned_64 := 16#48#;
   KEY_DOWN : constant Unsigned_64 := 16#50#;
   KEY_HOME : constant Unsigned_64 := 16#47#;
   KEY_PAGE_UP : constant Unsigned_64 := 16#49#;
   KEY_END : constant Unsigned_64 := 16#4F#;
   KEY_PAGE_DOWN : constant Unsigned_64 := 16#51#;
   KEY_F5 : constant Unsigned_64 := 16#3F#;

   function Decimal (value : Unsigned_64) return String is
      raw : constant String := Unsigned_64'Image (value);
   begin
      return raw (raw'First + 1 .. raw'Last);
   end Decimal;

   function Size_Label (item : Local_Item) return String is
      value : constant Unsigned_64 := item.sizeBytes;
   begin
      if (item.flags and DIRECTORY_ENTRY_SIZE_VALID) = 0 then
         return "--";
      end if;
      if value >= 1024 * 1024 then
         return Decimal (value / (1024 * 1024)) & " MiB";
      elsif value >= 1024 then
         return Decimal (value / 1024) & " KiB";
      else
         return Decimal (value) & " bytes";
      end if;
   end Size_Label;

   function Kind_Label (kind : Unsigned_8) return String is
   begin
      case kind is
         when DIRECTORY_KIND_FILE => return "File";
         when DIRECTORY_KIND_DIRECTORY => return "Folder";
         when DIRECTORY_KIND_SYMLINK => return "Link";
         when others => return "Object";
      end case;
   end Kind_Label;

   procedure Set_Source_Name (value : String) is
   begin
      sourceNameLength := Natural'Min (value'Length, sourceName'Length);
      sourceName := (others => ' ');
      for index in 1 .. sourceNameLength loop
         sourceName (index) := value (value'First + index - 1);
      end loop;
   end Set_Source_Name;

   procedure Load_From (path : String; success : out Boolean) is
      msg : Message;
      tag : MessageTag;
      directory : Directory_Handle;
      previousCursor : Unsigned_64 := 0;
      pageValid : Boolean;
   begin
      success := False;
      declare
         pathView : String (path'Range)
           with Import, Address => To_Address (Integer_Address (pageAddress));
      begin
         pathView := path;
      end;

      msg := Open_Directory_Request (pageGrant, path'Length);
      tag := capCall (CAP_SLOT_FILESYSTEM, msg);
      if tag.label /= REPLY_OK then
         return;
      end if;
      directory := Directory_Handle (msg.words (0));

      loop
         msg := Read_Directory_Page_Request (directory, pageGrant);
         tag := capCall (CAP_SLOT_FILESYSTEM, msg);
         if tag.label /= REPLY_OK then
            exit;
         end if;

         declare
            address : constant System.Address :=
              To_Address (Integer_Address (pageAddress));
            header : Directory_Page_Header
              with Import, Address => address;
            pageItems : Directory_Entries
              with Import,
                   Address => address + DIRECTORY_PAGE_HEADER_BYTES;
         begin
            pageValid := True;
            if header.version /= PROTOCOL_VERSION or else
              header.headerBytes /= DIRECTORY_PAGE_HEADER_BYTES or else
              header.entryBytes /= DIRECTORY_ENTRY_BYTES or else
              header.entryCount > MAXIMUM_DIRECTORY_PAGE_ENTRIES
            then
               pageValid := False;
            end if;

            if pageValid and then header.entryCount > 0 then
               for pageIndex in 0 .. Natural (header.entryCount) - 1 loop
                  if pageItems (pageIndex).nameLength >
                    MAXIMUM_DIRECTORY_NAME_BYTES
                  then
                     pageValid := False;
                     exit;
                  elsif itemCount = MAXIMUM_ITEMS then
                     listTruncated := True;
                     exit;
                  else
                     itemCount := itemCount + 1;
                     items (itemCount).nameLength :=
                       Natural (pageItems (pageIndex).nameLength);
                     items (itemCount).kind := pageItems (pageIndex).kind;
                     items (itemCount).flags := pageItems (pageIndex).flags;
                     items (itemCount).sizeBytes :=
                       pageItems (pageIndex).sizeBytes;
                     for nameIndex in 1 .. items (itemCount).nameLength loop
                        items (itemCount).name (nameIndex) := Character'Val
                          (pageItems (pageIndex).name (nameIndex));
                     end loop;
                  end if;
               end loop;
            end if;

            if pageValid and then
              (header.flags and DIRECTORY_PAGE_END) = 0 and then
              header.nextCursor <= previousCursor
            then
               pageValid := False;
            end if;

            if not pageValid then
               exit;
            end if;
            previousCursor := header.nextCursor;

            if listTruncated or else
              (header.flags and DIRECTORY_PAGE_END) /= 0
            then
               success := True;
               exit;
            end if;
         end;
      end loop;

      msg := Close_Directory_Request (directory);
      tag := capCall (CAP_SLOT_FILESYSTEM, msg);
      success := success and then tag.label = REPLY_OK;
   end Load_From;

   procedure Load_Directory is
      loaded : Boolean;
   begin
      itemCount := 0;
      selectedItem := 0;
      scrollRow := 0;
      listTruncated := False;
      loadSucceeded := False;
      if not pageGrantReady then
         return;
      end if;

      Load_From ("@nvme:0/", loaded);
      if loaded then
         Set_Source_Name ("NVMe volume 0");
         loadSucceeded := True;
         return;
      end if;

      itemCount := 0;
      Load_From ("@mem:0/", loaded);
      if loaded then
         Set_Source_Name ("Live workspace");
         loadSucceeded := True;
      else
         Set_Source_Name ("Unavailable");
      end if;
   end Load_Directory;

   procedure Ensure_Selected_Visible (visibleRows : Natural) is
   begin
      if selectedItem > 0 then
         if selectedItem <= scrollRow then
            scrollRow := selectedItem - 1;
         elsif selectedItem > scrollRow + visibleRows then
            scrollRow := selectedItem - visibleRows;
         end if;
      end if;
   end Ensure_Selected_Visible;

   procedure Render
     (win : in out CuBit.UI.App.Window; damage : CuBit.UI.Rect)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.CuBit_Alloy;
      c : constant CuBit.UI.Canvas := CuBit.UI.App.Canvas (win, damage);
      full : constant CuBit.UI.Rect := CuBit.UI.App.Full_Rect (win);
      toolbar : CuBit.UI.Rect;
      status : CuBit.UI.Rect;
      table : CuBit.UI.Rect;
      regions : CuBit.UI.Table_Regions;
      rowBounds : CuBit.UI.Rect;
      scrollBounds : CuBit.UI.Rect;
      refreshResult, rowResult, scrollResult : CuBit.UI.Widget_Result;
      visibleRows : Natural;
      maximumScroll : Natural := 0;
      previousScroll : Natural;
   begin
      CuBit.UI.State.Begin_Frame (ui);
      CuBit.UI.Controls.Clear (controls);
      CuBit.UI.Fill_Rect (c, full, colors.face);

      toolbar := (x => 8, y => 8, w => full.w - 16, h => 38);
      status := (x => 8, y => full.h - 30, w => full.w - 16, h => 22);
      table := (x => 8, y => 52, w => full.w - 16, h => full.h - 88);
      CuBit.UI.Widgets.Toolbar (c, toolbar, colors);
      CuBit.UI.Widgets.Button
        (c, ui, controls, CONTROL_REFRESH,
         (x => toolbar.x + 7, y => toolbar.y + 6, w => 82, h => 26),
         toolbar, colors, "Refresh", refreshResult);
      CuBit.UI.Labels.Label
        (c, (x => toolbar.x + 105, y => toolbar.y + 9,
             w => toolbar.w - 220, h => 20), colors,
         (if sourceNameLength = 0 then "No filesystem root" else
            sourceName (1 .. sourceNameLength)), muted => True);
      CuBit.UI.Widgets.Badge
        (c, (x => toolbar.x + toolbar.w - 105, y => toolbar.y + 8,
             w => 96, h => 21), colors, "Read only",
         CuBit.UI.Widgets.Badge_Good);

      CuBit.UI.Draw_Table_Viewport (c, table, colors);
      regions := CuBit.UI.Layout_Table (table);
      lastListBounds := regions.Rows;
      CuBit.UI.Tables.Resizable_Header
        (c, ui, controls, CONTROL_FIRST_COLUMN, CONTROL_SECOND_COLUMN,
         regions.Header, table, colors, "Name", "Kind", "Size",
         tableColumns,
         minimumFirst => 120, minimumSecond => 72, minimumThird => 90);
      visibleRows := regions.Rows.h / ROW_HEIGHT;
      if itemCount > visibleRows then
         maximumScroll := itemCount - visibleRows;
      end if;
      scrollRow := Natural'Min (scrollRow, maximumScroll);

      --  Evaluate scrolling before drawing rows so a thumb drag, arrow click,
      --  or page-track click is visible in the same presented frame.
      if maximumScroll > 0 and then visibleRows > 0 then
         previousScroll := scrollRow;
         scrollBounds :=
           (x => regions.Rows.x + regions.Rows.w - 14,
            y => regions.Rows.y, w => 14, h => regions.Rows.h);
         CuBit.UI.Widgets.Vertical_Scrollbar
           (c, ui, controls, CONTROL_SCROLLBAR, scrollBounds, table,
            colors, 0, itemCount - 1, scrollRow, scrollResult,
            pageSize => Positive (visibleRows));
         if scrollRow /= previousScroll and then not firstScrollbarLogged then
            debugPrint
              ("files: scrollbar scroll row=" &
               Decimal (Unsigned_64 (scrollRow)) & LF);
            firstScrollbarLogged := True;
         end if;
         if scrollRow /= previousScroll and then
           CuBit.UI.State.Active_Scrollbar_Part (ui) =
             CuBit.UI.Scrollbar_Thumb and then
           not firstScrollbarDragLogged
         then
            debugPrint
              ("files: scrollbar thumb drag row=" &
               Decimal (Unsigned_64 (scrollRow)) & LF);
            firstScrollbarDragLogged := True;
         end if;
      end if;

      if visibleRows > 0 then
         for visibleIndex in 0 .. visibleRows - 1 loop
            declare
               itemIndex : constant Natural := scrollRow + visibleIndex + 1;
            begin
               exit when itemIndex > itemCount;
               rowBounds :=
                 (x => regions.Rows.x,
                  y => regions.Rows.y + visibleIndex * ROW_HEIGHT,
                  w => regions.Rows.w -
                    (if maximumScroll > 0 then 15 else 0),
                  h => ROW_HEIGHT);
               CuBit.UI.Controls.Add
                 (controls, CONTROL_ROW_FIRST + visibleIndex,
                  rowBounds, rowBounds);
               rowResult := CuBit.UI.State.Button
                 (ui,
                  CuBit.UI.Controls.Bounds
                    (controls, CONTROL_ROW_FIRST + visibleIndex),
                  CuBit.UI.State.Widget_ID
                    (CONTROL_ROW_FIRST + visibleIndex));
               if rowResult.activated then
                  selectedItem := itemIndex;
               end if;
               CuBit.UI.Draw_Table_Row
                 (c, rowBounds, colors, selectedItem = itemIndex,
                  rowResult.hot,
                  items (itemIndex).name
                    (1 .. items (itemIndex).nameLength),
                  Kind_Label (items (itemIndex).kind),
                  Size_Label (items (itemIndex)), tableColumns);
            end;
         end loop;
      end if;

      CuBit.UI.Draw_Status_Bar
        (c, status, colors,
         (if loadSucceeded then
            Decimal (Unsigned_64 (itemCount)) &
              (if itemCount = 1 then " object" else " objects")
          else "No granted filesystem root is available."),
         (if listTruncated then "list truncated" else
            "explicit read authority"));
      CuBit.UI.State.Finish_Frame (ui);
      if not firstFrameLogged then
         debugPrint ("files: first frame presented" & LF);
         firstFrameLogged := True;
      end if;
   end Render;

   procedure Handle_Event
     (win : in out CuBit.UI.App.Window;
      event : CuBit.UI.App.Input_Event;
      dirty : in out CuBit.UI.Rect;
      running : in out Boolean)
   is
      wheel : Integer;
      visibleRows : Natural := 1;
      maximumScroll : Natural := 0;
      x, y : Natural;
      hit : CuBit.UI.Controls.Control_ID;
      selectionMoved : Boolean := False;
      oldScroll : Natural;
   begin
      if event.kind = CuBit.UI.App.INPUT_KEY_DOWN then
         if event.payload0 = CuBit.UI.App.KEY_ESC then
            running := False;
         elsif event.payload0 = KEY_F5 then
            Load_Directory;
            debugPrint ("files: refresh input received" & LF);
            dirty := CuBit.UI.App.Full_Rect (win);
         elsif event.payload0 = KEY_UP and then selectedItem > 1 then
            selectedItem := selectedItem - 1;
            selectionMoved := True;
         elsif event.payload0 = KEY_DOWN and then selectedItem < itemCount then
            selectedItem := selectedItem + 1;
            selectionMoved := True;
         elsif event.payload0 = KEY_HOME and then itemCount > 0 then
            selectedItem := 1;
            selectionMoved := True;
         elsif event.payload0 = KEY_END and then itemCount > 0 then
            selectedItem := itemCount;
            selectionMoved := True;
         elsif (event.payload0 = KEY_PAGE_UP or else
                event.payload0 = KEY_PAGE_DOWN) and then itemCount > 0
         then
            visibleRows := Natural'Max (1, lastListBounds.h / ROW_HEIGHT);
            if itemCount > visibleRows then
               maximumScroll := itemCount - visibleRows;
            end if;
            CuBit.UI.Apply_Wheel_Scroll
              (scrollRow, 0, maximumScroll,
               (if event.payload0 = KEY_PAGE_UP then 1 else -1),
               Positive (visibleRows));
            dirty := CuBit.UI.Union_Rect (dirty, lastListBounds);
         end if;
         if selectionMoved then
            visibleRows := Natural'Max (1, lastListBounds.h / ROW_HEIGHT);
            Ensure_Selected_Visible (visibleRows);
            dirty := CuBit.UI.Union_Rect (dirty, lastListBounds);
         end if;
      elsif event.kind = CuBit.UI.App.INPUT_CONFIGURE then
         dirty := CuBit.UI.App.Full_Rect (win);
      elsif event.kind = CuBit.UI.App.INPUT_RESYNC then
         activeColumnDrag := No_Column_Drag;
      elsif event.kind = CuBit.UI.App.INPUT_POINTER_DOWN then
         x := Natural (event.payload0 and 16#FFFF_FFFF#);
         y := Natural (Shift_Right (event.payload0, 32));
         hit := CuBit.UI.Controls.Hit (controls, x, y);
         case hit is
            when CONTROL_FIRST_COLUMN =>
               activeColumnDrag := First_Column_Drag;
            when CONTROL_SECOND_COLUMN =>
               activeColumnDrag := Second_Column_Drag;
            when others =>
               activeColumnDrag := No_Column_Drag;
         end case;
      elsif event.kind = CuBit.UI.App.INPUT_POINTER_UP then
         x := Natural (event.payload0 and 16#FFFF_FFFF#);
         y := Natural (Shift_Right (event.payload0, 32));
         hit := CuBit.UI.Controls.Hit (controls, x, y);
         if activeColumnDrag /= No_Column_Drag then
            debugPrint
              ("files: column resize complete first=" &
               Decimal (Unsigned_64 (tableColumns.First_Width)) &
               " second=" &
               Decimal (Unsigned_64 (tableColumns.Second_Width)) & LF);
         end if;
         activeColumnDrag := No_Column_Drag;
         if hit = CONTROL_REFRESH then
            Load_Directory;
            dirty := CuBit.UI.App.Full_Rect (win);
         end if;
      elsif event.kind = CuBit.UI.App.INPUT_POINTER_WHEEL and then
        CuBit.UI.Point_In_Rect
          (Natural (event.payload0 and 16#FFFF_FFFF#),
           Natural (Shift_Right (event.payload0, 32)), lastListBounds)
      then
         visibleRows := Natural'Max (1, lastListBounds.h / ROW_HEIGHT);
         if itemCount > visibleRows then
            maximumScroll := itemCount - visibleRows;
         end if;
         oldScroll := scrollRow;
         wheel := CuBit.UI.App.Pointer_Wheel_Delta (event);
         CuBit.UI.Apply_Wheel_Scroll
           (scrollRow, 0, maximumScroll, wheel);
         if scrollRow /= oldScroll then
            if not firstWheelLogged then
               debugPrint
                 ("files: wheel scroll row=" &
                  Decimal (Unsigned_64 (scrollRow)) & LF);
               firstWheelLogged := True;
            end if;
            dirty := CuBit.UI.Union_Rect (dirty, lastListBounds);
         end if;
      end if;
   end Handle_Event;

   procedure Run_UI is new CuBit.UI.App.Run
     (ui => ui, controls => controls,
      Render => Render, Handle_Event => Handle_Event);

begin
   debugPrint ("files: starting read-only filesystem browser" & LF);
   rawBuffer := syscall (SYSCALL_SBRK, 2 * PAGE_SIZE);
   if rawBuffer /= Unsigned_64'Last then
      pageAddress := (rawBuffer + PAGE_SIZE - 1) and not (PAGE_SIZE - 1);
      CuBit.Memory_Grants.Create_Via_Capability
        (slot => CAP_SLOT_FILESYSTEM,
         localAddr => To_Address (Integer_Address (pageAddress)),
         numPages => 1, readWrite => True,
         reference => pageGrant, success => pageGrantReady);
   end if;
   Load_Directory;
   if loadSucceeded then
      debugPrint ("files: directory page protocol ready" & LF);
   end if;

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
        (win, INITIAL_WIDTH, INITIAL_HEIGHT, flags, ok, title => "Files");
      if not ok then
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
      debugPrint ("files: native window ready" & LF);
   end;
   Run_UI (win);
   CuBit.UI.App.Close (win);
   if pageGrantReady then
      CuBit.Memory_Grants.Revoke (pageGrant, pageGrantReady);
   end if;
   ignore := syscall (SYSCALL_EXIT, 0);
end main;
