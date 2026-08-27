------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Security Center desktop client
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Authority; use CuBit.Authority;
with CuBit.UI;
with CuBit.UI.App;
with CuBit.UI.Controls;
with CuBit.UI.Labels;
with CuBit.UI.Layout;
with CuBit.UI.State;
with CuBit.UI.Tables;
with CuBit.UI.Trees;
with CuBit.UI.Widgets;
with Security_Center_Form;

procedure main is
   use ASCII;
   use type CuBit.UI.Widgets.Tab_Title_Access;

   SYSINFO_NUM_CPUS  : constant Unsigned_64 := 1400;
   SYSINFO_MEM_FREE  : constant Unsigned_64 := 1600;
   SYSINFO_MEM_TOTAL : constant Unsigned_64 := 1601;
   OP_STREAM_LIST    : constant Unsigned_32 := 16#0705#;
   OP_STREAM_AVAILABLE : constant Unsigned_32 := 16#0706#;
   REPLY_OK          : constant Unsigned_32 := 16#F000#;
   STREAM_LIST_TOKEN : constant Unsigned_64 := 16#5343_5354#;
   MAX_DRIVER_ID     : constant Natural := 17;
   MAX_CAP_SLOT      : constant Natural := 63;

   CAP_NULL         : constant Unsigned_64 := 0;
   CAP_ENDPOINT     : constant Unsigned_64 := 1;
   CAP_NOTIFICATION : constant Unsigned_64 := 2;
   CAP_MEMORY       : constant Unsigned_64 := 3;
   CAP_IOPORT       : constant Unsigned_64 := 4;
   CAP_IRQ          : constant Unsigned_64 := 5;
   CAP_PROCESS      : constant Unsigned_64 := 6;
   CAP_DEVICE_MEM   : constant Unsigned_64 := 7;
   CAP_REPLY        : constant Unsigned_64 := 8;
   CAP_RESOURCE     : constant Unsigned_64 := 9;

   initialW : constant Natural := 760;
   initialH : constant Natural := 520;

   win : CuBit.UI.App.Window;
   ignore : Unsigned_64;
   ui : CuBit.UI.State.UI_State;
   controls : CuBit.UI.Controls.Control_Map;

   CONTROL_TAB_BASE : constant CuBit.UI.Controls.Control_ID := 1;
   CONTROL_REFRESH  : constant CuBit.UI.Controls.Control_ID := 8;
   CONTROL_LOCKDOWN : constant CuBit.UI.Controls.Control_ID := 9;
   CONTROL_NAV_SCROLL : constant CuBit.UI.Controls.Control_ID := 10;
   CONTROL_ROW_1    : constant CuBit.UI.Controls.Control_ID := 20;
   CONTROL_GRANT_1  : constant CuBit.UI.Controls.Control_ID := 80;

   activeTab : Natural := 1;
   selectedProcess : Natural := 1;
   navigatorScroll : Natural := 0;
   selectedGrant : Natural := 1;
   refreshCount : Natural := 0;
   selectedStreamMask : Unsigned_64 := 0;
   selectedStreamCount : Unsigned_64 := 0;
   streamQueryPending : Boolean := False;

   type Cap_Info is record
      capType : Unsigned_64 := 0;
      rights  : Unsigned_64 := 0;
      badge   : Unsigned_64 := 0;
      ref     : Unsigned_64 := 0;
      param   : Unsigned_64 := 0;
      gen     : Unsigned_64 := 0;
   end record;

   type Cap_Cache_Array is array (Natural range 0 .. MAX_CAP_SLOT) of Cap_Info;
   capCache : Cap_Cache_Array := (others => (others => 0));
   capCachePID : Unsigned_64 := Unsigned_64'Last;
   capCacheCount : Natural := 0;
   capCacheValid : Boolean := False;

   type Authority_Info is record
      valid       : Boolean := False;
      authorityId : Unsigned_32 := 0;
      source      : Unsigned_8 := 0;
      reason      : Unsigned_8 := 0;
      requested   : Boolean := False;
      granted     : Boolean := False;
      capType     : Unsigned_64 := 0;
      rights      : Unsigned_64 := 0;
      ref         : Unsigned_64 := 0;
      param       : Unsigned_64 := 0;
   end record;
   NULL_AUTHORITY_INFO : constant Authority_Info :=
     (valid => False, authorityId => 0, source => 0, reason => 0,
      requested => False, granted => False, capType => 0, rights => 0,
      ref => 0, param => 0);

   type Authority_Cache_Array is
      array (Natural range 0 .. MAX_CAP_SLOT) of Authority_Info;
   authorityCache : Authority_Cache_Array;
   authorityCachePID : Unsigned_64 := Unsigned_64'Last;
   authorityCacheValid : Boolean := False;
   authorityBackendReported : Boolean := False;

   type Dashboard_Layout is record
      root : CuBit.UI.Rect := (others => 0);
      header : CuBit.UI.Rect := (others => 0);
      tabs : CuBit.UI.Rect := (others => 0);
      page : CuBit.UI.Rect := (others => 0);
      sidebar : CuBit.UI.Rect := (others => 0);
      content : CuBit.UI.Rect := (others => 0);
      status : CuBit.UI.Rect := (others => 0);
      refresh : CuBit.UI.Rect := (others => 0);
      lockdown : CuBit.UI.Rect := (others => 0);
   end record;

   function unpackLo32 (value : Unsigned_64) return Natural is
   begin
      return Natural (value and 16#FFFF_FFFF#);
   end unpackLo32;

   function unpackHi32 (value : Unsigned_64) return Natural is
   begin
      return Natural (Shift_Right (value, 32));
   end unpackHi32;

   function unpackSignedLo32 (value : Unsigned_64) return Integer is
      lo : constant Unsigned_64 := value and 16#FFFF_FFFF#;
   begin
      if (lo and 16#8000_0000#) /= 0 then
         return -Integer ((not lo + 1) and 16#FFFF_FFFF#);
      end if;
      return Integer (lo);
   end unpackSignedLo32;

   function addrToU64 (addr : System.Address) return Unsigned_64 is
   begin
      return Unsigned_64 (System.Storage_Elements.To_Integer (addr));
   end addrToU64;

   function driverName (id : Natural) return String is
   begin
      case id is
         when 1  => return "keyboard";
         when 2  => return "ata";
         when 3  => return "netstack";
         when 4  => return "procmgr";
         when 5  => return "nvme";
         when 6  => return "filesystem";
         when 7  => return "devmgr";
         when 8  => return "hda";
         when 9  => return "mixer";
         when 10 => return "mouse";
         when 11 => return "config";
         when 12 => return "netmgr";
         when 13 => return "logstore";
         when 14 => return "ipctest";
         when 15 => return "desktop";
         when 16 => return "display";
         when 17 => return "virtio-gpu";
         when others => return "service";
      end case;
   end driverName;

   function serviceCount return Unsigned_64 is
      count : Unsigned_64 := 0;
   begin
      for id in 1 .. MAX_DRIVER_ID loop
         if getInfo (SYSINFO_REGISTERED_DRIVER, Unsigned_64 (id)) /= 0 then
            count := count + 1;
         end if;
      end loop;
      return count;
   end serviceCount;

   function processCount return Natural is
      count : Natural := 0;
   begin
      for id in 1 .. MAX_DRIVER_ID loop
         if getInfo (SYSINFO_REGISTERED_DRIVER, Unsigned_64 (id)) /= 0 then
            count := count + 1;
         end if;
      end loop;
      return count;
   end processCount;

   function driverForRow (row : Natural) return Natural is
      seen : Natural := 0;
   begin
      for id in 1 .. MAX_DRIVER_ID loop
         if getInfo (SYSINFO_REGISTERED_DRIVER, Unsigned_64 (id)) /= 0 then
            seen := seen + 1;
            if seen = row then
               return id;
            end if;
         end if;
      end loop;
      return 0;
   end driverForRow;

   function pidForRow (row : Natural) return Unsigned_64 is
      driver : constant Natural := driverForRow (row);
   begin
      if driver = 0 then
         return 0;
      end if;
      return getInfo (SYSINFO_REGISTERED_DRIVER, Unsigned_64 (driver));
   end pidForRow;

   function memMiB (bytes : Unsigned_64) return Unsigned_64 is
   begin
      return bytes / (1024 * 1024);
   end memMiB;

   function processName (id : Natural) return String is
      driver : constant Natural := driverForRow (id);
   begin
      if driver = 0 then
         return "process";
      end if;
      return driverName (driver) & ".svc";
   end processName;

   function processProfile (id : Natural) return String is
      driver : constant Natural := driverForRow (id);
   begin
      case driver is
         when 4  => return "process authority";
         when 6  => return "filesystem authority";
         when 9  => return "audio authority";
         when 13 => return "stream collector";
         when 15 => return "session authority";
         when 16 => return "display owner";
         when 17 => return "device driver";
         when others => return "registered service";
      end case;
   end processProfile;

   function inspectCap
      (pid : Unsigned_64;
       slot : Natural;
       cap : out Cap_Info) return Boolean
   is
      ret : Unsigned_64;
   begin
      cap := (others => 0);
      ret := syscall
        (SYSCALL_INSPECT_CAP,
         pid,
         Unsigned_64 (slot),
         addrToU64 (cap'Address));
      return ret = 1;
   end inspectCap;

   procedure refreshCapCache (pid : Unsigned_64) is
      cap : Cap_Info;
   begin
      if capCacheValid and then capCachePID = pid then
         return;
      end if;

      capCache := (others => (others => 0));
      capCachePID := pid;
      capCacheCount := 0;

      if pid /= 0 then
         for slot in 0 .. MAX_CAP_SLOT loop
            if inspectCap (pid, slot, cap) then
               capCache (slot) := cap;
               if cap.capType /= CAP_NULL then
                  capCacheCount := capCacheCount + 1;
               end if;
            end if;
         end loop;
      end if;

      capCacheValid := True;
   end refreshCapCache;

   procedure refreshAuthorityCache (pid : Unsigned_64) is
      msg : Message;
      tag : MessageTag;
      meta : Unsigned_64;
      flags : Unsigned_64;
      foundAny : Boolean := False;
   begin
      if authorityCacheValid and then authorityCachePID = pid then
         return;
      end if;

      authorityCache := (others => NULL_AUTHORITY_INFO);
      authorityCachePID := pid;

      if pid /= 0 then
         for slot in 0 .. MAX_CAP_SLOT loop
            msg := NULL_MESSAGE;
            msg.tag := (label => OP_AUTHORITY_QUERY, length => 2,
                        flags => 0, badge => 0);
            msg.words (0) := pid;
            msg.words (1) := Unsigned_64 (slot);
            tag := capCall (CAP_SLOT_PROCMGR, msg);
            if tag.label = REPLY_OK and then tag.length = 4 then
               foundAny := True;
               meta := msg.words (0);
               flags := Shift_Right (meta, 56) and 16#FF#;
               authorityCache (slot) :=
                 (valid       => True,
                  authorityId => Unsigned_32 (meta and 16#FFFF_FFFF#),
                  source      => Unsigned_8 (Shift_Right (meta, 40) and 16#FF#),
                  reason      => Unsigned_8 (Shift_Right (meta, 48) and 16#FF#),
                  requested   =>
                    (flags and Unsigned_64 (AUTH_FLAG_REQUESTED)) /= 0,
                  granted     =>
                    (flags and Unsigned_64 (AUTH_FLAG_GRANTED)) /= 0,
                  capType     => msg.words (1) and 16#FF#,
                  rights      => Shift_Right (msg.words (1), 8) and 16#1F#,
                  ref         => msg.words (2),
                  param       => msg.words (3));
            end if;
         end loop;
      end if;
      authorityCacheValid := True;
      if foundAny and then not authorityBackendReported then
         debugPrint ("security-center: authority provenance ready" & LF);
         authorityBackendReported := True;
      end if;
   end refreshAuthorityCache;

   function authoritySourceName (source : Unsigned_8) return String is
   begin
      case source is
         when AUTH_SOURCE_MANIFEST => return "ELF manifest";
         when AUTH_SOURCE_KERNEL_BOOTSTRAP => return "kernel bootstrap";
         when AUTH_SOURCE_COMPATIBILITY => return "compatibility rule";
         when AUTH_SOURCE_IDENTITY_POLICY => return "package identity policy";
         when AUTH_SOURCE_CONFIG_POLICY => return "configured quota";
         when others => return "unknown source";
      end case;
   end authoritySourceName;

   function authorityReasonName (reason : Unsigned_8) return String is
   begin
      case reason is
         when AUTH_REASON_MANIFEST_REQUEST => return "manifest request";
         when AUTH_REASON_SELF_BOOTSTRAP => return "self bootstrap";
         when AUTH_REASON_FS_BOOTSTRAP => return "filesystem bootstrap";
         when AUTH_REASON_INPUT_COMPAT =>
            return "temporary input-focus compatibility";
         when AUTH_REASON_PROCESS_COMPAT =>
            return "temporary wildcard process compatibility";
         when AUTH_REASON_PACKAGE_ID => return "package identity matched";
         when AUTH_REASON_SERVICE_MISSING =>
            return "requested service was unavailable";
         when AUTH_REASON_MINT_FAILED => return "kernel rejected mint";
         when AUTH_REASON_CONFIG_QUOTA => return "configured resource quota";
         when others => return "no recorded reason";
      end case;
   end authorityReasonName;

   function authorityState
      (info : Authority_Info; effective : Boolean) return String
   is
   begin
      if not info.valid then
         if effective then
            return "effective (untracked)";
         end if;
         return "not recorded";
      elsif info.requested and then info.granted and then effective then
         return "requested / granted / effective";
      elsif info.requested and then not info.granted then
         return "requested / denied";
      elsif info.granted and then effective then
         return "ambient / effective";
      elsif info.granted then
         return "granted / no longer effective";
      end if;
      return "recorded / not granted";
   end authorityState;

   function rightsText (rights : Unsigned_64) return String is
      result : String (1 .. 5) := "-----";
   begin
      if (rights and 1) /= 0 then
         result (1) := 'R';
      end if;
      if (rights and 2) /= 0 then
         result (2) := 'W';
      end if;
      if (rights and 4) /= 0 then
         result (3) := 'X';
      end if;
      if (rights and 8) /= 0 then
         result (4) := 'G';
      end if;
      if (rights and 16) /= 0 then
         result (5) := 'V';
      end if;
      return result;
   end rightsText;

   function capTypeName (capType : Unsigned_64) return String is
   begin
      case capType is
         when CAP_ENDPOINT     => return "endpoint";
         when CAP_NOTIFICATION => return "notify";
         when CAP_MEMORY       => return "memory";
         when CAP_IOPORT       => return "ioport";
         when CAP_IRQ          => return "irq";
         when CAP_PROCESS      => return "process";
         when CAP_DEVICE_MEM   => return "device mem";
         when CAP_REPLY        => return "reply";
         when CAP_RESOURCE     => return "resource";
         when others           => return "null";
      end case;
   end capTypeName;

   function driverForPID (pid : Unsigned_64) return Natural is
   begin
      for id in 1 .. MAX_DRIVER_ID loop
         if getInfo (SYSINFO_REGISTERED_DRIVER, Unsigned_64 (id)) = pid then
            return id;
         end if;
      end loop;
      return 0;
   end driverForPID;

   function capObjectName (cap : Cap_Info) return String is
      driver : Natural;
   begin
      if cap.capType = CAP_ENDPOINT then
         driver := driverForPID (cap.ref);
         if driver /= 0 then
            return driverName (driver) & ".svc";
         end if;
         return "pid endpoint";
      elsif cap.capType = CAP_NOTIFICATION then
         if cap.ref <= Unsigned_64 (MAX_DRIVER_ID) then
            return driverName (Natural (cap.ref)) & " event";
         end if;
         return "notification";
      elsif cap.capType = CAP_PROCESS then
         if cap.ref = 0 then
            return "all processes";
         end if;
         return "process";
      elsif cap.capType = CAP_IOPORT then
         return "i/o port";
      elsif cap.capType = CAP_DEVICE_MEM then
         return "device memory";
      elsif cap.capType = CAP_RESOURCE then
         return "resource budget";
      end if;
      return "object";
   end capObjectName;

   function hasCap
      (pid : Unsigned_64;
       capType : Unsigned_64;
       ref : Unsigned_64 := Unsigned_64'Last) return Boolean
   is
   begin
      refreshCapCache (pid);
      for slot in 0 .. MAX_CAP_SLOT loop
         if capCache (slot).capType = capType and then
            (ref = Unsigned_64'Last or else capCache (slot).ref = ref)
         then
            return True;
         end if;
      end loop;
      return False;
   end hasCap;

   function hasEndpointToDriver
      (pid : Unsigned_64;
       driver : Unsigned_64) return Boolean
   is
      target : constant Unsigned_64 :=
        getInfo (SYSINFO_REGISTERED_DRIVER, driver);
   begin
      return target /= 0 and then hasCap (pid, CAP_ENDPOINT, target);
   end hasEndpointToDriver;

   function capCount (pid : Unsigned_64) return Natural is
   begin
      refreshCapCache (pid);
      return capCacheCount;
   end capCount;

   function capForRow
      (pid : Unsigned_64;
       row : Natural;
       cap : out Cap_Info;
       slotOut : out Natural) return Boolean
   is
      seen : Natural := 0;
   begin
      refreshCapCache (pid);
      for slot in 0 .. MAX_CAP_SLOT loop
         if capCache (slot).capType /= CAP_NULL then
            seen := seen + 1;
            if seen = row then
               cap := capCache (slot);
               slotOut := slot;
               return True;
            end if;
         end if;
      end loop;
      cap := (others => 0);
      slotOut := 0;
      return False;
   end capForRow;

   function capSlotLabel (id : Natural) return String is
      cap : Cap_Info;
      slot : Natural;
   begin
      if capForRow (pidForRow (selectedProcess), id, cap, slot) then
         return "slot" & Natural'Image (slot);
      end if;
      return "-";
   end capSlotLabel;

   function authorityCount (pid : Unsigned_64) return Natural is
      count : Natural := 0;
   begin
      refreshCapCache (pid);
      refreshAuthorityCache (pid);
      for slot in 0 .. MAX_CAP_SLOT loop
         if capCache (slot).capType /= CAP_NULL or else
            authorityCache (slot).valid
         then
            count := count + 1;
         end if;
      end loop;
      return count;
   end authorityCount;

   function authorityForRow
     (pid : Unsigned_64;
      row : Natural;
      slotOut : out Natural;
      cap : out Cap_Info;
      info : out Authority_Info) return Boolean
   is
      seen : Natural := 0;
   begin
      refreshCapCache (pid);
      refreshAuthorityCache (pid);
      for slot in 0 .. MAX_CAP_SLOT loop
         if capCache (slot).capType /= CAP_NULL or else
            authorityCache (slot).valid
         then
            seen := seen + 1;
            if seen = row then
               slotOut := slot;
               cap := capCache (slot);
               info := authorityCache (slot);
               return True;
            end if;
         end if;
      end loop;
      slotOut := 0;
      cap := (others => 0);
      info := NULL_AUTHORITY_INFO;
      return False;
   end authorityForRow;

   function streamName (id : Natural) return String is
   begin
      case id is
         when 1 => return "stdin";
         when 2 => return "stdout";
         when 3 => return "stderr";
         when 4 => return "log";
         when others => return "stream";
      end case;
   end streamName;

   function grantPath (id : Natural) return String is
      cap : Cap_Info;
      slot : Natural;
   begin
      if capForRow (pidForRow (selectedProcess), id, cap, slot) then
         if cap.capType = CAP_ENDPOINT and then
            cap.ref = getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_FS)
         then
            return "filesystem endpoint";
         end if;
         return capObjectName (cap);
      end if;
      return "none";
   end grantPath;

   function grantRights (id : Natural) return String is
      cap : Cap_Info;
      slot : Natural;
   begin
      if capForRow (pidForRow (selectedProcess), id, cap, slot) then
         return rightsText (cap.rights);
      end if;
      return "none";
   end grantRights;

   function grantSource (id : Natural) return String is
      cap : Cap_Info;
      slot : Natural;
   begin
      if capForRow (pidForRow (selectedProcess), id, cap, slot) then
         return capSlotLabel (id) & " " & capTypeName (cap.capType);
      end if;
      return "no capability";
   end grantSource;

   function computeLayout (width, height : Natural) return Dashboard_Layout is
      ret : Dashboard_Layout;
      frame : CuBit.UI.Layout.Dock_Frame;
      inspector : CuBit.UI.Rect;
      inspectorFrame : CuBit.UI.Layout.Dock_Frame;
      rootW : Natural := width;
      rootH : Natural := height;
      sidebarW : Natural := 180;
   begin
      --  Security Center is the first real resize client, so keep the model
      --  intentionally plain: a docked outer frame, a left object browser,
      --  and a right inspector whose tabs choose the current view.
      if rootW < initialW then
         rootW := initialW;
      end if;
      if rootH < initialH then
         rootH := initialH;
      end if;
      ret.root := (x => 0, y => 0, w => rootW, h => rootH);
      frame := CuBit.UI.Layout.Begin_Dock (ret.root);
      ret.header := CuBit.UI.Layout.Dock_Top (frame, 60);
      ret.status := CuBit.UI.Layout.Dock_Bottom (frame, 24);
      ret.page := CuBit.UI.Layout.Inset
        (CuBit.UI.Layout.Fill (frame), 14, 12, 14, 10);

      if ret.page.w > 720 then
         sidebarW := 200;
      end if;
      ret.sidebar :=
        (x => ret.page.x, y => ret.page.y, w => sidebarW, h => ret.page.h);
      inspector :=
        (x => ret.sidebar.x + ret.sidebar.w + 12,
         y => ret.page.y,
         w => ret.page.w - ret.sidebar.w - 12,
         h => ret.page.h);
      inspectorFrame := CuBit.UI.Layout.Begin_Dock (inspector);
      ret.tabs := CuBit.UI.Layout.Dock_Top (inspectorFrame, 32);
      ret.content := CuBit.UI.Layout.Inset
        (CuBit.UI.Layout.Fill (inspectorFrame), 0, 10, 0, 0);
      ret.refresh :=
        (x => ret.header.x + ret.header.w - 226,
         y => ret.header.y + 16, w => 96, h => 30);
      ret.lockdown :=
        (x => ret.header.x + ret.header.w - 120,
         y => ret.header.y + 16, w => 106, h => 30);
      return ret;
   end computeLayout;

   layout : Dashboard_Layout := computeLayout (initialW, initialH);

   function hitTabIndex (x, y : Natural) return Natural is
      padding : constant Natural := 24;
      stripH : constant Natural := Natural'Min (30, layout.tabs.h);
      tabH : constant Natural :=
         (if stripH > 4 then stripH - 4 else stripH);
      tabY : constant Natural := layout.tabs.y + (stripH - tabH);
      tabX : Natural := layout.tabs.x + 2;
      tabW : Natural;
      tabBounds : CuBit.UI.Rect;
      index : Natural;
   begin
      for i in Security_Center_Form.TAB_LABELS'Range loop
         if Security_Center_Form.TAB_LABELS (i) /= null and then
            tabX < layout.tabs.x + layout.tabs.w
         then
            index := Natural (i - Security_Center_Form.TAB_LABELS'First) + 1;
            tabW :=
              CuBit.UI.UI_Text_Width
                (Security_Center_Form.TAB_LABELS (i).all) + padding;
            tabW := Natural'Min (tabW, layout.tabs.x + layout.tabs.w - tabX);
            tabBounds := (x => tabX, y => tabY, w => tabW, h => tabH);
            if CuBit.UI.Point_In_Rect (x, y, tabBounds) then
               return index;
            end if;
            tabX := tabX + tabW;
         end if;
      end loop;

      return 0;
   end hitTabIndex;

   procedure drawText
      (c : CuBit.UI.Canvas;
       r : CuBit.UI.Rect;
       text : String;
       muted : Boolean := False)
   is
   begin
      CuBit.UI.Labels.Label (c, r, CuBit.UI.Classic, text, muted);
   end drawText;

   procedure drawConnector
      (c : CuBit.UI.Canvas;
       fromX, fromY, toX, toY : Natural;
       color : CuBit.UI.Color)
   is
      x1 : Natural := fromX;
      x2 : Natural := toX;
      y1 : Natural := fromY;
      y2 : Natural := toY;
   begin
      if x2 < x1 then
         x1 := toX;
         x2 := fromX;
      end if;
      if y2 < y1 then
         y1 := toY;
         y2 := fromY;
      end if;

      if x2 > x1 then
         CuBit.UI.Fill_Rect
           (c, (x => x1, y => fromY, w => x2 - x1 + 1, h => 2), color);
      end if;
      if y2 > y1 then
         CuBit.UI.Fill_Rect
           (c, (x => toX, y => y1, w => 2, h => y2 - y1 + 1), color);
      end if;
   end drawConnector;

   procedure requestSelectedStreams is
      pid : constant Unsigned_64 := pidForRow (selectedProcess);
      msg : Message := NULL_MESSAGE;
      ok  : Boolean;
   begin
      selectedStreamMask := 0;
      selectedStreamCount := 0;
      streamQueryPending := False;

      if pid = 0 or else pid > 255 then
         return;
      end if;

      msg.tag := (label => OP_STREAM_LIST,
                  length => 0,
                  flags => 0,
                  badge => 0);
      ok := submit (ProcessID (pid), msg, STREAM_LIST_TOKEN);
      streamQueryPending := ok;
   end requestSelectedStreams;

   procedure pollStreamQuery is
      comp : CompletionEntry;
      ret  : Unsigned_64;
   begin
      ret := Poll_Completion (comp'Address);
      if ret = 1 and then comp.token = STREAM_LIST_TOKEN then
         streamQueryPending := False;
         if comp.status = COMPLETION_OK and then
            comp.msg.tag.label = REPLY_OK
         then
            selectedStreamMask := comp.msg.words (0);
            selectedStreamCount := comp.msg.words (1);
         end if;
      end if;
   end pollStreamQuery;

   procedure scrollByWheel
      (value : in out Natural;
       maxValue : Natural;
       wheelDelta : Integer)
   is
   begin
      if wheelDelta > 0 then
         if value = 0 then
            return;
         end if;
         value := value - 1;
      elsif wheelDelta < 0 then
         value := Natural'Min (value + 1, maxValue);
      end if;
   end scrollByWheel;

   procedure drawStreamBadges
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect;
       mask : Unsigned_64)
   is
      x : Natural := bounds.x;
      y : constant Natural := bounds.y;
      r : CuBit.UI.Rect;
   begin
      for bit in 1 .. 7 loop
         if (mask and Shift_Left (Unsigned_64'(1), bit)) /= 0 then
            r := (x => x, y => y, w => 74, h => 26);
            CuBit.UI.Widgets.Badge
              (c, r, CuBit.UI.Classic, streamName (bit),
               CuBit.UI.Widgets.Badge_Good);
            x := x + 82;
         end if;
      end loop;
      if mask = 0 then
         drawText (c, bounds, "No active streams reported", True);
      end if;
   end drawStreamBadges;

   procedure drawAuthorityMap
      (c : CuBit.UI.Canvas;
       bounds : CuBit.UI.Rect)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.Classic;
      pid : constant Unsigned_64 := pidForRow (selectedProcess);
      totalCaps : constant Natural := capCount (pid);
      content : CuBit.UI.Rect;
      app : CuBit.UI.Rect;
      centerX : Natural;
      centerH : Natural := 54;
      cap : Cap_Info;
      slot : Natural;
      node : CuBit.UI.Rect;
      nodeY : Natural;
      nodeW : Natural := 150;
      rowsPerSide : Natural := 3;
      visibleCaps : Natural;
      more : CuBit.UI.Rect;

      function nodeLabel (info : Cap_Info) return String is
      begin
         if info.capType = CAP_ENDPOINT or else
            info.capType = CAP_NOTIFICATION
         then
            return capObjectName (info);
         else
            return capTypeName (info.capType);
         end if;
      end nodeLabel;
   begin
      CuBit.UI.Widgets.Group_Box
        (c, bounds, colors, "Effective authority map", content, 10);

      if content.h < 190 then
         centerH := 46;
      end if;
      centerX := content.x + content.w / 2;
      if content.w < 520 then
         nodeW := 124;
      end if;
      if content.h > 230 then
         rowsPerSide := 4;
      end if;
      visibleCaps := Natural'Min (rowsPerSide * 2, totalCaps);

      app :=
        (x => centerX - 82, y => content.y + content.h / 2 - centerH / 2,
         w => 164, h => centerH);

      CuBit.UI.Fill_Rect (c, app, colors.accent);
      CuBit.UI.Stroke_Rect (c, app, colors.edge, colors.shadow);
      CuBit.UI.Draw_UI_Text
        (c, app.x + 10, app.y + 8, processName (selectedProcess),
         colors.text, colors.accent);
      CuBit.UI.Draw_UI_Text
        (c, app.x + 10, app.y + 28, processProfile (selectedProcess),
         colors.text, colors.accent);

      if visibleCaps = 0 then
         CuBit.UI.Widgets.Badge
           (c, (x => centerX - 74, y => app.y + app.h + 18,
                w => 148, h => 28),
            colors, "no live caps", CuBit.UI.Widgets.Badge_Neutral);
         return;
      end if;

      for id in 1 .. visibleCaps loop
         if capForRow (pid, id, cap, slot) then
            if id <= rowsPerSide then
               nodeY := content.y + 18 + (id - 1) * 44;
               node := (x => content.x + 8, y => nodeY, w => nodeW, h => 34);
               drawConnector
                 (c, app.x, app.y + app.h / 2,
                  node.x + node.w, node.y + node.h / 2,
                  colors.shadow);
            else
               nodeY := content.y + 18 + (id - rowsPerSide - 1) * 44;
               node := (x => content.x + content.w - nodeW - 8,
                        y => nodeY, w => nodeW, h => 34);
               drawConnector
                 (c, app.x + app.w, app.y + app.h / 2,
                  node.x, node.y + node.h / 2,
                  colors.shadow);
            end if;

            CuBit.UI.Widgets.Badge
              (c, node, colors, nodeLabel (cap),
               (if cap.capType = CAP_ENDPOINT or else
                   cap.capType = CAP_NOTIFICATION
                then CuBit.UI.Widgets.Badge_Good
                else CuBit.UI.Widgets.Badge_Neutral));
         end if;
      end loop;

      if totalCaps > visibleCaps then
         more :=
           (x => centerX - 64, y => content.y + content.h - 34,
            w => 128, h => 28);
         CuBit.UI.Widgets.Badge
           (c, more, colors,
            "+" & Natural'Image (totalCaps - visibleCaps) & " more",
            CuBit.UI.Widgets.Badge_Neutral);
      end if;
   end drawAuthorityMap;

   procedure drawNavigator
      (c : CuBit.UI.Canvas)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.Classic;
      content : CuBit.UI.Rect;
      parent : CuBit.UI.Layout.Container;
      listArea : CuBit.UI.Rect;
      scrollBar : CuBit.UI.Rect;
      row : CuBit.UI.Rect;
      rowY : Natural;
      rowIndex : Natural := 0;
      visibleRows : Natural := 0;
      totalRows : constant Natural := processCount;
      maxScroll : Natural := 0;
      result : CuBit.UI.Widget_Result;
      scrollResult : CuBit.UI.Widget_Result;
      dummySelection : Natural := 0;
      rowId : constant CuBit.UI.Controls.Control_ID := CONTROL_ROW_1;
   begin
      CuBit.UI.Widgets.Group_Box
        (c, layout.sidebar, colors, "System", content, 8);
      scrollBar :=
        (x => content.x + content.w - 14, y => content.y + 28,
         w => 14, h => content.h - 28);
      listArea :=
        (x => content.x, y => content.y, w => content.w - 18,
         h => content.h);
      parent := CuBit.UI.Layout.Root (listArea);

      row := CuBit.UI.Layout.Resolve
        (parent, (x => 0, y => 0, w => listArea.w, h => 24));
      CuBit.UI.Trees.Tree_Item
        (c, ui, controls, rowId, row, layout.sidebar, colors,
         "Processes", 0, dummySelection,
         depth => 0, expanded => True, hasChildren => True,
         result => result);

      rowY := 28;
      if listArea.h > rowY + 24 then
         visibleRows := (listArea.h - rowY) / 24;
      end if;
      if totalRows > visibleRows then
         maxScroll := totalRows - visibleRows;
      end if;
      if navigatorScroll > maxScroll then
         navigatorScroll := maxScroll;
      end if;

      for driver in 1 .. MAX_DRIVER_ID loop
         if getInfo (SYSINFO_REGISTERED_DRIVER, Unsigned_64 (driver)) /= 0 then
            rowIndex := rowIndex + 1;
            if rowIndex > navigatorScroll and then
               rowIndex <= navigatorScroll + visibleRows
            then
               row := CuBit.UI.Layout.Resolve
                 (parent, (x => 0, y => rowY, w => listArea.w, h => 24));
               CuBit.UI.Trees.Tree_Item
                 (c, ui, controls, rowId + rowIndex, row, layout.sidebar, colors,
                  driverName (driver) & ".svc", rowIndex, selectedProcess,
                  depth => 1, expanded => False, hasChildren => False,
                  result => result);
               rowY := rowY + 24;
            end if;
         end if;
      end loop;

      if rowIndex = 0 then
         selectedProcess := 1;
      elsif selectedProcess > rowIndex then
         selectedProcess := rowIndex;
      end if;

      if maxScroll > 0 then
         CuBit.UI.Widgets.Vertical_Scrollbar
           (c, ui, controls, CONTROL_NAV_SCROLL,
            scrollBar, layout.sidebar, colors,
            0, maxScroll, navigatorScroll, scrollResult);
      end if;
   end drawNavigator;

   procedure drawOverview
      (c : CuBit.UI.Canvas)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.Classic;
      parent : constant CuBit.UI.Layout.Container :=
         CuBit.UI.Layout.Root (layout.content);
      card : CuBit.UI.Rect;
      content : CuBit.UI.Rect;
      result : CuBit.UI.Widget_Result;
      metricW : constant Natural := (layout.content.w - 24) / 4;
      metricH : constant Natural := 50;
      gap : constant Natural := 12;
      actionsH : Natural := 82;
      mapY : constant Natural := metricH + gap;
      mapH : Natural := 170;
      actionsY : Natural;
      textW : Natural;
   begin
      --  Prime launch provenance on the overview so inspection failures are
      --  visible immediately rather than only after opening the Caps tab.
      refreshAuthorityCache (pidForRow (selectedProcess));

      if layout.content.h < metricH + gap + actionsH + 150 then
         actionsH := 72;
      end if;

      if layout.content.h > metricH + gap * 2 + actionsH then
         mapH := layout.content.h - metricH - gap * 2 - actionsH;
      end if;
      if mapH < 150 then
         mapH := 150;
      end if;
      actionsY := mapY + mapH + gap;

      CuBit.UI.Widgets.Metric_Card
        (c, CuBit.UI.Layout.Resolve
              (parent, (x => 0, y => 0, w => metricW, h => metricH)),
         colors, "CPUs", Natural (getInfo (SYSINFO_NUM_CPUS)));
      CuBit.UI.Widgets.Metric_Card
        (c, CuBit.UI.Layout.Resolve
              (parent, (x => metricW + 8, y => 0, w => metricW, h => metricH)),
         colors, "Services", Natural (serviceCount));
      CuBit.UI.Widgets.Metric_Card
        (c, CuBit.UI.Layout.Resolve
              (parent, (x => (metricW + 8) * 2, y => 0, w => metricW, h => metricH)),
         colors, "Free MiB", Natural (memMiB (getInfo (SYSINFO_MEM_FREE))));
      CuBit.UI.Widgets.Metric_Card
        (c, CuBit.UI.Layout.Resolve
              (parent, (x => (metricW + 8) * 3, y => 0, w => metricW, h => metricH)),
         colors, "Total MiB", Natural (memMiB (getInfo (SYSINFO_MEM_TOTAL))));

      card := CuBit.UI.Layout.Resolve
        (parent, (x => 0, y => mapY, w => layout.content.w, h => mapH));
      drawAuthorityMap (c, card);

      card := CuBit.UI.Layout.Resolve
        (parent, (x => 0, y => actionsY, w => layout.content.w, h => actionsH));
      CuBit.UI.Widgets.Group_Box (c, card, colors, "Recommended actions",
                                  content, 10);
      textW := content.w;
      if textW > 128 then
         textW := textW - 128;
      end if;
      drawText
        (c, (x => content.x, y => content.y, w => textW, h => 18),
         "Inspect effective authority, not only manifest intent.");
      drawText
        (c, (x => content.x, y => content.y + 24, w => textW, h => 18),
         "Filesystem, network, secrets, surfaces, and realtime are explicit grants.",
         True);
      CuBit.UI.Widgets.Button
        (c, ui, controls, CONTROL_REFRESH,
         (x => content.x + content.w - 112, y => content.y + 8, w => 104, h => 30),
         card, colors, "Refresh", result);
      if result.activated then
         refreshCount := refreshCount + 1;
         capCacheValid := False;
         authorityCacheValid := False;
      end if;
   end drawOverview;

   procedure drawFilesystem
      (c : CuBit.UI.Canvas)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.Classic;
      pid : constant Unsigned_64 := pidForRow (selectedProcess);
      fsPID : constant Unsigned_64 :=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_FS);
      fsEndpoint : constant Boolean := hasEndpointToDriver (pid, DRIVER_FS);
      totalCaps : constant Natural := capCount (pid);
      parent : constant CuBit.UI.Layout.Container :=
         CuBit.UI.Layout.Root (layout.content);
      table : constant CuBit.UI.Rect :=
         CuBit.UI.Layout.Resolve
           (parent, (x => 0, y => 0, w => layout.content.w, h => 200));
      row : CuBit.UI.Rect;
      result : CuBit.UI.Widget_Result;
      rowY : Natural;
      rowId : CuBit.UI.Controls.Control_ID := CONTROL_GRANT_1;
      detail : CuBit.UI.Rect;
      content : CuBit.UI.Rect;
   begin
      if totalCaps = 0 then
         selectedGrant := 1;
      elsif selectedGrant > totalCaps then
         selectedGrant := totalCaps;
      end if;

      CuBit.UI.Widgets.Panel (c, table, colors, row, 8);
      CuBit.UI.Draw_Table_Header
        (c, (x => row.x, y => row.y, w => row.w, h => 22),
         colors, "Object", "Rights", "Capability");
      rowY := row.y + 22;
      for id in 1 .. Natural'Min (6, totalCaps) loop
         CuBit.UI.Tables.Row
           (c, ui, controls, rowId,
            (x => row.x, y => rowY, w => row.w, h => 24),
            table, colors,
            grantPath (id),
            grantRights (id),
            grantSource (id),
            id,
            selectedGrant,
            result);
         rowY := rowY + 24;
         rowId := rowId + 1;
      end loop;

      detail := CuBit.UI.Layout.Resolve
        (parent, (x => 0, y => 220, w => layout.content.w, h => 132));
      CuBit.UI.Widgets.Group_Box
        (c, detail, colors, "Filesystem authority", content, 10);
      CuBit.UI.Widgets.Key_Value
        (c, (x => content.x, y => content.y, w => content.w, h => 22),
         colors, "Filesystem endpoint",
         (if fsEndpoint then "present" else "absent"));
      CuBit.UI.Widgets.Key_Value
        (c, (x => content.x, y => content.y + 28, w => content.w, h => 22),
         colors, "Filesystem service PID", Unsigned_64'Image (fsPID));
      CuBit.UI.Widgets.Key_Value
        (c, (x => content.x, y => content.y + 56, w => content.w, h => 22),
         colors, "Selected cap", grantSource (selectedGrant), True);
      drawText
        (c, (x => content.x, y => content.y + 88, w => content.w, h => 18),
         "Path-level file grants are not kernel-visible yet; this view now shows the real endpoint authority.",
         True);
   end drawFilesystem;

   procedure drawCapabilities
      (c : CuBit.UI.Canvas)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.Classic;
      pid : constant Unsigned_64 := pidForRow (selectedProcess);
      totalCaps : constant Natural := authorityCount (pid);
      parent : constant CuBit.UI.Layout.Container :=
         CuBit.UI.Layout.Root (layout.content);
      table : constant CuBit.UI.Rect :=
         CuBit.UI.Layout.Resolve
           (parent, (x => 0, y => 0, w => layout.content.w, h => 232));
      row : CuBit.UI.Rect;
      result : CuBit.UI.Widget_Result;
      rowY : Natural;
      rowId : CuBit.UI.Controls.Control_ID := CONTROL_GRANT_1 + 16;
      notes : CuBit.UI.Rect;
      content : CuBit.UI.Rect;
      selectedSlot : Natural := 0;
      selectedCap : Cap_Info;
      selectedInfo : Authority_Info;
      selectedFound : Boolean;
   begin
      if totalCaps = 0 then
         selectedGrant := 1;
      elsif selectedGrant > totalCaps then
         selectedGrant := totalCaps;
      end if;

      CuBit.UI.Widgets.Panel (c, table, colors, row, 8);
      CuBit.UI.Draw_Table_Header
        (c, (x => row.x, y => row.y, w => row.w, h => 22),
         colors, "Authority", "State", "Source");
      rowY := row.y + 22;
      for id in 1 .. Natural'Min (8, totalCaps) loop
         declare
            slot : Natural;
            cap : Cap_Info;
            info : Authority_Info;
            found : constant Boolean :=
              authorityForRow (pid, id, slot, cap, info);
            pragma Unreferenced (found);
            effective : constant Boolean := cap.capType /= CAP_NULL;
            shownType : constant Unsigned_64 :=
              (if effective then cap.capType else info.capType);
         begin
            CuBit.UI.Tables.Row
              (c, ui, controls, rowId,
               (x => row.x, y => rowY, w => row.w, h => 24),
               table, colors,
               "slot" & Natural'Image (slot) & " " & capTypeName (shownType),
               authorityState (info, effective),
               (if info.valid then authoritySourceName (info.source)
                else "kernel state only"),
               id,
               selectedGrant,
               result);
         end;
         rowY := rowY + 24;
         rowId := rowId + 1;
      end loop;

      notes := CuBit.UI.Layout.Resolve
        (parent, (x => 0, y => 250, w => layout.content.w, h => 122));
      CuBit.UI.Widgets.Group_Box
        (c, notes, colors, "Selected authority explanation", content, 10);
      selectedFound := authorityForRow
        (pid, selectedGrant, selectedSlot, selectedCap, selectedInfo);
      CuBit.UI.Widgets.Key_Value
        (c, (x => content.x, y => content.y, w => content.w, h => 22),
         colors, "What",
         (if selectedFound
          then "slot" & Natural'Image (selectedSlot) & " " &
               capTypeName ((if selectedCap.capType /= CAP_NULL
                             then selectedCap.capType
                             else selectedInfo.capType))
          else "none"));
      CuBit.UI.Widgets.Key_Value
        (c, (x => content.x, y => content.y + 28, w => content.w, h => 22),
         colors, "Why",
         (if selectedInfo.valid then authorityReasonName (selectedInfo.reason)
          else "no launch provenance recorded"));
      CuBit.UI.Widgets.Key_Value
        (c, (x => content.x, y => content.y + 56, w => content.w, h => 22),
         colors, "Authority ID",
         (if selectedInfo.valid
          then Unsigned_32'Image (selectedInfo.authorityId) else "none"), True);
      drawText
        (c, (x => content.x, y => content.y + 88, w => content.w, h => 18),
         "Requested and granted come from procmgr; effective comes from the live kernel slot.",
         True);
   end drawCapabilities;

   procedure drawIPC
      (c : CuBit.UI.Canvas)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.Classic;
      content : CuBit.UI.Rect;
      left : CuBit.UI.Rect;
      right : CuBit.UI.Rect;
      midY : Natural;
   begin
      CuBit.UI.Widgets.Group_Box
        (c, layout.content, colors, "IPC and surface flow", content, 10);
      left := (x => content.x + 10, y => content.y + 34, w => 164, h => 42);
      right := (x => content.x + content.w - 184, y => content.y + 34,
                w => 164, h => 42);
      midY := left.y + 20;
      CuBit.UI.Fill_Rect (c, left, colors.face);
      CuBit.UI.Stroke_Rect (c, left, colors.edge, colors.shadow);
      CuBit.UI.Draw_UI_Text
        (c, left.x + 8, left.y + 12, processName (selectedProcess),
         colors.text, colors.face);
      CuBit.UI.Fill_Rect (c, right, colors.face);
      CuBit.UI.Stroke_Rect (c, right, colors.edge, colors.shadow);
      CuBit.UI.Draw_UI_Text
        (c, right.x + 8, right.y + 12, "desktop.svc",
         colors.text, colors.face);
      drawConnector
        (c, left.x + left.w, midY, right.x, midY, colors.accent);
      drawText
        (c, (x => content.x + 190, y => midY - 20, w => 180, h => 18),
         "surface + input");
      drawText
        (c, (x => content.x + 10, y => content.y + 110, w => content.w, h => 18),
         "Next backend: recent sends, completions, blocked waits, and capability-carrying messages.",
         True);
   end drawIPC;

   procedure drawStreams
      (c : CuBit.UI.Canvas)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.Classic;
      content : CuBit.UI.Rect;
   begin
      CuBit.UI.Widgets.Group_Box
        (c, layout.content, colors, "Streams for selected process",
         content, 10);
      drawText
        (c, (x => content.x, y => content.y, w => content.w, h => 18),
         processName (selectedProcess) & " exposes live stream routes.");
      drawText
        (c, (x => content.x, y => content.y + 26, w => content.w, h => 18),
         "Refresh asks the selected process for OP_STREAM_LIST.", True);
      if streamQueryPending then
         drawText
           (c, (x => content.x, y => content.y + 64,
                w => content.w, h => 18),
            "Waiting for stream reply...", True);
      else
         drawStreamBadges
           (c, (x => content.x, y => content.y + 64,
                w => content.w, h => 28),
            selectedStreamMask);
      end if;
      drawText
        (c, (x => content.x, y => content.y + 112, w => content.w, h => 18),
         "Next: route audit/log/metrics streams to files, apps, or network endpoints.",
         True);
      CuBit.UI.Draw_Natural_Value
        (c, (x => content.x, y => content.y + 144, w => 80, h => 18),
         colors, Natural (selectedStreamCount));
   end drawStreams;

   procedure drawPlaceholder
      (c : CuBit.UI.Canvas;
       title, message : String)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.Classic;
      content : CuBit.UI.Rect;
   begin
      CuBit.UI.Widgets.Group_Box
        (c, layout.content, colors, title, content, 12);
      drawText
        (c, (x => content.x, y => content.y, w => content.w, h => 18),
         message);
      drawText
        (c, (x => content.x, y => content.y + 30, w => content.w, h => 18),
         "The front-end surface exists now; the live backend can land behind it.",
         True);
   end drawPlaceholder;

   procedure render
      (win : in out CuBit.UI.App.Window; damage : CuBit.UI.Rect)
   is
      colors : constant CuBit.UI.Theme := CuBit.UI.Classic;
      c : constant CuBit.UI.Canvas := CuBit.UI.App.Canvas (win, damage);
      page : CuBit.UI.Rect;
      tabChanged : Boolean;
      buttonResult : CuBit.UI.Widget_Result;
   begin
      layout := computeLayout (CuBit.UI.App.Width (win),
                               CuBit.UI.App.Height (win));
      pollStreamQuery;
      CuBit.UI.Controls.Clear (controls);
      CuBit.UI.State.Begin_Frame (ui);
      CuBit.UI.State.Enter_Scope (ui);

      CuBit.UI.Fill_Rect (c, CuBit.UI.App.Full_Rect (win), colors.face);
      CuBit.UI.Fill_Rect (c, layout.root, colors.panel);
      CuBit.UI.Stroke_Rect (c, layout.root, colors.edge, colors.shadow);

      drawText
        (c, (x => layout.header.x + 8, y => layout.header.y + 8,
             w => 300, h => 20),
         "CuBit Security Center");
      drawText
        (c, (x => layout.header.x + 8, y => layout.header.y + 34,
             w => 420, h => 18),
         "Capability-aware system control surface", True);

      CuBit.UI.Widgets.Button
        (c, ui, controls, CONTROL_REFRESH,
         layout.refresh, layout.header, colors, "Refresh", buttonResult);
      if buttonResult.activated then
         refreshCount := refreshCount + 1;
         capCacheValid := False;
         requestSelectedStreams;
      end if;
      CuBit.UI.Widgets.Button
        (c, ui, controls, CONTROL_LOCKDOWN,
         layout.lockdown, layout.header, colors, "Lockdown", buttonResult);

      CuBit.UI.Widgets.Tab_Panel
        (c, ui, controls, CONTROL_TAB_BASE,
         layout.tabs, layout.root, colors,
         Security_Center_Form.TAB_LABELS, activeTab, page, tabChanged);

      drawNavigator (c);
      case activeTab is
         when 1 => drawOverview (c);
         when 2 => drawFilesystem (c);
         when 3 => drawCapabilities (c);
         when 4 => drawIPC (c);
         when 5 => drawStreams (c);
         when others => drawPlaceholder
            (c, "Launch",
             "Spawn applications with visible authority profiles.");
      end case;

      CuBit.UI.Draw_Status_Bar
        (c, layout.status, colors,
         "Ready. Tab switches views. Esc/Q exits.",
         (if refreshCount = 0 then "draft"
          else "refreshed"));

      CuBit.UI.State.Exit_Scope (ui);
      CuBit.UI.State.Finish_Frame (ui);
   end render;

   procedure handleEvent
      (win : in out CuBit.UI.App.Window;
       ev : CuBit.UI.App.Input_Event;
       dirty : in out CuBit.UI.Rect;
       running : in out Boolean)
   is
      x : Natural;
      y : Natural;
      down : Boolean;
      hit : CuBit.UI.Controls.Control_ID;
      wheelDelta : Integer;
      visibleRows : Natural := 0;
      maxScroll : Natural := 0;
   begin
      if ev.kind = CuBit.UI.App.INPUT_KEY_DOWN then
         if ev.payload0 = CuBit.UI.App.KEY_ESC or else
            ev.payload0 = CuBit.UI.App.KEY_Q
         then
            running := False;
         elsif ev.payload0 = 16#0F# then
            if activeTab >= Security_Center_Form.TAB_LABELS'Length then
               activeTab := 1;
            else
               activeTab := activeTab + 1;
            end if;
            dirty := CuBit.UI.App.Full_Rect (win);
         end if;
      elsif ev.kind = CuBit.UI.App.INPUT_CONFIGURE then
         dirty := CuBit.UI.App.Full_Rect (win);
      elsif ev.kind = Unsigned_64 (OP_STREAM_AVAILABLE) then
         if ev.payload0 = pidForRow (selectedProcess) then
            selectedStreamMask := ev.payload1;
            selectedStreamCount := 0;
            for bit in 0 .. 63 loop
               if (selectedStreamMask and Shift_Left (Unsigned_64'(1), bit))
                  /= 0
               then
                  selectedStreamCount := selectedStreamCount + 1;
               end if;
            end loop;
            streamQueryPending := False;
            dirty := CuBit.UI.App.Full_Rect (win);
         end if;
      elsif ev.kind = CuBit.UI.App.INPUT_POINTER_MOVE then
         x := unpackLo32 (ev.payload0);
         y := unpackHi32 (ev.payload0);
         down := (ev.payload1 and 1) /= 0;
         CuBit.UI.State.Set_Pointer (ui, x, y, down);
         dirty := CuBit.UI.App.Full_Rect (win);
      elsif ev.kind = CuBit.UI.App.INPUT_POINTER_WHEEL then
         x := unpackLo32 (ev.payload0);
         y := unpackHi32 (ev.payload0);
         wheelDelta := unpackSignedLo32 (ev.payload1);
         hit := CuBit.UI.Controls.Hit (controls, x, y);
         if CuBit.UI.Point_In_Rect (x, y, layout.sidebar) or else
            hit = CONTROL_NAV_SCROLL
         then
            if layout.sidebar.h > 80 then
               visibleRows := (layout.sidebar.h - 80) / 24;
            end if;
            if processCount > visibleRows then
               maxScroll := processCount - visibleRows;
            end if;
            scrollByWheel (navigatorScroll, maxScroll, wheelDelta);
            dirty := CuBit.UI.Union_Rect (dirty, layout.sidebar);
         end if;
      elsif ev.kind = CuBit.UI.App.INPUT_POINTER_DOWN then
         x := unpackLo32 (ev.payload0);
         y := unpackHi32 (ev.payload0);
         CuBit.UI.State.Set_Pointer (ui, x, y, True, pressed => True);
         dirty := CuBit.UI.App.Full_Rect (win);
      elsif ev.kind = CuBit.UI.App.INPUT_POINTER_UP then
         x := unpackLo32 (ev.payload0);
         y := unpackHi32 (ev.payload0);
         layout := computeLayout (CuBit.UI.App.Width (win),
                                  CuBit.UI.App.Height (win));
         hit := CuBit.UI.Controls.Hit (controls, x, y);
         declare
            tab : constant Natural := hitTabIndex (x, y);
         begin
            if tab /= 0 then
               activeTab := tab;
            elsif CuBit.UI.Point_In_Rect (x, y, layout.refresh) then
               refreshCount := refreshCount + 1;
               capCacheValid := False;
               authorityCacheValid := False;
               requestSelectedStreams;
            elsif hit >= CONTROL_TAB_BASE and then
               hit < CONTROL_TAB_BASE + Security_Center_Form.TAB_LABELS'Length
            then
               activeTab := hit - CONTROL_TAB_BASE + 1;
            end if;
         end;
         CuBit.UI.State.Set_Pointer (ui, x, y, False, released => True);
         dirty := CuBit.UI.App.Full_Rect (win);
      end if;
   end handleEvent;

   procedure runUI is new CuBit.UI.App.Run
      (Render       => render,
       Handle_Event => handleEvent);

begin
   debugPrint ("security-center: starting" & LF);

   declare
      ok : Boolean;
      flags : constant Unsigned_64 :=
         CuBit.UI.App.WINDOW_FLAG_DECORATED or
         CuBit.UI.App.WINDOW_FLAG_RESIZABLE or
         CuBit.UI.App.WINDOW_FLAG_MINIMIZABLE or
         CuBit.UI.App.WINDOW_FLAG_MAXIMIZABLE or
         CuBit.UI.App.WINDOW_FLAG_CLOSEABLE;
   begin
      CuBit.UI.App.Open (win, initialW, initialH, flags, ok);
      if not ok then
         debugPrint ("security-center: window open failed" & LF);
         ignore := syscall (SYSCALL_EXIT, 1);
         return;
      end if;
   end;

   runUI (win);

   CuBit.UI.App.Close (win);
   ignore := syscall (SYSCALL_EXIT, 0);
end main;
