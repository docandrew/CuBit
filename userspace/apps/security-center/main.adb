------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Security Center prototype
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with Font8x16;

procedure main is
   use ASCII;

   SYSINFO_FB_WIDTH  : constant Unsigned_64 := 1100;
   SYSINFO_FB_HEIGHT : constant Unsigned_64 := 1101;
   SYSINFO_FB_PITCH  : constant Unsigned_64 := 1102;
   SYSINFO_FB_BPP    : constant Unsigned_64 := 1103;
   SYSINFO_NUM_CPUS  : constant Unsigned_64 := 1400;
   SYSINFO_MEM_FREE  : constant Unsigned_64 := 1600;
   SYSINFO_MEM_TOTAL : constant Unsigned_64 := 1601;

   EVENT_KEYBOARD : constant Unsigned_32 := 1;

   fbWidth  : Natural := 0;
   fbHeight : Natural := 0;
   fbPitch  : Natural := 0;
   fbBpp    : Natural := 0;
   fbAddr   : System.Address := System.Null_Address;
   activeTab : Natural := 0;

   type DirtyState is record
      chrome  : Boolean := True;
      tabs    : Boolean := True;
      side    : Boolean := True;
      metrics : Boolean := True;
      content : Boolean := True;
   end record;

   dirty : DirtyState;

   C_BG        : constant Unsigned_32 := 16#0014_171A#;
   C_PANEL     : constant Unsigned_32 := 16#0021_252A#;
   C_PANEL_2   : constant Unsigned_32 := 16#002B_3036#;
   C_BORDER    : constant Unsigned_32 := 16#0048_515C#;
   C_TEXT      : constant Unsigned_32 := 16#00E8_ECEF#;
   C_MUTED     : constant Unsigned_32 := 16#0098_A2AD#;
   C_ACCENT    : constant Unsigned_32 := 16#0037_B4D8#;
   C_GOOD      : constant Unsigned_32 := 16#0049_C070#;
   C_WARN      : constant Unsigned_32 := 16#00D8_A137#;
   C_PURPLE    : constant Unsigned_32 := 16#0096_7ADC#;

   type Rect is record
      x : Natural;
      y : Natural;
      w : Natural;
      h : Natural;
   end record;

   function layoutMargin return Natural is (24);
   function layoutTop return Natural is (24);
   function layoutLeftW return Natural is (210);
   function layoutBodyY return Natural is (layoutTop + 96);
   function layoutMainX return Natural is
      (layoutMargin + layoutLeftW + 18);
   function layoutMainW return Natural is
      (fbWidth - (layoutMargin * 2) - layoutLeftW - 18);

   procedure invalidateAll is
   begin
      dirty := (chrome  => True,
                tabs    => True,
                side    => True,
                metrics => True,
                content => True);
   end invalidateAll;

   procedure invalidateView is
   begin
      dirty.tabs := True;
      dirty.side := True;
      dirty.content := True;
   end invalidateView;

   procedure putPixel (x, y : Natural; color : Unsigned_32) is
      offset : constant Storage_Offset :=
         Storage_Offset (y * fbPitch + x * 4);
      pixel : Unsigned_32 with Import, Address => fbAddr + offset;
   begin
      if x < fbWidth and then y < fbHeight then
         pixel := color;
      end if;
   end putPixel;

   procedure fillRect
      (x, y, w, h : Natural; color : Unsigned_32)
   is
      maxX : Natural := x + w;
      maxY : Natural := y + h;
   begin
      if x >= fbWidth or else y >= fbHeight then
         return;
      end if;

      if maxX > fbWidth then
         maxX := fbWidth;
      end if;
      if maxY > fbHeight then
         maxY := fbHeight;
      end if;

      for yy in y .. maxY - 1 loop
         for xx in x .. maxX - 1 loop
            putPixel (xx, yy, color);
         end loop;
      end loop;
   end fillRect;

   procedure fillRect (r : Rect; color : Unsigned_32) is
   begin
      fillRect (r.x, r.y, r.w, r.h, color);
   end fillRect;

   procedure strokeRect
      (x, y, w, h : Natural; color : Unsigned_32)
   is
   begin
      if w = 0 or else h = 0 then
         return;
      end if;

      fillRect (x, y, w, 1, color);
      fillRect (x, y + h - 1, w, 1, color);
      fillRect (x, y, 1, h, color);
      fillRect (x + w - 1, y, 1, h, color);
   end strokeRect;

   procedure drawGlyph
      (x, y : Natural;
       ch   : Character;
       fg   : Unsigned_32;
       bg   : Unsigned_32)
   is
      glyph : Font8x16.GlyphData renames Font8x16.font (Character'Pos (ch));
   begin
      for row in 0 .. Font8x16.GLYPH_HEIGHT - 1 loop
         declare
            bits : constant Unsigned_8 := glyph (row);
         begin
            for bit in 0 .. Font8x16.GLYPH_WIDTH - 1 loop
               if (bits and Shift_Right (16#80#, bit)) /= 0 then
                  putPixel (x + bit, y + row, fg);
               else
                  putPixel (x + bit, y + row, bg);
               end if;
            end loop;
         end;
      end loop;
   end drawGlyph;

   procedure drawText
      (x, y : Natural;
       s    : String;
       fg   : Unsigned_32;
       bg   : Unsigned_32)
   is
      cx : Natural := x;
   begin
      for i in s'Range loop
         if cx + Font8x16.GLYPH_WIDTH <= fbWidth then
            drawGlyph (cx, y, s (i), fg, bg);
         end if;
         cx := cx + Font8x16.GLYPH_WIDTH;
      end loop;
   end drawText;

   procedure drawUnsigned
      (x, y  : Natural;
       value : Unsigned_64;
       fg    : Unsigned_32;
       bg    : Unsigned_32)
   is
      buf : String (1 .. 20);
      pos : Natural := buf'Last;
      v   : Unsigned_64 := value;
   begin
      if v = 0 then
         drawText (x, y, "0", fg, bg);
         return;
      end if;

      while v > 0 and then pos >= buf'First loop
         buf (pos) :=
            Character'Val (Character'Pos ('0') + Natural (v mod 10));
         v := v / 10;
         exit when pos = buf'First;
         pos := pos - 1;
      end loop;

      drawText (x, y, buf (pos .. buf'Last), fg, bg);
   end drawUnsigned;

   procedure drawCard
      (x, y, w, h : Natural; title : String)
   is
   begin
      fillRect (x, y, w, h, C_PANEL);
      strokeRect (x, y, w, h, C_BORDER);
      fillRect (x, y, w, 28, C_PANEL_2);
      drawText (x + 12, y + 7, title, C_TEXT, C_PANEL_2);
   end drawCard;

   procedure drawPill
      (x, y, w : Natural; label : String; color : Unsigned_32)
   is
   begin
      fillRect (x, y, w, 24, color);
      drawText (x + 10, y + 5, label, C_TEXT, color);
   end drawPill;

   procedure drawTab
      (x, y, w : Natural; label : String; active : Boolean)
   is
      bg : constant Unsigned_32 := (if active then C_ACCENT else C_PANEL_2);
      fg : constant Unsigned_32 := (if active then C_TEXT else C_MUTED);
   begin
      fillRect (x, y, w, 32, bg);
      strokeRect (x, y, w, 32, (if active then C_ACCENT else C_BORDER));
      drawText (x + 12, y + 8, label, fg, bg);
   end drawTab;

   procedure drawMetricUnsigned
      (x, y : Natural; label : String; value : Unsigned_64; color : Unsigned_32)
   is
   begin
      fillRect (x, y, 150, 64, C_PANEL_2);
      strokeRect (x, y, 150, 64, C_BORDER);
      drawText (x + 12, y + 10, label, C_MUTED, C_PANEL_2);
      drawUnsigned (x + 12, y + 34, value, color, C_PANEL_2);
   end drawMetricUnsigned;

   procedure drawRow
      (x, y, w : Natural;
       a, b, c, d : String;
       bg : Unsigned_32)
   is
   begin
      fillRect (x, y, w, 24, bg);
      drawText (x + 10,  y + 4, a, C_TEXT,  bg);
      drawText (x + 82,  y + 4, b, C_MUTED, bg);
      drawText (x + 184, y + 4, c, C_TEXT,  bg);
      drawText (x + 330, y + 4, d, C_MUTED, bg);
   end drawRow;

   function serviceCount return Unsigned_64 is
      count : Unsigned_64 := 0;
   begin
      for id in 1 .. 14 loop
         if getInfo (SYSINFO_REGISTERED_DRIVER, Unsigned_64 (id)) /= 0 then
            count := count + 1;
         end if;
      end loop;
      return count;
   end serviceCount;

   function memMiB (bytes : Unsigned_64) return Unsigned_64 is
   begin
      return bytes / (1024 * 1024);
   end memMiB;

   procedure drawSidePanel
      (x, y, w, h : Natural)
   is
   begin
      drawCard (x, y, w, h, "Session");
      drawText (x + 14, y + 48, "mode", C_MUTED, C_PANEL);
      drawText (x + 90, y + 48, "direct fb", C_TEXT, C_PANEL);
      drawText (x + 14, y + 76, "active", C_MUTED, C_PANEL);
      case activeTab is
         when 0 => drawText (x + 90, y + 76, "processes", C_TEXT, C_PANEL);
         when 1 => drawText (x + 90, y + 76, "services", C_TEXT, C_PANEL);
         when 2 => drawText (x + 90, y + 76, "caps", C_TEXT, C_PANEL);
         when 3 => drawText (x + 90, y + 76, "ipc", C_TEXT, C_PANEL);
         when 4 => drawText (x + 90, y + 76, "streams", C_TEXT, C_PANEL);
         when others => drawText (x + 90, y + 76, "launch", C_TEXT, C_PANEL);
      end case;
      drawText (x + 14, y + 104, "keys", C_MUTED, C_PANEL);
      drawText (x + 14, y + 128, "Tab / 1-6 switch", C_TEXT, C_PANEL);
      drawText (x + 14, y + 152, "Q or Esc exits", C_TEXT, C_PANEL);
      drawText (x + 14, y + 196, "next", C_MUTED, C_PANEL);
      drawText (x + 14, y + 220, "input routing", C_TEXT, C_PANEL);
      drawText (x + 14, y + 244, "live cap tables", C_TEXT, C_PANEL);
      drawText (x + 14, y + 268, "desktop.svc", C_TEXT, C_PANEL);
   end drawSidePanel;

   procedure drawProcesses
      (x, y, w : Natural)
   is
   begin
      drawCard (x, y, w, 190, "Processes");
      drawRow (x + 14, y + 40, w - 28,
               "PID", "STATE", "NAME", "AUTHORITY", C_PANEL_2);
      drawRow (x + 14, y + 68, w - 28,
               "1", "READY", "procmgr.svc", "spawn / mint", C_PANEL);
      drawRow (x + 14, y + 96, w - 28,
               "2", "READY", "filesystem.svc", "@nvme / @ata", C_PANEL_2);
      drawRow (x + 14, y + 124, w - 28,
               "3", "READY", "shell.app", "fb / procmgr", C_PANEL);
      drawRow (x + 14, y + 152, w - 28,
               "?", "RUNNING", "security-center", "framebuffer", C_PANEL_2);

      drawCard (x, y + 208, w, 150, "Capability Actions");
      drawText (x + 18, y + 252,
                "Open file -> mint scoped file capability",
                C_TEXT, C_PANEL);
      drawText (x + 18, y + 280,
                "Disable network -> revoke or withhold @net",
                C_TEXT, C_PANEL);
      drawText (x + 18, y + 308,
                "Launch app -> choose authority profile first",
                C_TEXT, C_PANEL);
   end drawProcesses;

   procedure drawServices
      (x, y, w : Natural)
   is
      rowY : Natural := y + 40;
      pid  : Unsigned_64;
   begin
      drawCard (x, y, w, 360, "Registered Services");
      drawRow (x + 14, rowY, w - 28,
               "ID", "PID", "ROLE", "STATUS", C_PANEL_2);
      rowY := rowY + 28;

      for id in 1 .. 14 loop
         pid := getInfo (SYSINFO_REGISTERED_DRIVER, Unsigned_64 (id));
         if pid /= 0 then
            declare
               rowBg : constant Unsigned_32 :=
                  (if (id mod 2) = 0 then C_PANEL else C_PANEL_2);
            begin
               fillRect (x + 14, rowY, w - 28, 24, rowBg);
               drawUnsigned (x + 24, rowY + 4, Unsigned_64 (id), C_TEXT,
                             rowBg);
               drawUnsigned (x + 96, rowY + 4, pid, C_MUTED, rowBg);
               case id is
                  when 1  => drawText (x + 198, rowY + 4, "keyboard", C_TEXT, rowBg);
                  when 2  => drawText (x + 198, rowY + 4, "ata", C_TEXT, rowBg);
                  when 3  => drawText (x + 198, rowY + 4, "netstack", C_TEXT, rowBg);
                  when 4  => drawText (x + 198, rowY + 4, "procmgr", C_TEXT, rowBg);
                  when 5  => drawText (x + 198, rowY + 4, "nvme", C_TEXT, rowBg);
                  when 6  => drawText (x + 198, rowY + 4, "filesystem", C_TEXT, rowBg);
                  when 7  => drawText (x + 198, rowY + 4, "devmgr", C_TEXT, rowBg);
                  when 8  => drawText (x + 198, rowY + 4, "hda", C_TEXT, rowBg);
                  when 9  => drawText (x + 198, rowY + 4, "mixer", C_TEXT, rowBg);
                  when 10 => drawText (x + 198, rowY + 4, "mouse", C_TEXT, rowBg);
                  when 11 => drawText (x + 198, rowY + 4, "config", C_TEXT, rowBg);
                  when 12 => drawText (x + 198, rowY + 4, "netmgr", C_TEXT, rowBg);
                  when 13 => drawText (x + 198, rowY + 4, "logstore", C_TEXT, rowBg);
                  when others => drawText (x + 198, rowY + 4, "test", C_TEXT, rowBg);
               end case;
               drawText (x + 344, rowY + 4, "registered", C_GOOD, rowBg);
            end;
            rowY := rowY + 28;
         end if;
      end loop;
   end drawServices;

   procedure drawPlaceholder
      (x, y, w : Natural; title, text : String; color : Unsigned_32)
   is
   begin
      drawCard (x, y, w, 230, title);
      drawText (x + 18, y + 54, text, C_TEXT, C_PANEL);
      drawText (x + 18, y + 86,
                "This view is intentionally protocol-shaped before the",
                C_MUTED, C_PANEL);
      drawText (x + 18, y + 110,
                "compositor exists. The backend can land behind it.",
                C_MUTED, C_PANEL);
      fillRect (x + 18, y + 154, 180, 28, color);
      drawText (x + 30, y + 160, "planned backend", C_TEXT, color);
   end drawPlaceholder;

   procedure paintChrome is
      margin : constant Natural := layoutMargin;
      top    : constant Natural := layoutTop;
   begin
      fillRect (0, 0, fbWidth, fbHeight, C_BG);

      drawText (margin, top, "CuBit Security Center", C_TEXT, C_BG);
      drawText (margin, top + 22,
                "direct framebuffer prototype / future desktop client",
                C_MUTED, C_BG);

      drawPill (fbWidth - 300, top, 118, "IPC ASYNC", C_GOOD);
      drawPill (fbWidth - 170, top, 126, "CAPABILITY", C_ACCENT);
   end paintChrome;

   procedure paintTabs is
      margin : constant Natural := layoutMargin;
      top    : constant Natural := layoutTop;
      tabArea : constant Rect := (x => margin,
                                  y => top + 56,
                                  w => 650,
                                  h => 34);
   begin
      fillRect (tabArea, C_BG);
      drawTab (margin, top + 56, 104, "Processes", activeTab = 0);
      drawTab (margin + 112, top + 56, 92, "Services", activeTab = 1);
      drawTab (margin + 212, top + 56, 116, "Caps", activeTab = 2);
      drawTab (margin + 336, top + 56, 76, "IPC", activeTab = 3);
      drawTab (margin + 420, top + 56, 100, "Streams", activeTab = 4);
      drawTab (margin + 528, top + 56, 108, "Launch", activeTab = 5);
   end paintTabs;

   procedure paintSidePanel is
      margin : constant Natural := layoutMargin;
      bodyY  : constant Natural := layoutBodyY;
      panel  : constant Rect := (x => margin,
                                 y => bodyY,
                                 w => layoutLeftW,
                                 h => fbHeight - bodyY - margin);
   begin
      fillRect (panel, C_BG);
      drawSidePanel (panel.x, panel.y, panel.w, panel.h);
   end paintSidePanel;

   procedure paintMetrics is
      mainX  : constant Natural := layoutMainX;
      mainW  : constant Natural := layoutMainW;
      bodyY  : constant Natural := layoutBodyY;
      area   : constant Rect := (x => mainX,
                                 y => bodyY,
                                 w => mainW,
                                 h => 112);
   begin
      fillRect (area, C_BG);
      drawCard (mainX, bodyY, mainW, 112, "System Snapshot");
      drawMetricUnsigned (mainX + 18,  bodyY + 38, "CPUs",
                          getInfo (SYSINFO_NUM_CPUS), C_GOOD);
      drawMetricUnsigned (mainX + 184, bodyY + 38, "Services",
                          serviceCount, C_ACCENT);
      drawMetricUnsigned (mainX + 350, bodyY + 38, "Mem Free",
                          memMiB (getInfo (SYSINFO_MEM_FREE)), C_WARN);
      drawText (mainX + 418, bodyY + 72, "MiB", C_WARN, C_PANEL_2);
      drawMetricUnsigned (mainX + 516, bodyY + 38, "Mem Total",
                          memMiB (getInfo (SYSINFO_MEM_TOTAL)), C_PURPLE);
      drawText (mainX + 584, bodyY + 72, "MiB", C_PURPLE, C_PANEL_2);
   end paintMetrics;

   procedure paintContent is
      margin : constant Natural := layoutMargin;
      mainX  : constant Natural := layoutMainX;
      mainW  : constant Natural := layoutMainW;
      bodyY  : constant Natural := layoutBodyY;
      area   : constant Rect := (x => mainX,
                                 y => bodyY + 130,
                                 w => mainW,
                                 h => fbHeight - (bodyY + 130) - margin);
   begin
      fillRect (area, C_BG);
      case activeTab is
         when 0 =>
            drawProcesses (mainX, bodyY + 130, mainW);
         when 1 =>
            drawServices (mainX, bodyY + 130, mainW);
         when 2 =>
            drawPlaceholder (mainX, bodyY + 130, mainW, "Capabilities",
                             "Per-process cap tables will land here.",
                             C_ACCENT);
         when 3 =>
            drawPlaceholder (mainX, bodyY + 130, mainW, "IPC",
                             "Recent sends, completions, and blocked waits.",
                             C_GOOD);
         when 4 =>
            drawPlaceholder (mainX, bodyY + 130, mainW, "Streams",
                             "Subscribe to stdout, logs, metrics, health.",
                             C_PURPLE);
         when others =>
            drawPlaceholder (mainX, bodyY + 130, mainW, "Launcher",
                             "Spawn apps with selected authority profiles.",
                             C_WARN);
      end case;
   end paintContent;

   procedure paintDirty is
   begin
      if dirty.chrome then
         paintChrome;
         dirty.chrome := False;
         dirty.tabs := True;
         dirty.side := True;
         dirty.metrics := True;
         dirty.content := True;
      end if;

      if dirty.tabs then
         paintTabs;
         dirty.tabs := False;
      end if;

      if dirty.side then
         paintSidePanel;
         dirty.side := False;
      end if;

      if dirty.metrics then
         paintMetrics;
         dirty.metrics := False;
      end if;

      if dirty.content then
         paintContent;
         dirty.content := False;
      end if;
   end paintDirty;

   procedure handleKey (raw : Unsigned_8; running : in out Boolean) is
      release : constant Boolean := (raw and 16#80#) /= 0;
      code    : constant Unsigned_8 := raw and 16#7F#;
   begin
      if release then
         return;
      end if;

      case code is
         when 16#01# => -- Esc
            running := False;
         when 16#10# => -- Q
            running := False;
         when 16#0F# => -- Tab
            if activeTab = 5 then
               activeTab := 0;
            else
               activeTab := activeTab + 1;
            end if;
            invalidateView;
         when 16#02# => activeTab := 0; invalidateView; -- 1
         when 16#03# => activeTab := 1; invalidateView; -- 2
         when 16#04# => activeTab := 2; invalidateView; -- 3
         when 16#05# => activeTab := 3; invalidateView; -- 4
         when 16#06# => activeTab := 4; invalidateView; -- 5
         when 16#07# => activeTab := 5; invalidateView; -- 6
         when others =>
            null;
      end case;
   end handleKey;

   ret : Unsigned_64;
   eventMsg : Message;
   running : Boolean := True;
   idleTicks : Natural := 0;
begin
   debugPrint ("security-center: starting" & LF);

   ret := syscall (SYSCALL_MAPFB);
   if ret = Unsigned_64'Last then
      debugPrint ("security-center: MAPFB failed" & LF);
      ret := syscall (SYSCALL_EXIT, 1);
      return;
   end if;

   fbAddr   := To_Address (Integer_Address (ret));
   fbWidth  := Natural (getInfo (SYSINFO_FB_WIDTH));
   fbHeight := Natural (getInfo (SYSINFO_FB_HEIGHT));
   fbPitch  := Natural (getInfo (SYSINFO_FB_PITCH));
   fbBpp    := Natural (getInfo (SYSINFO_FB_BPP));

   if fbWidth < 640 or else fbHeight < 360 or else fbBpp /= 32 then
      debugPrint ("security-center: unsupported framebuffer" & LF);
      ret := syscall (SYSCALL_EXIT, 1);
      return;
   end if;

   ret := registerDriver (DRIVER_KEYBOARD);
   debugPrint ("" & LF);

   debugPrint ("security-center: frame drawn" & LF);
   invalidateAll;

   while running loop
      paintDirty;

      foundPoll : declare
         found : constant Boolean := Poll_Event (eventMsg);
      begin
         if found and then eventMsg.tag.label = EVENT_KEYBOARD then
            handleKey (Unsigned_8 (eventMsg.words (0) and 16#FF#), running);
         elsif syscall (SYSCALL_SLEEP, 10) = Unsigned_64'Last then
            null;
         else
            idleTicks := idleTicks + 1;
            if idleTicks >= 100 then
               idleTicks := 0;
               dirty.metrics := True;
            end if;
         end if;
      end foundPoll;
   end loop;

   fillRect (0, 0, fbWidth, fbHeight, C_BG);
   if syscall (SYSCALL_EXIT, 0) = Unsigned_64'Last then
      null;
   end if;
end main;
