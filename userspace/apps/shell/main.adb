------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Interactive Shell Application
--
--  Framebuffer console with keyboard input, line editing, and command
--  dispatch. Communicates with procmgr.svc for process spawning.
--
--  Capability slots:
--    4  = CAP_DEVICE_MEM (framebuffer access)
--    12 = CAP_ENDPOINT to procmgr
--    22 = CAP_ENDPOINT to display.svc
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Config;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Streams;
with Font8x16;

procedure main is
   use ASCII;

   --  IPC label constants
   OP_OPEN    : constant Unsigned_32 := 16#0001#;
   OP_CLOSE   : constant Unsigned_32 := 16#0002#;
   OP_READ    : constant Unsigned_32 := 16#0003#;
   OP_WRITE   : constant Unsigned_32 := 16#0004#;
   OP_READDIR : constant Unsigned_32 := 16#0007#;

   --  Open flags
   O_WRONLY   : constant Unsigned_64 := 1;
   O_CREAT    : constant Unsigned_64 := 64;
   O_TRUNC    : constant Unsigned_64 := 512;
   OP_SPAWN   : constant Unsigned_32 := 16#0100#;
   REPLY_OK   : constant Unsigned_32 := 16#F000#;
   REPLY_ERR  : constant Unsigned_32 := 16#F001#;

   --  Network IPC labels
   OP_NET_RESOLVE    : constant Unsigned_32 := 16#0410#;
   OP_NET_IF_DETAIL  : constant Unsigned_32 := 16#0436#;
   OP_NET_ROUTE_LIST : constant Unsigned_32 := 16#0437#;
   OP_NET_PING       : constant Unsigned_32 := 16#0438#;

   --  Display service labels.  The shell still renders into its mapped
   --  framebuffer, but on GPU-backed displays that memory is no longer the
   --  visible scanout.  In that case we grant the buffer to display.svc and
   --  ask it to present dirty rectangles after text redraws.
   OP_DISPLAY_ATTACH_BUFFER : constant Unsigned_32 := 16#0901#;
   OP_DISPLAY_PRESENT_RECT  : constant Unsigned_32 := 16#0902#;
   OP_DISPLAY_GET_STATUS    : constant Unsigned_32 := 16#0904#;
   OP_DISPLAY_ACQUIRE       : constant Unsigned_32 := 16#0905#;
   OP_DISPLAY_RELEASE       : constant Unsigned_32 := 16#0906#;

   DISPLAY_OK                 : constant Unsigned_64 := 0;
   DISPLAY_BACKEND_VIRTIO_GPU : constant Unsigned_64 := 3;

   --  Sysinfo query IDs for framebuffer
   SYSINFO_FB_WIDTH  : constant Unsigned_64 := 1100;
   SYSINFO_FB_HEIGHT : constant Unsigned_64 := 1101;
   SYSINFO_FB_PITCH  : constant Unsigned_64 := 1102;

   --  Sysinfo query IDs for memory
   SYSINFO_MEM_FREE  : constant Unsigned_64 := 1600;
   SYSINFO_MEM_TOTAL : constant Unsigned_64 := 1601;

   --  Framebuffer state
   fbWidth  : Natural := 0;
   fbHeight : Natural := 0;
   fbPitch  : Natural := 0;
   fbAddr   : System.Address := System.Null_Address;

   --  Optional display.svc present bridge.  This is active only when the
   --  display service reports a GPU backend; the linear framebuffer path keeps
   --  drawing directly to visible memory as before.
   displayAttached : Boolean := False;
   displayGrantId  : Unsigned_64 := 0;
   fullPresentNeeded : Boolean := False;

   --  Console grid dimensions
   cols : Natural := 0;
   rows : Natural := 0;

   --  Colors (BGRA format)
   FG_COLOR : constant Unsigned_32 := 16#00FF_FF00#;  -- green
   BG_COLOR : constant Unsigned_32 := 16#0000_0000#;  -- black

   --  Console state
   MAX_COLS : constant := 256;
   MAX_ROWS : constant := 64;

   type ScreenRow is array (0 .. MAX_COLS - 1) of Unsigned_8;
   type ScreenBuf is array (0 .. MAX_ROWS - 1) of ScreenRow;
   type DirtyFlags is array (0 .. MAX_ROWS - 1) of Boolean;

   screen    : ScreenBuf;
   dirty     : DirtyFlags;
   cursorRow : Natural := 0;
   cursorCol : Natural := 0;

   --  Line editing buffer
   LINE_MAX : constant := 256;
   lineBuf  : String (1 .. LINE_MAX);
   lineLen  : Natural := 0;

   --  Keyboard state
   shiftDown : Boolean := False;
   ctrlDown  : Boolean := False;

   --  Foreground child process tracking
   foregroundPID : Unsigned_64 := 0;
   EVENT_CHILD_EXIT : constant Unsigned_32 := 16#0103#;
   EVENT_CAP_FAULT  : constant Unsigned_32 := 16#0104#;

   --  Child stdout stream subscription
   childStream : CuBit.Streams.SubInfo;
   streamSubPending : Boolean := False;  -- async subscribe in flight
   streamDrainPolls : Natural := 0;      -- polls since child exit
   GRANT_REGION_BASE : constant Unsigned_64 := 16#4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096;
   STREAM_SUB_TOKEN  : constant Unsigned_64 := 42;
   STREAM_LIST_TOKEN : constant Unsigned_64 := 43;

   --  Buffer for reading child stream data (global to avoid stack growth)
   streamRdBuf : array (0 .. 511) of Unsigned_8;

   --  procmgr communication
   procmgrPID : ProcessID := NO_PROCESS;
   grantBuf   : System.Address := System.Null_Address;
   grantId    : Unsigned_64 := 0;
   GRANT_BUF_PAGES : constant := 1;

   --  Filesystem communication
   fsBuf       : System.Address := System.Null_Address;
   fsGrantId   : Unsigned_64 := 0;
   fsReady     : Boolean := False;
   FS_BUF_PAGES : constant := 4;  -- 16KB

   --  Netstack communication
   netstackReady : Boolean := False;

   --  Working directory state
   CWD_MAX : constant := 128;
   cwdBuf  : String (1 .. CWD_MAX);
   cwdLen  : Natural := 0;   -- 0 = ramdisk root (legacy)

   ---------------------------------------------------------------------------
   --  PS/2 Set 1 scancode to ASCII translation
   --  bit 7 clear = make (press), bit 7 set = break (release)
   ---------------------------------------------------------------------------
   type ScanTable is array (0 .. 127) of Unsigned_8;

   scancodeNormal : constant ScanTable := (
      16#00# => 0,
      16#01# => 27,    -- Escape
      16#02# => Character'Pos ('1'),
      16#03# => Character'Pos ('2'),
      16#04# => Character'Pos ('3'),
      16#05# => Character'Pos ('4'),
      16#06# => Character'Pos ('5'),
      16#07# => Character'Pos ('6'),
      16#08# => Character'Pos ('7'),
      16#09# => Character'Pos ('8'),
      16#0A# => Character'Pos ('9'),
      16#0B# => Character'Pos ('0'),
      16#0C# => Character'Pos ('-'),
      16#0D# => Character'Pos ('='),
      16#0E# => 8,     -- Backspace
      16#0F# => 9,     -- Tab
      16#10# => Character'Pos ('q'),
      16#11# => Character'Pos ('w'),
      16#12# => Character'Pos ('e'),
      16#13# => Character'Pos ('r'),
      16#14# => Character'Pos ('t'),
      16#15# => Character'Pos ('y'),
      16#16# => Character'Pos ('u'),
      16#17# => Character'Pos ('i'),
      16#18# => Character'Pos ('o'),
      16#19# => Character'Pos ('p'),
      16#1A# => Character'Pos ('['),
      16#1B# => Character'Pos (']'),
      16#1C# => 10,    -- Enter
      16#1D# => 0,     -- Left Ctrl
      16#1E# => Character'Pos ('a'),
      16#1F# => Character'Pos ('s'),
      16#20# => Character'Pos ('d'),
      16#21# => Character'Pos ('f'),
      16#22# => Character'Pos ('g'),
      16#23# => Character'Pos ('h'),
      16#24# => Character'Pos ('j'),
      16#25# => Character'Pos ('k'),
      16#26# => Character'Pos ('l'),
      16#27# => Character'Pos (';'),
      16#28# => Character'Pos ('''),
      16#29# => Character'Pos ('`'),
      16#2A# => 0,     -- Left Shift (handled separately)
      16#2B# => Character'Pos ('\'),
      16#2C# => Character'Pos ('z'),
      16#2D# => Character'Pos ('x'),
      16#2E# => Character'Pos ('c'),
      16#2F# => Character'Pos ('v'),
      16#30# => Character'Pos ('b'),
      16#31# => Character'Pos ('n'),
      16#32# => Character'Pos ('m'),
      16#33# => Character'Pos (','),
      16#34# => Character'Pos ('.'),
      16#35# => Character'Pos ('/'),
      16#36# => 0,     -- Right Shift (handled separately)
      16#37# => Character'Pos ('*'),
      16#38# => 0,     -- Left Alt
      16#39# => Character'Pos (' '),
      others => 0
   );

   scancodeShifted : constant ScanTable := (
      16#02# => Character'Pos ('!'),
      16#03# => Character'Pos ('@'),
      16#04# => Character'Pos ('#'),
      16#05# => Character'Pos ('$'),
      16#06# => Character'Pos ('%'),
      16#07# => Character'Pos ('^'),
      16#08# => Character'Pos ('&'),
      16#09# => Character'Pos ('*'),
      16#0A# => Character'Pos ('('),
      16#0B# => Character'Pos (')'),
      16#0C# => Character'Pos ('_'),
      16#0D# => Character'Pos ('+'),
      16#0E# => 8,     -- Backspace
      16#1A# => Character'Pos ('{'),
      16#1B# => Character'Pos ('}'),
      16#1C# => 10,    -- Enter
      16#27# => Character'Pos (':'),
      16#28# => Character'Pos ('"'),
      16#29# => Character'Pos ('~'),
      16#2B# => Character'Pos ('|'),
      16#33# => Character'Pos ('<'),
      16#34# => Character'Pos ('>'),
      16#35# => Character'Pos ('?'),
      16#39# => Character'Pos (' '),
      others => 0
   );

   ---------------------------------------------------------------------------
   --  printDec - print a small unsigned number in decimal
   ---------------------------------------------------------------------------
   procedure printDec (val : Unsigned_32) is
      buf : String (1 .. 10);
      pos : Natural := buf'Last;
      v   : Unsigned_32 := val;
   begin
      if v = 0 then
         debugPrint ("0");
         return;
      end if;
      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;
      debugPrint (buf (pos + 1 .. buf'Last));
   end printDec;

   procedure printDec64 (val : Unsigned_64) is
      buf : String (1 .. 20);
      pos : Natural := buf'Last;
      v   : Unsigned_64 := val;
   begin
      if v = 0 then
         debugPrint ("0");
         return;
      end if;
      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;
      debugPrint (buf (pos + 1 .. buf'Last));
   end printDec64;

   ---------------------------------------------------------------------------
   --  Framebuffer rendering
   ---------------------------------------------------------------------------

   --  Write a single pixel at (x, y) in 32-bit BGRA
   procedure putPixel (x, y : Natural; color : Unsigned_32) is
      offset : constant Storage_Offset :=
         Storage_Offset (y * fbPitch + x * 4);
      pixel : Unsigned_32 with Import, Address => fbAddr + offset;
   begin
      pixel := color;
   end putPixel;

   --  Render a single glyph at character grid position (col, row)
   procedure renderGlyph (col, row : Natural; ch : Unsigned_8) is
      baseX : constant Natural := col * Font8x16.GLYPH_WIDTH;
      baseY : constant Natural := row * Font8x16.GLYPH_HEIGHT;
      glyph : Font8x16.GlyphData renames Font8x16.font (Natural (ch));
   begin
      for glyphRow in 0 .. Font8x16.GLYPH_HEIGHT - 1 loop
         declare
            bits : constant Unsigned_8 := glyph (glyphRow);
         begin
            for bit in 0 .. 7 loop
               if (bits and Shift_Right (16#80#, bit)) /= 0 then
                  putPixel (baseX + bit, baseY + glyphRow, FG_COLOR);
               else
                  putPixel (baseX + bit, baseY + glyphRow, BG_COLOR);
               end if;
            end loop;
         end;
      end loop;
   end renderGlyph;

   procedure setupDisplayPresent;

   procedure releaseDisplayPresent is
      msg : Message := NULL_MESSAGE;
      tag : MessageTag;
   begin
      msg.tag := (label  => OP_DISPLAY_RELEASE,
                  length => 0,
                  flags  => 0,
                  badge  => 0);
      tag := capCall (CAP_SLOT_DISPLAY, msg);
      msg.tag := tag;
      displayAttached := False;
   end releaseDisplayPresent;

   --  Present a framebuffer rectangle through display.svc.  This is the
   --  narrow bridge that makes the old framebuffer console visible on primary
   --  GPU modes: shell draws bytes, display.svc copies them to scanout.
   procedure presentDisplayRect (x, y, w, h : Natural) is
      msg : Message := NULL_MESSAGE;
      tag : MessageTag;
      retry : Boolean := False;
   begin
      if not displayAttached or else w = 0 or else h = 0 then
         return;
      end if;

      loop
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_DISPLAY_PRESENT_RECT,
                     length => 4,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := Unsigned_64 (x);
         msg.words (1) := Unsigned_64 (y);
         msg.words (2) := Unsigned_64 (w);
         msg.words (3) := Unsigned_64 (h);

         tag := capCall (CAP_SLOT_DISPLAY, msg);
         msg.tag := tag;
         exit when tag.length >= 1 and then msg.words (0) = DISPLAY_OK;

         displayAttached := False;
         exit when retry;
         exit when foregroundPID /= 0;

         --  Another display client, such as desktop.svc, may have temporarily
         --  attached its own buffer. Reattach the CLI buffer once so returning
         --  from a graphical session does not leave the shell drawing into an
         --  invisible stale buffer.
         retry := True;
         setupDisplayPresent;
         exit when not displayAttached;
      end loop;

      if not displayAttached then
         debugPrint ("shell: display present disabled" & LF);
      end if;
   end presentDisplayRect;

   --  Attach the shell's mapped framebuffer to display.svc when the visible
   --  device is GPU-backed.  Linear framebuffer mode intentionally skips this
   --  path so legacy boot and plain VBE behavior remains unchanged.
   procedure setupDisplayPresent is
      status : Message := NULL_MESSAGE;
      attach : Message := NULL_MESSAGE;
      tag    : MessageTag;
      pages  : Unsigned_64;
      ok     : Boolean;
      pid    : Unsigned_64;
      acquire : Message := NULL_MESSAGE;
   begin
      pid := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DISPLAY);
      if pid = 0 or else pid = Unsigned_64'Last then
         debugPrint ("shell: display service unavailable" & LF);
         return;
      end if;
      debugPrint ("shell: display service pid=");
      printDec (Unsigned_32 (pid));
      debugPrint ("" & LF);

      status.tag := (label  => OP_DISPLAY_GET_STATUS,
                     length => 0,
                     flags  => 0,
                     badge  => 0);
      tag := capCall (CAP_SLOT_DISPLAY, status);
      status.tag := tag;

      if tag.length < 2 or else
         status.words (0) /= DISPLAY_BACKEND_VIRTIO_GPU
      then
         debugPrint ("shell: display backend=");
         if tag.length >= 1 then
            printDec (Unsigned_32 (status.words (0)));
         else
            debugPrint ("?");
         end if;
         debugPrint (" no present bridge" & LF);
         return;
      end if;

      acquire.tag := (label  => OP_DISPLAY_ACQUIRE,
                      length => 0,
                      flags  => 0,
                      badge  => 0);
      tag := capCall (CAP_SLOT_DISPLAY, acquire);
      acquire.tag := tag;
      if tag.length < 1 or else acquire.words (0) /= DISPLAY_OK then
         debugPrint ("shell: display acquire denied" & LF);
         return;
      end if;

      pages :=
        (Unsigned_64 (fbPitch) * Unsigned_64 (fbHeight) + 4095) / 4096;

      --  The service only needs to read the shell buffer, but grant it RW for
      --  now because the current shared-memory grant API treats framebuffer
      --  client buffers consistently with other display users.
      createGrantViaCap
        (slot      => CAP_SLOT_DISPLAY,
         localAddr => fbAddr,
         numPages  => Natural (pages),
         readWrite => True,
         grantId   => displayGrantId,
         success   => ok);

      if not ok then
         debugPrint ("shell: display grant failed" & LF);
         return;
      end if;

      attach.tag := (label  => OP_DISPLAY_ATTACH_BUFFER,
                     length => 4,
                     flags  => 0,
                     badge  => 0);
      attach.words (0) := displayGrantId;
      attach.words (1) := Unsigned_64 (fbWidth);
      attach.words (2) := Unsigned_64 (fbHeight);
      attach.words (3) := Unsigned_64 (fbPitch);

      tag := capCall (CAP_SLOT_DISPLAY, attach);
      attach.tag := tag;
      if tag.length >= 1 and then attach.words (0) = DISPLAY_OK then
         displayAttached := True;
         debugPrint ("shell: display buffer attached" & LF);
      else
         debugPrint ("shell: display attach failed" & LF);
         releaseDisplayPresent;
      end if;
   end setupDisplayPresent;

   procedure renderRow (row : Natural) is
   begin
      for col in 0 .. cols - 1 loop
         renderGlyph (col, row, screen (row)(col));
      end loop;
   end renderRow;

   --  Render all dirty lines to framebuffer
   procedure renderDirty is
      anyDirty : Boolean := False;
      minRow   : Natural := 0;
      maxRow   : Natural := 0;
   begin
      for row in 0 .. rows - 1 loop
         if dirty (row) then
            if not anyDirty then
               minRow := row;
               maxRow := row;
               anyDirty := True;
            else
               maxRow := row;
            end if;

            renderRow (row);
            dirty (row) := False;
         end if;
      end loop;

      if anyDirty then
         if fullPresentNeeded or else maxRow - minRow > 8 then
            --  Scrolls, clears, startup redraws, and large command-output
            --  bursts can affect many lines at once. Present the whole text
            --  grid for those cases so the compatibility console remains
            --  visually coherent on GPU-backed displays.
            if displayAttached then
               for row in 0 .. rows - 1 loop
                  renderRow (row);
               end loop;
            end if;
            presentDisplayRect
              (0, 0,
               cols * Font8x16.GLYPH_WIDTH,
               rows * Font8x16.GLYPH_HEIGHT);
            fullPresentNeeded := False;
         else
            --  Common interactive typing should be cheap: one dirty text row
            --  is only a few kilobytes instead of a full 1024x768 transfer.
            --  Include one neighboring row on each side so newline/prompt
            --  transitions cannot leave stale glyph pixels at row boundaries.
            declare
               presentMin : Natural := minRow;
               presentMax : Natural := maxRow;
            begin
               if presentMin > 0 then
                  presentMin := presentMin - 1;
               end if;
               if presentMax + 1 < rows then
                  presentMax := presentMax + 1;
               end if;

               if displayAttached then
                  for row in presentMin .. presentMax loop
                     renderRow (row);
                  end loop;
               end if;
               presentDisplayRect
                 (0,
                  presentMin * Font8x16.GLYPH_HEIGHT,
                  cols * Font8x16.GLYPH_WIDTH,
                  (presentMax - presentMin + 1) * Font8x16.GLYPH_HEIGHT);
            end;
         end if;
      end if;
   end renderDirty;

   ---------------------------------------------------------------------------
   --  Console text output
   ---------------------------------------------------------------------------

   --  Scroll screen up by one line
   procedure scrollUp is
   begin
      for row in 0 .. rows - 2 loop
         screen (row) := screen (row + 1);
         dirty (row) := True;
      end loop;
      --  Clear bottom row
      for col in 0 .. cols - 1 loop
         screen (rows - 1)(col) := Character'Pos (' ');
      end loop;
      dirty (rows - 1) := True;
      fullPresentNeeded := True;
   end scrollUp;

   --  Put a character at current cursor position and advance
   procedure putChar (ch : Character) is
   begin
      if ch = ASCII.LF then
         cursorCol := 0;
         cursorRow := cursorRow + 1;
         if cursorRow >= rows then
            scrollUp;
            cursorRow := rows - 1;
         end if;
      elsif ch = ASCII.CR then
         cursorCol := 0;
      elsif ch = ASCII.BS then
         if cursorCol > 0 then
            cursorCol := cursorCol - 1;
            screen (cursorRow)(cursorCol) := Character'Pos (' ');
            dirty (cursorRow) := True;
         end if;
      else
         if cursorCol >= cols then
            cursorCol := 0;
            cursorRow := cursorRow + 1;
            if cursorRow >= rows then
               scrollUp;
               cursorRow := rows - 1;
            end if;
         end if;
         screen (cursorRow)(cursorCol) := Character'Pos (ch);
         dirty (cursorRow) := True;
         cursorCol := cursorCol + 1;
      end if;
   end putChar;

   --  Print a string to the console
   procedure putStr (s : String) is
   begin
      for i in s'Range loop
         putChar (s (i));
      end loop;
   end putStr;

   --  Print a decimal number to the console
   procedure putDec (val : Unsigned_32) is
      buf : String (1 .. 10);
      pos : Natural := buf'Last;
      v   : Unsigned_32 := val;
   begin
      if v = 0 then
         putChar ('0');
         return;
      end if;
      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;
      putStr (buf (pos + 1 .. buf'Last));
   end putDec;

   function hasSuffix (s, suffix : String) return Boolean is
      offset : Natural;
   begin
      if s'Length < suffix'Length then
         return False;
      end if;

      offset := s'Last - suffix'Length + 1;
      for i in suffix'Range loop
         if s (offset + (i - suffix'First)) /= suffix (i) then
            return False;
         end if;
      end loop;

      return True;
   end hasSuffix;

   function shouldRunInBackground (filename : String) return Boolean is
   begin
      if hasSuffix (filename, "desktop.svc") then
         return False;
      end if;

      --  Long-lived services/drivers should not become the shell foreground
      --  child. A foreground child intentionally steals the command loop until
      --  it exits, which is correct for apps like DOOM and wrong for services
      --  such as desktop.svc.
      return hasSuffix (filename, ".svc") or else hasSuffix (filename, ".drv");
   end shouldRunInBackground;

   function shouldYieldDisplayToChild (filename : String) return Boolean is
   begin
      --  Graphical foreground clients own visibility through desktop.svc. The
      --  CLI shell keeps its old framebuffer mapped, but must not reattach it
      --  to display.svc while the desktop compositor is presenting.
      return hasSuffix (filename, "desktop-shell.app") or else
             hasSuffix (filename, "desktop.svc");
   end shouldYieldDisplayToChild;

   --  Print prompt
   procedure printPrompt is
   begin
      putStr ("cubit ");
      if cwdLen > 0 then
         putStr (cwdBuf (1 .. cwdLen));
      else
         putChar ('/');
      end if;
      putStr ("> ");
      renderDirty;
   end printPrompt;

   ---------------------------------------------------------------------------
   --  Command handling
   ---------------------------------------------------------------------------

   procedure cmdHelp is
   begin
      putStr ("Commands:" & LF);
      putStr ("  inspect <file>   - show app manifest metadata" & LF);
      putStr ("  bg <file>        - spawn process in background" & LF);
      putStr ("  cat <path>       - display file contents" & LF);
      putStr ("  cd [path]        - change working directory" & LF);
      putStr ("  clear            - clear screen" & LF);
      putStr ("  config <cmd>     - config store (get/set/delete/list)" & LF);
      putStr ("  echo <text>      - print text" & LF);
      putStr ("  head [-n N] <f>  - first N lines (default 10)" & LF);
      putStr ("  hexdump <path>   - hex+ASCII file dump" & LF);
      putStr ("  help             - show this help" & LF);
      putStr ("  ifconfig         - show network interface" & LF);
      putStr ("  kill <pid>       - terminate a process" & LF);
      putStr ("  logs             - show recent log entries" & LF);
      putStr ("  ls [path]        - list directory contents" & LF);
      putStr ("  mem              - show memory usage" & LF);
      putStr ("  nslookup <host>  - DNS lookup" & LF);
      putStr ("  ping <ip>        - send ICMP echo requests" & LF);
      putStr ("  volumes          - show storage volumes" & LF);
      putStr ("  ps               - list running processes" & LF);
      putStr ("  pwd              - print working directory" & LF);
      putStr ("  route            - show routing table" & LF);
      putStr ("  spawn <file>     - spawn a new process" & LF);
      putStr ("  streams <pid>    - list process streams" & LF);
      putStr ("  sysinfo          - system information" & LF);
      putStr ("  uptime           - time since boot" & LF);
      putStr ("  wc <path>        - line/word/byte counts" & LF);
      putStr ("  write <p> <text> - write text to file" & LF);
   end cmdHelp;

   procedure cmdClear is
   begin
      for row in 0 .. rows - 1 loop
         for col in 0 .. cols - 1 loop
            screen (row)(col) := Character'Pos (' ');
         end loop;
         dirty (row) := True;
      end loop;
      cursorRow := 0;
      cursorCol := 0;
      fullPresentNeeded := True;
   end cmdClear;

   procedure cmdEcho (arg : String) is
   begin
      putStr (arg);
      putChar (LF);
   end cmdEcho;

   procedure cmdSpawn (filename : String) is
      msg : Message;
      tag : MessageTag;
   begin
      if procmgrPID = NO_PROCESS then
         putStr ("error: procmgr not found" & LF);
         return;
      end if;

      if filename'Length = 0 then
         putStr ("usage: spawn <filename>" & LF);
         return;
      end if;

      --  Write filename into grant buffer with cwd prefix if needed
      declare
         buf : array (0 .. GRANT_BUF_PAGES * 4096 - 1) of Unsigned_8 with
            Import, Address => grantBuf;
         pos : Natural := 0;
         totalLen : Natural;
      begin
         if filename (filename'First) /= '@' and cwdLen > 0 then
            --  Prepend cwd
            for i in 1 .. cwdLen loop
               buf (pos) := Unsigned_8 (Character'Pos (cwdBuf (i)));
               pos := pos + 1;
            end loop;
         end if;
         for i in 0 .. filename'Length - 1 loop
            buf (pos) := Unsigned_8 (
               Character'Pos (filename (filename'First + i)));
            pos := pos + 1;
         end loop;
         totalLen := pos;

         --  Append cwd after filename for sandbox support
         for i in 1 .. cwdLen loop
            buf (totalLen + i - 1) :=
               Unsigned_8 (Character'Pos (cwdBuf (i)));
         end loop;

         --  Send OP_SPAWN to procmgr
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_SPAWN,
                     length => Unsigned_8 (totalLen),
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := grantId;
         msg.words (1) := 5;  -- default priority
         msg.words (2) := 0;  -- no sandbox override from shell
         msg.words (3) := Unsigned_64 (cwdLen);
         tag := capCall (CAP_SLOT_PROCMGR, msg);
      end;

      if tag.label = REPLY_OK then
         putStr ("spawned PID ");
         putDec (Unsigned_32 (msg.words (0)));
         putChar (LF);
         if shouldRunInBackground (filename) then
            renderDirty;
            return;
         end if;

         if shouldYieldDisplayToChild (filename) then
            releaseDisplayPresent;
         else
            renderDirty;
         end if;

         foregroundPID := msg.words (0);
         debugPrint ("shell: foregroundPID set to ");
         printDec (Unsigned_32 (foregroundPID));
         debugPrint ("" & LF);

         --  Submit async subscription to child's stdout stream
         declare
            subMsg : constant Message := (
               tag => (label  => CuBit.Streams.OP_STREAM_SUBSCRIBE,
                       length => 1,
                       flags  => 0,
                       badge  => 0),
               capBadge => 0,
               words    => (0 => Unsigned_64 (CuBit.Streams.STREAM_STDOUT),
                            others => 0));
            ok : Boolean;
         begin
            ok := submit (foregroundPID, subMsg, STREAM_SUB_TOKEN);
            if ok then
               streamSubPending := True;
               streamDrainPolls := 0;
               childStream.active := False;
               debugPrint ("shell: stream subscribe sent" & LF);
            else
               debugPrint ("shell: stream subscribe failed" & LF);
            end if;
         end;
      else
         putStr ("spawn failed" & LF);
      end if;
   end cmdSpawn;

   ---------------------------------------------------------------------------
   --  cmdBg - spawn a process in the background (no foreground tracking)
   ---------------------------------------------------------------------------
   procedure cmdBg (filename : String) is
      msg : Message;
      tag : MessageTag;
   begin
      if procmgrPID = NO_PROCESS then
         putStr ("error: procmgr not found" & LF);
         return;
      end if;

      if filename'Length = 0 then
         putStr ("usage: bg <filename>" & LF);
         return;
      end if;

      --  Write filename into grant buffer with cwd prefix if needed
      declare
         buf : array (0 .. GRANT_BUF_PAGES * 4096 - 1) of Unsigned_8 with
            Import, Address => grantBuf;
         pos : Natural := 0;
         totalLen : Natural;
      begin
         if filename (filename'First) /= '@' and cwdLen > 0 then
            for i in 1 .. cwdLen loop
               buf (pos) := Unsigned_8 (Character'Pos (cwdBuf (i)));
               pos := pos + 1;
            end loop;
         end if;
         for i in 0 .. filename'Length - 1 loop
            buf (pos) := Unsigned_8 (
               Character'Pos (filename (filename'First + i)));
            pos := pos + 1;
         end loop;
         totalLen := pos;

         --  Append cwd after filename for sandbox support
         for i in 1 .. cwdLen loop
            buf (totalLen + i - 1) :=
               Unsigned_8 (Character'Pos (cwdBuf (i)));
         end loop;

         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_SPAWN,
                     length => Unsigned_8 (totalLen),
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := grantId;
         msg.words (1) := 5;
         msg.words (2) := 0;  -- no sandbox override from shell
         msg.words (3) := Unsigned_64 (cwdLen);
         tag := capCall (CAP_SLOT_PROCMGR, msg);
      end;

      if tag.label = REPLY_OK then
         putStr ("[bg] PID ");
         putDec (Unsigned_32 (msg.words (0)));
         putChar (LF);
      else
         putStr ("bg: spawn failed" & LF);
      end if;
   end cmdBg;

   ---------------------------------------------------------------------------
   --  cmdKill - kill a process by PID
   ---------------------------------------------------------------------------
   procedure cmdKill (args : String) is
      pid : Unsigned_64 := 0;
      ch  : Character;
      ret : Unsigned_64;
   begin
      if args'Length = 0 then
         putStr ("usage: kill <pid>" & LF);
         return;
      end if;

      --  Parse decimal PID from args
      for i in args'Range loop
         ch := args (i);
         if ch >= '0' and ch <= '9' then
            pid := pid * 10 +
               Unsigned_64 (Character'Pos (ch) - Character'Pos ('0'));
         else
            putStr ("kill: invalid PID" & LF);
            return;
         end if;
      end loop;

      if pid = 0 or pid > 255 then
         putStr ("kill: PID out of range" & LF);
         return;
      end if;

      ret := killProcess (pid);
      if ret = 0 then
         putStr ("killed PID ");
         putDec (Unsigned_32 (pid));
         putChar (LF);
      else
         putStr ("kill: failed (no permission or invalid PID)" & LF);
      end if;
   end cmdKill;

   ---------------------------------------------------------------------------
   --  resolvePath - write resolved path into fsBuf, return effective length
   --  If path starts with '@' it is absolute (copy verbatim).
   --  If cwdLen=0, pass through as-is (ramdisk legacy).
   --  Otherwise prepend cwd.
   ---------------------------------------------------------------------------
   function resolvePath (path : String) return Natural is
      buf : array (0 .. FS_BUF_PAGES * 4096 - 1) of Character
        with Import, Address => fsBuf;
      pos : Natural := 0;
   begin
      --  Absolute scheme path
      if path'Length > 0 and then path (path'First) = '@' then
         for i in 0 .. path'Length - 1 loop
            buf (pos) := path (path'First + i);
            pos := pos + 1;
         end loop;
         return pos;
      end if;

      --  No cwd set: pass through (ramdisk legacy)
      if cwdLen = 0 then
         for i in 0 .. path'Length - 1 loop
            buf (pos) := path (path'First + i);
            pos := pos + 1;
         end loop;
         return pos;
      end if;

      --  Prepend cwd
      for i in 1 .. cwdLen loop
         buf (pos) := cwdBuf (i);
         pos := pos + 1;
      end loop;

      --  Append path
      for i in 0 .. path'Length - 1 loop
         buf (pos) := path (path'First + i);
         pos := pos + 1;
      end loop;

      return pos;
   end resolvePath;

   procedure cmdCat (path : String) is
      msg : Message;
      tag : MessageTag;
      handle : Unsigned_64;
   begin
      if not fsReady then
         putStr ("error: filesystem not available" & LF);
         return;
      end if;

      if path'Length = 0 then
         putStr ("usage: cat <path>" & LF);
         return;
      end if;

      --  Resolve path against cwd into fsBuf
      declare
         resolvedLen : constant Natural := resolvePath (path);
      begin
         --  OP_OPEN: words(0)=grantId, words(1)=pathLen
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_OPEN,
                     length => 3,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := fsGrantId;
         msg.words (1) := Unsigned_64 (resolvedLen);
         msg.words (2) := 0;
         tag := capCall (CAP_SLOT_FS, msg);
      end;

      if tag.label /= REPLY_OK then
         putStr ("cat: file not found" & LF);
         return;
      end if;

      handle := msg.words (0);

      --  Read loop
      loop
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_READ,
                     length => 3,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := handle;
         msg.words (1) := fsGrantId;
         msg.words (2) := Unsigned_64 (FS_BUF_PAGES * 4096);
         tag := capCall (CAP_SLOT_FS, msg);

         if tag.label /= REPLY_OK then
            exit;
         end if;

         declare
            bytesRead : constant Unsigned_64 := msg.words (0);
         begin
            exit when bytesRead = 0;

            --  Print data from grant buffer
            declare
               data : String (1 .. Natural (bytesRead))
                 with Import, Address => fsBuf;
            begin
               for i in data'Range loop
                  declare
                     ch : constant Character := data (i);
                  begin
                     --  Only print printable ASCII + newline/tab
                     if ch >= ' ' or ch = ASCII.LF or ch = ASCII.HT then
                        putChar (ch);
                     end if;
                  end;
               end loop;
            end;
         end;
      end loop;

      --  OP_CLOSE
      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_CLOSE,
                  length => 1,
                  flags  => 0,
                  badge  => 0);
      msg.words (0) := handle;
      tag := capCall (CAP_SLOT_FS, msg);

      putChar (LF);
      renderDirty;
   end cmdCat;

   procedure cmdLs (path : String) is
      msg : Message;
      tag : MessageTag;
      resolvedLen : Natural;
   begin
      if not fsReady then
         putStr ("error: filesystem not available" & LF);
         return;
      end if;

      --  Resolve path against cwd into fsBuf
      resolvedLen := resolvePath (path);

      --  OP_READDIR: words(0)=grantId, words(1)=pathLen
      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_READDIR,
                  length => 2,
                  flags  => 0,
                  badge  => 0);
      msg.words (0) := fsGrantId;
      msg.words (1) := Unsigned_64 (resolvedLen);
      tag := capCall (CAP_SLOT_FS, msg);

      if tag.label /= REPLY_OK then
         putStr ("ls: cannot list directory" & LF);
         return;
      end if;

      declare
         written : constant Unsigned_64 := msg.words (0);
      begin
         if written > 0 then
            declare
               data : String (1 .. Natural (written))
                 with Import, Address => fsBuf;
            begin
               putStr (data);
            end;
         end if;
      end;
   end cmdLs;

   ---------------------------------------------------------------------------
   --  putDec64 - print an Unsigned_64 in decimal
   ---------------------------------------------------------------------------
   procedure putDec64 (val : Unsigned_64) is
      buf : String (1 .. 20);
      pos : Natural := buf'Last;
      v   : Unsigned_64 := val;
   begin
      if v = 0 then
         putChar ('0');
         return;
      end if;
      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;
      putStr (buf (pos + 1 .. buf'Last));
   end putDec64;

   ---------------------------------------------------------------------------
   --  write <path> <text>   — create/overwrite file with text content
   ---------------------------------------------------------------------------
   procedure cmdWrite (arg : String) is
      msg    : Message;
      tag    : MessageTag;
      handle : Unsigned_64;
      sepIdx : Natural := 0;
   begin
      if not fsReady then
         putStr ("error: filesystem not available" & LF);
         return;
      end if;

      --  Find first space separating path from text
      for i in arg'Range loop
         if arg (i) = ' ' then
            sepIdx := i;
            exit;
         end if;
      end loop;

      if sepIdx = 0 or sepIdx = arg'Last then
         putStr ("usage: write <path> <text>" & LF);
         return;
      end if;

      declare
         path : String renames arg (arg'First .. sepIdx - 1);
         text : String renames arg (sepIdx + 1 .. arg'Last);
         resolvedLen : constant Natural := resolvePath (path);
      begin
         --  OP_OPEN with O_CREAT | O_TRUNC | O_WRONLY
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_OPEN,
                     length => 3,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := fsGrantId;
         msg.words (1) := Unsigned_64 (resolvedLen);
         msg.words (2) := O_CREAT or O_TRUNC or O_WRONLY;
         tag := capCall (CAP_SLOT_FS, msg);

         if tag.label /= REPLY_OK then
            putStr ("write: cannot open file" & LF);
            return;
         end if;

         handle := msg.words (0);

         --  Copy text into grant buffer
         declare
            buf : String (1 .. text'Length)
              with Import, Address => fsBuf;
         begin
            for i in text'Range loop
               buf (i - text'First + 1) := text (i);
            end loop;
         end;

         --  OP_WRITE
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_WRITE,
                     length => 3,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := handle;
         msg.words (1) := fsGrantId;
         msg.words (2) := Unsigned_64 (text'Length);
         tag := capCall (CAP_SLOT_FS, msg);

         if tag.label /= REPLY_OK then
            putStr ("write: write failed" & LF);
         else
            putStr ("wrote ");
            putDec64 (msg.words (0));
            putStr (" bytes" & LF);
         end if;

         --  OP_CLOSE
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_CLOSE,
                     length => 1,
                     flags  => 0,
                     badge  => 0);
         msg.words (0) := handle;
         tag := capCall (CAP_SLOT_FS, msg);
      end;
   end cmdWrite;

   ---------------------------------------------------------------------------
   --  putHex8 - print a byte as 2 hex digits
   ---------------------------------------------------------------------------
   procedure putHex8 (val : Unsigned_8) is
      hexChars : constant String := "0123456789abcdef";
   begin
      putChar (hexChars (Natural (Shift_Right (val, 4)) + 1));
      putChar (hexChars (Natural (val and 16#0F#) + 1));
   end putHex8;

   ---------------------------------------------------------------------------
   --  putHex32 - print a 32-bit value as 8 hex digits
   ---------------------------------------------------------------------------
   procedure putHex32 (val : Unsigned_32) is
   begin
      putHex8 (Unsigned_8 (Shift_Right (val, 24) and 16#FF#));
      putHex8 (Unsigned_8 (Shift_Right (val, 16) and 16#FF#));
      putHex8 (Unsigned_8 (Shift_Right (val, 8) and 16#FF#));
      putHex8 (Unsigned_8 (val and 16#FF#));
   end putHex32;

   ---------------------------------------------------------------------------
   --  putDecSigned - print a signed integer in decimal
   ---------------------------------------------------------------------------
   procedure putDecSigned (val : Integer) is
   begin
      if val < 0 then
         putChar ('-');
         putDec (Unsigned_32 (-(val)));
      else
         putDec (Unsigned_32 (val));
      end if;
   end putDecSigned;

   ---------------------------------------------------------------------------
   --  putPadded - print a string right-padded to a given width
   ---------------------------------------------------------------------------
   procedure putPadded (s : String; width : Natural) is
   begin
      putStr (s);
      if s'Length < width then
         for i in 1 .. width - s'Length loop
            putChar (' ');
         end loop;
      end if;
   end putPadded;

   ---------------------------------------------------------------------------
   --  putDecRight - print a decimal number right-justified in a field
   ---------------------------------------------------------------------------
   procedure putDecRight (val : Unsigned_32; width : Natural) is
      buf : String (1 .. 10);
      pos : Natural := buf'Last;
      v   : Unsigned_32 := val;
      len : Natural;
   begin
      if v = 0 then
         for i in 1 .. width - 1 loop
            putChar (' ');
         end loop;
         putChar ('0');
         return;
      end if;
      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;
      len := buf'Last - pos;
      if len < width then
         for i in 1 .. width - len loop
            putChar (' ');
         end loop;
      end if;
      putStr (buf (pos + 1 .. buf'Last));
   end putDecRight;

   ---------------------------------------------------------------------------
   --  putIP - print dotted-decimal IP from packed Unsigned_64 (low 32 bits)
   ---------------------------------------------------------------------------
   procedure putIP (packed : Unsigned_64) is
   begin
      putDec (Unsigned_32 (packed and 16#FF#));
      putChar ('.');
      putDec (Unsigned_32 (Shift_Right (packed, 8) and 16#FF#));
      putChar ('.');
      putDec (Unsigned_32 (Shift_Right (packed, 16) and 16#FF#));
      putChar ('.');
      putDec (Unsigned_32 (Shift_Right (packed, 24) and 16#FF#));
   end putIP;

   ---------------------------------------------------------------------------
   --  putMAC - print colon-separated hex MAC from packed Unsigned_64
   ---------------------------------------------------------------------------
   procedure putMAC (packed : Unsigned_64) is
   begin
      for i in 0 .. 5 loop
         if i > 0 then
            putChar (':');
         end if;
         putHex8 (Unsigned_8 (Shift_Right (packed, i * 8) and 16#FF#));
      end loop;
   end putMAC;

   ---------------------------------------------------------------------------
   --  parseIP - parse "A.B.C.D" string into packed Unsigned_64
   ---------------------------------------------------------------------------
   procedure parseIP (s      : String;
                      packed : out Unsigned_64;
                      ok     : out Boolean) is
      octet : Natural := 0;
      idx   : Natural := 0;
      bytes : array (0 .. 3) of Unsigned_64 := (others => 0);
   begin
      packed := 0;
      ok := False;

      for i in s'Range loop
         if s (i) = '.' then
            if idx >= 3 or octet > 255 then
               return;
            end if;
            bytes (idx) := Unsigned_64 (octet);
            idx := idx + 1;
            octet := 0;
         elsif s (i) >= '0' and s (i) <= '9' then
            octet := octet * 10 + (Character'Pos (s (i)) -
                                    Character'Pos ('0'));
            if octet > 255 then
               return;
            end if;
         else
            return;
         end if;
      end loop;

      if idx = 3 and octet <= 255 then
         bytes (3) := Unsigned_64 (octet);
         packed := bytes (0) or
                   Shift_Left (bytes (1), 8) or
                   Shift_Left (bytes (2), 16) or
                   Shift_Left (bytes (3), 24);
         ok := True;
      end if;
   end parseIP;

   ---------------------------------------------------------------------------
   --  cmdUptime
   ---------------------------------------------------------------------------
   procedure cmdUptime is
      ms   : Unsigned_64;
      secs : Unsigned_64;
      mins : Unsigned_64;
      hrs  : Unsigned_64;
      days : Unsigned_64;
   begin
      ms   := syscall (SYSCALL_GETTIME);
      days := ms / 86_400_000;
      hrs  := (ms / 3_600_000) mod 24;
      mins := (ms / 60_000) mod 60;
      secs := (ms / 1000) mod 60;

      putStr ("up ");
      if days > 0 then
         putDec64 (days);
         putStr ("d ");
      end if;
      putDec64 (hrs);
      putStr ("h ");
      putDec64 (mins);
      putStr ("m ");
      putDec64 (secs);
      putStr ("s" & LF);
   end cmdUptime;

   ---------------------------------------------------------------------------
   --  cmdMem - show memory usage
   ---------------------------------------------------------------------------
   procedure cmdMem is
      total : Unsigned_64;
      free  : Unsigned_64;
      used  : Unsigned_64;
   begin
      total := getInfo (SYSINFO_MEM_TOTAL);
      free  := getInfo (SYSINFO_MEM_FREE);

      if total = Unsigned_64'Last or free = Unsigned_64'Last then
         putStr ("error: memory info not available" & LF);
         return;
      end if;

      used := total - free;

      putStr ("Total:  ");
      putDec64 (total / 1024 / 1024);
      putStr (" MB" & LF);
      putStr ("Used:   ");
      putDec64 (used / 1024 / 1024);
      putStr (" MB" & LF);
      putStr ("Free:   ");
      putDec64 (free / 1024 / 1024);
      putStr (" MB" & LF);
   end cmdMem;

   ---------------------------------------------------------------------------
   --  cmdSysinfo
   ---------------------------------------------------------------------------
   SYSINFO_NUM_CPUS  : constant Unsigned_64 := 1400;
   SYSINFO_FB_BPP    : constant Unsigned_64 := 1103;

   procedure cmdSysinfo is
      val : Unsigned_64;
   begin
      --  CPU count
      val := getInfo (SYSINFO_NUM_CPUS);
      putStr ("CPUs:         ");
      putDec (Unsigned_32 (val));
      putChar (LF);

      --  Framebuffer
      putStr ("Framebuffer:  ");
      putDec (Unsigned_32 (getInfo (SYSINFO_FB_WIDTH)));
      putChar ('x');
      putDec (Unsigned_32 (getInfo (SYSINFO_FB_HEIGHT)));
      putStr (" pitch=");
      putDec (Unsigned_32 (getInfo (SYSINFO_FB_PITCH)));
      putStr (" bpp=");
      putDec (Unsigned_32 (getInfo (SYSINFO_FB_BPP)));
      putChar (LF);

      --  Registered drivers
      putStr ("Drivers:" & LF);
      declare
         type DriverInfo is record
            id   : Unsigned_64;
            name : String (1 .. 12);
            nlen : Natural;
         end record;
         type DriverTable is array (1 .. 5) of DriverInfo;
         drivers : constant DriverTable := (
            (1, "keyboard    ", 8),
            (2, "ata         ", 3),
            (3, "netstack    ", 8),
            (4, "procmgr     ", 7),
            (5, "nvme        ", 4));
         pid : Unsigned_64;
      begin
         for d of drivers loop
            pid := getInfo (SYSINFO_REGISTERED_DRIVER, d.id);
            if pid /= 0 and pid /= Unsigned_64'Last then
               putStr ("  ");
               putStr (d.name (1 .. d.nlen));
               putStr (" pid=");
               putDec (Unsigned_32 (pid));
               putChar (LF);
            end if;
         end loop;
      end;
   end cmdSysinfo;

   ---------------------------------------------------------------------------
   --  cmdVolumes - show available storage volumes
   ---------------------------------------------------------------------------
   procedure cmdVolumes is
      pid : Unsigned_64;
   begin
      putStr ("SCHEME      DRIVER  PID" & LF);

      --  Ramdisk is always available if FS is up
      if fsReady then
         declare
            fsPid : constant Unsigned_64 :=
               getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_FS);
         begin
            putStr ("(ramdisk)   cpio     ");
            putDec (Unsigned_32 (fsPid));
            putChar (LF);
         end;
      end if;

      --  ATA
      pid := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_ATA);
      if pid /= 0 and pid /= Unsigned_64'Last then
         putStr ("@ata:0/     ext2     ");
         putDec (Unsigned_32 (pid));
         putChar (LF);
      end if;

      --  NVMe
      pid := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_NVME);
      if pid /= 0 and pid /= Unsigned_64'Last then
         putStr ("@nvme:0/    ext2     ");
         putDec (Unsigned_32 (pid));
         putChar (LF);
      end if;
   end cmdVolumes;

   ---------------------------------------------------------------------------
   --  cmdPs
   ---------------------------------------------------------------------------
   SYSCALL_PROCLIST : constant Unsigned_64 := 71;
   PS_BUF_SIZE : constant := 8192;  -- 256 entries x 32 bytes

   procedure cmdPs is
      psBuf   : System.Address;
      ret     : Unsigned_64;
      count   : Unsigned_64;

      type StateNames is array (0 .. 11) of String (1 .. 10);
      stateTable : constant StateNames := (
         "INVALID   ",
         "READY     ",
         "RUNNING   ",
         "SLEEPING  ",
         "WAITING   ",
         "WAITEVT   ",
         "SENDING   ",
         "RECEIVING ",
         "WAITREPLY ",
         "WAITCOMP  ",
         "WAITNOTIF ",
         "SUSPENDED ");
   begin
      --  Allocate buffer via sbrk
      ret := syscall (SYSCALL_SBRK, PS_BUF_SIZE);
      if ret = Unsigned_64'Last then
         putStr ("ps: sbrk failed" & LF);
         return;
      end if;
      psBuf := To_Address (Integer_Address (ret));

      --  Call SYSCALL_PROCLIST
      count := syscall (SYSCALL_PROCLIST,
                        Unsigned_64 (To_Integer (psBuf)),
                        PS_BUF_SIZE);

      --  Print header
      putStr ("PID  NAME              STATE      CPU  PRI   MEM" & LF);
      putStr ("---  ----              -----      ---  ---   ---" & LF);

      --  Parse entries, skip unnamed SUSPENDED processes
      for i in 0 .. count - 1 loop
         declare
            offset : constant Storage_Offset := Storage_Offset (i * 32);
            entryAddr : constant System.Address := psBuf + offset;

            pidVal : Unsigned_16 with Import, Address => entryAddr;
            stateVal : Unsigned_8 with Import, Address => entryAddr + 2;
            cpuVal : Unsigned_8 with Import, Address => entryAddr + 3;
            priVal : Unsigned_16 with Import, Address => entryAddr + 4;
            nameField : String (1 .. 16) with Import, Address => entryAddr + 8;
            framesVal : Unsigned_32 with Import, Address => entryAddr + 24;

            stateIdx : Natural;
            nameEnd  : Natural := 0;
            pri      : Integer;
            memKB    : Unsigned_32;
         begin
            --  Name: find end (trim spaces/nulls)
            for j in reverse 1 .. 16 loop
               if nameField (j) /= ' ' and
                  nameField (j) /= Character'Val (0)
               then
                  nameEnd := j;
                  exit;
               end if;
            end loop;

            --  Skip unnamed SUSPENDED entries (reserved but unused slots)
            if nameEnd = 0 and Natural (stateVal) = 11 then
               goto Next_Entry;
            end if;

            --  PID right-justified in 3 chars
            putDecRight (Unsigned_32 (pidVal), 3);
            putStr ("  ");

            if nameEnd > 0 then
               putPadded (nameField (1 .. nameEnd), 18);
            else
               putPadded ("(unnamed)", 18);
            end if;

            --  State
            stateIdx := Natural (stateVal);
            if stateIdx <= 11 then
               putStr (stateTable (stateIdx));
            else
               putStr ("???       ");
            end if;
            putStr ("  ");

            --  CPU
            putDecRight (Unsigned_32 (cpuVal), 2);
            putStr ("  ");

            --  Priority (signed)
            pri := Integer (Integer_16 (priVal));
            putDecSigned (pri);

            --  Memory (frames * 4 = KB)
            memKB := framesVal * 4;
            putStr ("  ");
            putDecRight (memKB, 5);
            putChar ('K');

            putChar (LF);
         <<Next_Entry>>
         end;
      end loop;
   end cmdPs;

   ---------------------------------------------------------------------------
   --  cmdHexdump
   ---------------------------------------------------------------------------
   procedure cmdHexdump (path : String) is
      msg    : Message;
      tag    : MessageTag;
      handle : Unsigned_64;
      fileOffset : Unsigned_32 := 0;
   begin
      if not fsReady then
         putStr ("error: filesystem not available" & LF);
         return;
      end if;

      if path'Length = 0 then
         putStr ("usage: hexdump <path>" & LF);
         return;
      end if;

      --  Resolve path against cwd into fsBuf
      declare
         resolvedLen : constant Natural := resolvePath (path);
      begin
         --  OP_OPEN
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_OPEN,
                     length => 3, flags => 0, badge => 0);
         msg.words (0) := fsGrantId;
         msg.words (1) := Unsigned_64 (resolvedLen);
         msg.words (2) := 0;
         tag := capCall (CAP_SLOT_FS, msg);
      end;

      if tag.label /= REPLY_OK then
         putStr ("hexdump: file not found" & LF);
         return;
      end if;

      handle := msg.words (0);

      --  Read and dump loop
      loop
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_READ,
                     length => 3, flags => 0, badge => 0);
         msg.words (0) := handle;
         msg.words (1) := fsGrantId;
         msg.words (2) := Unsigned_64 (FS_BUF_PAGES * 4096);
         tag := capCall (CAP_SLOT_FS, msg);

         exit when tag.label /= REPLY_OK;

         declare
            bytesRead : constant Natural := Natural (msg.words (0));
            data : array (0 .. bytesRead - 1) of Unsigned_8
               with Import, Address => fsBuf;
            pos : Natural := 0;
            lineLen : Natural;
         begin
            exit when bytesRead = 0;

            while pos < bytesRead loop
               --  Bytes remaining on this line
               if bytesRead - pos >= 16 then
                  lineLen := 16;
               else
                  lineLen := bytesRead - pos;
               end if;

               --  Offset
               putHex32 (fileOffset);
               putStr ("  ");

               --  Hex bytes
               for j in 0 .. 15 loop
                  if j < lineLen then
                     putHex8 (data (pos + j));
                  else
                     putStr ("  ");
                  end if;
                  if j = 7 then
                     putChar (' ');
                  end if;
                  putChar (' ');
               end loop;

               --  ASCII
               putStr (" |");
               for j in 0 .. lineLen - 1 loop
                  declare
                     b : constant Unsigned_8 := data (pos + j);
                  begin
                     if b >= 32 and b <= 126 then
                        putChar (Character'Val (Natural (b)));
                     else
                        putChar ('.');
                     end if;
                  end;
               end loop;
               --  Pad ASCII field if short line
               for j in lineLen .. 15 loop
                  putChar (' ');
               end loop;
               putStr ("|" & LF);

               pos := pos + lineLen;
               fileOffset := fileOffset + Unsigned_32 (lineLen);
            end loop;
         end;
      end loop;

      --  OP_CLOSE
      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_CLOSE,
                  length => 1, flags => 0, badge => 0);
      msg.words (0) := handle;
      tag := capCall (CAP_SLOT_FS, msg);

      renderDirty;
   end cmdHexdump;

   ---------------------------------------------------------------------------
   --  cmdHead
   ---------------------------------------------------------------------------
   procedure cmdHead (args : String) is
      msg      : Message;
      tag      : MessageTag;
      handle   : Unsigned_64;
      maxLines : Natural := 10;
      lineCount : Natural := 0;
      pathStart : Natural := args'First;
   begin
      if not fsReady then
         putStr ("error: filesystem not available" & LF);
         return;
      end if;

      if args'Length = 0 then
         putStr ("usage: head [-n N] <path>" & LF);
         return;
      end if;

      --  Parse "-n N" prefix
      if args'Length > 3 and then
         args (args'First) = '-' and then
         args (args'First + 1) = 'n' and then
         args (args'First + 2) = ' '
      then
         --  Find number end
         declare
            numStart : constant Natural := args'First + 3;
            numEnd   : Natural := numStart;
            n        : Natural := 0;
         begin
            --  Skip to space after number
            while numEnd <= args'Last and then
                  args (numEnd) /= ' '
            loop
               numEnd := numEnd + 1;
            end loop;

            --  Parse number
            for k in numStart .. numEnd - 1 loop
               if args (k) >= '0' and args (k) <= '9' then
                  n := n * 10 +
                     (Character'Pos (args (k)) - Character'Pos ('0'));
               end if;
            end loop;

            if n > 0 then
               maxLines := n;
            end if;

            --  Skip space after number
            if numEnd <= args'Last and then args (numEnd) = ' ' then
               pathStart := numEnd + 1;
            else
               putStr ("usage: head [-n N] <path>" & LF);
               return;
            end if;
         end;
      end if;

      declare
         actualPath : String renames args (pathStart .. args'Last);
      begin
         if actualPath'Length = 0 then
            putStr ("usage: head [-n N] <path>" & LF);
            return;
         end if;

         --  Resolve path against cwd into fsBuf
         declare
            resolvedLen : constant Natural := resolvePath (actualPath);
         begin
            --  OP_OPEN
            msg := NULL_MESSAGE;
            msg.tag := (label  => OP_OPEN,
                        length => 3, flags => 0, badge => 0);
            msg.words (0) := fsGrantId;
            msg.words (1) := Unsigned_64 (resolvedLen);
            msg.words (2) := 0;
            tag := capCall (CAP_SLOT_FS, msg);
         end;

         if tag.label /= REPLY_OK then
            putStr ("head: file not found" & LF);
            return;
         end if;

         handle := msg.words (0);

         --  Read loop
         loop
            msg := NULL_MESSAGE;
            msg.tag := (label  => OP_READ,
                        length => 3, flags => 0, badge => 0);
            msg.words (0) := handle;
            msg.words (1) := fsGrantId;
            msg.words (2) := Unsigned_64 (FS_BUF_PAGES * 4096);
            tag := capCall (CAP_SLOT_FS, msg);

            exit when tag.label /= REPLY_OK;

            declare
               bytesRead : constant Natural := Natural (msg.words (0));
               data : String (1 .. bytesRead) with Import, Address => fsBuf;
               done : Boolean := False;
            begin
               exit when bytesRead = 0;

               for j in data'Range loop
                  putChar (data (j));
                  if data (j) = ASCII.LF then
                     lineCount := lineCount + 1;
                     if lineCount >= maxLines then
                        done := True;
                        exit;
                     end if;
                  end if;
               end loop;

               exit when done;
            end;
         end loop;

         --  OP_CLOSE
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_CLOSE,
                     length => 1, flags => 0, badge => 0);
         msg.words (0) := handle;
         tag := capCall (CAP_SLOT_FS, msg);

         renderDirty;
      end;
   end cmdHead;

   ---------------------------------------------------------------------------
   --  cmdWc
   ---------------------------------------------------------------------------
   procedure cmdWc (path : String) is
      msg      : Message;
      tag      : MessageTag;
      handle   : Unsigned_64;
      lines    : Unsigned_32 := 0;
      words    : Unsigned_32 := 0;
      bytes    : Unsigned_32 := 0;
      inWord   : Boolean := False;
   begin
      if not fsReady then
         putStr ("error: filesystem not available" & LF);
         return;
      end if;

      if path'Length = 0 then
         putStr ("usage: wc <path>" & LF);
         return;
      end if;

      --  Resolve path against cwd into fsBuf
      declare
         resolvedLen : constant Natural := resolvePath (path);
      begin
         --  OP_OPEN
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_OPEN,
                     length => 3, flags => 0, badge => 0);
         msg.words (0) := fsGrantId;
         msg.words (1) := Unsigned_64 (resolvedLen);
         msg.words (2) := 0;
         tag := capCall (CAP_SLOT_FS, msg);
      end;

      if tag.label /= REPLY_OK then
         putStr ("wc: file not found" & LF);
         return;
      end if;

      handle := msg.words (0);

      --  Read loop
      loop
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_READ,
                     length => 3, flags => 0, badge => 0);
         msg.words (0) := handle;
         msg.words (1) := fsGrantId;
         msg.words (2) := Unsigned_64 (FS_BUF_PAGES * 4096);
         tag := capCall (CAP_SLOT_FS, msg);

         exit when tag.label /= REPLY_OK;

         declare
            bytesRead : constant Natural := Natural (msg.words (0));
            data : array (0 .. bytesRead - 1) of Unsigned_8
               with Import, Address => fsBuf;
         begin
            exit when bytesRead = 0;

            bytes := bytes + Unsigned_32 (bytesRead);

            for j in data'Range loop
               declare
                  b : constant Unsigned_8 := data (j);
                  isSpace : constant Boolean :=
                     b = 16#20# or b = 16#09# or b = 16#0A# or
                     b = 16#0D#;
               begin
                  if b = 16#0A# then
                     lines := lines + 1;
                  end if;

                  if isSpace then
                     inWord := False;
                  elsif not inWord then
                     inWord := True;
                     words := words + 1;
                  end if;
               end;
            end loop;
         end;
      end loop;

      --  OP_CLOSE
      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_CLOSE,
                  length => 1, flags => 0, badge => 0);
      msg.words (0) := handle;
      tag := capCall (CAP_SLOT_FS, msg);

      --  Print results
      putStr ("  ");
      putDecRight (lines, 6);
      putStr ("  ");
      putDecRight (words, 6);
      putStr ("  ");
      putDecRight (bytes, 6);
      putStr ("  ");
      putStr (path);
      putChar (LF);
   end cmdWc;

   ---------------------------------------------------------------------------
   --  cmdInspect - show app manifest metadata (.cubit.id, .cubit.streams,
   --                .cubit.caps, .cubit.access)
   ---------------------------------------------------------------------------
   OP_SEEK : constant Unsigned_32 := 16#0006#;

   --  Read helpers: read typed values from fsBuf at byte offset
   function bufU8 (off : Natural) return Unsigned_8 is
      val : Unsigned_8 with Import,
         Address => fsBuf + Storage_Offset (off);
   begin
      return val;
   end bufU8;

   function bufU16 (off : Natural) return Unsigned_16 is
      val : Unsigned_16 with Import,
         Address => fsBuf + Storage_Offset (off);
   begin
      return val;
   end bufU16;

   function bufU32 (off : Natural) return Unsigned_32 is
      val : Unsigned_32 with Import,
         Address => fsBuf + Storage_Offset (off);
   begin
      return val;
   end bufU32;

   function bufU64 (off : Natural) return Unsigned_64 is
      val : Unsigned_64 with Import,
         Address => fsBuf + Storage_Offset (off);
   begin
      return val;
   end bufU64;

   --  Seek to an offset in an open file handle
   procedure seekTo (handle : Unsigned_64; offset : Unsigned_64) is
      msg : Message := NULL_MESSAGE;
      tag : MessageTag;
   begin
      msg.tag := (label  => OP_SEEK,
                  length => 3, flags => 0, badge => 0);
      msg.words (0) := handle;
      msg.words (1) := offset;
      msg.words (2) := 0;  -- SEEK_SET
      tag := capCall (CAP_SLOT_FS, msg);
   end seekTo;

   --  Read up to count bytes into fsBuf; returns actual bytes read
   function readChunk
     (handle : Unsigned_64;
      count  : Unsigned_64) return Unsigned_64
   is
      msg : Message := NULL_MESSAGE;
      tag : MessageTag;
   begin
      msg.tag := (label  => OP_READ,
                  length => 3, flags => 0, badge => 0);
      msg.words (0) := handle;
      msg.words (1) := fsGrantId;
      msg.words (2) := count;
      tag := capCall (CAP_SLOT_FS, msg);
      if tag.label /= REPLY_OK then
         return 0;
      end if;
      return msg.words (0);
   end readChunk;

   procedure cmdInspect (path : String) is
      msg    : Message;
      tag    : MessageTag;
      handle : Unsigned_64;
      fileSize   : Unsigned_64;
      e_shoff    : Unsigned_64;
      e_shnum    : Unsigned_16;
      bytesRead  : Unsigned_64;

      SHT_PROGBITS   : constant Unsigned_32 := 1;
      CACC_MAGIC     : constant Unsigned_32 := 16#43434143#;
      CBIT_MAGIC     : constant Unsigned_32 := 16#43424954#;
      CBID_MAGIC     : constant Unsigned_32 := 16#44494243#;
      CBST_MAGIC     : constant Unsigned_32 := 16#54534243#;
      foundAccess    : Boolean := False;
      foundCaps      : Boolean := False;
      foundId        : Boolean := False;
      foundStreams    : Boolean := False;
   begin
      if not fsReady then
         putStr ("error: filesystem not available" & LF);
         return;
      end if;

      if path'Length = 0 then
         putStr ("usage: inspect <file>" & LF);
         return;
      end if;

      --  Resolve path against cwd into fsBuf
      declare
         resolvedLen : constant Natural := resolvePath (path);
      begin
         --  OP_OPEN
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_OPEN,
                     length => 3, flags => 0, badge => 0);
         msg.words (0) := fsGrantId;
         msg.words (1) := Unsigned_64 (resolvedLen);
         msg.words (2) := 0;
         tag := capCall (CAP_SLOT_FS, msg);
      end;

      if tag.label /= REPLY_OK then
         putStr ("inspect: file not found" & LF);
         return;
      end if;

      handle   := msg.words (0);
      fileSize := msg.words (1);

      --  Read ELF header (first 64 bytes)
      bytesRead := readChunk (handle, 64);
      if bytesRead < 64 then
         putStr ("inspect: file too small" & LF);
         goto Close_File;
      end if;

      --  Validate ELF magic: 0x7F 'E' 'L' 'F'
      if bufU8 (0) /= 16#7F# or bufU8 (1) /= 16#45# or
         bufU8 (2) /= 16#4C# or bufU8 (3) /= 16#46#
      then
         putStr ("inspect: not an ELF file" & LF);
         goto Close_File;
      end if;

      e_shoff := bufU64 (40);
      e_shnum := bufU16 (60);

      if e_shoff = 0 or e_shnum = 0 then
         putStr ("inspect: no section headers" & LF);
         goto Close_File;
      end if;

      --  Read section header table into fsBuf
      --  Each section header is 64 bytes; limit to what fsBuf can hold
      declare
         shTableSize : constant Unsigned_64 :=
            Unsigned_64 (e_shnum) * 64;
         maxRead : constant Unsigned_64 :=
            Unsigned_64 (FS_BUF_PAGES * 4096);
         actualShNum : Unsigned_16;
      begin
         if shTableSize > maxRead then
            actualShNum := Unsigned_16 (maxRead / 64);
         else
            actualShNum := e_shnum;
         end if;

         seekTo (handle, e_shoff);
         bytesRead := readChunk (handle,
            Unsigned_64 (actualShNum) * 64);

         if bytesRead < Unsigned_64 (actualShNum) * 64 then
            actualShNum := Unsigned_16 (bytesRead / 64);
         end if;

         --  Copy section header info to stack before we reuse fsBuf.
         --  We need sh_type, sh_offset, sh_size for each PROGBITS section.
         declare
            MAX_SECTIONS : constant := 64;
            type SHInfo is record
               sh_type   : Unsigned_32;
               sh_offset : Unsigned_64;
               sh_size   : Unsigned_64;
            end record;
            sections : array (0 .. MAX_SECTIONS - 1) of SHInfo;
            numSections : Natural := 0;
         begin
            for i in 0 .. Natural (actualShNum) - 1 loop
               declare
                  base : constant Natural := i * 64;
                  sht  : constant Unsigned_32 := bufU32 (base + 4);
               begin
                  if sht = SHT_PROGBITS and numSections < MAX_SECTIONS then
                     sections (numSections).sh_type   := sht;
                     sections (numSections).sh_offset  :=
                        bufU64 (base + 24);
                     sections (numSections).sh_size    :=
                        bufU64 (base + 32);
                     numSections := numSections + 1;
                  end if;
               end;
            end loop;

            --  Scan each PROGBITS section for magic signatures
            for s in 0 .. numSections - 1 loop
               if sections (s).sh_size >= 4 and
                  sections (s).sh_offset + sections (s).sh_size <= fileSize
               then
                  --  Read first 4 bytes to check magic
                  seekTo (handle, sections (s).sh_offset);
                  bytesRead := readChunk (handle, 4);

                  if bytesRead >= 4 then
                     declare
                        magic : constant Unsigned_32 := bufU32 (0);
                     begin
                        -------------------------------------------------------
                        --  .cubit.id section (CBID)
                        -------------------------------------------------------
                        if magic = CBID_MAGIC and not foundId then
                           seekTo (handle, sections (s).sh_offset);
                           bytesRead := readChunk (handle,
                              sections (s).sh_size);

                           if bytesRead >= 8 then
                              declare
                                 ver : constant Unsigned_16 :=
                                    bufU16 (4);
                                 cnt : constant Unsigned_16 :=
                                    bufU16 (6);
                                 pos : Natural := 8;
                              begin
                                 putStr ("Package Identity "
                                    & "(.cubit.id v");
                                 putDec (Unsigned_32 (ver));
                                 putStr (", ");
                                 putDec (Unsigned_32 (cnt));
                                 putStr (" entries):" & LF);

                                 if ver = 1 and cnt > 0 then
                                    for e in 0 ..
                                       Natural (cnt) - 1
                                    loop
                                       exit when pos + 3 >
                                          Natural (bytesRead);

                                       declare
                                          kLen : constant Natural :=
                                             Natural (bufU8 (pos));
                                          vLen : constant Natural :=
                                             Natural (bufU16 (pos + 1));
                                       begin
                                          pos := pos + 3;
                                          exit when pos + kLen + vLen >
                                             Natural (bytesRead);

                                          putStr ("  ");
                                          --  Print key
                                          for c in 0 .. kLen - 1 loop
                                             putChar (
                                                Character'Val (
                                                   Natural (
                                                      bufU8 (pos + c))));
                                          end loop;
                                          putStr (" = ");
                                          --  Print value
                                          for c in 0 .. vLen - 1 loop
                                             declare
                                                ch : constant Unsigned_8 :=
                                                   bufU8 (
                                                      pos + kLen + c);
                                             begin
                                                if ch >= 32 and
                                                   ch < 127
                                                then
                                                   putChar (
                                                      Character'Val (
                                                         Natural (ch)));
                                                end if;
                                             end;
                                          end loop;
                                          putChar (LF);

                                          pos := pos + kLen + vLen;
                                       end;
                                    end loop;
                                 end if;
                              end;

                              foundId := True;
                           end if;

                        -------------------------------------------------------
                        --  .cubit.streams section (CBST)
                        -------------------------------------------------------
                        elsif magic = CBST_MAGIC and not foundStreams then
                           seekTo (handle, sections (s).sh_offset);
                           bytesRead := readChunk (handle,
                              sections (s).sh_size);

                           if bytesRead >= 8 then
                              declare
                                 ver : constant Unsigned_16 :=
                                    bufU16 (4);
                                 cnt : constant Unsigned_16 :=
                                    bufU16 (6);
                              begin
                                 putChar (LF);
                                 putStr ("Streams "
                                    & "(.cubit.streams v");
                                 putDec (Unsigned_32 (ver));
                                 putStr (", ");
                                 putDec (Unsigned_32 (cnt));
                                 putStr (" entries):" & LF);

                                 if ver = 1 and cnt > 0 and
                                    bytesRead >= 8 +
                                       Unsigned_64 (cnt) * 8
                                 then
                                    for e in 0 ..
                                       Natural (cnt) - 1
                                    loop
                                       declare
                                          eBase : constant Natural :=
                                             8 + e * 8;
                                          sid : constant Unsigned_16 :=
                                             bufU16 (eBase);
                                          pg  : constant Unsigned_16 :=
                                             bufU16 (eBase + 2);
                                          tt  : constant Unsigned_16 :=
                                             bufU16 (eBase + 4);
                                          fl  : constant Unsigned_16 :=
                                             bufU16 (eBase + 6);
                                       begin
                                          putStr ("  [");
                                          putDec (Unsigned_32 (e));
                                          putStr ("] ");

                                          --  Stream name
                                          case sid is
                                             when 1 =>
                                                putStr ("stdin ");
                                             when 2 =>
                                                putStr ("stdout");
                                             when 3 =>
                                                putStr ("stderr");
                                             when 4 =>
                                                putStr ("log   ");
                                             when 6 =>
                                                putStr ("metric");
                                             when 9 =>
                                                putStr ("health");
                                             when others =>
                                                putStr ("id=");
                                                putDec (
                                                   Unsigned_32 (sid));
                                          end case;

                                          putStr ("  pages=");
                                          putDec (Unsigned_32 (pg));

                                          putStr ("  type=");
                                          case tt is
                                             when 0 =>
                                                putStr ("raw");
                                             when 1 =>
                                                putStr ("text");
                                             when others =>
                                                putDec (
                                                   Unsigned_32 (tt));
                                          end case;

                                          if fl /= 0 then
                                             putStr ("  flags=");
                                             putDec (Unsigned_32 (fl));
                                          end if;

                                          putChar (LF);
                                       end;
                                    end loop;
                                 end if;
                              end;

                              foundStreams := True;
                           end if;

                        -------------------------------------------------------
                        --  .cubit.access section (CACC)
                        -------------------------------------------------------
                        elsif magic = CACC_MAGIC and not foundAccess then
                           seekTo (handle, sections (s).sh_offset);
                           bytesRead := readChunk (handle,
                              sections (s).sh_size);

                           if bytesRead >= 16 then
                              declare
                                 ver   : constant Unsigned_16 :=
                                    bufU16 (4);
                                 cnt   : constant Unsigned_16 :=
                                    bufU16 (6);
                              begin
                                 putChar (LF);
                                 putStr ("Filesystem Access "
                                    & "(.cubit.access v");
                                 putDec (Unsigned_32 (ver));
                                 putStr (", ");
                                 putDec (Unsigned_32 (cnt));
                                 putStr (" entries):" & LF);

                                 if ver = 1 and cnt > 0 and
                                    bytesRead >= 16 +
                                       Unsigned_64 (cnt) * 80
                                 then
                                    for e in 0 ..
                                       Natural (cnt) - 1
                                    loop
                                       declare
                                          eBase : constant Natural :=
                                             16 + e * 80;
                                          rights : constant Unsigned_8 :=
                                             bufU8 (eBase);
                                          pLen : constant Natural :=
                                             Natural (bufU8 (eBase + 1));
                                       begin
                                          putStr ("  [");
                                          putDec (Unsigned_32 (e));
                                          putStr ("] ");

                                          --  Rights flags
                                          if (rights and 1) /= 0 then
                                             putChar ('R');
                                          end if;
                                          if (rights and 2) /= 0 then
                                             putChar ('W');
                                          end if;
                                          if (rights and 4) /= 0 then
                                             putChar ('X');
                                          end if;
                                          if (rights and 8) /= 0 then
                                             putChar ('C');
                                          end if;

                                          putStr ("    ");

                                          --  Path prefix
                                          if pLen = 0 then
                                             putStr ("(wildcard)");
                                          else
                                             for c in 0 .. pLen - 1 loop
                                                declare
                                                   ch : constant
                                                      Unsigned_8 :=
                                                      bufU8 (eBase + 8 +
                                                         c);
                                                begin
                                                   if ch >= 32 and
                                                      ch < 127
                                                   then
                                                      putChar (
                                                         Character'Val (
                                                            Natural (ch)));
                                                   end if;
                                                end;
                                             end loop;
                                          end if;

                                          putChar (LF);
                                       end;
                                    end loop;
                                 end if;
                              end;

                              foundAccess := True;
                           end if;

                        -------------------------------------------------------
                        --  .cubit.caps section (CBIT)
                        -------------------------------------------------------
                        elsif magic = CBIT_MAGIC and not foundCaps then
                           seekTo (handle, sections (s).sh_offset);
                           bytesRead := readChunk (handle,
                              sections (s).sh_size);

                           if bytesRead >= 8 then
                              declare
                                 ver   : constant Unsigned_16 :=
                                    bufU16 (4);
                                 cnt   : constant Unsigned_16 :=
                                    bufU16 (6);

                                 --  Manifest request type names
                                 type ReqName is record
                                    name : String (1 .. 12);
                                    nlen : Natural;
                                 end record;
                                 reqNames : constant
                                    array (0 .. 9) of ReqName := (
                                    0 => ("UNKNOWN     ", 7),
                                    1 => ("FRAMEBUFFER ", 11),
                                    2 => ("SERVICE     ", 7),
                                    3 => ("IOPORT      ", 6),
                                    4 => ("IRQ         ", 3),
                                    5 => ("DEVICE_MEM  ", 10),
                                    6 => ("PROCESS     ", 7),
                                    7 => ("UNKNOWN     ", 7),
                                    8 => ("STREAM      ", 6),
                                    9 => ("RESOURCE    ", 8));
                              begin
                                 putChar (LF);
                                 putStr ("Capabilities "
                                    & "(.cubit.caps v");
                                 putDec (Unsigned_32 (ver));
                                 putStr (", ");
                                 putDec (Unsigned_32 (cnt));
                                 putStr (" entries):" & LF);

                                 if ver = 1 and cnt > 0 and
                                    bytesRead >= 8 +
                                       Unsigned_64 (cnt) * 16
                                 then
                                    for e in 0 ..
                                       Natural (cnt) - 1
                                    loop
                                       declare
                                          eBase : constant Natural :=
                                             8 + e * 16;
                                          reqType : constant Natural :=
                                             Natural (bufU8 (eBase));
                                          rights : constant Unsigned_8 :=
                                             bufU8 (eBase + 1);
                                          slot : constant Unsigned_8 :=
                                             bufU8 (eBase + 2);
                                          param0 : constant Unsigned_32 :=
                                             bufU32 (eBase + 4);
                                          rIdx : Natural := reqType;
                                       begin
                                          if rIdx > 9 then
                                             rIdx := 0;
                                          end if;

                                          putStr ("  [");
                                          putDec (Unsigned_32 (e));
                                          putStr ("] ");
                                          putStr (reqNames (rIdx).name
                                             (1 .. reqNames (rIdx).nlen));
                                          putChar (' ');

                                          --  Rights
                                          if (rights and 1) /= 0 then
                                             putChar ('R');
                                          end if;
                                          if (rights and 2) /= 0 then
                                             putChar ('W');
                                          end if;
                                          if (rights and 8) /= 0 then
                                             putChar ('G');
                                          end if;
                                          if (rights and 4) /= 0 then
                                             putChar ('X');
                                          end if;

                                          --  Slot
                                          putStr ("   slot ");
                                          putDec (Unsigned_32 (slot));

                                          --  Driver name for SERVICE type
                                          if reqType = 2 then
                                             putStr ("  ");
                                             case param0 is
                                                when 1 =>
                                                   putStr ("keyboard");
                                                when 2 =>
                                                   putStr ("ata");
                                                when 3 =>
                                                   putStr ("netstack");
                                                when 4 =>
                                                   putStr ("procmgr");
                                                when 5 =>
                                                   putStr ("nvme");
                                                when 6 =>
                                                   putStr ("fs");
                                                when 7 =>
                                                   putStr ("devmgr");
                                                when 8 =>
                                                   putStr ("hda");
                                                when 9 =>
                                                   putStr ("mixer");
                                                when 10 =>
                                                   putStr ("mouse");
                                                when 11 =>
                                                   putStr ("config");
                                                when 12 =>
                                                   putStr ("netmgr");
                                                when others =>
                                                   putStr ("driver=");
                                                   putDec (param0);
                                             end case;
                                          end if;

                                          --  Stream details: lo16=ID, hi16=pages
                                          if reqType = 8 then
                                             declare
                                                sid : constant Unsigned_32 :=
                                                   param0 and 16#FFFF#;
                                                pg  : constant Unsigned_32 :=
                                                   Shift_Right (param0, 16)
                                                      and 16#FFFF#;
                                             begin
                                                putStr ("  id=");
                                                case sid is
                                                   when 1 => putStr ("stdin");
                                                   when 2 => putStr ("stdout");
                                                   when 3 => putStr ("stderr");
                                                   when 4 => putStr ("log");
                                                   when others =>
                                                      putDec (sid);
                                                end case;
                                                putStr (" pages=");
                                                putDec (pg);
                                             end;
                                          end if;

                                          putChar (LF);
                                       end;
                                    end loop;
                                 end if;
                              end;

                              foundCaps := True;
                           end if;
                        end if;
                     end;
                  end if;
               end if;

               exit when foundId and foundStreams and
                         foundAccess and foundCaps;
            end loop;
         end;
      end;

      --  Report missing sections
      if not foundId then
         putStr ("Package Identity: (no .cubit.id)" & LF);
      end if;
      if not foundStreams and not foundCaps then
         putStr ("Streams: (none)" & LF);
      end if;
      if not foundCaps then
         putStr ("Capabilities: (none)" & LF);
      end if;
      if not foundAccess then
         putChar (LF);
         putStr ("Filesystem Access: (no .cubit.access)" & LF);
         putStr ("  deny-by-default (no filesystem access)" & LF);
      end if;

   <<Close_File>>
      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_CLOSE,
                  length => 1, flags => 0, badge => 0);
      msg.words (0) := handle;
      tag := capCall (CAP_SLOT_FS, msg);

      renderDirty;
   end cmdInspect;

   --  Manual string comparison (avoids memcmp dependency)
   function strEqual (a : String; b : String) return Boolean is
   begin
      if a'Length /= b'Length then
         return False;
      end if;
      for i in 0 .. a'Length - 1 loop
         if a (a'First + i) /= b (b'First + i) then
            return False;
         end if;
      end loop;
      return True;
   end strEqual;

   --  Check if line starts with a given prefix
   function startsWith (line : String; prefix : String) return Boolean is
   begin
      if line'Length < prefix'Length then
         return False;
      end if;
      for i in 0 .. prefix'Length - 1 loop
         if line (line'First + i) /= prefix (prefix'First + i) then
            return False;
         end if;
      end loop;
      return True;
   end startsWith;

   procedure cmdPwd is
   begin
      if cwdLen > 0 then
         putStr (cwdBuf (1 .. cwdLen));
      else
         putChar ('/');
      end if;
      putChar (LF);
   end cmdPwd;

   procedure cmdCd (path : String) is
   begin
      if path'Length = 0 then
         --  No args: print cwd
         cmdPwd;
         return;
      end if;

      --  "/" resets to ramdisk root
      if path'Length = 1 and then path (path'First) = '/' then
         cwdLen := 0;
         return;
      end if;

      --  Set new cwd
      if path'Length > CWD_MAX then
         putStr ("cd: path too long" & LF);
         return;
      end if;

      for i in 0 .. path'Length - 1 loop
         cwdBuf (i + 1) := path (path'First + i);
      end loop;
      cwdLen := path'Length;

      --  Ensure trailing '/'
      if cwdBuf (cwdLen) /= '/' then
         if cwdLen < CWD_MAX then
            cwdLen := cwdLen + 1;
            cwdBuf (cwdLen) := '/';
         end if;
      end if;
   end cmdCd;

   ---------------------------------------------------------------------------
   --  cmdConfig
   --  Handle "config get/set/delete/list" subcommands.
   ---------------------------------------------------------------------------
   procedure cmdConfig (arg : String) is
      use CuBit.Config;

      procedure printStatus (s : ConfigStatus) is
      begin
         case s is
            when OK          => null;
            when NotFound    => putStr ("error: not found" & LF);
            when Error       => putStr ("error" & LF);
            when AccessDenied => putStr ("error: access denied" & LF);
         end case;
      end printStatus;

   begin
      if startsWith (arg, "get ") and arg'Length > 4 then
         declare
            key    : String renames arg (arg'First + 4 .. arg'Last);
            val    : System.Address;
            valLen : Natural;
            st     : ConfigStatus;
         begin
            CuBit.Config.get (key, val, valLen, st);
            if st = OK then
               if valLen > 0 then
                  declare
                     buf : String (1 .. valLen)
                       with Import, Address => val;
                  begin
                     putStr (buf);
                  end;
               end if;
               putChar (LF);
            else
               printStatus (st);
            end if;
         end;

      elsif startsWith (arg, "set ") and arg'Length > 4 then
         --  Parse "set key value"
         declare
            rest    : String renames arg (arg'First + 4 .. arg'Last);
            spaceAt : Natural := 0;
         begin
            --  Find space separator between key and value
            for i in rest'Range loop
               if rest (i) = ' ' then
                  spaceAt := i;
                  exit;
               end if;
            end loop;

            if spaceAt = 0 then
               putStr ("usage: config set <key> <value>" & LF);
            else
               declare
                  key   : String renames rest (rest'First .. spaceAt - 1);
                  value : String renames rest (spaceAt + 1 .. rest'Last);
                  st    : ConfigStatus;
               begin
                  CuBit.Config.set (key, value'Address, value'Length, st);
                  if st = OK then
                     putStr ("ok" & LF);
                  else
                     printStatus (st);
                  end if;
               end;
            end if;
         end;

      elsif startsWith (arg, "delete ") and arg'Length > 7 then
         declare
            key : String renames arg (arg'First + 7 .. arg'Last);
            st  : ConfigStatus;
         begin
            CuBit.Config.delete (key, st);
            if st = OK then
               putStr ("ok" & LF);
            else
               printStatus (st);
            end if;
         end;

      elsif strEqual (arg, "save") then
         declare
            OP_CONFIG_SAVE : constant Unsigned_32 := 16#0605#;
            saveMsg : Message :=
              (tag => (label  => OP_CONFIG_SAVE,
                       length => 0,
                       flags  => 0,
                       badge  => 0),
               capBadge => 0,
               words => (others => 0));
         begin
            saveMsg.tag := capCall (CAP_SLOT_CONFIG, saveMsg);
            if saveMsg.tag.label = REPLY_OK then
               putStr ("config saved" & LF);
            else
               putStr ("config save failed" & LF);
            end if;
         end;

      elsif strEqual (arg, "load") then
         declare
            OP_CONFIG_LOAD : constant Unsigned_32 := 16#0604#;
            loadMsg : Message :=
              (tag => (label  => OP_CONFIG_LOAD,
                       length => 0,
                       flags  => 0,
                       badge  => 0),
               capBadge => 0,
               words => (others => 0));
         begin
            loadMsg.tag := capCall (CAP_SLOT_CONFIG, loadMsg);
            if loadMsg.tag.label = REPLY_OK then
               putStr ("config loaded" & LF);
            else
               putStr ("config load failed" & LF);
            end if;
         end;

      elsif startsWith (arg, "resolve ") and arg'Length > 8 then
         declare
            schemeName : String renames
              arg (arg'First + 8 .. arg'Last);
            info : CuBit.Config.SchemeInfo;
         begin
            info := CuBit.Config.resolveScheme (schemeName);
            if info.found then
               putStr ("  driver=");
               putDec (Unsigned_32 (info.driverID));
               putChar (LF);
               putStr ("  slot=");
               putDec (Unsigned_32 (info.capSlot));
               putChar (LF);
               putStr ("  pid=");
               putDec (Unsigned_32 (info.pid));
               putChar (LF);
            else
               putStr ("scheme not found: " & schemeName & LF);
            end if;
         end;

      elsif startsWith (arg, "list") then
         declare
            prefix : String (1 .. 0);
            keys   : System.Address;
            count  : Natural;
            st     : ConfigStatus;
         begin
            if arg'Length > 5 and then arg (arg'First + 4) = ' ' then
               --  Has a prefix argument
               declare
                  pfx : String renames
                     arg (arg'First + 5 .. arg'Last);
               begin
                  CuBit.Config.list (pfx, keys, count, st);
               end;
            else
               CuBit.Config.list (prefix, keys, count, st);
            end if;

            if st = OK then
               putDec (Unsigned_32 (count));
               putStr (" keys:" & LF);
               if count > 0 then
                  declare
                     buf : array (0 .. 4095) of Unsigned_8
                       with Import, Address => keys;
                     pos : Natural := 0;
                  begin
                     for k in 0 .. count - 1 loop
                        putStr ("  ");
                        while pos < 4096 and then buf (pos) /= 0 loop
                           putChar (Character'Val (buf (pos)));
                           pos := pos + 1;
                        end loop;
                        putChar (LF);
                        pos := pos + 1;  --  skip NUL
                     end loop;
                  end;
               end if;
            else
               printStatus (st);
            end if;
         end;

      else
         putStr ("usage: config get|set|delete|list|save|load|resolve" & LF);
         putStr ("  config get <key>" & LF);
         putStr ("  config set <key> <value>" & LF);
         putStr ("  config delete <key>" & LF);
         putStr ("  config list [prefix]" & LF);
         putStr ("  config save" & LF);
         putStr ("  config load" & LF);
         putStr ("  config resolve <scheme>" & LF);
      end if;
   end cmdConfig;

   ---------------------------------------------------------------------------
   --  cmdIfconfig - show network interface details
   ---------------------------------------------------------------------------
   procedure cmdIfconfig is
      msg : Message;
      tag : MessageTag;
   begin
      if not netstackReady then
         putStr ("error: netstack not available" & LF);
         return;
      end if;

      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_NET_IF_DETAIL,
                  length => 1, flags => 0, badge => 0);
      msg.words (0) := 0;  -- interface 0
      tag := capCall (CAP_SLOT_NET, msg);

      if tag.label /= REPLY_OK then
         putStr ("error: no interfaces" & LF);
         return;
      end if;

      putStr ("eth0:" & LF);

      --  State (high 32 bits of word 0)
      declare
         stateVal : constant Unsigned_64 :=
            Shift_Right (msg.words (0), 32) and 16#FF#;
      begin
         putStr ("  state:   ");
         if stateVal = 1 then
            putStr ("UP");
         elsif stateVal = 2 then
            putStr ("CONFIGURING");
         else
            putStr ("DOWN");
         end if;
         putChar (LF);
      end;

      --  IP (low 32 bits of word 0)
      putStr ("  inet:    ");
      putIP (msg.words (0) and 16#FFFF_FFFF#);
      putChar (LF);

      --  Netmask (low 32 bits of word 1)
      putStr ("  mask:    ");
      putIP (msg.words (1) and 16#FFFF_FFFF#);
      putChar (LF);

      --  Gateway (high 32 bits of word 1)
      putStr ("  gw:      ");
      putIP (Shift_Right (msg.words (1), 32) and 16#FFFF_FFFF#);
      putChar (LF);

      --  MAC (low 48 bits of word 2)
      putStr ("  ether:   ");
      putMAC (msg.words (2));
      putChar (LF);

      --  DNS (word 3: low 32 = primary, high 32 = secondary)
      putStr ("  dns:     ");
      putIP (msg.words (3) and 16#FFFF_FFFF#);
      declare
         sec : constant Unsigned_64 :=
            Shift_Right (msg.words (3), 32) and 16#FFFF_FFFF#;
      begin
         if sec /= 0 then
            putStr (", ");
            putIP (sec);
         end if;
      end;
      putChar (LF);
   end cmdIfconfig;

   ---------------------------------------------------------------------------
   --  cmdRoute - show routing table
   ---------------------------------------------------------------------------
   procedure cmdRoute is
      msg       : Message;
      tag       : MessageTag;
      startIdx  : Unsigned_64 := 0;
      total     : Natural := 0;
      printed   : Natural := 0;
   begin
      if not netstackReady then
         putStr ("error: netstack not available" & LF);
         return;
      end if;

      putStr ("DESTINATION     PREFIX  GATEWAY         IF  METRIC" & LF);

      loop
         msg := NULL_MESSAGE;
         msg.tag := (label  => OP_NET_ROUTE_LIST,
                     length => 1, flags => 0, badge => 0);
         msg.words (0) := startIdx;
         tag := capCall (CAP_SLOT_NET, msg);

         if tag.label /= REPLY_OK then
            exit;
         end if;

         total := Natural (tag.length);
         if total = 0 then
            exit;
         end if;

         --  Unpack up to 2 routes from the reply
         for slot in 0 .. 1 loop
            if printed < total then
               declare
                  w0 : constant Unsigned_64 := msg.words (slot * 2);
                  w1 : constant Unsigned_64 := msg.words (slot * 2 + 1);
                  dest   : constant Unsigned_64 := w0 and 16#FFFF_FFFF#;
                  prefix : constant Unsigned_64 :=
                     Shift_Right (w0, 32) and 16#FF#;
                  ifIdx  : constant Unsigned_64 :=
                     Shift_Right (w0, 40) and 16#FF#;
                  metric : constant Unsigned_64 :=
                     Shift_Right (w0, 48) and 16#FFFF#;
                  gw : constant Unsigned_64 := w1 and 16#FFFF_FFFF#;
               begin
                  --  Skip empty padding slots
                  if dest /= 0 or prefix /= 0 or gw /= 0 then
                     putIP (dest);
                     putStr ("  /");
                     putDec (Unsigned_32 (prefix));
                     if prefix < 10 then
                        putStr ("    ");
                     else
                        putStr ("   ");
                     end if;
                     putIP (gw);
                     putStr ("  ");
                     putDec (Unsigned_32 (ifIdx));
                     putStr ("   ");
                     putDec (Unsigned_32 (metric));
                     putChar (LF);
                  end if;
               end;
               printed := printed + 1;
            end if;
         end loop;

         --  Next page (flags = next startIndex)
         startIdx := Unsigned_64 (tag.flags);
         exit when printed >= total;
      end loop;

      if total = 0 then
         putStr ("(no routes)" & LF);
      end if;
   end cmdRoute;

   ---------------------------------------------------------------------------
   --  cmdPing - send ICMP echo requests
   ---------------------------------------------------------------------------
   procedure cmdPing (target : String) is
      dstPacked : Unsigned_64;
      ok : Boolean;
      msg : Message;
      tag : MessageTag;
   begin
      if not netstackReady then
         putStr ("error: netstack not available" & LF);
         return;
      end if;

      parseIP (target, dstPacked, ok);
      if not ok then
         putStr ("invalid IP address" & LF);
         return;
      end if;

      putStr ("PING ");
      putIP (dstPacked);
      putChar (LF);
      renderDirty;

      for seq in 1 .. 4 loop
         declare
            sendTs : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
            ignore : Unsigned_64;
         begin
            msg := NULL_MESSAGE;
            msg.tag := (label  => OP_NET_PING,
                        length => 3, flags => 0, badge => 0);
            msg.words (0) := dstPacked;
            msg.words (1) := Unsigned_64 (seq);
            msg.words (2) := sendTs;
            tag := capCall (CAP_SLOT_NET, msg);

            if tag.label = REPLY_OK then
               putStr ("reply from ");
               putIP (msg.words (1));
               putStr (" seq=");
               putDec (Unsigned_32 (msg.words (0)));
               putStr (" time=");
               putDec (Unsigned_32 (msg.words (2)));
               putStr ("ms" & LF);
            else
               putStr ("request timeout seq=");
               putDec (Unsigned_32 (seq));
               putChar (LF);
            end if;

            renderDirty;

            --  Wait ~1 second between pings (except after last)
            if seq < 4 then
               ignore := syscall (SYSCALL_SLEEP, 1000);
            end if;
         end;
      end loop;
   end cmdPing;

   ---------------------------------------------------------------------------
   --  cmdNslookup - DNS lookup
   ---------------------------------------------------------------------------
   procedure cmdNslookup (hostname : String) is
      msg : Message;
      tag : MessageTag;
      nameLen : constant Natural := hostname'Length;
   begin
      if not netstackReady then
         putStr ("error: netstack not available" & LF);
         return;
      end if;

      if nameLen = 0 or nameLen > 32 then
         putStr ("hostname too long (max 32)" & LF);
         return;
      end if;

      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_NET_RESOLVE,
                  length => Unsigned_8 (nameLen),
                  flags  => 0, badge => 0);

      --  Pack hostname bytes into message words
      declare
         raw : array (0 .. 31) of Unsigned_8 with
            Import, Address => msg.words'Address;
      begin
         for i in 0 .. 31 loop
            raw (i) := 0;
         end loop;
         for i in 0 .. nameLen - 1 loop
            raw (i) := Character'Pos (hostname (hostname'First + i));
         end loop;
      end;

      tag := capCall (CAP_SLOT_NET, msg);

      if tag.label = REPLY_OK then
         putStr ("Address: ");
         putIP (msg.words (0));
         putChar (LF);
      else
         putStr ("lookup failed" & LF);
      end if;
   end cmdNslookup;

   ---------------------------------------------------------------------------
   --  cmdStreams - query a process for its active streams
   ---------------------------------------------------------------------------
   procedure cmdStreams (args : String) is
      OP_STREAM_LIST : constant Unsigned_32 := 16#0705#;
      pid     : Unsigned_64 := 0;
      ch      : Character;
      msg     : Message;
      comp    : CompletionEntry;
      ok      : Boolean;
      ret     : Unsigned_64;
      bitmask : Unsigned_64;
      count   : Unsigned_32;
   begin
      if args'Length = 0 then
         putStr ("usage: streams <pid>" & LF);
         return;
      end if;

      --  Parse decimal PID
      for i in args'Range loop
         ch := args (i);
         if ch >= '0' and ch <= '9' then
            pid := pid * 10 +
               Unsigned_64 (Character'Pos (ch) - Character'Pos ('0'));
         else
            putStr ("streams: invalid PID" & LF);
            return;
         end if;
      end loop;

      if pid = 0 or pid > 255 then
         putStr ("streams: PID out of range" & LF);
         return;
      end if;

      --  Build and send OP_STREAM_LIST
      msg := NULL_MESSAGE;
      msg.tag := (label  => OP_STREAM_LIST,
                  length => 0, flags => 0, badge => 0);

      ok := submit (ProcessID (pid), msg, STREAM_LIST_TOKEN);
      if not ok then
         putStr ("streams: send failed" & LF);
         return;
      end if;

      --  Poll for completion with timeout
      for attempt in 1 .. 50 loop
         ret := Poll_Completion (comp'Address);
         if ret = 1 and then comp.token = STREAM_LIST_TOKEN then
            if comp.msg.tag.label = REPLY_OK then
               bitmask := comp.msg.words (0);
               count   := Unsigned_32 (comp.msg.words (1));

               putStr ("PID ");
               putDec (Unsigned_32 (pid));
               putStr (": ");
               putDec (count);
               putStr (" stream(s)" & LF);

               --  Scan bits 0..15 for active stream IDs
               for bit in 0 .. 15 loop
                  if (bitmask and Shift_Left (1, bit)) /= 0 then
                     putStr ("  stream ");
                     putDec (Unsigned_32 (bit));
                     putStr (" = ");
                     case bit is
                        when 1 => putStr ("stdin");
                        when 2 => putStr ("stdout");
                        when 3 => putStr ("stderr");
                        when 4 => putStr ("log");
                        when others =>
                           putStr ("id:");
                           putDec (Unsigned_32 (bit));
                     end case;
                     putChar (LF);
                  end if;
               end loop;
            elsif comp.msg.tag.label = REPLY_ERR then
               putStr ("streams: error from PID ");
               putDec (Unsigned_32 (pid));
               putChar (LF);
            else
               putStr ("streams: unexpected reply" & LF);
            end if;
            return;
         end if;

         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_SLEEP, 10);
         end;
      end loop;

      putStr ("streams: no response from PID ");
      putDec (Unsigned_32 (pid));
      putChar (LF);
   end cmdStreams;

   ---------------------------------------------------------------------------
   --  cmdLogs - query the log store service for recent log entries
   --  Uses async submit + Poll_Completion to get full reply message
   --  (send() only returns the tag, not the reply words).
   --  Logstore creates a temporary read-only grant to us, writes entries,
   --  then replies with the grant ID.
   ---------------------------------------------------------------------------
   logstorePID : ProcessID := NO_PROCESS;
   LOG_QUERY_TOKEN : constant Unsigned_64 := 99;

   procedure cmdLogs is
      OP_LOG_QUERY : constant Unsigned_32 := 16#0800#;
      LOG_MAX_ENTRIES : constant := 50;
   begin
      --  Lazy-discover logstore PID
      if logstorePID = NO_PROCESS then
         logstorePID := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_LOGSTORE);
         if logstorePID = 0 or logstorePID = Unsigned_64'Last then
            logstorePID := NO_PROCESS;
            putStr ("logstore not running" & LF);
            return;
         end if;
      end if;

      --  Submit async OP_LOG_QUERY
      declare
         qMsg : constant Message := (
            tag => (label  => OP_LOG_QUERY,
                    length => 2,
                    flags  => 0,
                    badge  => 0),
            capBadge => 0,
            words    => (0 => LOG_MAX_ENTRIES,
                         1 => 0,  --  filter: all PIDs
                         others => 0));
         ok : Boolean;
      begin
         ok := submit (logstorePID, qMsg, LOG_QUERY_TOKEN);
         if not ok then
            putStr ("error: submit failed" & LF);
            return;
         end if;
      end;

      --  Poll for completion (with brief timeout)
      for attempt in 1 .. 200 loop
         declare
            comp : CompletionEntry;
            ret  : Unsigned_64;
            ignore : Unsigned_64;
         begin
            ret := Poll_Completion (comp'Address);
            if ret = 1 and then comp.token = LOG_QUERY_TOKEN then
               if comp.msg.tag.label /= 16#F000# then
                  putStr ("error: log query failed" & LF);
                  return;
               end if;

               declare
                  written : constant Natural :=
                    Natural (comp.msg.words (0));
                  gid     : constant Unsigned_64 := comp.msg.words (2);
               begin
                  if written = 0 then
                     putStr ("no log entries" & LF);
                     return;
                  end if;

                  --  Read entries from grant region
                  declare
                     grantAddr : constant System.Address := To_Address (
                       Integer_Address (
                         GRANT_REGION_BASE + gid * GRANT_SLOT_SIZE));
                     eBuf   : array (0 .. 4095) of Unsigned_8
                       with Import, Address => grantAddr;
                     off    : Natural := 8;
                     maxOff : constant Natural := 4096;
                  begin
                     for i in 0 .. written - 1 loop
                        exit when off + 8 > maxOff;
                        declare
                           ePID : constant Unsigned_16 :=
                             Unsigned_16 (eBuf (off)) or
                             Shift_Left (Unsigned_16 (eBuf (off + 1)), 8);
                           eDataLen : constant Natural := Natural (
                             Unsigned_16 (eBuf (off + 2)) or
                             Shift_Left (
                                Unsigned_16 (eBuf (off + 3)), 8));
                           eTS : constant Unsigned_32 :=
                             Unsigned_32 (eBuf (off + 4)) or
                             Shift_Left (
                                Unsigned_32 (eBuf (off + 5)), 8) or
                             Shift_Left (
                                Unsigned_32 (eBuf (off + 6)), 16) or
                             Shift_Left (
                                Unsigned_32 (eBuf (off + 7)), 24);
                        begin
                           exit when off + 8 + eDataLen > maxOff;

                           putStr ("[PID ");
                           putDec (Unsigned_32 (ePID));
                           putStr ("] ");
                           putDec (eTS);
                           putStr (": ");

                           for j in 0 .. eDataLen - 1 loop
                              declare
                                 ch : constant Unsigned_8 :=
                                   eBuf (off + 8 + j);
                              begin
                                 if ch = 10 then
                                    putChar (LF);
                                 elsif ch >= 32 and ch < 127 then
                                    putChar (
                                       Character'Val (Natural (ch)));
                                 end if;
                              end;
                           end loop;

                           if eDataLen = 0 or else
                              eBuf (off + 8 + eDataLen - 1) /= 10
                           then
                              putChar (LF);
                           end if;

                           off := off + 8 + eDataLen;
                        end;
                     end loop;
                  end;
               end;
               return;
            end if;

            ignore := syscall (SYSCALL_SLEEP, 10);
         end;
      end loop;

      putStr ("logs: timeout waiting for response" & LF);
   end cmdLogs;

   procedure dispatchCommand is
      line : String renames lineBuf (1 .. lineLen);
   begin
      if lineLen = 0 then
         return;
      end if;

      if strEqual (line, "help") then
         cmdHelp;
      elsif startsWith (line, "cd ") and lineLen > 3 then
         cmdCd (line (4 .. lineLen));
      elsif strEqual (line, "cd") then
         cmdCd ("");
      elsif strEqual (line, "pwd") then
         cmdPwd;
      elsif startsWith (line, "inspect ") and lineLen > 8 then
         cmdInspect (line (9 .. lineLen));
      elsif strEqual (line, "inspect") then
         putStr ("usage: inspect <file>" & LF);
      elsif startsWith (line, "bg ") and lineLen > 3 then
         cmdBg (line (4 .. lineLen));
      elsif strEqual (line, "bg") then
         putStr ("usage: bg <filename>" & LF);
      elsif strEqual (line, "clear") then
         cmdClear;
      elsif startsWith (line, "echo ") and lineLen > 5 then
         cmdEcho (line (6 .. lineLen));
      elsif startsWith (line, "spawn ") and lineLen > 6 then
         cmdSpawn (line (7 .. lineLen));
      elsif strEqual (line, "spawn") then
         putStr ("usage: spawn <filename>" & LF);
      elsif startsWith (line, "kill ") and lineLen > 5 then
         cmdKill (line (6 .. lineLen));
      elsif strEqual (line, "kill") then
         putStr ("usage: kill <pid>" & LF);
      elsif startsWith (line, "cat ") and lineLen > 4 then
         cmdCat (line (5 .. lineLen));
      elsif strEqual (line, "cat") then
         putStr ("usage: cat <path>" & LF);
      elsif startsWith (line, "ls ") and lineLen > 3 then
         cmdLs (line (4 .. lineLen));
      elsif strEqual (line, "ls") then
         cmdLs ("");
      elsif strEqual (line, "logs") then
         cmdLogs;
      elsif strEqual (line, "uptime") then
         cmdUptime;
      elsif strEqual (line, "sysinfo") then
         cmdSysinfo;
      elsif strEqual (line, "mem") then
         cmdMem;
      elsif strEqual (line, "volumes") then
         cmdVolumes;
      elsif strEqual (line, "ps") then
         cmdPs;
      elsif startsWith (line, "hexdump ") and lineLen > 8 then
         cmdHexdump (line (9 .. lineLen));
      elsif strEqual (line, "hexdump") then
         putStr ("usage: hexdump <path>" & LF);
      elsif startsWith (line, "head ") and lineLen > 5 then
         cmdHead (line (6 .. lineLen));
      elsif strEqual (line, "head") then
         putStr ("usage: head [-n N] <path>" & LF);
      elsif startsWith (line, "wc ") and lineLen > 3 then
         cmdWc (line (4 .. lineLen));
      elsif strEqual (line, "wc") then
         putStr ("usage: wc <path>" & LF);
      elsif startsWith (line, "config ") and lineLen > 7 then
         cmdConfig (line (8 .. lineLen));
      elsif strEqual (line, "config") then
         cmdConfig ("");
      elsif strEqual (line, "ifconfig") then
         cmdIfconfig;
      elsif strEqual (line, "route") then
         cmdRoute;
      elsif startsWith (line, "ping ") and lineLen > 5 then
         cmdPing (line (6 .. lineLen));
      elsif strEqual (line, "ping") then
         putStr ("usage: ping <ip>" & LF);
      elsif startsWith (line, "nslookup ") and lineLen > 9 then
         cmdNslookup (line (10 .. lineLen));
      elsif strEqual (line, "nslookup") then
         putStr ("usage: nslookup <hostname>" & LF);
      elsif startsWith (line, "streams ") and lineLen > 8 then
         cmdStreams (line (9 .. lineLen));
      elsif strEqual (line, "streams") then
         putStr ("usage: streams <pid>" & LF);
      elsif startsWith (line, "write ") and lineLen > 6 then
         cmdWrite (line (7 .. lineLen));
      elsif strEqual (line, "write") then
         putStr ("usage: write <path> <text>" & LF);
      else
         putStr ("unknown command: ");
         putStr (line);
         putChar (LF);
      end if;
   end dispatchCommand;

   ---------------------------------------------------------------------------
   --  Keyboard processing
   ---------------------------------------------------------------------------
   procedure processKey (scancode : Unsigned_8) is
      isRelease : constant Boolean := (scancode and 16#80#) /= 0;
      code : constant Natural := Natural (scancode and 16#7F#);
      ch : Unsigned_8;
   begin
      --  Handle shift key state
      if code = 16#2A# or code = 16#36# then
         shiftDown := not isRelease;
         return;
      end if;

      --  Handle ctrl key state (Left Ctrl = 0x1D)
      if code = 16#1D# then
         ctrlDown := not isRelease;
         return;
      end if;

      --  Only process make (press) events
      if isRelease then
         return;
      end if;

      --  Ctrl+C: kill foreground process
      if ctrlDown and code = 16#2E# then
         if foregroundPID /= 0 then
            --  Kill revokes grants, so deactivate stream first
            childStream.active := False;
            streamSubPending := False;
            declare
               ret : Unsigned_64;
            begin
               ret := killProcess (foregroundPID);
               debugPrint ("shell: Ctrl+C kill pid=");
               printDec (Unsigned_32 (foregroundPID));
               debugPrint (" ret=");
               printDec (Unsigned_32 (ret));
               debugPrint ("" & LF);
            end;
            --  Don't clear foregroundPID here; the EVENT_CHILD_EXIT
            --  handler in the main loop will do it.
         end if;
         return;
      end if;

      --  Translate scancode to ASCII
      if shiftDown then
         ch := scancodeShifted (code);
         --  For alpha keys, shift means uppercase
         if ch = 0 then
            ch := scancodeNormal (code);
            --  Convert lowercase letter to uppercase
            if ch >= Character'Pos ('a') and ch <= Character'Pos ('z') then
               ch := ch - 32;
            end if;
         end if;
      else
         ch := scancodeNormal (code);
      end if;

      if ch = 0 then
         return;
      end if;

      --  Handle special keys
      if ch = 10 then
         --  Enter: dispatch command
         putChar (LF);
         renderDirty;
         dispatchCommand;
         lineLen := 0;
         if foregroundPID = 0 then
            printPrompt;
         end if;
      elsif ch = 8 then
         --  Backspace
         if lineLen > 0 then
            lineLen := lineLen - 1;
            putChar (ASCII.BS);
            renderDirty;
         end if;
      elsif ch >= 32 and ch < 127 then
         --  Printable character
         if lineLen < LINE_MAX then
            lineLen := lineLen + 1;
            lineBuf (lineLen) := Character'Val (Natural (ch));
            putChar (Character'Val (Natural (ch)));
            renderDirty;
         end if;
      end if;
   end processKey;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------
   eventMsg : Message;

begin
   debugPrint ("shell: starting..." & LF);

   --  Register as keyboard and mouse driver to receive input events
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_KEYBOARD);
      ignore := registerDriver (DRIVER_MOUSE);
   end;

   --  Map framebuffer
   declare
      ret : Unsigned_64;
   begin
      ret := syscall (SYSCALL_MAPFB);
      if ret = Unsigned_64'Last then
         debugPrint ("shell: MAPFB failed" & LF);
         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_EXIT, 1);
         end;
         return;
      end if;
      fbAddr := To_Address (Integer_Address (ret));
   end;

   --  Query framebuffer dimensions
   fbWidth  := Natural (getInfo (SYSINFO_FB_WIDTH));
   fbHeight := Natural (getInfo (SYSINFO_FB_HEIGHT));
   fbPitch  := Natural (getInfo (SYSINFO_FB_PITCH));

   debugPrint ("shell: FB ");
   printDec (Unsigned_32 (fbWidth));
   debugPrint ("x");
   printDec (Unsigned_32 (fbHeight));
   debugPrint ("" & LF);

   --  Calculate grid size
   cols := fbWidth / Font8x16.GLYPH_WIDTH;
   rows := fbHeight / Font8x16.GLYPH_HEIGHT;

   if cols > MAX_COLS then
      cols := MAX_COLS;
   end if;
   if rows > MAX_ROWS then
      rows := MAX_ROWS;
   end if;

   setupDisplayPresent;

   --  Initialize screen buffer
   for row in 0 .. MAX_ROWS - 1 loop
      for col in 0 .. MAX_COLS - 1 loop
         screen (row)(col) := Character'Pos (' ');
      end loop;
      dirty (row) := True;
   end loop;
   fullPresentNeeded := True;

   --  Discover procmgr PID
   declare
      retries : Natural := 0;
   begin
      loop
         procmgrPID := ProcessID (
            getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_PROCMGR));
         exit when procmgrPID /= 0 or retries > 50;
         retries := retries + 1;
         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_SLEEP, 10);
         end;
      end loop;
   end;

   if procmgrPID /= NO_PROCESS then
      debugPrint ("shell: found procmgr pid=");
      printDec (Unsigned_32 (procmgrPID));
      debugPrint ("" & LF);

      --  Allocate grant buffer for procmgr communication
      declare
         ret : Unsigned_64;
         ok  : Boolean;
      begin
         ret := syscall (SYSCALL_SBRK, Unsigned_64 (GRANT_BUF_PAGES * 4096));
         if ret /= Unsigned_64'Last then
            grantBuf := To_Address (Integer_Address (ret));
            createGrant (
               grantee   => procmgrPID,
               localAddr => grantBuf,
               numPages  => GRANT_BUF_PAGES,
               readWrite => True,
               grantId   => grantId,
               success   => ok);
            if not ok then
               debugPrint ("shell: grant to procmgr failed" & LF);
               procmgrPID := NO_PROCESS;
            end if;
         end if;
      end;
   else
      debugPrint ("shell: procmgr not found" & LF);
   end if;

   --  Set up filesystem grant buffer
   declare
      ret : Unsigned_64;
      ok  : Boolean;
   begin
      ret := syscall (SYSCALL_SBRK, Unsigned_64 (FS_BUF_PAGES * 4096));
      if ret /= Unsigned_64'Last then
         fsBuf := To_Address (Integer_Address (ret));
         createGrantViaCap (
            slot      => CAP_SLOT_FS,
            localAddr => fsBuf,
            numPages  => FS_BUF_PAGES,
            readWrite => True,
            grantId   => fsGrantId,
            success   => ok);
         if ok then
            fsReady := True;
            debugPrint ("shell: FS grant OK" & LF);
         else
            debugPrint ("shell: FS grant failed" & LF);
         end if;
      else
         debugPrint ("shell: sbrk for FS buf failed" & LF);
      end if;
   end;

   --  Check if netstack is available
   declare
      pid : Unsigned_64;
   begin
      pid := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_NETSTACK);
      if pid /= 0 and pid /= Unsigned_64'Last then
         netstackReady := True;
         debugPrint ("shell: netstack available pid=");
         printDec (Unsigned_32 (pid));
         debugPrint ("" & LF);
      end if;
   end;

   --  Auto-detect cwd from registered disk driver
   declare
      pid : Unsigned_64;
   begin
      pid := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_NVME);
      if pid /= 0 and pid /= Unsigned_64'Last then
         cwdBuf (1 .. 8) := "@nvme:0/";
         cwdLen := 8;
         debugPrint ("shell: cwd=@nvme:0/" & LF);
      else
         pid := getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_ATA);
         if pid /= 0 and pid /= Unsigned_64'Last then
            cwdBuf (1 .. 7) := "@ata:0/";
            cwdLen := 7;
            debugPrint ("shell: cwd=@ata:0/" & LF);
         else
            cwdLen := 0;
            debugPrint ("shell: cwd=/ (ramdisk)" & LF);
         end if;
      end if;
   end;

   --  Flush stale events that accumulated during initialization.
   --  PS/2 mouse init generates IRQs that can produce spurious keyboard
   --  events (e.g. Intellimouse device ID 0x03 = scancode '2').
   declare
      flushMsg : Message;
      flushed  : Boolean;
   begin
      loop
         flushed := Poll_Event (flushMsg);
         exit when not flushed;
      end loop;
   end;

   --  Welcome banner
   putStr ("CuBitOS Shell v0.1" & LF);
   putStr ("Type 'help' for available commands." & LF);
   putChar (LF);
   printPrompt;

   --  Main loop: poll keyboard events, or wait for foreground child
   loop
      if foregroundPID /= 0 then
         --  Poll for child exit event. We use Poll_Event because the
         --  legacy blocking Wait_Event path only returns the tag.
         declare
            found : Boolean;
         begin
            found := Poll_Event (eventMsg);
            if found then
               debugPrint ("shell: FG event label=");
               printDec (eventMsg.tag.label);
               debugPrint (" w0=");
               printDec64 (eventMsg.words (0));
               debugPrint (" want=");
               printDec64 (foregroundPID);
               debugPrint ("" & LF);
            end if;
            if found and then
               eventMsg.tag.label = EVENT_CHILD_EXIT and then
               eventMsg.words (0) = foregroundPID
            then
               debugPrint ("shell: child exited, reclaiming input" & LF);
               foregroundPID := 0;
               --  Grant auto-revoked by kernel on child exit, so don't
               --  try to drain the ring — the mapped pages are gone.
               childStream.active := False;
               --  Note: streamSubPending is NOT cleared here.  If the
               --  subscription completion hasn't arrived yet, it will
               --  be drained by the poll loop below (or after the
               --  foreground block, at the top of the main loop).

               --  Reclaim keyboard and mouse focus
               declare
                  ret : Unsigned_64;
               begin
                  ret := registerDriver (DRIVER_KEYBOARD);
                  debugPrint ("shell: registerDriver kbd=");
                  printDec (Unsigned_32 (ret));
                  ret := registerDriver (DRIVER_MOUSE);
                  debugPrint (" mouse=");
                  printDec (Unsigned_32 (ret));
                  debugPrint ("" & LF);
               end;
               --  Mark all lines dirty and redraw
               for row in 0 .. rows - 1 loop
                  dirty (row) := True;
               end loop;
               fullPresentNeeded := True;
               setupDisplayPresent;
               renderDirty;
               printPrompt;
            elsif found and then
               eventMsg.tag.label = EVENT_CAP_FAULT
            then
               debugPrint ("shell: cap fault pid=");
               printDec (Unsigned_32 (eventMsg.words (0)));
               debugPrint (" syscall=");
               printDec (Unsigned_32 (eventMsg.words (1)));
               debugPrint ("" & LF);
            elsif found and then
               eventMsg.tag.label = CuBit.Streams.OP_STREAM_AVAILABLE
            then
               debugPrint ("shell: stream available pid=");
               printDec (Unsigned_32 (eventMsg.words (0)));
               debugPrint (" mask=");
               printDec (Unsigned_32 (eventMsg.words (1)));
               debugPrint ("" & LF);
            elsif found and then eventMsg.tag.label = 1 then
               --  Keyboard event while foreground running: track Ctrl+C
               declare
                  raw : constant Unsigned_8 :=
                     Unsigned_8 (eventMsg.words (0) and 16#FF#);
                  isRel : constant Boolean := (raw and 16#80#) /= 0;
                  sc    : constant Unsigned_8 := raw and 16#7F#;
               begin
                  if sc = 16#1D# then
                     ctrlDown := not isRel;
                  elsif not isRel and ctrlDown and sc = 16#2E# then
                     if foregroundPID /= 0 then
                        --  Kill revokes grants, so deactivate stream
                        --  before kill to prevent reading revoked memory.
                        childStream.active := False;
                        streamSubPending := False;
                        declare
                           ret : Unsigned_64;
                        begin
                           ret := killProcess (foregroundPID);
                           debugPrint ("shell: Ctrl+C kill pid=");
                           printDec (Unsigned_32 (foregroundPID));
                           debugPrint (" ret=");
                           printDec (Unsigned_32 (ret));
                           debugPrint ("" & LF);
                        end;
                     end if;
                  end if;
               end;
            end if;

            --  Check for stream subscription completion
            if streamSubPending and not childStream.active then
               declare
                  comp : CompletionEntry;
                  ret  : Unsigned_64;
               begin
                  ret := Poll_Completion (comp'Address);
                  if ret = 1 and then comp.token = STREAM_SUB_TOKEN
                     and then comp.msg.tag.label = REPLY_OK
                  then
                     if foregroundPID /= 0 then
                        --  Child still alive: activate the stream.
                        childStream := (
                           active    => True,
                           grantBase => GRANT_REGION_BASE +
                              comp.msg.words (0) * GRANT_SLOT_SIZE,
                           cursor    => CuBit.Streams.CursorSlot (
                              comp.msg.words (1)),
                           cursorIdx => Unsigned_32 (comp.msg.words (3)),
                           capacity  => Unsigned_32 (comp.msg.words (2)));
                        debugPrint ("shell: stream subscribed" & LF);
                     end if;
                     --  Either way, the completion is consumed.
                     streamSubPending := False;
                  end if;
               end;
            end if;

            --  Poll child's stdout ring buffer
            if childStream.active then
               declare
                  tt      : CuBit.Streams.TypeTag;
                  n       : Unsigned_32;
                  gotData : Boolean := False;
               begin
                  loop
                     n := CuBit.Streams.streamRead (
                        childStream, streamRdBuf'Address, 512, tt);
                     exit when n = 0;
                     gotData := True;
                     for i in 0 .. Natural (n) - 1 loop
                        if streamRdBuf (i) = 10 then
                           putChar (LF);
                        elsif streamRdBuf (i) >= 32
                           and streamRdBuf (i) < 127
                        then
                           putChar (Character'Val (Natural (streamRdBuf (i))));
                        end if;
                     end loop;
                  end loop;
                  if gotData then
                     renderDirty;
                  end if;
               end;
            end if;

            if not found and not childStream.active then
               declare
                  ignore : Unsigned_64;
               begin
                  ignore := syscall (SYSCALL_SLEEP, 5);
               end;
            end if;
         end;
      else
         --  Normal: process keyboard events
         declare
            found : Boolean;
         begin
            found := Poll_Event (eventMsg);
            if found then
               --  Only process keyboard events (label=1), skip mouse/other
               if eventMsg.tag.label = 1 then
                  declare
                     raw : constant Unsigned_8 :=
                        Unsigned_8 (eventMsg.words (0) and 16#FF#);
                  begin
                     processKey (raw);
                  end;
               elsif eventMsg.tag.label = EVENT_CAP_FAULT then
                  putStr ("cap fault: pid=");
                  putDec (Unsigned_32 (eventMsg.words (0)));
                  putStr (" syscall=");
                  putDec (Unsigned_32 (eventMsg.words (1)));
                  putChar (LF);
                  renderDirty;
               end if;
            else
               declare
                  ignore : Unsigned_64;
               begin
                  ignore := syscall (SYSCALL_SLEEP, 1);
               end;
            end if;
         end;
      end if;

      --  Drain stale stream subscription completion after child exit.
      --  The child may have replied before dying (completion pending) or
      --  died before processing the subscribe (no completion ever comes).
      --  Poll a few times, then give up.
      if streamSubPending and foregroundPID = 0 then
         declare
            comp : CompletionEntry;
            ret  : Unsigned_64;
         begin
            ret := Poll_Completion (comp'Address);
            if ret = 1 then
               streamSubPending := False;
               streamDrainPolls := 0;
            else
               streamDrainPolls := streamDrainPolls + 1;
               if streamDrainPolls > 50 then
                  --  Give up: child died before replying.
                  streamSubPending := False;
                  streamDrainPolls := 0;
               end if;
            end if;
         end;
      end if;
   end loop;
end main;
