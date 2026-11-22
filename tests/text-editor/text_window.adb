with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with System;
with CuBit.Appearance;
with CuBit.UI;
with Text_Commands; use Text_Commands;
with Text_Shell; use Text_Shell;
with Text_Host_Files; use Text_Host_Files;
with Text_Host_Support; use Text_Host_Support;

--  CuBit Text in a Linux window (SDL2): the same shell and core, real
--  keyboard, text and pointer input, partial presents of what changed.
--    tests/text-editor/run.sh --window [--dark] [file...]
--  Files open under the granted roots: the repository (read-only) and
--  /home/doc/cubit-build-tmp/text-editor/files (read-write). F12 shows the
--  timing overlay, Ctrl+Q quits. TEXT_WINDOW_SNAPSHOT=path.ppm saves the
--  window once it settles, then exits.
procedure Text_Window is
   INITIAL_WIDTH : constant := 1400;
   INITIAL_HEIGHT : constant := 860;
   IDLE_WAIT_MS : constant := 500;
   PUMP_BUDGET : constant := 50_000;
   EDIT_BYTES : constant := 256 * 1_048_576;
   TEXT_BYTES : constant := 32;
   LEFT_BUTTON : constant := 1;
   REPO : constant String := "/home/doc/git/cubit";
   SCRATCH : constant String := "/home/doc/cubit-build-tmp/text-editor/files";

   --  As struct text_sdl_event in sdl/text_sdl.c.
   type SDL_Kind is (None, Key_Kind, Text_Kind, Down, Up, Motion, Wheel_Kind, Resized, Quit_Kind) with Convention => C;
   for SDL_Kind use (None => 0, Key_Kind => 1, Text_Kind => 2, Down => 3, Up => 4, Motion => 5, Wheel_Kind => 6,
                     Resized => 7, Quit_Kind => 8);
   type SDL_Event is record
      Kind : SDL_Kind;
      Scancode : int;
      Shift, Control, Alt : int;
      X, Y : int;
      Wheel : int;
      Button : int;
      Time_Ms : unsigned;
      Text : char_array (1 .. TEXT_BYTES);
   end record with Convention => C;

   function SDL_Open (Width, Height : int) return int with Import, Convention => C, External_Name => "text_sdl_open";
   function SDL_Surface (Pixels : out System.Address; Width, Height, Pitch : out int) return int
     with Import, Convention => C, External_Name => "text_sdl_surface";
   procedure SDL_Present (X, Y, W, H : int) with Import, Convention => C, External_Name => "text_sdl_present";
   function SDL_Next (Item : out SDL_Event; Timeout_Ms : int) return int
     with Import, Convention => C, External_Name => "text_sdl_next";
   procedure SDL_Close with Import, Convention => C, External_Name => "text_sdl_close";

   --  USB HID usages (SDL_Scancode).
   HID_A : constant := 4;
   HID_Z : constant := 29;
   HID_1 : constant := 30;
   HID_0 : constant := 39;
   HID_RETURN : constant := 40;
   HID_ESCAPE : constant := 41;
   HID_BACKSPACE : constant := 42;
   HID_TAB : constant := 43;
   HID_SPACE : constant := 44;
   HID_MINUS : constant := 45;
   HID_EQUALS : constant := 46;
   HID_LEFT_BRACKET : constant := 47;
   HID_RIGHT_BRACKET : constant := 48;
   HID_BACKSLASH : constant := 49;
   HID_SEMICOLON : constant := 51;
   HID_QUOTE : constant := 52;
   HID_GRAVE : constant := 53;
   HID_COMMA : constant := 54;
   HID_PERIOD : constant := 55;
   HID_SLASH : constant := 56;
   HID_F1 : constant := 58;
   HID_F12 : constant := 69;
   HID_INSERT : constant := 73;
   HID_HOME : constant := 74;
   HID_PAGE_UP : constant := 75;
   HID_DELETE : constant := 76;
   HID_END : constant := 77;
   HID_PAGE_DOWN : constant := 78;
   HID_RIGHT : constant := 79;
   HID_LEFT : constant := 80;
   HID_DOWN : constant := 81;
   HID_UP : constant := 82;

   function Key_Of (Code : int) return Text_Commands.Key is
     (case Code is
         when HID_A .. HID_Z => Text_Commands.Key'Val (Text_Commands.Key'Pos (Key_A) + Integer (Code) - HID_A),
         when HID_1 .. HID_1 + 8 => Text_Commands.Key'Val (Text_Commands.Key'Pos (Key_1) + Integer (Code) - HID_1),
         when HID_0 => Key_0,
         when HID_F1 .. HID_F12 => Text_Commands.Key'Val (Text_Commands.Key'Pos (F1) + Integer (Code) - HID_F1),
         when HID_RETURN => Enter, when HID_ESCAPE => Escape, when HID_BACKSPACE => Backspace, when HID_TAB => Tab,
         when HID_SPACE => Space, when HID_MINUS => Minus, when HID_EQUALS => Equals,
         when HID_LEFT_BRACKET => Left_Bracket, when HID_RIGHT_BRACKET => Right_Bracket,
         when HID_BACKSLASH => Backslash, when HID_SEMICOLON => Semicolon, when HID_QUOTE => Quote,
         when HID_GRAVE => Grave, when HID_COMMA => Comma, when HID_PERIOD => Period, when HID_SLASH => Slash,
         when HID_INSERT => Insert_Key, when HID_HOME => Home, when HID_PAGE_UP => Page_Up, when HID_DELETE => Delete,
         when HID_END => End_Key, when HID_PAGE_DOWN => Page_Down, when HID_RIGHT => Right, when HID_LEFT => Left,
         when HID_DOWN => Down, when HID_UP => Up,
         when others => No_Key);

   Dark : constant Boolean :=
     (for some K in 1 .. Ada.Command_Line.Argument_Count => Ada.Command_Line.Argument (K) = "--dark");
   SNAPSHOT : constant String :=
     (if Ada.Environment_Variables.Exists ("TEXT_WINDOW_SNAPSHOT")
      then Ada.Environment_Variables.Value ("TEXT_WINDOW_SNAPSHOT") else "");

   Host : aliased Host_Files;
   App : Shell;
   Item : SDL_Event;
   Running : Boolean := True;
   Busy : Boolean := True;
   Changed : Boolean;
   Damage : CuBit.UI.Rect;
   Pixels_At : System.Address;
   Width, Height, Pitch : int;
   Pump_Us : Unsigned_64 := 0;
   Forced_Full : Boolean := True;

   procedure Save_Window is
      BYTES_PER_PIXEL : constant := 4;
      Copy : constant Surface := New_Surface (Positive (Width), Positive (Height));
      Rows : constant Pixels (0 .. Natural (Height) * Natural (Pitch) / BYTES_PER_PIXEL - 1)
        with Import, Address => Pixels_At;
   begin
      for Y in 0 .. Natural (Height) - 1 loop
         for X in 0 .. Natural (Width) - 1 loop
            Copy.Image (Y * Natural (Width) + X) := Rows (Y * Natural (Pitch) / BYTES_PER_PIXEL + X);
         end loop;
      end loop;
      Save_PPM (Copy, SNAPSHOT);
   end Save_Window;

   function Wait_Ms return int is
      Due : constant Unsigned_64 := Next_Deadline_Us (App);
      Now : constant Unsigned_64 := Now_Us;
   begin
      return (if Due = Unsigned_64'Last then IDLE_WAIT_MS
              elsif Due <= Now then 1
              else int (Unsigned_64'Min ((Due - Now) / 1_000 + 1, IDLE_WAIT_MS)));
   end Wait_Ms;

   procedure Translate (E : SDL_Event) is
      Shift : constant Boolean := E.Shift /= 0;
      Control : constant Boolean := E.Control /= 0;
      Alt : constant Boolean := E.Alt /= 0;
   begin
      Set_Clock (App, Now_Us);
      case E.Kind is
         when Key_Kind =>
            if Key_Of (E.Scancode) /= No_Key then
               --  Printable keys without Ctrl or Alt arrive as text.
               if Control or else Alt or else Key_Of (E.Scancode) not in Key_A .. Key_9 | Space .. Grave then
                  Handle (App, Text_Shell.Key (Key_Of (E.Scancode), Control, Shift, Alt));
               end if;
            end if;
         when Text_Kind =>
            declare
               Text : constant String := To_Ada (E.Text);
            begin
               if Text'Length in 1 .. MAXIMUM_EVENT_TEXT then
                  Handle (App, Typed (Text));
               end if;
            end;
         when Down | Up =>
            if E.Button = LEFT_BUTTON then
               Handle (App, (Kind => Pointer_Event, Action => (if E.Kind = Down then Press else Release),
                             X => Natural (E.X), Y => Natural (E.Y), Time_Ms => Unsigned_64 (E.Time_Ms),
                             Control => Control, Shift => Shift, Alt => Alt, others => <>));
            end if;
         when Motion =>
            Handle (App, (Kind => Pointer_Event, Action => Move, X => Natural (E.X), Y => Natural (E.Y),
                          others => <>));
         when Wheel_Kind =>
            Handle (App, (Kind => Wheel_Event, Steps => Integer (E.Wheel), X => Natural (E.X), Y => Natural (E.Y),
                          others => <>));
         when Resized =>
            Forced_Full := True;
         when Quit_Kind =>
            Running := False;
         when None => null;
      end case;
   end Translate;
begin
   Ada.Directories.Create_Path (SCRATCH);
   Install_Themes ((if Dark then CuBit.Appearance.Alloy_Dark else CuBit.Appearance.Alloy_Light));
   Grant (Host, REPO, Writable => False, Label => "repo");
   Grant (Host, SCRATCH, Writable => True, Label => "scratch");
   Offer_Files (Host, Depth => 4, Limit => 8_000);
   if SDL_Open (INITIAL_WIDTH, INITIAL_HEIGHT) = 0 then
      Ada.Text_IO.Put_Line ("text-window: no SDL window (is DISPLAY or WAYLAND_DISPLAY set?)");
      return;
   end if;
   Initialize (App, Host'Unchecked_Access, Dark, EDIT_BYTES);
   for K in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Name : constant String := Ada.Command_Line.Argument (K);
      begin
         if Name /= "--dark" then
            Open (App, (if Name'Length > 0 and then Name (Name'First) = '/' then Name
                        else Ada.Directories.Full_Name (Name)));
            --  One load at a time: let it finish.
            loop
               Pump (App, PUMP_BUDGET, Busy, Changed);
               exit when not Loading (App);
            end loop;
         end if;
      end;
   end loop;
   if Tabs (App) = 0 then
      New_Document (App, SCRATCH & "/untitled.txt");
   end if;
   while Running and then not Quit_Requested (App) loop
      if SDL_Next (Item, (if Busy then 0 else Wait_Ms)) /= 0 then
         Translate (Item);
         while SDL_Next (Item, 0) /= 0 loop
            Translate (Item);
         end loop;
      end if;
      declare
         Start : constant Unsigned_64 := Now_Us;
      begin
         Set_Clock (App, Start);
         Pump (App, PUMP_BUDGET, Busy, Changed);
         Pump_Us := Now_Us - Start;
      end;
      if SDL_Surface (Pixels_At, Width, Height, Pitch) /= 0 then
         declare
            Full : constant CuBit.UI.Rect := (0, 0, Natural (Width), Natural (Height));
            Target : constant CuBit.UI.Canvas :=
              (addr => Pixels_At, width => Natural (Width), height => Natural (Height), pitch => Natural (Pitch),
               others => <>);
            Start : constant Unsigned_64 := Now_Us;
         begin
            Set_Bounds (App, Full);
            Take_Damage (App, Damage);
            if Forced_Full then
               Damage := Full;
               Set_Bounds (App, (0, 0, 0, 0));
               Set_Bounds (App, Full);
               Take_Damage (App, Damage);
               Damage := Full;
            end if;
            if not CuBit.UI.Is_Empty (Damage) then
               Render (App, CuBit.UI.With_Clip (Target, Damage), Full);
               Note_Frame (App, Now_Us - Start, Pump_Us);
               SDL_Present (int (Damage.x), int (Damage.y), int (Damage.w), int (Damage.h));
               Forced_Full := False;
            elsif SNAPSHOT'Length > 0 and then not Busy then
               Save_Window;
               Running := False;
            end if;
         end;
      end if;
   end loop;
   SDL_Close;
end Text_Window;
