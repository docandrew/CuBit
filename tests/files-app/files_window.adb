with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with System;
with CuBit.Appearance;
with CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.Icons;
with CuBit.UI.State;
with Files_Limits;
with Files_Mock_Service;
with Files_View; use Files_View;
with Files_Host_Support; use Files_Host_Support;

--  CuBit Files in a Linux window (SDL2), for iterating on the UI: the same
--  view, the mock filesystem service, real keyboard, text and pointer input,
--  partial presents of what changed. F12 shows the timing overlay.
--    tests/files-app/run.sh --window [--theme light|dark] [left-path [right-path]]
--  F11 switches light and dark (the toolkit's Alloy schemes).
--  Paths are places: @host:0/... (the host's /, read-only), @scratch:0/
--  (read-write), @synthetic:N/ (N generated entries).
procedure Files_Window is
   use type CuBit.Appearance.Color_Scheme;
   INITIAL_WIDTH : constant := 1400;
   INITIAL_HEIGHT : constant := 860;
   CAPACITY : constant := 2_000_000;
   NAME_BYTES : constant := CAPACITY * 24;
   --  A frame's slice of sorting and filtering (tests/files-app bench).
   FRAME_BUDGET : constant Files_Limits.Work_Budget := 20_000;
   --  The longest sleep with nothing due: how often an idle window looks at
   --  the namespace generation (until change notifications exist).
   IDLE_WAIT_MS : constant := 500;
   LEFT_BUTTON : constant := 1;
   TEXT_BYTES : constant := 32;

   --  As struct files_sdl_event in sdl/files_sdl.c.
   type SDL_Kind is (None, Key, Text, Down, Up, Move, Wheel_Event, Resized, Quit) with Convention => C;
   for SDL_Kind use (None => 0, Key => 1, Text => 2, Down => 3, Up => 4, Move => 5, Wheel_Event => 6,
                     Resized => 7, Quit => 8);
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

   function SDL_Open (Width, Height : int) return int with Import, Convention => C, External_Name => "files_sdl_open";
   function SDL_Surface (Pixels : out System.Address; Width, Height, Pitch : out int) return int
     with Import, Convention => C, External_Name => "files_sdl_surface";
   procedure SDL_Present (X, Y, W, H : int) with Import, Convention => C, External_Name => "files_sdl_present";
   function SDL_Next (Item : out SDL_Event; Timeout_Ms : int) return int
     with Import, Convention => C, External_Name => "files_sdl_next";
   procedure SDL_Close with Import, Convention => C, External_Name => "files_sdl_close";
   procedure SDL_Wake with Import, Convention => C, External_Name => "files_sdl_wake";

   --  USB HID usages (SDL_Scancode).
   HID_A : constant := 4;
   HID_B : constant := 5;
   HID_N : constant := 17;
   HID_T : constant := 23;
   HID_W : constant := 26;
   HID_D : constant := 7;
   HID_APPLICATION : constant := 101;
   HID_F10 : constant := 67;
   RIGHT_BUTTON : constant := 3;
   MIDDLE_BUTTON : constant := 2;
   HID_I : constant := 12;
   HID_R : constant := 21;
   HID_U : constant := 24;
   HID_RETURN : constant := 40;
   HID_ESCAPE : constant := 41;
   HID_BACKSPACE : constant := 42;
   HID_TAB : constant := 43;
   HID_SPACE : constant := 44;
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

   function Key_Of (Code : int) return Key_Name is
     (case Code is
         when HID_A => Letter_A, when HID_B => Letter_B, when HID_D => Letter_D, when HID_APPLICATION => Menu_Key,
         when HID_N => Letter_N, when HID_T => Letter_T, when HID_W => Letter_W,
         when HID_I => Letter_I, when HID_R => Letter_R, when HID_U => Letter_U,
         when HID_RETURN => Enter, when HID_ESCAPE => Escape, when HID_BACKSPACE => Backspace,
         when HID_TAB => Tab, when HID_SPACE => Space,
         when HID_F1 .. HID_F12 => Key_Name'Val (Key_Name'Pos (F1) + Integer (Code) - HID_F1),
         when HID_INSERT => Insert, when HID_HOME => Home, when HID_PAGE_UP => Page_Up,
         when HID_DELETE => Delete, when HID_END => End_Key, when HID_PAGE_DOWN => Page_Down,
         when HID_RIGHT => Right, when HID_LEFT => Left, when HID_DOWN => Down, when HID_UP => Up,
         when others => No_Key);

   --  Positional arguments (places) after any "--theme light|dark".
   function Option_Shift return Natural is
     (if Ada.Command_Line.Argument_Count >= 2 and then Ada.Command_Line.Argument (1) = "--theme" then 2 else 0);
   function Argument (N : Positive; Default : String) return String is
     (if Ada.Command_Line.Argument_Count >= N + Option_Shift then Ada.Command_Line.Argument (N + Option_Shift)
      else Default);
   --  The toolkit's real schemes (CuBit.UI.Palette): what Settings applies
   --  live on CuBit. F11 here switches them, as the platform would.
   Scheme : CuBit.Appearance.Color_Scheme :=
     (if Option_Shift = 2 and then Ada.Command_Line.Argument (2) = "dark" then CuBit.Appearance.Alloy_Dark
      else CuBit.Appearance.Alloy_Light);
   HID_F11 : constant := 68;
   Home_Place : constant String :=
     "@host:0" & (if Ada.Environment_Variables.Exists ("HOME") then Ada.Environment_Variables.Value ("HOME")
                  else "/");

   View : constant View_Access := new View_State;
   UI : constant UI_Access := new CuBit.UI.State.UI_State;
   Map : constant Map_Access := new CuBit.UI.Controls.Control_Map;
   Item : SDL_Event;
   Running : Boolean := True;
   Busy : Boolean := True;
   Changed, Redraw : Boolean;
   Damage : CuBit.UI.Rect;
   Pixels_At : System.Address;
   Width, Height, Pitch : int;
   Pump_Us : Unsigned_64 := 0;
   Forced_Full : Boolean := True;
   --  FILES_WINDOW_SNAPSHOT=path: save the window once settled, then quit
   --  (checks the interactive path without a person).
   SNAPSHOT : constant String :=
     (if Ada.Environment_Variables.Exists ("FILES_WINDOW_SNAPSHOT")
      then Ada.Environment_Variables.Value ("FILES_WINDOW_SNAPSHOT") else "");

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

   procedure Wake_Window is
   begin
      SDL_Wake;
   end Wake_Window;

   --  How long the loop may sleep: until input, the service's wake (an
   --  event) or the view's next deadline, at most IDLE_WAIT_MS.
   function Wait_Ms return int is
      Due : constant Unsigned_64 := Next_Deadline_Us (View.all);
      Now : constant Unsigned_64 := Now_Us;
   begin
      return (if Due = Unsigned_64'Last then IDLE_WAIT_MS
              elsif Due <= Now then 1
              else int (Unsigned_64'Min ((Due - Now) / 1_000 + 1, IDLE_WAIT_MS)));
   end Wait_Ms;

   procedure Send (E : Event) is
   begin
      Set_Clock (View.all, Now_Us);
      Handle (View.all, E, Map.all, Redraw);
   end Send;

   procedure Translate (E : SDL_Event) is
      Shift : constant Boolean := E.Shift /= 0;
      Control : constant Boolean := E.Control /= 0;
      Alt : constant Boolean := E.Alt /= 0;
   begin
      Set_Clock (View.all, Now_Us);
      case E.Kind is
         when Key =>
            if E.Scancode = HID_F11 then
               Scheme := (if Scheme = CuBit.Appearance.Alloy_Light then CuBit.Appearance.Alloy_Dark
                          else CuBit.Appearance.Alloy_Light);
               CuBit.UI.Set_Color_Scheme (Scheme);
               Forced_Full := True;
               Send ((Kind => Resize, others => <>));
            elsif E.Scancode = HID_F10 and then Shift then
               Send ((Kind => Key_Event, Key => Menu_Key, others => <>));
            elsif Key_Of (E.Scancode) /= No_Key then
               Send ((Kind => Key_Event, Key => Key_Of (E.Scancode), Shift => Shift, Control => Control,
                      Alt => Alt, others => <>));
            end if;
         when Text =>
            for C of To_Ada (E.Text) loop
               if C in ' ' .. '~' then
                  Send ((Kind => Text_Event, Character_Value => C, others => <>));
               end if;
            end loop;
         when Down | Up =>
            if E.Button = RIGHT_BUTTON then
               Pointer (View.all, UI.all, Map.all,
                        (if E.Kind = Down then CuBit.UI.Controls.Pointer_Press else CuBit.UI.Controls.Pointer_Release),
                        Natural (E.X), Natural (E.Y), Unsigned_64 (E.Time_Ms), Control, Shift, Secondary => True);
            elsif E.Button = MIDDLE_BUTTON then
               Pointer (View.all, UI.all, Map.all,
                        (if E.Kind = Down then CuBit.UI.Controls.Pointer_Press else CuBit.UI.Controls.Pointer_Release),
                        Natural (E.X), Natural (E.Y), Unsigned_64 (E.Time_Ms), Control, Shift, Middle => True);
            elsif E.Button = LEFT_BUTTON then
               Pointer (View.all, UI.all, Map.all,
                        (if E.Kind = Down then CuBit.UI.Controls.Pointer_Press else CuBit.UI.Controls.Pointer_Release),
                        Natural (E.X), Natural (E.Y), Unsigned_64 (E.Time_Ms), Control, Shift);
            end if;
         when Move =>
            Pointer (View.all, UI.all, Map.all, CuBit.UI.Controls.Pointer_Move, Natural (E.X), Natural (E.Y),
                     Unsigned_64 (E.Time_Ms));
         when Wheel_Event =>
            Send ((Kind => Wheel, Steps => Integer (E.Wheel), X => Natural (E.X), Y => Natural (E.Y), others => <>));
         when Resized =>
            Forced_Full := True;
            Send ((Kind => Resize, X => Natural (E.X), Y => Natural (E.Y), others => <>));
         when Quit =>
            Running := False;
         when None => null;
      end case;
   end Translate;
begin
   Configure_Service ("/");
   Install_Themes (Scheme);
   --  The service's wake posts an event to this loop (OP_FS_WAKE).
   Files_Mock_Service.Set_Wake_Hook (Wake_Window'Unrestricted_Access);
   if SDL_Open (INITIAL_WIDTH, INITIAL_HEIGHT) = 0 then
      Ada.Text_IO.Put_Line ("files-window: no SDL window (is DISPLAY or WAYLAND_DISPLAY set?)");
      return;
   end if;
   Initialize (View.all, CAPACITY, NAME_BYTES, Argument (1, Home_Place), Argument (2, "@synthetic:100000/"));
   Add_Place (View.all, "Home", Home_Place, CuBit.UI.Icons.Home_Folder);
   Add_Place (View.all, "Host root", "@host:0/", CuBit.UI.Icons.Drive);
   Add_Place (View.all, "Scratch", "@scratch:0/", CuBit.UI.Icons.Folder);
   Add_Place (View.all, "100k synthetic", "@synthetic:100000/", CuBit.UI.Icons.Network);
   Add_Place (View.all, "1M synthetic", "@synthetic:1000000/", CuBit.UI.Icons.Network);
   while Running and then not Quit_Requested (View.all) loop
      --  Wait only while there is nothing to do; then take every queued event.
      if SDL_Next (Item, (if Busy then 0 else Wait_Ms)) /= 0 then
         Translate (Item);
         while SDL_Next (Item, 0) /= 0 loop
            Translate (Item);
         end loop;
      end if;
      declare
         Start : constant Unsigned_64 := Now_Us;
      begin
         Pump (View.all, FRAME_BUDGET, Start, Busy, Changed);
         Pump_Us := Now_Us - Start;
      end;
      Take_Damage (View.all, Damage);
      if SDL_Surface (Pixels_At, Width, Height, Pitch) /= 0 then
         declare
            Full : constant CuBit.UI.Rect := (0, 0, Natural (Width), Natural (Height));
            Area : constant CuBit.UI.Rect := (if Forced_Full then Full else Damage);
            Target : constant CuBit.UI.Canvas :=
              (addr => Pixels_At, width => Natural (Width), height => Natural (Height), pitch => Natural (Pitch),
               others => <>);
            Start : constant Unsigned_64 := Now_Us;
         begin
            if not CuBit.UI.Is_Empty (Area) then
               Render (View.all, CuBit.UI.With_Clip (Target, Area), Full, UI.all, Map.all);
               Note_Frame (View.all, Now_Us - Start, Pump_Us);
               SDL_Present (int (Area.x), int (Area.y), int (Area.w), int (Area.h));
               Forced_Full := False;
            elsif SNAPSHOT'Length > 0 and then not Busy then
               Save_Window;
               Running := False;
            end if;
         end;
      end if;
   end loop;
   Close (View.all);
   SDL_Close;
end Files_Window;
