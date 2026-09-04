------------------------------------------------------------------------------
--  CuBit Control Language Workbench native desktop adapter
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use type System.Address;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Protocols;
with CuBit.String;
with CuBit.UI;
with CuBit.UI.App;

package body CCL_Workbench_Platform is

   MINIMUM_WIDTH  : constant Natural := 900;
   MINIMUM_HEIGHT : constant Natural := 400;
   MAXIMUM_WIDTH  : constant Natural := 1_280;
   MAXIMUM_HEIGHT : constant Natural := 720;
   REPLY_OK : constant Unsigned_32 := 16#F000#;

   KEY_BACKSPACE : constant Unsigned_64 := 16#0E#;
   KEY_ENTER     : constant Unsigned_64 := 16#1C#;
   KEY_A         : constant Unsigned_64 := 16#1E#;
   KEY_D         : constant Unsigned_64 := 16#20#;
   KEY_F         : constant Unsigned_64 := 16#21#;
   KEY_Y         : constant Unsigned_64 := 16#15#;
   KEY_Z         : constant Unsigned_64 := 16#2C#;
   KEY_RIGHT_BRACKET : constant Unsigned_64 := 16#1B#;
   KEY_BACKSLASH : constant Unsigned_64 := 16#2B#;
   KEY_F3        : constant Unsigned_64 := 16#3D#;
   KEY_F5        : constant Unsigned_64 := 16#3F#;
   KEY_HOME      : constant Unsigned_64 := 16#47#;
   KEY_UP        : constant Unsigned_64 := 16#48#;
   KEY_PAGE_UP   : constant Unsigned_64 := 16#49#;
   KEY_LEFT      : constant Unsigned_64 := 16#4B#;
   KEY_RIGHT     : constant Unsigned_64 := 16#4D#;
   KEY_END       : constant Unsigned_64 := 16#4F#;
   KEY_DOWN      : constant Unsigned_64 := 16#50#;
   KEY_PAGE_DOWN : constant Unsigned_64 := 16#51#;
   KEY_DELETE    : constant Unsigned_64 := 16#53#;

   MULTI_CLICK_MS     : constant Unsigned_64 := 500;
   MULTI_CLICK_RADIUS : constant Integer_64 := 5;

   Native_Window : aliased CuBit.UI.App.Window;
   Native_Open : Boolean := False;
   First_Frame_Presented : Boolean := False;
   Current_Modifiers : Unsigned_64 := 0;
   Last_Click_Ms : Unsigned_64 := 0;
   Click_Origin_X : Integer_64 := 0;
   Click_Origin_Y : Integer_64 := 0;
   Click_Count : Natural range 0 .. 3 := 0;

   procedure Activate is
   begin
      null;
   end Activate;

   function Window_Open
     (Width, Height : Integer_32) return System.Address
   with Export, Convention => C, External_Name => "ccl_window_open";

   function Window_Open
     (Width, Height : Integer_32) return System.Address
   is
      OK : Boolean;
      Requested_Width : Natural;
      Requested_Height : Natural;
      Flags : constant Unsigned_64 :=
        CuBit.UI.App.WINDOW_FLAG_DECORATED or
        CuBit.UI.App.WINDOW_FLAG_RESIZABLE or
        CuBit.UI.App.WINDOW_FLAG_MINIMIZABLE or
        CuBit.UI.App.WINDOW_FLAG_MAXIMIZABLE or
        CuBit.UI.App.WINDOW_FLAG_CLOSEABLE;
   begin
      if Width <= 0 or else Height <= 0 then
         return System.Null_Address;
      end if;
      Requested_Width := Natural'Max (MINIMUM_WIDTH, Natural (Width));
      Requested_Height := Natural'Max (MINIMUM_HEIGHT, Natural (Height));
      CuBit.UI.App.Open
        (Native_Window, Requested_Width, Requested_Height, Flags, OK,
         maximum_width => MAXIMUM_WIDTH,
         maximum_height => MAXIMUM_HEIGHT);
      Native_Open := OK;
      if OK then
         debugPrint ("ccl-workbench: native window ready" & ASCII.LF);
      else
         debugPrint ("ccl-workbench: native window failed" & ASCII.LF);
      end if;
      return (if OK then Native_Window'Address else System.Null_Address);
   end Window_Open;

   function Window_Has_System_Chrome return Integer_32
   with Export, Convention => C,
        External_Name => "ccl_window_has_system_chrome";

   function Window_Has_System_Chrome return Integer_32 is (1);

   function Window_Prepare_Frame
     (Handle : System.Address;
      Minimum_Width, Minimum_Height : Integer_32;
      Maximum_Width, Maximum_Height : Integer_32;
      Width, Height : access Integer_32) return Integer_32
   with Export, Convention => C,
        External_Name => "ccl_window_prepare_frame";

   function Window_Prepare_Frame
     (Handle : System.Address;
      Minimum_Width, Minimum_Height : Integer_32;
      Maximum_Width, Maximum_Height : Integer_32;
      Width, Height : access Integer_32) return Integer_32
   is
      pragma Unreferenced
        (Handle, Minimum_Width, Minimum_Height, Maximum_Width, Maximum_Height);
   begin
      if not Native_Open or else
        not CuBit.UI.App.Is_Open (Native_Window)
      then
         return 1;
      end if;
      Width.all := Integer_32 (CuBit.UI.App.Width (Native_Window));
      Height.all := Integer_32 (CuBit.UI.App.Height (Native_Window));
      return 0;
   end Window_Prepare_Frame;

   procedure Decode_Pointer
     (Packed : Unsigned_64; X, Y : access Integer_32)
   is
   begin
      X.all := Integer_32 (Packed and 16#FFFF_FFFF#);
      Y.all := Integer_32 (Shift_Right (Packed, 32));
   end Decode_Pointer;

   function Count_Click (X, Y : Integer_32) return Natural is
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      DX : constant Integer_64 := Integer_64 (X) - Click_Origin_X;
      DY : constant Integer_64 := Integer_64 (Y) - Click_Origin_Y;
      Continues : constant Boolean :=
        Click_Count > 0 and then Click_Count < 3 and then
        Now - Last_Click_Ms <= MULTI_CLICK_MS and then
        DX >= -MULTI_CLICK_RADIUS and then DX <= MULTI_CLICK_RADIUS and then
        DY >= -MULTI_CLICK_RADIUS and then DY <= MULTI_CLICK_RADIUS;
   begin
      if Continues then
         Click_Count := Click_Count + 1;
      else
         Click_Count := 1;
         Click_Origin_X := Integer_64 (X);
         Click_Origin_Y := Integer_64 (Y);
      end if;
      Last_Click_Ms := Now;
      return Click_Count;
   end Count_Click;

   function Window_Poll
     (Handle : System.Address; Kind : access Integer_32;
      Code, Modifiers : access Unsigned_32;
      X, Y : access Integer_32) return Integer_32
   with Export, Convention => C, External_Name => "ccl_window_poll";

   function Window_Poll
     (Handle : System.Address; Kind : access Integer_32;
      Code, Modifiers : access Unsigned_32;
      X, Y : access Integer_32) return Integer_32
   is
      pragma Unreferenced (Handle);
      Event : CuBit.UI.App.Input_Event;
      Found : Boolean;
      Key : Unsigned_64;
      Mods : Unsigned_64;
      Clicks : Natural;
      Wheel : Unsigned_64;
   begin
      Kind.all := 0;
      Code.all := 0;
      Modifiers.all := 0;
      X.all := 0;
      Y.all := 0;
      if not Native_Open then
         return 0;
      end if;

      CuBit.UI.App.Poll_Input (Native_Window, Event, Found);
      if not Found then
         return 0;
      end if;

      case Event.kind is
         when CuBit.UI.App.INPUT_TEXT =>
            Kind.all := 2;
            Code.all := Unsigned_32
              (Event.payload0 and 16#FFFF_FFFF#);
         when CuBit.UI.App.INPUT_POINTER_DOWN =>
            Decode_Pointer (Event.payload0, X, Y);
            Modifiers.all := Unsigned_32 (Current_Modifiers and 7);
            Clicks := Count_Click (X.all, Y.all);
            Kind.all := (if Clicks = 3 then 15
                         elsif Clicks = 2 then 14 else 11);
         when CuBit.UI.App.INPUT_POINTER_MOVE =>
            Decode_Pointer (Event.payload0, X, Y);
            Kind.all := (if (Event.payload1 and 1) /= 0 then 12 else 26);
         when CuBit.UI.App.INPUT_POINTER_UP =>
            Decode_Pointer (Event.payload0, X, Y);
            Kind.all := 13;
         when CuBit.UI.App.INPUT_POINTER_WHEEL =>
            Decode_Pointer (Event.payload0, X, Y);
            Wheel := Event.payload1 and 16#FFFF_FFFF#;
            Modifiers.all := Unsigned_32 (Current_Modifiers and 7);
            if (Current_Modifiers and CuBit.UI.App.KEYMOD_SHIFT) /= 0 then
               Kind.all := (if (Wheel and 16#8000_0000#) = 0 then 27 else 28);
            else
               Kind.all := (if (Wheel and 16#8000_0000#) = 0 then 18 else 19);
            end if;
         when CuBit.UI.App.INPUT_CONFIGURE =>
            --  Returning an otherwise ignored event invalidates the shared
            --  layout; Prepare_Frame observes the newly attached canvas.
            Kind.all := 0;
         when CuBit.UI.App.INPUT_KEY_UP =>
            Current_Modifiers := Event.payload1;
            return 0;
         when CuBit.UI.App.INPUT_KEY_DOWN =>
            Key := Event.payload0;
            Mods := Event.payload1;
            Current_Modifiers := Mods;
            Modifiers.all := Unsigned_32 (Mods and 7);
            if Key = KEY_Z and then (Mods and 2) /= 0 then
               Kind.all := (if (Mods and 1) /= 0 then 24 else 23);
            elsif Key = KEY_Y and then (Mods and 2) /= 0 then
               Kind.all := 24;
            elsif Key = KEY_D and then (Mods and 2) /= 0 then
               Kind.all := 31;
            elsif Key = KEY_F and then (Mods and 2) /= 0 then
               Kind.all := 32;
            elsif Key = KEY_F3 then
               Kind.all := 33;
            elsif Key = KEY_F5 or else
              (Key = KEY_ENTER and then (Mods and 2) /= 0)
            then
               Kind.all := 25;
            elsif Key = KEY_RIGHT_BRACKET and then (Mods and 2) /= 0 then
               Kind.all := (if (Mods and 1) /= 0 then 30 else 29);
            elsif Key = KEY_BACKSLASH and then (Mods and 3) = 3 then
               Kind.all := 29;
            elsif Key = CuBit.UI.App.KEY_ESC then Kind.all := 22;
            elsif Key = KEY_BACKSPACE then Kind.all := 3;
            elsif Key = KEY_ENTER then Kind.all := 4;
            elsif Key = KEY_LEFT then Kind.all := 5;
            elsif Key = KEY_RIGHT then Kind.all := 6;
            elsif Key = KEY_HOME then Kind.all := 7;
            elsif Key = KEY_END then Kind.all := 8;
            elsif Key = KEY_DELETE then Kind.all := 9;
            elsif Key = KEY_UP then Kind.all := 16;
            elsif Key = KEY_DOWN then Kind.all := 17;
            elsif Key = KEY_PAGE_UP then Kind.all := 20;
            elsif Key = KEY_PAGE_DOWN then Kind.all := 21;
            elsif Key = KEY_A and then (Mods and 2) /= 0 then
               Kind.all := 10;
            else
               return 0;
            end if;
         when others =>
            return 0;
      end case;
      return 1;
   end Window_Poll;

   function Window_Present
     (Handle, Pixels : System.Address;
      Pitch : Integer_32) return Integer_32
   with Export, Convention => C, External_Name => "ccl_window_present";

   function Window_Present
     (Handle, Pixels : System.Address;
      Pitch : Integer_32) return Integer_32
   is
      pragma Unreferenced (Handle);
      Target : constant CuBit.UI.Canvas := CuBit.UI.App.Canvas (Native_Window);
      Ignore : System.Address;
      Bytes_Per_Row : Storage_Count;
   begin
      if not Native_Open or else Pixels = System.Null_Address or else
        Pitch <= 0 or else Target.addr = System.Null_Address
      then
         return 1;
      end if;
      Bytes_Per_Row := Storage_Count (Target.width * 4);
      for Row in 0 .. Target.height - 1 loop
         Ignore := CuBit.String.memcpy
           (Target.addr + Storage_Offset (Row * Target.pitch),
            Pixels + Storage_Offset (Row * Natural (Pitch)),
            Bytes_Per_Row);
      end loop;
      CuBit.UI.App.Present (Native_Window, CuBit.UI.App.Full_Rect (Native_Window));
      if not First_Frame_Presented then
         debugPrint ("ccl-workbench: first frame presented" & ASCII.LF);
         First_Frame_Presented := True;
      end if;
      return 0;
   end Window_Present;

   procedure Window_Wait
   with Export, Convention => C, External_Name => "ccl_window_wait";

   procedure Window_Wait is
      Ignore : Unsigned_64;
   begin
      Ignore := syscall (SYSCALL_SLEEP, 10);
   end Window_Wait;

   function Window_Ticks return Unsigned_64
   with Export, Convention => C, External_Name => "ccl_window_ticks";

   function Window_Ticks return Unsigned_64 is
     (syscall (SYSCALL_GETTIME));

   function Window_Clock_Monotonic
     (Success : access Integer_32) return Unsigned_64
   with Export, Convention => C,
        External_Name => "ccl_window_clock_monotonic";

   function Window_Clock_Monotonic
     (Success : access Integer_32) return Unsigned_64
   is
      Request : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Tag : CuBit.Messages.MessageTag;
   begin
      Success.all := 0;
      Request.tag :=
        (label => CuBit.Protocols.CLOCK_OP_MONOTONIC_MS,
         length => 1, flags => 0, badge => 0);
      Request.words (0) := 0;
      Tag := capCall (CAP_SLOT_CLOCK, Request);
      if Tag.label = REPLY_OK and then Tag.length = 1 and then
        Request.words (0) <= Unsigned_64 (Integer_64'Last)
      then
         Success.all := 1;
         return Request.words (0);
      end if;
      return 0;
   end Window_Clock_Monotonic;

   procedure Window_Close (Handle : System.Address)
   with Export, Convention => C, External_Name => "ccl_window_close";

   procedure Window_Close (Handle : System.Address) is
      pragma Unreferenced (Handle);
   begin
      if Native_Open then
         CuBit.UI.App.Close (Native_Window);
         Native_Open := False;
      end if;
   end Window_Close;
end CCL_Workbench_Platform;
