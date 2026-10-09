pragma Ada_2022;
with CuBit.Desktop_Messages;
with CuBit.Desktop_Protocol;
with CuBit.Doom_Keys;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;

package body CuBit.Doom_Platform is

   package D renames CuBit.Desktop_Protocol;
   package K renames CuBit.Doom_Keys;
   use type D.Status_Code;
   use type D.Input_Event_Kind;
   use type D.Surface_Name;
   use type Interfaces.C.int;
   use type System.Address;

   --  doomgeneric.h's default resolution; DOOM renders XRGB8888 pixels.
   Screen_Width  : constant := 640;
   Screen_Height : constant := 400;
   Pixel_Bytes   : constant := 4;
   Screen_Pitch  : constant := Screen_Width * Pixel_Bytes;
   --  Room for the desktop's window decoration around the picture.
   Window_Width  : constant := Screen_Width + 20;
   Window_Height : constant := Screen_Height + 44;

   Page_Bytes   : constant := 4096;
   Frame_Pages  : constant :=
     (Screen_Pitch * Screen_Height + Page_Bytes - 1) / Page_Bytes;

   --  DOOM ticks at 35 Hz; presenting at about that rate keeps the window
   --  smooth without flooding the compositor.
   Present_Interval_Ms : constant := 28;
   --  Between DOOM tics, idle input polls back off this long (below a
   --  60 Hz frame period).
   Idle_Poll_Interval_Ms : constant := 8;
   Stats_Interval_Ms : constant := 1000;
   No_Completion_Token : constant Unsigned_64 := Unsigned_64'Last;

   type Pixel_Array is array (0 .. Screen_Width * Screen_Height - 1)
     of Unsigned_32 with Convention => C;

   --  The buffer lent to the desktop. Page aligned, zero-filled .bss. Not
   --  volatile: the present call's system call orders the writes (memory
   --  clobber) before the desktop reads them.
   Frame : Pixel_Array
     with Alignment => Page_Bytes, Suppress_Initialization;

   Screen_Buffer : System.Address
     with Import, Convention => C, External_Name => "DG_ScreenBuffer";
   Save_Directory : System.Address
     with Import, Convention => C, External_Name => "savegamedir";

   procedure Doom_Create (Count : Interfaces.C.int; Arguments : System.Address)
     with Import, Convention => C, External_Name => "doomgeneric_Create";
   procedure Doom_Tick
     with Import, Convention => C, External_Name => "doomgeneric_Tick";

   function Accessible (Path : System.Address; Mode : Interfaces.C.int)
     return Interfaces.C.int
     with Import, Convention => C, External_Name => "access";
   type Exit_Handler is access procedure with Convention => C;
   function At_Exit (Handler : Exit_Handler) return Interfaces.C.int
     with Import, Convention => C, External_Name => "atexit";
   procedure Process_Exit (Status : Interfaces.C.int)
     with Import, Convention => C, External_Name => "exit", No_Return;

   Read_Permission : constant Interfaces.C.int := 4;

   Desktop_Active : Boolean := False;
   Surface : D.Surface_Name := 0;
   Input_Serial : Unsigned_64 := 0;
   Next_Idle_Poll : Unsigned_64 := 0;
   Shutdown_Registered : Boolean := False;
   Shutdown_Done : Boolean := False;
   Keys : K.Queue := K.Empty_Queue;

   Last_Present : Unsigned_64 := 0;
   Presents, Skipped, Present_Total, Stats_Since : Unsigned_64 := 0;

   procedure Log (Text : String);
   procedure Log_Number (Value : Unsigned_64);
   function Send (Request : D.Wire_Message) return D.Wire_Message;
   function Open_Window return Boolean;
   procedure Poll_Input;
   procedure Report_Stats (Now : Unsigned_64);

   --  Status lines go to the debug console, where the headless runner
   --  reads them; DOOM's own printf output is its stdout stream.
   procedure Log (Text : String) is
   begin
      debugPrint (Text);
   end Log;

   procedure Log_Number (Value : Unsigned_64) is
      Digits_Max : constant := 20;
      Text : String (1 .. Digits_Max);
      First : Positive := Text'Last + 1;
      Rest : Unsigned_64 := Value;
   begin
      loop
         First := First - 1;
         Text (First) := Character'Val (Character'Pos ('0') + Rest mod 10);
         Rest := Rest / 10;
         exit when Rest = 0;
      end loop;
      Log (Text (First .. Text'Last));
   end Log_Number;

   function Now_Ms return Unsigned_64 is (syscall (SYSCALL_GETTIME));

   function Send (Request : D.Wire_Message) return D.Wire_Message is
      Msg : Message := CuBit.Desktop_Messages.From_Wire (Request);
      Returned : MessageTag;
   begin
      Returned := capCall (CAP_SLOT_DESKTOP, Msg, Wait_Forever);
      if Returned /= Msg.tag then
         return (others => <>);
      end if;
      return CuBit.Desktop_Messages.To_Wire (Msg);
   end Send;

   procedure Shutdown_Surface with Convention => C;
   procedure Shutdown_Surface is
      Ignored : D.Wire_Message;
   begin
      if Shutdown_Done then
         return;
      end if;
      Shutdown_Done := True;
      if Surface /= 0 then
         Ignored := Send (D.Encode_Destroy ((Surface => Surface)));
      end if;
      if Desktop_Active or else Surface /= 0 then
         Ignored := Send (D.Encode_Empty_Request (D.Goodbye));
      end if;
      Desktop_Active := False;
      Surface := 0;
   end Shutdown_Surface;

   function Open_Window return Boolean is
      Created : D.Creation_Result;
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Lent : Boolean;
      Ignored : D.Limits_Result;
      Size : constant D.Window_Bounds :=
        (Minimum_Width | Maximum_Width => Window_Width,
         Minimum_Height | Maximum_Height => Window_Height);
   begin
      if D.Decode_Hello_Result (Send (D.Encode_Hello (D.Current_Revision)))
           .Status /= D.Success
      then
         return False;
      end if;
      Created := D.Decode_Creation_Result
        (Send (D.Encode_Create ((Window_Width, Window_Height,
                                 D.Window_Surface))));
      if Created.Status /= D.Success then
         return False;
      end if;
      Surface := Created.Surface;
      Ignored := D.Decode_Limits_Result (Send (D.Encode_Limits
        ((Surface  => Created.Surface,
          Bounds   => Size,
          Features => [D.Decorated | D.Minimizable | D.Closeable |
                       D.Fixed_Size => True, others => False]))));

      CuBit.Memory_Grants.Create_Via_Capability
        (CAP_SLOT_DESKTOP, Frame'Address, Frame_Pages, False, Grant, Lent);
      if not Lent then
         return False;
      end if;
      if D.Decode_Status (Send (D.Encode_Attachment
           ((Surface => Created.Surface, Grant => Grant,
             Layout  => (Screen_Width, Screen_Height, Screen_Pitch)))),
           D.Attach_Buffer) /= D.Success
      then
         return False;
      end if;
      Desktop_Active := True;
      Log ("DOOM: Desktop surface attached." & ASCII.LF);
      return True;
   end Open_Window;

   procedure Poll_Input is
      Now : constant Unsigned_64 := Now_Ms;
      Saw_Event : Boolean := False;
   begin
      if not Desktop_Active or else Now < Next_Idle_Poll then
         return;
      end if;
      loop
         declare
            Result : constant D.Input_Result := D.Decode_Input_Result
              (Send (D.Encode_Input_Request
                 ((D.Poll_Input, Surface, Input_Serial))), D.Poll_Input);
         begin
            exit when Result.Status /= D.Success
              or else Result.Value.Kind = D.No_Input;
            Saw_Event := True;
            Input_Serial := Result.Value.Serial;
            if Result.Value.Kind in D.Key_Pressed | D.Key_Released then
               --  A valid key event's scan code is at most 127.
               K.Push (Keys,
                 (Code    => K.Translate (K.Scancode (Result.Value.Payload0)),
                  Pressed => Result.Value.Kind = D.Key_Pressed));
            end if;
         end;
      end loop;
      --  Keep active input crisp without polling an idle desktop dozens
      --  of times per tic.
      Next_Idle_Poll := (if Saw_Event then 0 else Now + Idle_Poll_Interval_Ms);
   end Poll_Input;

   procedure Report_Stats (Now : Unsigned_64) is
   begin
      if Stats_Since = 0 then
         Stats_Since := Now;
      elsif Now >= Stats_Since and then Now - Stats_Since >= Stats_Interval_Ms
      then
         Log ("DOOM: desktop presents=");
         Log_Number (Presents);
         Log (" skipped=");
         Log_Number (Skipped);
         Log (" present_ms=");
         Log_Number (Present_Total);
         Log ([1 => ASCII.LF]);
         Presents := 0;
         Skipped := 0;
         Present_Total := 0;
         Stats_Since := Now;
      end if;
   end Report_Stats;

   procedure Init is
   begin
      if not Shutdown_Registered then
         Shutdown_Registered := At_Exit (Shutdown_Surface'Access) = 0;
      end if;
      if Open_Window then
         Log ("DOOM: Running inside desktop surface." & ASCII.LF);
      else
         Log ("DOOM: desktop.svc unavailable; windowed DOOM requires the "
              & "desktop." & ASCII.LF);
         Process_Exit (1);
      end if;
   end Init;

   procedure Draw_Frame is
      Now : Unsigned_64;
      Done : Unsigned_64;
      Ignored : Boolean;
   begin
      if Desktop_Active then
         Now := Now_Ms;
         if Last_Present = 0 or else Now < Last_Present
           or else Now - Last_Present >= Present_Interval_Ms
         then
            declare
               Screen : constant Pixel_Array
                 with Import, Address => Screen_Buffer;
            begin
               Frame := Screen;
            end;
            Ignored := capSubmit (CAP_SLOT_DESKTOP,
              CuBit.Desktop_Messages.From_Wire
                (D.Encode_Present
                   ((Surface => Surface, Area => (X | Y => 0, Width | Height => 0)))),
              No_Completion_Token);
            Done := Now_Ms;
            Last_Present := Now;
            Presents := Presents + 1;
            if Done >= Now then
               Present_Total := Present_Total + (Done - Now);
            end if;
         else
            Skipped := Skipped + 1;
         end if;
         Report_Stats (Now);
      end if;
      Poll_Input;
   end Draw_Frame;

   procedure Sleep_Ms (Milliseconds : Unsigned_32) is
      Ignored : Unsigned_64;
   begin
      Ignored := syscall (SYSCALL_SLEEP, Unsigned_64 (Milliseconds));
   end Sleep_Ms;

   --  doomgeneric's tick counter is 32 bits and wraps.
   function Ticks_Ms return Unsigned_32 is
     (Unsigned_32 (Now_Ms mod 2 ** 32));

   function Get_Key (Pressed : access Interfaces.C.int;
                     Code : access Interfaces.C.unsigned_char)
                     return Interfaces.C.int
   is
      Item : K.Event;
      Found : Boolean;
   begin
      Poll_Input;
      K.Pop (Keys, Item, Found);
      if not Found then
         return 0;
      end if;
      Pressed.all := (if Item.Pressed then 1 else 0);
      Code.all := Interfaces.C.unsigned_char (Item.Code);
      return 1;
   end Get_Key;

   --  The desktop's title request is not used yet.
   procedure Set_Window_Title (Title : System.Address) is null;

   --  DOOM's command line and the save directory it is pointed at. C
   --  strings; DOOM keeps the pointers for the whole run.
   Program_Name : aliased constant String := "doom" & ASCII.NUL;
   Wad_Option   : aliased constant String := "-iwad" & ASCII.NUL;
   Nvme_Wad     : aliased constant String := "@nvme:0/doom1.wad" & ASCII.NUL;
   Ata_Wad      : aliased constant String := "@ata:0/doom1.wad" & ASCII.NUL;
   --  Live images: the optical disc (its apps/ tree), or the boot archive.
   Optical_Wad  : aliased constant String := "@cd:0/doom1.wad" & ASCII.NUL;
   Boot_Wad     : aliased constant String := "@boot/doom1.wad" & ASCII.NUL;
   --  Saves go to the WAD's volume root.
   Nvme_Saves   : aliased String := "@nvme:0/" & ASCII.NUL;
   Ata_Saves    : aliased String := "@ata:0/" & ASCII.NUL;

   Argument_Count : constant := 3;
   type Argument_Vector is array (0 .. Argument_Count) of System.Address
     with Convention => C;
   Arguments : Argument_Vector := [others => System.Null_Address];

   function Main return Interfaces.C.int is
   begin
      Log ("DOOM: Starting on CuBit OS..." & ASCII.LF);
      Arguments (0) := Program_Name'Address;
      Arguments (1) := Wad_Option'Address;
      if Accessible (Nvme_Wad'Address, Read_Permission) = 0 then
         Arguments (2) := Nvme_Wad'Address;
         Log ("DOOM: Using WAD from NVMe disk." & ASCII.LF);
      elsif Accessible (Ata_Wad'Address, Read_Permission) = 0 then
         Arguments (2) := Ata_Wad'Address;
         Log ("DOOM: Using WAD from ATA disk." & ASCII.LF);
      elsif Accessible (Optical_Wad'Address, Read_Permission) = 0 then
         Arguments (2) := Optical_Wad'Address;
         Log ("DOOM: Using WAD from the optical disc." & ASCII.LF);
      elsif Accessible (Boot_Wad'Address, Read_Permission) = 0 then
         Arguments (2) := Boot_Wad'Address;
         Log ("DOOM: Using WAD from the boot archive." & ASCII.LF);
      else
         Log ("DOOM: no doom1.wad on a disk, the optical disc or the boot "
              & "archive." & ASCII.LF);
         return 1;
      end if;
      Doom_Create (Argument_Count, Arguments'Address);
      --  DOOM picks a save directory under its configuration directory;
      --  saves go next to the WAD instead (read-only media: nowhere).
      if Arguments (2) = Nvme_Wad'Address then
         Save_Directory := Nvme_Saves'Address;
      elsif Arguments (2) = Ata_Wad'Address then
         Save_Directory := Ata_Saves'Address;
      end if;
      loop
         Doom_Tick;
      end loop;
   end Main;

end CuBit.Doom_Platform;
