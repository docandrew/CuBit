pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;

with CuBit.Audio;
with CuBit.Desktop_Messages;
with CuBit.Desktop_Protocol;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.SameBoy_Batches;
with CuBit.SameBoy_Frames; use CuBit.SameBoy_Frames;
with CuBit.SameBoy_Keys;

package body CuBit.SameBoy_Frontend is

   package C renames Interfaces.C;
   package D renames CuBit.Desktop_Protocol;
   package K renames CuBit.SameBoy_Keys;
   package Batches renames CuBit.SameBoy_Batches;
   use type C.int;
   use type C.long;
   use type System.Address;
   use type D.Status_Code;
   use type D.Input_Event_Kind;
   use type D.Surface_Name;
   use type CuBit.Audio.Volume;
   use type K.Command;

   --  C's bool (one byte).
   type C_Bool is new Boolean with Convention => C;

   --  Room for the desktop's window decoration around the picture.
   Window_Width  : constant := View_Width + 20;
   Window_Height : constant := View_Height + 44;
   Page_Bytes    : constant := 4096;
   Pixel_Bytes   : constant := 4;
   View_Pages    : constant :=
     (View_Width * View_Height * Pixel_Bytes + Page_Bytes - 1) / Page_Bytes;

   --  Cartridges: <directory>/00.gb .. 15.gb; ROM 00 must exist. The disk
   --  image's sameboy/, or the live image's optical disc.
   Rom_Slots        : constant := 16;
   Disk_Roms        : aliased constant String := "@nvme:0/sameboy/";
   Optical_Roms     : aliased constant String := "@cd:0/sameboy/";
   type Rom_Directory is (On_Disk, On_Optical_Disc);
   Minimum_Rom_Bytes : constant := 16#150#;   --  through the cartridge header
   Maximum_Rom_Bytes : constant := 8 * 1024 * 1024;
   Cgb_Flag_Offset  : constant := 16#143#;
   Cgb_Flag         : constant := 16#80#;
   --  SameBoy's GB_model_t.
   Model_Dmg_B      : constant C.unsigned := 16#002#;
   Model_Cgb_E      : constant C.unsigned := 16#205#;

   Audio_Rate       : constant := 48_000;
   Stereo           : constant := 2;
   --  Start the stream once 32 ms are queued; give up on a stream that
   --  accepts nothing for 250 ms.
   Audio_Prefill    : constant := 1536;
   Audio_Stall_Ms   : constant := 250;
   Input_Batch      : constant := 32;
   Frames_Reported  : constant := 120;
   Nanoseconds_Per_Ms : constant := 1_000_000;
   Paused_Poll_Ns   : constant := 10 * Nanoseconds_Per_Ms;
   Late_Reset_Ns    : constant := 100 * Nanoseconds_Per_Ms;
   Opaque           : constant := 16#FF00_0000#;

   Standard_Output  : constant C.int := 1;
   Read_Only        : constant C.int := 0;
   Seek_Set         : constant C.int := 0;
   Seek_End         : constant C.int := 2;

   ---------------------------------------------------------------------------
   --  The SameBoy core (Core/gb.h and friends).
   ---------------------------------------------------------------------------
   subtype Game_Boy is System.Address;

   type Log_Callback is access procedure
     (GB : Game_Boy; Text : System.Address; Attributes : C.int)
     with Convention => C;
   type Rgb_Callback is access function
     (GB : Game_Boy; R, G, B : C.unsigned_char) return Unsigned_32
     with Convention => C;
   type Boot_Rom_Callback is access procedure (GB : Game_Boy; Kind : C.int)
     with Convention => C;
   type Vblank_Callback is access procedure (GB : Game_Boy; Kind : C.int)
     with Convention => C;
   type Sample_Callback is access procedure
     (GB : Game_Boy; Sample : access constant Batches.Stereo_Frame)
     with Convention => C;

   function GB_Alloc return Game_Boy
     with Import, Convention => C, External_Name => "GB_alloc";
   function GB_Init (GB : Game_Boy; Model : C.unsigned) return Game_Boy
     with Import, Convention => C, External_Name => "GB_init";
   procedure GB_Free (GB : Game_Boy)
     with Import, Convention => C, External_Name => "GB_free";
   procedure GB_Dealloc (GB : Game_Boy)
     with Import, Convention => C, External_Name => "GB_dealloc";
   function GB_Run (GB : Game_Boy) return C.unsigned
     with Import, Convention => C, External_Name => "GB_run";
   function GB_Get_Clock_Rate (GB : Game_Boy) return Unsigned_32
     with Import, Convention => C, External_Name => "GB_get_clock_rate";
   procedure GB_Set_Key_State (GB : Game_Boy; Key : C.unsigned; Pressed : C_Bool)
     with Import, Convention => C, External_Name => "GB_set_key_state";
   procedure GB_Set_Key_Mask (GB : Game_Boy; Mask : C.unsigned)
     with Import, Convention => C, External_Name => "GB_set_key_mask";
   procedure GB_Reset (GB : Game_Boy)
     with Import, Convention => C, External_Name => "GB_reset";
   procedure GB_Load_Rom (GB : Game_Boy; Data : System.Address; Size : C.size_t)
     with Import, Convention => C, External_Name => "GB_load_rom_from_buffer";
   procedure GB_Load_Boot_Rom (GB : Game_Boy; Data : System.Address; Size : C.size_t)
     with Import, Convention => C, External_Name => "GB_load_boot_rom_from_buffer";
   function GB_Is_Cgb (GB : Game_Boy) return C_Bool
     with Import, Convention => C, External_Name => "GB_is_cgb";
   procedure GB_Set_Turbo_Mode (GB : Game_Boy; On, No_Frame_Skip : C_Bool)
     with Import, Convention => C, External_Name => "GB_set_turbo_mode";
   procedure GB_Set_Sample_Rate (GB : Game_Boy; Rate : C.unsigned)
     with Import, Convention => C, External_Name => "GB_set_sample_rate";
   procedure GB_Set_Sample_Callback (GB : Game_Boy; Callback : Sample_Callback)
     with Import, Convention => C, External_Name => "GB_apu_set_sample_callback";
   procedure GB_Set_Pixels_Output (GB : Game_Boy; Output : System.Address)
     with Import, Convention => C, External_Name => "GB_set_pixels_output";
   procedure GB_Set_Log_Callback (GB : Game_Boy; Callback : Log_Callback)
     with Import, Convention => C, External_Name => "GB_set_log_callback";
   procedure GB_Set_Rgb_Callback (GB : Game_Boy; Callback : Rgb_Callback)
     with Import, Convention => C, External_Name => "GB_set_rgb_encode_callback";
   procedure GB_Set_Boot_Rom_Callback (GB : Game_Boy; Callback : Boot_Rom_Callback)
     with Import, Convention => C, External_Name => "GB_set_boot_rom_load_callback";
   procedure GB_Set_Vblank_Callback (GB : Game_Boy; Callback : Vblank_Callback)
     with Import, Convention => C, External_Name => "GB_set_vblank_callback";

   --  SameBoy's open-source boot ROMs, embedded by bootroms.S.
   Dmg_Boot : constant Unsigned_8
     with Import, Convention => C, External_Name => "sb_dmg_boot";
   Dmg_Boot_End : constant Unsigned_8
     with Import, Convention => C, External_Name => "sb_dmg_boot_end";
   Cgb_Boot : constant Unsigned_8
     with Import, Convention => C, External_Name => "sb_cgb_boot";
   Cgb_Boot_End : constant Unsigned_8
     with Import, Convention => C, External_Name => "sb_cgb_boot_end";

   ---------------------------------------------------------------------------
   --  The libc.
   ---------------------------------------------------------------------------
   function Write (Descriptor : C.int; Data : System.Address; Count : C.size_t)
     return C.long
     with Import, Convention => C, External_Name => "write";
   function Length_Of (Text : System.Address) return C.size_t
     with Import, Convention => C, External_Name => "strlen";
   function Open (Path : System.Address; Flags : C.int) return C.int
     with Import, Convention => C, External_Name => "open";
   function Seek (Descriptor : C.int; Offset : C.long; Whence : C.int)
     return C.long
     with Import, Convention => C, External_Name => "lseek";
   function Read (Descriptor : C.int; Data : System.Address; Count : C.size_t)
     return C.long
     with Import, Convention => C, External_Name => "read";
   function Close (Descriptor : C.int) return C.int
     with Import, Convention => C, External_Name => "close";
   function Allocate (Size : C.size_t) return System.Address
     with Import, Convention => C, External_Name => "malloc";
   procedure Free (Data : System.Address)
     with Import, Convention => C, External_Name => "free";

   ---------------------------------------------------------------------------
   --  State.
   ---------------------------------------------------------------------------
   Gameboy : Game_Boy := System.Null_Address;
   Screen  : Screen_Pixels with Suppress_Initialization;
   --  The buffer lent to the desktop. Page aligned, zero-filled .bss. Not
   --  volatile: the present call's system call orders the writes (memory
   --  clobber) before the desktop reads them.
   View    : View_Pixels
     with Alignment => Page_Bytes, Suppress_Initialization;
   Surface : D.Surface_Name := 0;
   Input_Serial : Unsigned_64 := 0;
   Held : K.Held_Keys with Suppress_Initialization;   --  all False
   Frames : Natural := 0;
   Selected_Rom : Natural := 0;
   Roms : Rom_Directory := On_Disk;
   Paused, Change_Rom, Frame_Ready : Boolean := False;
   Running : Boolean := True;

   Samples : Batches.Batch with Suppress_Initialization;   --  empty
   Stream : CuBit.Audio.StreamHandle := CuBit.Audio.NULL_STREAM;
   Audio_Enabled, Audio_Started, Muted : Boolean := False;
   Audio_Primed : Natural := 0;
   Audio_Progress : Unsigned_64 := 0;
   Volume : K.Volume_Percent := K.Initial_Volume;

   --  Text built without the secondary stack: a fixed buffer.
   type Line is record
      Text : String (1 .. 80);
      Last : Natural := 0;
   end record;
   procedure Add (L : in out Line; Text : String);
   procedure Add_Number (L : in out Line; Value : Natural; Width : Positive := 1);

   procedure Say (Text : String);
   procedure Say_Line (Text : String);
   function Now_Ms return Unsigned_64;
   procedure Sleep_Ms (Milliseconds : Unsigned_64);
   function Send (Request : D.Wire_Message) return D.Wire_Message;
   procedure Set_Title (Text : String);
   procedure Apply_Volume;
   procedure Audio_Reset;
   procedure Audio_Flush;
   procedure Volume_Changed;
   procedure Shutdown;
   function Open_Window return Boolean;
   function Load_Rom (Index : Natural) return Boolean;
   procedure Poll_Input;
   procedure Present;
   function Run_Frame return Unsigned_64;
   function Run return C.int;

   --  Status lines go to the debug console, where test runners read them.
   procedure Say (Text : String) is
   begin
      debugPrint (Text);
   end Say;

   procedure Say_Line (Text : String) is
   begin
      Say (Text);
      Say ([1 => ASCII.LF]);
   end Say_Line;

   procedure Add (L : in out Line; Text : String) is
      Room : constant Natural := Natural'Min (Text'Length, L.Text'Last - L.Last);
   begin
      L.Text (L.Last + 1 .. L.Last + Room) :=
        Text (Text'First .. Text'First + Room - 1);
      L.Last := L.Last + Room;
   end Add;

   --  Decimal, zero-padded to at least Width digits.
   procedure Add_Number (L : in out Line; Value : Natural; Width : Positive := 1)
   is
      Digits_Max : constant := 10;
      Buffer : String (1 .. Digits_Max);
      First : Positive := Buffer'Last + 1;
      Rest : Natural := Value;
   begin
      loop
         First := First - 1;
         Buffer (First) := Character'Val (Character'Pos ('0') + Rest mod 10);
         Rest := Rest / 10;
         exit when Rest = 0 and then Buffer'Last - First + 1 >= Width;
      end loop;
      Add (L, Buffer (First .. Buffer'Last));
   end Add_Number;

   function Now_Ms return Unsigned_64 is (syscall (SYSCALL_GETTIME));

   procedure Sleep_Ms (Milliseconds : Unsigned_64) is
      Ignored : Unsigned_64;
   begin
      Ignored := syscall (SYSCALL_SLEEP, Milliseconds);
   end Sleep_Ms;

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

   procedure Set_Title (Text : String) is
      Ignored : D.Wire_Message;
   begin
      if Surface /= 0 then
         Ignored := Send (D.Encode_Title
           ((Surface => Surface, Title => D.Make_Title (Text))));
      end if;
   end Set_Title;

   ---------------------------------------------------------------------------
   --  Audio.
   ---------------------------------------------------------------------------
   procedure Apply_Volume is
   begin
      if Audio_Enabled then
         CuBit.Audio.setVolume
           (Stream, CuBit.Audio.Volume'(1.0)
              * (if Muted then 0 else Volume) / K.Maximum_Volume);
      end if;
   end Apply_Volume;

   --  Discard queued audio by closing the stream; open a new one unless
   --  paused.
   procedure Audio_Reset is
   begin
      if CuBit.Audio.isValid (Stream) then
         CuBit.Audio.close (Stream);
      end if;
      Batches.Clear (Samples);
      Audio_Primed := 0;
      Audio_Started := False;
      Audio_Enabled := False;
      if not Paused then
         Stream := CuBit.Audio.open (Audio_Rate, Stereo);
         Audio_Enabled := CuBit.Audio.isValid (Stream);
      end if;
      Apply_Volume;
      Audio_Progress := Now_Ms;
   end Audio_Reset;

   procedure Sample (GB : Game_Boy; Frame : access constant Batches.Stereo_Frame)
     with Convention => C;
   procedure Sample (GB : Game_Boy; Frame : access constant Batches.Stereo_Frame)
   is
      pragma Unreferenced (GB);
   begin
      if Audio_Enabled then
         Batches.Append (Samples, Frame.all);
      end if;
   end Sample;

   --  Partial writes keep the rest; back pressure is applied by the main
   --  loop, never inside the core's per-sample callback.
   procedure Audio_Flush is
      Written : Natural;
   begin
      if not Audio_Enabled then
         return;
      end if;
      Written := Natural'Min
        (CuBit.Audio.write (Stream, Samples.Frames (Samples.First)'Address,
                            Batches.Waiting (Samples)),
         Batches.Waiting (Samples));
      Batches.Accept_Written (Samples, Written);
      if Written > 0 then
         Audio_Progress := Now_Ms;
      end if;
      if not Audio_Started then
         Audio_Primed := Natural'Min (Audio_Primed + Written, Audio_Prefill);
         if Audio_Primed >= Audio_Prefill then
            CuBit.Audio.start (Stream);
            Audio_Started := True;
            Say_Line ("sameboy: native audio started (48000 Hz stereo)");
         end if;
      end if;
      if Samples.Overflow or else (Batches.Waiting (Samples) > 0 and then
           Now_Ms - Audio_Progress > Audio_Stall_Ms)
      then
         Say_Line ("sameboy: audio stalled/overflowed; continuing silently");
         CuBit.Audio.close (Stream);
         Audio_Enabled := False;
         Batches.Clear (Samples);
      end if;
   end Audio_Flush;

   procedure Volume_Changed is
      Title, Report : Line;
   begin
      Apply_Volume;
      Add (Title, "SameBoy - ");
      if Muted then
         Add (Title, "mute ");
      end if;
      Add_Number (Title, Volume);
      Add (Title, "%");
      Set_Title (Title.Text (1 .. Title.Last));
      Add (Report, "sameboy: volume=");
      Add_Number (Report, Volume);
      Add (Report, (if Muted then " mute=1" else " mute=0"));
      Say_Line (Report.Text (1 .. Report.Last));
   end Volume_Changed;

   ---------------------------------------------------------------------------
   --  Window.
   ---------------------------------------------------------------------------
   procedure Shutdown is
      Ignored : D.Wire_Message;
   begin
      if CuBit.Audio.isValid (Stream) then
         CuBit.Audio.close (Stream);
      end if;
      if Surface /= 0 then
         Ignored := Send (D.Encode_Destroy ((Surface => Surface)));
         Surface := 0;
      end if;
      Ignored := Send (D.Encode_Empty_Request (D.Goodbye));
   end Shutdown;

   function Open_Window return Boolean is
      Created : D.Creation_Result;
      Limits : D.Limits_Result;
      Grant : CuBit.Memory_Grants.Grant_Reference;
      Lent : Boolean;
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
      Limits := D.Decode_Limits_Result (Send (D.Encode_Limits
        ((Surface  => Created.Surface,
          Bounds   => Size,
          Features => [D.Decorated | D.Minimizable | D.Closeable |
                       D.Fixed_Size => True, others => False]))));
      if Limits.Status /= D.Success then
         return False;
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (CAP_SLOT_DESKTOP, View'Address, View_Pages, False, Grant, Lent);
      if not Lent or else D.Decode_Status (Send (D.Encode_Attachment
           ((Surface => Created.Surface, Grant => Grant,
             Layout  => (View_Width, View_Height,
                         View_Width * Pixel_Bytes)))),
           D.Attach_Buffer) /= D.Success
      then
         return False;
      end if;
      Set_Title ("SameBoy");
      return True;
   end Open_Window;

   ---------------------------------------------------------------------------
   --  Core callbacks.
   ---------------------------------------------------------------------------
   function Rgb (GB : Game_Boy; R, G, B : C.unsigned_char) return Unsigned_32
     with Convention => C;
   function Rgb (GB : Game_Boy; R, G, B : C.unsigned_char) return Unsigned_32
   is
      pragma Unreferenced (GB);
   begin
      return Opaque or Shift_Left (Unsigned_32 (R), 16)
        or Shift_Left (Unsigned_32 (G), 8) or Unsigned_32 (B);
   end Rgb;

   procedure Log (GB : Game_Boy; Text : System.Address; Attributes : C.int)
     with Convention => C;
   procedure Log (GB : Game_Boy; Text : System.Address; Attributes : C.int) is
      pragma Unreferenced (GB, Attributes);
      Ignored : C.long;
   begin
      Ignored := Write (Standard_Output, Text, Length_Of (Text));
   end Log;

   procedure Boot_Rom (GB : Game_Boy; Kind : C.int) with Convention => C;
   procedure Boot_Rom (GB : Game_Boy; Kind : C.int) is
      pragma Unreferenced (Kind);
      use System.Storage_Elements;
   begin
      if GB_Is_Cgb (GB) then
         GB_Load_Boot_Rom (GB, Cgb_Boot'Address,
           C.size_t (Cgb_Boot_End'Address - Cgb_Boot'Address));
      else
         GB_Load_Boot_Rom (GB, Dmg_Boot'Address,
           C.size_t (Dmg_Boot_End'Address - Dmg_Boot'Address));
      end if;
   end Boot_Rom;

   procedure Vblank (GB : Game_Boy; Kind : C.int) with Convention => C;
   procedure Vblank (GB : Game_Boy; Kind : C.int) is
      pragma Unreferenced (GB, Kind);
   begin
      Frame_Ready := True;
   end Vblank;

   ---------------------------------------------------------------------------
   --  Cartridges.
   ---------------------------------------------------------------------------
   function Load_Rom (Index : Natural) return Boolean is
      Path : Line;
      File : C.int;
      Length : C.long;
      Got : C.long;
      Total : C.long := 0;
      Rom : System.Address;
      Fresh : Game_Boy;
      Ignored : C.int;
      Title, Report : Line;
   begin
      Add (Path, (case Roms is when On_Disk => Disk_Roms,
                               when On_Optical_Disc => Optical_Roms));
      Add_Number (Path, Index, Width => 2);
      Add (Path, ".gb" & ASCII.NUL);
      File := Open (Path.Text'Address, Read_Only);
      if File < 0 then
         return False;
      end if;
      Length := Seek (File, 0, Seek_End);
      if Length < Minimum_Rom_Bytes or else Length > Maximum_Rom_Bytes
        or else Seek (File, 0, Seek_Set) /= 0
      then
         Ignored := Close (File);
         return False;
      end if;
      Rom := Allocate (C.size_t (Length));
      if Rom = System.Null_Address then
         Ignored := Close (File);
         return False;
      end if;
      declare
         use System.Storage_Elements;
      begin
         while Total < Length loop
            Got := Read (File, Rom + Storage_Offset (Total),
                         C.size_t (Length - Total));
            exit when Got <= 0;
            Total := Total + Got;
         end loop;
      end;
      Ignored := Close (File);
      Fresh := (if Total = Length then GB_Alloc else System.Null_Address);
      if Fresh = System.Null_Address then
         Free (Rom);
         return False;
      end if;
      if Gameboy /= System.Null_Address then
         GB_Free (Gameboy);
         GB_Dealloc (Gameboy);
      end if;
      declare
         Header : constant array (0 .. Cgb_Flag_Offset) of Unsigned_8
           with Import, Address => Rom;
      begin
         Gameboy := GB_Init (Fresh, (if (Header (Cgb_Flag_Offset) and Cgb_Flag) /= 0
                                     then Model_Cgb_E else Model_Dmg_B));
      end;
      GB_Set_Log_Callback (Gameboy, Log'Access);
      GB_Set_Rgb_Callback (Gameboy, Rgb'Access);
      GB_Set_Pixels_Output (Gameboy, Screen'Address);
      GB_Set_Boot_Rom_Callback (Gameboy, Boot_Rom'Access);
      GB_Set_Vblank_Callback (Gameboy, Vblank'Access);
      GB_Set_Sample_Callback (Gameboy, Sample'Access);
      GB_Set_Sample_Rate (Gameboy, Audio_Rate);
      GB_Set_Turbo_Mode (Gameboy, On => True, No_Frame_Skip => True);
      GB_Load_Rom (Gameboy, Rom, C.size_t (Length));
      Free (Rom);
      Selected_Rom := Index;
      Frames := 0;
      Paused := False;
      Audio_Reset;
      if not Audio_Enabled then
         Say_Line ("sameboy: mixer unavailable; continuing silently");
      end if;
      Held := K.No_Keys_Held;
      Add (Title, "SameBoy - ROM ");
      Add_Number (Title, Index, Width => 2);
      Set_Title (Title.Text (1 .. Title.Last));
      Add (Report, "sameboy: loaded ROM ");
      Add_Number (Report, Index, Width => 2);
      Add (Report, " (");
      Add_Number (Report, Natural (Length));
      Add (Report, " bytes)");
      Say_Line (Report.Text (1 .. Report.Last));
      return True;
   end Load_Rom;

   ---------------------------------------------------------------------------
   --  Input and output.
   ---------------------------------------------------------------------------
   procedure Poll_Input is
      Press : Boolean;
   begin
      for N in 1 .. Input_Batch loop
         declare
            Result : constant D.Input_Result := D.Decode_Input_Result
              (Send (D.Encode_Input_Request
                 ((D.Poll_Input, Surface, Input_Serial))), D.Poll_Input);
         begin
            if Result.Status /= D.Success then
               Running := False;
               return;
            end if;
            exit when Result.Value.Kind = D.No_Input;
            Input_Serial := Result.Value.Serial;
            case Result.Value.Kind is
               when D.Input_Resynchronized =>
                  --  Events were lost: release every button.
                  GB_Set_Key_Mask (Gameboy, 0);
                  Held := K.No_Keys_Held;
               when D.Close_Requested =>
                  Running := False;
               when D.Key_Pressed | D.Key_Released =>
                  declare
                     Down : constant Boolean := Result.Value.Kind = D.Key_Pressed;
                     --  A valid key event's scan code is at most 127.
                     Code : constant K.Scancode :=
                       K.Scancode (Result.Value.Payload0);
                     Bound : constant K.Binding := K.Bound (Code);
                  begin
                     K.Note (Held, Code, Down, Press);
                     if Bound.Is_Pad then
                        GB_Set_Key_State (Gameboy, K.Pad_Key'Enum_Rep (Bound.Key),
                                          C_Bool (Down));
                     elsif Bound.Action = K.Quit then
                        if Down then
                           Running := False;
                        end if;
                     elsif Press then
                        case Bound.Action is
                           when K.Toggle_Pause =>
                              Paused := not Paused;
                              Audio_Reset;
                              Set_Title (if Paused then "SameBoy - paused"
                                         else "SameBoy");
                           when K.Next_Rom => Change_Rom := True;
                           when K.Reset =>
                              GB_Reset (Gameboy);
                              Audio_Reset;
                           when K.Toggle_Mute =>
                              Muted := not Muted;
                              Volume_Changed;
                           when K.Quieter =>
                              Volume := K.Quieter (Volume);
                              Volume_Changed;
                           when K.Louder =>
                              Volume := K.Louder (Volume);
                              Volume_Changed;
                           when K.No_Command | K.Quit => null;
                        end case;
                     end if;
                  end;
               when others => null;
            end case;
            exit when not Running;
         end;
      end loop;
   end Poll_Input;

   procedure Present is
   begin
      Enlarge (Screen, View);
      if D.Decode_Status (Send (D.Encode_Present
           ((Surface => Surface, Area => (X | Y => 0, Width | Height => 0)))),
           D.Present_Surface) /= D.Success
      then
         Running := False;
      end if;
   end Present;

   --  Count exactly the cycles emulated: with host timekeeping disabled
   --  the core's own frame sync counter resets at VBlank.
   function Run_Frame return Unsigned_64 is
      Cycles : Unsigned_64 := 0;
   begin
      Frame_Ready := False;
      while not Frame_Ready loop
         Cycles := Cycles + Unsigned_64 (GB_Run (Gameboy));
      end loop;
      return Nanoseconds (Cycles, GB_Get_Clock_Rate (Gameboy));
   end Run_Frame;

   function Run return C.int is
      Target, Now, Elapsed : Unsigned_64;
   begin
      if not Open_Window then
         Say_Line ("sameboy: cannot create native window");
         return 1;
      end if;
      Say_Line ("sameboy: native window ready");
      if not Load_Rom (0) then
         Roms := On_Optical_Disc;
         if not Load_Rom (0) then
            Say_Line ("sameboy: cannot read sameboy/00.gb");
            return 1;
         end if;
      end if;
      Say_Line ("sameboy: arrows, Z/B, X/A, Enter/Start, Tab/Select; "
                & "P pause, F2 next ROM, F5 reset, Esc close");
      Say_Line ("sameboy: F8 mute, F9 quieter, F10 louder (this app only)");
      Target := Now_Ms * Nanoseconds_Per_Ms;
      while Running loop
         Poll_Input;
         exit when not Running;
         if Change_Rom then
            Change_Rom := False;
            declare
               Next : constant Natural := (Selected_Rom + 1) mod Rom_Slots;
            begin
               if not Load_Rom (Next) and then Next /= 0
                 and then not Load_Rom (0)
               then
                  null;
               end if;
            end;
            Target := Now_Ms * Nanoseconds_Per_Ms;
         end if;
         if not Paused then
            Audio_Flush;
            if Batches.Waiting (Samples) > 0 then
               Sleep_Ms (1);
               goto Continue;
            end if;
            Elapsed := Run_Frame;
            Audio_Flush;
            Present;
            Target := Target + Elapsed;
            Frames := Frames + 1;
            if Frames = Frames_Reported then
               Say_Line ("sameboy: 120 emulated frames");
            end if;
         else
            Target := Now_Ms * Nanoseconds_Per_Ms + Paused_Poll_Ns;
         end if;
         Now := Now_Ms * Nanoseconds_Per_Ms;
         if Target > Now then
            Sleep_Ms ((Target - Now) / Nanoseconds_Per_Ms);
         elsif Now - Target > Late_Reset_Ns then
            Target := Now;
         end if;
         <<Continue>>
      end loop;
      GB_Free (Gameboy);
      GB_Dealloc (Gameboy);
      return 0;
   end Run;

   function Main return C.int is
      Status : constant C.int := Run;
   begin
      Shutdown;
      if Status = 0 then
         Say_Line ("sameboy: clean exit");
      end if;
      return Status;
   end Main;

end CuBit.SameBoy_Frontend;
