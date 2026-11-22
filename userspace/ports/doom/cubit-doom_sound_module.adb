pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Doom_Lumps;
with CuBit.Doom_Mixer;
with CuBit.Doom_Sound;

package body CuBit.Doom_Sound_Module is

   package Mixer renames CuBit.Doom_Mixer;
   package Sound renames CuBit.Doom_Sound;
   use type Interfaces.C.int;
   use type Interfaces.C.char;
   use type Interfaces.C.size_t;

   --  z_zone.h: kept for the whole run.
   Zone_Static : constant int := 1;

   function Number_For_Name (Name : System.Address) return int
     with Import, Convention => C, External_Name => "W_GetNumForName";
   function Cache_Lump (Lump, Tag : int) return System.Address
     with Import, Convention => C, External_Name => "W_CacheLumpNum";
   function Lump_Length (Lump : unsigned) return int
     with Import, Convention => C, External_Name => "W_LumpLength";

   --  Configuration variables i_sound.c binds for its SDL sound module;
   --  this module reads neither.
   Use_Libsamplerate : int := 0
     with Export, Convention => C, External_Name => "use_libsamplerate";
   Libsamplerate_Scale : Interfaces.C.C_float := 0.65
     with Export, Convention => C, External_Name => "libsamplerate_scale";

   function Valid_Channel (Channel : int) return Boolean is
     (Channel >= 0 and then Channel < Mixer.Channel_Count);

   function Init (Use_Prefix : boolean_t) return boolean_t
     with Convention => C;
   function Init (Use_Prefix : boolean_t) return boolean_t is
      pragma Unreferenced (Use_Prefix);
   begin
      return (if Sound.Init then 1 else 0);
   end Init;

   procedure Shutdown with Convention => C;
   procedure Shutdown is
   begin
      Sound.Shutdown;
   end Shutdown;

   --  The WAD names sound lumps "DS" followed by the effect's name.
   function Get_Sfx_Lump (Sfx : access constant Sfx_Info) return int
     with Convention => C;
   function Get_Sfx_Lump (Sfx : access constant Sfx_Info) return int is
      Effect_Characters : constant := 6;
      Prefix_Length : constant := 2;
      Name : aliased Interfaces.C.char_array
        (0 .. Prefix_Length + Effect_Characters) :=
        [0 => 'D', 1 => 'S', others => Interfaces.C.nul];
      Last : Interfaces.C.size_t := Prefix_Length;
   begin
      for I in 0 .. Effect_Characters - 1 loop
         exit when Sfx.Name (I) = Interfaces.C.nul;
         Name (Last) := Sfx.Name (I);
         Last := Last + 1;
      end loop;
      return Number_For_Name (Name'Address);
   end Get_Sfx_Lump;

   procedure Update with Convention => C;
   procedure Update is
   begin
      Sound.Update;
   end Update;

   procedure Update_Parameters (Channel, Vol, Sep : int)
     with Convention => C;
   procedure Update_Parameters (Channel, Vol, Sep : int) is
   begin
      if Valid_Channel (Channel) then
         Sound.Update_Parameters
           (Mixer.Channel_Index (Channel), Integer (Vol), Integer (Sep));
      end if;
   end Update_Parameters;

   function Start (Sfx : access constant Sfx_Info; Channel, Vol, Sep : int)
     return int with Convention => C;
   function Start (Sfx : access constant Sfx_Info; Channel, Vol, Sep : int)
     return int
   is
      Length : int;
      Data   : System.Address;
   begin
      if not Valid_Channel (Channel) or else Sfx.Lump_Number < 0 then
         return -1;
      end if;
      Length := Lump_Length (unsigned (Sfx.Lump_Number));
      if Length < Doom_Lumps.Header_Bytes then
         return -1;
      end if;
      Data := Cache_Lump (Sfx.Lump_Number, Zone_Static);
      declare
         Head : constant Doom_Lumps.Header with Import, Address => Data;
         Found : constant Doom_Lumps.Sound :=
           Doom_Lumps.Parse (Head, Doom_Lumps.Lump_Length (Length));
      begin
         if not Found.Valid then
            return -1;
         end if;
         Sound.Start (Mixer.Channel_Index (Channel),
                      Data + Storage_Offset (Found.First),
                      Mixer.Sample_Count (Found.Count),
                      Mixer.Sample_Rate (Found.Rate), Integer (Vol),
                      Integer (Sep));
      end;
      return Channel;
   end Start;

   procedure Stop (Channel : int) with Convention => C;
   procedure Stop (Channel : int) is
   begin
      if Valid_Channel (Channel) then
         Sound.Stop (Mixer.Channel_Index (Channel));
      end if;
   end Stop;

   function Is_Playing (Channel : int) return boolean_t with Convention => C;
   function Is_Playing (Channel : int) return boolean_t is
     (if Valid_Channel (Channel)
        and then Sound.Is_Playing (Mixer.Channel_Index (Channel))
      then 1 else 0);

   --  Lumps are read on demand; nothing to precache.
   procedure Cache (Sounds : System.Address; Count : int) with Convention => C;
   procedure Cache (Sounds : System.Address; Count : int) is null;

   --  Music: not played.
   function Music_Init return boolean_t with Convention => C;
   function Music_Init return boolean_t is (0);
   procedure Nothing with Convention => C;
   procedure Nothing is null;
   procedure Music_Set_Volume (Volume : int) with Convention => C;
   procedure Music_Set_Volume (Volume : int) is null;
   function Music_Register (Data : System.Address; Length : int)
     return System.Address with Convention => C;
   function Music_Register (Data : System.Address; Length : int)
     return System.Address is (System.Null_Address);
   procedure Music_Unregister (Handle : System.Address) with Convention => C;
   procedure Music_Unregister (Handle : System.Address) is null;
   procedure Music_Play (Handle : System.Address; Looping : boolean_t)
     with Convention => C;
   procedure Music_Play (Handle : System.Address; Looping : boolean_t) is null;
   function Music_Playing return boolean_t with Convention => C;
   function Music_Playing return boolean_t is (0);

   Devices : aliased constant Device_List := [1 => Sound_Blaster];

   DG_Sound_Module : aliased constant Sound_Module :=
     (Devices           => Devices'Access,
      Device_Count      => Device_List'Length,
      Init              => Init'Access,
      Shutdown          => Shutdown'Access,
      Get_Sfx_Lump      => Get_Sfx_Lump'Access,
      Update            => Update'Access,
      Update_Parameters => Update_Parameters'Access,
      Start             => Start'Access,
      Stop              => Stop'Access,
      Is_Playing        => Is_Playing'Access,
      Cache             => Cache'Access)
     with Export, Convention => C, External_Name => "DG_sound_module";

   DG_Music_Module : aliased constant Music_Module :=
     (Devices      => Devices'Access,
      Device_Count => Device_List'Length,
      Init         => Music_Init'Access,
      Shutdown     => Nothing'Access,
      Set_Volume   => Music_Set_Volume'Access,
      Pause        => Nothing'Access,
      Resume       => Nothing'Access,
      Register     => Music_Register'Access,
      Unregister   => Music_Unregister'Access,
      Play         => Music_Play'Access,
      Stop         => Nothing'Access,
      Is_Playing   => Music_Playing'Access,
      Poll         => Nothing'Access)
     with Export, Convention => C, External_Name => "DG_music_module";

end CuBit.Doom_Sound_Module;
