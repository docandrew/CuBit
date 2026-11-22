------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  DOOM's sound and music modules (doomgeneric i_sound.h): the tables DOOM
--  calls through, exported with C layout. Sound effects play on
--  CuBit.Doom_Sound after CuBit.Doom_Lumps (proved) checks the lump.
--  Music is not played.
--
--  tests/doom-port checks these record layouts against i_sound.h.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with System;

package CuBit.Doom_Sound_Module
  with Elaborate_Body
is

   subtype int is Interfaces.C.int;
   subtype unsigned is Interfaces.C.unsigned;
   --  doomtype.h's boolean: a 32-bit enumeration in C11 builds.
   subtype boolean_t is unsigned;

   Sfx_Name_Length : constant := 9;
   type Sfx_Name is array (0 .. Sfx_Name_Length - 1) of Interfaces.C.char
     with Convention => C;

   --  sfxinfo_t
   type Sfx_Info is record
      Tag_Name      : System.Address;
      Name          : Sfx_Name;
      Priority      : int;
      Link          : System.Address;
      Pitch         : int;
      Volume        : int;
      Usefulness    : int;
      Lump_Number   : int;
      Channel_Limit : int;
      Driver_Data   : System.Address;
   end record
     with Convention => C;

   --  snddevice_t
   type Sound_Device is new unsigned;
   Sound_Blaster : constant Sound_Device := 3;
   type Device_List is array (1 .. 1) of Sound_Device with Convention => C;
   type Device_List_Access is access constant Device_List
     with Convention => C;

   type Init_Sound is access function (Use_Prefix : boolean_t) return boolean_t
     with Convention => C;
   type Action is access procedure with Convention => C;
   type Lump_Of is access function (Sfx : access constant Sfx_Info) return int
     with Convention => C;
   type Channel_Parameters is access procedure (Channel, Vol, Sep : int)
     with Convention => C;
   type Start_Sound is access function
     (Sfx : access constant Sfx_Info; Channel, Vol, Sep : int) return int
     with Convention => C;
   type Channel_Action is access procedure (Channel : int)
     with Convention => C;
   type Channel_Query is access function (Channel : int) return boolean_t
     with Convention => C;
   type Cache_Sounds is access procedure
     (Sounds : System.Address; Count : int)
     with Convention => C;

   --  sound_module_t
   type Sound_Module is record
      Devices            : Device_List_Access;
      Device_Count       : int;
      Init               : Init_Sound;
      Shutdown           : Action;
      Get_Sfx_Lump       : Lump_Of;
      Update             : Action;
      Update_Parameters  : Channel_Parameters;
      Start              : Start_Sound;
      Stop               : Channel_Action;
      Is_Playing         : Channel_Query;
      Cache              : Cache_Sounds;
   end record
     with Convention => C;

   type Init_Music is access function return boolean_t with Convention => C;
   type Music_Volume is access procedure (Volume : int) with Convention => C;
   type Register_Song is access function (Data : System.Address; Length : int)
     return System.Address with Convention => C;
   type Song_Action is access procedure (Handle : System.Address)
     with Convention => C;
   type Play_Song is access procedure
     (Handle : System.Address; Looping : boolean_t) with Convention => C;
   type Music_Query is access function return boolean_t with Convention => C;

   --  music_module_t
   type Music_Module is record
      Devices      : Device_List_Access;
      Device_Count : int;
      Init         : Init_Music;
      Shutdown     : Action;
      Set_Volume   : Music_Volume;
      Pause        : Action;
      Resume       : Action;
      Register     : Register_Song;
      Unregister   : Song_Action;
      Play         : Play_Song;
      Stop         : Action;
      Is_Playing   : Music_Query;
      Poll         : Action;
   end record
     with Convention => C;

end CuBit.Doom_Sound_Module;
