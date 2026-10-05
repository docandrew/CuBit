------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  doomgeneric's platform hooks for CuBit, and DOOM's main
--  (docs/c-removal.md). DOOM itself is unmodified C on the CuBit libc.
--
--  Video: a window on desktop.svc showing a buffer lent to it once, through
--  the proved CuBit.Desktop_Protocol codec. Input: desktop key events,
--  translated by CuBit.Doom_Keys. Time: the kernel's millisecond clock.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;
with System;

package CuBit.Doom_Platform is

   procedure Init
     with Export, Convention => C, External_Name => "DG_Init";
   procedure Draw_Frame
     with Export, Convention => C, External_Name => "DG_DrawFrame";
   procedure Sleep_Ms (Milliseconds : Unsigned_32)
     with Export, Convention => C, External_Name => "DG_SleepMs";
   function Ticks_Ms return Unsigned_32
     with Export, Convention => C, External_Name => "DG_GetTicksMs";
   function Get_Key (Pressed : access Interfaces.C.int;
                     Code : access Interfaces.C.unsigned_char)
                     return Interfaces.C.int
     with Export, Convention => C, External_Name => "DG_GetKey";
   procedure Set_Window_Title (Title : System.Address)
     with Export, Convention => C, External_Name => "DG_SetWindowTitle";

   function Main return Interfaces.C.int
     with Export, Convention => C, External_Name => "main";

end CuBit.Doom_Platform;
