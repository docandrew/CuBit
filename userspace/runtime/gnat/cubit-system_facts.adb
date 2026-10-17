------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with Process_Layout;
with System_Publications;

package body CuBit.System_Facts is

   package Pages renames System_Publications;

   --  The reader's types and constants are the shared layout's.
   pragma Compile_Time_Error
     (Secondary_Stack_Address /= Process_Layout.Secondary_Stack_Address,
      "secondary stack address differs from Process_Layout");
   pragma Compile_Time_Error
     (SP.Statistics'Size /= Pages.Statistics'Size or else
      SP.Published_Value'Enum_Rep (SP.Wall_Clock_Offset) /=
        Pages.Published_Value'Enum_Rep (Pages.Wall_Clock_Offset) or else
      SP.Published_Value'Enum_Rep (SP.Boot_Display_Native) /=
        Pages.Published_Value'Enum_Rep (Pages.Boot_Display_Native),
      "the system facts reader differs from System_Publications");

   Facts : Pages.Facts_Page
   with Import, Volatile, Address => System'To_Address (Pages.Facts_Address);
   Registry : Pages.Registry_Page
   with Import, Volatile, Address => System'To_Address (Pages.Registry_Address);
   Wall : Pages.Wall_Clock_Page
   with Import, Volatile, Address => System'To_Address (Pages.Wall_Clock_Address);

   function Facts_Published return Boolean is
     (Facts.Version = Pages.Layout_Version);

   function Processor_Count return Unsigned_64 is
     (if Facts_Published then Facts.Processor_Count else 0);

   function Memory_Bytes return Unsigned_64 is
     (if Facts_Published then Facts.Memory_Bytes else 0);

   function Framebuffer return SP.Framebuffer_Geometry is
     (if Facts_Published
      then (Width          => Facts.Framebuffer.Width,
            Height         => Facts.Framebuffer.Height,
            Pitch          => Facts.Framebuffer.Pitch,
            Bits_Per_Pixel => Facts.Framebuffer.Bits_Per_Pixel)
      else (others => 0));

   function Boot_Display_Native return Boolean is
     (Facts_Published and then Facts.Boot_Display_Native = 1);

   function Clock return SP.Clock_Diagnostic is
     (if Facts_Published
      then (Status       => Facts.Clock.Status,
            HPET_ID      => Facts.Clock.HPET_ID,
            Period_Fs    => Facts.Clock.Period_Fs,
            Timer_Offset => Facts.Clock.Timer_Offset,
            Timer_Before => Facts.Clock.Timer_Before,
            Timer_After  => Facts.Clock.Timer_After)
      else (others => 0));

   function Registered (Role : Unsigned_64) return Unsigned_64 is
     (if Role in Pages.Service_Role and then Registry.Version = Pages.Layout_Version
      then Registry.Identities (Role) else Pages.No_Identity);

   function Device_Memory_Denials return Unsigned_64 is
     (if Facts_Published then Facts.Device_Memory_Denials else 0);

   function Process_Kills return Unsigned_64 is
     (if Facts_Published then Facts.Process_Kills else 0);

   function Scanout_Overwrites return Unsigned_64 is
     (if Facts_Published then Facts.Scanout_Overwrites else 0);

   function Wall_Clock_Offset return Unsigned_64 is
     (if Wall.Version = Pages.Layout_Version then Wall.Offset_Ms else 0);

   procedure Read_Statistics
     (Value : out SP.Statistics; Success : out Boolean)
   is
      Result : Unsigned_64;
   begin
      Value := (others => 0);
      Result := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Read_Statistics,
         Unsigned_64 (System.Storage_Elements.To_Integer (Value'Address)),
         Pages.Statistics_Bytes);
      Success := Result = 0;
   end Read_Statistics;

   function Publish
     (Value : SP.Published_Value; Word : Unsigned_64) return Boolean is
     (CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Publish, SP.Published_Value'Enum_Rep (Value), Word) = 0);

end CuBit.System_Facts;
