-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- The kernel's side of the system publications (publication_pages.ads).
-------------------------------------------------------------------------------
with System;
with acpi;
with BuddyAllocator;
with Multiboot;
with Platform_Monotonic;
with Util;

package body Publication_Pages with
    SPARK_Mode => Off
is
    package SP renames System_Publications;
    use type Virtmem.PhysAddress;

    Facts_Frame, Registry_Frame, Wall_Clock_Frame : Virtmem.PhysAddress := 0;

    procedure Allocate (Frame : out Virtmem.PhysAddress) is
        Ignore : System.Address;
    begin
        BuddyAllocator.allocFrame (Frame);
        if Frame = 0 then
            raise Program_Error with "No frame for a system publication";
        end if;
        Ignore := Util.memset (Virtmem.P2Va (Frame), 0, SP.Page_Bytes);
    end Allocate;

    procedure Publish_Facts is
    begin
        if Facts_Frame /= 0 then
            raise Program_Error with "System facts published twice";
        end if;
        Allocate (Facts_Frame);
        Allocate (Registry_Frame);
        Allocate (Wall_Clock_Frame);
        declare
            Facts : SP.Facts_Page with
                Import, Volatile, Address => Virtmem.P2Va (Facts_Frame);
            Registry : SP.Registry_Page with
                Import, Volatile, Address => Virtmem.P2Va (Registry_Frame);
            Wall : SP.Wall_Clock_Page with
                Import, Volatile, Address => Virtmem.P2Va (Wall_Clock_Frame);
        begin
            Facts.Processor_Count := Unsigned_64 (acpi.numCPUs);
            Facts.Memory_Bytes := Unsigned_64 (BuddyAllocator.getTotalBytes);
            if Multiboot.Has_Graphics then
                Facts.Framebuffer :=
                  (Width          => Unsigned_64 (Multiboot.Framebuffer.Width),
                   Height         => Unsigned_64 (Multiboot.Framebuffer.Height),
                   Pitch          => Unsigned_64 (Multiboot.Framebuffer.Pitch),
                   Bits_Per_Pixel => 32);
            end if;
            Facts.Clock :=
              (Status       => Platform_Monotonic.Diagnostic (0),
               HPET_ID      => Platform_Monotonic.Diagnostic (1),
               Period_Fs    => Platform_Monotonic.Diagnostic (2),
               Timer_Offset => Platform_Monotonic.Diagnostic (3),
               Timer_Before => Platform_Monotonic.Diagnostic (4),
               Timer_After  => Platform_Monotonic.Diagnostic (5));
            Facts.Version := SP.Layout_Version;
            Registry.Version := SP.Layout_Version;
            Wall.Version := SP.Layout_Version;
        end;
    end Publish_Facts;

    procedure Map (Root : in out Virtmem.P4; Success : out Boolean) is
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        Success := False;
        if Facts_Frame = 0 then
            return;
        end if;
        mapPage (Facts_Frame, SP.Facts_Address, Virtmem.PG_USERDATARO, Root, Success);
        if Success then
            mapPage (Registry_Frame, SP.Registry_Address, Virtmem.PG_USERDATARO,
                     Root, Success);
        end if;
        if Success then
            mapPage (Wall_Clock_Frame, SP.Wall_Clock_Address, Virtmem.PG_USERDATARO,
                     Root, Success);
        end if;
    end Map;

    procedure Set_Registered (Role     : SP.Service_Role;
                              Identity : Unsigned_64)
    is
        Registry : SP.Registry_Page with
            Import, Volatile, Address => Virtmem.P2Va (Registry_Frame);
        Entry_Word : Unsigned_64 with
            Import, Atomic, Address => Registry.Identities (Role)'Address;
    begin
        if Registry_Frame /= 0 then
            Entry_Word := Identity;
        end if;
    end Set_Registered;

    procedure Set_Wall_Clock (Offset_Ms : Unsigned_64) is
        Wall : SP.Wall_Clock_Page with
            Import, Volatile, Address => Virtmem.P2Va (Wall_Clock_Frame);
        Offset_Word : Unsigned_64 with
            Import, Atomic, Address => Wall.Offset_Ms'Address;
    begin
        if Wall_Clock_Frame /= 0 then
            Offset_Word := Offset_Ms;
        end if;
    end Set_Wall_Clock;

    procedure Set_Device_Memory_Denials (Count : Unsigned_64) is
        Facts : SP.Facts_Page with
            Import, Volatile, Address => Virtmem.P2Va (Facts_Frame);
        Count_Word : Unsigned_64 with
            Import, Atomic, Address => Facts.Device_Memory_Denials'Address;
    begin
        if Facts_Frame /= 0 then
            Count_Word := Count;
        end if;
    end Set_Device_Memory_Denials;

    procedure Set_Process_Kills (Count : Unsigned_64) is
        Facts : SP.Facts_Page with
            Import, Volatile, Address => Virtmem.P2Va (Facts_Frame);
        Count_Word : Unsigned_64 with
            Import, Atomic, Address => Facts.Process_Kills'Address;
    begin
        if Facts_Frame /= 0 then
            Count_Word := Count;
        end if;
    end Set_Process_Kills;

    procedure Set_Boot_Display_Native (Native : Boolean) is
        Facts : SP.Facts_Page with
            Import, Volatile, Address => Virtmem.P2Va (Facts_Frame);
        Native_Word : Unsigned_64 with
            Import, Atomic, Address => Facts.Boot_Display_Native'Address;
    begin
        if Facts_Frame /= 0 then
            Native_Word := (if Native then 1 else 0);
        end if;
    end Set_Boot_Display_Native;
end Publication_Pages;
