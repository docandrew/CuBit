-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- The kernel's side of the per-CPU times publication (cpu_times_page.ads).
-------------------------------------------------------------------------------
with System;
with System.Storage_Elements; use System.Storage_Elements;
with BuddyAllocator;
with Util;

package body Cpu_Times_Page with
    SPARK_Mode => Off
is
    use type Virtmem.PhysAddress;

    Frame : Virtmem.PhysAddress := 0;
    Published_Processors : Natural := 0;

    procedure Publish (Processors     : CTP.Processor_Count;
                       Tick_Frequency : Unsigned_64;
                       Memory_Bytes   : Unsigned_64)
    is
        Ignore : System.Address;
    begin
        if Frame /= 0 then
            raise Program_Error with "CPU times published twice";
        end if;
        BuddyAllocator.allocFrame (Frame);
        if Frame = 0 then
            raise Program_Error with "No frame for the CPU times publication";
        end if;
        Ignore := Util.memset (Virtmem.P2Va (Frame), 0, CTP.Page_Bytes);
        declare
            Page : CTP.Page with
                Import, Volatile, Address => Virtmem.P2Va (Frame);
        begin
            Page.Head.Processors := Processors;
            Page.Head.Tick_Frequency := Tick_Frequency;
            Page.Head.Memory_Bytes := Memory_Bytes;
            Page.Head.Free_Memory_Bytes := Unsigned_64 (BuddyAllocator.getFreeBytes);
            Page.Head.Version := CTP.Layout_Version;
        end;
        Published_Processors := Natural (Processors);
    end Publish;

    procedure Write (CPU : Natural; Values : CTP.Fields) is
    begin
        if Frame = 0 or else CPU >= Published_Processors then
            return;
        end if;
        declare
            Target : CTP.CPU_Record with
                Import, Volatile,
                Address => Virtmem.P2Va (Frame) +
                           Storage_Offset (CTP.Header_Bytes + CPU * CTP.Record_Bytes);
        begin
            Target.Sequence := CTP.Opened (Target.Sequence);
            Target.Values := Values;
            Target.Sequence := CTP.Closed (Target.Sequence);
        end;
    end Write;

    procedure Set_Free_Memory (Bytes : Unsigned_64) is
    begin
        if Frame = 0 then
            return;
        end if;
        declare
            Page : CTP.Page with
                Import, Volatile, Address => Virtmem.P2Va (Frame);
            Free_Word : Unsigned_64 with
                Import, Atomic, Address => Page.Head.Free_Memory_Bytes'Address;
        begin
            Free_Word := Bytes;
        end;
    end Set_Free_Memory;

    procedure Map_Into (Root : in out Virtmem.P4; Success : out Boolean) is
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        Success := False;
        if Frame = 0 then
            return;
        end if;
        if Virtmem.tableWalk (CTP.Page_Address, Root) = Frame then
            Success := True;
        else
            mapPage (Frame, CTP.Page_Address, Virtmem.PG_USERDATARO, Root, Success);
        end if;
    end Map_Into;
end Cpu_Times_Page;
