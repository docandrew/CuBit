-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- The kernel log ring mapped into the log service (kernel_log_mapping.ads).
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with BuddyAllocator;
with Kernel_Log;
with Kernel_Log_Publication;
with Virtmem;

package body Kernel_Log_Mapping with
    SPARK_Mode => Off
is
    package KLP renames Kernel_Log_Publication;
    use type Virtmem.PhysAddress;

    procedure Map_Into (Reader : Process.ProcessID; Mapped : out Boolean) is
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
        First : constant Virtmem.PhysAddress := Virtmem.K2P (Kernel_Log.Area_Address);
    begin
        Mapped := True;
        Process.lockAddressSpace (Reader);
        for Page in Integer_Address range 0 .. KLP.Area_Pages - 1 loop
            declare
                Frame : constant Virtmem.PhysAddress := First + Page * KLP.Page_Bytes;
                Where : constant Virtmem.VirtAddress := KLP.Page_Address + Page * KLP.Page_Bytes;
                Done : Boolean;
            begin
                -- Registered again: the page is already there.
                if Virtmem.tableWalk (Where, Process.addrtab (Reader)) /= Frame then
                    mapPage (Frame, Where, Virtmem.PG_USERDATARO,
                             Process.addrtab (Reader), Done);
                    if not Done then
                        Mapped := False;
                        exit;
                    end if;
                end if;
            end;
        end loop;
        Process.unlockAddressSpace (Reader);
    end Map_Into;
end Kernel_Log_Mapping;
