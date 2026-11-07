with System; use System;
with BootAllocator;
with BuddyAllocator;
with Boot_Modules;
with Config;
with Mem_mgr;
with Multiboot;
with TextIO; use TextIO;
with Virtmem;

package body Boot_Reclaim with SPARK_Mode => Off is
    type Symbol is null record;
    sbootReclaim : Symbol
        with Import, Convention => C, External_Name => "sboot_reclaim";
    ebootReclaim : Symbol
        with Import, Convention => C, External_Name => "eboot_reclaim";

    type Step is (Pending, Done);
    sectionStep  : Step := Pending;
    metadataStep : Step := Pending;
    sectionReturned  : Storage_Count := 0;
    metadataReturned : Storage_Count := 0;

    function KiB (Bytes : Storage_Count) return Integer is
      (Integer (Bytes / 1024));

    procedure Retire_Boot_Allocator is
        use type BootAllocator.Allocator_Phase;
        first : constant Virtmem.PhysAddress :=
          Virtmem.K2P (To_Integer (sbootReclaim'Address));
        limit : constant Virtmem.PhysAddress :=
          Virtmem.K2P (To_Integer (ebootReclaim'Address));
        before : constant Storage_Count := BuddyAllocator.getTotalBytes;
    begin
        if sectionStep /= Pending or else
          BootAllocator.Phase /= BootAllocator.Active
        then
            raise ReclaimException with "Boot allocator retirement out of order";
        end if;
        -- The linker script page-aligns both ends; never free a partial page
        -- shared with other kernel data.
        if first mod Virtmem.FRAME_SIZE /= 0 or else
          limit mod Virtmem.FRAME_SIZE /= 0 or else limit < first
        then
            raise ReclaimException with "Reclaimable boot section is not page-aligned";
        end if;
        BootAllocator.Retire;
        sectionStep := Done;
        if limit = first then
            return;
        end if;
        declare
            page : Virtmem.PhysAddress := first;
        begin
            while page < limit loop
                Mem_mgr.retireKernelImagePage (page);
                page := page + Virtmem.FRAME_SIZE;
            end loop;
        end;
        BuddyAllocator.adoptFrames
          (first, Positive ((limit - first) / Virtmem.FRAME_SIZE));
        sectionReturned := Storage_Count (limit - first);
        print ("Boot reclaim: boot allocator retired, section returned ");
        print (KiB (sectionReturned)); print (" KiB (buddy total ");
        print (KiB (before)); print (" -> ");
        print (KiB (BuddyAllocator.getTotalBytes)); println (" KiB)");
    end Retire_Boot_Allocator;

    procedure Release_Loader_Metadata is
        before : constant Storage_Count := BuddyAllocator.getTotalBytes;
    begin
        if sectionStep /= Done or else metadataStep /= Pending then
            raise ReclaimException with "Loader metadata release out of order";
        end if;
        metadataStep := Done;
        for I in 1 .. Multiboot.Loader_Metadata_Count loop
            declare
                Span : constant Multiboot.Page_Span := Multiboot.Loader_Metadata (I);
                First : constant Boot_Modules.Address := Boot_Modules.Address'Max
                  (Span.First, Boot_Modules.Address (Config.MIN_PHYS_ALLOC));
            begin
                if First < Span.Limit then
                    BuddyAllocator.adoptFrames
                      (Virtmem.PhysAddress (First),
                       Positive ((Span.Limit - First) / Boot_Modules.Page_Bytes));
                    metadataReturned := metadataReturned +
                      Storage_Count (Span.Limit - First);
                end if;
            end;
        end loop;
        print ("Boot reclaim: loader metadata returned ");
        print (KiB (metadataReturned)); print (" KiB (buddy total ");
        print (KiB (before)); print (" -> ");
        print (KiB (BuddyAllocator.getTotalBytes)); println (" KiB)");
    end Release_Loader_Metadata;

    function Section_Bytes return Storage_Count is (sectionReturned);
    function Metadata_Bytes return Storage_Count is (metadataReturned);
end Boot_Reclaim;
