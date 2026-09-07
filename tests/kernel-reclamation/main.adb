with Ada.Text_IO;
with Interfaces; use Interfaces;
with Frame_Pins;
with TLB_Reclamation;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with SlabAllocator;
with StoragePools;
with Spinlocks;

procedure Main is
    use type Frame_Pins.Release_Action;
    use TLB_Reclamation;
    F : Frame_Pins.State;
    Action : Frame_Pins.Release_Action;
    OK : Boolean;
    R : State;
    Targets : CPU_Set;
    Seen : Acknowledgments;

    procedure Check_Slabs is
        Pool : SlabAllocator.Slab;
        Addresses : array (1 .. 130) of System.Address;
        Contender, Returned : System.Address;
        Storage : StoragePools.StoragePool;
        Before : array (StoragePools.slabs'Range) of Natural;

        procedure Competing_Allocation is
        begin
            SlabAllocator.Allocate (Pool, Contender);
        end Competing_Allocation;
    begin
        SlabAllocator.setup (Pool, 17 * 8, 2, 16);
        pragma Assert (Pool.alignment = 16 and Pool.paddedSize = 32);
        -- A 4 KiB block holds 128 padded objects. Force expansion, then check
        -- exclusivity and alignment of every object returned by the real slab.
        for I in Addresses'Range loop
            SlabAllocator.Allocate (Pool, Addresses (I));
            pragma Assert (To_Integer (Addresses (I)) mod 16 = 0);
            for J in Addresses'First .. I - 1 loop
                pragma Assert (Addresses (I) /= Addresses (J));
            end loop;
        end loop;
        pragma Assert (Pool.numBlocks = 2);
        for Address of Addresses loop
            SlabAllocator.Deallocate (Pool, Address);
        end loop;
        -- Move the free-list head in the interval preceding lock acquisition.
        Spinlocks.Before_Next_Lock := Competing_Allocation'Unrestricted_Access;
        SlabAllocator.Allocate (Pool, Returned);
        pragma Assert (Returned /= Contender);
        SlabAllocator.Deallocate (Pool, Contender);
        SlabAllocator.Deallocate (Pool, Returned);
        pragma Assert (Pool.numFree = 256);

        StoragePools.setup;
        for I in Before'Range loop
            Before (I) := StoragePools.slabs (I).numFree;
        end loop;
        StoragePools.Allocate (Storage, Returned, 32, 0);
        StoragePools.Deallocate (Storage, Returned, 32, 0);
        for I in Before'Range loop
            pragma Assert (StoragePools.slabs (I).numFree = Before (I));
        end loop;
        Ada.Text_IO.Put_Line ("SLAB-ADAPTER-CHECK: PASS (expansion, interleaving, alignment, small free)");
    end Check_Slabs;
begin
    -- Exercise every byte value through the actual production implementation.
    for Raw in Unsigned_8 loop
        F := Frame_Pins.Decode (Raw);
        pragma Assert (Frame_Pins.Encode (F) = Raw);
        Frame_Pins.Pin (F, OK);
        pragma Assert (OK = (Raw < 127));
        F := Frame_Pins.Decode (Raw);
        Frame_Pins.Request_Free (F, Action);
        pragma Assert ((Action = Frame_Pins.Reclaim_Frame) = ((Raw and 127) = 0));
        while Frame_Pins.Count (F) > 0 loop
            Frame_Pins.Unpin (F, OK, Action);
            pragma Assert (OK);
            pragma Assert ((Action = Frame_Pins.Reclaim_Frame) =
                           (Frame_Pins.Count (F) = 0));
        end loop;
        Frame_Pins.Unpin (F, OK, Action);
        pragma Assert (not OK and Action = Frame_Pins.Keep_Frame);
    end loop;

    -- Every target subset; withhold each required CPU in turn, then complete.
    -- Replayed observations from an earlier round must never unlock a new one.
    for Mask in 0 .. 2 ** CPU_Index'Last * 2 - 1 loop
        for CPU in CPU_Index loop
            Targets (CPU) := (Unsigned_64 (Mask) and
                              Shift_Left (Unsigned_64 (1), CPU)) /= 0;
        end loop;
        for Withheld in CPU_Index loop
            R := Initial_State;
            Begin_Round (R, Targets, OK);
            pragma Assert (OK);
            Seen := (others => Ticket (R));
            Seen (Withheld) := 0;
            Observe (R, Seen);
            pragma Assert (Can_Reclaim (R) = not Targets (Withheld));
            Seen (Withheld) := Ticket (R);
            Observe (R, Seen);
            Take_Completion (R, OK);
            pragma Assert (OK);
            Take_Completion (R, OK);
            pragma Assert (not OK);
            Begin_Round (R, (others => True), OK);
            pragma Assert (OK);
            Observe (R, Seen);
            Take_Completion (R, OK);
            pragma Assert (not OK);
        end loop;
    end loop;
    Ada.Text_IO.Put_Line ("RECLAMATION-STATE-CHECK: PASS (256 pin states, 2048 TLB schedules)");
    Check_Slabs;
end Main;
