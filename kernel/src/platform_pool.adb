pragma Ada_2022;
-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- Platform_Pool - implementation (ACPI-POOL-001)
-------------------------------------------------------------------------------
with System; use System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;

with BuddyAllocator;
with Capability_ABI;
with Device_Memory_Admission;
with Memory_Accounting;
with Physical_Range_Map;
with Physical_Ranges;
with Process.Owned_Memory;
with Process_Identities;
with Process_Memory_Budget;
with Spinlocks;
with TextIO; use TextIO;
with TLB_Shootdown;
with Virtmem;

package body Platform_Pool with
    SPARK_Mode => Off
is
    use type Capabilities.CapabilityType;
    use type Capabilities.Authority_Tag;
    use type Process.ProcessID;
    use type Sets.Decision_Kind;
    use type Sets.Interval;
    use type Sets.Add_Status;
    use type Sets.Pool_Member_Kind;
    use type Platform_Pool_ABI.Refusal;
    use type BuddyAllocator.Order;
    use type Virtmem.PhysAddress;
    use type Sets.Insert_Status;

    package ABI renames Platform_Pool_ABI;
    package CA renames Capability_ABI;

    pragma Compile_Time_Error
      (ABI.Maximum_Members /= Sets.Member_Ceiling
       or else ABI.Maximum_Claims /= Sets.Claim_Ceiling,
       "the pool's ABI ceilings and the proved core's must agree");

    Page_Bytes : constant := Sets.Page_Bytes;
    -- Exclusive end of the lower-half user address space.
    User_Address_Limit : constant Unsigned_64 := 16#0000_8000_0000_0000#;

    package State_Pointers is new System.Address_To_Access_Conversions (Sets.State);
    package Table_Pointers is new System.Address_To_Access_Conversions (Sets.Mapping_Table);
    use type State_Pointers.Object_Pointer;
    use type Table_Pointers.Object_Pointer;

    -- A block of 2 ** Order frames from the frame allocator, every frame
    -- charged to one memory account (released with BuddyAllocator.free,
    -- which refunds the charge).
    type Block is record
        Base  : System.Address := Null_Address;
        Order : BuddyAllocator.Order := 0;
    end record;

    -- A holder's mapping table, by process slot.
    type Holder_Record is record
        Id      : Unsigned_64 := Sets.No_Identity;
        Storage : Block;
        Table   : Table_Pointers.Object_Pointer := null;
    end record;

    Lock      : Spinlocks.Spinlock;
    Owner_Id  : Unsigned_64 := Sets.No_Identity;
    -- The declared pool (null until devmgr declares it) and its block.
    Pool      : State_Pointers.Object_Pointer := null;
    Pool_Storage : Block;
    -- In use: members are fixed from the first mapping or claim-worthy
    -- delegation on, so every claim that can matter is recorded.
    Sealed    : Boolean := False;
    Holders   : array (Process.ProcessID) of Holder_Record;

    function Identity_Word (Pid : Process.ProcessID) return Unsigned_64 is
      (Process_Identities.To_Word (Process.identityOf (Pid)));

    ---------------------------------------------------------------------------
    -- Charged blocks
    ---------------------------------------------------------------------------

    -- Bytes from the frame allocator, every frame charged to Account's
    -- metadata (the same charge Charged_Metadata_Pages takes per page).
    procedure Allocate (Account : Unsigned_64; Bytes : Storage_Count;
                        Result : out Block; OK : out Boolean) is
        Charge    : Memory_Accounting.Identity;
        Frames    : Unsigned_64;
        Bound     : Unsigned_64 := 0;
        Bound_Ok  : Boolean;
        Cancelled : Boolean;
    begin
        Result := (Base => Null_Address, Order => 0);
        OK := False;
        if Account = 0 or else Bytes = 0
          or else Bytes > BuddyAllocator.blockSize (BuddyAllocator.Order'Last)
        then
            return;
        end if;
        Result.Order := BuddyAllocator.getOrder (Bytes);
        Frames := 2 ** Natural (Result.Order);
        Memory_Accounting.Reserve
          (Memory_Accounting.Identity (Account), Process_Memory_Budget.Metadata,
           Frames, Charge, OK);
        if not OK then
            return;
        end if;
        BuddyAllocator.alloc (Result.Order, Result.Base);
        if Result.Base /= Null_Address then
            for I in 0 .. Frames - 1 loop
                BuddyAllocator.bindKernelFrameCharge
                  (Virtmem.V2P (Result.Base) + Virtmem.PhysAddress (I * Page_Bytes),
                   Charge, Bound_Ok);
                exit when not Bound_Ok;
                Bound := Bound + 1;
            end loop;
            if Bound = Frames then
                return;
            end if;
            -- Refunds the frames already bound when they are reclaimed.
            BuddyAllocator.free (Result.Order, Result.Base);
            Result.Base := Null_Address;
        end if;
        Memory_Accounting.Cancel_Unbound (Charge, Frames - Bound, Cancelled);
        if not Cancelled then
            raise Program_Error with "platform-pool: charge rollback failed";
        end if;
        OK := False;
    end Allocate;

    procedure Release (B : in out Block) is
    begin
        if B.Base /= Null_Address then
            BuddyAllocator.free (B.Order, B.Base);
        end if;
        B := (Base => Null_Address, Order => 0);
    end Release;

    function State_Bytes (Members, Claims : Positive) return Storage_Count is
        subtype Sized is Sets.State (Members, Claims);
    begin
        return Sized'Max_Size_In_Storage_Elements;
    end State_Bytes;

    function Table_Bytes (Room : Sets.Mapping_Room) return Storage_Count is
        subtype Sized is Sets.Mapping_Table (Room);
    begin
        return Sized'Max_Size_In_Storage_Elements;
    end Table_Bytes;

    -- The most mapping records a block of Order holds, at most the ceiling.
    function Room_Of (Order : BuddyAllocator.Order) return Sets.Mapping_Count is
        Bytes : constant Storage_Count := BuddyAllocator.blockSize (Order);
        Record_Bytes : constant Storage_Count :=
          Storage_Count (Sets.Mapping_Array'Component_Size / Storage_Unit);
        Room : Natural := Natural'Min
          (Sets.Mapping_Ceiling, Natural (Bytes / Record_Bytes));
    begin
        while Room > 0 and then Table_Bytes (Room) > Bytes loop
            Room := Room - 1;
        end loop;
        return Room;
    end Room_Of;

    -- A fresh, empty table of Room records in B (default initialized).
    function New_Table (B : Block; Room : Sets.Mapping_Room)
                        return Table_Pointers.Object_Pointer is
    begin
        declare
            pragma Warnings (Off, "*default initialization*");
            Fresh : Sets.Mapping_Table (Room) with Address => B.Base;
            pragma Warnings (On, "*default initialization*");
            pragma Unreferenced (Fresh);
        begin
            null;
        end;
        return Table_Pointers.To_Pointer (B.Base);
    end New_Table;

    -- A fresh, empty pool state sized Members x Claims in B.
    function New_State (B : Block; Members, Claims : Positive)
                        return State_Pointers.Object_Pointer is
    begin
        declare
            pragma Warnings (Off, "*default initialization*");
            Fresh : Sets.State (Members, Claims) with Address => B.Base;
            pragma Warnings (On, "*default initialization*");
            pragma Unreferenced (Fresh);
        begin
            null;
        end;
        return State_Pointers.To_Pointer (B.Base);
    end New_State;

    ---------------------------------------------------------------------------
    -- Log helpers
    ---------------------------------------------------------------------------

    -- [Base, Base + Bytes) as the pages it touches; Valid False when empty,
    -- wrapping or beyond the physical limit.
    procedure Pages_Of
      (Base, Bytes : Unsigned_64; Pages : out Sets.Interval; Valid : out Boolean) is
    begin
        Pages := (First => 0, Last => 0);
        Valid := Bytes > 0 and then Base < Physical_Ranges.Address_Limit
          and then Bytes - 1 <= Physical_Ranges.Address_Limit - 1 - Base;
        if Valid then
            Pages := (First => Base / Page_Bytes,
                      Last  => (Base + (Bytes - 1)) / Page_Bytes);
        end if;
    end Pages_Of;

    procedure Put_Pages (Label : String; Pages : Sets.Interval) is
    begin
        print (Label);
        print (Pages.First * Page_Bytes);
        print (" bytes=");
        print ((Pages.Last - Pages.First + 1) * Page_Bytes);
    end Put_Pages;

    function Kind_Name (Kind : Sets.Pool_Member_Kind) return String is
      (case Kind is
         when Sets.Device_BAR     => ABI.Member_Kind_Name (ABI.Device_BAR),
         when Sets.Chipset_Shared => ABI.Member_Kind_Name (ABI.Chipset_Shared));

    ---------------------------------------------------------------------------
    -- Setup
    ---------------------------------------------------------------------------

    procedure Set_Owner (Owner : Unsigned_64) is
    begin
        Spinlocks.enterCriticalSection (Lock);
        Owner_Id := Owner;
        Spinlocks.exitCriticalSection (Lock);
    end Set_Owner;

    function Declare_Pool
      (Caller : Process.ProcessID; Members, Claims : Unsigned_64) return Unsigned_64
    is
        Bytes  : Storage_Count;
        Answer : Unsigned_64 := 0;
        OK     : Boolean;
    begin
        if Members not in 1 .. Sets.Member_Ceiling
          or else Claims not in 1 .. Sets.Claim_Ceiling
        then
            return CA.Refusal_Word (CA.Invalid_Argument);
        end if;
        Bytes := State_Bytes (Positive (Members), Positive (Claims));
        Spinlocks.enterCriticalSection (Lock);
        if Sealed then
            Answer := CA.Refusal_Word (CA.Sealed);
        elsif Pool /= null then
            Answer := CA.Refusal_Word (CA.Invalid_Argument);
        else
            Allocate (Process.proctab (Caller).memoryAccount, Bytes, Pool_Storage, OK);
            if not OK then
                Answer := CA.Refusal_Word (CA.Out_Of_Memory);
            else
                Pool := New_State (Pool_Storage, Positive (Members), Positive (Claims));
            end if;
        end if;
        Spinlocks.exitCriticalSection (Lock);
        print ("platform-pool: declared members=");
        print (Members);
        print (" claims=");
        print (Claims);
        print (" bytes=");
        print (Unsigned_64 (Bytes));
        println (if Answer = 0 then " status=DECLARED" else " status=REFUSED");
        return Answer;
    end Declare_Pool;

    function Insert
      (Base, Bytes : Unsigned_64; Kind : Sets.Pool_Member_Kind) return Unsigned_64
    is
        Pages  : Sets.Interval;
        Valid  : Boolean;
        Status : Sets.Insert_Status := Sets.Invalid_Interval;
        Answer : Unsigned_64;
    begin
        Pages_Of (Base, Bytes, Pages, Valid);
        if not Valid or else Base mod Page_Bytes /= 0 or else Bytes mod Page_Bytes /= 0 then
            return CA.Refusal_Word (CA.Invalid_Argument);
        end if;
        Spinlocks.enterCriticalSection (Lock);
        if Pool = null then
            Answer := CA.Refusal_Word (CA.Invalid_Argument);
        elsif Sealed then
            Answer := CA.Refusal_Word (CA.Sealed);
        else
            Sets.Insert (Pool.all, (Pages => Pages, Kind => Kind), Status);
            Answer := (case Status is
                         when Sets.Inserted     => 0,
                         when Sets.Members_Full => CA.Refusal_Word (CA.Table_Full),
                         when Sets.Overlaps_Member | Sets.Invalid_Interval =>
                           CA.Refusal_Word (CA.Invalid_Argument));
        end if;
        Spinlocks.exitCriticalSection (Lock);
        Put_Pages ("platform-pool: member phys=", Pages);
        print (" kind=");
        print (Kind_Name (Kind));
        println (if Answer = CA.Refusal_Word (CA.Sealed) then " status=SEALED"
                 elsif Answer /= 0 and then Status = Sets.Invalid_Interval then " status=UNDECLARED"
                 else (case Status is
                         when Sets.Inserted         => " status=INSERTED",
                         when Sets.Invalid_Interval => " status=INVALID",
                         when Sets.Overlaps_Member  => " status=OVERLAPS_MEMBER",
                         when Sets.Members_Full     => " status=CAPACITY"));
        return Answer;
    end Insert;

    ---------------------------------------------------------------------------
    -- Revocation
    ---------------------------------------------------------------------------

    -- Unmap one taken mapping from Holder, page by page, only where it
    -- still maps the recorded physical page there. True when any page was
    -- removed (the caller then shoots the TLBs down).
    function Unmap_Taken (Holder : Process.ProcessID; M : Sets.Mapping) return Boolean is
        Removed : Boolean := False;
        Virtual : Integer_Address;
        Physical : Virtmem.PhysAddress;
        ok : Boolean;
    begin
        if Process.proctab (Holder).pgTable = Process.NO_PROCESS
          or else Identity_Word (Holder) /= M.Holder
        then
            return False;
        end if;
        Process.lockAddressSpace (Holder);
        for Page in M.Pages.First .. M.Pages.Last loop
            Virtual := Integer_Address
              ((M.Virtual_Page + (Page - M.Pages.First)) * Page_Bytes);
            Physical := Virtmem.tableWalk
              (Virtual, Process.addrtab (Process.proctab (Holder).pgTable));
            if Physical = Virtmem.PhysAddress (Page * Page_Bytes) then
                Virtmem.unmapPage
                  (Virtual, Process.addrtab (Process.proctab (Holder).pgTable), ok);
                Removed := Removed or else ok;
            end if;
        end loop;
        Process.unlockAddressSpace (Holder);
        return Removed;
    end Unmap_Taken;

    -- Take and unmap every mapping that now conflicts, under Lock. Only
    -- Device_BAR mappings ever conflict (Platform_Pool_Sets).
    procedure Revoke_Conflicting (Claimant : Unsigned_64; Claimed : Sets.Interval) is
        Found  : Boolean;
        Taken  : Sets.Mapping;
        Flush  : Boolean := False;
    begin
        for P in Holders'Range loop
            if Holders (P).Table /= null then
                loop
                    Sets.Take_Conflicting (Pool.all, Holders (P).Table.all, Found, Taken);
                    exit when not Found;
                    if Unmap_Taken (P, Taken) then
                        Flush := True;
                    end if;
                    Put_Pages ("platform-pool: revoked phys=", Taken.Pages);
                    print (" holder=");
                    print (Taken.Holder);
                    print (" claimant=");
                    print (Claimant);
                    Put_Pages (" claim=", Claimed);
                    println (" kind=DEVICE_BAR");
                end loop;
            end if;
        end loop;
        if Flush then
            TLB_Shootdown.Invalidate_All;
        end if;
    end Revoke_Conflicting;

    -- The first holder with a mapping overlapping Pages, or none.
    function Holder_Over (Pages : Sets.Interval) return Unsigned_64 is
    begin
        for P in Holders'Range loop
            if Holders (P).Table /= null then
                for K in 1 .. Holders (P).Table.Total loop
                    if Sets.Overlaps (Holders (P).Table.Items (K).Pages, Pages) then
                        return Holders (P).Table.Items (K).Holder;
                    end if;
                end loop;
            end if;
        end loop;
        return Sets.No_Identity;
    end Holder_Over;

    -- The claim just recorded (the last one) is shared over every
    -- Chipset_Shared member it overlaps: logged once per (member,
    -- claimant), that is, unless an earlier claim of the same claimant
    -- overlaps the member already. Under Lock.
    procedure Log_Shared (Claimant : Unsigned_64; Claimed : Sets.Interval) is
        S : Sets.State renames Pool.all;
        Earlier : Boolean;
    begin
        for I in 1 .. S.Member_Total loop
            if S.Members (I).Kind = Sets.Chipset_Shared
              and then Sets.Overlaps (Claimed, S.Members (I).Pages)
            then
                Earlier := False;
                for K in 1 .. S.Claim_Total - 1 loop
                    if S.Claims (K).Claimant = Claimant
                      and then Sets.Overlaps (S.Claims (K).Pages, S.Members (I).Pages)
                    then
                        Earlier := True;
                        exit;
                    end if;
                end loop;
                if not Earlier then
                    Put_Pages ("platform-pool: shared phys=", S.Members (I).Pages);
                    print (" holder=");
                    print (Holder_Over (S.Members (I).Pages));
                    print (" claimant=");
                    print (Claimant);
                    Put_Pages (" claim=", Claimed);
                    println (" kind=CHIPSET_SHARED");
                end if;
            end if;
        end loop;
    end Log_Shared;

    -- Step 4 of the flow (see the spec): record Claimant's device memory.
    function Claim (Claimant : Unsigned_64; Base, Bytes : Unsigned_64) return Claim_Outcome is
        Pages   : Sets.Interval;
        Valid   : Boolean;
        Status  : Sets.Claim_Status;
        Outcome : Claim_Outcome := Not_In_Pool;
    begin
        Pages_Of (Base, Bytes, Pages, Valid);
        if not Valid then
            return Not_In_Pool;   -- nothing mappable: no page to keep ACPI off
        end if;
        Spinlocks.enterCriticalSection (Lock);
        if Claimant = Owner_Id or else Claimant = Sets.No_Identity then
            Spinlocks.exitCriticalSection (Lock);
            return Not_In_Pool;
        end if;
        Sealed := True;
        if Pool = null then
            -- No pool was declared, and none can be from now on.
            Spinlocks.exitCriticalSection (Lock);
            return Not_In_Pool;
        end if;
        Sets.Add_Claim (Pool.all, Pages, Claimant, Status);
        case Status is
            when Sets.Duplicate | Sets.Outside_Members =>
                Outcome := Not_In_Pool;
            when Sets.Claims_Full =>
                Put_Pages ("platform-pool: refused claim phys=", Pages);
                print (" claimant=");
                print (Claimant);
                print (" claims=");
                print (Unsigned_64 (Pool.Claims_Room));
                println (" why=CAPACITY");
                Outcome := Refused_Capacity;
            when Sets.Recorded =>
                -- Revoked_From_Firmware when it covers any Device_BAR member
                -- (its mappings there go), else Shared_With_Firmware.
                Outcome := Shared_With_Firmware;
                for I in 1 .. Pool.Member_Total loop
                    if Pool.Members (I).Kind = Sets.Device_BAR
                      and then Sets.Overlaps (Pages, Pool.Members (I).Pages)
                    then
                        Outcome := Revoked_From_Firmware;
                    end if;
                end loop;
                Log_Shared (Claimant, Pages);
                Revoke_Conflicting (Claimant, Pages);
        end case;
        Spinlocks.exitCriticalSection (Lock);
        return Outcome;
    end Claim;

    function Delegating
      (Target : Process.ProcessID; Copy : Capabilities.Capability) return Claim_Outcome is
    begin
        if Target /= Process.NO_PROCESS
          and then Copy.capType = Capabilities.CAP_DEVICE_MEM
          and then Copy.authorityTag = Capabilities.NO_AUTHORITY_TAG
        then
            return Claim (Identity_Word (Target), Copy.object.ref, Copy.object.param);
        end if;
        return Not_In_Pool;
    end Delegating;

    function Mapping_Into
      (Target : Process.ProcessID; Physical, Bytes : Unsigned_64) return Claim_Outcome is
    begin
        if Target /= Process.NO_PROCESS then
            return Claim (Identity_Word (Target), Physical, Bytes);
        end if;
        return Not_In_Pool;
    end Mapping_Into;

    ---------------------------------------------------------------------------
    -- Mapping
    ---------------------------------------------------------------------------

    -- Caller's mapping table, created (one page) when it has none; False
    -- when its quota or memory refuses. Under Lock.
    procedure Ensure_Table (Caller : Process.ProcessID; Holder : Unsigned_64; OK : out Boolean) is
        H : Holder_Record renames Holders (Caller);
        B : Block;
    begin
        if H.Table /= null and then H.Id /= Holder then
            Release (H.Storage);   -- a previous incarnation's, never reclaimed
            H := (others => <>);
        end if;
        OK := H.Table /= null;
        if OK then
            return;
        end if;
        Allocate (Process.proctab (Caller).memoryAccount, Page_Bytes, B, OK);
        if OK then
            H := (Id => Holder, Storage => B, Table => New_Table (B, Room_Of (B.Order)));
        end if;
    end Ensure_Table;

    -- Caller's table, twice the size, its mappings copied over; False when
    -- it is at the ceiling or its quota or memory refuses. Under Lock.
    procedure Grow_Table (Caller : Process.ProcessID; OK : out Boolean) is
        H : Holder_Record renames Holders (Caller);
        B : Block;
        Grown : Table_Pointers.Object_Pointer;
    begin
        OK := False;
        if H.Table.Room >= Sets.Mapping_Ceiling or else H.Storage.Order = BuddyAllocator.Order'Last then
            return;
        end if;
        Allocate (Process.proctab (Caller).memoryAccount,
                  BuddyAllocator.blockSize (H.Storage.Order + 1), B, OK);
        if not OK then
            return;
        end if;
        Grown := New_Table (B, Room_Of (B.Order));
        Sets.Copy (H.Table.all, Grown.all);
        Release (H.Storage);
        H.Storage := B;
        H.Table := Grown;
    end Grow_Table;

    procedure Map
      (Caller    : Process.ProcessID;
       Authority : Capabilities.Capability;
       Physical, Virtual, Pages : Unsigned_64;
       Read_Only : Boolean;
       Mapped    : out Boolean;
       Why       : out Platform_Pool_ABI.Refusal;
       Record_Out : out Platform_Pool_ABI.Refusal_Record)
    is
        procedure mapPageInst is new Virtmem.mapPage (BuddyAllocator.allocFrame);

        Holder  : constant Unsigned_64 := Identity_Word (Caller);
        Bytes   : Unsigned_64;
        Request : Sets.Interval;
        Valid   : Boolean;
        Choice  : Sets.Decision;
        Added   : Sets.Add_Status;
        Table_Ok : Boolean;
        Page_Ok : Boolean;
        All_Ok  : Boolean := True;
        Member  : Sets.Member;
        Sharer  : Unsigned_64 := Sets.No_Identity;
        Mode    : constant Device_Memory_Admission.Access_Mode :=
          (if Read_Only then Device_Memory_Admission.Read_Only
           else Device_Memory_Admission.Read_Write);

        procedure Refuse (Reason : ABI.Refusal) is
        begin
            Why := Reason;
            Record_Out (ABI.Code) := ABI.Refusal_Word (Reason);
            print ("platform-pool: refused phys=");
            print (Physical);
            print (" pages=");
            print (Pages);
            print (" holder=");
            print (Holder);
            print (" why=");
            print (ABI.Name (Reason));
            if Reason = ABI.Claimed then
                print (" claimant=");
                print (Record_Out (ABI.Claimant));
                print (" claim=");
                print (Record_Out (ABI.Claim_First_Page) * Page_Bytes);
                print (" claim-bytes=");
                print (Record_Out (ABI.Claim_Pages) * Page_Bytes);
            end if;
            println;
        end Refuse;
    begin
        Mapped := False;
        Why := ABI.Invalid_Request;
        Record_Out := [others => 0];
        Record_Out (ABI.Holder) := Holder;

        if Pages = 0 or else Pages > ABI.Maximum_Pages
          or else Physical mod Page_Bytes /= 0
          or else Virtual = 0 or else Virtual mod Page_Bytes /= 0
          or else Virtual >= User_Address_Limit
          or else Pages * Page_Bytes > User_Address_Limit - Virtual
        then
            Refuse (ABI.Invalid_Request);
            return;
        end if;
        Bytes := Pages * Page_Bytes;
        Pages_Of (Physical, Bytes, Request, Valid);
        if not Valid then
            Refuse (ABI.Invalid_Request);
            return;
        end if;
        -- The capability's own range and rights bound the request; the pool
        -- bounds it further. The pages are device memory (re-checked: a
        -- member is device memory when it is inserted).
        if Authority.capType /= Capabilities.CAP_DEVICE_MEM
          or else Authority.authorityTag /= Capabilities.PLATFORM_POOL_TAG
          or else not Device_Memory_Admission.Covers
                    (Authority.object.ref, Authority.object.param, Physical, Bytes)
          or else not Device_Memory_Admission.Allows
                    (Authority.rights (Capabilities.RIGHT_READ),
                     Authority.rights (Capabilities.RIGHT_WRITE), Mode)
          or else not Physical_Ranges.Pool_Mappable_Bytes
                        (Physical_Range_Map.Kernel_Map, Physical, Bytes)
        then
            Refuse (ABI.Not_Covered);
            return;
        end if;

        Spinlocks.enterCriticalSection (Lock);
        if Pool = null then
            -- Nothing was declared: no page is in the pool.
            Spinlocks.exitCriticalSection (Lock);
            Refuse (ABI.Outside_Pool);
            return;
        end if;
        -- From the first mapping on the members are fixed.
        Sealed := True;
        Choice := Sets.Decide (Pool.all, Holder, Request);
        case Choice.Kind is
            when Sets.Admitted =>
                Member := Pool.Members (Choice.Member);
                if Choice.Claim /= 0 then
                    Sharer := Pool.Claims (Choice.Claim).Claimant;
                end if;
            when Sets.Claimed =>
                Record_Out (ABI.Claimant) := Pool.Claims (Choice.Claim).Claimant;
                Record_Out (ABI.Claim_First_Page) := Pool.Claims (Choice.Claim).Pages.First;
                Record_Out (ABI.Claim_Pages) :=
                  Pool.Claims (Choice.Claim).Pages.Last - Pool.Claims (Choice.Claim).Pages.First + 1;
                Record_Out (ABI.Member_First_Page) := Pool.Members (Choice.Member).Pages.First;
                Record_Out (ABI.Member_Pages) :=
                  Pool.Members (Choice.Member).Pages.Last
                  - Pool.Members (Choice.Member).Pages.First + 1;
                Spinlocks.exitCriticalSection (Lock);
                Refuse (ABI.Claimed);
                return;
            when Sets.Outside_Pool =>
                Spinlocks.exitCriticalSection (Lock);
                Refuse (ABI.Outside_Pool);
                return;
            when Sets.Invalid_Request =>
                Spinlocks.exitCriticalSection (Lock);
                Refuse (ABI.Invalid_Request);
                return;
        end case;

        -- Recorded before the pages are mapped, under the same lock a claim
        -- takes: a claim either finds the record or comes after the mapping
        -- is refused. The holder's table grows as it maps (its quota).
        Ensure_Table (Caller, Holder, Table_Ok);
        Added := Sets.Table_Full;
        if Table_Ok then
            declare
                Item : constant Sets.Mapping :=
                  (Pages => Request, Holder => Holder,
                   Virtual_Page => Virtual / Page_Bytes, Kind => Member.Kind);
            begin
                Sets.Add_Mapping (Pool.all, Holders (Caller).Table.all, Item, Added);
                if Added = Sets.Table_Full then
                    Grow_Table (Caller, Table_Ok);
                    if Table_Ok then
                        Sets.Add_Mapping (Pool.all, Holders (Caller).Table.all, Item, Added);
                    end if;
                end if;
            end;
        end if;
        if Added = Sets.Table_Full then
            Spinlocks.exitCriticalSection (Lock);
            Refuse (ABI.Mapping_Capacity);
            return;
        end if;

        Process.Owned_Memory.Lock;
        if Process.Owned_Memory.Physical_Conflict (Physical, Bytes) then
            Process.Owned_Memory.Unlock;
            Spinlocks.exitCriticalSection (Lock);
            Refuse (ABI.Not_Covered);
            return;
        end if;
        Process.lockAddressSpace (Caller);
        for I in 0 .. Pages - 1 loop
            mapPageInst
              (phys    => Virtmem.PhysAddress (Physical + I * Page_Bytes),
               virt    => Integer_Address (Virtual + I * Page_Bytes),
               flags   => (if Read_Only then Virtmem.PG_USERIO and not Virtmem.PG_WRITABLE
                           else Virtmem.PG_USERIO),
               myP4    => Process.addrtab (Caller),
               success => Page_Ok);
            All_Ok := All_Ok and then Page_Ok;
        end loop;
        Process.unlockAddressSpace (Caller);
        Process.Owned_Memory.Unlock;
        Spinlocks.exitCriticalSection (Lock);

        if not All_Ok then
            -- The record stays: a later claim still removes what was mapped.
            Refuse (ABI.Mapping_Failed);
            return;
        end if;
        Mapped := True;
        if Added = Sets.Added then
            -- First mapping of this region by this holder at this address:
            -- the profile line (the backend adds the AML name).
            print ("platform-pool: mapped phys=");
            print (Physical);
            print (" bytes=");
            print (Bytes);
            print (" holder=");
            print (Holder);
            print (" member=");
            print (Member.Pages.First * Page_Bytes);
            print (" member-bytes=");
            print ((Member.Pages.Last - Member.Pages.First + 1) * Page_Bytes);
            print (" kind=");
            print (Kind_Name (Member.Kind));
            if Sharer /= Sets.No_Identity then
                print (" shared-with=");
                print (Sharer);
            end if;
            println (if Read_Only then " mode=RO" else " mode=RW");
        end if;
    end Map;

    procedure Release_Process (Pid : Process.ProcessID) is
        Id : constant Unsigned_64 := Identity_Word (Pid);
    begin
        Spinlocks.enterCriticalSection (Lock);
        if Pool /= null then
            Sets.Release_Claimant (Pool.all, Id);
        end if;
        Release (Holders (Pid).Storage);
        Holders (Pid) := (others => <>);
        Spinlocks.exitCriticalSection (Lock);
    end Release_Process;

end Platform_Pool;
