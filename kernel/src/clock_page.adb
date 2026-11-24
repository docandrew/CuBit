-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- The kernel's side of the clock publication (clock_page.ads).
-------------------------------------------------------------------------------
with System;
with BuddyAllocator;
with Clock_Publication;
with Util;

package body Clock_Page with
    SPARK_Mode => Off
is
    package CP renames Clock_Publication;
    use type Virtmem.PhysAddress;

    Frame : Virtmem.PhysAddress := 0;
    Kernel_Copy : CP.Parameters := CP.Not_Published;
    Converting_State : Boolean := False;
    Shared_State : Boolean := False;

    function Converting return Boolean is (Converting_State);
    function Shared return Boolean is (Shared_State);

    -- The seqlock writer: odd while the fields change. Before the first
    -- process exists nobody reads it, but a later rebase would use this
    -- same sequence (the new base counter sampled after the opening store).
    procedure Write (Fields : CP.Parameters) is
        Target : CP.Page with
            Import, Volatile, Address => Virtmem.P2Va (Frame);
    begin
        Target.Sequence := CP.Opened (Target.Sequence);
        Target.Fields := Fields;
        Target.Sequence := CP.Closed (Target.Sequence);
    end Write;

    procedure Publish (Frequency         : Unsigned_64;
                       Base_Ticks        : Unsigned_64;
                       Base_Milliseconds : Unsigned_64;
                       Invariant         : Boolean)
    is
        Ignore : System.Address;
    begin
        if Frame /= 0 then
            raise Program_Error with "Clock publication published twice";
        end if;
        BuddyAllocator.allocFrame (Frame);
        if Frame = 0 then
            raise Program_Error with "No frame for the clock publication";
        end if;
        Ignore := Util.memset (Virtmem.P2Va (Frame), 0, CP.Page_Bytes);
        if Frequency in CP.Counter_Frequency and then
           Base_Milliseconds <= CP.Maximum_Base / CP.Nanoseconds_Per_Millisecond
        then
            Kernel_Copy := CP.Initial
              (Frequency, Base_Ticks,
               Base_Milliseconds * CP.Nanoseconds_Per_Millisecond);
            Converting_State := True;
            Shared_State := Invariant;
            if Invariant then
                Write (Kernel_Copy);
            end if;
        end if;
        Note_Milliseconds (Base_Milliseconds);
    end Publish;

    procedure Withdraw_Sharing is
    begin
        if not Shared_State then
            return;
        end if;
        Shared_State := False;
        Write (CP.Not_Published);
    end Withdraw_Sharing;

    procedure Note_Milliseconds (Milliseconds : Unsigned_64) is
    begin
        if Frame = 0 then
            return;
        end if;
        declare
            Word : Unsigned_64 with Import, Volatile, Atomic,
                Address => Virtmem.P2Va (Frame + CP.Milliseconds_Offset);
        begin
            Word := Milliseconds;
        end;
    end Note_Milliseconds;

    procedure Read (Counter     : Unsigned_64;
                    Nanoseconds : out Unsigned_64;
                    Success     : out Boolean)
    is
    begin
        CP.Convert (Kernel_Copy, Counter, Nanoseconds, Success);
    end Read;

    procedure Map (Root : in out Virtmem.P4; Success : out Boolean) is
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        Success := False;
        if Frame = 0 then
            return;
        end if;
        mapPage (Frame, CP.Page_Address, Virtmem.PG_USERDATARO, Root, Success);
    end Map;
end Clock_Page;
