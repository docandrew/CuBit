pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Machine_Code;
with BuddyAllocator;
with Util;

package body Readiness_Pages is
    use type Virtmem.PhysAddress;

    procedure Create (Root : in out Virtmem.P4; Frame : out Virtmem.PhysAddress) is
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
        Mapped : Boolean;
        Ignore : System.Address;
    begin
        BuddyAllocator.allocFrame (Frame);
        if Frame = No_Frame then
            return;
        end if;
        Ignore := Util.memset (Virtmem.P2Va (Frame), 0, RP.Page_Bytes);
        declare
            Page : RP.Readiness_Page with Import, Volatile, Address => Virtmem.P2Va (Frame);
        begin
            Page.Version := RP.Layout_Version;
        end;
        mapPage (Frame, RP.Page_Address, Virtmem.PG_USERDATARO, Root, Mapped);
        if not Mapped then
            BuddyAllocator.freeFrame (Frame);
            Frame := No_Frame;
        end if;
    end Create;

    procedure Release (Frame : in out Virtmem.PhysAddress) is
    begin
        if Frame /= No_Frame then
            BuddyAllocator.freeFrame (Frame);
            Frame := No_Frame;
        end if;
    end Release;

    -- One locked add: a class may be noted under different locks (events
    -- under a mailbox lock, and grant notices under grantLock), so the
    -- increment itself is atomic. 2**64 entries are never reached.
    procedure Note (Frame : Virtmem.PhysAddress; Class : RP.Queue_Class) is
    begin
        if Frame = No_Frame then
            return;
        end if;
        declare
            Page : RP.Readiness_Page with Import, Address => Virtmem.P2Va (Frame);
            One : RP.Count := 1;
        begin
            System.Machine_Code.Asm
              ("lock xaddq %0, %1",
               Outputs => (RP.Count'Asm_Output ("+r", One),
                           RP.Count'Asm_Output ("+m", Page.Produced (Class))),
               Clobber => "memory, cc", Volatile => True);
        end;
    end Note;

    procedure Note_Wake (Frame : Virtmem.PhysAddress) is
    begin
        if Frame = No_Frame then
            return;
        end if;
        declare
            Page : RP.Readiness_Page with Import, Address => Virtmem.P2Va (Frame);
            Counter : RP.Count with Import, Atomic, Address => Page.Wake_Generation'Address;
        begin
            Counter := RP.Next (Counter);
        end;
    end Note_Wake;
end Readiness_Pages;
