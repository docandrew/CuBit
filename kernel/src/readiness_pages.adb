pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with BuddyAllocator;
with Util;

package body Readiness_Pages is
    use type Virtmem.PhysAddress;

    procedure Create (Root     : in out Virtmem.P4;
                      Identity : Unsigned_64;
                      Frame    : out Virtmem.PhysAddress;
                      Counts   : out Count_Frames) is
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
        Mapped : Boolean;
        Ignore : System.Address;
    begin
        Counts := No_Count_Frames;
        BuddyAllocator.allocFrame (Frame);
        if Frame = No_Frame then
            return;
        end if;
        Ignore := Util.memset (Virtmem.P2Va (Frame), 0, RP.Page_Bytes);
        declare
            Page : RP.Readiness_Page with Import, Volatile, Address => Virtmem.P2Va (Frame);
        begin
            Page.Version := RP.Layout_Version;
            Page.Identity := Identity;
        end;
        mapPage (Frame, RP.Page_Address, Virtmem.PG_USERDATARO, Root, Mapped);
        for P in Count_Page loop
            exit when not Mapped;
            BuddyAllocator.allocFrame (Counts (P));
            Mapped := Counts (P) /= No_Frame;
            if Mapped then
                Ignore := Util.memset (Virtmem.P2Va (Counts (P)), 0, RP.Page_Bytes);
                mapPage (Counts (P),
                         RP.Thread_Counts_Address + Virtmem.VirtAddress (P) * RP.Page_Bytes,
                         Virtmem.PG_USERDATARO, Root, Mapped);
            end if;
        end loop;
        if not Mapped then
            Release (Frame, Counts);
        end if;
    end Create;

    procedure Release (Frame  : in out Virtmem.PhysAddress;
                       Counts : in out Count_Frames) is
    begin
        if Frame /= No_Frame then
            BuddyAllocator.freeFrame (Frame);
            Frame := No_Frame;
        end if;
        for C of Counts loop
            if C /= No_Frame then
                BuddyAllocator.freeFrame (C);
                C := No_Frame;
            end if;
        end loop;
    end Release;

    function Line (Counts : Count_Frames; Slot : RP.Thread_Slot)
      return System.Address is
      (Virtmem.P2Va (Counts (Count_Page (RP.Page_Of (Slot)))) +
         System.Storage_Elements.Storage_Offset (RP.Offset_Of (Slot)));

    procedure Clear_Line (Counts : Count_Frames; Slot : RP.Thread_Slot) is
        Ignore : System.Address;
    begin
        Ignore := Util.memset (Line (Counts, Slot), 0, RP.Slot_Bytes);
    end Clear_Line;

    procedure Set_Owned (Frame : Virtmem.PhysAddress; Bytes : Unsigned_64) is
    begin
        if Frame = No_Frame then
            return;
        end if;
        declare
            Page : RP.Readiness_Page with Import, Address => Virtmem.P2Va (Frame);
            Word : Unsigned_64 with Import, Atomic, Address => Page.Owned_Bytes'Address;
        begin
            Word := Bytes;
        end;
    end Set_Owned;

    procedure Note_Drop (Frame : Virtmem.PhysAddress) is
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
                           RP.Count'Asm_Output ("+m", Page.Event_Drops)),
               Clobber => "memory, cc", Volatile => True);
        end;
    end Note_Drop;

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
