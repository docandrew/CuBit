-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary System call census (ABI-002): which processes call which calls
-------------------------------------------------------------------------------
with System.Machine_Code; use System.Machine_Code;
with Process;
with Process_Identities;
with Spinlocks;
with TextIO; use TextIO;
with Time;

package body Syscall_Census is
    Report_Interval : constant Time.Duration := 1 * Time.Seconds;

    type Count_Table is array (Slot, Call) of aliased Unsigned_32
        with Volatile_Components;
    type Number_Table is array (Call) of Unsigned_64 with Atomic_Components;
    type Flags is array (Slot) of Boolean with Atomic_Components;

    Counts  : Count_Table := (others => (others => 0));
    -- The raw number of each call index, learnt as calls arrive.
    Numbers : Number_Table := (others => 0);
    Dirty   : Flags := (others => False);

    procedure Note (S : Slot; C : Call; Number : Unsigned_64) is
    begin
        Asm ("lock incl %0",
             Outputs => Unsigned_32'Asm_Output ("+m", Counts (S, C)),
             Volatile => True, Clobber => "memory, cc");
        Numbers (C) := Number;
        Dirty (S) := True;
    end Note;

    -- One line, built in a buffer and printed with one println so other
    -- CPUs' output cannot split it. One buffer (kernel stacks are small),
    -- shared by the reporter and the reaper under Line_Lock.
    Line_Length : constant := 4096;
    Decimal_Width : constant := 20;
    Line : String (1 .. Line_Length);
    Last : Natural := 0;
    Line_Lock : Spinlocks.Spinlock;

    procedure Report (S : Slot; Final : Boolean) is
        Any  : Boolean := False;

        procedure Put (Text : String) is
        begin
            if Last + Text'Length <= Line'Last then
                Line (Last + 1 .. Last + Text'Length) := Text;
                Last := Last + Text'Length;
            end if;
        end Put;

        procedure Put (Value : Unsigned_64) is
            Digits_Of : String (1 .. Decimal_Width);
            First : Natural := Digits_Of'Last + 1;
            V : Unsigned_64 := Value;
        begin
            loop
                First := First - 1;
                Digits_Of (First) := Character'Val (Character'Pos ('0') + Natural (V mod 10));
                V := V / 10;
                exit when V = 0;
            end loop;
            Put (Digits_Of (First .. Digits_Of'Last));
        end Put;

        Name : constant Process.ProcessName := Process.proctab (S).name;
    begin
        Spinlocks.enterCriticalSection (Line_Lock);
        Last := 0;
        Put ("CENSUS: id=");
        Put (Process_Identities.To_Word (Process.identityOf (S)));
        Put (" name=");
        for Ch of Name loop
            Put ((1 => (if Ch in ' ' | ASCII.NUL then '_' else Ch)));
        end loop;
        Put ((if Final then " final=1" else " final=0"));
        for C in Call loop
            declare
                N : constant Unsigned_32 := Counts (S, C);
            begin
                if N /= 0 then
                    Any := True;
                    Put (" ");
                    Put (Numbers (C));
                    Put ("=");
                    Put (Unsigned_64 (N));
                end if;
            end;
        end loop;
        if Any then
            println (Line (1 .. Last));
        end if;
        Spinlocks.exitCriticalSection (Line_Lock);
    end Report;

    procedure Report_Exit (S : Slot) is
    begin
        Dirty (S) := False;
        Report (S, Final => True);
        for C in Call loop
            Counts (S, C) := 0;
        end loop;
    end Report_Exit;

    -- Each round starts with the uptime, so a tool can turn the cumulative
    -- counts into rates: CENSUS: round ms=<Time.msTicks>.
    procedure Report_Round is
    begin
        Spinlocks.enterCriticalSection (Line_Lock);
        Last := 0;
        declare
            Value : Unsigned_64 := Time.msTicks;
            Digits_Of : String (1 .. Decimal_Width);
            First : Natural := Digits_Of'Last + 1;
            Text : constant String := "CENSUS: round ms=";
        begin
            loop
                First := First - 1;
                Digits_Of (First) := Character'Val (Character'Pos ('0') + Natural (Value mod 10));
                Value := Value / 10;
                exit when Value = 0;
            end loop;
            Line (1 .. Text'Length) := Text;
            Last := Text'Length + Digits_Of'Last - First + 1;
            Line (Text'Length + 1 .. Last) := Digits_Of (First .. Digits_Of'Last);
        end;
        println (Line (1 .. Last));
        Spinlocks.exitCriticalSection (Line_Lock);
    end Report_Round;

    procedure Reporter with No_Return is
    begin
        loop
            Process.sleep (Report_Interval);
            Report_Round;
            for S in Slot loop
                if Dirty (S) then
                    Dirty (S) := False;
                    Report (S, Final => False);
                end if;
            end loop;
        end loop;
    end Reporter;

    procedure Start is
    begin
        Process.startWorker (Reporter'Address, "Syscall census  ");
    end Start;
end Syscall_Census;
