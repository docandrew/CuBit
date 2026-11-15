-------------------------------------------------------------------------------
--  CuBitOS
--  Copyright (C) 2026 Jon Andrew
--
--  Legacy interrupt lines - implementation
-------------------------------------------------------------------------------
with InterruptNumbers;
with ioapic;
with Pic;
with Spinlocks;
with TextIO; use TextIO;

package body Interrupt_Lines with
    SPARK_Mode => Off   -- MMIO controller instantiation and the spinlock
is
    package Policy renames Interrupt_Line_Policy;
    use type Policy.Hardware_Action;
    use type Policy.Route_Result;
    use type ABI.Trigger_Mode;
    use type ABI.Pin_Polarity;
    use type System.Address;

    type Line_Table is array (Line_Vector) of Policy.Line_State;

    Lines      : Line_Table;
    Lines_Lock : Spinlocks.Spinlock;

    --  The driven I/O APIC; Null_Address in PIC mode.
    Controller_Base : System.Address := System.Null_Address;
    --  Whether its inputs are GSIs 0 .., i.e. the legacy lines.
    Serves_Lines    : Boolean := False;
    --  How many inputs it has (0 until Set_IO_APIC).
    Input_Count     : Natural := 0;

    ---------------------------------------------------------------------------
    --  The I/O APIC operations, under Lines_Lock.
    ---------------------------------------------------------------------------
    procedure Program
      (Vector : Line_Vector; CPU : ABI.Route_CPU;
       Trigger : ABI.Trigger_Mode; Polarity : ABI.Pin_Polarity;
       Masked : Boolean)
    is
        package Controller is new ioapic (Controller_Base);
    begin
        Controller.enableIRQ
          (InterruptNumbers.x86Interrupt (Vector), Unsigned_32 (CPU),
           levelTriggered => Trigger = ABI.Level,
           activeLow      => Polarity = ABI.Active_Low,
           masked         => Masked);
    end Program;

    procedure Set_Mask (Vector : Line_Vector; Masked : Boolean) is
    begin
        if Controller_Base /= System.Null_Address then
            declare
                package Controller is new ioapic (Controller_Base);
            begin
                Controller.setMasked (InterruptNumbers.x86Interrupt (Vector), Masked);
            end;
        end if;
    end Set_Mask;

    procedure Apply (Vector : Line_Vector; Action : Policy.Hardware_Action) is
    begin
        case Action is
            when Policy.Mask_Input   => Set_Mask (Vector, True);
            when Policy.Unmask_Input => Set_Mask (Vector, False);
            when Policy.No_Change | Policy.Program_Unmasked | Policy.Program_Masked =>
                null;   --  Route programs the whole entry itself
        end case;
    end Apply;

    ---------------------------------------------------------------------------
    -- Set_IO_APIC
    ---------------------------------------------------------------------------
    procedure Set_IO_APIC (Base : System.Address; GSI_Base : Unsigned_32) is
        package Controller is new ioapic (Base);
    begin
        Spinlocks.enterCriticalSection (Lines_Lock);
        Controller_Base := Base;
        Serves_Lines := GSI_Base = 0;
        Input_Count := Controller.redirectionEntries;
        Spinlocks.exitCriticalSection (Lines_Lock);
        print ("Interrupt lines: I/O APIC inputs");
        print (Unsigned_32 (Input_Count));
        print (", first GSI");
        println (GSI_Base);
        if not Serves_Lines then
            println (" WARNING: the driven I/O APIC does not serve GSI 0; legacy lines refused");
        end if;
    end Set_IO_APIC;

    ---------------------------------------------------------------------------
    -- Route
    ---------------------------------------------------------------------------
    procedure Route
      (Vector   : Line_Vector;
       Owner    : Owner_Word;
       CPU      : ABI.Route_CPU;
       Trigger  : ABI.Trigger_Mode;
       Polarity : ABI.Pin_Polarity;
       Outcome  : out ABI.Route_Outcome)
    is
        Result : Policy.Route_Result;
        Action : Policy.Hardware_Action;
    begin
        Spinlocks.enterCriticalSection (Lines_Lock);
        if Controller_Base = System.Null_Address then
            --  The 8259: edge lines only (no per-line hold across an
            --  acknowledgement here).
            if Trigger = ABI.Level then
                Outcome := ABI.Level_Needs_IO_APIC;
            else
                Policy.Route (Lines (Vector), Owner, Trigger, Polarity, Result, Action);
                case Result is
                    when Policy.Routed =>
                        Pic.enableIRQ (InterruptNumbers.x86Interrupt (Vector));
                        Outcome := ABI.Routed;
                    when Policy.Owners_Full   => Outcome := ABI.Owners_Full;
                    when Policy.Mode_Conflict => Outcome := ABI.Mode_Conflict;
                end case;
            end if;
        elsif not Serves_Lines then
            Outcome := ABI.No_Controller;
        elsif ABI.Input_Of (Vector) >= Input_Count then
            Outcome := ABI.Input_Absent;
        else
            Policy.Route (Lines (Vector), Owner, Trigger, Polarity, Result, Action);
            case Result is
                when Policy.Routed =>
                    Program (Vector, CPU, Trigger, Polarity,
                             Masked => Action = Policy.Program_Masked);
                    Outcome := ABI.Routed;
                when Policy.Owners_Full   => Outcome := ABI.Owners_Full;
                when Policy.Mode_Conflict => Outcome := ABI.Mode_Conflict;
            end case;
        end if;
        Spinlocks.exitCriticalSection (Lines_Lock);
    end Route;

    ---------------------------------------------------------------------------
    -- Deliver
    ---------------------------------------------------------------------------
    procedure Deliver (Vector : Line_Vector; Recipients : out Recipient_List) is
        Action   : Policy.Hardware_Action;
        Dispatch : Boolean;
    begin
        Spinlocks.enterCriticalSection (Lines_Lock);
        Policy.Deliver (Lines (Vector), Action, Dispatch);
        Apply (Vector, Action);
        if Dispatch then
            Recipients := Recipient_List (Lines (Vector).Owners);
        else
            Recipients := (others => Policy.No_Owner);
        end if;
        Spinlocks.exitCriticalSection (Lines_Lock);
    end Deliver;

    ---------------------------------------------------------------------------
    -- Acknowledge
    ---------------------------------------------------------------------------
    procedure Acknowledge
      (Vector    : Line_Vector;
       Owner     : Owner_Word;
       Accepted  : out Boolean;
       Delivered : out ABI.Delivery_Count)
    is
        Action : Policy.Hardware_Action;
    begin
        Spinlocks.enterCriticalSection (Lines_Lock);
        Policy.Acknowledge (Lines (Vector), Owner, Accepted, Action);
        Apply (Vector, Action);
        Delivered := Lines (Vector).Delivered;
        Spinlocks.exitCriticalSection (Lines_Lock);
    end Acknowledge;

    ---------------------------------------------------------------------------
    -- Release_Process
    ---------------------------------------------------------------------------
    procedure Release_Process (Owner : Owner_Word) is
        Action : Policy.Hardware_Action;
    begin
        Spinlocks.enterCriticalSection (Lines_Lock);
        for Vector in Line_Vector loop
            Policy.Release (Lines (Vector), Owner, Action);
            Apply (Vector, Action);
        end loop;
        Spinlocks.exitCriticalSection (Lines_Lock);
    end Release_Process;

end Interrupt_Lines;
