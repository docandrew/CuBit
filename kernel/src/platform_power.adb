pragma Ada_2022;
with Interfaces; use Interfaces;
with Platform_Power_ABI;
with Firmware_Tables.Power_Registers;
with acpi;
with Platform_Power_Policy;
with CPU_Parking;
with PerCpuData;
with TextIO;
with Time;
with System.Machine_Code;
with x86;

package body Platform_Power is
    package ABI renames Platform_Power_ABI;
    package Pol renames Platform_Power_Policy;
    package PR  renames Firmware_Tables.Power_Registers;
    use type ABI.Power_State, CPU_Parking.Outcome, PR.Register_Space;

    function registers return PR.Description is (acpi.powerRegisters);

    Decimal_Digits : constant := 10;
    Radix          : constant := 10;

    function Decimal (Value : Natural) return String is
        Result : String (1 .. Decimal_Digits) := (others => '0');
        V : Natural := Value;
        First : Positive := Result'Last;
    begin
        for I in reverse Result'Range loop
            Result (I) := Character'Val (Character'Pos ('0') + V mod Radix);
            V := V / Radix;
            First := I;
            exit when V = 0;
        end loop;
        return Result (First .. Result'Last);
    end Decimal;

    procedure Pause is
    begin
        System.Machine_Code.Asm ("pause", Volatile => True);
    end Pause;

    function Port (R : PR.Register) return x86.IOPort is
      (x86.IOPort (R.Address))
    with Pre => R.Present and then R.Space = PR.System_IO
                and then R.Address <= PR.Port_Space_Last;

    function Answer (Request : Process.Message; Why : ABI.Refusal) return Process.Message is
        Reply : Process.Message := Process.NULL_MESSAGE;
    begin
        Reply.tag.label := Request.tag.label;
        Reply.tag.length := ABI.Reply_Words;
        Reply.words (ABI.Outcome_Word) := ABI.Refusal_Word (Why);
        return Reply;
    end Answer;

    procedure Announce (Item : Pol.Request) is
    begin
        if Item.State = ABI.Reset then
            TextIO.println ("platform power: reset (FADT reset register)");
        else
            TextIO.println ("platform power: entering S5 (SLP_TYPa " &
                            Decimal (Natural (Item.Type_A)) & ", SLP_TYPb " &
                            Decimal (Natural (Item.Type_B)) & ")");
        end if;
    end Announce;

    -- Wait for the transition to take effect; False when the bound passed.
    procedure Settle is
        Started, Now : Unsigned_64;
        Clock : Boolean;
    begin
        Time.Read_Monotonic (Started, Clock);
        for Poll in 1 .. Settle_Polls_Without_Clock loop
            if Clock then
                Time.Read_Monotonic (Now, Clock);
                exit when Clock and then Now - Started >= Settle_Microseconds;
            end if;
            Pause;
        end loop;
    end Settle;

    procedure Perform (Item : Pol.Request; Steps : Pol.Plan_Type; Why : out ABI.Refusal) is
        Self : constant Natural := PerCpuData.getCPUNumber;
        Saved_A, Saved_B : Unsigned_16 := 0;
        Parking : CPU_Parking.Outcome;
        Online_CPUs : Natural := 0;
    begin
        Why := ABI.Did_Not_Take_Effect;
        if Self > CPU_Parking.CPU_Index'Last then
            Why := ABI.In_Progress;
            return;
        end if;
        for I in 1 .. Steps.Count loop
            case Steps.Steps (I) is
                when Pol.Drain_Console =>
                    -- Queued before the call, then this line: all of it
                    -- reaches the console before any register is written
                    -- (LOG-009). Output is direct from here on.
                    Announce (Item);
                    TextIO.stopAsynchronous;
                when Pol.Park_CPUs =>
                    CPU_Parking.Park_Others
                      (CPU_Parking.CPU_Index (Self), Process.cpuOnline, Parking);
                    if Parking = CPU_Parking.Busy then
                        TextIO.startAsynchronous;
                        Why := ABI.In_Progress;
                        return;
                    end if;
                    for C in CPU_Parking.CPU_Index loop
                        if Process.cpuOnline (C) and then C /= CPU_Parking.CPU_Index (Self) then
                            Online_CPUs := Online_CPUs + 1;
                        end if;
                    end loop;
                    TextIO.println ("platform power: " &
                                    Decimal (CPU_Parking.Parked_Count) & " of " &
                                    Decimal (Online_CPUs) & " other CPUs parked");
                when Pol.Read_A =>
                    x86.in16 (Port (registers.PM1A_Control), Saved_A);
                when Pol.Read_B =>
                    x86.in16 (Port (registers.PM1B_Control), Saved_B);
                when Pol.Stage_B =>
                    x86.out16 (Port (registers.PM1B_Control),
                               Pol.PM1_Value (Saved_B, Item.Type_B, Enable => False));
                when Pol.Stage_A =>
                    x86.out16 (Port (registers.PM1A_Control),
                               Pol.PM1_Value (Saved_A, Item.Type_A, Enable => False));
                when Pol.Enable_B =>
                    x86.out16 (Port (registers.PM1B_Control),
                               Pol.PM1_Value (Saved_B, Item.Type_B, Enable => True));
                when Pol.Enable_A =>
                    x86.out16 (Port (registers.PM1A_Control),
                               Pol.PM1_Value (Saved_A, Item.Type_A, Enable => True));
                when Pol.Reduced_Enable =>
                    x86.out8 (Port (registers.Sleep_Control), Pol.Reduced_Value (Item.Type_A));
                when Pol.Write_Reset =>
                    x86.out8 (Port (registers.Reset), registers.Reset_Value);
                when Pol.Settle =>
                    Settle;
                    -- Still here: the machine did not go off.
                    CPU_Parking.Release;
                    TextIO.startAsynchronous;
                    TextIO.println ("platform power: still running after the write; CPUs released");
                    Why := ABI.Did_Not_Take_Effect;
            end case;
        end loop;
    end Perform;

    procedure Call (Writable : Boolean;
                    Request  : Process.Message;
                    Reply    : out Process.Message)
    is
        Item : constant Pol.Request :=
          Pol.Decode (Writable, Request.tag.label, Request.tag.length,
                      Request.words (ABI.State_Word), Request.words (ABI.Type_A_Word),
                      Request.words (ABI.Type_B_Word), Request.words (ABI.Unused_Word));
        Admitted : Pol.Admission;
        Why : ABI.Refusal;
    begin
        if not Item.Valid then
            Reply := Answer (Request, Item.Why);
            return;
        end if;
        Admitted := Pol.Admit (Item, registers);
        if not Admitted.Admitted then
            Reply := Answer (Request, Admitted.Why);
            return;
        end if;
        PerCpuData.pushCLI;
        Perform (Item, Pol.Plan (Item, registers), Why);
        PerCpuData.popCLI;
        Reply := Answer (Request, Why);
    end Call;
end Platform_Power;
