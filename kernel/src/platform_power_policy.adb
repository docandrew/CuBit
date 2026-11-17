pragma Ada_2022;
package body Platform_Power_Policy with SPARK_Mode => On is

    function Decode (Writable : Boolean;
                     Label    : Unsigned_32;
                     Length   : Unsigned_8;
                     State_Word, Type_A_Word, Type_B_Word, Unused_Word : Unsigned_64)
                     return Request
    is
        State : ABI.Power_State;
    begin
        if not Writable then
            return (Valid => False, Why => ABI.Not_Permitted);
        elsif Label /= ABI.Operation'Enum_Rep (ABI.Enter_State) then
            return (Valid => False, Why => ABI.Unknown_Operation);
        elsif Length /= ABI.Request_Words or else Unused_Word /= 0 then
            return (Valid => False, Why => ABI.Malformed);
        end if;
        if State_Word = ABI.Power_State'Enum_Rep (ABI.S3) then
            State := ABI.S3;
        elsif State_Word = ABI.Power_State'Enum_Rep (ABI.S4) then
            State := ABI.S4;
        elsif State_Word = ABI.Power_State'Enum_Rep (ABI.S5) then
            State := ABI.S5;
        elsif State_Word = ABI.Power_State'Enum_Rep (ABI.Reset) then
            State := ABI.Reset;
        else
            return (Valid => False, Why => ABI.Malformed);
        end if;
        if Type_A_Word > Sleep_Type'Last or else Type_B_Word > Sleep_Type'Last
          or else (State = ABI.Reset and then (Type_A_Word /= 0 or else Type_B_Word /= 0))
        then
            return (Valid => False, Why => ABI.Sleep_Type_Out_Of_Range);
        end if;
        return (Valid  => True, State => State,
                Type_A => Type_A_Word, Type_B => Type_B_Word);
    end Decode;

    function Admit (Item : Request; Registers : PR.Description) return Admission is
    begin
        case Item.State is
            when ABI.S3 | ABI.S4 =>
                return (Admitted => False, Why => ABI.Not_Yet);
            when ABI.Reset =>
                if not Registers.Reset.Present then
                    return (Admitted => False, Why => ABI.No_Reset_Register);
                elsif not Usable (Registers.Reset) then
                    return (Admitted => False, Why => ABI.Unsupported_Register);
                end if;
                return (Admitted => True);
            when ABI.S5 =>
                case Registers.Model is
                    when PR.No_Sleep_Registers =>
                        return (Admitted => False, Why => ABI.No_Sleep_Registers);
                    when PR.PM1A_Only =>
                        if not Usable (Registers.PM1A_Control) then
                            return (Admitted => False, Why => ABI.Unsupported_Register);
                        end if;
                    when PR.PM1A_And_B =>
                        if not Usable (Registers.PM1A_Control)
                          or else not Usable (Registers.PM1B_Control)
                        then
                            return (Admitted => False, Why => ABI.Unsupported_Register);
                        end if;
                    when PR.Hardware_Reduced =>
                        if not Usable (Registers.Sleep_Control) then
                            return (Admitted => False, Why => ABI.Unsupported_Register);
                        end if;
                end case;
                return (Admitted => True);
        end case;
    end Admit;

    function Plan (Item : Request; Registers : PR.Description) return Plan_Type is
        Result : Plan_Type;
    begin
        Result.Steps (1) := Drain_Console;
        Result.Steps (2) := Park_CPUs;
        if Item.State = ABI.Reset then
            Result.Steps (3) := Write_Reset;
            Result.Steps (4) := Settle;
            Result.Count := 4;
        else
            case Registers.Model is
                when PR.Hardware_Reduced =>
                    Result.Steps (3) := Reduced_Enable;
                    Result.Steps (4) := Settle;
                    Result.Count := 4;
                when PR.PM1A_Only =>
                    Result.Steps (3 .. 6) := [Read_A, Stage_A, Enable_A, Settle];
                    Result.Count := 6;
                when PR.PM1A_And_B | PR.No_Sleep_Registers =>
                    Result.Steps (3 .. 9) :=
                      [Read_B, Read_A, Stage_B, Stage_A, Enable_B, Enable_A, Settle];
                    Result.Count := Maximum_Steps;
            end case;
        end if;
        return Result;
    end Plan;

end Platform_Power_Policy;
