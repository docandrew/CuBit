pragma Ada_2022;

package body Guest_Speculation with
    SPARK_Mode => On
is
    function Flag (On : Boolean; N : Bit_Number) return Unsigned_64 is
      (if On then Shift_Left (1, N) else 0);

    -- One term per bit SPEC_CTRL defines; Offered is False for the rest.
    function Writable_Bits (H : Host_CPUID) return Control_Mask is
      (Flag (Offered (H, Control_IBRS), Control_IBRS)
       or Flag (Offered (H, Control_STIBP), Control_STIBP)
       or Flag (Offered (H, Control_SSBD), Control_SSBD)
       or Flag (Offered (H, Control_IPRED_DIS_U), Control_IPRED_DIS_U)
       or Flag (Offered (H, Control_IPRED_DIS_S), Control_IPRED_DIS_S)
       or Flag (Offered (H, Control_RRSBA_DIS_U), Control_RRSBA_DIS_U)
       or Flag (Offered (H, Control_RRSBA_DIS_S), Control_RRSBA_DIS_S)
       or Flag (Offered (H, Control_PSFD), Control_PSFD)
       or Flag (Offered (H, Control_DDPD_U), Control_DDPD_U)
       or Flag (Offered (H, Control_BHI_DIS_S), Control_BHI_DIS_S));

    function Guest_Write (Mask : Control_Mask; Value : Control_Value) return Write_Verdict is
    begin
        if not Present (Mask) or else not Value_Allowed (Mask, Value) then
            return Fault;
        elsif Value = 0 then
            return Keep_Intercepting;
        else
            return Pass_Writes;
        end if;
    end Guest_Write;

    function Switch_Mode
      (Mask : Control_Mask; Hardware_Switched, Writes_Passed : Boolean) return Switch is
    begin
        if not Present (Mask) or else Hardware_Switched then
            return No_Switch;
        elsif Writes_Passed then
            return Load_And_Save;
        else
            return Load_Guest;
        end if;
    end Switch_Mode;

    function Guest_EFER_Bits (Base : Unsigned_64; H : Host_CPUID; SVM : Boolean) return Unsigned_64 is
    begin
        return Base or Flag (SVM and then Bit (H.Extended_2_EAX, Extended_2_AutoIBRS),
                             EFER_AutoIBRS_Bit);
    end Guest_EFER_Bits;
end Guest_Speculation;
