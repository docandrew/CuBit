-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
-------------------------------------------------------------------------------
pragma Ada_2022;
package body Grant_Windows with
    SPARK_Mode => On
is
    function Is_Empty (S : Window_Set) return Boolean is
    begin
        for I in Word_Index loop
            if S.Words (I) /= 0 then
                return False;
            end if;
            pragma Loop_Invariant (for all J in Word_Index'First .. I => S.Words (J) = 0);
        end loop;
        return True;
    end Is_Empty;

    procedure Allocate (S : in out Window_Set; W : out Window; Found : out Boolean) is
        Full : constant Unsigned_64 := Unsigned_64'Last;
    begin
        W := Window'First;
        Found := False;
        for I in Word_Index loop
            if S.Words (I) /= Full then
                for B in Bit_Index loop
                    if (S.Words (I) and Bit (B)) = 0 then
                        W := I * Word_Bits + B;
                        pragma Assert (W / Word_Bits = I and then W mod Word_Bits = B);
                        S.Words (I) := S.Words (I) or Bit (B);
                        Found := True;
                        return;
                    end if;
                end loop;
            end if;
        end loop;
    end Allocate;

    procedure Release (S : in out Window_Set; W : Window) is
    begin
        S.Words (W / Word_Bits) := S.Words (W / Word_Bits) and not Bit (W mod Word_Bits);
    end Release;
end Grant_Windows;
