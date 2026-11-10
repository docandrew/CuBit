pragma Ada_2022;
package body IO_Page_Tables with SPARK_Mode => On is

    procedure Lemma_Recompose (Address : Unsigned_64; W : Address_Width) is
    begin
        -- Each index part is the address's bits of that level, in place.
        pragma Assert (Index_Part (Address, 1) = (Address and 16#0000_0000_001F_F000#));
        pragma Assert (Index_Part (Address, 2) = (Address and 16#0000_0000_3FE0_0000#));
        pragma Assert (Index_Part (Address, 3) = (Address and 16#0000_007F_C000_0000#));
        pragma Assert (Index_Part (Address, 4) = (Address and 16#0000_FF80_0000_0000#));
        case W is
            when Width_39 =>
                pragma Assert (Address < 2 ** 39);
                pragma Assert (Compose (Address, W) = (Address and (2 ** 39 - 1)));
            when Width_48 =>
                pragma Assert (Address < 2 ** 48);
                pragma Assert (Compose (Address, W) = (Address and (2 ** 48 - 1)));
        end case;
    end Lemma_Recompose;

    function Choose_Width
      (SAGAW : SAGAW_Field; MGAW_Bits, Host_Bits : Natural) return Width_Choice
    is
    begin
        for W in Address_Width loop
            pragma Loop_Invariant
              (for all V in Address_Width'First .. W =>
                 (if V /= W then
                    not (Supports (SAGAW, V) and then Width_Bits (V) >= Host_Bits
                         and then Width_Bits (V) <= MGAW_Bits)));
            if Supports (SAGAW, W) and then Width_Bits (W) >= Host_Bits
              and then Width_Bits (W) <= MGAW_Bits
            then
                return (Found => True, Width => W);
            end if;
        end loop;
        return (Found => False);
    end Choose_Width;
end IO_Page_Tables;
