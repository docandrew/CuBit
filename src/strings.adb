--with Interfaces; use Interfaces;

package body strings 
    with SPARK_Mode => On
is
    -- Convert Unsigned_8 to a hex string
    function toHexString(r : in Unsigned_8) return HexString8 is
        ret : HexString8 := "0x77";
    begin
        ret(1) := '0';
        ret(2) := 'x';
        ret(3) := hexdigits(HexDigitRange(Shift_Right((r and 16#F0#), 4)));
        ret(4) := hexdigits(HexDigitRange(r and 16#0F#));
        return ret;
    end toHexString;

    -- Convert Unsigned_16 integer to a hex string
    function toHexString(r : in Unsigned_16) return HexString16 is
        ret : HexString16 := "0xDEAD";
    begin
        ret(1) := '0';
        ret(2) := 'x';
        ret(3) := hexdigits(HexDigitRange(Shift_Right((r and 16#F000#), 12)));
        ret(4) := hexdigits(HexDigitRange(Shift_Right((r and 16#0F00#), 8)));
        ret(5) := hexdigits(HexDigitRange(Shift_Right((r and 16#00F0#), 4)));
        ret(6) := hexdigits(HexDigitRange(r and 16#000F#));
        return ret;
    end toHexString;

    -- Convert Unsigned_32 integer to a hex string
    function toHexString(r : in Unsigned_32) return HexString32 is
        ret : HexString32 := "0xDEADBEEF";
    begin
        ret(1) := '0';
        ret(2) := 'x';
        ret(3) := hexdigits(HexDigitRange(Shift_Right((r and 16#F000_0000#), 28)));
        ret(4) := hexdigits(HexDigitRange(Shift_Right((r and 16#0F00_0000#), 24)));
        ret(5) := hexdigits(HexDigitRange(Shift_Right((r and 16#00F0_0000#), 20)));
        ret(6) := hexdigits(HexDigitRange(Shift_Right((r and 16#000F_0000#), 16)));
        ret(7) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_F000#), 12)));
        ret(8) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0F00#), 8)));
        ret(9) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_00F0#), 4)));
        ret(10) := hexdigits(HexDigitRange(r and 16#0000_000F#));
        return ret;
    end toHexString;

    -- Convert Unsigned_64 integer to a hex string
    function toHexString(r : in Unsigned_64) return HexString64 is
        ret : HexString64 := "0xDEADBEEFDEADBEEF";
    begin
        ret(1) := '0';
        ret(2) := 'x';
        ret(3) := hexdigits(HexDigitRange(Shift_Right((r and 16#F000_0000_0000_0000#), 60)));
        ret(4) := hexdigits(HexDigitRange(Shift_Right((r and 16#0F00_0000_0000_0000#), 56)));
        ret(5) := hexdigits(HexDigitRange(Shift_Right((r and 16#00F0_0000_0000_0000#), 52)));
        ret(6) := hexdigits(HexDigitRange(Shift_Right((r and 16#000F_0000_0000_0000#), 48)));
        ret(7) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_F000_0000_0000#), 44)));
        ret(8) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0F00_0000_0000#), 40)));
        ret(9) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_00F0_0000_0000#), 36)));
        ret(10) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_000F_0000_0000#), 32)));
        ret(11) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_F000_0000#), 28)));
        ret(12) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_0F00_0000#), 24)));
        ret(13) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_00F0_0000#), 20)));
        ret(14) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_000F_0000#), 16)));
        ret(15) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_0000_F000#), 12)));
        ret(16) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_0000_0F00#), 8)));
        ret(17) := hexdigits(HexDigitRange(Shift_Right((r and 16#0000_0000_0000_00F0#), 4)));
        ret(18) := hexdigits(HexDigitRange(r and 16#0000_0000_0000_000F#));
        return ret;
    end toHexString;

end strings;