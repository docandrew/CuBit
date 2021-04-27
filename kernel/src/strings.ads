with Interfaces; use Interfaces;

package strings 
    with SPARK_Mode => On
is
    type HexDigitRange is range 0..15;
    type HexDigitsType is array (HexDigitRange) of Character;
    hexdigits : constant HexDigitsType := "0123456789ABCDEF";

    type DigitRange is range 0..9;
    type DigitsType is array (DigitRange) of Character;
    decdigits : constant DigitsType := "0123456789";

    subtype HexString8  is String (1..4);
    subtype HexString16 is String (1..6);
    subtype HexString32 is String (1..10);
    subtype HexString64 is String (1..18);
    subtype DecString32 is String (1..11);

    -- Convert unsigned integers to hex strings
    function toHexString(r : in Unsigned_8) return HexString8;
    function toHexString(r : in Unsigned_16) return HexString16;
    function toHexString(r : in Unsigned_32) return HexString32;
    function toHexString(r : in Unsigned_64) return HexString64;

end strings;