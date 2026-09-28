with Interfaces; use Interfaces;

-- Boot-protocol snapshots, not arbitrary HID report descriptors.
package USB_Keyboards with SPARK_Mode is
   type Report is array (Positive range 1 .. 8) of Unsigned_8;
   type Key_Set is array (Unsigned_8) of Boolean;
   type Decode_Result is (Decoded, Rollover, Malformed);
   procedure Decode (Data : Report; Keys : out Key_Set; Result : out Decode_Result)
     with Post =>
       (if Result /= Decoded then (for all K in Keys'Range => not Keys (K)));

   type State is private;
   type Changes is record
      Released, Pressed : Key_Set := [others => False];
   end record;
   -- Invalid/error snapshots preserve the previous state; never invent releases.
   -- Consumer resynchronization/disconnect is a separate explicit operation.
   procedure Update
     (Previous : in out State; Data : Report; Events : out Changes;
      Result : out Decode_Result);
   procedure Release_All (Previous : in out State; Events : out Changes);
private
   type State is record
      Held : Key_Set := [others => False];
   end record;
end USB_Keyboards;
