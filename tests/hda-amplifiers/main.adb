with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with HDA_Amplifiers; use HDA_Amplifiers;

procedure Main is
   Caps : Capabilities;
   Gain : Gain_Step;
begin
   --  ALC3253 DAC: 0.75 dB steps, unity and maximum at step 87.
   Caps := Decode (16#0002_5757#);
   pragma Assert (Caps.Zero_DB = 87 and Caps.Maximum = 87);
   pragma Assert (not Caps.Can_Mute and Initial_Gain (Caps) = 87);

   --  ALC3253 speaker pin: only a mute bit; gain MUST be zero.
   Caps := Decode (16#8000_0000#);
   pragma Assert (Caps.Can_Mute and Initial_Gain (Caps) = 0);

   --  A codec with amplification above unity must not start at maximum.
   pragma Assert (Initial_Gain (Decode (16#001F_4A25#)) = 37);
   --  Even an inconsistent offset cannot exceed the advertised gain range.
   pragma Assert (Initial_Gain (Decode (16#0000_207F#)) = 32);

   --  Exhaust every combination of the two seven-bit gain fields, with
   --  unrelated/reserved/step-size bits set; these must not affect decoding.
   for Offset in Gain_Step loop
      for Maximum in Gain_Step loop
         Caps := Decode (16#FFFF_8080# or Unsigned_32 (Offset) or
           Shift_Left (Unsigned_32 (Maximum), 8));
         Gain := Initial_Gain (Caps);
         pragma Assert (Caps.Zero_DB = Offset and Caps.Maximum = Maximum);
         pragma Assert (Caps.Can_Mute);
         pragma Assert (Gain = Natural'Min (Offset, Maximum));
      end loop;
   end loop;
   Put_Line ("PASS: HDA amplifier fixtures and 16384 gain combinations");
end Main;
