with Interfaces; use Interfaces;

package body HDA_Amplifiers with SPARK_Mode => On is
   function Decode (Raw : Unsigned_32) return Capabilities is
     ((Zero_DB => Gain_Step (Raw and 16#7F#),
       Maximum => Gain_Step (Shift_Right (Raw, 8) and 16#7F#),
       Can_Mute => (Raw and 16#8000_0000#) /= 0));
end HDA_Amplifiers;
