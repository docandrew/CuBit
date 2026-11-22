with Interfaces;

--  Hardware-independent decoding of HDA output amplifier parameters.
package HDA_Amplifiers with Pure, SPARK_Mode => On is
   subtype Gain_Step is Natural range 0 .. 127;

   type Capabilities is record
      Zero_DB : Gain_Step;
      Maximum : Gain_Step;
      Can_Mute : Boolean;
   end record;

   function Decode (Raw : Interfaces.Unsigned_32) return Capabilities;

   --  Never request positive gain or a step beyond the advertised range.
   --  A mute-only amplifier has Zero_DB = Maximum = 0.
   function Initial_Gain (Caps : Capabilities) return Gain_Step is
     (Gain_Step'Min (Caps.Zero_DB, Caps.Maximum));
end HDA_Amplifiers;
