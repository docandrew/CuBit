with Interfaces.C;
with System;
package Penny_Audio_Transport is
   function Open return Interfaces.C.int
     with Export, Convention => C, External_Name => "penny_audio_open";
   function Write (Data : System.Address; Frames : Interfaces.C.unsigned)
     return Interfaces.C.unsigned
     with Export, Convention => C, External_Name => "penny_audio_write";
   function Queued return Interfaces.Integer_64
     with Export, Convention => C, External_Name => "penny_audio_queued";
   function Capacity return Interfaces.Integer_64
     with Export, Convention => C, External_Name => "penny_audio_capacity";
   procedure Start with Export, Convention => C, External_Name => "penny_audio_start";
   procedure Close with Export, Convention => C, External_Name => "penny_audio_close";
end Penny_Audio_Transport;
