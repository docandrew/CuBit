with Interfaces.C;
with System;

--  Small single-stream adapter for C ports. All transport, grants and ring
--  publication stay in CuBit.Audio. Calls must be serialized by the client.
package CuBit.Audio_C is
   function Open return Interfaces.C.int
     with Export, Convention => C, External_Name => "cubit_audio_open";
   function Write (Data : System.Address; Frames : Interfaces.C.unsigned)
     return Interfaces.C.unsigned
     with Export, Convention => C, External_Name => "cubit_audio_write";
   procedure Start
     with Export, Convention => C, External_Name => "cubit_audio_start";
   procedure Close
     with Export, Convention => C, External_Name => "cubit_audio_close";
   procedure Set_Volume (Percent : Interfaces.C.unsigned)
     with Export, Convention => C, External_Name => "cubit_audio_volume";
end CuBit.Audio_C;
