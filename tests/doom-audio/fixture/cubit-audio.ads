with System;
with Interfaces; use Interfaces;
package CuBit.Audio is
   type StreamHandle is new Boolean;
   NULL_STREAM : constant StreamHandle := False;
   function open (Rate : Natural; Channels : Natural) return StreamHandle;
   function isValid (Stream : StreamHandle) return Boolean;
   procedure start (Stream : StreamHandle);
   procedure close (Stream : in out StreamHandle);
   function write (Stream : StreamHandle; Buffer : System.Address;
                   Frames : Natural) return Natural;

   -- Host-only controllable sink: consume any prefix, including zero frames.
   procedure Reset;
   Limit : Natural := Natural'Last;
   Calls : Natural := 0;
   Frame_Count : Natural := 0;
   subtype Sample_Index is Natural range 0 .. 65_535;
   type Sample_Array is array (Sample_Index) of Integer_16;
   Captured : Sample_Array;
end CuBit.Audio;
