with CuBit.Audio;

package body CuBit.Audio_C is
   use type Interfaces.C.unsigned;
   use type System.Address;
   Stream : Audio.StreamHandle := Audio.NULL_STREAM;

   function Open return Interfaces.C.int is
   begin
      if Audio.isValid (Stream) then
         return 1;
      end if;
      Stream := Audio.open (48_000, 2);
      return (if Audio.isValid (Stream) then 1 else 0);
   end Open;

   function Write (Data : System.Address; Frames : Interfaces.C.unsigned)
     return Interfaces.C.unsigned is
   begin
      if Data = System.Null_Address or else
        Frames > Interfaces.C.unsigned (Natural'Last)
      then
         return 0;
      end if;
      return Interfaces.C.unsigned
        (Audio.write (Stream, Data, Natural (Frames)));
   end Write;

   procedure Start is
   begin
      Audio.start (Stream);
   end Start;

   procedure Close is
   begin
      Audio.close (Stream);
   end Close;

   procedure Set_Volume (Percent : Interfaces.C.unsigned) is
      use type Audio.Volume;
   begin
      if Percent <= 100 then
         Audio.setVolume
           (Stream, Audio.Volume'(1.0) * Natural (Percent) / 100);
      end if;
   end Set_Volume;
end CuBit.Audio_C;
