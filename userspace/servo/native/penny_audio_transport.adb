with CuBit.Audio;

package body Penny_Audio_Transport is
   package Audio renames CuBit.Audio;
   use type Interfaces.C.unsigned;
   use type Interfaces.Integer_64;
   Stream : Audio.StreamHandle := Audio.NULL_STREAM;
   function Open return Interfaces.C.int is
   begin
      if Audio.isValid (Stream) then return 0; end if;
      Stream := Audio.open (48_000, 2);
      return (if Audio.isValid (Stream) then 1 else 0);
   end Open;
   function Write (Data : System.Address; Frames : Interfaces.C.unsigned)
     return Interfaces.C.unsigned is
   begin
      if Frames > Interfaces.C.unsigned (Natural'Last) then return 0; end if;
      return Interfaces.C.unsigned (Audio.write (Stream, Data, Natural (Frames)));
   end Write;
   function Queued return Interfaces.Integer_64 is
      State : constant Audio.Playback_Status := Audio.Playback (Stream);
   begin
      return (if State.Valid then
         Interfaces.Integer_64 (State.Ring_Frames + State.Device_Frames) else -1);
   end Queued;
   function Capacity return Interfaces.Integer_64 is
      State : constant Audio.Playback_Status := Audio.Playback (Stream);
   begin
      return (if State.Valid then Interfaces.Integer_64
        (State.Ring_Capacity_Frames + State.Device_Capacity_Frames) else -1);
   end Capacity;
   procedure Start is
   begin
      Audio.start (Stream);
   end Start;
   procedure Close is
   begin
      Audio.close (Stream);
   end Close;
end Penny_Audio_Transport;
