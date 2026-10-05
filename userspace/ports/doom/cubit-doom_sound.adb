pragma Ada_2022;
with CuBit.Audio;
with CuBit.Messages;

package body CuBit.Doom_Sound is

   Stereo : constant := 2;

   --  Spelled out (not Silent) so the table needs no elaboration code.
   Channels : array (Channel_Index) of Channel :=
     [others => (Active => False, Length => 0, At_Sample => 0, Advance => 0,
                 Left => 0, Right => 0)];
   --  Read only while the channel is active; zero-filled .bss until then.
   Sources  : array (Channel_Index) of System.Address
     with Suppress_Initialization;

   Stream : CuBit.Audio.StreamHandle := CuBit.Audio.NULL_STREAM;

   Mixed  : Mix_Buffer with Suppress_Initialization;
   Output : Output_Buffer with Suppress_Initialization;

   --  The suffix of Output the stream has not accepted yet; Mix_Frames is
   --  none. One batch is kept: nothing is remixed over an unwritten suffix.
   subtype Frame_Offset is Natural range 0 .. Mix_Frames;
   Pending_First : Frame_Offset := Mix_Frames;

   procedure Flush_Pending;
   procedure Flush_Pending is
   begin
      if Pending_First < Mix_Frames then
         declare
            --  CuBit.Audio.write accepts a prefix of the frames offered.
            subtype Accepted is Natural range 0 .. Mix_Frames - Pending_First;
            Written : constant Accepted := CuBit.Audio.write
              (Stream, Output (Pending_First * Stereo)'Address,
               Mix_Frames - Pending_First);
         begin
            Pending_First := Pending_First + Written;
         end;
      end if;
   end Flush_Pending;

   function Init return Boolean is
   begin
      Pending_First := Mix_Frames;
      Channels := [others => Silent];
      Stream := CuBit.Audio.open (Output_Rate, Stereo);
      if not CuBit.Audio.isValid (Stream) then
         CuBit.Messages.debugPrint ("doom_snd: mixer open failed" & ASCII.LF);
         return False;
      end if;
      CuBit.Audio.start (Stream);
      CuBit.Messages.debugPrint ("doom_snd: init OK" & ASCII.LF);
      return True;
   end Init;

   procedure Shutdown is
   begin
      if CuBit.Audio.isValid (Stream) then
         CuBit.Audio.close (Stream);
      end if;
      Pending_First := Mix_Frames;
      Channels := [others => Silent];
   end Shutdown;

   procedure Update is
      Any_Active : Boolean := False;
   begin
      if not CuBit.Audio.isValid (Stream) then
         return;
      end if;
      Flush_Pending;
      if Pending_First < Mix_Frames then
         return;
      end if;
      for C of Channels loop
         Any_Active := Any_Active or else C.Active;
      end loop;
      if not Any_Active then
         return;
      end if;
      Mixed := [others => 0];
      for I in Channel_Index loop
         if Channels (I).Active then
            declare
               Samples : constant Byte_Array
                 (0 .. Channels (I).Length - 1)
                 with Import, Address => Sources (I);
            begin
               Mix (Samples, Channels (I), Mixed);
            end;
         end if;
      end loop;
      for I in Output'Range loop
         Output (I) := Clamped (Mixed (I));
      end loop;
      Pending_First := 0;
      Flush_Pending;
   end Update;

   procedure Start (Channel : Channel_Index; Data : System.Address;
                    Length : Sample_Count; Rate : Sample_Rate;
                    Vol, Sep : Integer) is
   begin
      Channels (Channel) := Started (Length, Rate, Vol, Sep);
      Sources (Channel) := Data;
   end Start;

   procedure Stop (Channel : Channel_Index) is
   begin
      Channels (Channel).Active := False;
   end Stop;

   function Is_Playing (Channel : Channel_Index) return Boolean is
     (Channels (Channel).Active);

   procedure Update_Parameters (Channel : Channel_Index; Vol, Sep : Integer)
   is
   begin
      if Channels (Channel).Active then
         Pan (Vol, Sep, Channels (Channel).Left, Channels (Channel).Right);
      end if;
   end Update_Parameters;

end CuBit.Doom_Sound;
