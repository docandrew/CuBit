------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  DOOM sound engine implementation
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Audio;
with CuBit.Messages; use CuBit.Messages;

package body CuBit.Doom_Sound is

   --  Output format: 48 kHz, stereo, S16LE
   OUTPUT_RATE : constant := 48000;

   --  Mix buffer: enough for one game tic (~28 ms at 35 Hz)
   --  48000 / 35 ~= 1371 frames; round up to 1536
   MIX_FRAMES  : constant := 1536;

   type Sample16 is new Integer_16;
   type MixSample is new Integer_32;

   type MixBufArray is array (0 .. MIX_FRAMES * 2 - 1) of MixSample;
   type OutBufArray is array (0 .. MIX_FRAMES * 2 - 1) of Sample16
      with Convention => C;

   ---------------------------------------------------------------------------
   --  Channel state
   ---------------------------------------------------------------------------
   type ChannelInfo is record
      active     : Boolean     := False;
      data       : System.Address := System.Null_Address;
      dataLen    : Unsigned_32 := 0;   --  total samples
      sampleRate : Unsigned_32 := 0;   --  source Hz
      pos        : Unsigned_32 := 0;   --  playback position (16.16 fixed)
      step       : Unsigned_32 := 0;   --  resample step (16.16 fixed)
      volL       : Integer     := 0;   --  left volume 0..127
      volR       : Integer     := 0;   --  right volume 0..127
   end record;

   channels : array (0 .. MAX_CHANNELS - 1) of ChannelInfo;

   --  Our mixer stream handle
   stream : CuBit.Audio.StreamHandle := CuBit.Audio.NULL_STREAM;

   mixBuf : MixBufArray;
   outBuf : OutBufArray;

   ---------------------------------------------------------------------------
   --  computeVolumes - convert DOOM vol (0-127) and sep (0-254) to L/R
   ---------------------------------------------------------------------------
   procedure computeVolumes (vol : Integer;
                              sep : Integer;
                              vL  : out Integer;
                              vR  : out Integer);

   procedure computeVolumes (vol : Integer;
                              sep : Integer;
                              vL  : out Integer;
                              vR  : out Integer) is
      --  sep: 0=full left, 128=centre, 254=full right
      left  : Integer := 254 - sep;
      right : Integer := sep;
   begin
      --  Scale by volume
      vL := (vol * left) / 254;
      vR := (vol * right) / 254;

      if vL > 127 then vL := 127; end if;
      if vR > 127 then vR := 127; end if;
      if vL < 0 then vL := 0; end if;
      if vR < 0 then vR := 0; end if;
   end computeVolumes;

   ---------------------------------------------------------------------------
   --  sndInit
   ---------------------------------------------------------------------------
   function sndInit return Integer is
   begin
      stream := CuBit.Audio.open (OUTPUT_RATE, 2);

      if not CuBit.Audio.isValid (stream) then
         debugPrint ("doom_snd: mixer open failed" & ASCII.LF);
         return 0;
      end if;

      CuBit.Audio.start (stream);
      debugPrint ("doom_snd: init OK" & ASCII.LF);
      return 1;
   end sndInit;

   ---------------------------------------------------------------------------
   --  sndShutdown
   ---------------------------------------------------------------------------
   procedure sndShutdown is
   begin
      if CuBit.Audio.isValid (stream) then
         CuBit.Audio.close (stream);
      end if;
   end sndShutdown;

   ---------------------------------------------------------------------------
   --  sndStartChannel
   ---------------------------------------------------------------------------
   function sndStartChannel (data       : System.Address;
                              dataLen    : Unsigned_32;
                              sampleRate : Unsigned_32;
                              channel    : Integer;
                              vol        : Integer;
                              sep        : Integer) return Integer
   is
      ch  : Integer := channel;
      vL  : Integer;
      vR  : Integer;
   begin
      if ch < 0 or ch >= MAX_CHANNELS then
         return -1;
      end if;

      if dataLen = 0 or sampleRate = 0 then
         return -1;
      end if;

      computeVolumes (vol, sep, vL, vR);

      --  Resample step: source_rate / output_rate in 16.16
      --  = (sampleRate * 65536) / OUTPUT_RATE
      channels (ch) := (active     => True,
                         data       => data,
                         dataLen    => dataLen,
                         sampleRate => sampleRate,
                         pos        => 0,
                         step       => (sampleRate * 65536) /
                                       OUTPUT_RATE,
                         volL       => vL,
                         volR       => vR);

      return ch;
   end sndStartChannel;

   ---------------------------------------------------------------------------
   --  sndStopChannel
   ---------------------------------------------------------------------------
   procedure sndStopChannel (channel : Integer) is
   begin
      if channel >= 0 and channel < MAX_CHANNELS then
         channels (channel).active := False;
      end if;
   end sndStopChannel;

   ---------------------------------------------------------------------------
   --  sndIsPlaying
   ---------------------------------------------------------------------------
   function sndIsPlaying (channel : Integer) return Integer is
   begin
      if (channel >= 0 and channel < MAX_CHANNELS)
         and then channels (channel).active
      then
         return 1;
      end if;
      return 0;
   end sndIsPlaying;

   ---------------------------------------------------------------------------
   --  sndUpdateParams
   ---------------------------------------------------------------------------
   procedure sndUpdateParams (channel : Integer;
                               vol     : Integer;
                               sep     : Integer)
   is
      vL : Integer;
      vR : Integer;
   begin
      if channel < 0 or channel >= MAX_CHANNELS then
         return;
      end if;
      if not channels (channel).active then
         return;
      end if;

      computeVolumes (vol, sep, vL, vR);
      channels (channel).volL := vL;
      channels (channel).volR := vR;
   end sndUpdateParams;

   ---------------------------------------------------------------------------
   --  sndUpdate - mix all active channels, write to ring buffer
   ---------------------------------------------------------------------------
   procedure sndUpdate is
      sampleIdx : Natural;
      raw       : Integer;
      scaledL   : MixSample;
      scaledR   : MixSample;
      val       : MixSample;
      written   : Natural;
      anyActive : Boolean := False;
   begin
      if not CuBit.Audio.isValid (stream) then
         return;
      end if;

      --  Check if any channel is active before doing work
      for ch in channels'Range loop
         if channels (ch).active then
            anyActive := True;
            exit;
         end if;
      end loop;

      if not anyActive then
         return;
      end if;

      --  Clear mix buffer
      for i in mixBuf'Range loop
         mixBuf (i) := 0;
      end loop;

      --  Mix each active channel
      for ch in channels'Range loop
         if channels (ch).active then
            mixChannel : declare
               c : ChannelInfo renames channels (ch);

               type U8Array is
                  array (Natural range <>) of Unsigned_8
                  with Convention => C;

               srcData : U8Array (0 .. Natural (c.dataLen) - 1)
                  with Import, Address => c.data;

               pos : Unsigned_32 := c.pos;
            begin
               for f in 0 .. MIX_FRAMES - 1 loop
                  sampleIdx := Natural (Shift_Right (pos, 16));

                  if sampleIdx >= Natural (c.dataLen) then
                     c.active := False;
                     exit;
                  end if;

                  --  Convert 8-bit unsigned (0-255) to signed
                  raw := Integer (srcData (sampleIdx)) - 128;

                  --  Scale to 16-bit range and apply volume
                  scaledL := MixSample (raw * c.volL * 2);
                  scaledR := MixSample (raw * c.volR * 2);

                  mixBuf (f * 2)     :=
                     mixBuf (f * 2)     + scaledL;
                  mixBuf (f * 2 + 1) :=
                     mixBuf (f * 2 + 1) + scaledR;

                  pos := pos + c.step;
               end loop;

               c.pos := pos;
            end mixChannel;
         end if;
      end loop;

      --  Clamp 32-bit mix to 16-bit and write to output buffer
      for i in outBuf'Range loop
         val := mixBuf (i);
         if val > 32767 then
            val := 32767;
         elsif val < -32768 then
            val := -32768;
         end if;
         outBuf (i) := Sample16 (val);
      end loop;

      --  Write mixed output to ring buffer
      written := CuBit.Audio.write (stream, outBuf'Address,
                                     MIX_FRAMES);
   end sndUpdate;

end CuBit.Doom_Sound;
