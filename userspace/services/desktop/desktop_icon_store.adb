package body Desktop_Icon_Store with SPARK_Mode,
  Refined_State => (State => (Phase_Now, Decoder, Mismatch, Atlas))
is
   use type Q.Phase;

   Phase_Now : Status := Not_Loaded;
   Decoder   : Q.Decoder;
   Mismatch  : Boolean := False;
   --  Zero-initialised, so it occupies .bss, not the executable.
   Atlas : Q.Pixel_Buffer (0 .. Atlas_Pixels - 1) := [others => 0];

   function Current return Status is (Phase_Now) with Refined_Global => Phase_Now;

   function Decoding return Boolean is
     (Q.Valid (Decoder) and then Q.Limit (Decoder) = Atlas_Pixels)
     with Global => Decoder;

   function Pixel (Icon : Desktop_Icons.Icon_ID; X, Y : Coordinate)
     return Interfaces.Unsigned_32 is
     (if Phase_Now = Loaded
      then Atlas ((Desktop_Icons.Icon_ID'Pos (Icon) * Icon_Size + Y) * Atlas_Width + X)
      else Fallback (X, Y))
     with Refined_Global => (Phase_Now, Atlas);

   procedure Begin_Load is
   begin
      Q.Start (Decoder, Atlas_Pixels);
      Mismatch := False;
      Phase_Now := Loading;
   end Begin_Load;

   procedure Feed (Input : Q.Byte_Array) is
   begin
      if Mismatch or else not Decoding or else Q.Current (Decoder) = Q.Failed then
         return;
      end if;
      Q.Feed (Decoder, Input, Atlas);
      --  Reject another size as soon as the header says so.
      if Q.Current (Decoder) in Q.Reading_Pixels | Q.Reading_Marker | Q.Complete and then
        (Q.Width (Decoder) /= Atlas_Width or else Q.Height (Decoder) /= Atlas_Height)
      then
         Mismatch := True;
      end if;
   end Feed;

   function Wants_More return Boolean is
     (not Mismatch and then Decoding and then Q.Current (Decoder) /= Q.Failed)
     with Refined_Global => (Input => (Mismatch, Decoder), Proof_In => Phase_Now);

   procedure End_Load (Read_Failed : Boolean; Result : out Load_Result) is
   begin
      Result := (others => <>);
      if Decoding then
         Q.Finish (Decoder);
         Result := (Loaded => not Read_Failed and then not Mismatch and then
                              Q.Current (Decoder) = Q.Complete,
                    Error => Q.Error (Decoder), Wrong_Size => Mismatch,
                    Width => Q.Width (Decoder), Height => Q.Height (Decoder));
      end if;
      Phase_Now := (if Result.Loaded then Loaded else Unavailable);
   end End_Load;
end Desktop_Icon_Store;
