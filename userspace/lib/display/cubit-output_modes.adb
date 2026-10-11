pragma Ada_2022;
package body CuBit.Output_Modes with SPARK_Mode is
   use type DP.Payload;
   --  Explicit decoding of an untrusted status word: no unchecked enum
   --  conversion, totality evident to readers and the prover.
   function Status_Of (Word : Unsigned_64) return DP.Status_Code is
     (case Word is
        when 0 => DP.Success,
        when 1 => DP.Denied,
        when 2 => DP.Bad_Object,
        when 3 => DP.Bad_State,
        when 4 => DP.Invalid_Request,
        when 5 => DP.Unsupported,
        when others => DP.Resources_Exhausted);

   function Decode_Set_Mode (Service : OD.Provider; Wire : DP.Wire_Message)
      return Mode_Decoding is
   begin
      if Wire.Label /= Code (Service, Set_Mode) or else Wire.Length /= 4 or else
        Wire.Flags /= 0 or else Wire.Reserved /= 0 or else
        Wire.Words (0) not in 1 .. Unsigned_64 (OD.Extent'Last) or else
        Wire.Words (1) not in 1 .. Unsigned_64 (OD.Extent'Last) or else
        Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Item : constant Mode := (OD.Extent (Wire.Words (0)), OD.Extent (Wire.Words (1)));
         Back : constant DP.Wire_Message := Encode_Set_Mode (Service, Item);
      begin
         pragma Assert (Back.Words (0) = Wire.Words (0));
         pragma Assert (Back.Words (1) = Wire.Words (1));
         pragma Assert (Back.Words = Wire.Words);
         pragma Assert (Back = Wire);
         return (True, Item);
      end;
   end Decode_Set_Mode;

   function Decode_Result (Service : OD.Provider; Wire : DP.Wire_Message)
      return Result_Decoding is
   begin
      if Wire.Label /= Code (Service, Set_Mode) or else Wire.Length /= 4 or else
        Wire.Flags /= 0 or else Wire.Reserved /= 0 or else
        Wire.Words (0) > DP.Status_Code'Enum_Rep (DP.Status_Code'Last) or else
        Wire.Words (1) not in 1 .. Unsigned_64 (OD.Extent'Last) or else
        Wire.Words (2) not in 1 .. Unsigned_64 (OD.Extent'Last) or else
        Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Item : constant Mode_Result :=
           (Status_Of (Wire.Words (0)),
            (OD.Extent (Wire.Words (1)), OD.Extent (Wire.Words (2))));
         Back : constant DP.Wire_Message := Encode_Result (Service, Item);
      begin
         pragma Assert (Back.Words (0) = Wire.Words (0));
         pragma Assert (Back.Words (1) = Wire.Words (1));
         pragma Assert (Back.Words (2) = Wire.Words (2));
         pragma Assert (Back.Words = Wire.Words);
         pragma Assert (Back = Wire);
         return (True, Item);
      end;
   end Decode_Result;

   function Fit (Requested, Minimum : Mode; Budget : Byte_Count) return Mode is
      Raised : constant Mode :=
        (OD.Extent'Max (Requested.Width, Minimum.Width),
         OD.Extent'Max (Requested.Height, Minimum.Height));
      Candidate : Mode;
      Width, Height : Natural;
   begin
      if Requested.Width >= Minimum.Width and then
        Requested.Height >= Minimum.Height
      then
         pragma Assert (Raised = Requested);
      end if;
      if Bytes (Raised) <= Budget then
         return Raised;
      end if;
      for Step in reverse 1 .. Scale_Steps - 1 loop
         pragma Assert (Natural (Raised.Width) * Step <= Natural (Raised.Width) * Scale_Steps);
         pragma Assert (Natural (Raised.Height) * Step <= Natural (Raised.Height) * Scale_Steps);
         Width := Natural (Raised.Width) * Step / Scale_Steps;
         Height := Natural (Raised.Height) * Step / Scale_Steps;
         pragma Assert (Width <= Natural (Raised.Width));
         pragma Assert (Height <= Natural (Raised.Height));
         Candidate :=
           (OD.Extent'Max (Minimum.Width, Width),
            OD.Extent'Max (Minimum.Height, Height));
         if Bytes (Candidate) <= Budget then
            return Candidate;
         end if;
      end loop;
      return Minimum;
   end Fit;
end CuBit.Output_Modes;
