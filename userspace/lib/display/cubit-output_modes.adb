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
      return Request_Decoding is
      Best : constant Unsigned_64 := Rate_Choice'Enum_Rep (Best_Rate);
      Exact : constant Unsigned_64 := Rate_Choice'Enum_Rep (Exact_Rate);
   begin
      if Wire.Label /= Code (Service, Set_Mode) or else Wire.Length /= 4 or else
        Wire.Flags /= 0 or else Wire.Reserved /= 0 or else
        Wire.Words (0) not in 1 .. Unsigned_64 (OD.Extent'Last) or else
        Wire.Words (1) not in 1 .. Unsigned_64 (OD.Extent'Last) or else
        not ((Wire.Words (2) = Best and then Wire.Words (3) = 0) or else
             (Wire.Words (2) = Exact and then
              Wire.Words (3) in Unsigned_64 (Millihertz'First) .. Unsigned_64 (Millihertz'Last)))
      then
         return (Valid => False);
      end if;
      declare
         Size : constant Mode := (OD.Extent (Wire.Words (0)), OD.Extent (Wire.Words (1)));
         Item : constant Mode_Request :=
           (if Wire.Words (2) = Best then (Best_Rate, Size)
            else (Exact_Rate, Size, Millihertz (Wire.Words (3))));
         Back : constant DP.Wire_Message := Encode_Set_Mode (Service, Item);
      begin
         pragma Assert (Back.Words (0) = Wire.Words (0));
         pragma Assert (Back.Words (1) = Wire.Words (1));
         pragma Assert (Back.Words (2) = Wire.Words (2));
         pragma Assert (Back.Words (3) = Wire.Words (3));
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

   function Decode_List_Request (Service : OD.Provider; Wire : DP.Wire_Message)
      return Index_Decoding is
   begin
      if Wire.Label /= Code (Service, List_Mode) or else Wire.Length /= 4 or else
        Wire.Flags /= 0 or else Wire.Reserved /= 0 or else
        Wire.Words (0) not in 1 .. Unsigned_64 (Mode_Index'Last) or else
        Wire.Words (1) /= 0 or else Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Back : constant DP.Wire_Message :=
           Encode_List_Request (Service, Mode_Index (Wire.Words (0)));
      begin
         pragma Assert (Back.Words = Wire.Words);
         return (True, Mode_Index (Wire.Words (0)));
      end;
   end Decode_List_Request;

   Count_Shift : constant := 16;
   Verdict_Shift : constant := 32;
   Preferred_Bit : constant Unsigned_64 := 2 ** 40;
   Current_Bit : constant Unsigned_64 := 2 ** 41;
   Field_16 : constant Unsigned_64 := 2 ** 16 - 1;
   Byte_Field : constant Unsigned_64 := 2 ** 8 - 1;

   function Encode_Listing (Service : OD.Provider; Item : Listing) return DP.Wire_Message is
     ((Label => Code (Service, List_Mode), Length => 4, Flags => 0, Reserved => 0,
       Words =>
         [Unsigned_64 (Item.Index) or Shift_Left (Unsigned_64 (Item.Count), Count_Shift) or
            Shift_Left (Unsigned_64 (Admission'Enum_Rep (Item.Item.Verdict)), Verdict_Shift) or
            (if Item.Item.Preferred then Preferred_Bit else 0) or
            (if Item.Item.Current then Current_Bit else 0),
          Unsigned_64 (Item.Item.Size.Width) + Unsigned_64 (Item.Item.Size.Height) * 2 ** 16,
          Unsigned_64 (Item.Item.Refresh_Millihertz), Unsigned_64 (Item.Item.Pixel_kHz)]));

   function Decode_Listing (Service : OD.Provider; Wire : DP.Wire_Message)
      return Listing_Decoding
   is
      W0 : constant Unsigned_64 := Wire.Words (0);
      Index : constant Unsigned_64 := W0 and Field_16;
      Count : constant Unsigned_64 := Shift_Right (W0, Count_Shift) and Field_16;
      Verdict : constant Unsigned_64 := Shift_Right (W0, Verdict_Shift) and Byte_Field;
      Width : constant Unsigned_64 := Wire.Words (1) mod 2 ** 16;
      Height : constant Unsigned_64 := Wire.Words (1) / 2 ** 16;
   begin
      if Wire.Label /= Code (Service, List_Mode) or else Wire.Length /= 4 or else
        Wire.Flags /= 0 or else Wire.Reserved /= 0 or else
        Index = 0 or else Index > Count or else
        Verdict > Admission'Enum_Rep (Admission'Last) or else
        Shift_Right (W0, 42) /= 0 or else
        Width not in 1 .. Unsigned_64 (OD.Extent'Last) or else
        Height not in 1 .. Unsigned_64 (OD.Extent'Last) or else
        Wire.Words (2) > Unsigned_64 (Unsigned_32'Last) or else
        Wire.Words (3) > Unsigned_64 (Unsigned_32'Last)
      then
         return (Valid => False);
      end if;
      declare
         Item : constant Listing :=
           (Index => Mode_Index (Index), Count => Mode_Count (Count),
            Item => (Size => (OD.Extent (Width), OD.Extent (Height)),
                     Refresh_Millihertz => Unsigned_32 (Wire.Words (2)),
                     Pixel_kHz => Unsigned_32 (Wire.Words (3)),
                     Verdict => (case Verdict is
                                   when 0 => Admitted, when 1 => Sync_Unsupported,
                                   when 2 => Port_Unsupported, when 3 => Link_Unsupported,
                                   when 4 => Above_Source_Clock, when 5 => Above_Sink_Clock,
                                   when 6 => Clock_Hole, when 7 => Needs_Scrambling,
                                   when 8 => Needs_Higher_CD_Clock, when 9 => Below_Minimum_Size,
                                   when others => Above_Scanout_Buffers),
                     Preferred => (W0 and Preferred_Bit) /= 0,
                     Current => (W0 and Current_Bit) /= 0));
      begin
         pragma Assert (Item.Index <= Item.Count);
         return (True, Item);
      end;
   end Decode_Listing;

   Serial_Bits : constant := 32;
   Product_Shift : constant := 32;
   Name_Count_Shift : constant := 48;
   Letter_Bits : constant := 8;
   Letter_Field : constant Unsigned_64 := 2 ** Letter_Bits - 1;
   Vendor_Letters : constant := 3;
   --  Name characters in word 1 after the vendor, and in word 2.
   First_Word_Letters : constant := 5;
   Second_Word_Letters : constant := 8;
   pragma Compile_Time_Error
     (First_Word_Letters + Second_Word_Letters /= Name_Length,
      "the identity's name words hold exactly Name_Length characters");

   function Letter (Word : Unsigned_64; Position : Natural) return Unsigned_64 is
     (Shift_Right (Word, Letter_Bits * Position) and Letter_Field)
     with Pre => Position < 8;
   function Byte (Ch : Character) return Unsigned_64 is (Unsigned_64 (Character'Pos (Ch)));

   function Encode_Identity (Service : OD.Provider; Item : Monitor_Identity)
      return DP.Wire_Message
   is
      Header : constant Unsigned_64 := Unsigned_64 (Item.Serial) or
        Shift_Left (Unsigned_64 (Item.Product), Product_Shift) or
        Shift_Left (Unsigned_64 (Item.Name_Last), Name_Count_Shift);
      First, Second : Unsigned_64 := 0;
   begin
      for I in 1 .. Vendor_Letters loop
         First := First or Shift_Left (Byte (Item.Vendor (I)), Letter_Bits * (I - 1));
      end loop;
      for I in 1 .. First_Word_Letters loop
         First := First or Shift_Left (Byte (Item.Name (I)), Letter_Bits * (Vendor_Letters + I - 1));
      end loop;
      for I in 1 .. Second_Word_Letters loop
         Second := Second or Shift_Left (Byte (Item.Name (First_Word_Letters + I)), Letter_Bits * (I - 1));
      end loop;
      return (Label => Code (Service, Identify), Length => 4, Flags => 0, Reserved => 0,
              Words => [Header, First, Second, 0]);
   end Encode_Identity;

   function Decode_Identity (Service : OD.Provider; Wire : DP.Wire_Message)
      return Identity_Decoding
   is
      Header : constant Unsigned_64 := Wire.Words (0);
      Count : constant Unsigned_64 := Shift_Right (Header, Name_Count_Shift);
      Item : Monitor_Identity;
      Code_Point : Unsigned_64;
   begin
      if Wire.Label /= Code (Service, Identify) or else Wire.Length /= 4 or else
        Wire.Flags /= 0 or else Wire.Reserved /= 0 or else Wire.Words (3) /= 0 or else
        Count > Name_Length or else
        Shift_Right (Wire.Words (1), Letter_Bits * (Vendor_Letters + First_Word_Letters)) /= 0
      then
         return (Valid => False);
      end if;
      Item.Serial := Unsigned_32 (Header and (2 ** Serial_Bits - 1));
      Item.Product := Unsigned_16 (Shift_Right (Header, Product_Shift) and 16#FFFF#);
      Item.Name_Last := Name_Count (Count);
      for I in 1 .. Vendor_Letters loop
         Code_Point := Letter (Wire.Words (1), I - 1);
         if Code_Point not in Character'Pos ('A') .. Character'Pos ('Z') and then
           Code_Point /= Character'Pos ('?')
         then
            return (Valid => False);
         end if;
         Item.Vendor (I) := Character'Val (Code_Point);
      end loop;
      for I in 1 .. Name_Length loop
         Code_Point :=
           (if I <= First_Word_Letters then Letter (Wire.Words (1), Vendor_Letters + I - 1)
            else Letter (Wire.Words (2), I - First_Word_Letters - 1));
         if Code_Point not in Character'Pos (Name_Character'First) .. Character'Pos (Name_Character'Last) or else
           (I > Item.Name_Last and then Code_Point /= Character'Pos (' '))
         then
            return (Valid => False);
         end if;
         Item.Name (I) := Character'Val (Code_Point);
         pragma Loop_Invariant
           (for all J in 1 .. I => (if J > Item.Name_Last then Item.Name (J) = ' '));
      end loop;
      return (True, Item);
   end Decode_Identity;

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
