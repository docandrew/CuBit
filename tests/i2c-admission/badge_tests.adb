pragma Ada_2022;
with Ada.Text_IO;
with I2C_Channel_Badges;
procedure Badge_Tests is
   package B renames I2C_Channel_Badges;
   use type B.Word;
   use type B.Address_Value;
   use type B.Address_Mode;
   use type B.Target;
   use type B.Encode_Status;
   use type B.Decode_Status;
   Speeds : constant array (Positive range <>) of B.Hertz :=
     [1, 100_000, 400_000, 1_000_000, 3_400_000, B.Hertz'Last];
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin if not OK then raise Program_Error with Why; end if; Checks := Checks + 1; end Check;
   procedure Round_Trip (Value : B.Target) is
      E : constant B.Encoding := B.Encode (Value);
      D : B.Decoding;
   begin
      Check ((E.Status = B.Encoded) = B.Valid (Value), "total encode admission");
      if E.Status = B.Encoded then
         D := B.Decode (E.Badge);
         Check (D.Status = B.Decoded and then D.Value = Value, "all fields round trip without narrowing");
         Check (B.Encode (D.Value).Badge = E.Badge and B.Valid_Word (E.Badge), "canonical decode/reencode");
      end if;
   end Round_Trip;
   Value : B.Target;
   Badge : B.Word;
begin
   for Mode in B.Address_Mode loop
      for Address in B.Address_Value loop
         for Speed of Speeds loop Round_Trip ((Mode, Address, Speed)); end loop;
         Round_Trip ((Mode, Address, 0));
      end loop;
   end loop;
   Value := (B.Seven_Bit, 16#15#, 400_000);
   Badge := B.Encode (Value).Badge;
   Check (Badge = 16#4932_1015_0006_1A80#, "golden little-field numeric value");
   Check (B.Decode (0).Status = B.Wrong_Domain, "unrelated zero badge");
   Check (B.Decode (Badge + B.Domain_Base).Status = B.Wrong_Domain, "wrong service domain");
   Check (B.Decode (Badge + B.Version_Base).Status = B.Wrong_Version, "unsupported version");
   Check (B.Decode (Badge + B.Reserved_Base).Status = B.Reserved_Bits, "reserved bit cannot alias encoding");
   Check (B.Decode (Badge - B.Word (Value.Maximum_Hz)).Status = B.Zero_Speed, "zero frequency refused");
   Check (B.Decode (B.Domain * B.Domain_Base + B.Version * B.Version_Base +
      128 * B.Address_Base + 1).Status = B.Invalid_Address, "seven-bit field cannot truncate");
   Round_Trip ((B.Ten_Bit, B.Address_Value'Last, B.Hertz'Last));
   Ada.Text_IO.Put_Line ("I2C channel badge checks" & Checks'Image);
end Badge_Tests;
