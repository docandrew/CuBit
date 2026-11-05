------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Net_Link with SPARK_Mode is

   procedure Lemma_Address_Round_Trip (A : Hardware_Address) is null;

   ---------------------------------------------------------------------------
   --  Names
   ---------------------------------------------------------------------------
   Hex_Digits : constant String (1 .. 16) := "0123456789abcdef";
   Nibble_Bits : constant := 4;
   Nibble_Mask : constant Unsigned_32 := 16#F#;
   PCI_Bus_Shift      : constant := 8;
   PCI_Device_Shift   : constant := 3;
   PCI_Device_Mask    : constant Unsigned_32 := 16#1F#;
   PCI_Function_Mask  : constant Unsigned_32 := 16#7#;
   PCI_Byte_Mask      : constant Unsigned_32 := 16#FF#;

   function Hex (V : Unsigned_32) return Character is
     (Hex_Digits (Natural (V and Nibble_Mask) + 1));

   function Name_Of (Identity : Stable_Identity; Factory : Hardware_Address)
     return Interface_Name
   is
      N : Interface_Name;
   begin
      case Identity.Kind is
         when PCI_Function =>
            declare
               L   : constant Unsigned_32 := Identity.Location;
               Bus : constant Unsigned_32 := Shift_Right (L, PCI_Bus_Shift) and PCI_Byte_Mask;
               Dev : constant Unsigned_32 := Shift_Right (L, PCI_Device_Shift) and PCI_Device_Mask;
               Fn  : constant Unsigned_32 := L and PCI_Function_Mask;
               Image : constant String (1 .. 11) :=
                 ['p', 'c', 'i', '-', Hex (Shift_Right (Bus, Nibble_Bits)), Hex (Bus), '-',
                  Hex (Shift_Right (Dev, Nibble_Bits)), Hex (Dev), '-', Hex (Fn)];
            begin
               N.Text (1 .. Image'Length) := Image;
               N.Length := Image'Length;
            end;
         when Factory_Address =>
            N.Text (1 .. 4) := "mac-";
            for I in Octet_Index loop
               N.Text (5 + 2 * I) := Hex (Shift_Right (Unsigned_32 (Factory (I)), Nibble_Bits));
               N.Text (6 + 2 * I) := Hex (Unsigned_32 (Factory (I)));
            end loop;
            N.Length := Name_Characters;
      end case;
      return N;
   end Name_Of;

   ---------------------------------------------------------------------------
   --  Attach
   ---------------------------------------------------------------------------
   function Field (W : Unsigned_64; Shift : Natural; Mask : Unsigned_64) return Unsigned_64 is
     (Shift_Right (W, Shift) and Mask)
   with Pre => Shift < 64;

   function Decode (W : Words3) return Decoded_Attach is
      Kind_Value  : constant Unsigned_64 := Field (W (0), Kind_Shift, Field_Mask_8);
      Size_Value  : constant Unsigned_64 := Field (W (0), MTU_Shift, Field_Mask_16);
      Offload_Value : constant Unsigned_64 := Shift_Right (W (0), Offload_Shift);
      Identity_Value : constant Unsigned_64 := Field (W (2), Kind_Shift, Field_Mask_8);
      Location_Value : constant Unsigned_64 := Shift_Right (W (2), Location_Shift);
      R : Attach_Request;
   begin
      if Kind_Value = Unsigned_64 (Link_Kind'Enum_Rep (Ethernet)) then
         R.Kind := Ethernet;
      elsif Kind_Value = Unsigned_64 (Link_Kind'Enum_Rep (Wireless)) then
         R.Kind := Wireless;
      else
         return (Valid => False);
      end if;
      if Size_Value not in Minimum_MTU .. Maximum_MTU
        or else (Offload_Value and not Offload_Bits) /= 0
        or else not Valid_Address_Word (W (1))
      then
         return (Valid => False);
      end if;
      R.Size := MTU (Size_Value);
      R.Offload := (Receive_Checksum  => (Offload_Value and Receive_Checksum_Bit) /= 0,
                    Transmit_Checksum => (Offload_Value and Transmit_Checksum_Bit) /= 0,
                    Segmentation      => (Offload_Value and Segmentation_Bit) /= 0);
      R.Address := Address_Of (W (1));
      if Identity_Value = Unsigned_64 (Identity_Kind'Enum_Rep (PCI_Function))
        and then Location_Value <= Unsigned_64 (PCI_Location'Last)
      then
         R.Identity := (Kind => PCI_Function, Location => PCI_Location (Location_Value));
      elsif Identity_Value = Unsigned_64 (Identity_Kind'Enum_Rep (Factory_Address))
        and then Location_Value = 0
      then
         R.Identity := (Kind => Factory_Address);
      else
         return (Valid => False);
      end if;
      --  Every bit is accounted for only if re-encoding gives W back
      --  (unused bits between fields must be zero).
      if not Valid (R) or else Encode (R) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Request => R);
   end Decode;

   ---------------------------------------------------------------------------
   --  The link report
   ---------------------------------------------------------------------------
   function Byte_Of (W : Unsigned_32; K : Natural) return Unsigned_8 is
     (Unsigned_8 (Shift_Right (W, Octet_Bits * K) and Byte_Mask_32))
   with Pre => K < Octets_Per_Word;

   function Pack (B0, B1, B2, B3 : Unsigned_8) return Unsigned_32 is
     (Unsigned_32 (B0) or Shift_Left (Unsigned_32 (B1), 8) or
      Shift_Left (Unsigned_32 (B2), 16) or Shift_Left (Unsigned_32 (B3), 24));

   function Encode (S : Link_State) return Report_Words is
      W : Report_Words := [others => 0];
   begin
      case S.Condition is
         when Down =>
            W (Condition_Word) := Unsigned_32 (Link_Condition'Enum_Rep (Down)) or
              Shift_Left (Unsigned_32 (Down_Reason'Enum_Rep (S.Reason)), Reason_Shift);
         when Dormant =>
            W (Condition_Word) := Unsigned_32 (Link_Condition'Enum_Rep (Dormant));
         when Up =>
            W (Condition_Word) := Unsigned_32 (Link_Condition'Enum_Rep (Up)) or
              Shift_Left (Unsigned_32 (Link_Kind'Enum_Rep (S.Detail.Kind)), Up_Kind_Shift);
            W (Speed_Word) := S.Detail.Speed;
            if S.Detail.Kind = Wireless then
               declare
                  N : Network_Identity renames S.Detail.Network;
                  B : Hardware_Address renames S.Detail.BSSID;
               begin
                  W (Network_Word) := Unsigned_32 (Wireless_Security'Enum_Rep (N.Security)) or
                    Shift_Left (Unsigned_32 (N.Length), Length_Shift);
                  W (BSSID_Low_Word) := Pack (B (0), B (1), B (2), B (3));
                  W (BSSID_High_Word) := Pack (B (4), B (5), 0, 0);
                  for K in 0 .. SSID_Octets / Octets_Per_Word - 1 loop
                     W (SSID_First_Word + K) :=
                       Pack (N.SSID (K * Octets_Per_Word + 1), N.SSID (K * Octets_Per_Word + 2),
                             N.SSID (K * Octets_Per_Word + 3), N.SSID (K * Octets_Per_Word + 4));
                  end loop;
               end;
            end if;
      end case;
      return W;
   end Encode;

   function Decode (W : Report_Words) return Decoded_Report is
      Condition_Value : constant Unsigned_32 := W (Condition_Word) and Byte_Mask_32;
      Reason_Value    : constant Unsigned_32 :=
        Shift_Right (W (Condition_Word), Reason_Shift) and Byte_Mask_32;
      Kind_Value      : constant Unsigned_32 := Shift_Right (W (Condition_Word), Up_Kind_Shift);
      S : Link_State;
   begin
      if Condition_Value = Unsigned_32 (Link_Condition'Enum_Rep (Down)) then
         declare
            Reason : Down_Reason := Not_Reported;
            Found  : Boolean := False;
         begin
            for R in Down_Reason loop
               if Reason_Value = Unsigned_32 (Down_Reason'Enum_Rep (R)) then
                  Reason := R;
                  Found := True;
               end if;
            end loop;
            if not Found then
               return (Valid => False);
            end if;
            S := (Condition => Down, Reason => Reason);
         end;
      elsif Condition_Value = Unsigned_32 (Link_Condition'Enum_Rep (Dormant)) then
         S := (Condition => Dormant);
      elsif Condition_Value = Unsigned_32 (Link_Condition'Enum_Rep (Up)) then
         if Kind_Value = Unsigned_32 (Link_Kind'Enum_Rep (Ethernet)) then
            S := (Condition => Up,
                  Detail => (Kind => Ethernet, Speed => W (Speed_Word)));
         elsif Kind_Value = Unsigned_32 (Link_Kind'Enum_Rep (Wireless)) then
            declare
               Security_Value : constant Unsigned_32 := W (Network_Word) and Byte_Mask_32;
               Length_Value   : constant Unsigned_32 := Shift_Right (W (Network_Word), Length_Shift);
               N : Network_Identity;
               Found : Boolean := False;
            begin
               for Sec in Wireless_Security loop
                  if Security_Value = Unsigned_32 (Wireless_Security'Enum_Rep (Sec)) then
                     N.Security := Sec;
                     Found := True;
                  end if;
               end loop;
               if not Found or else Length_Value > SSID_Octets then
                  return (Valid => False);
               end if;
               N.Length := SSID_Length (Length_Value);
               for K in 0 .. SSID_Octets / Octets_Per_Word - 1 loop
                  for J in 0 .. Octets_Per_Word - 1 loop
                     N.SSID (K * Octets_Per_Word + J + 1) := Byte_Of (W (SSID_First_Word + K), J);
                  end loop;
               end loop;
               if not Normal (N) then
                  return (Valid => False);
               end if;
               S := (Condition => Up,
                     Detail => (Kind => Wireless, Speed => W (Speed_Word),
                                BSSID => [Byte_Of (W (BSSID_Low_Word), 0),
                                          Byte_Of (W (BSSID_Low_Word), 1),
                                          Byte_Of (W (BSSID_Low_Word), 2),
                                          Byte_Of (W (BSSID_Low_Word), 3),
                                          Byte_Of (W (BSSID_High_Word), 0),
                                          Byte_Of (W (BSSID_High_Word), 1)],
                                Network => N));
            end;
         else
            return (Valid => False);
         end if;
      else
         return (Valid => False);
      end if;
      --  Unused fields and bits must be zero: re-encoding gives W back.
      if Encode (S) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, State => S);
   end Decode;

end CuBit.Net_Link;
