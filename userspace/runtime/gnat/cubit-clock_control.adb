pragma Ada_2022;
package body CuBit.Clock_Control with SPARK_Mode is
   Authenticated_Bit : constant Unsigned_64 := Shift_Left (1, 40);

   function Encode (Item : Sample) return Words is
     [Item.UTC_MS, Item.Observed_Monotonic_MS,
      Unsigned_64 (Item.Uncertainty_MS) or
      Shift_Left (Unsigned_64 (Item.Sources), 32) or
      (if Item.Authenticated then Authenticated_Bit else 0),
      0];

   procedure Decode
     (Value : Words; Item : out Sample; Success : out Boolean)
   is
      Sources_Field : constant Unsigned_64 :=
        Shift_Right (Value (2), 32) and 255;
   begin
      Item := (others => <>);
      Success := Value (3) = 0 and then Shift_Right (Value (2), 41) = 0;
      if Success then
         --  With bits 41..63 clear, word 2 is exactly its three fields.
         pragma Assert (Value (2) < 2 ** 41);
         pragma Assert (Sources_Field <= 255);
         pragma Assert
           (Shift_Left (Sources_Field, 32) =
              (Value (2) and 16#00FF_0000_0000#));
         pragma Assert
           (Value (2) =
              ((Value (2) and 16#FFFF_FFFF#) or
               (Value (2) and 16#00FF_0000_0000#) or
               (Value (2) and Authenticated_Bit)));
         Item :=
           (UTC_MS => Value (0),
            Observed_Monotonic_MS => Value (1),
            Uncertainty_MS => Unsigned_32 (Value (2) and 16#FFFF_FFFF#),
            Sources => Natural (Sources_Field),
            Authenticated => (Value (2) and Authenticated_Bit) /= 0);
         pragma Assert (Unsigned_64 (Item.Sources) = Sources_Field);
         pragma Assert
           (Unsigned_64 (Item.Uncertainty_MS) = (Value (2) and 16#FFFF_FFFF#));
         pragma Assert
           ((if Item.Authenticated then Authenticated_Bit else 0) =
              (Value (2) and Authenticated_Bit));
         pragma Assert (Encode (Item) (2) = Value (2));
      end if;
   end Decode;
end CuBit.Clock_Control;
