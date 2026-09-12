with CuBit.Messages; use CuBit.Messages;
package body Presentation_Test_Policy is
   use Interfaces;
   function Verify_Buffer
     (Address : System.Address; Bytes, Frame : Unsigned_64) return Boolean
   is
      Pixels : array (Natural range 0 .. Natural (Bytes / 4) - 1) of Unsigned_32
        with Import, Volatile, Address => Address;
      function Fingerprint return Unsigned_64 is
         Value : Unsigned_64 := 16#CB17_5AFE#;
      begin
         for Pixel of Pixels loop
            Value := Rotate_Left (Value, 7) xor Unsigned_64 (Pixel);
            Value := Value * 16#100_0000_01B3#;
         end loop;
         return Value;
      end Fingerprint;
      Before : constant Unsigned_64 := Fingerprint;
      Ignored : Unsigned_64;
   begin
      debugPrint ("display-test: hold" & Frame'Image & ASCII.LF);
      Ignored := syscall (SYSCALL_SLEEP, 75);
      if Fingerprint /= Before then
         debugPrint ("TEST: FAIL display transfer mutated while held" & ASCII.LF);
         return False;
      end if;
      debugPrint ("display-test: stable" & Frame'Image & ASCII.LF);
      return True;
   end Verify_Buffer;
end Presentation_Test_Policy;
