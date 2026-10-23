package body Storehouse.Canonical with SPARK_Mode is
   use Interfaces;

   Information_Mask : constant := 16#1F#;   --  low five bits of the initial byte
   Bits_Per_Byte : constant := 8;

   function Head (Major : Major_Type; Value : Unsigned_64) return Byte_Array is
      Length : constant Head_Length := Length_Of (Value);
      Result : Byte_Array (1 .. Length) := [others => 0];
   begin
      Result (1) := Initial (Major, Value);
      for K in 1 .. Length - 1 loop
         Result (1 + K) := Byte_Of (Value, Length - 1 - K);
         pragma Loop_Invariant (Result (1) = Initial (Major, Value));
         pragma Loop_Invariant
           (for all J in 1 .. K => Result (1 + J) = Byte_Of (Value, Length - 1 - J));
      end loop;
      return Result;
   end Head;

   --  Major types this profile admits, from the initial byte's top bits.
   procedure Classify (Top : Byte; Major : out Major_Type; Valid : out Boolean)
   with Post => (if Valid then Code (Major) = Top);
   procedure Classify (Top : Byte; Major : out Major_Type; Valid : out Boolean) is
   begin
      Major := Unsigned_Integer;
      Valid := True;
      case Top is
         when 0 => Major := Unsigned_Integer;
         when 2 => Major := Byte_String;
         when 3 => Major := Text_String;
         when 4 => Major := Array_Of;
         when others => Valid := False;
      end case;
   end Classify;

   procedure Read_Head
     (Data : Byte_Array; First : Positive;
      Major : out Major_Type; Value : out Unsigned_64;
      Next : out Positive; Valid : out Boolean)
   is
      Initial_Byte : Byte;
      Information : Byte;
      Follow : Natural range 0 .. 8;
      Accumulated : Unsigned_64 := 0;
   begin
      Major := Unsigned_Integer;
      Value := 0;
      Next := First;
      Valid := False;
      if First not in Data'Range then
         return;
      end if;
      Initial_Byte := Data (First);
      Classify (Initial_Byte / Major_Shift, Major, Valid);
      if not Valid then
         return;
      end if;
      Valid := False;
      Information := Initial_Byte and Information_Mask;
      case Information is
         when 0 .. Immediate_Limit - 1 => Follow := 0;
         when One_Byte_Follows => Follow := 1;
         when Two_Bytes_Follow => Follow := 2;
         when Four_Bytes_Follow => Follow := 4;
         when Eight_Bytes_Follow => Follow := 8;
         when others => return;
      end case;
      if Follow = 0 then
         Value := Unsigned_64 (Information);
         Next := First + 1;
         Valid := True;
         pragma Assert (Initial_Byte = Initial (Major, Value));
         return;
      end if;
      if Data'Last - First < Follow then
         return;
      end if;
      for K in 1 .. Follow loop
         Accumulated := Shift_Left (Accumulated, Bits_Per_Byte) or Unsigned_64 (Data (First + K));
      end loop;
      --  Shortest form only: any other length is a second encoding. The
      --  bytes must be exactly what the proved encoder writes for the value.
      if Length_Of (Accumulated) /= 1 + Follow then
         return;
      end if;
      declare
         Expected : constant Byte_Array := Head (Major, Accumulated);
      begin
         for I in First .. First + Follow loop
            if Data (I) /= Expected (I - First + 1) then
               return;
            end if;
            pragma Loop_Invariant
              (for all J in First .. I => Data (J) = Expected (J - First + 1));
         end loop;
         pragma Assert (Data (First .. First + Follow) = Expected);
      end;
      Value := Accumulated;
      Next := First + 1 + Follow;
      Valid := True;
   end Read_Head;

end Storehouse.Canonical;
