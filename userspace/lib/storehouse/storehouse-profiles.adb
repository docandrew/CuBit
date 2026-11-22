package body Storehouse.Profiles with SPARK_Mode is

   procedure Lemma_Profile_Name_Is_Component (S : String) is
   begin
      pragma Assert (for all C of S => C /= Storehouse.Paths.Separator);
      pragma Assert (for all I in S'Range => not Storehouse.Paths.Dot_Component (S, I));
   end Lemma_Profile_Name_Is_Component;

   type Power_Table is array (0 .. Maximum_Digits) of Positive;
   Powers : constant Power_Table :=
     [1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000, 1_000_000_000];

   function Digit_Image (D : Natural) return Character is
     (Character'Val (Character'Pos ('0') + D))
   with Pre => D < Decimal_Base, Post => Is_Digit (Digit_Image'Result) and then (Digit_Image'Result = '0') = (D = 0);

   function Image (N : Generation_Number) return String is
      Result : String (1 .. Maximum_Digits) := [others => '0'];
      Rest : Natural := N;
      First : Positive range 1 .. Maximum_Digits := Maximum_Digits;
   begin
      for I in reverse 1 .. Maximum_Digits loop
         pragma Loop_Invariant (Rest in 1 .. Powers (I) - 1);
         pragma Loop_Invariant (for all J in I + 1 .. Maximum_Digits => Is_Digit (Result (J)));
         Result (I) := Digit_Image (Rest mod Decimal_Base);
         if Rest < Decimal_Base then
            --  Rest is the leading digit, so not zero.
            First := I;
            exit;
         end if;
         Rest := Rest / Decimal_Base;
      end loop;
      pragma Assert (Result (First) /= '0');
      declare
         Length : constant Positive := Maximum_Digits - First + 1;
         Image_Text : constant String (1 .. Length) := Result (First .. Maximum_Digits);
      begin
         return Image_Text;
      end;
   end Image;

   function Value (S : String) return Generation_Number is
      Result : Natural := 0;
   begin
      for I in S'Range loop
         pragma Loop_Invariant (if I = S'First then Result = 0 else Result in 1 .. Powers (I - S'First) - 1);
         Result := Result * Decimal_Base + (Character'Pos (S (I)) - Character'Pos ('0'));
      end loop;
      return Result;
   end Value;

end Storehouse.Profiles;
