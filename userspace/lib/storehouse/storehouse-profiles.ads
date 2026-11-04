with Storehouse.Paths;

--  Profiles and their generations (docs/ccl-build.md, "Generations"). A
--  profile is a named sequence of generations; generation N is a file whose
--  name is N's decimal image, and the profile's current generation is a
--  pointer replaced by one rename. The names are proved to be single path
--  components, so a profile name can never climb out of the Storehouse,
--  and every number has exactly one name.
package Storehouse.Profiles with SPARK_Mode, Pure is

   --  Lowercase kebab, like component names: a letter, then letters,
   --  digits and single dashes, not ending in a dash.
   Maximum_Name_Length : constant := 64;

   function Is_Name_Letter (C : Character) return Boolean is (C in 'a' .. 'z' | '0' .. '9' | '-');

   function Is_Profile_Name (S : String) return Boolean is
     (S'Length in 1 .. Maximum_Name_Length
      and then (for all C of S => Is_Name_Letter (C))
      and then S (S'First) in 'a' .. 'z'
      and then S (S'Last) /= '-'
      and then (for all I in S'First .. S'Last - 1 => not (S (I) = '-' and then S (I + 1) = '-')));

   --  A profile name is one relative path component.
   procedure Lemma_Profile_Name_Is_Component (S : String)
   with Ghost, Global => null,
        Pre => Is_Profile_Name (S),
        Post => Storehouse.Paths.Is_Relative (S) and then (for all C of S => C /= Storehouse.Paths.Separator);

   --  Nine decimal digits: generation numbers never wrap, and a name's
   --  value fits an Integer without overflow.
   Maximum_Digits : constant := 9;
   Last_Generation : constant := 999_999_999;
   Decimal_Base : constant := 10;
   subtype Generation_Number is Positive range 1 .. Last_Generation;
   --  A count of generations, or the latest number when 0 means "none yet".
   subtype Generation_Count is Natural range 0 .. Last_Generation;

   function Is_Digit (C : Character) return Boolean is (C in '0' .. '9');

   --  The decimal image without leading zeros: one name per number.
   function Is_Generation_Name (S : String) return Boolean is
     (S'Length in 1 .. Maximum_Digits
      and then (for all C of S => Is_Digit (C))
      and then S (S'First) /= '0');

   function Image (N : Generation_Number) return String
   with Post => Is_Generation_Name (Image'Result) and then Image'Result'First = 1;

   function Value (S : String) return Generation_Number
   with Pre => Is_Generation_Name (S);

   --  The number after Latest (0: the first generation).
   function Next (Latest : Generation_Count) return Generation_Number is (Latest + 1)
   with Pre => Latest < Last_Generation;

end Storehouse.Profiles;
