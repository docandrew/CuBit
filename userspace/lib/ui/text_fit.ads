--  Fit a one-line label into a width: the whole text when it fits, else its
--  longest prefix (that one more character would not keep fitting) followed
--  by Ellipsis, else nothing. Width is the caller's one measure (CuBit.Fonts.
--  Width: kerned, 1/64-pixel advances), always applied to the whole candidate
--  string, so the ellipsis is measured in context and single-character
--  widths are never summed. Hint is only a starting guess for the prefix
--  length (CuBit.Fonts.Boundary_At); any value gives the same guarantees.
generic
   with function Width (Text : String) return Natural;
   with function Hint (Text : String; X : Natural) return Natural;
package Text_Fit with SPARK_Mode, Pure is
   --  The bundled faces cover printable ASCII only: no U+2026.
   Ellipsis : constant String := "...";
   Ellipsis_Length : constant := 3;
   pragma Compile_Time_Error (Ellipsis'Length /= Ellipsis_Length, "Ellipsis_Length");
   --  Room after the text's last index for the appended ellipsis.
   Last_Index : constant := Positive'Last - Ellipsis_Length;

   --  Text's first Count characters followed by Ellipsis fit Available.
   function Prefix_Fits (Text : String; Count : Natural; Available : Natural) return Boolean is
     (Width (Text (Text'First .. Text'First - 1 + Count) & Ellipsis) <= Available)
     with Pre => Text'Last <= Last_Index and then Count <= Text'Length;

   --  How many characters of Text are shown.
   function Kept (Text : String; Available : Natural) return Natural
     with Pre => Text'Last <= Last_Index,
          Post => Kept'Result <= Text'Length and then
            (if Text'Length = 0 or else Width (Text) <= Available then Kept'Result = Text'Length
             else Kept'Result < Text'Length and then
               (if Prefix_Fits (Text, Kept'Result, Available) then
                  Kept'Result + 1 = Text'Length or else
                  not Prefix_Fits (Text, Kept'Result + 1, Available)
                else Kept'Result = 0));

   --  The text drawn in Available: Text, a prefix & Ellipsis, or "".
   function Fitted (Text : String; Available : Natural) return String
     with Pre => Text'Last <= Last_Index,
          Post => (if Text'Length = 0 or else Width (Text) <= Available then Fitted'Result = Text
                   elsif Prefix_Fits (Text, Kept (Text, Available), Available) then
                     Fitted'Result = Text (Text'First .. Text'First - 1 + Kept (Text, Available)) & Ellipsis
                   else Fitted'Result'Length = 0);

   --  The same from the other end, for paths (the folder you are in is at
   --  the end): Ellipsis followed by Text's last Count characters.
   function Suffix_Fits (Text : String; Count : Natural; Available : Natural) return Boolean is
     (Width (Ellipsis & Text (Text'Last + 1 - Count .. Text'Last)) <= Available)
     with Pre => Text'Last <= Last_Index and then Count <= Text'Length;

   function Kept_Tail (Text : String; Available : Natural) return Natural
     with Pre => Text'Last <= Last_Index,
          Post => Kept_Tail'Result <= Text'Length and then
            (if Text'Length = 0 or else Width (Text) <= Available then Kept_Tail'Result = Text'Length
             else Kept_Tail'Result < Text'Length and then
               (if Suffix_Fits (Text, Kept_Tail'Result, Available) then
                  Kept_Tail'Result + 1 = Text'Length or else
                  not Suffix_Fits (Text, Kept_Tail'Result + 1, Available)
                else Kept_Tail'Result = 0));

   --  Text, Ellipsis & a suffix, or "".
   function Fitted_Tail (Text : String; Available : Natural) return String
     with Pre => Text'Last <= Last_Index,
          Post => (if Text'Length = 0 or else Width (Text) <= Available then Fitted_Tail'Result = Text
                   elsif Suffix_Fits (Text, Kept_Tail (Text, Available), Available) then
                     Fitted_Tail'Result =
                       Ellipsis & Text (Text'Last + 1 - Kept_Tail (Text, Available) .. Text'Last)
                   else Fitted_Tail'Result'Length = 0);
end Text_Fit;
