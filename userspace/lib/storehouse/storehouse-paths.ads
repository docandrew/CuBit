--  The names a build may use (docs/ccl-build.md, "Steps"). A step's files
--  are named relative to its directory, never absolutely, and a path cannot
--  climb out of it. Literal arguments cannot name files at all: a file
--  enters a step only as a typed input or output, so it is hashed.
package Storehouse.Paths with SPARK_Mode, Pure is

   --  Linux PATH_MAX parity (docs/self-hosting.md: paths are short).
   Maximum_Length : constant := 4096;
   Separator : constant Character := '/';

   function Is_Name_Character (C : Character) return Boolean is
     (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' | '+');

   --  A component of only dots ("." or "..") starting at I.
   function Dot_Component (Path : String; I : Positive) return Boolean is
     (Path (I) = '.'
      and then (I = Path'First or else Path (I - 1) = Separator)
      and then (I = Path'Last or else Path (I + 1) = Separator
                or else (Path (I + 1) = '.'
                         and then (I + 1 = Path'Last or else Path (I + 2) = Separator))))
   with Pre => I in Path'Range;

   --  Relative, nonempty components of name characters, none "." or "..".
   function Is_Relative (Path : String) return Boolean is
     (Path'Length in 1 .. Maximum_Length
      and then (for all C of Path => C = Separator or else Is_Name_Character (C))
      and then Path (Path'First) /= Separator
      and then Path (Path'Last) /= Separator
      and then (for all I in Path'First .. Path'Last - 1 =>
                  not (Path (I) = Separator and then Path (I + 1) = Separator))
      and then (for all I in Path'Range => not Dot_Component (Path, I)));

   --  Literal argument text: printable, and no separator, so no path.
   function Is_Literal (Text : String) return Boolean is
     (Text'Length <= Maximum_Length
      and then (for all C of Text => C in ' ' .. '~' and then C /= Separator));

end Storehouse.Paths;
