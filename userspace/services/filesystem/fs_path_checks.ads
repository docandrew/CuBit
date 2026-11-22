------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Path checks of the filesystem service, proved (SPARK): ".." traversal
--  detection and the canonical relative path watches and change events
--  are matched by.
------------------------------------------------------------------------------
package Fs_Path_Checks with SPARK_Mode, Pure is

   Separator : constant Character := '/';
   Dot       : constant Character := '.';

   --  Path has a ".." component at I: the two dots, each end at the path's
   --  end or a separator.
   function Parent_Component_At (Path : String; I : Positive) return Boolean is
     (Path (I) = Dot and then Path (I + 1) = Dot and then
      (I = Path'First or else Path (I - 1) = Separator) and then
      (I + 1 = Path'Last or else Path (I + 2) = Separator))
     with Pre => I >= Path'First and then I < Path'Last;

   --  Whether the path contains a ".." component (exactly "..", a leading
   --  "../", a trailing "/.." or an inner "/../").
   function Has_Traversal (Path : String) return Boolean
     with Post => Has_Traversal'Result =
                  (Path'Length >= 2 and then
                   (for some I in Path'First .. Path'Last - 1 =>
                      Parent_Component_At (Path, I)));

   --  Path without empty components: no leading, trailing or doubled
   --  separator; the other characters in order.
   function Canonical (Path : String) return String
     with Post => Canonical'Result'First = 1 and then
                  Canonical'Result'Length <= Path'Length and then
                  (for all K in Canonical'Result'Range =>
                     (if Canonical'Result (K) = Separator then
                        K > 1 and then K < Canonical'Result'Last and then
                        Canonical'Result (K - 1) /= Separator));

end Fs_Path_Checks;
