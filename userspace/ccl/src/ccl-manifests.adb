with CCL.Manifests.Keywords;
with CCL.Manifests.Typed;

package body CCL.Manifests with SPARK_Mode => On is
   --  The first form names the notation: (Executable_Manifest ...), or the
   --  CCL type declarations a typed manifest's parameters use ahead of it,
   --  (type ...), is a typed CCL program; anything else goes to the keyword
   --  reader until every manifest is typed.
   function Typed_Form (Source : String) return Boolean is
      KEYWORD : constant String := "(Executable_Manifest";
      DECLARATION : constant String := "(type ";
      Cursor : Natural := Source'First;
   begin
      while Cursor <= Source'Last loop
         if Source (Cursor) = '#' then
            while Cursor <= Source'Last and then Source (Cursor) /= ASCII.LF loop
               Cursor := Cursor + 1;
            end loop;
         elsif Source (Cursor) in ' ' | ASCII.HT | ASCII.LF | ASCII.CR then
            Cursor := Cursor + 1;
         else
            return (Source'Last - Cursor + 1 >= KEYWORD'Length and then
                    Source (Cursor .. Cursor + KEYWORD'Length - 1) = KEYWORD)
              or else (Source'Last - Cursor + 1 >= DECLARATION'Length and then
                       Source (Cursor .. Cursor + DECLARATION'Length - 1) = DECLARATION);
         end if;
      end loop;
      return False;
   end Typed_Form;

   procedure Compile
     (Source, Catalog_Source : String; Result : out Compilation_Result; Schema_Source : String := "") is
   begin
      if Typed_Form (Source) then
         Typed.Compile (Source, Catalog_Source, Schema_Source, Result);
      else
         Keywords.Compile (Source, Catalog_Source, Result);
      end if;
   end Compile;
end CCL.Manifests;
