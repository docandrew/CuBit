with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

--  lexicon.* and its commands (CCL.Interfaces.Lexicon, docs/lexicon.md):
--  each call asks CCL_Lexicon_Source for answer pages and builds the typed
--  result. A result holds what one CCL value image can (CCL.Objects): an
--  entry shows its senses from first_sense on, as many as fit, and says
--  how many there are (total_senses); lexicon.dict-page continues.
package CCL_Lexicon_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Lexicon_Bindings;
