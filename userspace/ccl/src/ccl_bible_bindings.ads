with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;

--  bible.* (CCL.Interfaces.Bible, APP-005): references parsed by the proved
--  Bible_Parser and checked against each translation's versification,
--  verses and passages read from the Bible images (CCL_Bible_Source), and
--  search over their indexes. A result holds what one CCL value image can
--  (CCL.Objects): a passage or a search gives as many whole verses as fit
--  from its start, and says how many there are (total).
package CCL_Bible_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Bible_Bindings;
