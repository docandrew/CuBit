with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Interfaces.Console;

--  console.* (interfaces/console.schema): the console's own endpoints,
--  answered by the console that instantiates this with its window and view.
generic
   with procedure Set_Title (Text : String);
   with function Notation return CCL.Interfaces.Console.Notation;
   with procedure Set_Notation (Value : CCL.Interfaces.Console.Notation);
   with function Theme return CCL.Interfaces.Console.Theme;
   with procedure Set_Theme (Value : CCL.Interfaces.Console.Theme);
   with function Stats return CCL.Interfaces.Console.Statistics;
package CCL_Console_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
end CCL_Console_Bindings;
