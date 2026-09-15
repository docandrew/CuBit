with CCL.Catalog;
--  Bounded, inert call-head inspection for editor hints, not a parser or
--  authority decision. Offset counts source characters before the caret.
package CCL.Call_Context with SPARK_Mode is
   Maximum_Source : constant := 1_024;
   Maximum_Name : constant := CCL.Catalog.MAX_NAME_LENGTH * 2 + 1;
   type Context is record
      Name : String (1 .. Maximum_Name) := [others => ' '];
      Length : Natural range 0 .. Maximum_Name := 0;
      Arguments_Started : Boolean := False;
      Available : Boolean := False;
   end record;
   procedure Inspect (Source : String; Offset : Natural; Result : out Context);
end CCL.Call_Context;
