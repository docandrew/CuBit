--  Alternative source views, not an alternative evaluator. Both readers
--  must pass the existing CCL analyzer before a conversion can be published.
package CCL.Language.Views with SPARK_Mode => On is
   type Surface is (Lisp, Basic);
   --  Pretty-printed views run longer than the canonical source.
   Maximum_View_Length : constant := 2 * CCL.Language.MAX_SOURCE_LENGTH;
   subtype View_Length is Natural range 0 .. Maximum_View_Length;
   type Text is record
      Length : View_Length := 0;
      Data : String (1 .. Maximum_View_Length) := [others => ' '];
   end record;
   type Span is record
      First, After_Last : Natural range 0 .. Maximum_View_Length + 1 := 0;
   end record;
   type Node_Spans is array (Node_Index) of Span;
   type Conversion_Status is
     (Converted, Invalid_Source, Capacity_Exceeded);
   type Conversion is record
      Status : Conversion_Status := Invalid_Source;
      Diagnostic : Diagnostic_Code := No_Diagnostic;
      Position : Natural := 0;
      Rendered : Text;
      --  Stable, compact Lisp spelling used for interpretation/compilation.
      --  Comments are retained in Rendered, not in this executable spelling.
      Canonical : Text;
      Input_Nodes, Output_Nodes : Node_Spans := [others => (others => 0)];
   end record;
   function Detect (Source : String) return Surface;
   --  Check => False (BASIC into Lisp only) stops after reading: Canonical is
   --  the lowered Lisp, not yet analyzed. A REPL session uses this to check
   --  the entry in the context of its kept definitions and values.
   procedure Convert
     (Source : String; From, Into : Surface;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Result : out Conversion;
      Check : Boolean := True);
   --  From = Into formats the current view. Rendered uses two-space
   --  indentation and an 80-column wrapping target (literals stay intact).
   --  Comments retain their order/text but currently move above the program.
   --  On any failure, callers must retain their original document unchanged.
end CCL.Language.Views;
