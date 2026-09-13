with CCL.Language;

--  Bounded declaration scanner. Scalar expressions use the real CCL evaluator.
--  No host adapter, file access, or capability grant is available here.
package CCL.Declarations with SPARK_Mode => On is
   type Format_Version is (V1);
   type Format_Selection (Supported : Boolean := False) is record
      case Supported is
         when True => Version : Format_Version;
         when False => null;
      end case;
   end record;
   --  Header versions are symbolic tags, not evaluated CCL expressions.
   function Select_Format (Token : String) return Format_Selection;
   MAX_SOURCE : constant := 8_192;
   type Diagnostic_Code is
     (No_Error, Source_Too_Long, Unexpected_End, Expected_Form,
      Invalid_Expression, Nesting_Too_Deep);
   type Symbol is record
      Length : Natural range 0 .. 64 := 0;
      Data : String (1 .. 64) := [others => ' '];
   end record;
   type Scanner is private;
   procedure Start (Item : out Scanner; Source : String);
   procedure Open_Form (Item : in out Scanner);
   procedure Close_Form (Item : in out Scanner);
   procedure Read_Symbol (Item : in out Scanner; Value : out Symbol);
   procedure Evaluate
     (Item : in out Scanner; Value : out CCL.Language.Interpretation_Result);
   function Matches (Value : Symbol; Text : String) return Boolean;
   function At_Close (Item : Scanner) return Boolean;
   function At_End (Item : Scanner) return Boolean;
   function Failed (Item : Scanner) return Boolean;
   function Diagnostic (Item : Scanner) return Diagnostic_Code;
   function Position (Item : Scanner) return Positive;
private
   type Scanner is record
      Text : String (1 .. MAX_SOURCE) := [others => ' '];
      Last : Natural range 0 .. MAX_SOURCE := 0;
      Cursor : Positive range 1 .. MAX_SOURCE + 1 := 1;
      Error : Diagnostic_Code := No_Error;
   end record;
end CCL.Declarations;
