with CuBit.UI.Editor.Documents;

package CuBit.UI.Editor.Search with SPARK_Mode is
   MAX_PATTERN_LENGTH : constant := 256;
   subtype Search_Position is Natural range
     0 .. CuBit.UI.Editor.Documents.MAX_DOCUMENT_CAPACITY + 1;

   type Search_Status is
     (Match_Found, No_Match, Empty_Pattern, Pattern_Too_Long);

   type Search_Result is record
      Status : Search_Status := No_Match;
      First  : Search_Position := 0;
      Last   : Search_Position := 0;
   end record;

   procedure Find_Next
     (Text, Pattern : String;
      Start_At : CuBit.UI.Editor.Documents.Document_Position;
      Wrap : Boolean;
      Whole_Word : Boolean;
      Case_Sensitive : Boolean;
      Result : out Search_Result)
   with Pre =>
     Text'Length <= CuBit.UI.Editor.Documents.MAX_DOCUMENT_CAPACITY and then
     Start_At <= Text'Length + 1;
end CuBit.UI.Editor.Search;
