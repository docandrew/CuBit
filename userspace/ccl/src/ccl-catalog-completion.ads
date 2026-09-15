--  Read-only inspection of an already-authorized catalog snapshot. No lookup
--  of other catalogs, host calls, or runtime binding acquisition occurs here.
package CCL.Catalog.Completion with SPARK_Mode is
   Maximum_Qualified_Name : constant := MAX_NAME_LENGTH * 2 + 1;
   subtype Qualified_Length is Natural range 0 .. Maximum_Qualified_Name;
   type Suggestion is record
      Name : String (1 .. Maximum_Qualified_Name) := [others => ' '];
      Length : Qualified_Length := 0;
      Contract : Resolved_Operation;
   end record;
   Maximum_Suggestions : constant := 16;
   subtype Suggestion_Count is Natural range 0 .. Maximum_Suggestions;
   subtype Match_Count is Natural range 0 .. MAX_INTERFACES * MAX_OPERATIONS;
   type Suggestion_Array is array (Positive range 1 .. Maximum_Suggestions) of Suggestion;
   type Match_List is record
      Items : Suggestion_Array;
      Count : Suggestion_Count := 0;
      Total : Match_Count := 0;
   end record;
   --  Exact, case-sensitive prefix. Empty prefix enumerates the supplied view.
   --  Results follow publication order. Total reports matches beyond capacity;
   --  no truncated name or incomplete list is represented as complete.
   procedure Find
     (Catalog : Interface_Catalog; Prefix : String; Matches : out Match_List);
end CCL.Catalog.Completion;
