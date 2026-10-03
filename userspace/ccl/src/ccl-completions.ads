with CCL.Catalog;
with CCL.Catalog.Completion;

--  What completes the text before the caret: the operations a name could
--  become (host operations of the catalog, then the language's own words),
--  or, inside a call's arguments, the called operation's signature. Shared
--  by every front end (the native console, the Observatory over wire
--  operation 10), so completion is the same everywhere.
package CCL.Completions with SPARK_Mode is
   type Origin is (Host_Operation, Builtin, Form);
   type Candidate is record
      Suggestion : CCL.Catalog.Completion.Suggestion;
      Origin : Completions.Origin := Host_Operation;
   end record;
   Maximum_Candidates : constant := 10;
   subtype Candidate_Count is Natural range 0 .. Maximum_Candidates;
   type Candidate_Array is array (1 .. Maximum_Candidates) of Candidate;
   type Result is record
      Candidates : Candidate_Array;
      Count : Candidate_Count := 0;
      --  More matched than are listed: keep typing to narrow.
      Beyond : Boolean := False;
      --  The name typed so far (candidates complete it).
      Prefix_Length : Natural := 0;
      --  Inside a call's arguments, or on a finished name: what is being
      --  called. Host operations show their catalog signature, the
      --  language's own words their CCL.Hints hint (Describe).
      Signature : CCL.Catalog.Completion.Suggestion;
      Signature_Visible : Boolean := False;
      Signature_Origin : Origin := Host_Operation;
   end record;
   --  Before is the text up to the caret; After_Caret is the character at
   --  the caret (' ' when none): completing inside a word would rewrite
   --  what follows, so it offers nothing.
   procedure Complete
     (Catalog : CCL.Catalog.Interface_Catalog; Before : String; After_Caret : Character;
      Item : out Result);
   --  A host operation's signature as CCL writes a call to it.
   function Signature_Image (S : CCL.Catalog.Completion.Suggestion) return String;
   --  What a completed or called name is: a host signature, or the hint for
   --  a built-in, form or operator.
   function Describe (S : CCL.Catalog.Completion.Suggestion; From : Origin) return String;
end CCL.Completions;
