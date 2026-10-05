with CCL.Evaluation;
with CCL.Diagnostics;
with CCL.Types;

package body CCL.Sessions with SPARK_Mode is
   use type CCL.Language.Interpretation_Status;
   use type CCL.Language.Diagnostic_Code;
   use type CCL.VM.Value_Kind;
   use type CCL.Language.Views.Conversion_Status;

   function Slot (First : History_Index; Offset : History_Count) return History_Index is
     (1 + (First - 1 + Offset) mod Maximum_History);

   procedure Initialize (Item : out Session) is
   begin
      Item := (others => <>);
      CCL.Catalog.Initialize (Item.Catalog);
   end Initialize;

   procedure Initialize
     (Item : out Session; Visible_Interfaces : CCL.Catalog.Interface_Catalog) is
   begin
      Item := (Catalog => Visible_Interfaces, others => <>);
   end Initialize;

   procedure Clear_History (Item : in out Session) is
   begin
      Item.Entries := [others => <>];
      Item.Count := 0;
      Item.Oldest := 1;
   end Clear_History;

   function Length (Item : Session) return History_Count is (Item.Count);

   procedure Complete
     (Item : Session; Prefix : String;
      Matches : out CCL.Catalog.Completion.Match_List) is
   begin
      CCL.Catalog.Completion.Find (Item.Catalog, Prefix, Matches);
   end Complete;

   procedure Complete_At
     (Item : Session; Before : String; After_Caret : Character;
      Result : out CCL.Completions.Result) is
   begin
      CCL.Completions.Complete (Item.Catalog, Before, After_Caret, Result);
   end Complete_At;

   procedure Describe
     (Item : Session; Name : String;
      Operation : out CCL.Catalog.Resolved_Operation; Found : out Boolean) is
   begin
      CCL.Catalog.Resolve (Item.Catalog, Name, Operation, Found);
   end Describe;

   procedure Recall
     (Item : Session; Index : History_Index; Entry_Value : out Submission;
      Found : out Boolean) is
   begin
      Found := Index <= Item.Count;
      Entry_Value := (others => <>);
      if Found then Entry_Value := Item.Entries (Slot (Item.Oldest, Index - 1)); end if;
   end Recall;

   procedure Record_Submission
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Outcome : CCL.Language.Interpretation_Result)
   is
      Value : Submission;
      Target : History_Index;
   begin
      Value.Fuel := Fuel;
      Value.Source_Length := Natural'Min (Source'Length, Value.Source'Length);
      Value.Source_Truncated := Source'Length > Value.Source'Length;
      if Value.Source_Length > 0 then
         Value.Source (1 .. Value.Source_Length) :=
           Source (Source'First .. Source'First + (Value.Source_Length - 1));
      end if;
      Value.Outcome := Outcome;
      Target := Slot (Item.Oldest, Item.Count);
      Item.Entries (Target) := Value;
      if Item.Count = Maximum_History then
         Item.Oldest := Slot (Item.Oldest, 1);
      else
         Item.Count := Item.Count + 1;
      end if;
   end Record_Submission;

   --  The program for one entry. Either dialect may be typed (the REPL has no
   --  mode): input starting with '(' is Lisp; anything else is read as BASIC
   --  and converted to canonical Lisp. If it does not read as BASIC it is
   --  evaluated as typed, so atoms (the same in both) and Lisp diagnostics
   --  still work.
   ---------------------------------------------------------------------------
   --  Session environment: kept definitions and values (see the spec).
   ---------------------------------------------------------------------------

   procedure Reset_Environment (Item : in out Session) is
   begin
      Item.Definitions := [others => ' '];
      Item.Definitions_Length := 0;
      Item.Definition_Count := 0;
      Item.Values := [others => <>];
      Item.Value_Count := 0;
   end Reset_Environment;

   function Kept_Definitions (Item : Session) return Natural is (Item.Definition_Count);

   function Definitions_Source (Item : Session) return String is
     (Item.Definitions (1 .. Item.Definitions_Length));
   function Kept_Values (Item : Session) return Natural is (Item.Value_Count);
   function Holds_Stream (Item : Session; Handle : CCL.Streams.Handle) return Boolean is
     (CCL.Streams."/=" (Handle, CCL.Streams.No_Handle) and then
      (for some V in 1 .. Item.Value_Count => CCL.Streams."=" (Item.Values (V).Stream, Handle)));

   function Is_Blank (C : Character) return Boolean is
     (C in ' ' | ASCII.HT | ASCII.LF | ASCII.CR);
   function Is_Name_Char (C : Character) return Boolean is
     (not Is_Blank (C) and then C not in '(' | ')' | '[' | ']' | '"' | '#');

   --  The next top-level form of Lisp source Text at or after Cursor: a
   --  bracketed form (strings and comments respected), a string, or an atom.
   procedure Next_Form
     (Text : String; Cursor : in out Positive; First, Last : out Natural; Found : out Boolean)
   is
      Depth : Natural := 0;
      In_String : Boolean := False;
   begin
      First := 0; Last := 0; Found := False;
      loop
         while Cursor <= Text'Last and then Is_Blank (Text (Cursor)) loop
            Cursor := Cursor + 1;
         end loop;
         exit when Cursor > Text'Last or else Text (Cursor) /= '#';
         while Cursor <= Text'Last and then Text (Cursor) /= ASCII.LF loop
            Cursor := Cursor + 1;
         end loop;
      end loop;
      if Cursor > Text'Last then return; end if;
      Found := True;
      First := Cursor;
      if Text (Cursor) in '(' | '[' | '"' then
         In_String := Text (Cursor) = '"';
         if not In_String then Depth := 1; end if;
         Cursor := Cursor + 1;
         while Cursor <= Text'Last loop
            if In_String then
               if Text (Cursor) = '\' then
                  Cursor := Cursor + 1;
               elsif Text (Cursor) = '"' then
                  In_String := False;
                  exit when Depth = 0;
               end if;
            elsif Text (Cursor) = '"' then
               In_String := True;
            elsif Text (Cursor) = '#' then
               while Cursor <= Text'Last and then Text (Cursor) /= ASCII.LF loop
                  Cursor := Cursor + 1;
               end loop;
            elsif Text (Cursor) in '(' | '[' then
               Depth := Depth + 1;
            elsif Text (Cursor) in ')' | ']' then
               exit when Depth <= 1;
               Depth := Depth - 1;
            end if;
            exit when Cursor = Text'Last;
            Cursor := Cursor + 1;
         end loop;
         Last := Natural'Min (Cursor, Text'Last);
         Cursor := Last + 1;
      else
         while Cursor <= Text'Last and then Is_Name_Char (Text (Cursor)) loop
            Cursor := Cursor + 1;
         end loop;
         Last := Cursor - 1;
      end if;
   end Next_Form;

   type Form_Kind is (Definition_Form, Value_Form, Body_Form);
   Placeholder_Body : constant String := "0";
   type Form is record
      Kind : Form_Kind := Body_Form;
      First, Last : Natural := 0;
      --  Definitions and values: the name. Values: the expression.
      Name_First, Name_Last : Natural := 0;
      Expression_First, Expression_Last : Natural := 0;
   end record;

   --  (define (f ...) ...) and (type T ...) define; (define x expr) keeps a value.
   function Classify (Text : String; First, Last : Natural) return Form is
      Result : Form := (First => First, Last => Last, others => <>);
      function Starts_With (Prefix : String) return Boolean is
        (Last - First + 1 > Prefix'Length and then
         Text (First .. First + Prefix'Length - 1) = Prefix and then
         Is_Blank (Text (First + Prefix'Length)));
      P : Natural;
   begin
      if Starts_With ("(define") or else Starts_With ("(type") then
         P := First + (if Starts_With ("(define") then 7 else 5);
         while P <= Last and then Is_Blank (Text (P)) loop P := P + 1; end loop;
         if P > Last then return Result; end if;
         if Text (P) = '(' then
            if Starts_With ("(type") then return Result; end if;
            Result.Kind := Definition_Form;
            P := P + 1;
         elsif Starts_With ("(type") then
            Result.Kind := Definition_Form;
         else
            Result.Kind := Value_Form;
         end if;
         Result.Name_First := P;
         while P <= Last and then Is_Name_Char (Text (P)) loop P := P + 1; end loop;
         Result.Name_Last := P - 1;
         if Result.Name_Last < Result.Name_First then
            Result.Kind := Body_Form; return Result;
         end if;
         if Result.Kind = Value_Form then
            Result.Expression_First := P;
            Result.Expression_Last := Last - 1;
            --  A kept value's name is read here, not by the language: it must
            --  be an identifier (a letter, then letters, digits, - _ ?), or
            --  the form is left to the language, which reports it.
            if Text (Last) /= ')' or else Result.Expression_Last < Result.Expression_First or else
              Text (Result.Name_First) not in 'a' .. 'z' | 'A' .. 'Z' or else
              (for some C of Text (Result.Name_First .. Result.Name_Last) =>
                 C not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '?') or else
              Text (Result.Name_First .. Result.Name_Last) in "true" | "false"
            then
               Result.Kind := Body_Form;
            end if;
         end if;
      end if;
      return Result;
   end Classify;

   --  A literal the language reads back as the same value, if there is one.
   procedure Literal_Of
     (Outcome : CCL.Language.Interpretation_Result; Literal : out String;
      Length : out Natural; Kept : out Boolean)
   is
      procedure Add (Text : String) is
      begin
         if Kept and then Text'Length <= Literal'Length - Length then
            Literal (Literal'First + Length .. Literal'First + Length + Text'Length - 1) := Text;
            Length := Length + Text'Length;
         else
            Kept := False;
         end if;
      end Add;
      procedure Add_Quoted (Text : String) is
      begin
         Add ("""");
         for C of Text loop
            case C is
               when '"' => Add ("\""");
               when '\' => Add ("\\");
               when ASCII.LF => Add ("\n");
               when ASCII.CR => Add ("\r");
               when ASCII.HT => Add ("\t");
               when ' ' .. '!' | '#' .. '[' | ']' .. '~' => Add ([1 => C]);
               when others => Kept := False;
            end case;
         end loop;
         Add ("""");
      end Add_Quoted;
      function Trimmed (Image : String) return String is
        (if Image'Length > 0 and then Image (Image'First) = ' '
         then Image (Image'First + 1 .. Image'Last) else Image);
      use type CCL.Language.Static_Type;
      Text_First : Positive := 1;
   begin
      Literal := [others => ' ']; Length := 0; Kept := True;
      if Outcome.Status /= CCL.Language.Succeeded or else not Outcome.Has_Value then
         Kept := False;
      elsif Outcome.Has_List then
         --  A complete list whose literal the reader accepts (at most 16).
         if Outcome.List_Total /= Outcome.List_Length or else Outcome.List_Length > 16 or else
           Outcome.List_Length = 0 or else
           Outcome.List_Element_Type not in CCL.Language.Integer_Type | CCL.Language.Boolean_Type |
             CCL.Language.String_Type
         then
            Kept := False;
         else
            Add ("[");
            for I in 1 .. Outcome.List_Length loop
               if I > 1 then Add (" "); end if;
               if Outcome.List_Element_Type = CCL.Language.String_Type then
                  Add_Quoted (Outcome.List_Text.Data (Text_First .. Outcome.List_Text_Ends (I)));
                  Text_First := Outcome.List_Text_Ends (I) + 1;
               elsif Outcome.List_Element_Type = CCL.Language.Boolean_Type then
                  Add ((if Outcome.List_Values (I).Boolean then "true" else "false"));
               else
                  Add (Trimmed (Interfaces.Integer_64'Image (Outcome.List_Values (I).Integer)));
               end if;
            end loop;
            Add ("]");
         end if;
      elsif Outcome.Has_Stream then
         --  The session's own name for the stream; it grants nothing.
         Add ((if Outcome.Is_Task then "(task " else "(stream ") &
              Outcome.Stream_Element.Data (1 .. Outcome.Stream_Element.Length) &
              CCL.Streams.Handle'Image (Outcome.Stream) & ")");
      elsif Outcome.Has_Literal then
         --  Already the canonical literal the reader accepts.
         Add (Outcome.Literal.Data (1 .. Outcome.Literal.Length));
      elsif Outcome.Has_Text then
         Add_Quoted (Outcome.Result_Text.Data (1 .. Outcome.Result_Text.Length));
      elsif Outcome.Has_Function or else Outcome.Has_Character then
         Kept := False;
      else
         case Result_Type (Outcome) is
            when CCL.Language.Integer_Type =>
               Add (Trimmed (Interfaces.Integer_64'Image (Outcome.Result_Value.Integer)));
            when CCL.Language.Boolean_Type =>
               Add ((if Outcome.Result_Value.Boolean then "true" else "false"));
            when CCL.Types.Declared_Type =>
               --  Enumeration members only (no payload): Type.Member.
               if Outcome.Variant_Payload_Type = CCL.Language.Unit_Type then
                  Add (CCL.Types.Image (Outcome.Variant_Type_Name) & "." &
                       CCL.Types.Image (Outcome.Variant_Member_Name));
               else
                  Kept := False;
               end if;
            when others => Kept := False;
         end case;
      end if;
   end Literal_Of;

   --  What one entry will change, and the program that checks and runs it.
   type Plan is record
      Program : CCL.Language.Views.Text;
      Definitions : String (1 .. Maximum_Definitions_Length) := [others => ' '];
      Definitions_Length : Natural range 0 .. Maximum_Definitions_Length := 0;
      Definition_Count : Natural := 0;
      Binding : Kept_Value;
      Fits : Boolean := True;
      --  The entry itself defines (a define or type form), new or again.
      Defines : Boolean := False;
      --  Where the entry's own text starts in Program: diagnostics are
      --  reported relative to what was typed.
      Entry_Offset : Natural := 0;
   end record;

   procedure Assemble
     (Item : Session; Entry_Text : String; Drop_Body : Boolean; Result : out Plan)
   is
      Maximum_Forms : constant := CCL.Language.MAX_FUNCTIONS + 2;
      type Form_Array is array (1 .. Maximum_Forms) of Form;
      Forms : Form_Array := [others => <>];
      Form_Count : Natural := 0;
      Used : array (1 .. Maximum_Forms) of Boolean := [others => False];
      Body_First : Natural := 0;
      Cursor : Positive := Entry_Text'First;
      First, Last : Natural;
      Found : Boolean;
      Names : String (1 .. 200) := [others => ' '];
      Names_Length : Natural := 0;

      procedure Add_Program (Text : String) is
      begin
         if Result.Fits and then Text'Length <= CCL.Language.MAX_SOURCE_LENGTH - Result.Program.Length then
            Result.Program.Data (Result.Program.Length + 1 .. Result.Program.Length + Text'Length) := Text;
            Result.Program.Length := Result.Program.Length + Text'Length;
         else
            Result.Fits := False;
         end if;
      end Add_Program;
      procedure Keep_Definition (Text : String) is
      begin
         if Result.Fits and then Text'Length + 1 <= Maximum_Definitions_Length - Result.Definitions_Length then
            Result.Definitions (Result.Definitions_Length + 1 .. Result.Definitions_Length + Text'Length) := Text;
            Result.Definitions_Length := Result.Definitions_Length + Text'Length + 1;
            Result.Definition_Count := Result.Definition_Count + 1;
         else
            Result.Fits := False;
         end if;
      end Keep_Definition;
      function Entry_Name (F : Form) return String is (Entry_Text (F.Name_First .. F.Name_Last));
   begin
      Result := (others => <>);
      --  The entry's forms: definitions, then at most one value or a body.
      loop
         exit when Cursor > Entry_Text'Last;
         Next_Form (Entry_Text, Cursor, First, Last, Found);
         exit when not Found;
         if Form_Count = Maximum_Forms then Result.Fits := False; return; end if;
         Form_Count := Form_Count + 1;
         Forms (Form_Count) := Classify (Entry_Text, First, Last);
         if Drop_Body and then Forms (Form_Count).Kind = Body_Form and then
           Entry_Text (First .. Last) = Placeholder_Body
         then
            Form_Count := Form_Count - 1;
            exit;
         elsif Forms (Form_Count).Kind = Body_Form or else
           (Forms (Form_Count).Kind = Value_Form and then Cursor <= Entry_Text'Last)
         then
            --  A body runs to the end of the entry; a value binding must be
            --  last, or it is ordinary (erroneous) source for the language.
            Forms (Form_Count).Kind := Body_Form;
            Body_First := First;
            exit;
         end if;
      end loop;

      --  Kept definitions in order, each replaced by the entry's definition
      --  of the same name; then the entry's new definitions.
      declare
         Kept_Cursor : Positive := 1;
         Kept : Form;
      begin
         while Item.Definitions_Length > 0 and then Kept_Cursor <= Item.Definitions_Length loop
            Next_Form (Item.Definitions (1 .. Item.Definitions_Length), Kept_Cursor, First, Last, Found);
            exit when not Found;
            Kept := Classify (Item.Definitions (1 .. Item.Definitions_Length), First, Last);
            Found := False;
            for F in 1 .. Form_Count loop
               if Forms (F).Kind = Definition_Form and then not Used (F) and then
                 Entry_Name (Forms (F)) = Item.Definitions (Kept.Name_First .. Kept.Name_Last)
               then
                  Keep_Definition (Entry_Text (Forms (F).First .. Forms (F).Last));
                  Used (F) := True;
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               Keep_Definition (Item.Definitions (First .. Last));
            end if;
         end loop;
      end;
      for F in 1 .. Form_Count loop
         if Forms (F).Kind = Definition_Form then
            Result.Defines := True;
            if not Used (F) then
               Keep_Definition (Entry_Text (Forms (F).First .. Forms (F).Last));
            end if;
            if Names_Length + Entry_Name (Forms (F))'Length + 2 <= Names'Length then
               if Names_Length > 0 then
                  Names (Names_Length + 1 .. Names_Length + 2) := ", ";
                  Names_Length := Names_Length + 2;
               end if;
               Names (Names_Length + 1 .. Names_Length + Entry_Name (Forms (F))'Length) := Entry_Name (Forms (F));
               Names_Length := Names_Length + Entry_Name (Forms (F))'Length;
            end if;
         end if;
      end loop;
      Add_Program (Result.Definitions (1 .. Result.Definitions_Length));

      --  Kept values in scope of the body, innermost last.
      for V in 1 .. Item.Value_Count loop
         Add_Program ("(let ((" & Item.Values (V).Name (1 .. Item.Values (V).Name_Length) & " " &
                      Item.Values (V).Literal (1 .. Item.Values (V).Literal_Length) & ")) ");
      end loop;
      Result.Entry_Offset := Result.Program.Length;
      if Form_Count > 0 and then Forms (Form_Count).Kind = Value_Form then
         declare
            F : constant Form := Forms (Form_Count);
         begin
            if F.Name_Last - F.Name_First + 1 <= CCL.Language.MAX_NAME_LENGTH then
               Result.Binding.Name_Length := F.Name_Last - F.Name_First + 1;
               Result.Binding.Name (1 .. Result.Binding.Name_Length) := Entry_Text (F.Name_First .. F.Name_Last);
            end if;
            Add_Program (Entry_Text (F.Expression_First .. F.Expression_Last));
         end;
      elsif Body_First > 0 then
         Add_Program (Entry_Text (Body_First .. Entry_Text'Last));
      else
         --  Definitions only: the result names what was defined.
         Add_Program ("""defined " & Names (1 .. Names_Length) & """");
      end if;
      for V in 1 .. Item.Value_Count loop
         Add_Program (")");
      end loop;
   end Assemble;

   --  Keep what a successful entry defined or bound.
   procedure Commit
     (Item : in out Session; Planned : Plan; Outcome : in out CCL.Language.Interpretation_Result)
   is
      Value : Kept_Value := Planned.Binding;
      Kept : Boolean;
      Slot_Index : Natural := 0;
   begin
      if Outcome.Status /= CCL.Language.Succeeded then return; end if;
      if Value.Name_Length > 0 then
         Literal_Of (Outcome, Value.Literal, Value.Literal_Length, Kept);
         Value.Stream := (if Outcome.Has_Stream then Outcome.Stream else CCL.Streams.No_Handle);
         for V in 1 .. Item.Value_Count loop
            if Item.Values (V).Name (1 .. Item.Values (V).Name_Length) = Value.Name (1 .. Value.Name_Length) then
               Slot_Index := V;
            end if;
         end loop;
         if Slot_Index = 0 and then Item.Value_Count = Maximum_Kept_Values then
            Kept := False;
         end if;
         if not Kept then
            Outcome.Status := CCL.Language.Session_Value_Not_Kept;
            return;
         end if;
         if Slot_Index = 0 then
            Item.Value_Count := Item.Value_Count + 1;
            Slot_Index := Item.Value_Count;
         end if;
         Item.Values (Slot_Index) := Value;
      end if;
      Item.Definitions := Planned.Definitions;
      Item.Definitions_Length := Planned.Definitions_Length;
      Item.Definition_Count := Natural'Min (Planned.Definition_Count, CCL.Language.MAX_FUNCTIONS);
   end Commit;

   --  A text result for session commands.
   procedure Show (Message : String; Outcome : out CCL.Language.Interpretation_Result) is
      Length : constant Natural := Natural'Min (Message'Length, Outcome.Result_Text.Data'Length);
   begin
      Outcome := (others => <>);
      Outcome.Status := CCL.Language.Succeeded;
      Outcome.Has_Value := True;
      Outcome.Has_Text := True;
      Outcome.Result_Text.Data (1 .. Length) := Message (Message'First .. Message'First + Length - 1);
      Outcome.Result_Text.Length := Length;
   end Show;

   procedure Note
     (Item : in out Session; Source, Message : String;
      Outcome : out CCL.Language.Interpretation_Result) is
   begin
      Show (Message, Outcome);
      Record_Submission (Item, Source, 0, Outcome);
   end Note;

   --  :env and :reset. True when Source was one of them.
   procedure Run_Command
     (Item : in out Session; Source : String; Outcome : out CCL.Language.Interpretation_Result;
      Handled : out Boolean)
   is
      First : Natural := Source'First;
      Last : Natural := Source'Last;
      Report : String (1 .. CCL.Language.MAX_TEXT_BYTES) := [others => ' '];
      Report_Length : Natural := 0;
      procedure Add (Text : String) is
      begin
         if Text'Length <= Report'Length - Report_Length then
            Report (Report_Length + 1 .. Report_Length + Text'Length) := Text;
            Report_Length := Report_Length + Text'Length;
         end if;
      end Add;
   begin
      Outcome := (others => <>);
      Handled := False;
      while First <= Last and then Is_Blank (Source (First)) loop First := First + 1; end loop;
      while Last >= First and then Is_Blank (Source (Last)) loop Last := Last - 1; end loop;
      if Source (First .. Last) = ":help" then
         Add ("Lisp or BASIC: (+ 20 22) or 20 + 22. Keep: FUNCTION f(x AS Integer) AS Integer ... END, " &
              "(define (f (x Integer)) Integer ...), LET x = ..., (define x ...). " &
              "Pipelines: xs | where(FUNCTION(n) n > 2) | sum. Commands: :env :reset :help" &
              " (Workbench: :files :save NAME :load NAME). Builtins:");
         for Operation in CCL.Language.Builtin_Operation range
           CCL.Language.Each_Builtin .. CCL.Language.Builtin_Operation'Last
         loop
            Add (" " & CCL.Language.Builtin_Name (Operation));
         end loop;
         Add (" length at concat to-string");
         Show (Report (1 .. Report_Length), Outcome);
         Handled := True;
      elsif Source (First .. Last) = ":reset" then
         Reset_Environment (Item);
         Show ("session environment cleared", Outcome);
         Handled := True;
      elsif Source (First .. Last) = ":env" then
         declare
            Cursor : Positive := 1;
            F_First, F_Last : Natural;
            Found : Boolean;
            F : Form;
            Count : Natural := 0;
         begin
            Add ("definitions:");
            while Item.Definitions_Length > 0 and then Cursor <= Item.Definitions_Length loop
               Next_Form (Item.Definitions (1 .. Item.Definitions_Length), Cursor, F_First, F_Last, Found);
               exit when not Found;
               F := Classify (Item.Definitions (1 .. Item.Definitions_Length), F_First, F_Last);
               Add ((if Count = 0 then " " else ", ") & Item.Definitions (F.Name_First .. F.Name_Last));
               Count := Count + 1;
            end loop;
            if Count = 0 then Add (" none"); end if;
            Add ("; values:");
            for V in 1 .. Item.Value_Count loop
               Add ((if V = 1 then " " else ", ") & Item.Values (V).Name (1 .. Item.Values (V).Name_Length) &
                    " = " & Item.Values (V).Literal (1 .. Item.Values (V).Literal_Length));
            end loop;
            if Item.Value_Count = 0 then Add (" none"); end if;
         end;
         Show (Report (1 .. Report_Length), Outcome);
         Handled := True;
      end if;
   end Run_Command;

   type Reading is record
      Basic_Tried : Boolean := False;
      --  BASIC definitions typed without a trailing expression: a placeholder
      --  body was added to lower them and must be dropped.
      Drop_Body : Boolean := False;
      Diagnostic : CCL.Language.Diagnostic_Code := CCL.Language.No_Diagnostic;
      Position : Natural := 0;
   end record;

   procedure Prepare
     (Item : Session; Source : String; Program : out CCL.Language.Views.Text;
      Read : out Reading)
   is
      Converted : CCL.Language.Views.Conversion;
      First : Natural := Source'First;
   begin
      Program := (others => <>);
      Read := (others => <>);
      while First <= Source'Last and then Source (First) in ' ' | ASCII.HT | ASCII.LF | ASCII.CR loop
         First := First + 1;
      end loop;
      if First <= Source'Last and then Source (First) /= '(' and then
        Source'Length <= CCL.Language.Views.Maximum_View_Length
      then
         CCL.Language.Views.Convert
           (Source, CCL.Language.Views.Basic, CCL.Language.Views.Lisp, Item.Catalog, Converted,
            Check => False);
         if Converted.Status = CCL.Language.Views.Converted then
            Program := Converted.Canonical;
            return;
         end if;
         Read := (Basic_Tried => True, Diagnostic => Converted.Diagnostic,
                  Position => Converted.Position, Drop_Body => False);
         --  FUNCTION/TYPE definitions on their own: lower with a placeholder
         --  body, which Assemble drops.
         if (Source'Last - First >= 8 and then Source (First .. First + 8) = "FUNCTION ") or else
           (Source'Last - First >= 4 and then Source (First .. First + 4) = "TYPE ")
         then
            declare
               Value : CCL.Language.Views.Conversion;
            begin
               if Source'Length + 2 <= CCL.Language.Views.Maximum_View_Length then
                  CCL.Language.Views.Convert
                    (Source & ASCII.LF & Placeholder_Body, CCL.Language.Views.Basic,
                     CCL.Language.Views.Lisp, Item.Catalog, Value, Check => False);
                  if Value.Status = CCL.Language.Views.Converted then
                     Program := Value.Canonical;
                     Read := (Drop_Body => True, others => <>);
                     return;
                  end if;
               end if;
            end;
         end if;
         --  LET x = expr on its own (no IN) keeps a value: (define x expr).
         declare
            P : Natural := First + 4;
            Name_First, Name_Last : Natural;
            Value : CCL.Language.Views.Conversion;
         begin
            if Source'Last - First >= 4 and then Source (First .. First + 3) = "LET " then
               while P <= Source'Last and then Is_Blank (Source (P)) loop P := P + 1; end loop;
               Name_First := P;
               while P <= Source'Last and then Is_Name_Char (Source (P)) and then Source (P) /= '=' loop
                  P := P + 1;
               end loop;
               Name_Last := P - 1;
               while P <= Source'Last and then Is_Blank (Source (P)) loop P := P + 1; end loop;
               if Name_Last >= Name_First and then P < Source'Last and then Source (P) = '=' then
                  CCL.Language.Views.Convert
                    (Source (P + 1 .. Source'Last), CCL.Language.Views.Basic, CCL.Language.Views.Lisp,
                     Item.Catalog, Value, Check => False);
                  if Value.Status = CCL.Language.Views.Converted and then
                    Value.Canonical.Length + (Name_Last - Name_First + 1) + 10 <= Program.Data'Length
                  then
                     declare
                        Text : constant String := "(define " & Source (Name_First .. Name_Last) & " " &
                          Value.Canonical.Data (1 .. Value.Canonical.Length) & ")";
                     begin
                        Program.Data (1 .. Text'Length) := Text;
                        Program.Length := Text'Length;
                        Read := (others => <>);
                        return;
                     end;
                  end if;
               end if;
            end if;
         end;
      end if;
      if Source'Length <= CCL.Language.Views.Maximum_View_Length then
         Program.Length := Source'Length;
         Program.Data (1 .. Source'Length) := Source;
      end if;
   end Prepare;

   --  Input that read neither as BASIC nor as Lisp was most likely meant as
   --  BASIC (it did not start with '('): report where BASIC went wrong.
   procedure Explain (Read : Reading; Outcome : in out CCL.Language.Interpretation_Result) is
   begin
      if Read.Basic_Tried and then Read.Diagnostic /= CCL.Language.No_Diagnostic and then
        Outcome.Status in CCL.Language.Parse_Failed | CCL.Language.Type_Check_Failed
      then
         Outcome.Diagnostic := Read.Diagnostic;
         Outcome.Diagnostic_Position := Read.Position;
      end if;
   end Explain;

   --  After evaluation: diagnostics relative to the typed entry, then keep
   --  what a successful entry defined or bound.
   --  Diagnostics relative to what was typed, and explained.
   procedure Report
     (Planned : Plan; Read : Reading; Outcome : in out CCL.Language.Interpretation_Result) is
   begin
      if Outcome.Diagnostic_Position > Planned.Entry_Offset then
         Outcome.Diagnostic_Position := Outcome.Diagnostic_Position - Planned.Entry_Offset;
      elsif Outcome.Diagnostic_Position > 0 and then Planned.Entry_Offset > 0 then
         --  Inside a kept definition (a redefinition broke a dependent).
         Outcome.Diagnostic_Position := 0;
      end if;
      Explain (Read, Outcome);
   end Report;

   procedure Finish
     (Item : in out Session; Planned : Plan; Read : Reading;
      Outcome : in out CCL.Language.Interpretation_Result) is
   begin
      Report (Planned, Read, Outcome);
      Commit (Item, Planned, Outcome);
   end Finish;

   procedure Too_Large (Outcome : out CCL.Language.Interpretation_Result) is
   begin
      Outcome := (others => <>);
      Outcome.Status := CCL.Language.Parse_Failed;
      Outcome.Diagnostic := CCL.Language.Source_Too_Long;
   end Too_Large;

   procedure Submit
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Outcome : out CCL.Language.Interpretation_Result)
   is
      Program : CCL.Language.Views.Text;
      Read : Reading;
      Planned : Plan;
      Handled : Boolean;
   begin
      Run_Command (Item, Source, Outcome, Handled);
      if not Handled then
         Prepare (Item, Source, Program, Read);
         Assemble (Item, Program.Data (1 .. Program.Length), Read.Drop_Body, Planned);
         if Planned.Fits then
            CCL.Evaluation.Evaluate
              (Planned.Program.Data (1 .. Planned.Program.Length), Fuel, Item.Catalog, Outcome);
            Finish (Item, Planned, Read, Outcome);
         else
            Too_Large (Outcome);
         end if;
      end if;
      Record_Submission (Item, Source, Fuel, Outcome);
   end Submit;

   procedure Submit_With_Host
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result)
   is
      procedure Evaluate is new CCL.Evaluation.Evaluate_With_Host (Host_Context, Invoke);
      Program : CCL.Language.Views.Text;
      Read : Reading;
      Planned : Plan;
      Handled : Boolean;
   begin
      --  Evaluate the original source, never a truncated history copy.
      Run_Command (Item, Source, Outcome, Handled);
      if not Handled then
         Prepare (Item, Source, Program, Read);
         Assemble (Item, Program.Data (1 .. Program.Length), Read.Drop_Body, Planned);
         if Planned.Fits then
            Evaluate (Planned.Program.Data (1 .. Planned.Program.Length), Fuel, Item.Catalog,
                      Grants, Context, Outcome);
            Finish (Item, Planned, Read, Outcome);
         else
            Too_Large (Outcome);
         end if;
      end if;
      Record_Submission (Item, Source, Fuel, Outcome);
   end Submit_With_Host;

   procedure Submit_With_Values
     (Item : in out Session; Source : String; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result;
      Shown : String := "")
   is
      procedure Evaluate is new CCL.Evaluation.Evaluate_With_Values
        (Host_Context, Invoke, Read_Stream => Read_Stream);
      Program : CCL.Language.Views.Text;
      Read : Reading;
      Planned : Plan;
      Handled : Boolean;
   begin
      Run_Command (Item, Source, Outcome, Handled);
      if not Handled then
         Prepare (Item, Source, Program, Read);
         Assemble (Item, Program.Data (1 .. Program.Length), Read.Drop_Body, Planned);
         if Planned.Fits then
            Evaluate (Planned.Program.Data (1 .. Planned.Program.Length), Fuel, Item.Catalog,
                      Grants, Context, Outcome);
            Finish (Item, Planned, Read, Outcome);
         else
            Too_Large (Outcome);
         end if;
      end if;
      Record_Submission (Item, (if Shown'Length > 0 then Shown else Source), Fuel, Outcome);
   end Submit_With_Values;

   procedure Reevaluate_With_Values
     (Item : in out Session; Index : History_Index; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result; Reevaluated : out Boolean)
   is
      procedure Evaluate is new CCL.Evaluation.Evaluate_With_Values
        (Host_Context, Invoke, Read_Stream => Read_Stream);
      Entry_Value : Submission;
      Found : Boolean;
      Program : CCL.Language.Views.Text;
      Read : Reading;
      Planned : Plan;
   begin
      Outcome := (others => <>);
      Reevaluated := False;
      Recall (Item, Index, Entry_Value, Found);
      if not Found or else Entry_Value.Source_Truncated or else Entry_Value.Source_Length = 0 or else
        Entry_Value.Source (1) = ':'
      then
         return;
      end if;
      Prepare (Item, Entry_Value.Source (1 .. Entry_Value.Source_Length), Program, Read);
      Assemble (Item, Program.Data (1 .. Program.Length), Read.Drop_Body, Planned);
      --  Only a plain expression: it names no value and defines nothing.
      if not Planned.Fits or else Planned.Binding.Name_Length > 0 or else Planned.Defines then
         return;
      end if;
      Evaluate (Planned.Program.Data (1 .. Planned.Program.Length), Fuel, Item.Catalog,
                Grants, Context, Outcome);
      Report (Planned, Read, Outcome);
      Item.Entries (Slot (Item.Oldest, Index - 1)).Outcome := Outcome;
      Reevaluated := True;
   end Reevaluate_With_Values;

   procedure Resume_With_Values
     (Item : in out Session; Index : History_Index; Fuel : Fuel_Budget;
      Grants : CCL.Catalog.Granted_Bindings; Context : in out Host_Context;
      Outcome : out CCL.Language.Interpretation_Result; Resumed : out Boolean)
   is
      procedure Evaluate is new CCL.Evaluation.Evaluate_With_Values
        (Host_Context, Invoke, Read_Stream => Read_Stream);
      Entry_Value : Submission;
      Found : Boolean;
      Program : CCL.Language.Views.Text;
      Read : Reading;
      Planned : Plan;
   begin
      Outcome := (others => <>);
      Resumed := False;
      Recall (Item, Index, Entry_Value, Found);
      if not Found or else Entry_Value.Outcome.Status /= CCL.Language.Waiting_On_Task or else
        Entry_Value.Source_Truncated or else Entry_Value.Source_Length = 0
      then
         return;
      end if;
      Prepare (Item, Entry_Value.Source (1 .. Entry_Value.Source_Length), Program, Read);
      Assemble (Item, Program.Data (1 .. Program.Length), Read.Drop_Body, Planned);
      if not Planned.Fits then
         return;
      end if;
      Evaluate (Planned.Program.Data (1 .. Planned.Program.Length), Fuel, Item.Catalog,
                Grants, Context, Outcome);
      --  Still waiting (on a later task of the same entry): nothing to bind yet.
      if Outcome.Status = CCL.Language.Waiting_On_Task then
         Report (Planned, Read, Outcome);
      else
         Finish (Item, Planned, Read, Outcome);
      end if;
      Item.Entries (Slot (Item.Oldest, Index - 1)).Outcome := Outcome;
      Resumed := True;
   end Resume_With_Values;

   function Waiting_Entry (Item : Session; Waited_On : CCL.Streams.Handle) return History_Count is
      use type CCL.Streams.Handle;
      Entry_Value : Submission;
      Found : Boolean;
   begin
      for Index in 1 .. Length (Item) loop
         Recall (Item, Index, Entry_Value, Found);
         if Found and then Entry_Value.Outcome.Status = CCL.Language.Waiting_On_Task and then
           Entry_Value.Outcome.Waited_On = Waited_On
         then
            return Index;
         end if;
      end loop;
      return 0;
   end Waiting_Entry;

   procedure Abandon_Wait
     (Item : in out Session; Index : History_Index; Why : CuBit.Failures.Failure) is
   begin
      if Index <= Length (Item) then
         declare
            Outcome : CCL.Language.Interpretation_Result renames
              Item.Entries (Slot (Item.Oldest, Index - 1)).Outcome;
         begin
            if Outcome.Status = CCL.Language.Waiting_On_Task then
               Outcome.Status := CCL.Language.Host_Call_Failed;
               Outcome.Failure := Why;
            end if;
         end;
      end if;
   end Abandon_Wait;

   procedure View_Source
     (Item : Session; Source : String; Into : CCL.Language.Views.Surface;
      Shown : out CCL.Language.Views.Text; Converted : out Boolean)
   is
      package V renames CCL.Language.Views;
      use type V.Surface;
      From : V.Surface;
      Alone : V.Conversion;
      procedure Keep (Text : String) is
      begin
         Shown.Length := Text'Length;
         Shown.Data (1 .. Text'Length) := Text;
         Converted := True;
      end Keep;
   begin
      Shown := (others => <>);
      Converted := False;
      if Source'Length = 0 or else Source'Length > CCL.Language.MAX_SOURCE_LENGTH then return; end if;
      --  Session commands read the same in both notations.
      if Source (Source'First) = ':' then Keep (Source); return; end if;
      From := V.Detect (Source);
      if From = Into then Keep (Source); return; end if;
      --  BASIC lowers to Lisp without its context; Lisp is checked first.
      V.Convert (Source, From, Into, Item.Catalog, Alone, Check => From = V.Lisp);
      if Alone.Status = V.Converted then
         --  Unchecked BASIC lowering fills only Canonical, the Lisp itself.
         Shown := (if From = V.Lisp then Alone.Rendered else Alone.Canonical);
         Converted := True;
         return;
      end if;
      if From /= V.Lisp then return; end if;
      --  In the session's context: the program it would run, converted
      --  whole, then the entry's own node cut out of the rendering.
      declare
         Prepared : V.Text;
         Read : Reading;
         Planned : Plan;
         Whole : V.Conversion;
         Best : CCL.Language.Node_Reference := CCL.Language.NO_NODE;
         Best_End : Natural := 0;
      begin
         Prepare (Item, Source, Prepared, Read);
         Assemble (Item, Prepared.Data (1 .. Prepared.Length), Read.Drop_Body, Planned);
         if not Planned.Fits or else Planned.Binding.Name_Length > 0 or else Planned.Defines then
            return;
         end if;
         V.Convert (Planned.Program.Data (1 .. Planned.Program.Length), V.Lisp, Into,
                    Item.Catalog, Whole);
         if Whole.Status /= V.Converted then return; end if;
         --  The outermost node that starts where the entry does.
         for N in Whole.Input_Nodes'Range loop
            if Whole.Input_Nodes (N).First = Planned.Entry_Offset + 1 and then
              Whole.Input_Nodes (N).After_Last > Best_End
            then
               Best := N; Best_End := Whole.Input_Nodes (N).After_Last;
            end if;
         end loop;
         if Best = CCL.Language.NO_NODE then return; end if;
         declare
            S : constant V.Span := Whole.Output_Nodes (Best);
         begin
            if S.First in 1 .. Whole.Rendered.Length and then S.After_Last in S.First + 1 .. Whole.Rendered.Length + 1 then
               Keep (Whole.Rendered.Data (S.First .. S.After_Last - 1));
            end if;
         end;
      end;
   end View_Source;

   procedure Expression_Program
     (Item : Session; Source : String; Program : out String; Length : out Natural;
      Plain : out Boolean)
   is
      Prepared : CCL.Language.Views.Text;
      Read : Reading;
      Planned : Plan;
   begin
      Program := [others => ' '];
      Length := 0;
      Plain := False;
      if Source'Length = 0 or else Source (Source'First) = ':' then return; end if;
      Prepare (Item, Source, Prepared, Read);
      Assemble (Item, Prepared.Data (1 .. Prepared.Length), Read.Drop_Body, Planned);
      if Planned.Fits and then Planned.Binding.Name_Length = 0 and then not Planned.Defines and then
        Planned.Program.Length <= Program'Length
      then
         Length := Planned.Program.Length;
         Program (1 .. Length) := Planned.Program.Data (1 .. Length);
         Plain := True;
      end if;
   end Expression_Program;

   function Result_Type (Outcome : CCL.Language.Interpretation_Result)
     return CCL.Language.Static_Type is
   begin
      if Outcome.Status /= CCL.Language.Succeeded or else not Outcome.Has_Value then
         return CCL.Language.Invalid_Type;
      elsif Outcome.Has_List then return Outcome.List_Type;
      elsif Outcome.Has_Function then return CCL.Language.Invalid_Type;
      elsif Outcome.Has_Literal then return Outcome.Literal_Type;
      elsif Outcome.Has_Text then return CCL.Language.String_Type;
      elsif Outcome.Has_Character then return CCL.Language.Character_Type;
      elsif Outcome.Variant_Type in CCL.Types.Declared_Type then return Outcome.Variant_Type;
      elsif Outcome.Result_Value.Kind = CCL.VM.Integer_Value then return CCL.Language.Integer_Type;
      elsif Outcome.Result_Value.Kind = CCL.VM.Boolean_Value then return CCL.Language.Boolean_Type;
      else return CCL.Language.Invalid_Type;
      end if;
   end Result_Type;

   --  One line for a list result: List<Integer>: [10, 20, 30]. Strings are
   --  quoted; enumeration members show their position until results carry
   --  member names.
   function List_Image (Outcome : CCL.Language.Interpretation_Result) return String is
      use type CCL.Language.Static_Type;
      use type Interfaces.Integer_64;
      Maximum : constant := 2 * CCL.Language.MAX_TEXT_BYTES;
      Buffer : String (1 .. Maximum) := [others => ' '];
      Last : Natural range 0 .. Maximum := 0;
      Text_First : Positive := 1;
      Element_Type : constant CCL.Language.Static_Type := Outcome.List_Element_Type;
      procedure Add (Item : String) is
      begin
         if Item'Length <= Maximum - Last then
            Buffer (Last + 1 .. Last + Item'Length) := Item;
            Last := Last + Item'Length;
         end if;
      end Add;
      function Trimmed (Item : String) return String is
        (if Item'Length > 0 and then Item (Item'First) = ' '
         then Item (Item'First + 1 .. Item'Last) else Item);
   begin
      for I in 1 .. Outcome.List_Length loop
         if I > 1 then Add (", "); end if;
         if Element_Type = CCL.Language.String_Type then
            Add ('"' & Outcome.List_Text.Data
                   (Text_First .. Outcome.List_Text_Ends (I)) & '"');
            Text_First := Outcome.List_Text_Ends (I) + 1;
         elsif Element_Type = CCL.Language.Boolean_Type then
            Add ((if Outcome.List_Values (I).Boolean then "true" else "false"));
         elsif Element_Type = CCL.Language.Character_Type then
            Add ("'" & Character'Val (Natural (Outcome.List_Values (I).Integer mod 256)) & "'");
         elsif Element_Type = CCL.Language.Integer_Type then
            Add (Trimmed (Interfaces.Integer_64'Image (Outcome.List_Values (I).Integer)));
         else
            Add ("#" & Trimmed (Interfaces.Integer_64'Image (Outcome.List_Values (I).Integer)));
         end if;
      end loop;
      if Outcome.List_Total > Outcome.List_Length then
         Add ((if Outcome.List_Length > 0 then ", " else "") & "... " &
              Trimmed (Natural'Image (Outcome.List_Total - Outcome.List_Length)) & " more");
      end if;
      return "List<" &
        (if Element_Type = CCL.Language.String_Type then "String"
         elsif Element_Type = CCL.Language.Boolean_Type then "Boolean"
         elsif Element_Type = CCL.Language.Character_Type then "Character"
         elsif Element_Type = CCL.Language.Integer_Type then "Integer"
         else "enumeration") & ">: [" & Buffer (1 .. Last) & "]";
   end List_Image;

   function Trimmed_Image (Value : Interfaces.Integer_64) return String is
     (if Interfaces."<" (Value, 0) then Interfaces.Integer_64'Image (Value)
      else Interfaces.Integer_64'Image (Value)
             (2 .. Interfaces.Integer_64'Image (Value)'Last));

   function Result_Type_Image (Outcome : CCL.Language.Interpretation_Result) return String is
   begin
      if Outcome.Status /= CCL.Language.Succeeded then
         return "";
      elsif Outcome.Has_Stream then
         return (if Outcome.Is_Task then "Task<" else "Stream<") &
           Outcome.Stream_Element.Data (1 .. Outcome.Stream_Element.Length) & ">";
      elsif Outcome.Has_List then
         declare
            Image : constant String := List_Image (Outcome);
            Separator : Natural := Image'First;
         begin
            while Separator < Image'Last and then Image (Separator) /= ':' loop
               pragma Loop_Invariant (Separator in Image'Range);
               pragma Loop_Variant (Increases => Separator);
               Separator := Separator + 1;
            end loop;
            return Image (Image'First .. Separator - 1);
         end;
      elsif Outcome.Has_Function then
         return "Function";
      elsif Outcome.Has_Literal then
         declare
            Name : constant String := CCL.Types.Image (Outcome.Literal_Type_Name);
            List_Prefix : constant String := "List-";
         begin
            --  The registry names List<T> "List-T"; show it as written.
            return (if Name'Length > List_Prefix'Length and then
                      Name (Name'First .. Name'First + List_Prefix'Length - 1) = List_Prefix
                    then "List<" & Name (Name'First + List_Prefix'Length .. Name'Last) & ">"
                    else Name);
         end;
      end if;
      case Result_Type (Outcome) is
         when CCL.Language.Integer_Type => return "Integer";
         when CCL.Language.Boolean_Type => return "Boolean";
         when CCL.Language.String_Type => return "String";
         when CCL.Language.Character_Type => return "Character";
         when CCL.Language.Invalid_Type => return "";
         when CCL.Language.Handler_Type => return "Handler";
         when CCL.Language.Unit_Type => return "Unit";
         when CCL.Types.Declared_Type => return CCL.Types.Image (Outcome.Variant_Type_Name);
      end case;
   end Result_Type_Image;

   function Result_Value_Image (Outcome : CCL.Language.Interpretation_Result) return String is
   begin
      if Outcome.Status = CCL.Language.Host_Import_Required then
         return CCL.Diagnostics.Message (Outcome.Status);
      elsif Outcome.Status in CCL.Language.Host_Call_Failed | CCL.Language.Host_Authority_Denied |
                              CCL.Language.Host_Contract_Unsupported and then
        Outcome.Failed_Operation.Length > 0
      then
         --  Which operation, why, and how to allow it (docs/ccl-errors.md).
         return CuBit.Failures.Explain (CCL.Types.Image (Outcome.Failed_Operation), Outcome.Failure);
      elsif Outcome.Status /= CCL.Language.Succeeded then
         return CCL.Diagnostics.Message (Outcome.Status) &
           (if Outcome.Diagnostic = CCL.Language.No_Diagnostic then ""
            --  A type mismatch names what wanted which type, and what it got.
            elsif Outcome.Diagnostic = CCL.Language.Field_Type_Mismatch
              and then Outcome.Diagnostic_Expected.Length > 0 and then Outcome.Diagnostic_Found.Length > 0
            then ": field " & CCL.Types.Image (Outcome.Diagnostic_Subject) & " takes " &
                 CCL.Types.Image (Outcome.Diagnostic_Expected) & ", not " &
                 CCL.Types.Image (Outcome.Diagnostic_Found)
            elsif Outcome.Diagnostic = CCL.Language.Argument_Type_Mismatch
              and then Outcome.Diagnostic_Expected.Length > 0 and then Outcome.Diagnostic_Found.Length > 0
            then ": " & CCL.Types.Image (Outcome.Diagnostic_Subject) & " takes " &
                 CCL.Types.Image (Outcome.Diagnostic_Expected) & ", not " &
                 CCL.Types.Image (Outcome.Diagnostic_Found)
            elsif Outcome.Diagnostic_Subject.Length > 0
            then ": field " & CCL.Types.Image (Outcome.Diagnostic_Subject) & ": " &
                 CCL.Diagnostics.Message (Outcome.Diagnostic)
            else ": " & CCL.Diagnostics.Message (Outcome.Diagnostic)) &
           (if Outcome.Diagnostic_Position = 0 then ""
            else " at character" & Natural'Image (Outcome.Diagnostic_Position));
      elsif Outcome.Has_Stream and then Outcome.Is_Task then
         --  Its state when the entry ran (or last re-ran, as a live card).
         return (if Outcome.Task_Done
                 then "Done " & Outcome.Task_Value.Data (1 .. Outcome.Task_Value.Length)
                 else "Running");
      elsif Outcome.Has_Stream then
         --  A description, never a literal: # starts a comment, so it
         --  cannot be typed back in.
         return "#<stream" & CCL.Streams.Handle'Image (Outcome.Stream) & ">";
      elsif Outcome.Has_List then
         declare
            Image : constant String := List_Image (Outcome);
            Separator : Natural := Image'First;
         begin
            while Separator < Image'Last and then Image (Separator) /= ':' loop
               pragma Loop_Invariant (Separator in Image'Range);
               pragma Loop_Variant (Increases => Separator);
               Separator := Separator + 1;
            end loop;
            return Image (Natural'Min (Separator + 2, Image'Last + 1) .. Image'Last);
         end;
      elsif Outcome.Has_Function then
         return CCL.Types.Image (Outcome.Function_Name);
      elsif Outcome.Has_Literal then
         return Outcome.Literal.Data (1 .. Outcome.Literal.Length);
      end if;
      case Result_Type (Outcome) is
         when CCL.Language.Integer_Type => return Trimmed_Image (Outcome.Result_Value.Integer);
         when CCL.Language.Boolean_Type =>
            return (if Outcome.Result_Value.Boolean then "true" else "false");
         when CCL.Language.String_Type =>
            return Outcome.Result_Text.Data (1 .. Outcome.Result_Text.Length);
         when CCL.Language.Character_Type => return [1 => Outcome.Result_Character];
         when CCL.Language.Invalid_Type => return "ok";
         when CCL.Language.Handler_Type | CCL.Language.Unit_Type => return "";
         when CCL.Types.Declared_Type =>
            return CCL.Types.Image (Outcome.Variant_Member_Name) &
              (case Outcome.Variant_Payload_Type is
                when CCL.Language.Integer_Type => "(" & Interfaces.Integer_64'Image (Outcome.Result_Value.Integer) & ")",
                when CCL.Language.Boolean_Type => (if Outcome.Result_Value.Boolean then "(true)" else "(false)"),
                when others => "");
      end case;
   end Result_Value_Image;

   function Result_Image (Outcome : CCL.Language.Interpretation_Result) return String is
      Type_Image : constant String := Result_Type_Image (Outcome);
      Value_Image : constant String := Result_Value_Image (Outcome);
   begin
      if Type_Image'Length = 0 then
         return Value_Image;
      elsif Value_Image'Length = 0 then
         return Type_Image;
      elsif Outcome.Status = CCL.Language.Succeeded and then not Outcome.Has_List and then
        not Outcome.Has_Function and then not Outcome.Has_Literal and then
        Result_Type (Outcome) in CCL.Types.Declared_Type
      then
         --  A variant reads as it is written: Type.Member(payload).
         return Type_Image & "." & Value_Image;
      else
         return Type_Image & ": " & Value_Image;
      end if;
   end Result_Image;
end CCL.Sessions;
