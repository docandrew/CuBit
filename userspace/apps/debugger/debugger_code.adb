------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Text_Limits;
with Text_Syntax;

package body Debugger_Code is
   use type Text_Syntax.Language;
   use type DM.Text;
   Marks_At : DM.Microseconds := DM.NEVER;

   procedure Set_Now (Now : DM.Microseconds) is
   begin
      Marks_At := Now;
   end Set_Now;

   function Current_Line (S : DM.Snapshot) return Natural is
     (if S.Frame_Total >= S.Frame_Sel and then S.Frame_Total > 0
         and then S.Frames (S.Frame_Sel).File = DM.Image (S.Source_File)
      then S.Frames (S.Frame_Sel).Line else 0);

   function Language_Name (S : DM.Snapshot) return String is
     (case Text_Syntax.Language_Of (DM.Image (S.Source_File)) is
        when Text_Syntax.Ada_Source => "Ada", when Text_Syntax.Rust_Source => "Rust",
        when Text_Syntax.C_Source => "C", when Text_Syntax.CCL_Source => "CCL",
        when Text_Syntax.Markdown => "Markdown", when Text_Syntax.Plain_Text => "text");

   function Mentions (Line, Word : String) return Boolean is
      function Lower (C : Character) return Character is
        (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);
      function Ident (C : Character) return Boolean is (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_');
   begin
      if Word'Length = 0 or else Word'Length > Line'Length then
         return False;
      end if;
      for I in Line'First .. Line'Last - Word'Length + 1 loop
         if (I = Line'First or else not Ident (Line (I - 1)))
           and then (I + Word'Length > Line'Last or else not Ident (Line (I + Word'Length)))
           and then (for all K in 0 .. Word'Length - 1 => Lower (Line (I + K)) = Lower (Word (Word'First + K)))
         then
            return True;
         end if;
      end loop;
      return False;
   end Mentions;

   function Marker (S : DM.Snapshot; N : Positive) return Line_Marker is
   begin
      if N = Current_Line (S) then
         return Current_Line;
      end if;
      for F in 1 .. S.Frame_Total loop
         if F /= S.Frame_Sel and then S.Frames (F).Line = N and then S.Frames (F).File = DM.Image (S.Source_File) then
            return Caller_Line;
         end if;
      end loop;
      return No_Marker;
   end Marker;

   ---------------------------------------------------------------------------
   --  Each line's lexer state at its start, for the file last drawn
   --  (constructs that span lines: Rust block comments and raw strings).
   type State_Array is array (DM.Line_Number) of Text_Syntax.Line_State;
   States : State_Array := [others => Text_Syntax.INITIAL_STATE];
   States_File : DM.Text;
   States_Lines : Natural := 0;
   States_Pool : Natural := 0;

   procedure Prepare (S : DM.Snapshot; Lang : Text_Syntax.Language) is
      Spans : Text_Syntax.Span_Array;
      Count : Text_Syntax.Span_Count;
      Finish : Text_Syntax.Line_State;
   begin
      if DM.Image (States_File) = DM.Image (S.Source_File) and then States_Lines = S.Line_Total
        and then States_Pool = S.Pool_Used
      then
         return;
      end if;
      States (1) := Text_Syntax.INITIAL_STATE;
      for N in 1 .. S.Line_Total loop
         declare
            Raw : constant String := DM.Source_Line (S, N);
            Text : constant String (1 .. Natural'Min (Raw'Length, Text_Limits.SEGMENT_BYTES)) :=
              Raw (Raw'First .. Raw'First + Natural'Min (Raw'Length, Text_Limits.SEGMENT_BYTES) - 1);
         begin
            Text_Syntax.Lex_Line (Lang, Text, States (N), Spans, Count, Finish);
            if N < DM.MAX_SOURCE_LINES then
               States (N + 1) := Finish;
            end if;
         end;
      end loop;
      States_File := S.Source_File;
      States_Lines := S.Line_Total;
      States_Pool := S.Pool_Used;
   end Prepare;

   function Style_Of (Kind : Text_Syntax.Token_Kind) return Token_Style is
     (case Kind is
        when Text_Syntax.Keyword | Text_Syntax.Preprocessor => Keyword,
        when Text_Syntax.Type_Name => Type_Name,
        when Text_Syntax.Number => Number,
        when Text_Syntax.String_Text | Text_Syntax.Character_Text | Text_Syntax.Escape => Literal,
        when Text_Syntax.Comment => Comment,
        when Text_Syntax.Attribute | Text_Syntax.Label => Attribute,
        when Text_Syntax.Operator => Punctuation,
        when others => Plain);

   procedure Colorize
     (S : DM.Snapshot; N : Positive; Text : String;
      Paint : not null access procedure (First, Last : Positive; Style : Token_Style))
   is
      Lang : constant Text_Syntax.Language := Text_Syntax.Language_Of (DM.Image (S.Source_File));
      Length : constant Natural := Natural'Min (Text'Length, Text_Limits.SEGMENT_BYTES);
      Line_Text : constant String (1 .. Length) := Text (Text'First .. Text'First + Length - 1);
      Spans : Text_Syntax.Span_Array;
      Count : Text_Syntax.Span_Count;
      Finish : Text_Syntax.Line_State;
   begin
      if Lang = Text_Syntax.Plain_Text or else N > S.Line_Total then
         return;
      end if;
      Prepare (S, Lang);
      Text_Syntax.Lex_Line (Lang, Line_Text, States (N), Spans, Count, Finish);
      for K in 1 .. Count loop
         if Style_Of (Spans (K).Kind) /= Plain then
            Paint (Text'First + Spans (K).First - 1, Text'First + Spans (K).First + Spans (K).Length - 2,
                   Style_Of (Spans (K).Kind));
         end if;
      end loop;
   end Colorize;

   ---------------------------------------------------------------------------
   --  Inline values on the current line: every top-level local it names.
   function Annotation (S : DM.Snapshot; N : Positive) return String is
      SEPARATOR : constant String := "   ";
      function From (I : Positive) return String is
      begin
         if I > S.Local_Total then
            return "";
         end if;
         declare
            Rest : constant String := From (I + 1);
         begin
            if S.Locals (I).Depth = 0 and then Mentions (Line (S, N), DM.Image (S.Locals (I).Name)) then
               return DM.Image (S.Locals (I).Name) & " = " & DM.Without_Package (S, DM.Image (S.Locals (I).Value))
                 & (if Rest'Length > 0 then SEPARATOR & Rest else "");
            end if;
            return Rest;
         end;
      end From;
   begin
      return (if N = Current_Line (S) then From (1) else "");
   end Annotation;

   function Annotation_Marked (S : DM.Snapshot; N : Positive) return Boolean is
     (N = Current_Line (S) and then
        (for some I in 1 .. S.Local_Total =>
           S.Locals (I).Depth = 0 and then Mentions (Line (S, N), DM.Image (S.Locals (I).Name))
           and then DM.Marked (S.Locals (I).Changed_At, S, Marks_At)));
end Debugger_Code;
