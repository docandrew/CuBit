--  Lexicon core timings (hosted; build release for -O3 -gnatp numbers).
with Ada.Calendar; use Ada.Calendar;
with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with Lexicon_Answers; use Lexicon_Answers;
with Lexicon_Images; use Lexicon_Images;
with Lexicon_Support; use Lexicon_Support;
with Lexicon_Wire; use Lexicon_Wire;

procedure Lexicon_Bench is
   Path : constant String :=
     (if Ada.Environment_Variables.Exists ("LEXICON_IMAGE")
      then Ada.Environment_Variables.Value ("LEXICON_IMAGE") else "build/english.lexicon");
   Image : constant Image_Access := Load (Path);
   L : Layout;
   Status : Open_Status;
   Page : Answer_Bytes (0 .. 4095);
   Used : Natural;
   Paragraph : constant String :=
     "The quick brown fox jumps over the lazy dog while the committee recieves its "
     & "correspondance and considers the accomodation of every neccessary amendment to the "
     & "proposal, which was writen hastily and submitted late on Tuesday evening.";

   procedure Measure (What : String; Op : Operation; Text : String; Rounds : Positive) is
      Start : constant Ada.Calendar.Time := Clock;
      Elapsed : Duration;
   begin
      for R in 1 .. Rounds loop
         Answer (Image.all, L, Op, Text, 0, 5, Page, Used);
      end loop;
      Elapsed := Clock - Start;
      Put_Line (What & ":" & Duration'Image (Elapsed * 1_000_000 / Rounds) & " us per call");
   end Measure;
begin
   Open (Image.all, L, Status);
   Measure ("define cataract", Define, "cataract", 20_000);
   Measure ("define break (first page)", Define, "break", 2_000);
   Measure ("thesaurus wharf", Thesaurus, "wharf", 20_000);
   Measure ("suggest recieve", Suggest, "recieve", 200);
   Measure ("check paragraph (" & Natural'Image (Paragraph'Length) & " bytes, 6 errors)", Check, Paragraph, 50);
   Measure ("check clean sentence", Check, "The quick brown fox jumps over the lazy dog.", 20_000);
end Lexicon_Bench;
