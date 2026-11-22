--  Hosted tests for Text_Fit (the toolkit's fit-with-ellipsis), over the
--  model measure and over the real bundled Sans and Monospace layouts.
--  Every width from 0 to past the whole text: the result fits; the whole
--  text when it fits; otherwise the longest prefix (suffix) followed
--  (preceded) by "..." that fits, found by exhaustive search, i.e. the
--  Boundary_At start guess never settles on a shorter local fit; and ""
--  only when not even "..." fits.
with Ada.Text_IO;
with CuBit.Fonts;
with Model_Width;
with Text_Fit;
with Text_Fit_Model;

procedure Main is
   Failures : Natural := 0;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & What);
      end if;
   end Check;

   generic
      Name : String;
      with function Width (Text : String) return Natural;
      with package Fit is new Text_Fit (Width => Width, others => <>);
   procedure Exhaustive (Text : String);

   procedure Exhaustive (Text : String) is
      Whole : constant Natural := Width (Text);
      Slack : constant := 3;
   begin
      for Available in 0 .. Whole + Slack loop
         declare
            Head : constant String := Fit.Fitted (Text, Available);
            Tail : constant String := Fit.Fitted_Tail (Text, Available);
            Label : constant String := Name & " """ & Text & """ in" & Available'Image;
            Best_Head, Best_Tail : Integer := -1;
         begin
            for Count in 0 .. Text'Length loop
               if Width (Text (Text'First .. Text'First - 1 + Count) & Fit.Ellipsis) <= Available then
                  Best_Head := Count;
               end if;
               if Width (Fit.Ellipsis & Text (Text'Last + 1 - Count .. Text'Last)) <= Available then
                  Best_Tail := Count;
               end if;
            end loop;
            Check (Width (Head) <= Available, Label & ": head fits");
            Check (Width (Tail) <= Available, Label & ": tail fits");
            if Whole <= Available then
               Check (Head = Text and Tail = Text, Label & ": whole");
            elsif Best_Head < 0 then
               Check (Head = "" and Tail = "", Label & ": nothing fits");
            else
               Check (Head = Text (Text'First .. Text'First - 1 + Best_Head) & Fit.Ellipsis,
                      Label & ": longest head, got """ & Head & """");
               Check (Best_Tail >= 0 and then
                        Tail = Fit.Ellipsis & Text (Text'Last + 1 - Best_Tail .. Text'Last),
                      Label & ": longest tail, got """ & Tail & """");
            end if;
         end;
      end loop;
   end Exhaustive;

   function Sans_Width (Text : String) return Natural is (CuBit.Fonts.Width (CuBit.Fonts.Sans, Text));
   function Sans_Hint (Text : String; X : Natural) return Natural is
     (CuBit.Fonts.Boundary_At (CuBit.Fonts.Sans, Text, X));
   function Mono_Width (Text : String) return Natural is (CuBit.Fonts.Width (CuBit.Fonts.Monospace, Text));
   function Mono_Hint (Text : String; X : Natural) return Natural is
     (CuBit.Fonts.Boundary_At (CuBit.Fonts.Monospace, Text, X));
   package Sans_Fit is new Text_Fit (Sans_Width, Sans_Hint);
   package Mono_Fit is new Text_Fit (Mono_Width, Mono_Hint);

   procedure Model is new Exhaustive ("model", Model_Width.Width, Text_Fit_Model);
   procedure Sans is new Exhaustive ("sans", Sans_Width, Sans_Fit);
   procedure Mono is new Exhaustive ("mono", Mono_Width, Mono_Fit);

   --  The labels that were cut mid-word, kerning pairs, a path, an index
   --  that does not start at 1.
   type Sample is access constant String;
   Samples : constant array (1 .. 9) of Sample :=
     [new String'("Documents"), new String'("lost+found"),
      new String'("untitled.ccl  |  F5 interpret  |  Compile for VM"),
      new String'("AVAWAY To Ty Va"), new String'("/Volumes/Data/Projects/cubit/userspace"),
      new String'("x"), new String'(""), new String'("..."), new String'("AVAVAVAVAV")];
   Offset : constant String (100 .. 108) := "Documents";
begin
   for S of Samples loop
      Model (S.all);
      Sans (S.all);
      Mono (S.all);
   end loop;
   Sans (Offset);
   Model (Offset);
   --  The concrete cases from the screenshots.
   Check (Sans_Fit.Fitted ("Documents", Sans_Width ("Docume...")) = "Docume...", "Documents -> Docume...");
   Check (Sans_Fit.Fitted_Tail ("/a/b/folder", Sans_Width (".../folder")) = ".../folder", "path tail");
   Ada.Text_IO.Put_Line ("text-fit:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures > 0 then
      raise Program_Error;
   end if;
end Main;
