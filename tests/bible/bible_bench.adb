--  Timings of the Bible core, -O3 -gnatp as the app compiles it
--  (tests/bible/run.sh --bench): loading and indexing an image, reading a
--  chapter, parsing references, searching. A quiet host please.
with Ada.Calendar; use Ada.Calendar;
with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with Bible_Canon; use Bible_Canon;
with Bible_Images; use Bible_Images;
with Bible_Parser; use Bible_Parser;
with Bible_References; use Bible_References;
with Bible_Search;
with Bible_Support; use Bible_Support;
with Bible_Text;

procedure Bible_Bench is
   Directory : constant String :=
     (if Ada.Environment_Variables.Exists ("BIBLE_DIR") then Ada.Environment_Variables.Value ("BIBLE_DIR")
      else "build/bible");
   Started : Time;
   function Micros (Since : Time; Times : Positive := 1) return String is
     (Duration'Image ((Clock - Since) * 1_000_000 / Times) & " us");
   T : Translation_Access;
   Data : Image_Access;
   L : Layout;
   O : Open_Status;
   I : Book_Index;
   S : Index_Status;
   R : Result;
   Sink : Natural := 0;
   type Hits_Access is access Bible_Search.Hit_Array;
   Hits : constant Hits_Access := new Bible_Search.Hit_Array (1 .. 1_000);
   type Work_Access is access Bible_Search.Work_Area;
   Work : constant Work_Access := new Bible_Search.Work_Area;
begin
   Data := Load_Bytes (Directory & "/kjv.bible");
   Started := Clock;
   for K in 1 .. 100 loop
      Open (Data.all, L, O);
      Index (Data.all, L, I, S);
   end loop;
   Put_Line ("open + index KJV (" & Data'Length'Image & " bytes):" & Micros (Started, 100));
   T := Load (Directory & "/kjv.bible");

   Started := Clock;
   for K in 1 .. 1_000 loop
      for V in 1 .. Verses_In (T.Index.Table, Psalms, 119) loop
         declare
            Row : constant Image_Size := Verse_Row (T.L, T.Index, (Psalms, 119, V));
            VR : constant Verse_Record := Read_Verse (T.Data.all, T.L, Row);
         begin
            for Seg in VR.First_Segment .. VR.First_Segment + VR.Segments - 1 loop
               declare
                  SR : constant Segment_Record := Read_Segment (T.Data.all, T.L, Seg);
               begin
                  Sink := Sink + Bible_Text.Face_Text (Slice (T.all, SR.Content))'Length;
               end;
            end loop;
         end;
      end loop;
   end loop;
   Put_Line ("read + face-map Psalm 119 (176 verses):" & Micros (Started, 1_000));

   Started := Clock;
   for K in 1 .. 100_000 loop
      Parse ("1 Cor 13:4-7", T.Index.Table, R);
   end loop;
   Put_Line ("parse ""1 Cor 13:4-7"":" & Micros (Started, 100_000));
   Started := Clock;
   for K in 1 .. 100_000 loop
      Parse ("Jhon 3:16", T.Index.Table, R);
   end loop;
   Put_Line ("parse an unknown book:" & Micros (Started, 100_000));

   declare
      procedure Time_Search (Text : String; Prefix : Boolean) is
         Q : Bible_Search.Query;
         N : Bible_Search.Normalize_Status;
         Found, Total : Natural;
         Status : Bible_Search.Search_Status;
         Rounds : constant := 200;
      begin
         Bible_Search.Normalize (Text, Q, N);
         Started := Clock;
         for K in 1 .. Rounds loop
            Bible_Search.Search (T.Data.all, T.L, Q, Prefix, Work.all, Hits.all, Found, Total, Status);
         end loop;
         Put_Line ("search """ & Text & """" & (if Prefix then " (prefix)" else "") & "," & Total'Image &
                   " verses:" & Micros (Started, Rounds));
         Started := Clock;
         for K in 1 .. Found loop
            declare
               Back : Reference;
               Ok : Boolean;
            begin
               Locate (T.L, T.Index, Hits (K), Back, Ok);
               Sink := Sink + Back.Verse;
            end;
         end loop;
         Put_Line ("  locate" & Found'Image & " hits:" & Micros (Started));
      end Time_Search;
   begin
      Time_Search ("faith", False);
      Time_Search ("God so loved", False);
      Time_Search ("lo", True);
      Time_Search ("the", False);
      Time_Search ("a", True);
   end;
   Put_Line ("(sink" & Sink'Image & ")");
end Bible_Bench;
