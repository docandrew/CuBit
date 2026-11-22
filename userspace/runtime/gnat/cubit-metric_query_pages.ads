------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The page a metrics reader lends metrics.svc for List_Series, Read_Range
--  and Read_Window (MET-002, CuBit.Metric_Protocol): its layout, in words,
--  shared by the service and every reader (CuBit.Metric_Queries).
--
--  @description
--  The reader writes its request's selector or cursor at the start of the
--  page, lends it writable, and metrics.svc overwrites it with the answer.
--  Texts are packed into words, low byte first, after a length word.
--
--  List_Series: request [page, skip, 0, 0]; page in: the selector
--    (Match, the name, the source); answer [rows, selected in all, 0, 0];
--    page out: up to List_Rows rows of List_Row_Words.
--  Read_Range: request [page, series ID, from us, to us]; page in: [skip,
--    step us] (step 0: raw samples; else the coarsest tier no wider);
--    answer [rows, in the range in all, gaps, resolution us]; page out:
--    the oldest raw time held, the gaps overlapping the range (at most
--    Range_Gap_Rows), then the rows (raw: time, value; windows: Window_Row_Words).
--  Read_Window: request [page, series ID or 0, from us, to us]; with 0 the
--    page holds a selector and every selected series is merged (their
--    sketches add); answer [series merged, 0, 0, 0]; page out: Stats words.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Metric_Query_Pages with Pure, SPARK_Mode is
   Page_Bytes : constant := 4_096;
   Page_Words : constant := Page_Bytes / 8;
   subtype Word_Index is Natural range 0 .. Page_Words - 1;
   type Page is array (Word_Index) of Unsigned_64;

   Text_Bytes : constant := 64;
   Text_Words : constant := Text_Bytes / 8;
   Short_Text_Bytes : constant := 32;
   Short_Text_Words : constant := Short_Text_Bytes / 8;

   --  Selector (page in): how the name matches (0 any, 1 exact, 2 prefix),
   --  then the name and the source (empty: any source), each a length word
   --  and Text_Words of text.
   Select_Match : constant Word_Index := 0;
   Select_Name : constant Word_Index := 1;
   Select_Source : constant Word_Index := Select_Name + 1 + Text_Words;

   --  A List_Series row.
   Row_Id : constant := 0;
   Row_Kind : constant := 1;
   Row_Unit : constant := 2;
   Row_Published : constant := 3;
   Row_Retained : constant := 4;
   Row_Evicted : constant := 5;
   Row_Shed : constant := 6;
   Row_Refused : constant := 7;
   Row_First : constant := 8;
   Row_Last : constant := 9;
   Row_Last_Value : constant := 10;
   Row_Raw_From : constant := 11;
   Row_Gaps : constant := 12;
   Row_Raw_Bytes : constant := 13;
   Row_Name : constant := 14;
   Row_Source : constant := Row_Name + 1 + Text_Words;
   Row_Program : constant := Row_Source + 1 + Text_Words;
   Row_Instance : constant := Row_Program + 1 + Short_Text_Words;
   List_Row_Words : constant := Row_Instance + 1 + Short_Text_Words;
   List_Rows : constant := Page_Words / List_Row_Words;

   --  Read_Range.
   Range_Skip : constant Word_Index := 0;
   Range_Step : constant Word_Index := 1;
   Range_Raw_From : constant Word_Index := 0;
   Range_Gaps_At : constant Word_Index := 4;
   Gap_Row_Words : constant := 4;     --  first us, last us, count, reason
   Range_Gap_Rows : constant := 8;
   Range_Rows_At : constant Word_Index := Range_Gaps_At + Range_Gap_Rows * Gap_Row_Words;
   Raw_Row_Words : constant := 2;     --  time us, value
   Window_Row_Words : constant := 8;  --  start, count, sum, min, max, last, p50, p99
   Raw_Rows : constant := (Page_Words - Range_Rows_At) / Raw_Row_Words;
   Window_Rows : constant := (Page_Words - Range_Rows_At) / Window_Row_Words;

   --  Read_Window's answer.
   Stat_Count : constant := 0;
   Stat_Sum : constant := 1;
   Stat_Minimum : constant := 2;
   Stat_Maximum : constant := 3;
   Stat_Last : constant := 4;
   Stat_P50 : constant := 5;
   Stat_P90 : constant := 6;
   Stat_P99 : constant := 7;
   Stat_P999 : constant := 8;
   --  Per second, in thousandths: a counter's increments, others' samples.
   Stat_Rate_Milli : constant := 9;
   Stat_Missing : constant := 10;
   Stat_Resolution : constant := 11;
   Stat_Kind : constant := 12;
   Stat_Unit : constant := 13;
   Stat_Error_Permille : constant := 14;
   Stat_Sum_Saturated : constant := 15;
   Stat_Words : constant := 16;

   --  Put Text (at most Bytes of it) at Into (At_Word): its length, then
   --  its bytes packed.
   procedure Put_Text (Into : in out Page; At_Word : Word_Index; Text : String; Bytes : Positive)
   with Pre => Bytes mod 8 = 0 and then At_Word <= Page_Words - 1 - Bytes / 8;
   --  The text at From (At_Word), at most Bytes long.
   function Get_Text (From : Page; At_Word : Word_Index; Bytes : Positive) return String
   with Pre => Bytes mod 8 = 0 and then At_Word <= Page_Words - 1 - Bytes / 8;
end CuBit.Metric_Query_Pages;
