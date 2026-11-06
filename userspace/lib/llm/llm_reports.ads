------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What llm.svc logs about an exchange (AGENT-001 slice 1, agent-security.md
--  "Explainable agent action"): who asked, under which declared mission,
--  which model, the usage and cost, how it stopped and how long it took.
--
--  @description
--  Never the key, the prompt or the response text: the record is built
--  from metadata only. Texts that come from outside (the declared mission,
--  the model the server named, the server's error message) are shown as
--  printable ASCII only, cut to a named length, and with every occurrence
--  of the key replaced (Redact), in case a server echoes it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Llm_Protocol;
with Llm_Costs;

package Llm_Reports with SPARK_Mode is
   package P renames CuBit.Llm_Protocol;

   Redacted : constant String := "[redacted]";
   Maximum_Shown : constant := 160;

   Maximum_Name : constant := 32;
   function Stop_Name (Value : P.Stop_Reason) return String
   with Post => Stop_Name'Result'Length <= Maximum_Name;
   function Error_Name (Value : P.Error_Kind) return String
   with Post => Error_Name'Result'Length <= Maximum_Name;
   function Problem_Name (Value : P.Problem) return String
   with Post => Problem_Name'Result'Length <= Maximum_Name;

   --  Text with every occurrence of Key replaced by Redacted, then every
   --  byte outside printable ASCII by '?', cut to Maximum_Shown.
   function Redact (Text, Key : String) return String
   with Pre => Text'Length <= 4 * Maximum_Shown and then Key'Length <= P.Maximum_Key_Bytes,
        Post => Redact'Result'Length <= Maximum_Shown;

   type Exchange_Facts is record
      Exchange : Unsigned_64 := 0;
      Source : Unsigned_64 := 0;          --  the asking process
      Stop : P.Stop_Reason := P.Disconnected;
      Error : P.Error_Kind := P.No_Error;
      Fault : P.Problem := P.No_Problem;
      Status : Natural := 0;
      Usage : Llm_Costs.Usage;
      Priced : Boolean := False;
      Cost : Llm_Costs.Nano_Usd := 0;
      Duration_Ms : Unsigned_64 := 0;
      Text_Bytes : Unsigned_64 := 0;
      --  Tool calls the response asked for (a captured exchange).
      Tool_Calls : Natural := 0;
   end record;

   --  The longest log line (everything in it is bounded well below).
   Maximum_Line : constant := 1_024;
   --  One log line: "llm: exchange 3 source 12 mission "console" model
   --  claude-opus-5-5 stop end_turn input 25 output 12 ..." Mission and
   --  Model are redacted here (Key is only compared, never copied).
   function Line (Facts : Exchange_Facts; Mission, Model, Key : String) return String
   with Pre => Mission'Length <= 4 * Maximum_Shown and then Model'Length <= 4 * Maximum_Shown
               and then Key'Length <= P.Maximum_Key_Bytes;
end Llm_Reports;
