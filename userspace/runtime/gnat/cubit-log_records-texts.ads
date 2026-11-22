------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  String-returning views of log records, for programs with a secondary
--  stack (logstore, the apps, CCL). The record core (CuBit.Log_Records)
--  returns nothing unconstrained, so programs built with
--  No_Secondary_Stack (netstack, libc) publish through it alone.
------------------------------------------------------------------------------
pragma Ada_2022;
package CuBit.Log_Records.Texts with Pure, SPARK_Mode is
   function Text (Item : Log_Record) return String
   with Post => Text'Result'First = 1 and then Text'Result'Length = Text_Length (Item);
   function Name (Item : Field) return String
   with Post => Name'Result'First = 1 and then Name'Result'Length = Name_Length (Item)
                and then Has_Name (Item, Name'Result);

   --  A severity as the CCL value it is (CCL.Interfaces.Logs's Severity):
   --  "Severity.Information". Settings hold this spelling.
   function Severity_Literal (Level : Severity) return String is
     (case Level is
         when Trace => "Severity.Trace", when Debug => "Severity.Debug",
         when Information => "Severity.Information", when Warning => "Severity.Warning",
         when Error => "Severity.Error", when Critical => "Severity.Critical");
   function Is_Severity_Literal (Text : String) return Boolean is
     (for some Level in Severity => Text = Severity_Literal (Level));
   function Severity_Of (Text : String) return Severity
     with Pre => Is_Severity_Literal (Text),
          Post => Severity_Literal (Severity_Of'Result) = Text;
end CuBit.Log_Records.Texts;
