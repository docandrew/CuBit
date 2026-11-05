pragma Ada_2022;
with Ada.Strings.Fixed; with Ada.Text_IO; with Timing_Test_Control;
package body ACPI_Native_Log is
   procedure Info (Text : String) is
      use Ada.Strings.Fixed;
   begin
      Timing_Test_Control.Lines := Timing_Test_Control.Lines+1;
      if Text'Length > 240 then raise Program_Error with "timing log too long"; end if;
      if Index (Text,"kind=MODULE us=80 external_us=40 unaccounted_us=40") > 0 then Timing_Test_Control.Found_Module := True; end if;
      if Index (Text,"quality=COMPLETE") > 0 then Timing_Test_Control.Complete := True; end if;
      if Index (Text,"expected=CAFE0003 replacement=CAFE0000 observed=CAFE0003") > 0 then Timing_Test_Control.Found_FACS := True; end if;
      if Index (Text,"outcome=SIGNAL_DEFERRED signal=NOT_SENT") > 0 then Timing_Test_Control.Found_Deferred := True; end if;
      if Index (Text,"op=MAINTENANCE") > 0 and Index (Text,"signal=SIGNALED") > 0 then Timing_Test_Control.Found_Signal := True; end if;
      if Index (Text,"kind=WORK_ALLOCATION us=25 external_us=0 unaccounted_us=25") > 0 then Timing_Test_Control.Found_Allocation := True; end if;
      if Index (Text,"kind=WORK_LOOKUP us=60 external_us=10 unaccounted_us=50") > 0 then Timing_Test_Control.Found_Lookup := True; end if;
      if Index (Text,"basis=WALL_ONLY us=25 from=WORK_ALLOCATION:ENTER to=WORK_ALLOCATION:LEAVE") > 0 then Timing_Test_Control.Found_Gap := True; end if;
      if Index (Text,"timing-work ok dropped=1 open=0 overflow=0") > 0 then Timing_Test_Control.Found_Drop := True; end if;
      if Index (Text,"quality=INCOMPLETE") > 0 then Timing_Test_Control.Incomplete := True; end if;
      if Index (Text,"kind=WORK_ALLOCATION us=0 external_us=0 unaccounted_us=0 n=0 samples=0 max_us=0") > 0 then Timing_Test_Control.Found_Zero_Allocation := True; end if;
      Ada.Text_IO.Put_Line (Text);
   end Info;
end ACPI_Native_Log;
