with Observatory_Metric_Observer;
procedure Compile_Observer is
   package Observer is new Observatory_Metric_Observer (17);
begin
   Observer.Close;
end Compile_Observer;
