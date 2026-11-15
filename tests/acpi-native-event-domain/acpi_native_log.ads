package ACPI_Native_Log is
   Maximum_Lines:constant Positive:=256;
   Maximum_Length:constant Positive:=192;
   subtype Line is String(1..Maximum_Length);
   Count:Natural:=0;
   Lines:array(1..Maximum_Lines) of Line;
   Lengths:array(1..Maximum_Lines) of Natural;
   procedure Copied(Text:String;Peer_Quiet:Boolean;Success:Boolean:=False);
   procedure Info(Text:String);
   procedure Clear;
   function Saw(Text:String) return Boolean;
end ACPI_Native_Log;
