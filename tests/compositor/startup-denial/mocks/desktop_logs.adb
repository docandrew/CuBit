with Test_Control;
package body Desktop_Logs is
 procedure Write(Text : String) is
  Prefix : constant String := "DESKTOP-VULKAN: setup unavailable stage=";
 begin
  if Text'Length >= Prefix'Length and then
    Text(Text'First .. Text'First+Prefix'Length-1)=Prefix then
   Test_Control.Stage_Logs := Test_Control.Stage_Logs+1;
   Test_Control.Last_Stage := (others=>' ');
   Test_Control.Stage_Length := Text'Length-Prefix'Length-1;
   Test_Control.Last_Stage(1..Test_Control.Stage_Length) :=
     Text(Text'First+Prefix'Length .. Text'Last-1);
  end if;
 end Write;
end Desktop_Logs;
