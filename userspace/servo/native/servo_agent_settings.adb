with CCL.Setting_Codecs.Browser_User_Agent;

package body Servo_Agent_Settings is
   package Codec renames CCL.Setting_Codecs.Browser_User_Agent;

   procedure Decode (Source : String; Item : out Value; Success : out Boolean) is
      Setting : Codec.Browser_User_Agent_Value;
   begin
      Codec.Decode (Source, Setting, Success);
      Item := (if not Success then Penny
               else (case Setting.presentation is
                       when Codec.Penny => Penny, when Codec.Linux => Linux,
                       when Codec.Windows => Windows, when Codec.Mac => Mac));
   end Decode;

   function Encode (Item : Value) return String is
     ("(Browser_User_Agent presentation => Agent_Presentation." & Name (Item) & ")");
end Servo_Agent_Settings;
