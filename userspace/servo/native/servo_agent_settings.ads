--  Penny's User-Agent presentation (docs/servo-port.md, "User-Agent"):
--  `browser.servo.user-agent`, a typed CCL Browser_User_Agent
--  (CCL.Interfaces.Browser_Agent), e.g.
--  (Browser_User_Agent presentation => Agent_Presentation.Windows): every
--  site's presentation. Read once at startup and handed to cubitshell
--  (Servo_Shell.User_Agent), which owns the User-Agent strings. Absent:
--  Penny's own.
package Servo_Agent_Settings is
   --  Agent_Presentation's literals, in its order (cubitshell's
   --  user_agent::Presentation reads the position).
   type Presentation is (Penny, Linux, Windows, Mac);
   subtype Value is Presentation;
   procedure Decode (Source : String; Item : out Value; Success : out Boolean);
   function Encode (Item : Value) return String;
   --  The literal's CCL spelling.
   function Name (Item : Presentation) return String is
     (case Item is
         when Penny => "Penny", when Linux => "Linux", when Windows => "Windows", when Mac => "Mac");
end Servo_Agent_Settings;
