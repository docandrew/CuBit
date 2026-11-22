with Servo_Agent_Settings;

--  One host's User-Agent presentation: `browser.servo.site-agent.<host>`,
--  a typed CCL Site_User_Agent (CCL.Interfaces.Browser_Agent), e.g.
--  browser.servo.site-agent.example.com =
--    (Site_User_Agent presentation => Agent_Presentation.Windows).
--  It covers the host and its subdomains, over the global presentation
--  (Servo_Agent_Settings); cubitshell's table picks the most specific host.
package Servo_Site_Agent_Settings is
   subtype Value is Servo_Agent_Settings.Presentation;
   procedure Decode (Source : String; Item : out Value; Success : out Boolean);
   function Encode (Item : Value) return String;
end Servo_Site_Agent_Settings;
