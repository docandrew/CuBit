--  Penny's User-Agent presentation as CCL types (docs/servo-port.md,
--  "User-Agent"). Penny presents its own User-Agent, the CuBit form
--  "Mozilla/5.0 (CuBit; x86_64; rv:...) Servo/... Firefox/..."
--  (Agent_Presentation.Penny); for compatibility a user may choose another
--  platform's Firefox instead, a fixed User-Agent and navigator.platform
--  per choice (cubitshell's user_agent.rs holds the strings), never free
--  text. (The literal is Penny, not CuBit: an enumeration literal CuBit
--  would hide the CuBit package in the generated decoders.)
--
--  `browser.servo.user-agent` is one Browser_User_Agent: every site's
--  presentation. Absent: Penny.
--  `browser.servo.site-agent.<host>` is one Site_User_Agent: the
--  presentation for that host and its subdomains, over the global one
--  (the most specific host wins), e.g.
--    browser.servo.site-agent.example.com =
--      (Site_User_Agent presentation => Agent_Presentation.Windows)
--
--  Both are checked against these declarations when the configuration is
--  compiled (CCL.Typed_Settings) and read back by Penny with the decoders
--  generated from them (CCL.Setting_Codecs.Browser_User_Agent and
--  .Site_User_Agent). Two root types, one per setting: each setting kind
--  has its own decoder.
package CCL.Interfaces.Browser_Agent with SPARK_Mode, Pure is
   PRESENTATION_SOURCE : constant String :=
     "(type Agent_Presentation (enum Penny Linux Windows Mac)) ";
   TYPE_SOURCE : constant String := PRESENTATION_SOURCE &
     "(type Browser_User_Agent (record (presentation Agent_Presentation Agent_Presentation.Penny)))";
   ROOT_TYPE_NAME : constant String := "Browser_User_Agent";
   SITE_TYPE_SOURCE : constant String := PRESENTATION_SOURCE &
     "(type Site_User_Agent (record (presentation Agent_Presentation Agent_Presentation.Penny)))";
   SITE_ROOT_TYPE_NAME : constant String := "Site_User_Agent";
end CCL.Interfaces.Browser_Agent;
