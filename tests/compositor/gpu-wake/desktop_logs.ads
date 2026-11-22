with Ada.Strings.Unbounded;
--  Hosted stand-in: records what Desktop would log.
package Desktop_Logs is
   Lines : Natural := 0;
   Last_Line : Ada.Strings.Unbounded.Unbounded_String;
   procedure Write (Text : String);
end Desktop_Logs;
