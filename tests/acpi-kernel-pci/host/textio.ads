with Ada.Strings.Unbounded;
package TextIO is
   Captured : Ada.Strings.Unbounded.Unbounded_String;
   procedure Print (Value : String);
   procedure Print (Value : Integer);
   procedure Println (Value : String);
end TextIO;
