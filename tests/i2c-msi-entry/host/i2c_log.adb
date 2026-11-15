package body I2C_Log is
   procedure Stage (Name : String; Success : Boolean; Reason : String; Details : String := "") is
      pragma Unreferenced (Name, Success, Reason, Details);
   begin null; end Stage;
   procedure Note (Text : String) is pragma Unreferenced (Text); begin null; end Note;
   procedure Disable (Reason : String) is pragma Unreferenced (Reason); begin null; end Disable;
   procedure Summary (Good : Boolean) is pragma Unreferenced (Good); begin null; end Summary;
   function Hex (Name : String; Value : Interfaces.Unsigned_64) return String is
      pragma Unreferenced (Name, Value);
   begin return ""; end Hex;
end I2C_Log;
