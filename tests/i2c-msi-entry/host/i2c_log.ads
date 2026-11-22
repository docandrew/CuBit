with Interfaces;
package I2C_Log is
   procedure Stage (Name : String; Success : Boolean; Reason : String; Details : String := "");
   procedure Note (Text : String);
   procedure Disable (Reason : String);
   procedure Summary (Good : Boolean);
   function Hex (Name : String; Value : Interfaces.Unsigned_64) return String;
end I2C_Log;
