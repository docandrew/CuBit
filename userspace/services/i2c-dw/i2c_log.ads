pragma Ada_2022;
with Interfaces;
package I2C_Log is
   procedure Stage (Name : String; OK : Boolean; Reason : String; Details : String := "");
   procedure Summary (Ready : Boolean);
   procedure Note (Text : String);
   procedure Disable (Reason : String);
   function Hex (Key : String; Value : Interfaces.Unsigned_64) return String;
end I2C_Log;
