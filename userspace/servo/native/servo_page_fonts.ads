-- Whether Penny's pages can show text. Servo's font list reports it once at
-- startup (cubitshell, cubit_servo_fonts_missing); every window's status bar
-- shows a missing font list instead of silently drawing pages without text.
package Servo_Page_Fonts with SPARK_Mode is
   type Font_List_State is (Available, Missing);
   State : Font_List_State := Available;
end Servo_Page_Fonts;
