pragma Ada_2022;
with Interfaces;
-- Linux v6.12 i2c-hid behaviour facts only, no code copied: reset ack wait
-- is bounded to1000ms; Elan skips postreset wake and ignores0xffff IRQ data.
package HID_Reset_Policy with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_64;
   subtype Vendor is Interfaces.Unsigned_16;
   subtype Byte is Interfaces.Unsigned_8;
   use type Word, Vendor, Byte;
   Ack_Window : constant Word := 1_000;
   function Elan (ID : Vendor) return Boolean is (ID=16#04F3#);
   function Bogus_IRQ (ID : Vendor; Low, High : Byte) return Boolean is
     (Elan(ID) and then Low=16#FF# and then High=16#FF#);
   procedure Ack_Deadline (Now, Overall : Word; Until_Time : out Word; Accepted : out Boolean)
     with Post => Accepted=(Now<Overall and then Overall<Word'Last) and then
       (if Accepted then Until_Time>Now and then Until_Time<=Overall and then
          Until_Time-Now=Word'Min(Ack_Window,Overall-Now)
        else Until_Time=0);
end HID_Reset_Policy;
