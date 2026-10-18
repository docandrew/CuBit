--  Small instances of the channel core units for hosted tests and proof.
pragma Ada_2022;
with CuBit.Bell_Sets;
with CuBit.Channel_Admission;
with CuBit.Channel_Schedule;
with CuBit.Request_Tags;
with CuBit.Channel_Limits;

package Core_Instances with SPARK_Mode is
   type Class is (Urgent, Interactive, Normal);
   package Bells is new CuBit.Bell_Sets (Bucket_Count => 8, Ways => 2);
   package Admission is new CuBit.Channel_Admission (Offer_Count => 4);
   package Schedule is new CuBit.Channel_Schedule (Class => Class);
   package Tags is new CuBit.Request_Tags (Capacity => 8);

   --  The production instances (proved as instantiated in the runtime):
   --  CuBit.Channel_Bells' set, Channel_Servers' schedule over the declared
   --  classes, its admission at the manifest's most offers, and a client's
   --  tags at the standard request channel's completion slots (2 ** 5).
   package Process_Bells is new CuBit.Bell_Sets (Bucket_Count => 64, Ways => 4);
   package Declared_Schedule is new CuBit.Channel_Schedule (CuBit.Channel_Limits.Channel_Class);
   package Most_Offers is new CuBit.Channel_Admission (Offer_Count => 8);
   package Channel_Tags is new CuBit.Request_Tags (Capacity => 32);
end Core_Instances;
