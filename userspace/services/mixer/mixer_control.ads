with Interfaces; use Interfaces;

--  Client control admission, before narrowing any untrusted message fields.
package Mixer_Control with SPARK_Mode, Pure is
   type Owner_Table is array (Natural range <>) of Unsigned_64;
   type Words is array (0 .. 3) of Unsigned_64;
   function Master_Allowed
     (Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Data : Words;
      Caller, Control_Authority : Unsigned_64) return Boolean;
   function Allowed
     (Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Data : Words; Caller : Unsigned_64;
      Owners : Owner_Table) return Boolean;
end Mixer_Control;
