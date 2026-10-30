with Interfaces;
package AML_Object_Identifiers with SPARK_Mode, Pure is
   Max_Objects : constant := 2048;
   subtype Object_ID is Natural range 0 .. Max_Objects;
   type Slot_Incarnation is new Interfaces.Unsigned_64;
   No_Incarnation : constant Slot_Incarnation := 0;
   First_Incarnation : constant Slot_Incarnation := 1;
   subtype Incarnation_Budget is Slot_Incarnation range First_Incarnation .. Slot_Incarnation'Last;
   -- Structural store-local metadata; arena identity supplies owner isolation.
   type Object_Address is private;
   No_Address : constant Object_Address;
   function Make_Address (Slot : Object_ID; Stamp : Slot_Incarnation) return Object_Address;
   function Present (Address : Object_Address) return Boolean;
   function Slot_Of (Address : Object_Address) return Object_ID;
   function Incarnation_Of (Address : Object_Address) return Slot_Incarnation;
private
   type Object_Address is record
      Slot : Object_ID := 0;
      Stamp : Slot_Incarnation := No_Incarnation;
   end record;
   No_Address : constant Object_Address := (others => <>);
   function Make_Address (Slot : Object_ID; Stamp : Slot_Incarnation) return Object_Address is
     (if Slot = 0 or else Stamp = No_Incarnation then No_Address else (Slot, Stamp));
   function Present (Address : Object_Address) return Boolean is
     (Address.Slot /= 0 and then Address.Stamp /= No_Incarnation);
   function Slot_Of (Address : Object_Address) return Object_ID is (Address.Slot);
   function Incarnation_Of (Address : Object_Address) return Slot_Incarnation is (Address.Stamp);
end AML_Object_Identifiers;
