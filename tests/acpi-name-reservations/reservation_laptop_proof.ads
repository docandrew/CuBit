pragma Ada_2022;
with AML_Child_Indices;
with AML_References;
with AML_Definition_Loads;
package reservation_laptop_proof with SPARK_Mode is
   package Index is new AML_Child_Indices (16384);
   package R is new Index.Name_Reservations
     (AML_References.Node_Incarnation, AML_Definition_Loads.Definition_Identity,
      AML_Definition_Loads.No_Definition);
   use type AML_References.Node_Incarnation;
   procedure Exercise (Count, Owner : R.Node_ID;
      Last_Stamp, Owner_Stamp : AML_References.Node_Incarnation;
      Definition : AML_Definition_Loads.Definition_Identity)
     with Pre => Count < 16384 and then Owner <= Count
       and then Last_Stamp < AML_References.Node_Incarnation'Last
       and then (if Owner = 0 then Owner_Stamp = 0
         else Owner_Stamp > 0 and then Owner_Stamp <= Last_Stamp);
end reservation_laptop_proof;
