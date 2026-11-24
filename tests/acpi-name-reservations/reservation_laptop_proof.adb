pragma Ada_2022;
package body reservation_laptop_proof with SPARK_Mode is
   procedure Exercise (Count, Owner : R.Node_ID;
      Last_Stamp, Owner_Stamp : AML_References.Node_Incarnation;
      Definition : AML_Definition_Loads.Definition_Identity) is
      Ticket : R.Ticket;
   begin
      R.Reserve (Ticket, Count, Owner, Last_Stamp, Owner_Stamp, Definition);
      pragma Assert (R.Active (Ticket));
      pragma Assert (R.Node (Ticket) = Count + 1 and then Owner < R.Node (Ticket));
      pragma Assert (R.Prior_Count (Ticket) = Count and then R.Prior_Stamp (Ticket) = Last_Stamp);
      pragma Assert (not R.Matches (Ticket, Count, Owner, Last_Stamp + 1, Owner_Stamp, Definition));
      pragma Assert (not R.Matches (Ticket, Count + 1, Owner, Last_Stamp, Owner_Stamp, Definition));
      pragma Assert (not R.Matches (Ticket, Count + 1, Owner, Last_Stamp + 1, Owner_Stamp + 1, Definition));
      R.Release (Ticket, Count + 1, Owner, Last_Stamp + 1, Owner_Stamp, Definition);
      pragma Assert (not R.Active (Ticket));
   end Exercise;
end reservation_laptop_proof;
