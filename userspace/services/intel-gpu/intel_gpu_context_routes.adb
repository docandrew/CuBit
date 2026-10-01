package body Intel_GPU_Context_Routes with SPARK_Mode is
   function Count (Object : Registry) return Natural is (Object.Used);
   function Matches (Object : Registry; Fence : Unsigned_16;
                     ID : Unsigned_32) return Boolean is
     (if ID = No_Context then
        (for all I in 1 .. Object.Used =>
          Fence not in Object.Items (I).First .. Object.Items (I).Last)
      else (for some I in 1 .. Object.Used => Object.Items (I).ID = ID and
          Fence in Object.Items (I).First .. Object.Items (I).Last));
   function Owner (Object : Registry; Fence : Unsigned_16) return Unsigned_32 is
   begin
      for I in 1 .. Object.Used loop
         if Fence in Object.Items (I).First .. Object.Items (I).Last then
            return Object.Items (I).ID;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I =>
             Fence not in Object.Items (J).First .. Object.Items (J).Last);
      end loop;
      return No_Context;
   end Owner;
   function Contains (Object : Registry; ID : Unsigned_32) return Boolean is
     (for some I in 1 .. Object.Used => Object.Items (I).ID = ID);
   function Select_Destination
     (Object : Registry; Payload : Intel_GPU_GuC_Context_Event.Words;
      Fence : Unsigned_16) return Destination is
      package Events renames Intel_GPU_GuC_Context_Event;
      Item : constant Events.Event := Events.Decode (Payload, Fence);
      ID : Unsigned_32 := No_Context;
   begin
      case Item.Tag is
         when Events.Malformed => return (Invalid_Message, No_Context);
         when Events.Scheduling_Done =>
            if Contains (Object, Item.ID) then ID := Item.ID; end if;
         when Events.Request_Failure => ID := Owner (Object, Fence);
         when Events.Other_Message => null;
      end case;
      return (if ID = No_Context then (Unclaimed, No_Context)
              else (Context_Message, ID));
   end Select_Destination;
   procedure Register
     (Object : in out Registry; ID : Unsigned_32;
      First, Last : Unsigned_16; Accepted : out Boolean) is
   begin
      Accepted := False;
      if ID >= No_Context or First = 0 or
        Unsigned_32 (Last) < Unsigned_32 (First) + 3 or
        Object.Used = Capacity then return; end if;
      for I in 1 .. Object.Used loop
         if Object.Items (I).ID = ID or else
           (First <= Object.Items (I).Last and Last >= Object.Items (I).First)
         then return; end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => Object.Items (J).ID /= ID and
             (First > Object.Items (J).Last or Last < Object.Items (J).First));
      end loop;
      Object.Used := Object.Used + 1;
      Object.Items (Object.Used) := (ID, First, Last);
      Accepted := True;
   end Register;
end Intel_GPU_Context_Routes;
