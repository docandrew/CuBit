package body Network_Channel_Handles with SPARK_Mode is
   procedure Allocate
     (Item : in out Table; Owner, Authority : Unsigned_64;
      Index : out Channel_Reference) is
   begin
      Index := No_Channel;
      if Owner = 0 or Authority = 0 or Item.Next_Id = No_Handle then
         return;
      end if;
      for I in Item.Slots'Range loop
         if Item.Slots (I).Id = No_Handle then
            Item.Slots (I) := (Id => Item.Next_Id, Owner => Owner, Authority => Authority);
            if Item.Next_Id = Handle'Last then
               Item.Next_Id := No_Handle; -- permanently exhausted, never wrap
            else
               Item.Next_Id := Item.Next_Id + 1;
            end if;
            Index := I;
            return;
         end if;
      end loop;
   end Allocate;

   function Value (Item : Table; Index : Channel_Index) return Handle is
     (Item.Slots (Index).Id);

   function Resolve
     (Item : Table; Owner, Authority : Unsigned_64; Id : Handle)
      return Channel_Reference is
   begin
      if Id /= No_Handle then
         for I in Item.Slots'Range loop
            if Item.Slots (I).Id = Id and then
              Item.Slots (I).Owner = Owner and then
              Item.Slots (I).Authority = Authority
            then
               return I;
            end if;
         end loop;
      end if;
      return No_Channel;
   end Resolve;

   procedure Release (Item : in out Table; Index : Channel_Index) is
   begin
      Item.Slots (Index) := (others => <>);
   end Release;
end Network_Channel_Handles;
