package body Servo_Tab_Projection with SPARK_Mode is
   function Valid (Value : Snapshot) return Boolean is
      Previous : Unsigned_64 := 0;
      Found : Boolean := False;
   begin
      if Value.Count > Capacity or else Value.Total < Unsigned_64 (Value.Count)
        or else Value.Reserved /= 0
      then return False; end if;
      if Value.Count = 0 then
         if Value.Total /= 0 or else Value.Active /= 0 then return False; end if;
      elsif Value.Active = 0 then return False;
      end if;
      for I in Slot loop
         declare
            Item : Row renames Value.Items (I);
         begin
            if Item.Reserved /= 0 or else Item.Length > 64 then return False; end if;
            if Unsigned_32 (I) <= Value.Count then
               if Item.ID <= Previous then return False; end if;
               Previous := Item.ID;
               Found := Found or Item.ID = Value.Active;
            elsif Item.ID /= 0 or else Item.Length /= 0 then return False;
            end if;
            for J in Item.Text'Range loop
               if Unsigned_32 (J) > Item.Length and then Item.Text (J) /= 0
               then return False; end if;
            end loop;
         end;
      end loop;
      return Value.Count = 0 or Found;
   end Valid;

   function Active_Row (Value : Snapshot) return Selection is
   begin
      for I in Slot loop
         if Unsigned_32 (I) <= Value.Count and then Value.Items (I).ID = Value.Active
         then return I; end if;
      end loop;
      return 0;
   end Active_Row;

   function ID_At (Value : Snapshot; Position : Natural) return Unsigned_64 is
   begin
      if Position in Slot and then Unsigned_64 (Position) <= Unsigned_64 (Value.Count)
      then return Value.Items (Position).ID; end if;
      return 0;
   end ID_At;

   function Same_Mapping (Left, Right : Snapshot) return Boolean is
   begin
      if Left.Count /= Right.Count then return False; end if;
      for I in Slot loop
         if Left.Items (I).ID /= Right.Items (I).ID then return False; end if;
      end loop;
      return True;
   end Same_Mapping;

   procedure Publish
     (Current : in out Snapshot; Incoming : Snapshot; Accepted : out Boolean) is
   begin
      Accepted := Valid (Incoming);
      if Accepted then Current := Incoming; end if;
   end Publish;
end Servo_Tab_Projection;
