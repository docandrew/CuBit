pragma Ada_2022;
package body UDP_Channels with SPARK_Mode is
   Ephemeral_Count : constant := Natural (Last_Ephemeral - First_Ephemeral) + 1;

   procedure Open
     (Item : in out Table; Index : Channel_Index;
      Address : Unsigned_32; Port : Unsigned_16; Success : out Boolean)
   is
      Candidate : Unsigned_16 := Item.Next_Port;
   begin
      Success := False;
      if Item.Channels (Index).Active or else Address = 0 or else Port = 0 then
         return;
      end if;
      if Candidate < First_Ephemeral then
         Candidate := First_Ephemeral;
      end if;
      for Attempt in 1 .. Ephemeral_Count loop
         pragma Loop_Invariant (Candidate >= First_Ephemeral);
         pragma Loop_Invariant (not Item.Channels (Index).Active);
         if not Port_In_Use (Item, Candidate) then
            Item.Channels (Index) :=
              (Active => True, Local_Port => Candidate,
               Remote_Address => Address, Remote_Port => Port);
            Item.Next_Port :=
              (if Candidate = Last_Ephemeral then First_Ephemeral else Candidate + 1);
            Success := True;
            return;
         end if;
         Candidate :=
           (if Candidate = Last_Ephemeral then First_Ephemeral else Candidate + 1);
      end loop;
   end Open;

   procedure Deliver
     (Item : Table; Destination_Port : Unsigned_16;
      Source_Address : Unsigned_32; Source_Port : Unsigned_16;
      Payload_Bytes : Natural; Index : out Channel_Index; Result : out Delivery)
   is
   begin
      Index := Channel_Index'First;
      if Payload_Bytes > Maximum_Payload then
         Result := Oversized;
         return;
      end if;
      for I in Channel_Index loop
         declare
            C : Channel renames Item.Channels (I);
         begin
            if C.Active and then C.Local_Port = Destination_Port and then
              C.Remote_Address = Source_Address and then C.Remote_Port = Source_Port
            then
               Index := I;
               Result := Matched;
               return;
            end if;
         end;
      end loop;
      Result := No_Channel;
   end Deliver;

   procedure Close (Item : in out Table; Index : Channel_Index) is
   begin
      Item.Channels (Index) := (others => <>);
   end Close;
end UDP_Channels;
