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
               Remote_Address => Address, Remote_Port => Port,
               Head => 0, Count => 0, Pending => Item.Channels (Index).Pending);
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
     (Item : in out Table; Destination_Port : Unsigned_16;
      Source_Address : Unsigned_32; Source_Port : Unsigned_16;
      Payload : Byte_Array; Index : out Channel_Index; Result : out Delivery)
   is
   begin
      Index := Channel_Index'First;
      if Payload'Length > Maximum_Payload then
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
               if C.Count = Queue_Depth then
                  Result := Queue_Full;
                  return;
               end if;
               declare
                  Slot : constant Queue_Index := (C.Head + C.Count) mod Queue_Depth;
               begin
                  C.Pending (Slot).Length := Payload'Length;
                  for K in 0 .. Payload'Length - 1 loop
                     C.Pending (Slot).Data (K + 1) := Payload (Payload'First + K);
                  end loop;
               end;
               C.Count := C.Count + 1;
               Result := Queued;
               return;
            end if;
         end;
      end loop;
      Result := No_Channel;
   end Deliver;

   procedure Take
     (Item : in out Table; Index : Channel_Index; Output : out Byte_Array;
      Length : out Natural; Truncated : out Boolean; Found : out Boolean)
   is
      C : Channel renames Item.Channels (Index);
   begin
      Output := [others => 0];
      Length := 0;
      Truncated := False;
      Found := False;
      if not C.Active or else C.Count = 0 then
         return;
      end if;
      declare
         Head : constant Queue_Index := C.Head;
         Stored : constant Payload_Length := C.Pending (Head).Length;
         Copied : constant Natural := Natural'Min (Stored, Output'Length);
      begin
         for K in 0 .. Copied - 1 loop
            pragma Loop_Invariant (Copied <= Output'Length);
            Output (Output'First + K) := C.Pending (Head).Data (K + 1);
         end loop;
         Length := Copied;
         Truncated := Stored > Output'Length;
         --  Received bytes are peer-visible, but clear them anyway so a later
         --  channel on this slot cannot observe a previous datagram.
         C.Pending (Head) := (Length => 0, Data => [others => 0]);
      end;
      C.Head := (C.Head + 1) mod Queue_Depth;
      C.Count := C.Count - 1;
      Found := True;
   end Take;

   procedure Close (Item : in out Table; Index : Channel_Index) is
   begin
      Item.Channels (Index) := (others => <>);
   end Close;
end UDP_Channels;
