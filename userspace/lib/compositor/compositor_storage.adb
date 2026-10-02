package body Compositor_Storage with SPARK_Mode is
   function Total (S : State) return Long_Long_Integer is
     (Long_Long_Integer (S.Items (1).Size) + Long_Long_Integer (S.Items (2).Size) +
      Long_Long_Integer (S.Items (3).Size) + Long_Long_Integer (S.Items (4).Size) +
      Long_Long_Integer (S.Items (5).Size) + Long_Long_Integer (S.Items (6).Size) +
      Long_Long_Integer (S.Items (7).Size) + Long_Long_Integer (S.Items (8).Size));
   procedure Reserve (S : in out State; Size : Positive; T : out Ticket) is
   begin
      T := No_Ticket;
      if S.Serial = Last_Identity or Size > S.Capacity - S.Used then return; end if;
      for I in Slot loop
         if S.Items (I).Stage = Free then
            S.Serial := S.Serial + 1;
            S.Items (I) := (S.Serial, Size, Allocating);
            S.Used := S.Used + Size;
            T := (I, S.Serial);
            return;
         end if;
      end loop;
   end Reserve;
   procedure Allocated (S : in out State; T : Ticket; Success : Boolean) is
   begin
      S.Items (T.Position).Stage := (if Success then Live else Quarantined);
   end Allocated;
   procedure Begin_Release (S : in out State; T : Ticket; Readers_Retired : Boolean) is
   begin
      if Readers_Retired then S.Items (T.Position).Stage := Releasing; end if;
   end Begin_Release;
   procedure Released (S : in out State; T : Ticket; Confirmed : Boolean) is
   begin
      if Confirmed then
         S.Used := S.Used - S.Items (T.Position).Size;
         S.Items (T.Position).Size := 0;
         S.Items (T.Position).Stage := Free;
      else
         S.Items (T.Position).Stage := Quarantined;
      end if;
   end Released;
end Compositor_Storage;
