package body Intel_GPU_Table_References is
   function Capacity (Object : Map) return Positive is
     (Positive'Min (Quota, Records.Capacity (Object.Items)));
   function Generation (Object : Map) return Unsigned_64 is (Object.Epoch);
   function Metadata_Bytes return Unsigned_64 is
     ((Unsigned_64 (Quota - Bootstrap) * Unsigned_64 (Entry_Record'Object_Size / 8)
       + 4095) / 4096 * 4096);
   procedure Extend
     (Object : in out Map; Base, Bytes : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Bytes > Metadata_Bytes then return; end if;
      Records.Extend (Object.Items, Base, Bytes, Accepted);
   end Extend;
   function Get
     (Object : Map; Expected_Generation : Unsigned_64; Ordinal : Natural)
      return Natural is
      Item : Entry_Record;
   begin
      if Expected_Generation /= Object.Epoch or else
        Ordinal = 0 or else Ordinal > Capacity (Object) then return 0; end if;
      Item := Records.Get (Object.Items, Ordinal);
      return (if Item.Epoch = Object.Epoch then Item.ID else 0);
   end Get;
   procedure Put
     (Object : in out Map; Expected_Generation : Unsigned_64;
      Ordinal, ID : Natural; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Expected_Generation /= Object.Epoch or else Ordinal = 0 or else
        Ordinal > Capacity (Object) or else ID = 0 then return; end if;
      Records.Put (Object.Items, Ordinal, (Object.Epoch, ID));
      Accepted := True;
   end Put;
   procedure Reopen
     (Object : in out Map; Expected_Generation, Next_Generation : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Expected_Generation /= Object.Epoch or else Object.Epoch = Unsigned_64'Last
        or else Next_Generation /= Object.Epoch + 1 then return; end if;
      Object.Epoch := Next_Generation;
      Accepted := True;
   end Reopen;
end Intel_GPU_Table_References;
