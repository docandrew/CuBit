package body Region_Registry with SPARK_Mode => On is
   function Exclusive (Object : Registry) return Boolean is
     (for all I in Slot => (for all J in Slot =>
       (if I < J and Object.Items (I).Status /= Absent and
         Object.Items (J).Status /= Absent and
         Object.Items (I).Physical_Base /= 0 and Object.Items (J).Physical_Base /= 0
        then
          (if Object.Items (I).Physical_Base <= Object.Items (J).Physical_Base
           then Object.Items (I).Bytes <=
             Object.Items (J).Physical_Base - Object.Items (I).Physical_Base
           else Object.Items (J).Bytes <=
             Object.Items (I).Physical_Base - Object.Items (J).Physical_Base))));
   function Valid (Owner, Base, Bytes : Unsigned_64) return Boolean is
     (Owner /= 0 and First_Address < Limit_Address and
      Limit_Address <= 2 ** 47 and Base /= 0 and Base >= First_Address and
      Base < Limit_Address and Bytes /= 0 and
      Bytes <= Limit_Address - Base and Base mod 4096 = 0 and Bytes mod 4096 = 0);

   function State (Object : Registry; Owner : Unsigned_64; Key : Handle) return Phase is
     (if Owner /= 0 and Object.Items (Key.Index).Owner = Owner and
         Object.Items (Key.Index).Generation = Key.Generation
      then Object.Items (Key.Index).Status else Absent);

   function Overlaps (Object : Registry; Owner, Base, Bytes : Unsigned_64) return Boolean is
   begin
      -- Fail closed for malformed ranges; subtraction avoids end overflow.
      if Owner = 0 or Bytes = 0 or Bytes > Unsigned_64'Last - Base then
         return True;
      end if;
      for Item of Object.Items loop
         if Item.Status /= Absent and then Item.Owner = Owner and then
           (if Base <= Item.Base then Bytes > Item.Base - Base
            else Item.Bytes > Base - Item.Base)
         then return True; end if;
      end loop;
      return False;
   end Overlaps;

   function Describe (Object : Registry; Owner : Unsigned_64; Key : Handle)
     return Description is
      Current : constant Phase := State (Object, Owner, Key);
   begin
      if Current = Absent then return (Absent, 0, 0, 0); end if;
      return (Current, Object.Items (Key.Index).Base, Object.Items (Key.Index).Bytes,
              Object.Items (Key.Index).Physical_Base);
   end Describe;

   procedure Reserve (Object : in out Registry; Owner, Base, Bytes : Unsigned_64;
                      Key : out Handle; Success : out Boolean) is
   begin
      Key := (Slot'First, 0);
      Success := False;
      if not Valid (Owner, Base, Bytes) or else
        Overlaps (Object, Owner, Base, Bytes) then return; end if;
      for I in Slot loop
         if Object.Items (I).Status = Absent and not Object.Items (I).Exhausted then
            Object.Items (I).Owner := Owner;
            Object.Items (I).Base := Base;
            Object.Items (I).Bytes := Bytes;
            Object.Items (I).Physical_Base := 0;
            Object.Items (I).Status := Reserved;
            Key := (I, Object.Items (I).Generation);
            Success := True;
            return;
         end if;
      end loop;
   end Reserve;

   function Physical_Overlap (Object : Registry; Base, Bytes : Unsigned_64)
     return Boolean is
     (Bytes = 0 or else Bytes > Unsigned_64'Last - Base or else
       (for some Item of Object.Items =>
         Item.Status /= Absent and then Item.Physical_Base /= 0 and then
           (if Base <= Item.Physical_Base then Bytes > Item.Physical_Base - Base
            else Item.Bytes > Base - Item.Physical_Base)));

   procedure Bind_Backing (Object : in out Registry; Owner : Unsigned_64;
                          Key : Handle; Physical_Base : Unsigned_64;
                          Success : out Boolean) is
      Item : Entry_Record renames Object.Items (Key.Index);
   begin
      Success := False;
      if State (Object, Owner, Key) /= Reserved or Item.Physical_Base /= 0 or
        Physical_Base = 0 or Physical_Base mod 4096 /= 0 or
        Physical_Base >= 2 ** 48 then return; end if;
      if Item.Bytes > 2 ** 48 - Physical_Base or else
        Physical_Overlap (Object, Physical_Base, Item.Bytes) then return; end if;
      Item.Physical_Base := Physical_Base;
      Success := True;
   end Bind_Backing;

   procedure Commit (Object : in out Registry; Owner : Unsigned_64; Key : Handle;
                     Success : out Boolean) is
   begin
      Success := State (Object, Owner, Key) = Reserved and
        Object.Items (Key.Index).Physical_Base /= 0;
      if Success then Object.Items (Key.Index).Status := Live; end if;
   end Commit;

   procedure Begin_Retirement (Object : in out Registry; Owner : Unsigned_64;
                               Key : Handle; Success : out Boolean) is
      Current : constant Phase := State (Object, Owner, Key);
   begin
      Success := Current = Reserved or Current = Live;
      if Success then Object.Items (Key.Index).Status := Retiring; end if;
   end Begin_Retirement;

   procedure Finish_Retirement (Object : in out Registry; Owner : Unsigned_64;
                                Key : Handle; Success : out Boolean) is
      Item : Entry_Record renames Object.Items (Key.Index);
   begin
      Success := State (Object, Owner, Key) = Retiring;
      if not Success then return; end if;
      Item.Status := Absent;
      Item.Owner := 0;
      if Item.Generation = Unsigned_64'Last then
         Item.Exhausted := True;
      else
         Item.Generation := Item.Generation + 1;
      end if;
   end Finish_Retirement;
end Region_Registry;
