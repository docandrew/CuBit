pragma Ada_2022;
package body AML_Objects with SPARK_Mode is
   function Slot_Bound (Store : State) return Object_ID is (Store.Used);
   function Live_Count (Store : State) return Object_ID is (Store.Live_Used);
   function Is_Live (Store : State; ID : Object_ID) return Boolean is
     (ID /= No_Object and then ID <= Store.Used and then Store.Objects (ID).Occupied);
   function Last_Incarnation (Store : State; ID : Object_ID)
      return AML_Object_Identifiers.Slot_Incarnation is (Store.Objects (ID).Stamp);
   function Incarnation_Limit (Store : State) return AML_Object_Identifiers.Incarnation_Budget is
     (Store.Generation_Limit);
   function Address_Of (Store : State; ID : Object_ID) return AML_Object_Identifiers.Object_Address is
     (if not Is_Live (Store, ID) then AML_Object_Identifiers.No_Address
      else AML_Object_Identifiers.Make_Address (ID, Store.Objects (ID).Stamp));
   function Matches_Address (Store : State; Address : AML_Object_Identifiers.Object_Address) return Boolean is
     (AML_Object_Identifiers.Present (Address)
      and then Is_Live (Store, AML_Object_Identifiers.Slot_Of (Address))
      and then Store.Objects (AML_Object_Identifiers.Slot_Of (Address)).Stamp =
        AML_Object_Identifiers.Incarnation_Of (Address));
   function Usage_Of (Store : State) return Usage is
     ((Objects => Store.Live_Used, Bytes => Store.Bytes_Used, Elements => Store.Elements_Used));
   function Byte_Count (Store : State) return Natural is (Store.Bytes_Used);
   function Element_Count (Store : State) return Natural is (Store.Elements_Used);
   function Extends (Store, Prior : State) return Boolean is
     (Store.Generation_Limit = Prior.Generation_Limit
      and then Store.Used >= Prior.Used and then Store.Live_Used >= Prior.Live_Used
      and then Store.Bytes_Used >= Prior.Bytes_Used and then Store.Elements_Used >= Prior.Elements_Used
      and then (for all I in 1 .. Max_Objects =>
        Store.Objects (I).Stamp >= Prior.Objects (I).Stamp
        and then (if Prior.Objects (I).Occupied then Store.Objects (I) = Prior.Objects (I)))
      and then Store.Bytes (1 .. Prior.Bytes_Used) = Prior.Bytes (1 .. Prior.Bytes_Used)
      and then Store.Elements (1 .. Prior.Elements_Used) = Prior.Elements (1 .. Prior.Elements_Used));
   function Fresh_Allocation (Store, Prior : State; ID : Object_ID) return Boolean is
     (Is_Live (Store, ID) and then not Is_Live (Prior, ID)
      and then Prior.Objects (ID).Stamp < Prior.Generation_Limit
      and then Store.Objects (ID).Stamp = Prior.Objects (ID).Stamp + 1
      and then Store.Generation_Limit = Prior.Generation_Limit
      and then Store.Live_Used = Prior.Live_Used + 1
      and then Store.Used = Object_ID'Max (Prior.Used, ID)
      and then Extends (Store, Prior)
      and then (for all I in 1 .. Max_Objects => (if I /= ID then Store.Objects (I) = Prior.Objects (I)))
      and then (case Store.Objects (ID).Tag is
        when Integer_Object | Reference_Object =>
          Store.Bytes_Used = Prior.Bytes_Used and then Store.Elements_Used = Prior.Elements_Used
          and then Store.Objects (ID).First = 0 and then Store.Objects (ID).Size = 0,
        when String_Object | Buffer_Object =>
          Store.Objects (ID).First = Prior.Bytes_Used
          and then Store.Objects (ID).Size <= Max_Bytes - Prior.Bytes_Used
          and then Store.Bytes_Used = Prior.Bytes_Used + Store.Objects (ID).Size
          and then Store.Elements_Used = Prior.Elements_Used,
        when Package_Object =>
          Store.Objects (ID).First = Prior.Elements_Used
          and then Store.Objects (ID).Size <= Max_Elements - Prior.Elements_Used
          and then Store.Elements_Used = Prior.Elements_Used + Store.Objects (ID).Size
          and then Store.Bytes_Used = Prior.Bytes_Used));
   function Valid (Store : State) return Boolean is
      Live : Object_ID := 0;
   begin
      for I in 1 .. Max_Objects loop
         declare Item : Object_Record renames Store.Objects (I); begin
            if Item.Stamp > Store.Generation_Limit then return False; end if;
            if I > Store.Used and then Item.Stamp /= AML_Object_Identifiers.No_Incarnation then return False; end if;
            if not Item.Occupied then
               if Item /= Object_Record'(Stamp => Item.Stamp, others => <>) then return False; end if;
            else
               if I > Store.Used or else Item.Stamp = AML_Object_Identifiers.No_Incarnation then return False; end if;
               Live := Live + 1;
               if Item.Tag /= Integer_Object and then Item.Origin /= AML_Decode.Ordinary_Integer then return False; end if;
               if Item.Tag /= Reference_Object and then Item.Reference /= AML_References.No_Reference then return False; end if;
               case Item.Tag is
                  when Integer_Object => null;
                  when Reference_Object =>
                     if Item.First /= 0 or else Item.Size /= 0 or else Item.Value /= 0
                       or else not AML_References.Well_Formed (Item.Reference) then return False; end if;
                  when String_Object | Buffer_Object =>
                     if Item.First > Store.Bytes_Used or else Item.Size > Store.Bytes_Used - Item.First then return False; end if;
                  when Package_Object =>
                     if Item.First > Store.Elements_Used or else Item.Size > Store.Elements_Used - Item.First then return False; end if;
                     for J in 1 .. Item.Size loop
                        if Store.Elements (Item.First + J) /= No_Object and then
                          not Is_Live (Store, Store.Elements (Item.First + J)) then return False; end if;
                     end loop;
               end case;
            end if;
         end;
      end loop;
      return Live = Store.Live_Used;
   end Valid;
   function Empty (Max_Incarnation : AML_Object_Identifiers.Incarnation_Budget :=
      AML_Object_Identifiers.Slot_Incarnation'Last) return State is
     ((Generation_Limit => Max_Incarnation, others => <>));
   function Kind (Store : State; ID : Object_ID) return Object_Kind is (Store.Objects (ID).Tag);
   function Length (Store : State; ID : Object_ID) return Natural is (Store.Objects (ID).Size);
   function Origin_Of (Store : State; ID : Object_ID) return AML_Decode.Integer_Origin is
     (Store.Objects (ID).Origin);
   function Integer_Data (Store : State; ID : Object_ID) return AML_Decode.Integer_Value is
     (Store.Objects (ID).Value);
   function Reference_Data (Store : State; ID : Object_ID) return AML_References.Reference is
     (Store.Objects (ID).Reference);
   function Reference_Allocated
     (Store, Prior : State; ID : Object_ID; Value : AML_References.Reference)
      return Boolean is
     (Fresh_Allocation (Store, Prior, ID)
      and then Store = (Prior with delta Used => Store.Used, Live_Used => Prior.Live_Used + 1,
        Objects => (Prior.Objects with delta ID =>
          Object_Record'(Occupied => True, Stamp => Prior.Objects (ID).Stamp + 1,
                         Tag => Reference_Object, Reference => Value, others => <>))));
   function Byte_Data (Store : State; ID : Object_ID) return AML_Decode.Bytes is
     (Store.Bytes (Store.Objects (ID).First + 1 .. Store.Objects (ID).First + Store.Objects (ID).Size));
   function Element (Store : State; ID : Object_ID; Index : Natural) return Object_ID is
     (Store.Elements (Store.Objects (ID).First + Index + 1));
   function Integer_Updated
     (Store, Prior : State; ID : Object_ID; Value : AML_Decode.Integer_Value)
      return Boolean is
     (Store = (Prior with delta Objects =>
       (Prior.Objects with delta ID =>
         (Prior.Objects (ID) with delta Value => Value))));
   function Stored_Byte (Store : State; ID : Object_ID; Index : Natural)
     return AML_Decode.Byte is
     (Store.Bytes (Store.Objects (ID).First + Index + 1));
   function Stored_Byte_Updated
     (Store, Prior : State; ID : Object_ID; Index : Natural;
      Value : AML_Decode.Byte) return Boolean is
     (Store = (Prior with delta Bytes =>
       (Prior.Bytes with delta Prior.Objects (ID).First + Index + 1 => Value)));
   procedure Set_Stored_Byte
     (Store : in out State; ID : Object_ID; Index : Natural; Value : AML_Decode.Byte)
   is
   begin
      Store.Bytes (Store.Objects (ID).First + Index + 1) := Value;
   end Set_Stored_Byte;
   function String_Replaced
     (Store, Prior : State; ID : Object_ID; Data : AML_Decode.Bytes)
      return Boolean is
     (Store.Used = Prior.Used
      and then Store.Live_Used = Prior.Live_Used
      and then Store.Generation_Limit = Prior.Generation_Limit
      and then Store.Elements_Used = Prior.Elements_Used
      and then Store.Elements = Prior.Elements
      and then Store.Objects =
        (Prior.Objects with delta ID =>
          (Prior.Objects (ID) with delta
             First => Prior.Bytes_Used, Size => Data'Length))
      and then Data'Length <= Max_Bytes - Prior.Bytes_Used
      and then Store.Bytes_Used = Prior.Bytes_Used + Data'Length
      and then Store.Bytes (1 .. Prior.Bytes_Used) =
        Prior.Bytes (1 .. Prior.Bytes_Used)
      and then Store.Bytes (Prior.Bytes_Used + 1 .. Store.Bytes_Used) = Data
      and then Store.Bytes (Store.Bytes_Used + 1 .. Max_Bytes) =
        Prior.Bytes (Store.Bytes_Used + 1 .. Max_Bytes));

   procedure Replace_String
     (Store : in out State; ID : Object_ID; Data : AML_Decode.Bytes;
      Status : out String_Update_Status)
   is
      Start : constant Natural := Store.Bytes_Used;
   begin
      if not Is_Live (Store, ID) then
         Status := Invalid_String_ID;
         return;
      end if;
      if Store.Objects (ID).Tag /= String_Object then
         Status := Not_A_String;
         return;
      end if;
      if Data'Length > Max_Bytes - Start then
         Status := String_Byte_Limit;
         return;
      end if;
      --  All rejection checks precede writes. Old extents remain reserved;
      --  references follow the stable object ID to the new extent.
      for I in 1 .. Data'Length loop
         Store.Bytes (Start + I) := Data (Data'First + (I - 1));
      end loop;
      Store.Objects (ID).First := Start;
      Store.Objects (ID).Size := Data'Length;
      Store.Bytes_Used := Start + Data'Length;
      Status := String_Updated;
   end Replace_String;

   function Buffer_Stored
     (Store, Prior : State; ID : Object_ID; Data : AML_Decode.Bytes)
      return Boolean
   is
      Expected : State := Prior;
      Start : Natural := Prior.Objects (ID).First;
      Size : Natural := Prior.Objects (ID).Size;
   begin
      if Size = 0 then
         if Data'Length > Max_Bytes - Prior.Bytes_Used then return False; end if;
         Start := Prior.Bytes_Used; Size := Data'Length;
         Expected.Objects (ID).First := Start;
         Expected.Objects (ID).Size := Size;
         Expected.Bytes_Used := Start + Size;
      end if;
      for I in 1 .. Size loop
         Expected.Bytes (Start + I) :=
           (if I <= Data'Length then Data (Data'First + (I - 1)) else 0);
      end loop;
      return Store = Expected;
   end Buffer_Stored;

   procedure Store_Buffer
     (Store : in out State; ID : Object_ID; Data : AML_Decode.Bytes;
      Status : out Buffer_Update_Status)
   is
      Start, Size : Natural;
   begin
      if not Is_Live (Store, ID) then
         Status := Invalid_Buffer_ID; return;
      end if;
      if Store.Objects (ID).Tag /= Buffer_Object then
         Status := Not_A_Buffer; return;
      end if;
      Start := Store.Objects (ID).First; Size := Store.Objects (ID).Size;
      if Size = 0 then
         if Data'Length > Max_Bytes - Store.Bytes_Used then
            Status := Buffer_Byte_Limit; return;
         end if;
         Start := Store.Bytes_Used; Size := Data'Length;
         Store.Objects (ID).First := Start;
         Store.Objects (ID).Size := Size;
         Store.Bytes_Used := Start + Size;
      end if;
      for I in 1 .. Size loop
         Store.Bytes (Start + I) :=
           (if I <= Data'Length then Data (Data'First + (I - 1)) else 0);
      end loop;
      Status := Buffer_Updated;
   end Store_Buffer;

   procedure Set_Integer
     (Store : in out State; ID : Object_ID; Value : AML_Decode.Integer_Value) is
   begin
      Store.Objects (ID).Value := Value;
   end Set_Integer;
   procedure Select_Slot (Store : State; ID : out Object_ID; Status : out Allocation_Status) is
   begin
      ID := No_Object;
      for I in 1 .. Max_Objects loop
         if not Store.Objects (I).Occupied and then Store.Objects (I).Stamp < Store.Generation_Limit then
            ID := I; Status := Allocated; return;
         end if;
      end loop;
      Status := (if Store.Live_Used = Max_Objects then Object_Limit else Generation_Limit);
   end Select_Slot;
   procedure Occupy (Store : in out State; ID : Object_ID; Item : Object_Record) with
     Pre => ID /= No_Object and then not Is_Live (Store, ID)
       and then Store.Objects (ID).Stamp < Store.Generation_Limit and then Store.Live_Used < Max_Objects;
   procedure Occupy (Store : in out State; ID : Object_ID; Item : Object_Record) is
      Stamp : constant AML_Object_Identifiers.Slot_Incarnation := Store.Objects (ID).Stamp + 1;
   begin
      Store.Objects (ID) := (Item with delta Occupied => True, Stamp => Stamp);
      Store.Used := Object_ID'Max (Store.Used, ID);
      Store.Live_Used := Store.Live_Used + 1;
   end Occupy;
   procedure New_Reference (Store : in out State; Value : AML_References.Reference;
                            ID : out Object_ID; Status : out Allocation_Status) is
      Slot : Object_ID;
   begin
      ID := No_Object;
      if not AML_References.Well_Formed (Value) then Status := Invalid_Reference; return; end if;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      Occupy (Store, Slot, (Tag => Reference_Object, Reference => Value, others => <>));
      ID := Slot;
   end New_Reference;
   procedure New_Integer (Store : in out State; Value : AML_Decode.Integer_Value;
                          ID : out Object_ID; Status : out Allocation_Status;
                          Origin : AML_Decode.Integer_Origin := AML_Decode.Ordinary_Integer) is
      Slot : Object_ID;
   begin
      ID := No_Object;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      Occupy (Store, Slot, (Tag => Integer_Object, Value => Value, Origin => Origin, others => <>));
      ID := Slot;
   end New_Integer;
   procedure New_Bytes (Store : in out State; Tag : Byte_Kind; Data : AML_Decode.Bytes;
                        ID : out Object_ID; Status : out Allocation_Status) is
      Start : constant Natural := Store.Bytes_Used;
      Slot : Object_ID;
   begin
      ID := No_Object;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      if Data'Length > Max_Bytes - Store.Bytes_Used then Status := Byte_Limit; return; end if;
      for I in 1 .. Data'Length loop
         Store.Bytes (Start + I) := Data (Data'First + (I - 1));
      end loop;
      Store.Bytes_Used := Store.Bytes_Used + Data'Length;
      Occupy (Store, Slot, (Tag => Tag, First => Start, Size => Data'Length, others => <>));
      ID := Slot;
   end New_Bytes;
   procedure New_Package (Store : in out State; Size : Natural;
                          ID : out Object_ID; Status : out Allocation_Status) is
      Start : constant Natural := Store.Elements_Used;
      Slot : Object_ID;
   begin
      ID := No_Object;
      Select_Slot (Store, Slot, Status); if Status /= Allocated then return; end if;
      if Size > Max_Elements - Store.Elements_Used then Status := Element_Limit; return; end if;
      for I in 1 .. Size loop Store.Elements (Start + I) := No_Object; end loop;
      Store.Elements_Used := Store.Elements_Used + Size;
      Occupy (Store, Slot, (Tag => Package_Object, First => Start, Size => Size, others => <>));
      ID := Slot;
   end New_Package;
   function Element_Updated
     (Store, Prior : State; ID : Object_ID; Index : Natural; Value : Object_ID)
     return Boolean is
     (Store = (Prior with delta Elements =>
       (Prior.Elements with delta Prior.Objects (ID).First + Index + 1 => Value)));
   procedure Set_Element (Store : in out State; ID : Object_ID; Index : Natural; Value : Object_ID) is
   begin
      Store.Elements (Store.Objects (ID).First + Index + 1) := Value;
   end Set_Element;
end AML_Objects;
