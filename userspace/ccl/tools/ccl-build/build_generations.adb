with Ada.Calendar.Formatting;
with Build_Encoding;
with Storehouse.Hex;

package body Build_Generations is
   use type Interfaces.Unsigned_64;

   Generation_Tag : constant String := "cubit.generation";
   Generation_Version : constant := 1;
   Generation_Fields : constant := 5;    --  tag, version, number, created, content
   Content_Tag : constant String := "cubit.generation-content";
   Content_Version : constant := 1;
   Content_Fields : constant := 4;       --  tag, version, parameters, components
   Component_Fields : constant := 2;     --  name, outputs
   Item_Fields : constant := 3;          --  name, kind, identity
   Current_Tag : constant String := "cubit.current";
   Current_Version : constant := 1;
   Current_Fields : constant := 3;       --  tag, version, number
   Root_Tag : constant String := "cubit.root";
   Root_First_Version : constant := 1;   --  products only
   Root_Version : constant := 2;         --  products and pins
   Root_First_Fields : constant := 3;
   Root_Fields : constant := 4;

   Profiles_Directory : constant String := "profiles";
   Current_Name : constant String := "current";
   Lock_Prefix : constant String := "profile-";
   Epoch_Year : constant := 1970;

   function "/" (Directory, Name : String) return String is (Directory & "/" & Name);

   function Profile_Directory (Profile : String) return String is (Profiles_Directory / Profile);
   function Generation_Name (Profile : String; Number : Generation_Number) return String is
     (Profile_Directory (Profile) / Image (Number));
   function Current_Pointer (Profile : String) return String is (Profile_Directory (Profile) / Current_Name);

   procedure Add_Item (W : in out Build_Encoding.Writer; I : Item) is
   begin
      W.Add_Array (Item_Fields);
      W.Add_Text (To_String (I.Name));
      W.Add_Unsigned (Entry_Kind'Pos (I.Kind));
      W.Add_Digest (I.Identity);
   end Add_Item;

   procedure Add_Items (W : in out Build_Encoding.Writer; Items : Item_Vectors.Vector) is
   begin
      W.Add_Array (Natural (Items.Length));
      for I of Items loop
         Add_Item (W, I);
      end loop;
   end Add_Items;

   procedure Add_Content (W : in out Build_Encoding.Writer; C : Content) is
   begin
      W.Add_Array (Content_Fields);
      W.Add_Text (Content_Tag);
      W.Add_Unsigned (Content_Version);
      Add_Items (W, C.Parameters);
      W.Add_Array (Natural (C.Components.Length));
      for Part of C.Components loop
         W.Add_Array (Component_Fields);
         W.Add_Text (To_String (Part.Name));
         Add_Items (W, Part.Outputs);
      end loop;
   end Add_Content;

   function Identity (C : Content) return Digest is
      W : Build_Encoding.Writer;
   begin
      Add_Content (W, C);
      return W.Identity;
   end Identity;

   procedure Expect (Condition : Boolean; What : String) is
   begin
      if not Condition then
         raise Build_Encoding.Decode_Error with What;
      end if;
   end Expect;

   function Read_Item (R : in out Build_Encoding.Reader) return Item is
   begin
      Expect (Build_Encoding.Read_Array (R) = Item_Fields, "not an item");
      declare
         Name : constant String := Build_Encoding.Read_Text (R);
         Kind : constant Interfaces.Unsigned_64 := Build_Encoding.Read_Unsigned (R);
      begin
         Expect (Kind <= Entry_Kind'Pos (Entry_Kind'Last), "not an entry kind");
         return (To_Unbounded_String (Name), Entry_Kind'Val (Kind), Build_Encoding.Read_Digest (R));
      end;
   end Read_Item;

   function Read_Items (R : in out Build_Encoding.Reader) return Item_Vectors.Vector is
      Result : Item_Vectors.Vector;
   begin
      for N in 1 .. Build_Encoding.Read_Array (R) loop
         Result.Append (Read_Item (R));
      end loop;
      return Result;
   end Read_Items;

   function Read_Content (R : in out Build_Encoding.Reader) return Content is
      Result : Content;
   begin
      Expect (Build_Encoding.Read_Array (R) = Content_Fields
              and then Build_Encoding.Read_Text (R) = Content_Tag
              and then Build_Encoding.Read_Unsigned (R) = Content_Version, "not a generation's content");
      Result.Parameters := Read_Items (R);
      for N in 1 .. Build_Encoding.Read_Array (R) loop
         Expect (Build_Encoding.Read_Array (R) = Component_Fields, "not a component");
         declare
            Name : constant String := Build_Encoding.Read_Text (R);
         begin
            Result.Components.Append (Component'(To_Unbounded_String (Name), Read_Items (R)));
         end;
      end loop;
      return Result;
   end Read_Content;

   function Encode (G : Generation) return Byte_Array is
      W : Build_Encoding.Writer;
   begin
      W.Add_Array (Generation_Fields);
      W.Add_Text (Generation_Tag);
      W.Add_Unsigned (Generation_Version);
      W.Add_Unsigned (Interfaces.Unsigned_64 (G.Number));
      W.Add_Unsigned (G.Created);
      Add_Content (W, G.Value);
      return W.Bytes;
   end Encode;

   function Decode (Data : Byte_Array; Number : Generation_Number) return Generation is
      R : Build_Encoding.Reader := (Length => Data'Length, Data => Data, Position => 1);
   begin
      Expect (Build_Encoding.Read_Array (R) = Generation_Fields
              and then Build_Encoding.Read_Text (R) = Generation_Tag
              and then Build_Encoding.Read_Unsigned (R) = Generation_Version, "not a generation");
      Expect (Build_Encoding.Read_Unsigned (R) = Interfaces.Unsigned_64 (Number), "names another number");
      declare
         Created : constant Seconds := Build_Encoding.Read_Unsigned (R);
         Value : constant Content := Read_Content (R);
      begin
         Expect (Build_Encoding.At_End (R), "bytes after the generation");
         return (Number, Created, Value);
      end;
   end Decode;

   function Profiles (Store : Build_Store.Store) return Name_Vectors.Vector is
      Result : Name_Vectors.Vector;
   begin
      for Name of Store.Names (Profiles_Directory) loop
         if Is_Profile_Name (Name) then
            Result.Append (Name);
         end if;
      end loop;
      return Result;
   end Profiles;

   function Numbers (Store : Build_Store.Store; Profile : String) return Number_Vectors.Vector is
      package Sorting is new Number_Vectors.Generic_Sorting;
      Result : Number_Vectors.Vector;
   begin
      for Name of Store.Names (Profile_Directory (Profile)) loop
         if Is_Generation_Name (Name) then
            Result.Append (Value (Name));
         end if;
      end loop;
      Sorting.Sort (Result);
      return Result;
   end Numbers;

   function Current (Store : Build_Store.Store; Profile : String) return Generation_Count is
   begin
      if not Store.Record_Exists (Current_Pointer (Profile)) then
         return 0;
      end if;
      declare
         Data : constant Byte_Array := Store.Read_Record (Current_Pointer (Profile));
         R : Build_Encoding.Reader := (Length => Data'Length, Data => Data, Position => 1);
      begin
         Expect (Build_Encoding.Read_Array (R) = Current_Fields
                 and then Build_Encoding.Read_Text (R) = Current_Tag
                 and then Build_Encoding.Read_Unsigned (R) = Current_Version, "not a current pointer");
         declare
            Number : constant Interfaces.Unsigned_64 := Build_Encoding.Read_Unsigned (R);
         begin
            Expect (Number in 1 .. Last_Generation and then Build_Encoding.At_End (R), "not a generation number");
            return Generation_Number (Number);
         end;
      end;
   exception
      when Build_Encoding.Decode_Error =>
         raise Generation_Error with "profile " & Profile & ": the current pointer is damaged";
   end Current;

   function Read (Store : Build_Store.Store; Profile : String; Number : Generation_Number) return Generation is
   begin
      if not Store.Record_Exists (Generation_Name (Profile, Number)) then
         raise Generation_Error with "profile " & Profile & " has no generation " & Image (Number);
      end if;
      return Decode (Store.Read_Record (Generation_Name (Profile, Number)), Number);
   exception
      when Build_Encoding.Decode_Error =>
         raise Generation_Error with "profile " & Profile & ": generation " & Image (Number) & " is damaged";
   end Read;

   --  The entries Value names that are not in the store, by name.
   function Missing (Store : Build_Store.Store; Value : Content) return String is
      Result : Unbounded_String;
      procedure Need (I : Item) is
      begin
         if not Store.Contains (I.Kind, I.Identity) then
            Append (Result, " " & To_String (I.Name) & " (" & Storehouse.Hex.Image (I.Identity) & ")");
         end if;
      end Need;
   begin
      for I of Value.Parameters loop
         Need (I);
      end loop;
      for Part of Value.Components loop
         for I of Part.Outputs loop
            Need (I);
         end loop;
      end loop;
      return To_String (Result);
   end Missing;

   procedure Point (Store : Build_Store.Store; Profile : String; Number : Generation_Number) is
      W : Build_Encoding.Writer;
   begin
      W.Add_Array (Current_Fields);
      W.Add_Text (Current_Tag);
      W.Add_Unsigned (Current_Version);
      W.Add_Unsigned (Interfaces.Unsigned_64 (Number));
      Store.Publish_Replace (Current_Pointer (Profile), W.Bytes);
   end Point;

   function Now return Seconds is
     (Seconds (Ada.Calendar."-" (Ada.Calendar.Clock, Ada.Calendar.Formatting.Time_Of (Epoch_Year, 1, 1))));

   procedure Add
     (Store : Build_Store.Store; Profile : String; Value : Content;
      Number : out Generation_Number; Added : out Boolean)
   is
      Held : Build_Store.Named_Lock;
   begin
      Store.Lock (Lock_Prefix & Profile, Held, "ccl-build: waiting for another change to profile " & Profile);
      declare
         Absent : constant String := Missing (Store, Value);
         Existing : constant Number_Vectors.Vector := Numbers (Store, Profile);
         Now_Current : constant Generation_Count := Current (Store, Profile);
         Latest : constant Generation_Count := (if Existing.Is_Empty then 0 else Existing.Last_Element);
         Placed : Boolean;
      begin
         if Absent /= "" then
            raise Generation_Error with "not in the store:" & Absent;
         end if;
         if Now_Current /= 0 and then Identity (Read (Store, Profile, Now_Current).Value) = Identity (Value) then
            Number := Now_Current;
            Added := False;
         else
            if Latest = Last_Generation then
               raise Generation_Error with "profile " & Profile & " has used every generation number";
            end if;
            Number := Next (Latest);
            Store.Publish_New (Generation_Name (Profile, Number), Encode ((Number, Now, Value)), Placed);
            if not Placed then
               raise Generation_Error with "generation " & Image (Number) & " of " & Profile & " appeared meanwhile";
            end if;
            Point (Store, Profile, Number);
            Added := True;
         end if;
      end;
      Build_Store.Unlock (Held);
   exception
      when others =>
         Build_Store.Unlock (Held);
         raise;
   end Add;

   --  Switches under the profile's lock, which the caller holds.
   procedure Switch_Locked (Store : Build_Store.Store; Profile : String; Number : Generation_Number) is
      Absent : constant String := Missing (Store, Read (Store, Profile, Number).Value);
   begin
      if Absent /= "" then
         raise Generation_Error with "generation " & Image (Number) & " of " & Profile &
           " names entries no longer in the store:" & Absent;
      end if;
      Point (Store, Profile, Number);
   end Switch_Locked;

   procedure Switch (Store : Build_Store.Store; Profile : String; Number : Generation_Number) is
      Held : Build_Store.Named_Lock;
   begin
      Store.Lock (Lock_Prefix & Profile, Held, "ccl-build: waiting for another change to profile " & Profile);
      Switch_Locked (Store, Profile, Number);
      Build_Store.Unlock (Held);
   exception
      when others =>
         Build_Store.Unlock (Held);
         raise;
   end Switch;

   procedure Rollback (Store : Build_Store.Store; Profile : String; Number : out Generation_Number) is
      Held : Build_Store.Named_Lock;
   begin
      Store.Lock (Lock_Prefix & Profile, Held, "ccl-build: waiting for another change to profile " & Profile);
      declare
         Now_Current : constant Generation_Count := Current (Store, Profile);
         Before : Generation_Count := 0;
      begin
         if Now_Current = 0 then
            raise Generation_Error with "profile " & Profile & " has no current generation";
         end if;
         for N of Numbers (Store, Profile) loop
            if N < Now_Current then
               Before := N;
            end if;
         end loop;
         if Before = 0 then
            raise Generation_Error with "profile " & Profile & " has no generation before " & Image (Now_Current);
         end if;
         Switch_Locked (Store, Profile, Before);
         Number := Before;
      end;
      Build_Store.Unlock (Held);
   exception
      when others =>
         Build_Store.Unlock (Held);
         raise;
   end Rollback;

   procedure Delete_Old
     (Store : Build_Store.Store; Profile : String; Keep : Generation_Number; Deleted : out Natural)
   is
      Held : Build_Store.Named_Lock;
   begin
      Deleted := 0;
      Store.Lock (Lock_Prefix & Profile, Held, "ccl-build: waiting for another change to profile " & Profile);
      declare
         All_Numbers : constant Number_Vectors.Vector := Numbers (Store, Profile);
         Now_Current : constant Generation_Count := Current (Store, Profile);
         Kept : Natural := 0;
      begin
         for Position in reverse All_Numbers.First_Index .. All_Numbers.Last_Index loop
            declare
               N : constant Generation_Number := All_Numbers (Position);
            begin
               if Kept < Keep then
                  Kept := Kept + 1;
               elsif N /= Now_Current then
                  Store.Delete_Record (Generation_Name (Profile, N));
                  Deleted := Deleted + 1;
               end if;
            end;
         end loop;
      end;
      Build_Store.Unlock (Held);
   exception
      when others =>
         Build_Store.Unlock (Held);
         raise;
   end Delete_Old;

   function Encode_Root (Products, Pins : Item_Vectors.Vector) return Byte_Array is
      W : Build_Encoding.Writer;
   begin
      W.Add_Array (Root_Fields);
      W.Add_Text (Root_Tag);
      W.Add_Unsigned (Root_Version);
      Add_Items (W, Products);
      Add_Items (W, Pins);
      return W.Bytes;
   end Encode_Root;

   procedure Decode_Root (Data : Byte_Array; Products, Pins : out Item_Vectors.Vector) is
      R : Build_Encoding.Reader := (Length => Data'Length, Data => Data, Position => 1);
      Fields : constant Natural := Build_Encoding.Read_Array (R);
   begin
      Expect (Build_Encoding.Read_Text (R) = Root_Tag, "not a root record");
      case Build_Encoding.Read_Unsigned (R) is
         when Root_First_Version =>
            Expect (Fields = Root_First_Fields, "not a root record");
            Products := Read_Items (R);
            Pins.Clear;
         when Root_Version =>
            Expect (Fields = Root_Fields, "not a root record");
            Products := Read_Items (R);
            Pins := Read_Items (R);
         when others =>
            raise Build_Encoding.Decode_Error with "an unknown root record version";
      end case;
      Expect (Build_Encoding.At_End (R), "bytes after the root record");
   end Decode_Root;

end Build_Generations;
