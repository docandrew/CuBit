with Ada.Unchecked_Conversion;
package body Memory_Account_Store is
   package Accounts renames Process_Memory_Accounts;
   use type Records.Reference;
   use type Records.Result;
   use type System.Address;
   use type Indexes.Insert_Result;
   function Allocate_Index_Page return System.Address is
     (Allocate (4096, 4096));
   function To_Reference is new Ada.Unchecked_Conversion
     (System.Address, Records.Reference);

   function Identity (Object : Store; Item : Handle) return Unsigned_64 is
     (if Valid (Object, Item) then Item.Token else 0);
   function Resolve (Object : Store; Token : Unsigned_64) return Handle is
      Address : constant System.Address := Indexes.Find (Object.Index, Token);
      Item : Handle;
   begin
      if Address = System.Null_Address then return No_Account; end if;
      Item := (To_Reference (Address), Token, Object'Address);
      if not Valid (Object, Item) then return No_Account; end if;
      return Item;
   end Resolve;

   function Charge_Identity
     (Object : Store; Item : Handle; Kind : Process_Memory_Budget.Charge_Kind)
      return Unsigned_64
   is
      Token : constant Unsigned_64 := Identity (Object, Item);
   begin
      if Token = 0 or else Token > Max_Identity then return 0; end if;
      return Token * 4 + Unsigned_64 (Process_Memory_Budget.Charge_Kind'Pos (Kind));
   end Charge_Identity;

   procedure Refund_Physical
     (Object : in out Store; Charge : Unsigned_64; Pages : Unsigned_64;
      OK : out Boolean)
   is
      Tag : constant Unsigned_64 := Charge mod 4;
      Item : Handle;
   begin
      OK := False;
      if Charge / 4 = 0 or else Tag >
        Unsigned_64 (Process_Memory_Budget.Charge_Kind'Pos
          (Process_Memory_Budget.Charge_Kind'Last)) or else Pages = 0
      then return; end if;
      Item := Resolve (Object, Charge / 4);
      Refund (Object, Item, Process_Memory_Budget.Charge_Kind'Val (Tag), Pages, OK);
   end Refund_Physical;

   function Valid (Object : Store; Item : Handle) return Boolean is
   begin
      return Item.Owner = Object'Address and then Item.Ref /= null and then
        Item.Token /= 0 and then
        Records.Value (Item.Ref).Allocated and then
        Accounts.Identity (Records.Value (Item.Ref).Data) = Item.Token;
   end Valid;

   function Metadata_Bytes (Object : Store) return Unsigned_64 is
     (Records.Metadata_Bytes (Object.Pool) + Indexes.Bytes (Object.Index));
   function Block_Bytes return Unsigned_64 is (Records.Block_Bytes);

   procedure Open
     (Object : in out Store; Byte_Limit : Unsigned_64;
      Item : out Handle; Status : out Open_Result)
   is
      Data : Accounts.Account;
      Ref : Records.Reference;
      Result : Records.Result;
      Indexed : Indexes.Insert_Result;
      OK : Boolean;
   begin
      Item := No_Account;
      if Object.Last_Identity >= Max_Identity then
         Status := Identity_Exhausted;
         return;
      end if;
      if Indexes.Bytes (Object.Index) > Byte_Limit then
         Status := Metadata_Limit; return;
      end if;
      Accounts.Open (Data, Object.Last_Identity + 1, OK);
      if not OK then raise Program_Error; end if;
      if Records.Empty (Object.Free) then
         Records.Reserve (Object.Pool, (Data, True),
           Byte_Limit - Indexes.Bytes (Object.Index), Ref, Result);
      else
         Records.Pop (Object.Free, Ref);
         Records.Set_Value (Ref, (Data, True));
         Result := Records.Ready;
      end if;
      case Result is
         when Records.Ready =>
            if Records.Metadata_Bytes (Object.Pool) > Byte_Limit then
               Indexed := Indexes.Metadata_Limit;
            else
               Indexes.Insert (Object.Index, Object.Last_Identity + 1,
                 Ref.all'Address, Byte_Limit - Records.Metadata_Bytes (Object.Pool),
                 Indexed);
            end if;
            if Indexed /= Indexes.Inserted then
               Records.Set_Value (Ref, (Data, False));
               Records.Push (Object.Free, Ref);
               case Indexed is
                  when Indexes.Metadata_Limit => Status := Metadata_Limit;
                  when Indexes.No_Memory => Status := No_Memory;
                  when others => Status := Invalid_Backing;
               end case;
               return;
            end if;
            Object.Last_Identity := Object.Last_Identity + 1;
            Item := (Ref, Object.Last_Identity, Object'Address);
            Status := Opened;
         when Records.Metadata_Quota => Status := Metadata_Limit;
         when Records.No_Memory => Status := No_Memory;
         when Records.Invalid_Backing => Status := Invalid_Backing;
      end case;
   end Open;

   procedure Inspect
     (Object : Store; Item : Handle; Live : out Boolean;
      Pages, Limit : out Unsigned_64; OK : out Boolean) is
   begin
      Live := False;
      Pages := 0;
      Limit := 0;
      OK := Valid (Object, Item);
      if OK then
         declare
            Data : constant Accounts.Account := Records.Value (Item.Ref).Data;
         begin
            Live := Accounts.Active (Data);
            Pages := Accounts.Used (Data);
            Limit := Accounts.Limit (Data);
         end;
      end if;
   end Inspect;

   procedure Adopt
     (Object : in out Store; Item : Handle; Pages : Unsigned_64;
      OK : out Boolean)
   is
      Data : Accounts.Account;
   begin
      OK := Valid (Object, Item);
      if not OK then return; end if;
      Data := Records.Value (Item.Ref).Data;
      Accounts.Adopt (Data, Item.Token, Pages, OK);
      if OK then Records.Set_Value (Item.Ref, (Data, True)); end if;
   end Adopt;

   procedure Reserve
     (Object : in out Store; Item : Handle;
      Kind : Process_Memory_Budget.Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean)
   is
      Data : Accounts.Account;
   begin
      OK := Valid (Object, Item);
      if not OK then return; end if;
      Data := Records.Value (Item.Ref).Data;
      Accounts.Reserve (Data, Item.Token, Kind, Pages, OK);
      if OK then Records.Set_Value (Item.Ref, (Data, True)); end if;
   end Reserve;

   procedure Store_Or_Retire
     (Object : in out Store; Item : in out Handle; Data : Accounts.Account) is
      Removed : Boolean;
   begin
      if Accounts.Reusable (Data) then
         Indexes.Remove (Object.Index, Item.Token, Item.Ref.all'Address, Removed);
         if not Removed then raise Program_Error; end if;
         -- Keep the underlying node leased so stale handles can inspect its
         -- explicit state without a nonlocal exception in the kernel runtime.
         -- Pop/Set_Value on Open reuse these slots before growing the pool.
         Records.Set_Value (Item.Ref, (Data, False));
         Records.Push (Object.Free, Item.Ref);
         Item := No_Account;
      else
         Records.Set_Value (Item.Ref, (Data, True));
      end if;
   end Store_Or_Retire;

   procedure Close
     (Object : in out Store; Item : in out Handle; OK : out Boolean)
   is
      Data : Accounts.Account;
   begin
      OK := Valid (Object, Item);
      if not OK then return; end if;
      Data := Records.Value (Item.Ref).Data;
      Accounts.Close (Data, Item.Token, OK);
      if OK then Store_Or_Retire (Object, Item, Data); end if;
   end Close;

   procedure Refund
     (Object : in out Store; Item : in out Handle;
      Kind : Process_Memory_Budget.Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean)
   is
      Data : Accounts.Account;
   begin
      OK := Valid (Object, Item);
      if not OK then return; end if;
      Data := Records.Value (Item.Ref).Data;
      Accounts.Refund (Data, Item.Token, Kind, Pages, OK);
      if OK then Store_Or_Retire (Object, Item, Data); end if;
   end Refund;
end Memory_Account_Store;
