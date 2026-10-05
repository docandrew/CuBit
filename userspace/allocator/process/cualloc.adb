pragma Ada_2022;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Address_To_Access_Conversions;
with Heap_Classes;
with Heap_Extents;
with Heap_Slab_Instance;

package body CuAlloc is
   package S renames Heap_Slab_Instance;
   package E renames Heap_Extents;

   SMALL_LIMIT : constant := 4_096;
   --  Larger, or aligned beyond a page: a reservation of its own.
   HUGE_THRESHOLD : constant := 1_048_576;
   ARENA_PAYLOAD : constant := 16 * 1_048_576;
   --  Backing is added in steps of at least this much (fewer provider calls).
   COMMIT_STEP : constant := 1_048_576;
   MAXIMUM_ARENAS : constant := 65_536;

   type Arena_Kind is (Unused, Small, Medium, Huge);
   type Arena_Id is range 0 .. MAXIMUM_ARENAS;
   subtype Valid_Id is Arena_Id range 1 .. MAXIMUM_ARENAS;
   No_Arena : constant Arena_Id := 0;

   type Arena_Record is record
      Base : Unsigned_64 := 0;         --  the reservation
      Total : Unsigned_64 := 0;        --  its bytes
      Payload : Unsigned_64 := 0;      --  first payload byte
      Committed : Unsigned_64 := 0;    --  bytes backed, from Base
      Bytes : Unsigned_64 := 0;        --  huge: the block's usable bytes
      Next_Free : Arena_Id := No_Arena; --  unused: the next unused record
      Kind : Arena_Kind := Unused;
   end record;

   package Records is new System.Address_To_Access_Conversions (Arena_Record);
   package Slab_States is new System.Address_To_Access_Conversions (S.State);
   package Extent_States is new System.Address_To_Access_Conversions (E.State);
   type Index_Entry is new Arena_Id;
   package Index_Entries is new System.Address_To_Access_Conversions (Index_Entry);

   function Round_Up (Value, Unit : Unsigned_64) return Unsigned_64 is
     ((Value + Unit - 1) / Unit * Unit);

   --  Functions, not constants: object sizes are not static, and the libc
   --  links this with No_Elaboration_Code.
   function RECORD_BYTES return Unsigned_64 is
     (Unsigned_64 (Arena_Record'Max_Size_In_Storage_Elements));
   function SLAB_META return Unsigned_64 is
     (Round_Up (Unsigned_64 (S.State'Max_Size_In_Storage_Elements), Page_Bytes));
   function EXTENT_META return Unsigned_64 is
     (Round_Up (Unsigned_64 (E.State'Max_Size_In_Storage_Elements), Page_Bytes));

   --  The directory: arena records by id, and arena ids sorted by base.
   type Directory is record
      Base : Unsigned_64 := 0;
      Total : Unsigned_64 := 0;
      Committed : Unsigned_64 := 0;
   end record;
   Arenas_Area, Sorted_Area : Directory;
   Last_Id : Arena_Id := No_Arena;       --  records in use: 1 .. Last_Id
   Sorted_Count : Arena_Id := No_Arena;
   First_Free : Arena_Id := No_Arena;
   Live_Arenas : Natural := 0;
   Reserved_Total, Committed_Total : Unsigned_64 := 0;
   Ready : Boolean := False;
   --  Where small requests of each class, and medium ones, last succeeded.
   Current_Small : array (Heap_Classes.Size_Class) of Arena_Id := [others => No_Arena];
   Current_Medium : Arena_Id := No_Arena;

   function To_Address (Value : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (Integer_Address (Value)));

   function Arena (Id : Valid_Id) return Records.Object_Pointer is
     (Records.To_Pointer (To_Address (Arenas_Area.Base + Unsigned_64 (Id - 1) * RECORD_BYTES)));
   function Sorted (Position : Valid_Id) return Index_Entries.Object_Pointer is
     (Index_Entries.To_Pointer (To_Address (Sorted_Area.Base + Unsigned_64 (Position - 1) * 4)));

   --  Back the reservation at Base up to Upto bytes (Committed: what is
   --  backed now, updated).
   function Ensure
     (Base, Total : Unsigned_64; Committed : in out Unsigned_64; Upto : Unsigned_64) return Boolean
   is
      Step : Unsigned_64;
   begin
      while Committed < Upto loop
         Step := Unsigned_64'Min
           (Unsigned_64'Min (Maximum_Commit, Round_Up (Upto - Committed, COMMIT_STEP)),
            Total - Committed);
         if Step = 0 or else not Commit (Base, Committed, Step) then
            return False;
         end if;
         Committed := Committed + Step;
         Committed_Total := Committed_Total + Step;
      end loop;
      return True;
   end Ensure;

   function Ensure_Arena (Id : Valid_Id; Upto : Unsigned_64) return Boolean is
      A : constant Records.Object_Pointer := Arena (Id);
   begin
      return Ensure (A.Base, A.Total, A.Committed, Upto);
   end Ensure_Arena;

   function Start return Boolean is
      Records_Total : constant Unsigned_64 := Round_Up (MAXIMUM_ARENAS * RECORD_BYTES, Page_Bytes);
      Sorted_Total : constant Unsigned_64 := Round_Up (MAXIMUM_ARENAS * 4, Page_Bytes);
   begin
      if Ready then return True; end if;
      Arenas_Area := (Base => Reserve (Records_Total), Total => Records_Total, Committed => 0);
      if Arenas_Area.Base = 0 then return False; end if;
      Sorted_Area := (Base => Reserve (Sorted_Total), Total => Sorted_Total, Committed => 0);
      if Sorted_Area.Base = 0 then
         if Release (Arenas_Area.Base, Arenas_Area.Total) then null; end if;
         return False;
      end if;
      Reserved_Total := Records_Total + Sorted_Total;
      Ready := True;
      return True;
   end Start;

   --  A record for a new arena (reused or appended), its directory backing
   --  in place; No_Arena when there is none.
   function New_Record return Arena_Id is
      Id : Arena_Id;
   begin
      if First_Free /= No_Arena then
         Id := First_Free;
         First_Free := Arena (Id).Next_Free;
         return Id;
      elsif Last_Id = MAXIMUM_ARENAS then
         return No_Arena;
      end if;
      if not Ensure (Arenas_Area.Base, Arenas_Area.Total, Arenas_Area.Committed,
                     Unsigned_64 (Last_Id + 1) * RECORD_BYTES)
        or else not Ensure (Sorted_Area.Base, Sorted_Area.Total, Sorted_Area.Committed,
                            Unsigned_64 (Last_Id + 1) * 4)
      then
         return No_Arena;
      end if;
      Last_Id := Last_Id + 1;
      return Last_Id;
   end New_Record;

   --  The first sorted position whose arena's base is above Address.
   function Upper_Bound (Address : Unsigned_64) return Arena_Id is
      Low : Arena_Id := 1;
      High : Arena_Id := Sorted_Count + 1;
      Middle : Arena_Id;
   begin
      while Low < High loop
         Middle := Low + (High - Low) / 2;
         if Arena (Valid_Id (Sorted (Middle).all)).Base <= Address then
            Low := Middle + 1;
         else
            High := Middle;
         end if;
      end loop;
      return Low;
   end Upper_Bound;

   procedure Insert_Sorted (Id : Valid_Id) is
      Position : constant Arena_Id := Upper_Bound (Arena (Id).Base);
   begin
      for P in reverse Position .. Sorted_Count loop
         Sorted (P + 1).all := Sorted (P).all;
      end loop;
      Sorted (Position).all := Index_Entry (Id);
      Sorted_Count := Sorted_Count + 1;
   end Insert_Sorted;

   procedure Remove_Sorted (Id : Valid_Id) is
      Position : constant Arena_Id := Upper_Bound (Arena (Id).Base) - 1;
   begin
      for P in Position .. Sorted_Count - 1 loop
         Sorted (P).all := Sorted (P + 1).all;
      end loop;
      Sorted_Count := Sorted_Count - 1;
   end Remove_Sorted;

   --  The arena whose reservation holds Address, or No_Arena.
   function Arena_Of (Address : Unsigned_64) return Arena_Id is
      Position : Arena_Id;
   begin
      if not Ready or else Sorted_Count = 0 then return No_Arena; end if;
      Position := Upper_Bound (Address);
      if Position = 1 then return No_Arena; end if;
      declare
         Id : constant Valid_Id := Valid_Id (Sorted (Position - 1).all);
         A : constant Records.Object_Pointer := Arena (Id);
      begin
         return (if Address - A.Base < A.Total then Id else No_Arena);
      end;
   end Arena_Of;

   --  A new arena of Kind: Meta bytes of metadata, then Payload bytes
   --  (aligned to Alignment), the metadata backed.
   function Create (Kind : Arena_Kind; Meta, Payload, Alignment : Unsigned_64) return Arena_Id is
      Id : constant Arena_Id := New_Record;
      Total : constant Unsigned_64 := Round_Up (Meta + Payload + Alignment - Page_Bytes, Page_Bytes);
      Base : Unsigned_64;
   begin
      if Id = No_Arena then return No_Arena; end if;
      Base := Reserve (Total);
      if Base = 0 then
         Arena (Id).all := (Kind => Unused, Next_Free => First_Free, others => <>);
         First_Free := Id;
         return No_Arena;
      end if;
      Arena (Id).all :=
        (Base => Base, Total => Total, Payload => Round_Up (Base + Meta, Alignment),
         Committed => 0, Bytes => 0, Next_Free => No_Arena, Kind => Kind);
      Reserved_Total := Reserved_Total + Total;
      if not Ensure_Arena (Id, Meta) then
         if Release (Base, Total) then null; end if;
         Reserved_Total := Reserved_Total - Total;
         Committed_Total := Committed_Total - Arena (Id).Committed;
         Arena (Id).all := (Kind => Unused, Next_Free => First_Free, others => <>);
         First_Free := Id;
         return No_Arena;
      end if;
      Insert_Sorted (Id);
      Live_Arenas := Live_Arenas + 1;
      return Id;
   end Create;

   procedure Retire (Id : Valid_Id) is
      A : constant Records.Object_Pointer := Arena (Id);
   begin
      Remove_Sorted (Id);
      Reserved_Total := Reserved_Total - A.Total;
      Committed_Total := Committed_Total - A.Committed;
      if Release (A.Base, A.Total) then null; end if;
      A.all := (Kind => Unused, Next_Free => First_Free, others => <>);
      First_Free := Id;
      Live_Arenas := Live_Arenas - 1;
   end Retire;

   ------------------------------------------------------------------------
   --  Small blocks
   ------------------------------------------------------------------------
   function Slabs (Id : Valid_Id) return Slab_States.Object_Pointer is
     (Slab_States.To_Pointer (To_Address (Arena (Id).Base)));

   function Try_Small (Id : Valid_Id; Size : Heap_Classes.Request_Size) return Unsigned_64 is
      Value : S.Allocation;
      Status : S.Release_Status;
      Item : Unsigned_64;
   begin
      S.Allocate (Slabs (Id).all, Size, Value);
      if not Value.Value.Success then return No_Address; end if;
      Item := Arena (Id).Payload + Unsigned_64 (Value.Value.Position);
      if not Ensure_Arena
        (Id, Item - Arena (Id).Base + Unsigned_64 (Heap_Classes.Stride (Heap_Classes.Class_For (Size))))
      then
         S.Release (Slabs (Id).all, Value.Value.Position, Status);
         return No_Address;
      end if;
      return Item;
   end Try_Small;

   function Allocate_Small (Size : Heap_Classes.Request_Size) return Unsigned_64 is
      Class : constant Heap_Classes.Size_Class := Heap_Classes.Class_For (Size);
      Current : constant Arena_Id := Current_Small (Class);
      Item : Unsigned_64;
      Fresh : Arena_Id;
   begin
      if Current /= No_Arena then
         Item := Try_Small (Current, Size);
         if Item /= No_Address then return Item; end if;
      end if;
      for Id in 1 .. Last_Id loop
         if Id /= Current and then Arena (Id).Kind = Small then
            Item := Try_Small (Id, Size);
            if Item /= No_Address then
               Current_Small (Class) := Id;
               return Item;
            end if;
         end if;
      end loop;
      Fresh := Create (Small, SLAB_META, ARENA_PAYLOAD, Page_Bytes);
      if Fresh = No_Arena then return No_Address; end if;
      S.Initialize (Slabs (Fresh).all);
      Current_Small (Class) := Fresh;
      return Try_Small (Fresh, Size);
   end Allocate_Small;

   ------------------------------------------------------------------------
   --  Medium blocks
   ------------------------------------------------------------------------
   function Extents (Id : Valid_Id) return Extent_States.Object_Pointer is
     (Extent_States.To_Pointer (To_Address (Arena (Id).Base)));

   function Try_Medium (Id : Valid_Id; Pages : E.Run_Length) return Unsigned_64 is
      First : E.Page_Reference;
      Released : Boolean;
      Item : Unsigned_64;
   begin
      E.Allocate (Extents (Id).all, Pages, 1, First);
      if First = E.No_Page then return No_Address; end if;
      Item := Arena (Id).Payload + Unsigned_64 (First - 1) * Page_Bytes;
      if not Ensure_Arena (Id, Item - Arena (Id).Base + Unsigned_64 (Pages) * Page_Bytes) then
         E.Release (Extents (Id).all, First, Released);
         return No_Address;
      end if;
      return Item;
   end Try_Medium;

   function Allocate_Medium (Bytes : Unsigned_64) return Unsigned_64 is
      Pages : constant E.Run_Length := E.Run_Length ((Bytes + Page_Bytes - 1) / Page_Bytes);
      Item : Unsigned_64;
      Fresh : Arena_Id;
   begin
      if Current_Medium /= No_Arena then
         Item := Try_Medium (Current_Medium, Pages);
         if Item /= No_Address then return Item; end if;
      end if;
      for Id in 1 .. Last_Id loop
         if Id /= Current_Medium and then Arena (Id).Kind = Medium then
            Item := Try_Medium (Id, Pages);
            if Item /= No_Address then
               Current_Medium := Id;
               return Item;
            end if;
         end if;
      end loop;
      Fresh := Create (Medium, EXTENT_META, ARENA_PAYLOAD, Page_Bytes);
      if Fresh = No_Arena then return No_Address; end if;
      E.Initialize (Extents (Fresh).all);
      Current_Medium := Fresh;
      return Try_Medium (Fresh, Pages);
   end Allocate_Medium;

   ------------------------------------------------------------------------
   --  Huge blocks: a reservation each, backed whole.
   ------------------------------------------------------------------------
   function Allocate_Huge (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Usable : constant Unsigned_64 := Round_Up (Bytes, Page_Bytes);
      Id : constant Arena_Id := Create (Huge, 0, Usable, Unsigned_64'Max (Alignment, Page_Bytes));
   begin
      if Id = No_Arena then return No_Address; end if;
      Arena (Id).Bytes := Usable;
      if not Ensure_Arena (Id, Arena (Id).Payload - Arena (Id).Base + Usable) then
         Retire (Id);
         return No_Address;
      end if;
      return Arena (Id).Payload;
   end Allocate_Huge;

   function Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Wanted : constant Unsigned_64 := Unsigned_64'Max (Bytes, 1);
      Align : constant Unsigned_64 := Unsigned_64'Max (Alignment, 16);
   begin
      if Align > Maximum_Alignment or else (Align and (Align - 1)) /= 0 or else
        Wanted > Unsigned_64'Last / 2 or else not Start
      then
         return No_Address;
      elsif Wanted <= SMALL_LIMIT and then Align <= SMALL_LIMIT then
         return Allocate_Small (Heap_Classes.Request_Size (Unsigned_64'Max (Wanted, Align)));
      elsif Wanted <= HUGE_THRESHOLD and then Align <= Page_Bytes then
         return Allocate_Medium (Wanted);
      else
         return Allocate_Huge (Wanted, Align);
      end if;
   end Allocate;

   procedure Zero (Item, Bytes : Unsigned_64) is
      Target : Storage_Array (1 .. Storage_Offset (Bytes)) with Import, Address => To_Address (Item);
   begin
      Target := [others => 0];
   end Zero;

   function Allocate_Zeroed (Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      Item : constant Unsigned_64 := Allocate (Bytes, Alignment);
      Id : Arena_Id;
   begin
      if Item = No_Address then return No_Address; end if;
      Id := Arena_Of (Item);
      --  A huge block is fresh backing, which reads as zero already.
      if Id /= No_Arena and then Arena (Id).Kind /= Huge then
         Zero (Item, Unsigned_64'Max (Bytes, 1));
      end if;
      return Item;
   end Allocate_Zeroed;

   procedure Free (Item : Unsigned_64) is
      Id : constant Arena_Id := (if Item = No_Address then No_Arena else Arena_Of (Item));
   begin
      if Id = No_Arena then return; end if;
      declare
         A : constant Records.Object_Pointer := Arena (Id);
      begin
         if Item < A.Payload or else Item - A.Payload >= ARENA_PAYLOAD then
            return;
         end if;
         case A.Kind is
            when Small =>
               declare
                  Status : S.Release_Status;
               begin
                  S.Release (Slabs (Id).all, S.Offset (Item - A.Payload), Status);
               end;
            when Medium =>
               if (Item - A.Payload) mod Page_Bytes = 0 then
                  declare
                     Released : Boolean;
                  begin
                     E.Release (Extents (Id).all, E.Page_Id ((Item - A.Payload) / Page_Bytes + 1), Released);
                  end;
               end if;
            when Huge =>
               if Item = A.Payload then Retire (Id); end if;
            when Unused => null;
         end case;
      end;
   end Free;

   function Usable_Size (Item : Unsigned_64) return Unsigned_64 is
      Id : constant Arena_Id := (if Item = No_Address then No_Arena else Arena_Of (Item));
   begin
      if Id = No_Arena then return 0; end if;
      declare
         A : constant Records.Object_Pointer := Arena (Id);
      begin
         if Item < A.Payload then return 0; end if;
         case A.Kind is
            when Small =>
               if Item - A.Payload < ARENA_PAYLOAD and then
                 S.Live (Slabs (Id).all, S.Offset (Item - A.Payload))
               then
                  return Unsigned_64 (Heap_Classes.Stride
                    (S.Class_Of (Slabs (Id).all, S.Page_Of (S.Offset (Item - A.Payload)))));
               end if;
            when Medium =>
               if Item - A.Payload < ARENA_PAYLOAD and then (Item - A.Payload) mod Page_Bytes = 0 then
                  return Unsigned_64 (E.Length (Extents (Id).all,
                                                E.Page_Id ((Item - A.Payload) / Page_Bytes + 1))) * Page_Bytes;
               end if;
            when Huge =>
               if Item = A.Payload then return A.Bytes; end if;
            when Unused => null;
         end case;
      end;
      return 0;
   end Usable_Size;

   function Reallocate (Item, Bytes : Unsigned_64) return Unsigned_64 is
      Held : Unsigned_64;
      Moved : Unsigned_64;
   begin
      if Item = No_Address then
         return Allocate (Bytes, 16);
      end if;
      Held := Usable_Size (Item);
      if Held = 0 then
         return No_Address;
      elsif Unsigned_64'Max (Bytes, 1) <= Held then
         return Item;
      end if;
      Moved := Allocate (Bytes, 16);
      if Moved /= No_Address then
         declare
            Source : Storage_Array (1 .. Storage_Offset (Held)) with Import, Address => To_Address (Item);
            Target : Storage_Array (1 .. Storage_Offset (Held)) with Import, Address => To_Address (Moved);
         begin
            Target := Source;
         end;
         Free (Item);
      end if;
      return Moved;
   end Reallocate;

   function Arena_Count return Natural is (Live_Arenas);
   function Reserved_Bytes return Unsigned_64 is (Reserved_Total);
   function Committed_Bytes return Unsigned_64 is (Committed_Total);
end CuAlloc;
