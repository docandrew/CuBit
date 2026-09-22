pragma Ada_2022;
package body Memory_Grants.Loans with SPARK_Mode is
   procedure Configure
     (Item : in out State; Parent : Reference; Pages : Page_Count;
      Access_Mode : Permission; Forwarding : Forwarding_Policy;
      Applied : out Boolean)
   is
   begin
      Applied := Item.Stage = Unconfigured;
      if Applied then
         Item.Parent := Parent;
         Item.Pages := Pages;
         Item.Access_Mode := Access_Mode;
         Item.Forwarding := Forwarding;
         Item.Stage := Accepting;
      end if;
   end Configure;

   procedure Reserve
     (Item : in out State; Requested : Terms; Loan : out Loan_Reference;
      Result : out Reservation_Result)
   is
   begin
      Loan := No_Loan;
      Result := Denied;
      if not Admits (Item, Requested) then
         return;
      elsif Item.Last_Sequence = Maximum_Sequence then
         Result := Exhausted;
         return;
      end if;
      for Index in Loan_Index loop
         if Item.Entries (Index).Stage = Absent then
            Item.Last_Sequence := Item.Last_Sequence + 1;
            Item.Entries (Index) := (Mapping, Item.Last_Sequence, 0, Requested);
            Loan := (Item.Parent, Index, Item.Last_Sequence);
            Result := Reserved;
            return;
         end if;
      end loop;
      Result := Full;
   end Reserve;

   procedure Publish
     (Item : in out State; Loan : Loan_Reference; Applied : out Boolean)
   is
   begin
      Applied := Phase_Of (Item, Loan) = Mapping;
      if Applied then
         Item.Entries (Loan.Index).Stage := Available;
      end if;
   end Publish;

   procedure Acquire
     (Item : in out State; Loan : Loan_Reference; Applied : out Boolean)
   is
   begin
      Applied := Phase_Of (Item, Loan) = Available and then
        Readers (Item, Loan) < Acquisition_Count'Last;
      if Applied then
         Item.Entries (Loan.Index).Count := Item.Entries (Loan.Index).Count + 1;
      end if;
   end Acquire;

   procedure Return_Reader
     (Item : in out State; Loan : Loan_Reference; Applied : out Boolean)
   is
   begin
      Applied := Live (Item, Loan) and then Readers (Item, Loan) > 0;
      if Applied then
         Item.Entries (Loan.Index).Count := Item.Entries (Loan.Index).Count - 1;
         if Item.Entries (Loan.Index).Stage = Draining and then
            Item.Entries (Loan.Index).Count = 0
         then
            Item.Entries (Loan.Index).Stage := Unmapping;
         end if;
      end if;
   end Return_Reader;

   procedure Revoke
     (Item : in out State; Loan : Loan_Reference; Applied : out Boolean)
   is
   begin
      Applied := Phase_Of (Item, Loan) in Mapping | Available;
      if Applied then
         Item.Entries (Loan.Index).Stage :=
           (if Item.Entries (Loan.Index).Count = 0 then Unmapping else Draining);
      end if;
   end Revoke;

   procedure Finish_Retirement
     (Item : in out State; Loan : Loan_Reference; Applied : out Boolean)
   is
   begin
      Applied := Phase_Of (Item, Loan) = Unmapping;
      if Applied then
         Item.Entries (Loan.Index) := (others => <>);
      end if;
   end Finish_Retirement;

   procedure Close (Item : in out State) is
   begin
      if Item.Stage /= Accepting then
         return;
      end if;
      for Index in Loan_Index loop
         if Item.Entries (Index).Stage in Mapping | Available then
            Item.Entries (Index).Stage :=
              (if Item.Entries (Index).Count = 0 then Unmapping else Draining);
         end if;
         pragma Loop_Invariant (Item.Stage = Accepting);
         pragma Loop_Invariant (Valid (Item));
         pragma Loop_Invariant
           (for all J in Loan_Index'First .. Index =>
              Item.Entries (J).Stage not in Mapping | Available);
      end loop;
      Item.Stage := Closing;
   end Close;

   procedure Release_Parent (Item : in out State; Applied : out Boolean) is
   begin
      Applied := Item.Stage = Closing and then Empty (Item);
      if Applied then
         Item.Stage := Retired;
      end if;
   end Release_Parent;
end Memory_Grants.Loans;
