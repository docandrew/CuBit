pragma Ada_2022;
with AML_Identity.Issuer;
package body ACPI_Platform_Region_Cache with SPARK_Mode is
   use type AML_Identity.Identity, Layouts.Region;
   Page_Bytes : constant Word := Layouts.Page_Bytes;
   procedure Bump (Value : in out Word) is
   begin if Value < Word'Last then Value := Value + 1; end if; end Bump;
   function Pending (State : Context) return Boolean is (State.Working or else State.Captured.Present);
   function Admitting (State : Context) return Boolean is
     (State.Stage = Ready and then not State.Working and then not State.Captured.Present);
   function Uncertain (State : Context) return Observation is (State.Captured);
   function Observe (State : Context) return Statistics is
      R : Statistics := (Stage => State.Stage, Working => State.Working,
         Regions => State.Region_Last, Mapping_Slots => State.Map_Last,
         Published_Mappings => State.Published, Reserved_Bytes => State.Cursor - State.First,
         Map_Calls => State.Calls, Uncertain => State.Captured.Present, others => <>);
   begin
      for I in 1 .. State.Region_Last loop
         if State.Regions (I).Info.Stage = Accessible then R.Mapped_Regions := R.Mapped_Regions+1;
         else R.Refused_Regions := R.Refused_Regions+1; end if;
      end loop;
      for I in 1 .. State.Map_Last loop
         if State.Maps (I).Stage = Fenced then R.Fenced_Bytes := R.Fenced_Bytes + State.Maps (I).Pages*Page_Bytes; end if;
      end loop;
      return R;
   end Observe;
   function Matching (State : Context; Item : Handle) return Boolean is
     (Item.Owner /= AML_Identity.No_Identity and then Item.Owner = State.Identity
      and then Item.Slot /= 0 and then Item.Slot <= State.Region_Last);
   function At_Page (State : Context; Page : Word) return Mapping_Count is
      Lo : Word := 1;
      Hi : Word := Word (State.Published);
   begin
      if State.Recent /= 0 and then Page >= State.Maps (State.Recent).First_Page
        and then Page - State.Maps (State.Recent).First_Page < State.Maps (State.Recent).Pages
      then return State.Recent; end if;
      while Lo <= Hi loop
         declare
            Mid : constant Word := Lo + (Hi-Lo)/2;
            ID : constant Mapping_Count := State.Order (Mapping_Index (Mid));
            M : constant Mapping_Entry := State.Maps (ID);
         begin
            if Page < M.First_Page then Hi := Mid-1;
            elsif Page - M.First_Page >= M.Pages then Lo := Mid+1;
            else return ID; end if;
         end;
      end loop;
      return 0;
   end At_Page;
   function Next_Start (State : Context; Page, Limit : Word) return Word is
      Lo : Word := 1;
      Hi : Word := Word (State.Published);
      Next : Word := Limit;
   begin
      while Lo <= Hi loop
         declare Mid : constant Word := Lo+(Hi-Lo)/2;
            First : constant Word := State.Maps (State.Order (Mapping_Index (Mid))).First_Page;
         begin
            if First > Page then Next := Word'Min (Next,First); Hi := Mid-1;
            else Lo := Mid+1; end if;
         end;
      end loop;
      return Next;
   end Next_Start;
   procedure Insert (State : in out Context; ID : Mapping_Index) is
      Position : Mapping_Index := State.Published+1;
   begin
      while Position > 1 and then State.Maps (State.Order (Position-1)).First_Page > State.Maps (ID).First_Page loop
         State.Order (Position) := State.Order (Position-1); Position := Position-1;
      end loop;
      State.Order (Position) := ID; State.Published := State.Published+1;
   end Insert;
   procedure Sample (State : in out Context; Deadline : Word; Result : out Outcome) is
      Now : Word;
      Available : Boolean;
   begin
      Result := Clock_Unavailable; Read_Clock (Now,Available);
      if not Available then return; end if;
      if State.Seen_Clock and then Now < State.Last_Clock then Result := Clock_Reversed; return; end if;
      State.Last_Clock := Now; State.Seen_Clock := True;
      Result := (if Now >= Deadline then Deadline_Expired else Mapped);
   end Sample;
   procedure Initialize (State : in out Context; Held : Authority;
      Arena_First, Arena_Limit : Word; Accepted : out Boolean) is
      Issued : Boolean;
   begin
      Accepted := False;
      if State.Stage /= Fresh or else State.Working then return; end if;
      State.Stage := Initializing; State.Working := True;
      if Held = No_Authority or else Arena_First = 0 or else Arena_First >= Arena_Limit
        or else Arena_Limit > Layouts.Virtual_Limit
        or else Arena_First mod Page_Bytes /= 0 or else Arena_Limit mod Page_Bytes /= 0
        or else not Arena_Is_Exclusive (Arena_First,Arena_Limit) or else not Inspect_Pool (Held)
      then State.Stage := Failed; State.Working := False; return; end if;
      AML_Identity.Issuer.Issue (State.Identity,Issued);
      if not Issued then State.Stage := Failed; State.Working := False; return; end if;
      State.Held := Held; State.First := Arena_First; State.Cursor := Arena_First; State.Limit := Arena_Limit;
      State.Stage := Ready; State.Working := False; Accepted := True;
   end Initialize;
   function Split_Depth return Positive is
      Pages : Positive := ABI.Maximum_Pages;
      Depth : Natural := 0;
   begin
      while Pages > 1 loop Pages := Pages/2+Pages mod 2; Depth := Depth+1; end loop;
      return Positive'Max (1,Depth);
   end Split_Depth;
   Maximum_Depth : constant Positive := Split_Depth;
   type Split_Stack is array (Positive range 1 .. Maximum_Depth) of Word;
   procedure Map_Gap (State : in out Context; Slot : Mapping_Index; Deadline : Word;
      Region : Region_Index; Result : out Outcome; Raw : out Word; Why : out ABI.Refusal_Record) is
      Offset : Word := 0;
      Pages : Word;
      Starts, Counts : Split_Stack := [others => 0];
      Depth : Natural range 0 .. Maximum_Depth := 0;
      Attempts : Word := 0;
      Gap : Mapping_Entry renames State.Maps (Slot);
      Info : Profile renames State.Regions (Region).Info;
   begin
      Result := Mapped; Raw := 0; Why := [others => 0];
      while Offset < Gap.Pages loop
         Pages := Word'Min (Word (ABI.Maximum_Pages),Gap.Pages-Offset);
         loop
            Sample (State,Deadline,Result); if Result /= Mapped then return; end if;
            if Attempts >= 2*Gap.Pages then Result := Internal_Limit; return; end if;
            Attempts := Attempts+1; Bump (State.Calls); Bump (Info.Map_Calls);
            Map_Chunk (State.Held,(Gap.First_Page+Offset)*Page_Bytes,Gap.Virtual+Offset*Page_Bytes,
               Pages,False,Raw,Why);
            if Raw = 0 and then (for all Value of Why => Value = 0) then
               Gap.Confirmed := Gap.Confirmed+Pages;
               Info.Confirmed_New_Pages := Info.Confirmed_New_Pages+Pages;
               Sample (State,Deadline,Result); if Result /= Mapped then return; end if;
               if Depth = 0 then Offset := Offset+Pages; exit; end if;
               Offset := Starts (Depth); Pages := Counts (Depth); Depth := Depth-1;
            elsif Raw = ABI.Refusal_Word (ABI.Outside_Pool)
              and then Why (ABI.Code) = Raw and then Pages > 1
            then
               -- OUTSIDE_POOL has no mapping/record effect in this call.
               -- Split in order, never replay a successful or unknown chunk.
               if Depth = Maximum_Depth then Result := Internal_Limit; return; end if;
               Depth := Depth+1; Starts (Depth) := Offset+Pages/2; Counts (Depth) := Pages-Pages/2;
               Pages := Pages/2;
            else
               Result := (if Raw in ABI.Refusal_Word (ABI.Invalid_Request) .. ABI.Refusal_Word (ABI.Mapping_Capacity)
                  and then Why (ABI.Code) = Raw then Kernel_Refused else Mapping_Unknown);
               return;
            end if;
         end loop;
      end loop;
      Gap.Stage := Complete;
   end Map_Gap;
   procedure Map_Pool_Region_For_AML (State : in out Context; Origin : Source;
      Geometry : Layouts.Region; Deadline_Microseconds : Word; Result : out Map_Result;
      History : History_State := History_Unavailable) is
      First, Limit, Position, New_Pages, Gaps, Finish, Count : Word;
      ID : Mapping_Count;
      Before : Mapping_Count;
      Slot : Region_Index;
      Status : Outcome;
      Raw : Word := 0;
      Why : ABI.Refusal_Record := [others => 0];
   begin
      Result := (others => <>);
      if State.Working then Result.Status := Reentered; return; end if;
      if State.Stage /= Ready then return; end if;
      Result.Status := Invalid_Region; if not Layouts.Valid (Geometry) then return; end if;
      Result.Status := Invalid_Deadline;
      if Deadline_Microseconds = No_Finite_Deadline then return; end if;
      State.Working := True;
      Sample (State,Deadline_Microseconds,Status);
      if Status /= Mapped then Result.Status := Status; State.Working := False; return; end if;
      for I in 1 .. State.Region_Last loop
         if State.Regions (I).Info.Geometry = Geometry then
            Bump (State.Regions (I).Info.Repeats);
            Result := (Status => (if State.Regions (I).Info.Stage = Accessible then Reused else State.Regions (I).Info.Result),
               Region => (State.Identity,I),Raw_Status => State.Regions (I).Info.Raw_Status,Why => State.Regions (I).Info.Why);
            State.Working := False; return;
         end if;
      end loop;
      Result.Status := Region_Capacity;
      if State.Region_Last = Region_Count'Last then State.Working := False; return; end if;
      First := Layouts.Envelope (Geometry).First_Page;
      Limit := First+Layouts.Envelope (Geometry).Pages;
      -- Preflight all missing page runs and VA/slot capacity before mapping.
      Position := First; New_Pages := 0; Gaps := 0;
      while Position < Limit loop
         ID := At_Page (State,Position);
         if ID /= 0 then Position := Word'Min (Limit,State.Maps (ID).First_Page+State.Maps (ID).Pages);
         else Finish := Next_Start (State,Position,Limit); New_Pages := New_Pages+Finish-Position; Gaps := Gaps+1; Position := Finish;
         end if;
      end loop;
      Result.Status := Mapping_Capacity;
      if Gaps > Word (Maximum_Mappings-State.Map_Last) then State.Working := False; return; end if;
      Result.Status := Arena_Exhausted;
      if New_Pages > (State.Limit-State.Cursor)/Page_Bytes then State.Working := False; return; end if;
      State.Region_Last := State.Region_Last+1; Slot := State.Region_Last;
      State.Regions (Slot).Info := (Available => True,Origin => Origin,Geometry => Geometry,
         History => History,others => <>);
      Before := State.Map_Last; Position := First;
      while Position < Limit loop
         ID := At_Page (State,Position);
         if ID /= 0 then Position := Word'Min (Limit,State.Maps (ID).First_Page+State.Maps (ID).Pages);
         else
            Finish := Next_Start (State,Position,Limit); Count := Finish-Position;
            State.Map_Last := State.Map_Last+1; ID := State.Map_Last;
            -- Reserve the WHOLE run before the first possibly partial syscall.
            State.Maps (ID) := (Reserved,Position,Count,State.Cursor,0);
            State.Cursor := State.Cursor+Count*Page_Bytes;
            Map_Gap (State,ID,Deadline_Microseconds,Slot,Status,Raw,Why);
            if Status /= Mapped then
               for New_ID in Before+1 .. State.Map_Last loop State.Maps (New_ID).Stage := Fenced; end loop;
               State.Regions (Slot).Info.Stage := Refused; State.Regions (Slot).Info.Result := Status;
               State.Regions (Slot).Info.Raw_Status := Raw; State.Regions (Slot).Info.Why := Why;
               State.Regions (Slot).Info.Needs_Note := True;
               Result := (Status,(State.Identity,Slot),Raw,Why); State.Working := False; return;
            end if;
            Position := Finish;
         end if;
      end loop;
      -- Publish no new page association until the entire region succeeded.
      if State.Map_Last > Before then
         for New_ID in Before+1 .. State.Map_Last loop Insert (State,New_ID); end loop;
      end if;
      State.Regions (Slot).Info.Stage := Accessible; State.Regions (Slot).Info.Result := Mapped;
      State.Regions (Slot).Info.Needs_Note := State.Regions (Slot).Info.Map_Calls > 1;
      Result := (Status => Mapped,Region => (State.Identity,Slot),others => <>);
      State.Working := False;
   end Map_Pool_Region_For_AML;
   procedure Describe (State : Context; Item : Handle; Geometry : out Layouts.Region; Accepted : out Boolean) is
   begin
      Geometry := (others => <>);
      Accepted := State.Stage = Ready and then not State.Working and then Matching (State,Item)
         and then State.Regions (Item.Slot).Info.Stage = Accessible;
      if Accepted then Geometry := State.Regions (Item.Slot).Info.Geometry; end if;
   end Describe;
   procedure Access_Region (State : in out Context; Item : Handle; Offset : Word;
      Width : T.Access_Width; For_Write : Boolean; Input : Word; Result : out Data_Result) is
      Accepted, Complete : Boolean;
      Geometry : Layouts.Region;
      Address, Value : Word;
      ID : Mapping_Count;
      View : Layouts.Access_Result;
   begin
      Result := (others => <>); Describe (State,Item,Geometry,Accepted);
      if not Accepted or else Offset >= Geometry.Length or else Word (T.Octets (Width)) > Geometry.Length-Offset
        or else not T.Aligned (Geometry.Address+Offset,Width)
        or else (if For_Write then Input > Layouts.Mask (Width) else Input /= 0)
        or else State.Operation = Word'Last then return; end if;
      Address := Geometry.Address+Offset; ID := At_Page (State,Address/Page_Bytes);
      if ID = 0 then return; end if;
      View := Layouts.Resolve ((State.Maps (ID).First_Page*Page_Bytes,State.Maps (ID).Pages*Page_Bytes),
         State.Maps (ID).Virtual,Address-State.Maps (ID).First_Page*Page_Bytes,Width,For_Write,Input);
      if not View.Valid then return; end if;
      State.Working := True;
      if not May_Access (For_Write) then State.Working := False; return; end if;
      State.Operation := State.Operation+1; State.Recent := ID;
      declare Info : Profile renames State.Regions (Item.Slot).Info; begin
         if Info.First_Operation = 0 then Info.First_Operation := State.Operation; Info.Needs_Note := True; end if;
         Info.Last_Operation := State.Operation;
         if For_Write then Bump (Info.Write_Attempts); else Bump (Info.Read_Attempts); end if;
      end;
      Transfer (View.Virtual,Width,For_Write,Input,Value,Complete);
      if not Complete then
         State.Captured := (True,Item,Offset,Width,For_Write,Input,State.Operation);
         State.Stage := Blocked; State.Regions (Item.Slot).Info.Stage := Data_Uncertain;
         State.Regions (Item.Slot).Info.Needs_Note := True;
         Result := (Indeterminate,0,State.Operation);
      elsif not For_Write and then Value > Layouts.Mask (Width) then
         State.Stage := Blocked; State.Regions (Item.Slot).Info.Stage := Data_Fault;
         State.Regions (Item.Slot).Info.Needs_Note := True;
         Result := (Failed_Completed,0,0);
      else Result := (Completed,(if For_Write then 0 else Value),0);
      end if;
      State.Working := False;
   end Access_Region;
   procedure Close_Admission (State : in out Context) is
   begin if not State.Working then State.Stage := Closed; end if; end Close_Admission;
   function Read_Profile (State : Context; Ordinal : Positive) return Profile is
   begin
      if Ordinal > State.Region_Last then return (others => <>); end if;
      return State.Regions (Ordinal).Info;
   end Read_Profile;
   procedure Note_Drained (State : in out Context; Ordinal : Positive; Accepted : out Boolean) is
   begin
      Accepted := not Pending (State) and then Ordinal <= State.Region_Last;
      if Accepted then State.Regions (Ordinal).Info.Needs_Note := False; end if;
   end Note_Drained;
end ACPI_Platform_Region_Cache;
