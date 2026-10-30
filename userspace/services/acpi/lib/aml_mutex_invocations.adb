package body AML_Mutex_Invocations with SPARK_Mode is
   use type AML_Identity.Identity;
   use type AML_Frame_Handles.Invocation_Domain;
   use type AML_Frame_Handles.Invocation_Serial;
   use type AML_Frame_Handles.Frame_Handle;
   use type AML_Object_Identifiers.Object_Address;

   function Token_Frame (Token : Method_Token) return AML_Frame_Handles.Frame_Handle is
     (Token.Frame);
   function Observe (S : State) return Snapshot is (S.Data);
   function Active (S : State) return Boolean is (S.Data.Running);
   function Current_Level (S : State) return Sync_Level is (S.Data.Level);
   function Held_Count (S : State) return Root_Position is (S.Data.Count);
   function Issued_Tokens (S : State) return Natural is (S.Data.Issued);
   function Owner_Of (S : State) return AML_Identity.Identity is (S.Data.Owner);

   function Find (S : State; Address : AML_Object_Identifiers.Object_Address)
      return Lock_Index is
   begin
      for I in S.Data.Locks'Range loop
         if S.Data.Locks (I).Depth /= 0 and then
           S.Data.Locks (I).Address = Address then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   function Depth_Of (S : State; Address : AML_Object_Identifiers.Object_Address)
      return Acquisition_Depth is
      I : constant Lock_Index := Find (S, Address);
   begin
      return (if I = 0 then 0 else S.Data.Locks (I).Depth);
   end Depth_Of;

   function Held_Address (S : State; Position : Root_Position)
      return AML_Object_Identifiers.Object_Address is
      I : Lock_Index := S.Data.Head;
   begin
      if Position = 0 or else Position > S.Data.Count then
         return AML_Object_Identifiers.No_Address;
      end if;
      for N in 1 .. Position - 1 loop
         if I = 0 then
            return AML_Object_Identifiers.No_Address;
         end if;
         I := S.Data.Locks (I).Next;
      end loop;
      return (if I = 0 then AML_Object_Identifiers.No_Address
              else S.Data.Locks (I).Address);
   end Held_Address;

   function Valid (S : State) return Boolean is
      Seen : array (S.Data.Locks'Range) of Boolean := [others => False];
      Counts : array (S.Data.Methods'Range) of Natural := [others => 0];
      I : Lock_Index := S.Data.Head;
      Previous : Lock_Index := 0;
      Count : Natural := 0;
   begin
      if S.Data.Running then
         if S.Data.Owner = AML_Identity.No_Identity or else
           S.Data.Last_Serial = 0 or else S.Data.Domain /=
             AML_Frame_Handles.Bind_Domain (S.Data.Owner, S.Data.Last_Serial)
         then
            return False;
         end if;
      elsif S.Data.Domain /= AML_Frame_Handles.No_Domain or else
        S.Data.Head /= 0 or else S.Data.Count /= 0 or else
        S.Data.Top /= 0 or else S.Data.Level /= 0 then
         return False;
      end if;
      while I /= 0 loop
         if Seen (I) or else S.Data.Locks (I).Depth = 0 or else
           S.Data.Locks (I).Previous /= Previous or else
           not AML_Object_Identifiers.Present (S.Data.Locks (I).Address)
         then
            return False;
         end if;
         Seen (I) := True;
         Count := Count + 1;
         Previous := I;
         I := S.Data.Locks (I).Next;
      end loop;
      if Count /= S.Data.Count then
         return False;
      end if;
      for L in S.Data.Locks'Range loop
         if Seen (L) /= (S.Data.Locks (L).Depth /= 0) then
            return False;
         end if;
         if not Seen (L) and then S.Data.Locks (L) /= Lock_Record'(others => <>) then
            return False;
         end if;
         if Seen (L) then
         for R in S.Data.Locks'First .. L - 1 loop
            if Seen (L) and then Seen (R) and then
              S.Data.Locks (L).Address = S.Data.Locks (R).Address then
               return False;
            end if;
         end loop;
         end if;
      end loop;
      for E in S.Data.Entries'Range loop
         if E <= S.Data.Top then
            if S.Data.Entries (E).Stamp = 0 or else
              S.Data.Entries (E).Stamp > S.Data.Issued or else
              S.Data.Entries (E).Method = 0 or else
              not AML_Frame_Handles.Belongs_To (S.Data.Entries (E).Frame, S.Data.Domain)
            then
               return False;
            end if;
            Counts (S.Data.Entries (E).Method) := Counts (S.Data.Entries (E).Method) + 1;
            for P in S.Data.Entries'First .. E - 1 loop
               if S.Data.Entries (P).Stamp >= S.Data.Entries (E).Stamp or else
                 S.Data.Entries (P).Frame = S.Data.Entries (E).Frame then
                  return False;
               end if;
            end loop;
         elsif S.Data.Entries (E) /= Entry_Record'(others => <>) then
            return False;
         end if;
      end loop;
      for M in S.Data.Methods'Range loop
         if Counts (M) /= S.Data.Methods (M).Depth then
            return False;
         end if;
         if Counts (M) = 0 then
            if S.Data.Methods (M) /= Method_Record'(others => <>) then
               return False;
            end if;
         elsif S.Data.Methods (M).Key = No_Method then
            return False;
         end if;
         for P in S.Data.Methods'First .. M - 1 loop
            if Counts (M) /= 0 and then Counts (P) /= 0 and then
              S.Data.Methods (M).Key = S.Data.Methods (P).Key then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Valid;

   function Check_Admission
     (S : State; C : Admission; Domain : AML_Frame_Handles.Invocation_Domain)
      return Outcome is
   begin
      if not Valid (S) then
         return Invalid_State;
      elsif not S.Data.Running or else Domain /= S.Data.Domain or else
        not Domain_Approved (C, S.Data.Owner, Domain) then
         return Invalid_Domain;
      end if;
      return Succeeded;
   end Check_Admission;

   function Held_Level (S : State; Position : Root_Position) return Sync_Level is
      I : constant Lock_Index := Find (S, Held_Address (S, Position));
   begin
      return (if I = 0 then 0 else S.Data.Locks (I).Level);
   end Held_Level;

   function Method_Count (S : State) return Method_Position is
      Count : Method_Position := 0;
   begin
      for M of S.Data.Methods loop
         if M.Depth /= 0 then Count := Count + 1; end if;
      end loop;
      return Count;
   end Method_Count;

   function Method_Slot (S : State; Position : Method_Position) return Method_Index is
      Count : Method_Position := 0;
   begin
      if Position = 0 then return 0; end if;
      for I in S.Data.Methods'Range loop
         if S.Data.Methods (I).Depth /= 0 then
            Count := Count + 1;
            if Count = Position then return I; end if;
         end if;
      end loop;
      return 0;
   end Method_Slot;

   function Method_At (S : State; Position : Method_Position) return Method_Key is
      I : constant Method_Index := Method_Slot (S, Position);
   begin
      return (if I = 0 then No_Method else S.Data.Methods (I).Key);
   end Method_At;

   function Method_Level (S : State; Position : Method_Position) return Sync_Level is
      I : constant Method_Index := Method_Slot (S, Position);
   begin
      return (if I = 0 then 0 else S.Data.Methods (I).Level);
   end Method_Level;

   procedure Rotate_Epoch
     (S : in out State; C : Admission; New_Owner : AML_Identity.Identity;
      Result : out Outcome) is
   begin
      Result := Invalid_State;
      if not Valid (S) then return; end if;
      if S.Data.Running or else S.Data.Count /= 0 or else S.Data.Top /= 0 then
         Result := Busy; return;
      end if;
      Result := Invalid_Domain;
      if New_Owner = AML_Identity.No_Identity or else New_Owner = S.Data.Owner
        or else not Epoch_Approved (C, S.Data.Owner, New_Owner) then return; end if;
      S.Data.Owner := New_Owner;
      S.Data.Last_Serial := 0;
      -- Preserve Issued: epoch rotation cannot bypass exhausted token budget.
      Result := Succeeded;
   end Rotate_Epoch;

   procedure Begin_Invocation
     (S : in out State; C : Admission; Owner : AML_Identity.Identity;
      Serial : AML_Frame_Handles.Invocation_Serial;
      Domain : AML_Frame_Handles.Invocation_Domain; Result : out Outcome) is
   begin
      Result := Invalid_State;
      if not Valid (S) then
         return;
      end if;
      Result := Invalid_Domain;
      if Owner = AML_Identity.No_Identity or else Serial = 0 or else
        (S.Data.Owner /= AML_Identity.No_Identity and then Owner /= S.Data.Owner) or else
        Domain /= AML_Frame_Handles.Bind_Domain (Owner, Serial) or else
        not Domain_Approved (C, Owner, Domain) then
         return;
      end if;
      if S.Data.Running then
         Result := Busy;
         return;
      elsif Serial <= S.Data.Last_Serial then
         return;
      end if;
      S.Data.Owner := Owner;
      S.Data.Last_Serial := Serial;
      S.Data.Domain := Domain;
      S.Data.Running := True;
      S.Data.Level := 0;
      Result := Succeeded;
   end Begin_Invocation;

   procedure Acquire_Uncontended
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain;
      Address : AML_Object_Identifiers.Object_Address; Level : Sync_Level;
      Global_Lock : Boolean; Result : out Outcome) is
      I : Lock_Index;
   begin
      Result := Check_Admission (S, C, Domain);
      if Result /= Succeeded then return; end if;
      Result := Invalid_Object;
      if not AML_Object_Identifiers.Present (Address) or else
        not Mutex_Approved (C, S.Data.Owner, Address, Level) then return; end if;
      if Global_Lock then Result := Unsupported_Global; return; end if;
      -- ACPICA checks ordering BEFORE recursive ownership.
      if S.Data.Level > Level then Result := Order_Error; return; end if;
      I := Find (S, Address);
      if I /= 0 then
         if S.Data.Locks (I).Level /= Level then Result := Invalid_Object; return; end if;
         if S.Data.Locks (I).Depth = Max_Acquisition_Depth then
            Result := Depth_Limit; return;
         end if;
         S.Data.Locks (I).Depth := S.Data.Locks (I).Depth + 1;
      else
         if S.Data.Count = Max_Explicit_Locks then Result := Capacity_Limit; return; end if;
         for J in S.Data.Locks'Range loop
            if S.Data.Locks (J).Depth = 0 then I := J; exit; end if;
         end loop;
         S.Data.Locks (I) :=
           (Address => Address, Level => Level, Original => S.Data.Level,
            Depth => 1, Previous => 0, Next => S.Data.Head);
         if S.Data.Head /= 0 then S.Data.Locks (S.Data.Head).Previous := I; end if;
         S.Data.Head := I;
         S.Data.Count := S.Data.Count + 1;
         S.Data.Level := Level;
      end if;
      Result := Succeeded;
   end Acquire_Uncontended;

   procedure Release
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain;
      Address : AML_Object_Identifiers.Object_Address; Result : out Outcome) is
      I, P, N : Lock_Index;
      Restore : Sync_Level;
   begin
      Result := Check_Admission (S, C, Domain);
      if Result /= Succeeded then return; end if;
      I := Find (S, Address);
      if I = 0 then Result := Not_Acquired; return; end if;
      if not Mutex_Approved (C, S.Data.Owner, Address, S.Data.Locks (I).Level) then
         Result := Invalid_Object; return;
      end if;
      if S.Data.Level /= S.Data.Locks (I).Level then Result := Order_Error; return; end if;
      if S.Data.Locks (I).Depth > 1 then
         S.Data.Locks (I).Depth := S.Data.Locks (I).Depth - 1;
      else
         -- ACPICA compatibility: save HEAD original before non-LIFO unlink;
         -- migrate removed original to its preceding (newer) acquired lock.
         Restore := S.Data.Locks (S.Data.Head).Original;
         P := S.Data.Locks (I).Previous;
         N := S.Data.Locks (I).Next;
         if N /= 0 then S.Data.Locks (N).Previous := P; end if;
         if P /= 0 then
            S.Data.Locks (P).Next := N;
            S.Data.Locks (P).Original := S.Data.Locks (I).Original;
         else
            S.Data.Head := N;
         end if;
         S.Data.Locks (I) := (others => <>);
         S.Data.Count := S.Data.Count - 1;
         S.Data.Level := Restore;
      end if;
      Result := Succeeded;
   end Release;

   procedure Enter_Method
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain; Key : Method_Key;
      Level : Sync_Level; Frame : AML_Frame_Handles.Frame_Handle;
      Token : out Method_Token; Result : out Outcome) is
      M, Free : Method_Index := 0;
   begin
      Token := No_Token;
      Result := Check_Admission (S, C, Domain);
      if Result /= Succeeded then return; end if;
      if Key = No_Method or else not Method_Approved (C, S.Data.Owner, Key, Level) then
         Result := Invalid_Method; return;
      end if;
      if not AML_Frame_Handles.Belongs_To (Frame, Domain) or else
        not Frame_Approved (C, Frame) then Result := Invalid_Frame; return; end if;
      for E in 1 .. S.Data.Top loop
         if S.Data.Entries (E).Frame = Frame then Result := Invalid_Frame; return; end if;
      end loop;
      if S.Data.Level > Level then Result := Order_Error; return; end if;
      for I in S.Data.Methods'Range loop
         if S.Data.Methods (I).Depth /= 0 and then S.Data.Methods (I).Key = Key then
            M := I;
         elsif S.Data.Methods (I).Depth = 0 and then Free = 0 then
            Free := I;
         end if;
      end loop;
      if M /= 0 then
         if S.Data.Methods (M).Level /= Level then Result := Invalid_Method; return; end if;
         if S.Data.Methods (M).Depth = Max_Acquisition_Depth then
            Result := Depth_Limit; return;
         end if;
      elsif Free = 0 then Result := Capacity_Limit; return;
      end if;
      if S.Data.Top = Max_Method_Entries then Result := Capacity_Limit; return; end if;
      if S.Data.Issued = Max_Token_Stamp then Result := Token_Limit; return; end if;
      if M = 0 then
         M := Free;
         S.Data.Methods (M) := (Key => Key, Level => Level,
                               Original => S.Data.Level, Depth => 1);
         S.Data.Level := Level;
      else
         S.Data.Methods (M).Depth := S.Data.Methods (M).Depth + 1;
      end if;
      S.Data.Issued := S.Data.Issued + 1;
      S.Data.Top := S.Data.Top + 1;
      S.Data.Entries (S.Data.Top) := (Frame => Frame, Stamp => S.Data.Issued, Method => M);
      Token := (Domain => Domain, Position => S.Data.Top,
                Stamp => S.Data.Issued, Frame => Frame);
      Result := Succeeded;
   end Enter_Method;

   procedure Pop_Method (S : in out State) is
      M : constant Method_Index := S.Data.Entries (S.Data.Top).Method;
   begin
      if S.Data.Methods (M).Depth = 1 then
         S.Data.Level := S.Data.Methods (M).Original;
         S.Data.Methods (M) := (others => <>);
      else
         S.Data.Methods (M).Depth := S.Data.Methods (M).Depth - 1;
      end if;
      S.Data.Entries (S.Data.Top) := (others => <>);
      S.Data.Top := S.Data.Top - 1;
   end Pop_Method;

   procedure Leave_Method
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain;
      Token : Method_Token; Result : out Outcome) is
      M : Method_Index;
   begin
      Result := Check_Admission (S, C, Domain);
      if Result /= Succeeded then return; end if;
      if S.Data.Top = 0 or else Token.Position /= S.Data.Top or else
        Token.Domain /= Domain or else Token.Stamp /= S.Data.Entries (S.Data.Top).Stamp or else
        Token.Frame /= S.Data.Entries (S.Data.Top).Frame then
         Result := Stale_Token; return;
      end if;
      if not Frame_Approved (C, Token.Frame) then Result := Invalid_Frame; return; end if;
      M := S.Data.Entries (S.Data.Top).Method;
      if not Method_Approved (C, S.Data.Owner, S.Data.Methods (M).Key, S.Data.Methods (M).Level) then
         Result := Invalid_Method; return;
      end if;
      Pop_Method (S);
      Result := Succeeded;
   end Leave_Method;

   procedure Clear_Explicit (S : in out State) is
      I : Lock_Index;
   begin
      while S.Data.Head /= 0 loop
         I := S.Data.Head;
         S.Data.Level := S.Data.Locks (I).Original;
         S.Data.Head := S.Data.Locks (I).Next;
         S.Data.Locks (I) := (others => <>);
      end loop;
      S.Data.Count := 0;
      S.Data.Level := 0;
      S.Data.Running := False;
      S.Data.Domain := AML_Frame_Handles.No_Domain;
   end Clear_Explicit;

   procedure End_Invocation
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain; Result : out Outcome) is
   begin
      Result := Check_Admission (S, C, Domain);
      if Result /= Succeeded then return; end if;
      if S.Data.Top /= 0 then Result := Busy; return; end if;
      Clear_Explicit (S);
   end End_Invocation;

   procedure Force_Exit
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain; Result : out Outcome) is
   begin
      Result := Check_Admission (S, C, Domain);
      if Result /= Succeeded then return; end if;
      while S.Data.Top /= 0 loop Pop_Method (S); end loop;
      Clear_Explicit (S);
   end Force_Exit;
end AML_Mutex_Invocations;
