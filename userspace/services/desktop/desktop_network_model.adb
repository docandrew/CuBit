------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Desktop_Network_Model with SPARK_Mode is

   function Interface_Of (N : NMR.Report; Kind : CuBit.Net_Link.Link_Kind) return NMR.Record_Count is
   begin
      for I in 1 .. N.Count loop
         if N.Interfaces (I).Kind = Kind then
            return I;
         end if;
      end loop;
      return 0;
   end Interface_Of;

   function Name_Of (E : WS.BSS_Entry) return C.SSID is
      S : C.SSID;
   begin
      S.Length := E.Length;
      for I in 1 .. E.Length loop
         S.Octets (I) := E.SSID (I);
      end loop;
      return S;
   end Name_Of;

   First_Printable : constant := 32;
   Last_Printable  : constant := 126;

   function Text_Of (S : C.SSID) return String is
      R : String (1 .. S.Length) := [others => '?'];
   begin
      for I in R'Range loop
         if S.Octets (I) in First_Printable .. Last_Printable then
            R (I) := Character'Val (S.Octets (I));
         end if;
      end loop;
      return R;
   end Text_Of;

   function Kind_Of (E : WS.BSS_Entry) return Security is
     (if E.RSN then Secured elsif WS.Privacy (E) then Unsupported else Open);

   --  A comes before B: the current network first, then the stronger.
   function Before (A, B : Network) return Boolean is
     ((A.Current and then not B.Current)
      or else (A.Current = B.Current and then A.Signal > B.Signal));

   procedure Build_List (R : WR.Report; List : out Network_List; Count : out Network_Count) is
      Joined : constant Boolean := R.Target_Valid and then R.State in C.Joining .. C.Connected;
      Target : constant C.SSID := Name_Of (R.Target);

      procedure Add (E : WS.BSS_Entry; Remembered : Boolean) is
         Name : constant C.SSID := Name_Of (E);
      begin
         if E.Length = 0 then
            return;
         end if;
         for I in 1 .. Count loop
            if Same (List (I).Name, Name) then
               if E.Signal > List (I).Signal then
                  List (I).Signal := E.Signal;
                  List (I).Kind := Kind_Of (E);
               end if;
               List (I).Remembered := List (I).Remembered or else Remembered;
               return;
            end if;
         end loop;
         if Count < Maximum_Networks then
            Count := Count + 1;
            List (Count) := (Name => Name, Signal => E.Signal, Kind => Kind_Of (E),
                             Remembered => Remembered,
                             Current => Joined and then Same (Name, Target));
         end if;
      end Add;
   begin
      List := [others => (others => <>)];
      Count := 0;
      for I in 1 .. R.Scan.Summary.Count loop
         Add (R.Scan.Entries (I), R.Remembered (I));
      end loop;
      --  The network joined stays listed even when the last scan missed it.
      if Joined then
         Add (R.Target, False);
      end if;
      --  Insertion sort (Count is at most Maximum_Networks).
      for I in 2 .. Count loop
         declare
            Item : constant Network := List (I);
            J : Natural := I - 1;
         begin
            while J >= 1 and then Before (Item, List (J)) loop
               pragma Loop_Invariant (J in 1 .. I - 1 and then I <= Count);
               List (J + 1) := List (J);
               J := J - 1;
            end loop;
            List (J + 1) := Item;
         end;
      end loop;
   end Build_List;

   ---------------------------------------------------------------------------
   --  Joining
   ---------------------------------------------------------------------------
   procedure Choose (J : in out Join_State; N : Network; Next : out Request) is
   begin
      Next := Nothing;
      if Busy (J) then
         return;
      elsif N.Kind = Unsupported then
         J := (Current => Failed, Target => N.Name, Why => Not_Supported, others => <>);
      elsif N.Current then
         null;   --  already joined or being joined
      elsif N.Kind = Open or else N.Remembered then
         J := (Current => Starting, Target => N.Name, Why => None, others => <>);
         Next := Join;
      else
         J := (Current => Entering_Passphrase, Target => N.Name, Why => None, others => <>);
      end if;
   end Choose;

   procedure Passphrase_Entered (J : in out Join_State; Length : Natural; Next : out Request) is
   begin
      Next := Nothing;
      if J.Current = Entering_Passphrase and then Length in C.Passphrase_Length then
         J.Current := Storing;
         J.Why := None;
         J.Stored_Now := True;
         Next := Store_Passphrase;
      end if;
   end Passphrase_Entered;

   procedure Cancel (J : in out Join_State) is
   begin
      if J.Current in Entering_Passphrase | Failed then
         J := (others => <>);
      end if;
   end Cancel;

   function Refusal_Failure (Why_Not : C.Refusal) return Failure is
     (case Why_Not is
         when C.Not_Found     => Out_Of_Range,
         when C.Not_Joinable  => Not_Supported,
         when C.Busy          => Refused,
         when C.No_Radio | C.Malformed | C.Not_Stored | C.No_Passphrase => Unavailable);

   procedure Finished (J : in out Join_State; Done : Request; Delivered, OK : Boolean;
                       Why_Not : C.Refusal; Next : out Request) is
   begin
      Next := Nothing;
      case Done is
         when Store_Passphrase =>
            if J.Current /= Storing then
               return;
            elsif Delivered and then OK then
               J.Current := Starting;
               Next := Join;
            elsif Delivered and then Why_Not = C.Malformed then
               --  wifi.svc refused the characters: ask again.
               J.Current := Entering_Passphrase;
               J.Why := Wrong_Passphrase;
               J.Stored_Now := False;
            else
               J.Current := Failed;
               J.Why := Unavailable;
            end if;
         when Join =>
            if J.Current /= Starting then
               return;
            elsif Delivered and then OK then
               J.Current := Joining;
               J.Seen_Handshake := False;
            elsif Delivered and then Why_Not = C.No_Passphrase then
               J.Current := Entering_Passphrase;
               J.Why := None;
            else
               J.Current := Failed;
               J.Why := (if Delivered then Refusal_Failure (Why_Not) else Unavailable);
            end if;
         when Forget | Nothing =>
            null;
      end case;
   end Finished;

   procedure Observe (J : in out Join_State; R : WR.Report; Address : Unsigned_32;
                      Now : Unsigned_64; Next : out Request) is
      Ours : constant Boolean := R.Target_Valid and then Same (Name_Of (R.Target), J.Target);
   begin
      Next := Nothing;
      if J.Current not in Joining | Waiting_Address | Connected then
         return;
      end if;
      case R.State is
         when C.No_Radio =>
            J.Current := Failed;
            J.Why := Unavailable;
         when C.Joining | C.Handshaking =>
            if Ours then
               J.Current := Joining;
               J.Seen_Handshake := J.Seen_Handshake or else R.State = C.Handshaking;
            else
               J := (others => <>);   --  someone joined another network
            end if;
         when C.Connected =>
            if not Ours then
               J := (others => <>);
            elsif Address /= 0 then
               J.Current := Connected;
               J.Why := None;
               J.Stored_Now := False;
            elsif J.Current /= Waiting_Address then
               J.Current := Waiting_Address;
               J.Since := Now;
            end if;
         when C.Idle =>
            J.Why := Failure_From (R.Failure, J.Seen_Handshake or else J.Current = Joining);
            if J.Why = Wrong_Passphrase and then J.Stored_Now then
               --  Do not remember a passphrase the network refused.
               J.Current := Entering_Passphrase;
               J.Stored_Now := False;
               Next := Forget;
            elsif J.Why = Wrong_Passphrase then
               J.Current := Entering_Passphrase;
            else
               J.Current := Failed;
            end if;
      end case;
   end Observe;

   procedure Tick (J : in out Join_State; Now : Unsigned_64) is
   begin
      if J.Current = Waiting_Address and then J.Why = None
        and then Now >= J.Since and then Now - J.Since >= Address_Wait_Ms
      then
         J.Why := No_Address;
      end if;
   end Tick;

   function Status_Text (J : Join_State) return String is
     (case J.Current is
         when Idle => "",
         when Entering_Passphrase =>
           (if J.Why = None then "Passphrase, then Enter" else Failure_Text (J.Why)),
         when Storing => "Saving passphrase",
         when Starting | Joining => "Joining",
         when Waiting_Address =>
           (if J.Why = No_Address then Failure_Text (No_Address) else "Getting an address"),
         when Connected => "Connected",
         when Failed => Failure_Text (J.Why));
end Desktop_Network_Model;
