------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Wifi_MLME with SPARK_Mode is

   procedure Add (A : in out Actions; Item : Action_Kind)
   with Pre => A.Count < Maximum_Actions,
        Post => A.Count = A.Count'Old + 1 and then A.Items (A.Count) = Item
                and then (for all I in 1 .. A.Count'Old => A.Items (I) = A.Items'Old (I))
   is
   begin
      A.Count := A.Count + 1;
      A.Items (A.Count) := Item;
   end Add;

   function Later (Now, Span : Milliseconds) return Milliseconds is
     (if Span > Milliseconds'Last - Now then Milliseconds'Last else Now + Span);

   --  Back to Idle with Why; closes the port if it was open.
   procedure Fail (S : in out State; Why : Failure; A : in out Actions)
   with Pre  => A.Count <= Maximum_Actions - 3 and then not Opens (A),
        Post => S.Current = Idle and then not S.Port_Open and then S.Reason = Why
                and then not Opens (A) and then A.Count <= A.Count'Old + 3
   is
   begin
      if S.Port_Open then
         Add (A, Close_Port);
      end if;
      if S.Current = Handshaking or else S.Current = Connected then
         Add (A, Stop_Handshake);
      end if;
      Add (A, Report_Failure);
      S := (Current => Idle, Kind => S.Kind, Bounds => S.Bounds, Deadline => 0, Tries => 0,
            Port_Open => False, Reason => Why);
   end Fail;

   procedure Step (S : in out State; E : Event; Do_Now : out Actions) is
   begin
      Do_Now := (others => <>);
      case E.Kind is
         when Join =>
            if S.Current /= Idle or else E.Bounds.Frame_Attempts = 0 then
               return;
            end if;
            S := (Current => Authenticating, Kind => E.Network, Bounds => E.Bounds,
                  Deadline => Later (E.Now, E.Bounds.Frame_Timeout), Tries => 1,
                  Port_Open => False, Reason => None);
            Add (Do_Now, Send_Authentication);

         when Authentication_Answer =>
            if S.Current /= Authenticating then
               return;
            elsif not E.Success then
               Fail (S, Authentication_Refused, Do_Now);
            else
               S.Current := Associating;
               S.Tries := 1;
               S.Deadline := Later (E.Now, S.Bounds.Frame_Timeout);
               Add (Do_Now, Send_Association);
            end if;

         when Association_Answer =>
            if S.Current /= Associating then
               return;
            elsif not E.Success then
               Fail (S, Association_Refused, Do_Now);
            elsif S.Kind = Open then
               S.Current := Connected;
               S.Tries := 0;
               S.Deadline := 0;
               S.Port_Open := True;
               Add (Do_Now, Open_Port);
               Add (Do_Now, Report_Connected);
            else
               S.Current := Handshaking;
               S.Tries := 0;
               S.Deadline := Later (E.Now, S.Bounds.Handshake_Timeout);
               Add (Do_Now, Start_Handshake);
            end if;

         when Handshake_Established =>
            if S.Current = Handshaking then
               S.Current := Connected;
               S.Deadline := 0;
               S.Port_Open := True;
               Add (Do_Now, Open_Port);
               Add (Do_Now, Report_Connected);
            end if;

         when Handshake_Refused =>
            if S.Current in Handshaking | Connected then
               Fail (S, Handshake_Failed, Do_Now);
            end if;

         when Tick =>
            if S.Current in Authenticating | Associating | Handshaking and then E.Now >= S.Deadline then
               if S.Current = Handshaking then
                  Fail (S, Handshake_Timeout, Do_Now);
               elsif S.Tries >= S.Bounds.Frame_Attempts then
                  Fail (S, (if S.Current = Authenticating then Authentication_Timeout
                            else Association_Timeout), Do_Now);
               else
                  S.Tries := S.Tries + 1;
                  S.Deadline := Later (E.Now, S.Bounds.Frame_Timeout);
                  Add (Do_Now, (if S.Current = Authenticating then Send_Authentication
                                else Send_Association));
               end if;
            end if;

         when Deauthentication =>
            if S.Current /= Idle then
               Fail (S, Deauthenticated, Do_Now);
            end if;

         when Disassociation =>
            if S.Current /= Idle then
               Fail (S, Disassociated, Do_Now);
            end if;

         when Leave =>
            if S.Current /= Idle then
               Fail (S, Left, Do_Now);
            end if;
      end case;
   end Step;

end Wifi_MLME;
