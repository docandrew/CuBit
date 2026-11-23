------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The clone conversation (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;

package body Git_Clone_Sessions is
   use type Git_Packets.Event_Kind;
   use type Git_Protocol.Reply_Kind;
   use type Git_Protocol.Ref_Reply_Kind;
   use type Git_Protocol.Fetch_Reply_Kind;
   use type Git_Protocol.Fetch_Refusal;
   use type Git_Protocol.Object_Id;
   use type Clones.Outcome;
   package P renames Git_Protocol;

   LF : constant Character := Character'Val (10);

   procedure Free is new Ada.Unchecked_Deallocation (Git_Packets.Reader, Packet_Reader_Access);

   function From_Clone (O : Clones.Outcome) return Outcome is
     (case O is
         when Clones.In_Progress => In_Progress,
         when Clones.Cloned => Cloned,
         when Clones.Target_Exists => Target_Exists,
         when Clones.Bad_Directory => Bad_Directory,
         when Clones.Bad_Url => Bad_Url,
         when Clones.Write_Failed => Write_Failed,
         when Clones.Bad_Pack => Bad_Pack,
         when Clones.Bad_Delta => Bad_Delta,
         when Clones.Too_Many_Objects => Too_Many_Objects,
         when Clones.Too_Large => Too_Large,
         when Clones.Missing_Object => Missing_Object,
         when Clones.Bad_Object => Bad_Object,
         when Clones.Path_Too_Long => Path_Too_Long,
         when Clones.No_Refs => No_Refs);

   --  How the current exchange's response is refused.
   function Refusal_Of (E : Exchange) return Outcome is
     (case E is
         when Advertisement => Not_Smart_Http,
         when List_Refs => Bad_Refs,
         when Fetch_Pack | Finished => Bad_Fetch);

   function Next (C : Session) return Exchange is (C.Next);
   function Result (C : Session) return Outcome is (C.Result);
   function Detail (C : Session) return String is (To_String (C.Detail));
   function Format (C : Session) return P.Object_Format is (Clones.Format (C.Clone));
   function Refs (C : Session) return Natural is (C.Ref_Total);
   function Objects (C : Session) return Natural is (Clones.Objects (C.Clone));
   function Files (C : Session) return Natural is (Clones.Files (C.Clone));

   --  The end: the result, the packet reader let go.
   procedure Conclude (C : in out Session; O : Outcome; Why : String) is
   begin
      if C.Result = In_Progress then
         C.Result := O;
         C.Detail := To_Text (Why);
      end if;
      C.Next := Finished;
      Free (C.Packets);
   end Conclude;

   --  A refusal: everything the clone made is removed.
   procedure Fail (C : in out Session; S : in out Store; O : Outcome; Why : String) is
   begin
      if C.Next /= Finished then
         Clones.Abandon (C.Clone, S);
         Conclude (C, O, Why);
      end if;
   end Fail;

   procedure Start (C : in out Session; S : in out Store; Parent, Directory, Url : String; Ceiling : Limits) is
   begin
      C.Ceiling := Ceiling;
      Clones.Start (C.Clone, S, Parent, Directory, Url,
                    (Maximum_Objects => Ceiling.Maximum_Objects, Maximum_Bytes => Ceiling.Maximum_Bytes));
      if Clones.Result (C.Clone) /= Clones.In_Progress then
         Conclude (C, From_Clone (Clones.Result (C.Clone)), Clones.Detail (C.Clone));
         return;
      end if;
      C.Packets := new Git_Packets.Reader;
      Git_Packets.Start (C.Packets.all);
      P.Start (C.Advert);
      C.Next := Advertisement;
   end Start;

   function Request_Body (C : Session) return String is
      Format_Line : constant String :=
        (if C.Format_Named then P.Packet ("object-format=" & P.Format_Name (Format (C)) & LF) else "");
   begin
      if C.Next = List_Refs then
         return P.Packet ("command=ls-refs" & LF) & Format_Line & P.DELIMITER
           & P.Packet ("peel" & LF) & P.Packet ("symrefs" & LF) & P.Packet ("ref-prefix HEAD" & LF)
           & P.Packet ("ref-prefix refs/heads/" & LF) & P.Packet ("ref-prefix refs/tags/" & LF) & P.FLUSH;
      end if;
      declare
         Wants : Text;
      begin
         for K in 1 .. C.Wants.Length loop
            Append (Wants, P.Packet ("want " & P.To_Hex (C.Wants (K), Format (C)) & LF));
         end loop;
         return P.Packet ("command=fetch" & LF) & Format_Line & P.DELIMITER
           & P.Packet ("ofs-delta" & LF) & P.Packet ("no-progress" & LF) & To_String (Wants)
           & P.Packet ("done" & LF) & P.FLUSH;
      end;
   end Request_Body;

   --  One packet of the current response.
   procedure Handle (C : in out Session; S : in out Store; Kind : Git_Packets.Packet_Kind; Payload : Byte_Array) is
   begin
      case C.Next is
         when Advertisement =>
            declare
               R : P.Reply_Kind;
            begin
               P.Feed (C.Advert, Kind, Payload, R);
               if R = P.Refused then
                  Fail (C, S, Not_Smart_Http, P.Name (P.Failure (C.Advert)));
               elsif R = P.Ended then
                  C.Ended := True;
               end if;
            end;
         when List_Refs =>
            declare
               R : P.Ref_Reply_Kind;
            begin
               P.Feed (C.Ref_List, Kind, Payload, R);
               case R is
                  when P.Ref_Read =>
                     Clones.Add_Ref
                       (C.Clone, P.Ref_Name (C.Ref_List), P.Ref_Id (C.Ref_List),
                        (if P.Has_Symref (C.Ref_List) then P.Symref_Target (C.Ref_List) else ""),
                        P.Has_Peeled (C.Ref_List),
                        (if P.Has_Peeled (C.Ref_List) then P.Peeled_Id (C.Ref_List) else P.NO_ID));
                     if (for all K in 1 .. C.Wants.Length => C.Wants (K) /= P.Ref_Id (C.Ref_List)) then
                        C.Wants.Append (P.Ref_Id (C.Ref_List));
                     end if;
                  when P.Refs_Ended =>
                     C.Ended := True;
                  when P.Refs_Refused =>
                     Fail (C, S, Bad_Refs, P.Name (P.Failure (C.Ref_List)));
               end case;
            end;
         when Fetch_Pack =>
            declare
               R : P.Fetch_Reply;
            begin
               P.Feed (C.Fetch, Kind, Payload, R);
               case R.Kind is
                  when P.Pack_Data =>
                     Clones.Pack_Data (C.Clone, S, Payload (R.First .. R.Last));
                     if Clones.Result (C.Clone) /= Clones.In_Progress then
                        Conclude (C, From_Clone (Clones.Result (C.Clone)), Clones.Detail (C.Clone));
                        Clones.Abandon (C.Clone, S);
                     end if;
                  when P.Fetch_Ended =>
                     C.Ended := True;
                  when P.Fetch_Refused =>
                     Fail (C, S, (if P.Failure (C.Fetch) = P.Remote_Error then Remote_Error else Bad_Fetch),
                           P.Name (P.Failure (C.Fetch)) & " " & P.Message (C.Fetch));
                  when P.Progress | P.Nothing =>
                     null;
               end case;
            end;
         when Finished =>
            null;
      end case;
   end Handle;

   procedure Take (C : in out Session; S : in out Store; Data : Byte_Array) is
      Next : Positive := Data'First;
      E : Git_Packets.Event;
   begin
      while C.Next /= Finished and then Next <= Data'Last loop
         Git_Packets.Step (C.Packets.all, Data, Next, E);
         case E.Kind is
            when Git_Packets.Need_More =>
               exit;
            when Git_Packets.Refused =>
               Fail (C, S, Refusal_Of (C.Next), "PACKET " & Git_Packets.Name (E.Why));
            when Git_Packets.Packet =>
               Handle (C, S, E.Packet, Git_Packets.Payload (C.Packets.all));
         end case;
      end loop;
   end Take;

   procedure Response_Ended (C : in out Session; S : in out Store) is
      E : Git_Packets.Event;
   begin
      if C.Next = Finished then
         return;
      end if;
      Git_Packets.Input_Ended (C.Packets.all, E);
      if E.Kind = Git_Packets.Refused then
         Fail (C, S, Refusal_Of (C.Next), "PACKET " & Git_Packets.Name (E.Why));
         return;
      elsif not C.Ended then
         Fail (C, S, Refusal_Of (C.Next), "the response ended early");
         return;
      end if;
      C.Ended := False;
      Git_Packets.Start (C.Packets.all);
      case C.Next is
         when Advertisement =>
            C.Format_Named := P.Offers_Object_Format (C.Advert);
            Clones.Set_Format (C.Clone, P.Format (C.Advert));
            P.Start (C.Ref_List, C.Ceiling.Maximum_Refs, P.Format (C.Advert));
            C.Next := List_Refs;
         when List_Refs =>
            C.Ref_Total := P.Count (C.Ref_List);
            if C.Wants.Is_Empty then
               --  An empty repository: nothing to fetch (Finish says No_Refs).
               Clones.Finish (C.Clone, S);
               Conclude (C, From_Clone (Clones.Result (C.Clone)), Clones.Detail (C.Clone));
            else
               P.Start (C.Fetch);
               C.Next := Fetch_Pack;
            end if;
         when Fetch_Pack =>
            Clones.Finish (C.Clone, S);
            Conclude (C, From_Clone (Clones.Result (C.Clone)), Clones.Detail (C.Clone));
         when Finished =>
            null;
      end case;
   end Response_Ended;

   procedure Abandon (C : in out Session; S : in out Store; Detail : String) is
   begin
      Fail (C, S, Abandoned, Detail);
   end Abandon;
end Git_Clone_Sessions;
