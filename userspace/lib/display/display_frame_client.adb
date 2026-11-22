------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Display_Pool_Protocol;

package body Display_Frame_Client is

   package PW renames CuBit.Display_Pool_Protocol;
   use type FR.RC.Token;

   procedure Attach (C : in out Client; Own_Region, Display_Region : System.Address) is
   begin
      FR.RC.Attach_Client (C.Ring, Own_Region, Display_Region);
      C.Attached := True;
   end Attach;

   procedure Detach (C : in out Client) is
   begin
      if C.Attached then
         FR.RC.Detach_Client (C.Ring);
      end if;
      C.Attached := False;
   end Detach;

   function Attached (C : Client) return Boolean is (C.Attached);

   function Outstanding (C : Client) return Natural is
      Count : Natural := 0;
   begin
      for E of C.Table loop
         if E.Used then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Outstanding;

   procedure Submit
     (C : in out Client; Item : FR.Present; Token, Frame : Unsigned_64;
      Result : out Submit_Result; Wake_Display : out Boolean)
   is
      Free : Natural := 0;
      Submitted : Boolean;
   begin
      Wake_Display := False;
      if not C.Attached then
         Result := Not_Attached;
         return;
      end if;
      Result := Full;
      if C.Next_Tag = FR.RC.Token'Last or else not FR.RC.Can_Submit (C.Ring) then
         return;
      end if;
      for I in C.Table'Range loop
         if not C.Table (I).Used then
            Free := I;
            exit;
         end if;
      end loop;
      if Free = 0 then
         return;
      end if;
      C.Next_Tag := C.Next_Tag + 1;
      FR.RC.Submit (C.Ring, C.Next_Tag, FR.Encode (Item), Submitted, Wake_Display);
      if not Submitted then
         Wake_Display := False;
         return;
      end if;
      C.Table (Free) := (Used => True, Tag => C.Next_Tag, Token => Token, Frame => Frame);
      Result := Queued;
   end Submit;

   procedure Count_Stray (C : in out Client) is
   begin
      if C.Strays < Unsigned_64'Last then
         C.Strays := C.Strays + 1;
      end if;
   end Count_Stray;

   procedure Reap (C : in out Client; Item : out Reaped; Got : out Boolean) is
      Taken : FR.RC.Queues.Completion;
      Found : Boolean;
   begin
      Item := (others => <>);
      Got := False;
      if not C.Attached then
         return;
      end if;
      loop
         FR.RC.Reap (C.Ring, Taken, Found);
         exit when not Found;
         declare
            --  Taken is this side's copy; decoded once.
            Answer : constant FR.Answer_Decoding := FR.Decode (Taken.Answer);
            Matched : Natural := 0;
         begin
            if Answer.Valid then
               for I in C.Table'Range loop
                  if C.Table (I).Used and then C.Table (I).Tag = Taken.Tag then
                     Matched := I;
                     exit;
                  end if;
               end loop;
            end if;
            if Matched /= 0 then
               declare
                  Result : constant PW.Completion_Decoding :=
                    PW.Decode_Completion (Answer.Value.Result);
               begin
                  --  A frame result must name the frame its tag carried; an
                  --  answer that is no frame result (Display could not read
                  --  the request) is that present's typed failure.
                  if Result.Valid and then Result.Value.Result.Frame /= C.Table (Matched).Frame then
                     Matched := 0;
                  end if;
               end;
            end if;
            if Matched = 0 then
               Count_Stray (C);
            else
               Item := (Token => C.Table (Matched).Token, Result => Answer.Value.Result,
                        Taken_Us => Answer.Value.Taken_Us, Answered_Us => Answer.Value.Answered_Us);
               C.Table (Matched) := (others => <>);
               Got := True;
               return;
            end if;
         end;
      end loop;
   end Reap;

   procedure Fail_Next (C : in out Client; Token : out Unsigned_64; Got : out Boolean) is
   begin
      Token := 0;
      Got := False;
      if C.Attached then
         return;
      end if;
      for E of C.Table loop
         if E.Used then
            Token := E.Token;
            E := (others => <>);
            Got := True;
            return;
         end if;
      end loop;
   end Fail_Next;

   function Arm (C : in out Client) return Boolean is
     (not C.Attached or else FR.RC.Arm_For_Completions (C.Ring));

   procedure Disarm (C : Client) is
   begin
      if C.Attached then
         FR.RC.Disarm_For_Completions (C.Ring);
      end if;
   end Disarm;

   function Strays (C : Client) return Unsigned_64 is (C.Strays);

end Display_Frame_Client;
