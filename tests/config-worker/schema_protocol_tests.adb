with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects.Schemas;
with Config_Schema_Protocol;
with Config_Schema_Worker;

procedure Schema_Protocol_Tests is
   package P renames Config_Schema_Protocol;
   package S renames CCL.Objects.Schemas;
   use type P.Operation;
   use type P.Reply_Kind;
   use type CCL.Objects.Binding;
   use type S.Image;
   Registry : CCL.Types.Registry;
   Contract, Empty : CCL.Objects.Binding;
   Input, Output, Changed : P.Frame;
   Good : Boolean;
   Calls, Checks : Natural := 0;
   Backend_Kind : P.Reply_Kind := P.Created;
   Return_Valid_Type : Boolean := True;
   Empty_Metadata : constant S.Image := (others => <>);
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with "schema protocol check" & Checks'Image; end if;
   end Check;
   procedure Invoke
     (Action : P.Operation; Name, Context : String;
      Contract : CCL.Objects.Binding; Recovered : out CCL.Objects.Binding;
      Result : out P.Reply_Kind)
   is
   begin
      Check (Name = "org.cubit.settings" and Context = "machine");
      Check (CCL.Objects.Is_Bound (Contract) = (Action = P.Create));
      Calls := Calls + 1;
      Recovered := (if Return_Valid_Type then Schema_Protocol_Tests.Contract else Empty);
      Result := Backend_Kind;
   end Invoke;
   package Worker is new Config_Schema_Worker (Invoke);
begin
   Check (P.Frame'Size = 8 * 4096 * 8 and P.Frame'Alignment = 4096);
   CCL.Objects.Bind (Registry, CCL.Types.Integer_Type, [1,2,3,4], Contract, Good);
   Check (Good);
   for Action in P.Operation loop
      P.Make_Request (Action, 1, 2, "org.cubit.settings", "machine", Contract, Input, Good);
      Check (Good and P.Valid_Request (Input));
      for Kind in P.Reply_Kind loop
         P.Make_Reply (Input, Kind, Contract, Output, Good);
         Check (Good = (if Action = P.Create then Kind in P.Created | P.Already_Exists |
           P.Definition_Conflict | P.Management_Conflict | P.Rejected | P.Uncertain
           else Kind in P.Loaded | P.Loaded_Managed | P.Absent | P.Load_Failed));
         Check (P.Valid_Reply (Output, Input) = Good);
         if Good then
            Check ((Output.Metadata /= Empty_Metadata) = (Kind in P.Loaded | P.Loaded_Managed));
            Changed := Output; Changed.Token := 3; Check (not P.Valid_Reply (Changed, Input));
            Changed := Output; Changed.Session := 2; Check (not P.Valid_Reply (Changed, Input));
            Changed := Output; Changed.Name (1) := 'x'; Check (not P.Valid_Reply (Changed, Input));
            Changed := Output; Changed.Context (1) := 'x'; Check (not P.Valid_Reply (Changed, Input));
         end if;
      end loop;
      for Mutation in 1 .. 14 loop
         Changed := Input;
         case Mutation is
            when 1 => Changed.Format := 2;
            when 2 => Changed.Action := Unsigned_32'Last;
            when 3 => Changed.Reply := 1;
            when 4 => Changed.Reserved := 1;
            when 5 => Changed.Session := 0;
            when 6 => Changed.Token := 0;
            when 7 => Changed.Token := Unsigned_64'Last;
            when 8 => Changed.Name_Length := 129;
            when 9 => Changed.Context_Length := Unsigned_32'Last;
            when 10 => Changed.Name (128) := 'x';
            when 11 => Changed.Context (128) := 'x';
            when 12 => Changed.Padding (1) := 1;
            when 13 => Changed.Name (1) := '.';
            when 14 => Changed.Metadata.Reserved := 1;
         end case;
         Check (not P.Valid_Request (Changed));
         declare
            State : Worker.State;
            Before : constant Natural := Calls;
         begin
            Worker.Handle (State, Changed, Output, Good);
            Check (not Good and Calls = Before and not Worker.Needs_Recovery (State));
         end;
      end loop;
      for Kind in P.Reply_Kind loop
         for Valid_Type in Boolean loop
            declare
               State : Worker.State;
               Expected_Failure : constant Boolean :=
                 (if Action = P.Create then Kind not in P.Created | P.Already_Exists |
                    P.Definition_Conflict | P.Management_Conflict | P.Rejected
                  else Kind not in P.Loaded | P.Loaded_Managed | P.Absent or else
                    (Kind in P.Loaded | P.Loaded_Managed and not Valid_Type));
               Before : constant Natural := Calls;
            begin
               Backend_Kind := Kind; Return_Valid_Type := Valid_Type;
               Worker.Handle (State, Input, Output, Good);
               Check (Good and P.Valid_Reply (Output, Input) and Calls = Before + 1);
               Check (Worker.Needs_Recovery (State) = Expected_Failure);
               if Expected_Failure then
                  Check (Output.Reply = P.Reply_Kind'Enum_Rep
                    (if Action = P.Create then P.Uncertain else P.Load_Failed));
                  Worker.Handle (State, Input, Output, Good);
                  Check (Good and Calls = Before + 1);
               end if;
            end;
         end loop;
      end loop;
   end loop;
   P.Make_Request (P.Create, 1, 2, "org.cubit", "machine", Empty, Input, Good);
   Check (not Good);
   P.Make_Request (P.Recover, 1, 2, "org.cubit", "machine", Empty, Input, Good);
   Check (Good);
   P.Make_Request (P.Recover, 1, 2, "", "machine", Empty, Input, Good);
   Check (not Good);
   Ada.Text_IO.Put_Line ("Native schema protocol/executor: PASS" & Checks'Image & " checks");
end Schema_Protocol_Tests;
