------------------------------------------------------------------------------
--  Hosted tests for Grant_Doorbells and Grant_Bell_Lists (CAP-002 step 3,
--  docs/capability-handles.md section 4): the kernel's production units,
--  driven the way Process.IPC drives them, against a reference model.
--
--  Grants live in an array (as the kernel's grant records do); each process
--  has a bell list linked through them. Random operations: create a grant,
--  ring it (by its owner), take a process's oldest rung grant, restore a
--  taken bell, end a grant, and ask what a waiter that saw a sequence should
--  do. After every operation the whole structure is compared with the
--  model: each list's order (FIFO), reciprocal links, counts, pending bits
--  equal to membership, ring counts and ends.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Grant_Doorbells; use Grant_Doorbells;
with Grant_Bell_Lists;

procedure Main is

   Grants    : constant := 64;
   Processes : constant := 4;
   Steps     : constant := 400_000;

   type Slot is range 0 .. Grants;
   No_Slot : constant Slot := 0;
   subtype Grant_Slot is Slot range 1 .. Grants;
   type Process_Id is range 0 .. Processes;
   No_Process : constant Process_Id := 0;
   subtype Live_Process is Process_Id range 1 .. Processes;

   type Record_Of_Grant is record
      Live    : Boolean := False;
      Life    : Natural := 0;
      Owner, Grantee : Process_Id := No_Process;
      B       : Bell := Silent;
      Prev, Next : Slot := No_Slot;
   end record;
   Table : array (Grant_Slot) of Record_Of_Grant;

   function Prev_Of (S : Slot) return Slot is (Table (S).Prev);
   function Next_Of (S : Slot) return Slot is (Table (S).Next);
   procedure Set_Prev (S, V : Slot) is begin Table (S).Prev := V; end Set_Prev;
   procedure Set_Next (S, V : Slot) is begin Table (S).Next := V; end Set_Next;

   package Lists is new Grant_Bell_Lists
     (Slot, No_Slot, Prev_Of, Next_Of, Set_Prev, Set_Next);
   Bell_Lists : array (Live_Process) of Lists.List := [others => Lists.Empty];

   --  The model.
   package Slot_Vectors is new Ada.Containers.Vectors (Positive, Grant_Slot);
   use type Slot_Vectors.Vector;
   type Model_Grant is record
      Live, Pending, Ended : Boolean := False;
      Grantee : Process_Id := No_Process;
   end record;
   Model : array (Grant_Slot) of Model_Grant;
   Queues : array (Live_Process) of Slot_Vectors.Vector;

   package Random_Natural is new Ada.Numerics.Discrete_Random (Natural);
   Gen : Random_Natural.Generator;
   function Pick (N : Positive) return Natural is (Random_Natural.Random (Gen) mod N);

   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures <= 10 then
            Put_Line ("FAIL: " & What);
         end if;
      end if;
   end Check;

   procedure Compare (Step : Natural) is
      S : Slot;
      Previous : Slot;
      Walked : Slot_Vectors.Vector;
   begin
      for P in Live_Process loop
         Walked.Clear;
         S := Bell_Lists (P).Head;
         Previous := No_Slot;
         while S /= No_Slot and then Natural (Walked.Length) <= Grants loop
            Check (Table (S).Prev = Previous, "reciprocal link at step" & Step'Image);
            Walked.Append (S);
            Previous := S;
            S := Table (S).Next;
         end loop;
         Check (Bell_Lists (P).Tail = Previous, "tail at step" & Step'Image);
         Check (Walked = Queues (P), "list order equals model FIFO at step" & Step'Image);
         Check (Bell_Lists (P).Count = Natural (Queues (P).Length), "count at step" & Step'Image);
      end loop;
      for G in Grant_Slot loop
         if Table (G).Live or else Model (G).Live then
            Check (Table (G).B.Pending = Model (G).Pending, "pending bit at step" & Step'Image);
            Check (Table (G).B.Ended = Model (G).Ended, "ended at step" & Step'Image);
            Check ((Table (G).Grantee /= No_Process and then
                    Queues (Table (G).Grantee).Contains (G)) = Model (G).Pending,
                   "pending equals membership at step" & Step'Image);
         end if;
      end loop;
   end Compare;

   Lives : Natural := 0;
   procedure Create is
      G : constant Grant_Slot := Grant_Slot (1 + Pick (Grants));
   begin
      if Table (G).Live then
         return;
      end if;
      Lives := Lives + 1;
      Table (G) := (Live => True, Life => Lives, Owner => Live_Process (1 + Pick (Processes)),
                    Grantee => Live_Process (1 + Pick (Processes)), others => <>);
      Model (G) := (Live => True, Grantee => Table (G).Grantee, others => <>);
   end Create;

   procedure Do_Ring is
      G : constant Grant_Slot := Grant_Slot (1 + Pick (Grants));
      Outcome : Ring_Outcome;
      Newly : Boolean;
   begin
      if not Table (G).Live then
         return;
      end if;
      Ring (Table (G).B, Outcome, Newly);
      if Model (G).Ended then
         Check (Outcome = Ended_Grant and then not Newly, "ring after end refused");
         return;
      end if;
      Check (Outcome = Rung, "ring accepted");
      Check (Newly = not Model (G).Pending, "newly iff not pending");
      if Newly then
         Lists.Append (Bell_Lists (Table (G).Grantee), G);
         Queues (Table (G).Grantee).Append (G);
      end if;
      Model (G).Pending := True;
   end Do_Ring;



   procedure Do_Take is
      P : constant Live_Process := Live_Process (1 + Pick (Processes));
      S : Slot;
      Taken : Boolean;
   begin
      Lists.Pop (Bell_Lists (P), S);
      if Queues (P).Is_Empty then
         Check (S = No_Slot, "empty list pops nothing");
         return;
      end if;
      Check (S = Queues (P).First_Element, "oldest first");
      Queues (P).Delete_First;
      if S /= No_Slot then
         Take (Table (S).B, Taken);
         Check (Taken, "a listed grant's bell is pending");
         Model (S).Pending := False;
      end if;
   end Do_Take;


   procedure Do_End is
      G : constant Grant_Slot := Grant_Slot (1 + Pick (Grants));
      Was_Pending : Boolean;
      Index : Slot_Vectors.Extended_Index;
   begin
      if not Table (G).Live then
         return;
      end if;
      if Pick (3) = 0 and then Model (G).Ended then
         --  Retire: the record is reset (invalidateGrant); never while linked.
         Check (not Table (G).B.Pending, "no retired record is linked");
         Table (G) := (others => <>);
         Model (G) := (others => <>);
         return;
      end if;
      Finish (Table (G).B, Was_Pending);
      Check (Was_Pending = Model (G).Pending, "finish reports pending");
      if Was_Pending then
         Lists.Remove (Bell_Lists (Table (G).Grantee), G);
         Index := Queues (Table (G).Grantee).Find_Index (G);
         Queues (Table (G).Grantee).Delete (Index);
      end if;
      Model (G).Pending := False;
      Model (G).Ended := True;
   end Do_End;




   Seed : Integer := 20261017;
begin
   if Ada.Command_Line.Argument_Count > 0 then
      Seed := Integer'Value (Ada.Command_Line.Argument (1));
   end if;
   Random_Natural.Reset (Gen, Seed);
   for Step in 1 .. Steps loop
      case Pick (10) is
         when 0 | 1 => Create;
         when 2 | 3 | 4 => Do_Ring;
         when 5 | 6 => Do_Take;
         when 7 | 8 => Do_End;
         when others => Do_Take;
      end case;
      Compare (Step);
   end loop;
   Put_Line ("grant-doorbells: checks" & Checks'Image & ", failures" & Failures'Image);
   if Failures > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;
