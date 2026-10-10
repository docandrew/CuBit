with Interfaces;
with CCL.Catalog;
with CCL.Evaluation;
with CCL.Interfaces.Desktop_Launch;
with CCL.Language;
with CCL.Objects;
with CCL.Types;

package body CCL.Typed_Settings is
   use type CCL.Language.Interpretation_Status;
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Shape;
   use type Standard.Interfaces.Unsigned_64;
   package Views renames CCL.Objects.Views;

   LAUNCH_PREFIX : constant String := "desktop.launch.";
   FUEL : constant := 100_000;
   --  A local binding: the value never leaves the process as an image.
   LOCAL_KEY : constant CCL.Objects.Schema_Key :=
     [16#4C41_554E_4348_454E#, 16#5452_595F_4C4F_4341#, 16#4C5F_4B45_5900_0000#, 16#0000_0000_0000_0001#];

   function Kind_Of (Key : String) return Setting_Kind is
     (if Key'Length > LAUNCH_PREFIX'Length
         and then Key (Key'First .. Key'First + LAUNCH_PREFIX'Length - 1) = LAUNCH_PREFIX
      then Launch_Entry_Setting else Untyped);

   function Schema (Kind : Setting_Kind) return String is
     (case Kind is
         when Launch_Entry_Setting => CCL.Interfaces.Desktop_Launch.TYPE_SOURCE,
         when Untyped => "");
   function Root_Name (Kind : Setting_Kind) return String is
     (case Kind is
         when Launch_Entry_Setting => CCL.Interfaces.Desktop_Launch.ROOT_TYPE_NAME,
         when Untyped => "");

   function Named (Object : Views.Snapshot; At_Cursor : Views.Cursor; Name : String) return Views.Cursor is
      Shape : constant CCL.Types.Description := Views.Describe (Object, At_Cursor);
   begin
      for Part in 1 .. Shape.Count loop
         if CCL.Types.Image (Shape.Parts (Part).Identifier) = Name then
            return Views.Field (Object, At_Cursor, Part);
         end if;
      end loop;
      return Views.No_Value;
   end Named;

   function Alternative_Name (Object : Views.Snapshot; At_Cursor : Views.Cursor) return String is
      Choice : constant Natural := Views.Alternative (Object, At_Cursor);
      Shape : constant CCL.Types.Description := Views.Describe (Object, At_Cursor);
   begin
      return (if Choice in 1 .. Shape.Count then CCL.Types.Image (Shape.Parts (Choice).Identifier) else "");
   end Alternative_Name;

   --  The value's canonical source: records name every field in order,
   --  sums are Type.Member or (Type.Member payload), strings are quoted with
   --  \" and \\ escaped, lists are [...].
   function Render (Object : Views.Snapshot; At_Cursor : Views.Cursor) return String is
      Kind : constant CCL.Types.Type_Reference := Views.Type_Of (Object, At_Cursor);
      Shape : constant CCL.Types.Description := Views.Describe (Object, At_Cursor);
      function Quote (Text : String) return String is
      begin
         for K in Text'Range loop
            if Text (K) in '"' | '\' then
               return Text (Text'First .. K - 1) & '\' & Text (K) & Quote (Text (K + 1 .. Text'Last));
            end if;
         end loop;
         return Text;
      end Quote;
      function Fields (From : Positive) return String is
        (if From > Shape.Count then ""
         else " " & CCL.Types.Image (Shape.Parts (From).Identifier) & " => "
              & Render (Object, Views.Field (Object, At_Cursor, From)) & Fields (From + 1));
      function Elements (From : Positive) return String is
        (if From > Views.Length (Object, At_Cursor) then ""
         else (if From > 1 then " " else "") & Render (Object, Views.Element (Object, At_Cursor, From))
              & Elements (From + 1));
   begin
      if Kind = CCL.Types.String_Type then
         return '"' & Quote (Views.Text (Object, At_Cursor)) & '"';
      elsif Kind = CCL.Types.Integer_Type then
         declare
            Image : constant String :=
              Standard.Interfaces.Integer_64'Image (CCL.Objects.Integer_Of (Views.Scalar (Object, At_Cursor)));
         begin
            return (if Image (Image'First) = ' ' then Image (Image'First + 1 .. Image'Last) else Image);
         end;
      elsif Kind = CCL.Types.Boolean_Type then
         return (if Views.Scalar (Object, At_Cursor).First = 1 then "true" else "false");
      end if;
      case Shape.Form is
         when CCL.Types.Product =>
            return "(" & CCL.Types.Image (Shape.Identifier) & Fields (1) & ")";
         when CCL.Types.Sum =>
            declare
               Choice : constant Natural := Views.Alternative (Object, At_Cursor);
               Member : constant String :=
                 CCL.Types.Image (Shape.Identifier) & "." & Alternative_Name (Object, At_Cursor);
            begin
               if Choice not in 1 .. Shape.Count then
                  return "";
               elsif Shape.Parts (Choice).Payload = CCL.Types.Unit_Type
                 or else Shape.Parts (Choice).Payload = CCL.Types.Invalid_Type
               then
                  return Member;
               end if;
               return "(" & Member & " " & Render (Object, Views.Payload (Object, At_Cursor)) & ")";
            end;
         when CCL.Types.Sequence =>
            return "[" & Elements (1) & "]";
         when others =>
            return "";
      end case;
   end Render;

   --  Schema and value as one program: analysed, evaluated, captured.
   procedure Capture
     (Kind : Setting_Kind; Source : String; Object : in out Views.Snapshot; Result : in out Check_Result)
   is
      Program : constant String := Schema (Kind) & ASCII.LF & Source;
      Offset : constant Natural := Schema (Kind)'Length + 1;
      Catalog : CCL.Catalog.Interface_Catalog;
      Checked : CCL.Language.Analysis_Result;
      Root : CCL.Types.Type_Reference;
      Contract : CCL.Objects.Binding;
      Bound, Captured : Boolean;
      Value : CCL.Language.Object_Interpretation_Result;
      procedure Fail (Text : String; At_Position : Natural) is
      begin
         Result.Success := False;
         Result.Position := (if At_Position > Offset then At_Position - Offset else 0);
         Result.Message_Length := Natural'Min (Text'Length, MAXIMUM_MESSAGE);
         Result.Message (1 .. Result.Message_Length) := Text (Text'First .. Text'First + Result.Message_Length - 1);
      end Fail;
   begin
      Result.Success := False;
      CCL.Catalog.Initialize (Catalog);
      CCL.Language.Analyze (Program, Catalog, Checked);
      if CCL.Language."/=" (CCL.Language.Analysis_Status_Of (Checked), CCL.Language.Analysis_Succeeded) then
         Fail ("not a " & Root_Name (Kind) & ": "
               & (if CCL.Language."=" (CCL.Language.Analysis_Diagnostic (Checked), CCL.Language.Unknown_Name)
                  then "no such name"
                  elsif CCL.Language."=" (CCL.Language.Analysis_Status_Of (Checked),
                                          CCL.Language.Analysis_Type_Check_Failed)
                  then "a field or member does not have its declared type" else "it does not read as CCL"),
               CCL.Language.Analysis_Diagnostic_Position (Checked));
         return;
      end if;
      Root := CCL.Types.Find (CCL.Language.Analysis_Types (Checked), CCL.Types.Named (Root_Name (Kind)));
      CCL.Objects.Bind (CCL.Language.Analysis_Types (Checked), Root, LOCAL_KEY, Contract, Bound);
      if not Bound then
         Fail ("the schema does not declare a persistable " & Root_Name (Kind), 0);
         return;
      end if;
      CCL.Evaluation.Evaluate_Object (Program, FUEL, Contract, Value);
      if Value.Status /= CCL.Language.Succeeded or else not Value.Has_Value then
         Fail ("not a " & Root_Name (Kind) & ": it does not evaluate", Value.Diagnostic_Position);
         return;
      end if;
      Views.Capture (Object, Contract, Value.Value, Captured);
      if not Captured then
         Fail ("the value does not fit a " & Root_Name (Kind), 0);
         return;
      end if;
      Result.Success := True;
   end Capture;

   type Snapshot_Access is access Views.Snapshot;
   Work : Snapshot_Access;

   procedure Check (Kind : Setting_Kind; Source : String; Result : out Check_Result) is
   begin
      Result := (others => <>);
      if Work = null then
         Work := new Views.Snapshot;
      end if;
      Capture (Kind, Source, Work.all, Result);
      if not Result.Success then
         return;
      end if;
      declare
         Text : constant String := Render (Work.all, Views.Root (Work.all));
      begin
         if Text'Length = 0 or else Text'Length > MAXIMUM_CANONICAL then
            Result.Success := False;
            Result.Message_Length := 0;
            return;
         end if;
         Result.Length := Text'Length;
         Result.Canonical (1 .. Text'Length) := Text;
      end;
   end Check;

   procedure Read
     (Kind : Setting_Kind; Canonical : String; Object : in out Views.Snapshot; Success : out Boolean)
   is
      Result : Check_Result;
   begin
      Capture (Kind, Canonical, Object, Result);
      Success := Result.Success and then Render (Object, Views.Root (Object)) = Canonical;
   end Read;
end CCL.Typed_Settings;
