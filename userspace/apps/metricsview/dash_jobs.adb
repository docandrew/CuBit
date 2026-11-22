pragma Ada_2022;
with Ada.Unchecked_Deallocation;
with Dash_Sync;

package body Dash_Jobs is
   function Reason_For (Result : File_Result) return String is
     (case Result is
        when File_Done => "",
        when File_Missing => "it is not there",
        when File_Refused => "this app may not use the Dashboards folder",
        when File_Too_Large => "it is larger than a dashboard can be",
        when File_Unavailable => "this app has no folder for dashboards",
        when File_Late => "the disk did not answer",
        when File_Failed => "the disk refused it");

   package body Worker is
      type Bytes_Access is access String;
      Buffer : Bytes_Access;

      procedure Step (V : in out Dash_View.View) is
         J : Dash_View.Job;
         Source : Bytes_Access;
         procedure Free is new Ada.Unchecked_Deallocation (String, Bytes_Access);
         use type Dash_View.Job_Kind;
      begin
         Dash_Sync.Lock;
         Dash_View.Take_Job (V, J);
         if J.Kind = Dash_View.Save_New_File then
            Source := new String'(Dash_View.Dashboard_Source (V) & ASCII.LF);
         end if;
         Dash_Sync.Unlock;
         declare
            Leaf : constant String := J.Leaf.Text (1 .. J.Leaf.Length);
         begin
            case J.Kind is
               when Dash_View.No_Job =>
                  null;
               when Dash_View.List_Folder =>
                  declare
                     Listing : Dash_View.Folder_Listing;
                  begin
                     List (Listing);
                     Dash_Sync.Lock;
                     Dash_View.End_List (V, Listing);
                     Dash_Sync.Unlock;
                  end;
               when Dash_View.Open_File =>
                  declare
                     Got : Natural := 0;
                     Result : File_Result;
                  begin
                     if Buffer = null then
                        Buffer := new String (1 .. Maximum_File_Bytes);
                     end if;
                     Read (Leaf, Buffer.all, Got, Result);
                     Dash_Sync.Lock;
                     Dash_View.End_Open
                       (V, Leaf, Buffer (1 .. (if Result = File_Done then Got else 0)), Result = File_Done,
                        (if Result /= File_Done then "can't read it: " & Reason_For (Result) else ""));
                     Dash_Sync.Unlock;
                  end;
               when Dash_View.Save_New_File =>
                  declare
                     Result : File_Result := File_Done;
                     Already : constant Boolean := Exists (Leaf);
                  begin
                     if not Already then
                        Write (Leaf, Source.all, Result);
                     end if;
                     Dash_Sync.Lock;
                     Dash_View.End_Save_New
                       (V, Leaf, not Already and then Result = File_Done,
                        (if Already then "there is one by that name already: choose another"
                         elsif Result /= File_Done then "can't save it: " & Reason_For (Result) else ""));
                     Dash_Sync.Unlock;
                  end;
            end case;
         end;
         if Source /= null then
            Free (Source);
         end if;
      end Step;
   end Worker;
end Dash_Jobs;
