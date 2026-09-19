--  Owned, non-capturing code reference. It contains neither authority nor a
--  machine address. The receiving owner validates it when registering code.
package CCL.Handler_References with SPARK_Mode is
   Maximum_Source : constant := 1024;
   Maximum_Name : constant := 32;
   type Reference is private;
   procedure Create (Source, Name : String; Item : out Reference; Success : out Boolean);
   function Valid (Item : Reference) return Boolean;
   function Source (Item : Reference) return String;
   function Name (Item : Reference) return String;
private
   type Reference is record
      Source_Length : Natural range 0 .. Maximum_Source := 0;
      Name_Length : Natural range 0 .. Maximum_Name := 0;
      Text : String (1 .. Maximum_Source) := [others => ' '];
      Entry_Name : String (1 .. Maximum_Name) := [others => ' '];
   end record;
   function Valid (Item : Reference) return Boolean is
     (Item.Source_Length > 0 and Item.Name_Length > 0);
   function Source (Item : Reference) return String is (Item.Text (1 .. Item.Source_Length));
   function Name (Item : Reference) return String is (Item.Entry_Name (1 .. Item.Name_Length));
end CCL.Handler_References;
