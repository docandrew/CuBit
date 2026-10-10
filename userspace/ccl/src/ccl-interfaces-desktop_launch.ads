--  The desktop's Apps menu entries as CCL types (docs/development-backlog.md
--  UI-013): each `desktop.launch.<key>` setting is one Launch_Entry, checked
--  against these declarations when the configuration is compiled
--  (CCL.Typed_Settings) and read back the same way by the desktop. The
--  categories are the Apps menu's submenus.
package CCL.Interfaces.Desktop_Launch with SPARK_Mode, Pure is
   TYPE_SOURCE : constant String :=
     "(type App_Category (enum System Development Web Games Media Tools)) " &
     "(type Launch_Icon (enum Workbench Console Logs Trace Doom Devices Penny Files Gameboy Settings Inspector Mesa Boot)) " &
     "(type Launch_Action (variant (Program String) (Settings))) " &
     "(type Launch_Entry (record (label String) (action Launch_Action) " &
     "(icon Launch_Icon Launch_Icon.Files) (category App_Category App_Category.Tools) " &
     "(single_instance Boolean false)))";
   ROOT_TYPE_NAME : constant String := "Launch_Entry";
   --  The members, in declaration order (for the desktop's Ada side).
   type App_Category is (System, Development, Web, Games, Media, Tools);
   type Launch_Icon is
     (Workbench, Console, Logs, Trace, Doom, Devices, Penny, Files, Gameboy, Settings, Inspector, Mesa, Boot);
end CCL.Interfaces.Desktop_Launch;
