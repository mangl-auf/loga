with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Environment_Variables;

package body Debug is
    procedure New_Logger (Self : in out Logger; Name : String) is
        package Env renames Ada.Environment_Variables;

        Env_Var : Unbounded_String;
    begin
        begin
            Env_Var := To_Unbounded_String (Env.Value ("DEBUG"));
        exception
            when Constraint_Error => 
                Self.Disabled := True;
                return;
        end;
        if To_String (Env_Var) /= Name then
            Self.Disabled := True;
        end if;

        Self.Name := To_Unbounded_String (Name);
        if Current_Color > Colors_As_Integer (Colors'Last) then
            Current_Color := Colors_As_Integer (Colors'Val (1)); -- Getting second element of Color enum 'cos first element should be unreachable
        end if;
        Self.Color := Colors'Val (Current_Color - 30);
        Current_Color := Current_Color + 1;
    end New_Logger;

    procedure Colorize_Output (Color : Colors) is
        use Ada.Integer_Text_IO;
    begin
        Put (Character'Val (27) & "[");
        Put (Colors_As_Integer (Color), Width => 0);
        Put ("m");
    end Colorize_Output;

    procedure Log (Self : Logger; Message : String) is
    begin
        if not Self.Disabled then 
            Colorize_Output (Self.Color);
            Put ("[" & To_String (Self.Name) & "]: ");
            Colorize_Output (Normal);
            Put_Line (Message);
        end if;
    end Log;
end Debug;