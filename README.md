# Loga
Loga is simple Ada library for logging. Equivalent of "debug" npm package for Node.JS
## Usage example
`main.adb`
```ada
with Loga;
with Some_Cool_Http_Lib; use Some_Cool_Http_Lib;

procedure Main is
   Http_Logger : Loga.Logger;
   Task_Logger : Loga.Logger;
   task T;
   task body T is
      Task_Logger.New_Logger ("task");
      for I in 1 .. 3 loop
         Task_Logger.Log ("Hello from task loop" & I'Image & " iteration!");
         delay 1.0;
      end loop;
   end T;
begin
   Http_Logger.New_Logger ("http");
   Http_Logger.Log ("Starting job");
   Http_Logger.Log ("Creating GET request to www.google.com");
   Create_Get_Request ("www.google.com");
   Http_Logger.Log ("Done!");
   Http_Logger.Log ("Finishing job");
end Main;
```
Then compile it and run this command:
```
DEBUG=task,http ./main
```
Output:
![Image of output](https://github.com/mangl-auf/loga/blob/master/output.png?raw=true)
## Todo
Add wildcards support for DEBUG environment variable.  
Check Windows compatibility :3
