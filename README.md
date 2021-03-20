# Loga
Loga is simple Ada library for logging. Equivalent of "debug" npm package for Node.JS

## Usage example
`main.adb`
```ada
with Loga;
with Some_Cool_Http_Lib; use Some_Cool_Http_Lib;

procedure Main is
   Http_Logger : constant Loga.Logger := Loga.New_Logger ("http");

   task T;
   task body T is
      Task_Logger : constant Loga.Logger := Loga.New_Logger ("task");
   begin
      for I in 1 .. 3 loop
         Task_Logger.Log ("Hello from task loop" & I'Image & " iteration!");
         delay 1.0;
      end loop;
   end T;
begin
   Http_Logger.Log ("Starting job");
   Http_Logger.Log ("Creating GET request to www.google.com");
   Create_Get_Request ("www.google.com");
   Http_Logger.Log ("Done!");
   Http_Logger.Log ("Finishing job");
end Main;
```

Then compile it and run this command:
```
DEBUG="task,http" ./main
```

<figcaption>

__Output:__

</figcaption>

![Image of output](https://github.com/mangl-auf/loga/blob/master/output.png?raw=true)

#### Wildcard usage example
`main.adb`
```ada
with Loga;
with Ada.Text_IO;

procedure Main is
   My_Library_Main_Logger      : constant Loga.Logger := Loga.New_Logger ("my_library:main");
   My_Library_Function_Logger  : constant Loga.Logger := Loga.New_Logger ("my_library:function");

   task T;

   task body T is
      My_Library_Tasking_Logger : constant Loga.Logger := Loga.New_Logger ("my_library:tasking");
   begin
      for I in 1 .. 3 loop
         My_Library_Tasking_Logger.Log ("Hello from task!");
      end loop;
   end T;

   function Square (X : Natural) return Positive is
   begin
      My_Library_Function_Logger.Log ("Funciton 'Square' has been invoked");
      return X * X;
   end Square;
begin
   My_Library_Main_Logger.Log ("Starting main process executing");
   declare
      Result : Positive := Square (8);
   begin
      Ada.Text_IO.Put_Line ("Result:" & Result'Img);
   end;
   My_Library_Main_Logger.Log ("Finishing main process executing");
end Main;
```

Then compile it and run this command:
```
DEBUG="my_library:*" ./main
```

<figcaption>

__Output:__

</figcaption>

![Image of wildcard usage example output](https://github.com/mangl-auf/loga/blob/master/wildcard-example-output.png?raw=true)

You can also set color you want for you logger:
```ada
Logger_With_My_Color         : Loga.Logger := Loga.New_Logger (Name => "logger1",
                                                               Color => Loga.Green);
Another_Logger_With_My_Color : Loga.Logger := Loga.New_Logger (Name => "logger2",
                                                               Color => Logger_With_My_Color.Get_Color);
```

## Todo
Add NORMAL wildcards support, i.e. `lib*some*` should work (Now it doesn't), but in general wildcards work and you can see it in example.
Add some more colors
Check Windows compatibility :3
